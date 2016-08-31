CREATE TABLE HOSPITAL_DISCHARGE_FAILURE(
EXTRACT_HESID varchar2(32), 
HES_YEAR NUMBER(4,0), 
NUM_CIPS NUMBER(4,0), 
FIRST_HOSP DATE, 
MAX_EMERGENCY_DATE DATE,
died NUMBER(1,0),
LSOA01CD varchar2(10), 
IMD_GROUP varchar2(3),
SEX VARCHAR2(1), 
AGE_GROUP VARCHAR2(10),
discharge_failure NUMBER(1,0)
)

CREATE OR REPLACE PROCEDURE COMPUTE_HOSP_DIS_FAILURE
AUTHID CURRENT_USER
AS
BEGIN

FOR year_i IN 2001..2011 LOOP
  INSERT INTO HOSPITAL_DISCHARGE_FAILURE 
  SELECT c.EXTRACT_HESID, 
  year_i, 
  COUNT(c.CIPS_NUMBER) NUM_CIPS, 
  MIN(c.admidate) FIRST_HOSP, 
  MAX(e.MAX_EMERGENCY_DATE) MAX_EMERGENCY_DATE,
  CASE WHEN MAX(m.dod) IS NULL THEN 0 ELSE 1 END AS died,
  MAX(c.SOAL) AS LSOA01CD, 
  'Q' || MAX(imd.QUINTILE) AS IMD_GROUP,
  CASE MAX(pat.sex) WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END AS SEX, 
  MAX(c.AGE_GROUP) AS AGE_GROUP,
  CASE WHEN (MAX(m.dod) IS NOT NULL) OR (MAX(NVL(e.MAX_EMERGENCY_DATE,to_date('01-JAN-1970')))>MIN(c.admidate)) THEN 1 ELSE 0 END AS discharge_failure
  FROM 
  (SELECT cips.cips_number, cips.extract_hesid, cips.admidate, cips.soal, cips.hes_year, a.age_group FROM
  HES_INPATIENT_CIPS cips
  INNER JOIN
  CIPS_AGE a
  ON cips.cips_number = a.cips_number) c
  INNER JOIN
  IMD_2010 imd
  ON c.SOAL = imd.LSOA01CD
  LEFT JOIN 
  HES_MORTALITY_OPTIM m
  ON m.EXTRACT_HESID = c.EXTRACT_HESID AND m.HES_YEAR = C.HES_YEAR
  INNER JOIN HES_PATIENT pat
  ON c.EXTRACT_HESID = pat.EXTRACT_HESID
  LEFT JOIN
  (SELECT EXTRACT_HESID, year_i AS HES_YEAR, MAX(admidate) AS MAX_EMERGENCY_DATE FROM HES_INPATIENT_CIPS 
  WHERE admimeth BETWEEN 20 AND 30 AND HES_YEAR=year_i
  GROUP BY EXTRACT_HESID) e
  ON e.EXTRACT_HESID = c.EXTRACT_HESID AND e.HES_YEAR = c.HES_YEAR
  WHERE c.HES_YEAR=year_i
  GROUP BY c.EXTRACT_HESID;
  COMMIT;
END LOOP;

END COMPUTE_HOSP_DIS_FAILURE;

CREATE TABLE inpatient_wait_by_specialty AS
  SELECT 
  e.HES_YEAR,
  MAINSPEF,
  e.SOAL AS LSOA01CD,
  COUNT(DISTINCT(c.extract_hesid)) AS pop,
  SUM(c.elecdur) AS inpatient_wait 
  FROM HES_INPATIENT_EPISODE e
  INNER JOIN HES_INPATIENT_CIPS c
  ON c.cips_number = e.cips_number
  WHERE 
  NVL(c.elecdur,366) < 366 AND c.admimeth BETWEEN 11 and 12
  GROUP BY e.hes_year, e.mainspef, e.soal;


INSERT INTO HEPI_INDICATORS_ADJUSTED_LSOA 
SELECT 'inpatient_wait_adjusted', obs.hes_year, obs.lsoa01cd, observed, expected, national_avg_wait, lsoa_population, imd_quintile,
CASE WHEN expected > 0 THEN (observed/expected)*national_avg_wait*lsoa_population ELSE 0 END AS adjusted_wait
FROM
	-- observed
	(SELECT hes_year, lsoa01cd, SUM(inpatient_wait) AS observed FROM 
	INPATIENT_WAIT_BY_SPECIALTY
	GROUP BY hes_year, lsoa01cd) obs
INNER JOIN
	-- expected
	(SELECT hes_year, lsoa01cd, SUM(adjusted_wait) AS expected FROM
		(SELECT lsoa01cd, nat.hes_year, nat.mainspef, nat.national_avg_wait_spec*pop.lsoa_pop_spec AS adjusted_wait FROM
		-- LSOA pop specialty
		(SELECT hes_year, lsoa01cd, NVL(mainspef,'U') mainspef, SUM(pop) AS lsoa_pop_spec FROM 
		INPATIENT_WAIT_BY_SPECIALTY
		GROUP BY hes_year, lsoa01cd, mainspef) pop
		INNER JOIN
		-- national rate
		(SELECT hes_year, NVL(mainspef,'U') mainspef, SUM(inpatient_wait)/SUM(pop) AS national_avg_wait_spec FROM 
		INPATIENT_WAIT_BY_SPECIALTY
		GROUP BY hes_year, mainspef) nat
		ON pop.hes_year=nat.hes_year AND pop.mainspef=nat.mainspef) adj
	GROUP BY hes_year, lsoa01cd) exp
ON obs.hes_year=exp.hes_year AND obs.lsoa01cd=exp.lsoa01cd
INNER JOIN
	-- crude national average wait
	(SELECT hes_year, SUM(inpatient_wait)/SUM(pop) AS national_avg_wait FROM 
	INPATIENT_WAIT_BY_SPECIALTY GROUP BY hes_year) nat
ON obs.hes_year=nat.hes_year
INNER JOIN
	-- LSOA population
	(SELECT lsoa01cd, hes_year, SUM(pop) AS lsoa_population FROM 
	INPATIENT_WAIT_BY_SPECIALTY GROUP BY hes_year, lsoa01cd) pop
ON pop.hes_year=obs.hes_year AND pop.lsoa01cd=obs.lsoa01cd
INNER JOIN 
	(SELECT lsoa01cd, quintile AS imd_quintile FROM IMD_2010) imd
ON obs.lsoa01cd=imd.lsoa01cd;
COMMIT;

INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
'inpatient_wait_adjusted', 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
FROM hepi_indicators_adjusted_lsoa WHERE indicator='inpatient_wait_adjusted' GROUP BY lsoa01cd, indicator, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd, indicator;
COMMIT;


INSERT INTO HEPI_INDICATORS_ADJUSTED_LSOA
SELECT obs.indicator || '_adjusted', obs.year, obs.lsoa01cd, observed, expected, national_rate, population, imd_quintile,
CASE WHEN expected > 0 THEN (observed/expected)*national_rate*population ELSE 0 END AS adjusted_count
FROM
	-- observed
	(SELECT indicator, year, lsoa01cd, SUM(indicator_value) AS observed FROM 
	HEPI_INDICATORS_LSOA 
  WHERE indicator IN ('unplanned_hospitalisations','hosp_discharge_failure','hosp_discharge_failure_no_death','amenable_mortality','all_mortality')
	GROUP BY indicator, year, lsoa01cd) obs
INNER JOIN
	-- expected
	(SELECT indicator, year, lsoa01cd, SUM(adjusted_count) AS expected FROM
		(SELECT indicator, pop.lsoa01cd, nat.year, nat.age_group, nat.sex, nat.national_rate*pop.lsoa_age_sex_pop AS adjusted_count FROM
		-- LSOA population
		(SELECT lsoa01cd, year, age_group, sex, indicator_value AS lsoa_age_sex_pop FROM 
		HEPI_INDICATORS_LSOA 
		WHERE indicator='population') pop
		INNER JOIN
		-- national rate
		(SELECT nc.indicator, nc.year, nc.age_group, nc.sex, national_count, national_pop, (national_count/national_pop) AS national_rate FROM
			(SELECT indicator, year, age_group, sex, SUM(indicator_value) AS national_count FROM 
			HEPI_INDICATORS_LSOA 
      WHERE indicator IN ('unplanned_hospitalisations','hosp_discharge_failure','hosp_discharge_failure_no_death','amenable_mortality','all_mortality')
			GROUP BY indicator, year, age_group, sex) nc
			INNER JOIN
			(SELECT year, age_group, sex, SUM(indicator_value) AS national_pop FROM 
			HEPI_INDICATORS_LSOA 
			WHERE indicator='population' AND indicator_value>0
			GROUP BY year, age_group, sex) np
			ON nc.year = np.year AND nc.age_group=np.age_group AND nc.sex=np.sex) nat
		ON pop.year=nat.year AND pop.age_group=nat.age_group AND pop.sex=nat.sex) adj
	GROUP BY indicator, year, lsoa01cd) exp
ON obs.year=exp.year AND obs.indicator=exp.indicator AND obs.lsoa01cd=exp.lsoa01cd
INNER JOIN
	-- crude national rate
	(SELECT indicator, ind.year, (indicator_value/population) AS national_rate FROM
	(SELECT indicator, year, indicator_value FROM HEPI_INDICATORS
	WHERE age_group='ALL' AND sex='A' AND imd_group='ALL' AND indicator IN ('unplanned_hospitalisations','hosp_discharge_failure','hosp_discharge_failure_no_death','amenable_mortality','all_mortality')) ind
	INNER JOIN
	(SELECT year, indicator_value AS population FROM HEPI_INDICATORS
	WHERE age_group='ALL' AND sex='A' AND imd_group='ALL' AND indicator='population') pop
	ON ind.year=pop.year) nat
ON obs.year=nat.year AND obs.indicator=nat.indicator
INNER JOIN
	-- LSOA population
	(SELECT lsoa01cd, year, SUM(indicator_value) AS population FROM HEPI_INDICATORS_LSOA WHERE indicator='population' GROUP BY year, lsoa01cd) pop
ON pop.year=obs.year AND pop.lsoa01cd=obs.lsoa01cd
INNER JOIN 
	(SELECT lsoa01cd, quintile AS imd_quintile FROM IMD_2010) imd
ON obs.lsoa01cd=imd.lsoa01cd;

COMMIT;

INSERT INTO HEPI_INDICATORS_LSOA_2011
	SELECT 
	lsoa11cd, 
	indicator, 
	year, 
	SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
	FROM 
	(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
	FROM hepi_indicators_adjusted_lsoa WHERE indicator IN ('unplanned_hospitalisations_adjusted','hosp_discharge_failure_adjusted','hosp_discharge_failure_no_death_adjusted','amenable_mortality_adjusted','all_mortality_adjusted') GROUP BY lsoa01cd, indicator, year) ind
	INNER JOIN
	LSOA_2001_2011_LOOKUP lkup
	ON ind.lsoa01cd=lkup.lsoa01cd
	GROUP BY year, lsoa11cd, indicator;
  
 COMMIT;

--- disease specific adjustment

INSERT INTO HEPI_INDICATORS_ADJUSTED_LSOA
SELECT obs.indicator || '_adjusted', obs.year, obs.lsoa01cd, observed, expected, national_rate, population, imd_quintile,
CASE WHEN expected > 0 THEN (observed/expected)*national_rate*population ELSE 0 END AS adjusted_count
FROM
	-- observed
	(SELECT indicator, year, lsoa01cd, SUM(indicator_value) AS observed FROM 
	HEPI_INDICATORS_LSOA 
  WHERE indicator IN ('diabetes_emergency_hosp','chd_emergency_hosp','diabetes_mortality','chd_mortality')
	GROUP BY indicator, year, lsoa01cd) obs
INNER JOIN
	-- expected
	(SELECT indicator, year, lsoa01cd, SUM(adjusted_count) AS expected FROM
		(SELECT indicator, pop.lsoa01cd, nat.year, nat.age_group, nat.sex, nat.national_rate*pop.lsoa_age_sex_pop AS adjusted_count FROM
		-- LSOA population
		(SELECT lsoa01cd, year, age_group, sex, indicator_value AS lsoa_age_sex_pop FROM 
		HEPI_INDICATORS_LSOA 
		WHERE indicator='population') pop
		INNER JOIN
		-- national rate
		(SELECT nc.indicator, nc.year, nc.age_group, nc.sex, national_count, national_pop, (national_count/national_pop) AS national_rate FROM
			(SELECT indicator, year, age_group, sex, SUM(indicator_value) AS national_count FROM 
			HEPI_INDICATORS_LSOA 
      WHERE indicator IN ('diabetes_emergency_hosp','chd_emergency_hosp','diabetes_mortality','chd_mortality')
			GROUP BY indicator, year, age_group, sex) nc
			INNER JOIN
			(SELECT year, age_group, sex, SUM(indicator_value) AS national_pop FROM 
			HEPI_INDICATORS_LSOA 
			WHERE indicator='population' AND indicator_value>0
			GROUP BY year, age_group, sex) np
			ON nc.year = np.year AND nc.age_group=np.age_group AND nc.sex=np.sex) nat
		ON pop.year=nat.year AND pop.age_group=nat.age_group AND pop.sex=nat.sex) adj
	GROUP BY indicator, year, lsoa01cd) exp
ON obs.year=exp.year AND obs.indicator=exp.indicator AND obs.lsoa01cd=exp.lsoa01cd
INNER JOIN
	-- crude national rate
	(SELECT indicator, ind.year, (indicator_value/population) AS national_rate FROM
	(SELECT indicator, year, indicator_value FROM HEPI_INDICATORS
	WHERE age_group='ALL' AND sex='A' AND imd_group='ALL' AND indicator IN ('diabetes_emergency_hosp','chd_emergency_hosp','diabetes_mortality','chd_mortality')) ind
	INNER JOIN
	(SELECT year, indicator_value AS population FROM HEPI_INDICATORS
	WHERE age_group='ALL' AND sex='A' AND imd_group='ALL' AND indicator='population') pop
	ON ind.year=pop.year) nat
ON obs.year=nat.year AND obs.indicator=nat.indicator
INNER JOIN
	-- LSOA population
	(SELECT lsoa01cd, year, SUM(indicator_value) AS population FROM HEPI_INDICATORS_LSOA WHERE indicator='population' GROUP BY year, lsoa01cd) pop
ON pop.year=obs.year AND pop.lsoa01cd=obs.lsoa01cd
INNER JOIN 
	(SELECT lsoa01cd, quintile AS imd_quintile FROM IMD_2010) imd
ON obs.lsoa01cd=imd.lsoa01cd;

COMMIT;

INSERT INTO HEPI_INDICATORS_LSOA_2011
	SELECT 
	lsoa11cd, 
	indicator, 
	year, 
	SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
	FROM 
	(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
	FROM hepi_indicators_adjusted_lsoa WHERE indicator IN ('diabetes_emergency_hosp','chd_emergency_hosp','diabetes_mortality','chd_mortality') GROUP BY lsoa01cd, indicator, year) ind
	INNER JOIN
	LSOA_2001_2011_LOOKUP lkup
	ON ind.lsoa01cd=lkup.lsoa01cd
	GROUP BY year, lsoa11cd, indicator;
  
 COMMIT;