create or replace PROCEDURE ADJUST_INDICATORS
AUTHID CURRENT_USER
AS
BEGIN

EXECUTE IMMEDIATE 'CREATE TABLE HEPI_INDICATORS_ADJUSTED_LSOA AS
SELECT obs.indicator, obs.year, obs.lsoa01cd, observed, expected, national_rate, population, imd_quintile,
CASE WHEN expected > 0 THEN (observed/expected)*national_rate*population ELSE 0 END AS adjusted_count
FROM
	-- observed
	(SELECT indicator, year, lsoa01cd, SUM(indicator_value) AS observed FROM 
	HEPI_INDICATORS_LSOA 
	GROUP BY indicator, year, lsoa01cd) obs
INNER JOIN
	-- expected
	(SELECT indicator, year, lsoa01cd, SUM(adjusted_count) AS expected FROM
		(SELECT indicator, pop.lsoa01cd, nat.year, nat.age_group, nat.sex, nat.national_rate*pop.lsoa_age_sex_pop AS adjusted_count FROM
		-- LSOA population
		(SELECT lsoa01cd, year, age_group, sex, indicator_value AS lsoa_age_sex_pop FROM 
		HEPI_INDICATORS_LSOA 
		WHERE indicator=''population'') pop
		INNER JOIN
		-- national rate
		(SELECT nc.indicator, nc.year, nc.age_group, nc.sex, national_count, national_pop, (national_count/national_pop) AS national_rate FROM
			(SELECT indicator, year, age_group, sex, SUM(indicator_value) AS national_count FROM 
			HEPI_INDICATORS_LSOA 
			GROUP BY indicator, year, age_group, sex) nc
			INNER JOIN
			(SELECT year, age_group, sex, SUM(indicator_value) AS national_pop FROM 
			HEPI_INDICATORS_LSOA 
			WHERE indicator=''population'' AND indicator_value>0
			GROUP BY year, age_group, sex) np
			ON nc.year = np.year AND nc.age_group=np.age_group AND nc.sex=np.sex) nat
		ON pop.year=nat.year AND pop.age_group=nat.age_group AND pop.sex=nat.sex) adj
	GROUP BY indicator, year, lsoa01cd) exp
ON obs.year=exp.year AND obs.indicator=exp.indicator AND obs.lsoa01cd=exp.lsoa01cd
INNER JOIN
	-- crude national rate
	(SELECT indicator, ind.year, (indicator_value/population) AS national_rate FROM
	(SELECT indicator, year, indicator_value FROM HEPI_INDICATORS
	WHERE age_group=''ALL'' AND sex=''A'' AND imd_group=''ALL'') ind
	INNER JOIN
	(SELECT year, indicator_value AS population FROM HEPI_INDICATORS
	WHERE age_group=''ALL'' AND sex=''A'' AND imd_group=''ALL'' AND indicator=''population'') pop
	ON ind.year=pop.year) nat
ON obs.year=nat.year AND obs.indicator=nat.indicator
INNER JOIN
	-- LSOA population
	(SELECT lsoa01cd, year, SUM(indicator_value) AS population FROM HEPI_INDICATORS_LSOA WHERE indicator=''population''GROUP BY year, lsoa01cd) pop
ON pop.year=obs.year AND pop.lsoa01cd=obs.lsoa01cd
INNER JOIN 
	(SELECT lsoa01cd, quintile AS imd_quintile FROM IMD_2010) imd
ON obs.lsoa01cd=imd.lsoa01cd';

EXECUTE IMMEDIATE 'CREATE TABLE HEPI_INDICATORS_ADJUSTED AS
SELECT indicator, year, imd_quintile, SUM(adjusted_count) AS indicator_value FROM
HEPI_INDICATORS_ADJUSTED_LSOA
GROUP BY indicator, year, imd_quintile';


END ADJUST_INDICATORS;



INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
'unplanned_hospitalisations_adjusted', 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
FROM hepi_indicators_adjusted_lsoa WHERE indicator='unplanned_hospitalisations' GROUP BY lsoa01cd, indicator, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd, indicator;

INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
'all_mortality_adjusted', 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
FROM hepi_indicators_adjusted_lsoa WHERE indicator='all_mortality' GROUP BY lsoa01cd, indicator, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd, indicator;

INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
'amenable_mortality_adjusted', 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
FROM hepi_indicators_adjusted_lsoa WHERE indicator='amenable_mortality' GROUP BY lsoa01cd, indicator, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd, indicator;


INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
'twelve_month_mortality_adjusted', 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
FROM hepi_indicators_adjusted_lsoa WHERE indicator='twelve_month_mortality' GROUP BY lsoa01cd, indicator, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd, indicator;


INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
'need_adjusted_population', 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, year, SUM(normalised_need_adjusted_pop) indicator_value 
FROM need_adjusted_population GROUP BY lsoa01cd, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd;


INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
'hospital_deaths', 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, indicator, year, SUM(adjusted_count) indicator_value 
FROM hepi_indicators_adjusted_lsoa WHERE indicator='hospital_deaths' GROUP BY lsoa01cd, indicator, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd, indicator;
