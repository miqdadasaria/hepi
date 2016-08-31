CREATE TABLE GMS_GP(
year NUMBER(4,0),
gpgmc VARCHAR2(20),
gp_type VARCHAR2(50),
sex_code VARCHAR2(1),
age NUMBER(3,0),
prac_code VARCHAR2(10),
contract VARCHAR2(50),
sessions NUMBER,
hours VARCHAR2(20),
fte NUMBER,
cqual VARCHAR2(2)
) TABLESPACE HEPI;

CREATE TABLE GMS_prac AS
SELECT g.year, g.prac_code, NVL(g.gp_head_count,0) gp_head_count, NVL(g.gp_fte,0) gp_fte, a.gp_head_count_all, a.gp_fte_all, NVL(reg.gp_head_count_reg,0) gp_head_count_reg, NVL(reg.gp_fte_reg,0) gp_fte_reg, NVL(ret.gp_head_count_ret,0) gp_head_count_ret, NVL(ret.gp_fte_ret,0) gp_fte_ret FROM
(SELECT year, prac_code, COUNT(gpgmc) gp_head_count_all, SUM(fte) gp_fte_all FROM GMS_GP GROUP BY year, prac_code) a
LEFT JOIN 
(SELECT year, prac_code, COUNT(gpgmc) gp_head_count, SUM(fte) gp_fte FROM GMS_GP WHERE gp_type NOT IN ('GP REG','GP RET') GROUP BY year, prac_code) g
ON a.year=g.year AND a.prac_code=g.prac_code
LEFT JOIN 
(SELECT year, prac_code, COUNT(gpgmc) gp_head_count_reg, SUM(fte) gp_fte_reg FROM GMS_GP WHERE gp_type = 'GP REG' GROUP BY year, prac_code) reg
ON reg.year=a.year AND reg.prac_code=a.prac_code
LEFT JOIN 
(SELECT year, prac_code, COUNT(gpgmc) gp_head_count_ret, SUM(fte) gp_fte_ret FROM GMS_GP WHERE gp_type = 'GP RET' GROUP BY year, prac_code) ret
ON ret.year=a.year AND ret.prac_code=a.prac_code;


CREATE TABLE gp_lsoa_count AS
SELECT year, lsoa AS lsoa01cd, gp_head_count, gp_fte, gp_head_count_reg, gp_fte_reg, gp_head_count_ret, gp_fte_ret, gp_head_count_all, gp_fte_all
FROM (
SELECT 
a.year, 
a.lsoa, 
SUM(a.lsoa_prop*g.gp_head_count) gp_head_count, 
SUM(a.lsoa_prop*g.gp_fte) gp_fte, 
SUM(a.lsoa_prop*g.gp_head_count_reg) gp_head_count_reg, 
SUM(a.lsoa_prop*g.gp_fte_reg) gp_fte_reg, 
SUM(a.lsoa_prop*g.gp_head_count_ret) gp_head_count_ret, 
SUM(a.lsoa_prop*g.gp_fte_ret) gp_fte_ret, 
SUM(a.lsoa_prop*g.gp_head_count_all) gp_head_count_all, 
SUM(a.lsoa_prop*g.gp_fte_all) gp_fte_all 
FROM
(SELECT year, prcode, lsoa, lsoa_prop FROM ads_proportions) a
LEFT JOIN
(SELECT year, prac_code, gp_head_count, gp_fte, gp_head_count_all, gp_fte_all, gp_head_count_reg, gp_fte_reg, gp_head_count_ret, gp_fte_ret FROM gms_prac) g
ON a.prcode=g.prac_code AND a.year=g.year
GROUP BY a.year, a.lsoa
);


CREATE TABLE gp_practice_lookup (
	prac_code VARCHAR2(6),
	ccg13cdh VARCHAR2(3),
	name VARCHAR2(255)
) TABLESPACE HEPI;


CREATE TABLE gp_practice_fingertips (
	prac_code VARCHAR2(6),
	IMD_score_2011 NUMBER,
	IDACI_2010 NUMBER,
	DDAOPI_2010 NUMBER,
	perc_recommend_2010 NUMBER,
	perc_same_day_2011 NUMBER,
	perc_logstanding_illness_2011 NUMBER,
	perc_health_prob_2011 NUMBER,
	perc_carer_2011	NUMBER,
  	disab_per_100k NUMBER,
	perc_nursing_home_2010 NUMBER,
	perc_emp_or_student_2011 NUMBER,
	perc_unemp_2011 NUMBER,
	qof_points_2010 NUMBER,
	name VARCHAR2(255),
	address VARCHAR2(255),
	postcode VARCHAR2(10)
) TABLESPACE HEPI;

CREATE TABLE GMS_NURSE(
year NUMBER(4,0),
prac_code VARCHAR2(10),
nurse_hc NUMBER,
nurse_fte NUMBER
) TABLESPACE HEPI;

CREATE TABLE nurse_lsoa_count AS
SELECT year, lsoa AS lsoa01cd, nurse_hc, nurse_fte
FROM (
SELECT 
a.year, 
a.lsoa, 
SUM(a.lsoa_prop*g.nurse_hc) nurse_hc, 
SUM(a.lsoa_prop*g.nurse_fte) nurse_fte 
FROM
(SELECT year, prcode, lsoa, lsoa_prop FROM ads_proportions WHERE year=2013) a
LEFT JOIN
(SELECT year, prac_code, nurse_hc, nurse_fte FROM gms_nurse) g
ON a.prcode=g.prac_code AND a.year=g.year
GROUP BY a.year, a.lsoa
);

CREATE TABLE GMS_PCT_NURSE(
year NUMBER(4,0),
pct_code VARCHAR2(10),
nurse_hc NUMBER,
nurse_fte NUMBER
) TABLESPACE HEPI;

CREATE TABLE PCT_DETAILS AS
SELECT 
			year, 
			pct_code,
			pco11cd,
			imd_score,
			ROUND(SUM(pop)) AS pop, 
			ROUND(SUM(adjusted_pop)) AS adjusted_pop,
			ROUND(SUM(gp_fte),2) AS gp_fte,
			ROUND(SUM(NVL(nurse_fte, pct_nurse_fte)),2) AS nurse_fte
			FROM
			(SELECT
			lsoa.*, pct_nurse_fte FROM (
			SELECT    	need.year,
			pct.pct_code,
			pct.pco11cd,
			pct.imd_score,
			SUM(population * WEIGHT_2001_TO_2011) AS pop, 
			SUM(normalised_need_adjusted_pop * WEIGHT_2001_TO_2011) AS adjusted_pop,
			SUM(gp_fte * WEIGHT_2001_TO_2011) AS gp_fte,
			SUM(nurse_fte * WEIGHT_2001_TO_2011) AS nurse_fte
			FROM NEED_ADJUSTED_POPULATION need
			INNER JOIN GP_LSOA_COUNT gp
			ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd
			LEFT JOIN NURSE_LSOA_COUNT nurse
			ON nurse.year = need.year AND nurse.lsoa01cd = need.lsoa01cd
			INNER JOIN LSOA_2001_2011_LOOKUP lkup
			ON gp.lsoa01cd = lkup.lsoa01cd
			INNER JOIN NHS_GEOGRAPHY geo
			ON lkup.lsoa11cd = geo.lsoa11cd
			INNER JOIN PCO_IMD pct
			ON pct.pco11cd = geo.pco11cd
			GROUP BY need.year, pct.pco11cd, pct_code, pct.imd_score ORDER BY need.year, pct_code, pct.quintile) lsoa
			LEFT JOIN 
			(SELECT YEAR, PCT_CODE, NURSE_FTE AS PCT_NURSE_FTE FROM GMS_PCT_NURSE) pct_nurse
			ON pct_nurse.pct_code = lsoa.pct_code AND pct_nurse.year=lsoa.year)
			GROUP BY year, pct_code, pco11cd, imd_score;

ALTER TABLE PCT_DETAILS ADD imd_quintile INTEGER;
ALTER TABLE PCT_DETAILS ADD imd_decile INTEGER;


MERGE INTO PCT_DETAILS pct USING
(SELECT pct.year, pct.pct_code, 
CASE 
WHEN cum_pop/total_pop < 0.2 THEN '5'
WHEN cum_pop/total_pop >=0.2 AND cum_pop/total_pop <0.4 THEN '4'
WHEN cum_pop/total_pop >=0.4 AND cum_pop/total_pop <0.6 THEN '3'
WHEN cum_pop/total_pop >=0.6 AND cum_pop/total_pop <0.8 THEN '2'
WHEN cum_pop/total_pop >= 0.8 THEN '1'
END imd_quintile,
CASE 
WHEN cum_pop/total_pop < 0.1 THEN '10'
WHEN cum_pop/total_pop >=0.1 AND cum_pop/total_pop <0.2 THEN '9'
WHEN cum_pop/total_pop >=0.2 AND cum_pop/total_pop <0.3 THEN '8'
WHEN cum_pop/total_pop >=0.3 AND cum_pop/total_pop <0.4 THEN '7'
WHEN cum_pop/total_pop >=0.4 AND cum_pop/total_pop <0.5 THEN '6'
WHEN cum_pop/total_pop >=0.5 AND cum_pop/total_pop <0.6 THEN '5'
WHEN cum_pop/total_pop >=0.6 AND cum_pop/total_pop <0.7 THEN '4'
WHEN cum_pop/total_pop >=0.7 AND cum_pop/total_pop <0.8 THEN '3'
WHEN cum_pop/total_pop >=0.8 AND cum_pop/total_pop <0.9 THEN '2'
WHEN cum_pop/total_pop >= 0.9 THEN '1'
END imd_decile
FROM
(SELECT year, pct_code, SUM(pop) OVER (PARTITION BY year ORDER BY imd_score) cum_pop FROM pct_details) pct
INNER JOIN
(SELECT year, SUM(pop) total_pop from pct_details GROUP BY year) pop
ON pct.year = pop.year) imd
ON (pct.pct_code = imd.pct_code AND pct.year = imd.year)
WHEN MATCHED THEN
  UPDATE SET
    pct.imd_quintile = imd.imd_quintile,
    pct.imd_decile = imd.imd_decile;