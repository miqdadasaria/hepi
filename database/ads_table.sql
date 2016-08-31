CREATE TABLE ADS(
      year NUMBER(4,0),
      prcode VARCHAR2(10),
      lsoa VARCHAR2(10),
      m08a1 NUMBER,
      m08a2 NUMBER,
      m08a3 NUMBER,
      m08a4 NUMBER,
      m08a5 NUMBER,
      m08a6 NUMBER,
      m08a7 NUMBER,
      m08a8 NUMBER,
      m08a9 NUMBER,
      m08a10 NUMBER,
      m08a11 NUMBER,
      m08a12 NUMBER,
      m08a13 NUMBER,
      m08a14 NUMBER,
      m08a15 NUMBER,
      m08a16 NUMBER,
      m08a17 NUMBER,
      m08a18 NUMBER,
      f08a1 NUMBER,
      f08a2 NUMBER,
      f08a3 NUMBER,
      f08a4 NUMBER,
      f08a5 NUMBER,
      f08a6 NUMBER,
      f08a7 NUMBER,
      f08a8 NUMBER,
      f08a9 NUMBER,
      f08a10 NUMBER,
      f08a11 NUMBER,
      f08a12 NUMBER,
      f08a13 NUMBER,
      f08a14 NUMBER,
      f08a15 NUMBER,
      f08a16 NUMBER,
      f08a17 NUMBER,
      f08a18 NUMBER,
      total NUMBER
 ) TABLESPACE HEPI;

CREATE TABLE ADS_2013(
prcode VARCHAR2(10),
age NUMBER,
lsoa11cd VARCHAR2(10),
lsoa01cd VARCHAR2(10),
pop NUMBER
) TABLESPACE HEPI;

INSERT INTO ADS (year, prcode, lsoa, total) (SELECT 2013, prcode, lsoa01cd, SUM(pop) FROM ads_2013 GROUP BY prcode, lsoa01cd);

CREATE TABLE ADS_proportions AS
SELECT a.year, a.prcode, a.lsoa, a.total, b.prac_total, (a.total/b.prac_total) lsoa_prop, c.lsoa_total, (a.total/c.lsoa_total) prac_prop 
FROM
(SELECT year, prcode, lsoa, total FROM ADS WHERE LSOA LIKE 'E%') a 
INNER JOIN
(SELECT year, prcode, SUM(total) prac_total FROM ADS WHERE LSOA LIKE 'E%' GROUP BY year, prcode) b 
ON a.prcode=b.prcode AND a.year = b.year
INNER JOIN
(SELECT year, lsoa, SUM(total) lsoa_total FROM ADS WHERE LSOA LIKE 'E%' GROUP BY year, lsoa) c 
ON a.lsoa=c.lsoa AND a.year = c.year;


CREATE TABLE GP_PRAC_DETAILS AS
SELECT 
att.year,
att.prcode AS practice_code, 
name AS practice_name, 
geo.ccg13cdh,
ccg13cd,
ccg13nm,
gp_hc, 
gp_fte, 
gp_hc_ret, 
gp_fte_ret,
gp_hc_reg, 
gp_fte_reg,
gp_hc_all, 
gp_fte_all,
prac_total,
pop,
adjusted_pop,
imd_score
FROM 
(SELECT ads.year, ads.prcode, ads.prac_total, SUM(lsoa.pop*ads.prac_prop) AS pop, SUM(lsoa.adjusted_pop*ads.prac_prop) AS adjusted_pop, SUM(lsoa.imd_score*ads.lsoa_prop) AS imd_score
FROM
ADS_PROPORTIONS ads
INNER JOIN
(SELECT nap.year, nap.lsoa01cd, nap.population AS pop, nap.normalised_need_adjusted_pop AS adjusted_pop, imd.overall_score AS imd_score FROM 
NEED_ADJUSTED_POPULATION nap
INNER JOIN 
IMD_2010 imd
ON nap.lsoa01cd=imd.lsoa01cd) lsoa
ON ads.year=lsoa.year AND ads.lsoa=lsoa.lsoa01cd
GROUP BY ads.prcode, ads.prac_total, ads.year) att
LEFT JOIN 
(SELECT year, prac_code, gp_head_count AS gp_hc, gp_fte, gp_head_count_reg AS gp_hc_reg, gp_fte_reg, gp_head_count_ret AS gp_hc_ret, gp_fte_ret, gp_head_count_all AS gp_hc_all, gp_fte_all FROM GMS_PRAC) prac
ON att.prcode = prac.prac_code AND att.year=prac.year
LEFT JOIN
GP_PRACTICE_LOOKUP lkup
ON att.prcode=lkup.prac_code
LEFT JOIN 
(SELECT DISTINCT(ccg13cdh), ccg13cd, ccg13nm FROM NHS_GEOGRAPHY) geo
ON lkup.ccg13cdh=geo.ccg13cdh;

ALTER TABLE GP_PRAC_DETAILS ADD imd_quintile INTEGER;
ALTER TABLE GP_PRAC_DETAILS ADD imd_decile INTEGER;


MERGE INTO GP_PRAC_DETAILS gp USING
(SELECT gp_prac.year, gp_prac.practice_code, 
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
(SELECT year, practice_code, SUM(pop) OVER (PARTITION BY year ORDER BY imd_score) cum_pop FROM gp_prac_details) gp_prac
INNER JOIN
(SELECT year, SUM(pop) total_pop from gp_prac_details GROUP BY year) pop
ON gp_prac.year = pop.year) imd
ON (gp.practice_code = imd.practice_code AND gp.year = imd.year)
WHEN MATCHED THEN
  UPDATE SET
    gp.imd_quintile = imd.imd_quintile,
    gp.imd_decile = imd.imd_decile;


CREATE TABLE CCG_DETAILS AS
SELECT 
year,
ccg13cdh,
ccg13cd,
ccg13nm,
SUM(gp_hc) AS gp_hc,
SUM(gp_fte) AS gp_fte,
SUM(gp_hc_ret) AS gp_hc_ret, 
SUM(gp_fte_ret) AS gp_fte_ret,
SUM(gp_hc_reg) AS gp_hc_reg, 
SUM(gp_fte_reg) AS gp_fte_reg,
SUM(gp_hc_all) AS gp_hc_all, 
SUM(gp_fte_all) AS gp_fte_all,
SUM(prac_total) AS pop_prac,
SUM(pop) AS pop,
SUM(adjusted_pop) adjusted_pop,
SUM(pop*imd_score)/SUM(pop) imd_score
FROM GP_PRAC_DETAILS 
GROUP BY year, ccg13cdh, ccg13cd, ccg13nm;

ALTER TABLE CCG_DETAILS ADD imd_quintile INTEGER;
ALTER TABLE CCG_DETAILS ADD imd_decile INTEGER;


MERGE INTO CCG_DETAILS ccg USING
(SELECT ccg.year, ccg.ccg13cd, 
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
(SELECT year, ccg13cd, SUM(pop) OVER (PARTITION BY year ORDER BY imd_score) cum_pop FROM ccg_details) ccg
INNER JOIN
(SELECT year, SUM(pop) total_pop from ccg_details GROUP BY year) pop
ON ccg.year = pop.year) imd
ON (ccg.ccg13cd = imd.ccg13cd AND ccg.year = imd.year)
WHEN MATCHED THEN
  UPDATE SET
    ccg.imd_quintile = imd.imd_quintile,
    ccg.imd_decile = imd.imd_decile;

ALTER TABLE CCG_DETAILS ADD NORMALISED_RANK NUMBER;

MERGE INTO CCG_DETAILS ccg USING
(SELECT max_year.year, ccg13nm, cum_pop/max_pop normalised_rank FROM
((SELECT year, max(cum_pop) max_pop FROM (
SELECT year, ccg13nm, imd_score, 
sum(pop) OVER(PARTITION BY year ORDER BY imd_score DESC) cum_pop
FROM ccg_details)
GROUP BY YEAR) max_year
INNER JOIN
(SELECT year, ccg13nm, imd_score, 
sum(pop) OVER(PARTITION BY year ORDER BY imd_score DESC) cum_pop
FROM ccg_details) cum_year
ON max_year.year = cum_year.year)
ORDER BY year, normalised_rank) norm_rank
ON(ccg.year = norm_rank.year AND ccg.ccg13nm = norm_rank.ccg13nm)
WHEN MATCHED THEN
	UPDATE SET
	ccg.normalised_rank = norm_rank.normalised_rank;