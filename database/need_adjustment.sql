CREATE TABLE NEED_ADJUSTED_POPULATION AS 
SELECT year, lsoa01cd, SUM(population) AS population, SUM(need_adjusted_population) AS need_adjusted_population, imd_decile, imd_quintile
FROM
(SELECT pop.year, pop.lsoa01cd, pop.pop_subgroup, pop.population, 
pop.population*w.mean_weight*POWER(1.054,imd.health_score) AS need_adjusted_population, 
imd.quintile AS imd_quintile, imd.decile AS imd_decile 
FROM (
(SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_m' AS pop_subgroup FROM ONS_POPULATION WHERE age < 5 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 5 AND 14  AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 15 AND 44 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 45 AND 64 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 65 AND 74 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 75 AND 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_m' AS pop_subgroup FROM ONS_POPULATION WHERE age > 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_f' AS pop_subgroup FROM ONS_POPULATION WHERE age < 5 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 5 AND 14  AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 15 AND 44 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 45 AND 64 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 65 AND 74 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 75 AND 84 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_f' AS pop_subgroup FROM ONS_POPULATION WHERE age > 84 AND sex='F' GROUP BY year, lsoa01cd) pop
INNER JOIN
(SELECT lsoa01cd, health_score, quintile, decile FROM IMD_2010) imd
ON pop.lsoa01cd = imd.lsoa01cd
INNER JOIN 
NEED_ADJUSTMENT_WEIGHTS w
ON pop.pop_subgroup = w.category
))
GROUP BY year, lsoa01cd, imd_decile, imd_quintile;



INSERT INTO NEED_ADJUSTED_POPULATION 
SELECT year, lsoa01cd, SUM(population) AS population, SUM(need_adjusted_population) AS need_adjusted_population, imd_decile, imd_quintile
FROM
(SELECT pop.year, pop.lsoa01cd, pop.pop_subgroup, pop.population, 
pop.population*w.mean_weight*POWER(1.054,imd.health_score) AS need_adjusted_population, 
imd.quintile AS imd_quintile, imd.decile AS imd_decile 
FROM (
(SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age < 5 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 5 AND 14  AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 15 AND 44 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 45 AND 64 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 65 AND 74 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 75 AND 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age > 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age < 5 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 5 AND 14  AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 15 AND 44 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 45 AND 64 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 65 AND 74 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 75 AND 84 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age > 84 AND sex='F' GROUP BY year, lsoa01cd) pop
INNER JOIN
(SELECT lsoa01cd, health_score, quintile, decile FROM IMD_2010) imd
ON pop.lsoa01cd = imd.lsoa01cd
INNER JOIN 
NEED_ADJUSTMENT_WEIGHTS w
ON pop.pop_subgroup = w.category
))
GROUP BY year, lsoa01cd, imd_decile, imd_quintile;

ALTER TABLE NEED_ADJUSTED_POPULATION ADD (normalised_need_adjusted_pop NUMBER);


MERGE INTO NEED_ADJUSTED_POPULATION need
USING (
SELECT need_lsoa.year, need_lsoa.lsoa01cd, fac_year.normalising_factor FROM
((SELECT year, lsoa01cd FROM NEED_ADJUSTED_POPULATION) need_lsoa
INNER JOIN
(SELECT year, SUM(population)/SUM(need_adjusted_population) AS normalising_factor FROM NEED_ADJUSTED_POPULATION GROUP BY year) fac_year
ON need_lsoa.year = fac_year.year)) fac
ON (fac.lsoa01cd = need.lsoa01cd AND fac.year = need.year)
WHEN MATCHED THEN
   UPDATE SET 
   need.normalised_need_adjusted_pop = need.need_adjusted_population * fac.normalising_factor;




SELECT need.year, 
SUM(population) AS pop, 
ROUND(SUM(need_adjusted_population)) AS need_adjusted, 
ROUND(SUM(normalised_need_adjusted_pop)) AS normalised_need_adjusted,
ROUND(SUM(gp_head_count),2) AS gp_head_count, 
ROUND(SUM(gp_fte),2) AS gp_fte,
ROUND((SUM(gp_head_count)/SUM(population)) * 100000,2) AS hc_per100k_raw,
ROUND((SUM(gp_fte)/SUM(population)) * 100000,2) AS fte_per100k_raw,
ROUND((SUM(gp_head_count)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS hc_per100k_adj_normalised,
ROUND((SUM(gp_fte)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS fte_per100k_adj_normalised
FROM NEED_ADJUSTED_POPULATION need
LEFT JOIN GP_LSOA_COUNT gp
ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd
GROUP BY need.year ORDER BY need.year;


SELECT need.year, 
need.imd_quintile,
SUM(population) AS pop, 
ROUND(SUM(need_adjusted_population)) AS need_adjusted, 
ROUND(SUM(normalised_need_adjusted_pop)) AS normalised_need_adjusted,
ROUND(SUM(gp_head_count),2) AS gp_head_count, 
ROUND(SUM(gp_fte),2) AS gp_fte,
ROUND((SUM(gp_head_count)/SUM(population)) * 100000,2) AS hc_per100k_raw,
ROUND((SUM(gp_fte)/SUM(population)) * 100000,2) AS fte_per100k_raw,
ROUND((SUM(gp_head_count)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS hc_per100k_adj_normalised,
ROUND((SUM(gp_fte)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS fte_per100k_adj_normalised
FROM NEED_ADJUSTED_POPULATION need
LEFT JOIN GP_LSOA_COUNT gp
ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd
GROUP BY need.year, need.imd_quintile ORDER BY need.year, need.imd_quintile;

SELECT need.year, 
need.imd_decile,
SUM(population) AS pop, 
ROUND(SUM(need_adjusted_population)) AS need_adjusted, 
ROUND(SUM(normalised_need_adjusted_pop)) AS normalised_need_adjusted,
ROUND(SUM(gp_head_count),2) AS gp_head_count, 
ROUND(SUM(gp_fte),2) AS gp_fte,
ROUND((SUM(gp_head_count)/SUM(population)) * 100000,2) AS hc_per100k_raw,
ROUND((SUM(gp_fte)/SUM(population)) * 100000,2) AS fte_per100k_raw,
ROUND((SUM(gp_head_count)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS hc_per100k_adj_normalised,
ROUND((SUM(gp_fte)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS fte_per100k_adj_normalised
FROM NEED_ADJUSTED_POPULATION need
LEFT JOIN GP_LSOA_COUNT gp
ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd
GROUP BY need.year, need.imd_decile ORDER BY need.year, need.imd_decile;

SELECT need.year, 
'D0'||need.imd_decile AS IMD_GROUP,
'ALL' AS AGE_GROUP,
'ALL' AS SEX,
ROUND((SUM(gp_head_count)/SUM(population)) * 100000,2) AS hc_per100k_raw,
ROUND((SUM(gp_fte)/SUM(population)) * 100000,2) AS fte_per100k_raw,
ROUND((SUM(gp_head_count)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS hc_per100k_adj_normalised,
ROUND((SUM(gp_fte)/SUM(normalised_need_adjusted_pop)) * 100000,2) AS fte_per100k_adj_normalised
FROM NEED_ADJUSTED_POPULATION need
LEFT JOIN GP_LSOA_COUNT gp
ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd
WHERE need.year > 2003
GROUP BY need.year, need.imd_decile ORDER BY need.year, need.imd_decile;


CREATE GLOBAL TEMPORARY TABLE POPULATION_SUMMARY_TEMP(
  year NUMBER(4,0),
  lsoa01cd VARCHAR2(9),
  subgroup VARCHAR2(100),
  population NUMBER,
  imd_quintile NUMBER(1,0),
  imd_decile NUMBER (2,0),
  imd_health_score NUMBER
) ON COMMIT PRESERVE ROWS ;

CREATE GLOBAL TEMPORARY TABLE NEED_ADJUSTED_POPULATION_TEMP ON COMMIT PRESERVE ROWS 
AS SELECT * FROM NEED_ADJUSTED_POPULATION WHERE 1=2;

CREATE TABLE INDICATOR_GP_SUPPLY_RESULTS(
iteration INTEGER,
year NUMBER(4,0), 
imd_decile NUMBER(2,0),
pop NUMBER, 
need_adjusted_pop NUMBER, 
normalised_need_adjusted_pop NUMBER,
gp_head_count_total NUMBER, 
gp_fte_total NUMBER,
hc_per100k_raw NUMBER,
fte_per100k_raw NUMBER,
hc_per100k_adj_normalised NUMBER,
fte_per100k_adj_normalised NUMBER
) TABLESPACE HEPI;


CREATE OR REPLACE PROCEDURE INDICATOR_GP_SUPPLY(iterations IN INTEGER)
AUTHID CURRENT_USER
AS 
imd_health_weight NUMBER;
BEGIN

-- clean out results table
EXECUTE IMMEDIATE 'TRUNCATE TABLE INDICATOR_GP_SUPPLY_RESULTS';

-- clean out temp table
EXECUTE IMMEDIATE 'TRUNCATE TABLE POPULATION_SUMMARY_TEMP';

-- select population into categories into temp table
INSERT INTO POPULATION_SUMMARY_TEMP 
SELECT pop.year, pop.lsoa01cd, pop.pop_subgroup, pop.population, 
imd.quintile AS imd_quintile, imd.decile AS imd_decile, imd.health_score AS imd_health_score 
FROM (
(SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_m' AS pop_subgroup FROM ONS_POPULATION WHERE age < 5 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 5 AND 14  AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 15 AND 44 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 45 AND 64 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 65 AND 74 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_m' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 75 AND 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_m' AS pop_subgroup FROM ONS_POPULATION WHERE age > 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_f' AS pop_subgroup FROM ONS_POPULATION WHERE age < 5 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 5 AND 14  AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 15 AND 44 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 45 AND 64 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 65 AND 74 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_f' AS pop_subgroup FROM ONS_POPULATION WHERE age BETWEEN 75 AND 84 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_f' AS pop_subgroup FROM ONS_POPULATION WHERE age > 84 AND sex='F' GROUP BY year, lsoa01cd) pop
INNER JOIN
(SELECT lsoa01cd, health_score, quintile, decile FROM IMD_2010) imd
ON pop.lsoa01cd = imd.lsoa01cd);

INSERT INTO POPULATION_SUMMARY_TEMP 
SELECT pop.year, pop.lsoa01cd, pop.pop_subgroup, pop.population, 
imd.quintile AS imd_quintile, imd.decile AS imd_decile, imd.health_score AS imd_health_score 
FROM (
(SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age < 5 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 5 AND 14  AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 15 AND 44 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 45 AND 64 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 65 AND 74 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 75 AND 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_m' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age > 84 AND sex='M' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_0_4_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age < 5 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_5_14_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 5 AND 14  AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_15_44_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 15 AND 44 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_45_64_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 45 AND 64 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_65_74_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 65 AND 74 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_75_84_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age BETWEEN 75 AND 84 AND sex='F' GROUP BY year, lsoa01cd
UNION
SELECT year, lsoa01cd, SUM(population) AS population, 'pop_85plus_f' AS pop_subgroup FROM ONS_POPULATION_LSOA_2011_2001 WHERE age > 84 AND sex='F' GROUP BY year, lsoa01cd) pop
INNER JOIN
(SELECT lsoa01cd, health_score, quintile, decile FROM IMD_2010) imd
ON pop.lsoa01cd = imd.lsoa01cd);

dbms_random.seed(1234);

-- repeatedly select random weights calculate need adjusted population aggregate 
-- insert numbers for D1 and D10 into results table
FOR i IN 1..iterations
LOOP
    -- clean out temp table for next iteration
    EXECUTE IMMEDIATE 'TRUNCATE TABLE NEED_ADJUSTED_POPULATION_TEMP';
    -- generate random imd_health_score weight
    SELECT exp(ln(mean_weight) + dbms_random.normal*((ln(weight_uci)-ln(weight_lci))/2)) INTO imd_health_weight FROM NEED_ADJUSTMENT_WEIGHTS WHERE category = 'imd_health';
    
    INSERT INTO NEED_ADJUSTED_POPULATION_TEMP (year, lsoa01cd, population, need_adjusted_population, imd_decile, imd_quintile) 
    SELECT 
    year, 
    lsoa01cd, 
    SUM(population) AS population, 
    SUM(need_adjusted_population) AS need_adjusted_population, 
    imd_decile, 
    imd_quintile
    FROM
    (SELECT
    pop.year, 
    pop.lsoa01cd, 
    pop.population, 
    pop.population*w.random_weight*POWER(imd_health_weight,pop.imd_health_score) AS need_adjusted_population,
    pop.imd_decile,
    pop.imd_quintile
    FROM
    POPULATION_SUMMARY_TEMP pop
    INNER JOIN 
    (SELECT category, exp(ln(mean_weight) + dbms_random.normal*((ln(weight_uci)-ln(weight_lci))/2)) random_weight  FROM NEED_ADJUSTMENT_WEIGHTS) w
    ON pop.subgroup = w.category)
    GROUP BY year, lsoa01cd, imd_decile, imd_quintile;
    
    MERGE INTO NEED_ADJUSTED_POPULATION_TEMP need
    USING (
    SELECT need_lsoa.year, need_lsoa.lsoa01cd, fac_year.normalising_factor FROM
    ((SELECT year, lsoa01cd FROM NEED_ADJUSTED_POPULATION_TEMP) need_lsoa
    INNER JOIN
    (SELECT year, SUM(population)/SUM(need_adjusted_population) AS normalising_factor FROM NEED_ADJUSTED_POPULATION_TEMP GROUP BY year) fac_year
    ON need_lsoa.year = fac_year.year)) fac
    ON (fac.lsoa01cd = need.lsoa01cd AND fac.year = need.year)
    WHEN MATCHED THEN
       UPDATE SET 
       need.normalised_need_adjusted_pop = need.need_adjusted_population * fac.normalising_factor;
    
    INSERT INTO INDICATOR_GP_SUPPLY_RESULTS SELECT 
    i AS iteration,
    need.year, 
    need.imd_decile,
    SUM(population) AS pop, 
    ROUND(SUM(need_adjusted_population)) AS need_adjusted, 
    ROUND(SUM(normalised_need_adjusted_pop)) AS normalised_need_adjusted,
    ROUND(SUM(gp_head_count),2) AS gp_head_count, 
    ROUND(SUM(gp_fte),2) AS gp_fte,
    ROUND((SUM(gp_head_count)/SUM(population)) * 100000,2) AS hc_per100k_raw,
    ROUND((SUM(gp_fte)/SUM(population)) * 100000,2) AS fte_per100k_raw,
    SUM(gp_head_count)/SUM(normalised_need_adjusted_pop) * 100000 AS hc_per100k_adj_normalised,
    SUM(gp_fte)/SUM(normalised_need_adjusted_pop) * 100000 AS fte_per100k_adj_normalised
    FROM NEED_ADJUSTED_POPULATION_TEMP need
    LEFT JOIN GP_LSOA_COUNT gp
    ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd 
    WHERE need.imd_decile IN (1,10)
    GROUP BY need.year, need.imd_decile ORDER BY need.year, need.imd_decile;

END LOOP;

END INDICATOR_GP_SUPPLY;


-- assuming 1000 iterations

-- GP headcount median and confidence intervals
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY hc_per100k_adj_normalised) RN 
FROM INDICATOR_GP_SUPPLY_RESULTS g 
WHERE year > 2003 AND (RN = 25 OR RN = 500 OR RN=975)
ORDER BY decile, year, RN;

-- GP fte median and confidence intervals
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY fte_per100k_adj_normalised) RN 
FROM INDICATOR_GP_SUPPLY_RESULTS g 
WHERE year > 2003 AND (RN = 25 OR RN = 500 OR RN=975)
ORDER BY decile, year, RN;

ALTER TABLE NHS_GEOGRAPHY ADD (under_doctored NUMBER(1,0));
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 0;

UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE 'ASHTON, LEIGH%';
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE 'BARKING AND DAGENHAM';
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Barnsley');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Birmingham East and North');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Blackburn with Darwen%');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Blackpool');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Bolton%');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Calderdale');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Dudley');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Greenwich Teaching');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Halton and St Helens');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Hammersmith and Fulham');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Hartlepool');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Havering');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Heart of Birmingham Teaching');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Heywood, Middleton and Rochdale');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Hounslow');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Hull%');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Knowsley');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Leicester City');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Liverpool');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Luton');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Manchester%');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Medway');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Newcastle');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('North Lancashire%');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Nottingham City');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Oldham');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Redcar and Cleveland');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Salford');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Sandwell');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Sefton');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('South Tyneside');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Stoke on Trent');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Sunderland Teaching');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Tameside and Glossop');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Walsall Teaching');
UPDATE NHS_GEOGRAPHY SET UNDER_DOCTORED = 1 WHERE UPPER(PCO11NM) LIKE UPPER('Wolverhampton City');


SELECT need.year, 
geo.pco11cd,
geo.pco11nm,
geo.under_doctored,
ROUND(SUM(need.population * lkup.weight_2001_to_2011)) AS pop, 
ROUND(SUM(need_adjusted_population * lkup.weight_2001_to_2011)) AS need_adjusted, 
ROUND(SUM(normalised_need_adjusted_pop * lkup.weight_2001_to_2011)) AS normalised_need_adjusted,
ROUND(SUM(gp_head_count * lkup.weight_2001_to_2011),2) AS gp_head_count, 
ROUND(SUM(gp_fte * lkup.weight_2001_to_2011),2) AS gp_fte,
ROUND((SUM(gp_head_count * lkup.weight_2001_to_2011)/SUM(population * lkup.weight_2001_to_2011)) * 100000,2) AS hc_per100k_raw,
ROUND((SUM(gp_fte * lkup.weight_2001_to_2011)/SUM(population * lkup.weight_2001_to_2011)) * 100000,2) AS fte_per100k_raw,
ROUND((SUM(gp_head_count * lkup.weight_2001_to_2011)/SUM(normalised_need_adjusted_pop * lkup.weight_2001_to_2011)) * 100000,2) AS hc_per100k_adj_normalised,
ROUND((SUM(gp_fte * lkup.weight_2001_to_2011)/SUM(normalised_need_adjusted_pop * lkup.weight_2001_to_2011)) * 100000,2) AS fte_per100k_adj_normalised
FROM NEED_ADJUSTED_POPULATION need
LEFT JOIN GP_LSOA_COUNT gp
ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd
INNER JOIN LSOA_2001_2011_LOOKUP lkup
ON need.lsoa01cd = lkup.lsoa01cd
INNER JOIN NHS_GEOGRAPHY geo
ON lkup.lsoa11cd = geo.lsoa11cd
WHERE need.year IN (2006,2011)
GROUP BY need.year, geo.pco11nm, geo.pco11cd, geo.under_doctored ORDER BY need.year, GEO.PCO11NM;


SELECT need.year, 
geo.pco11cd,
geo.pco11nm,
geo.under_doctored,
ROUND(SUM(need.population * lkup.weight_2001_to_2011)) AS pop, 
ROUND(SUM(need_adjusted_population * lkup.weight_2001_to_2011)) AS need_adjusted, 
ROUND(SUM(normalised_need_adjusted_pop * lkup.weight_2001_to_2011)) AS normalised_need_adjusted,
ROUND(SUM(gp_head_count * lkup.weight_2001_to_2011),2) AS gp_head_count, 
ROUND(SUM(gp_fte * lkup.weight_2001_to_2011),2) AS gp_fte,
ROUND((SUM(gp_head_count * lkup.weight_2001_to_2011)/SUM(population * lkup.weight_2001_to_2011)) * 100000,2) AS hc_per100k_raw,
ROUND((SUM(gp_fte * lkup.weight_2001_to_2011)/SUM(population * lkup.weight_2001_to_2011)) * 100000,2) AS fte_per100k_raw,
ROUND((SUM(gp_head_count * lkup.weight_2001_to_2011)/SUM(normalised_need_adjusted_pop * lkup.weight_2001_to_2011)) * 100000,2) AS hc_per100k_adj_normalised,
ROUND((SUM(gp_fte * lkup.weight_2001_to_2011)/SUM(normalised_need_adjusted_pop * lkup.weight_2001_to_2011)) * 100000,2) AS fte_per100k_adj_normalised
FROM NEED_ADJUSTED_POPULATION need
INNER JOIN GP_LSOA_COUNT gp
ON gp.year = need.year AND gp.lsoa01cd = need.lsoa01cd
INNER JOIN LSOA_2001_2011_LOOKUP lkup
ON need.lsoa01cd = lkup.lsoa01cd
INNER JOIN NHS_GEOGRAPHY geo
ON lkup.lsoa11cd = geo.lsoa11cd
WHERE need.year IN (2006,2011) AND need.imd_decile = 1
GROUP BY need.year, geo.pco11nm, geo.pco11cd, geo.under_doctored ORDER BY need.year, GEO.PCO11NM;

-- GP hc median and confidence intervals
SELECT l.year, l.imd_decile, median, lci, uci FROM
(SELECT year, imd_decile, hc_per100k_adj_normalised AS LCI FROM (
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY hc_per100k_adj_normalised) RN FROM INDICATOR_GP_SUPPLY_RESULTS g)
WHERE year > 2003 AND RN = 9
ORDER BY imd_decile, year, RN) l
INNER JOIN 
(SELECT year, imd_decile, hc_per100k_adj_normalised AS median FROM (
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY hc_per100k_adj_normalised) RN FROM INDICATOR_GP_SUPPLY_RESULTS g)
WHERE year > 2003 AND RN = 178
ORDER BY imd_decile, year, RN) m
ON l.year = m.year AND l.imd_decile = m.imd_decile 
INNER JOIN 
(SELECT year, imd_decile, hc_per100k_adj_normalised AS UCI FROM (
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY hc_per100k_adj_normalised) RN FROM INDICATOR_GP_SUPPLY_RESULTS g)
WHERE year > 2003 AND RN = 347
ORDER BY imd_decile, year, RN) u
ON l.year = u.year AND l.imd_decile = u.imd_decile; 

-- GP fte median and confidence intervals
SELECT l.year, l.imd_decile, median, lci, uci FROM
(SELECT year, imd_decile, fte_per100k_adj_normalised AS LCI FROM (
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY fte_per100k_adj_normalised) RN FROM INDICATOR_GP_SUPPLY_RESULTS g)
WHERE year > 2003 AND RN = 9
ORDER BY imd_decile, year, RN) l
INNER JOIN 
(SELECT year, imd_decile, fte_per100k_adj_normalised AS median FROM (
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY fte_per100k_adj_normalised) RN FROM INDICATOR_GP_SUPPLY_RESULTS g)
WHERE year > 2003 AND RN = 178
ORDER BY imd_decile, year, RN) m
ON l.year = m.year AND l.imd_decile = m.imd_decile 
INNER JOIN 
(SELECT year, imd_decile, fte_per100k_adj_normalised AS UCI FROM (
SELECT g.*, ROW_NUMBER() OVER (PARTITION BY year, imd_decile ORDER BY fte_per100k_adj_normalised) RN FROM INDICATOR_GP_SUPPLY_RESULTS g)
WHERE year > 2003 AND RN = 347
ORDER BY imd_decile, year, RN) u
ON l.year = u.year AND l.imd_decile = u.imd_decile; 


create table nuffield_pbra_2013(
prac_code VARCHAR2(10),
need_adjusted_pop NUMBER,
renormalised_need_adjusted_pop NUMBER
);