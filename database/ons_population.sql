DROP TABLE ONS_POPULATION;
CREATE TABLE ONS_POPULATION(
	YEAR NUMBER(*,0), 
	SEX VARCHAR2(1 BYTE), 
	LSOA11CD VARCHAR2(9 BYTE), 
	ALL_AGES NUMBER(*,0), 
	AGE_0 NUMBER(*,0), 
	AGE_1 NUMBER(*,0), 
	AGE_2 NUMBER(*,0), 
	AGE_3 NUMBER(*,0), 
	AGE_4 NUMBER(*,0), 
	AGE_5 NUMBER(*,0), 
	AGE_6 NUMBER(*,0), 
	AGE_7 NUMBER(*,0), 
	AGE_8 NUMBER(*,0), 
	AGE_9 NUMBER(*,0), 
	AGE_10 NUMBER(*,0), 
	AGE_11 NUMBER(*,0), 
	AGE_12 NUMBER(*,0), 
	AGE_13 NUMBER(*,0), 
	AGE_14 NUMBER(*,0), 
	AGE_15 NUMBER(*,0), 
	AGE_16 NUMBER(*,0), 
	AGE_17 NUMBER(*,0), 
	AGE_18 NUMBER(*,0), 
	AGE_19 NUMBER(*,0), 
	AGE_20 NUMBER(*,0), 
	AGE_21 NUMBER(*,0), 
	AGE_22 NUMBER(*,0), 
	AGE_23 NUMBER(*,0), 
	AGE_24 NUMBER(*,0), 
	AGE_25 NUMBER(*,0), 
	AGE_26 NUMBER(*,0), 
	AGE_27 NUMBER(*,0), 
	AGE_28 NUMBER(*,0), 
	AGE_29 NUMBER(*,0), 
	AGE_30 NUMBER(*,0), 
	AGE_31 NUMBER(*,0), 
	AGE_32 NUMBER(*,0), 
	AGE_33 NUMBER(*,0), 
	AGE_34 NUMBER(*,0), 
	AGE_35 NUMBER(*,0), 
	AGE_36 NUMBER(*,0), 
	AGE_37 NUMBER(*,0), 
	AGE_38 NUMBER(*,0), 
	AGE_39 NUMBER(*,0), 
	AGE_40 NUMBER(*,0), 
	AGE_41 NUMBER(*,0), 
	AGE_42 NUMBER(*,0), 
	AGE_43 NUMBER(*,0), 
	AGE_44 NUMBER(*,0), 
	AGE_45 NUMBER(*,0), 
	AGE_46 NUMBER(*,0), 
	AGE_47 NUMBER(*,0), 
	AGE_48 NUMBER(*,0), 
	AGE_49 NUMBER(*,0), 
	AGE_50 NUMBER(*,0), 
	AGE_51 NUMBER(*,0), 
	AGE_52 NUMBER(*,0), 
	AGE_53 NUMBER(*,0), 
	AGE_54 NUMBER(*,0), 
	AGE_55 NUMBER(*,0), 
	AGE_56 NUMBER(*,0), 
	AGE_57 NUMBER(*,0), 
	AGE_58 NUMBER(*,0), 
	AGE_59 NUMBER(*,0), 
	AGE_60 NUMBER(*,0), 
	AGE_61 NUMBER(*,0), 
	AGE_62 NUMBER(*,0), 
	AGE_63 NUMBER(*,0), 
	AGE_64 NUMBER(*,0), 
	AGE_65 NUMBER(*,0), 
	AGE_66 NUMBER(*,0), 
	AGE_67 NUMBER(*,0), 
	AGE_68 NUMBER(*,0), 
	AGE_69 NUMBER(*,0), 
	AGE_70 NUMBER(*,0), 
	AGE_71 NUMBER(*,0), 
	AGE_72 NUMBER(*,0), 
	AGE_73 NUMBER(*,0), 
	AGE_74 NUMBER(*,0), 
	AGE_75 NUMBER(*,0), 
	AGE_76 NUMBER(*,0), 
	AGE_77 NUMBER(*,0), 
	AGE_78 NUMBER(*,0), 
	AGE_79 NUMBER(*,0), 
	AGE_80 NUMBER(*,0), 
	AGE_81 NUMBER(*,0), 
	AGE_82 NUMBER(*,0), 
	AGE_83 NUMBER(*,0), 
	AGE_84 NUMBER(*,0), 
	AGE_85 NUMBER(*,0), 
	AGE_86 NUMBER(*,0), 
	AGE_87 NUMBER(*,0), 
	AGE_88 NUMBER(*,0), 
	AGE_89 NUMBER(*,0), 
	AGE_90PLUS NUMBER(*,0)
   )TABLESPACE HEPI ;

CREATE TABLE ONS_POPULATION_SUMMARY(
	YEAR NUMBER(4,0),
	IMD_GROUP VARCHAR2(3),
	AGE_GROUP VARCHAR2(10),
	SEX VARCHAR(1),
	POPULATION NUMBER(*,0)
) TABLESPACE HEPI;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, 'D'||TO_CHAR(DECILE), '0to19', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA11CD=IMD.LSOA11CD AND POP.AGE <20
GROUP BY YEAR, DECILE, SEX;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, 'D'||TO_CHAR(DECILE), '20to74', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA11CD=IMD.LSOA11CD AND POP.AGE >19 AND POP.AGE <75
GROUP BY YEAR, DECILE, SEX;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, 'D'||TO_CHAR(DECILE), '75plus', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA11CD=IMD.LSOA11CD AND POP.AGE >74
GROUP BY YEAR, DECILE, SEX;






INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, 'ALL', AGE_GROUP, SEX, SUM(POPULATION) FROM
ONS_POPULATION_SUMMARY WHERE
SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL' AND IMD_GROUP NOT LIKE 'Q%'
GROUP BY AGE_GROUP, SEX, YEAR;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, IMD_GROUP, AGE_GROUP, 'A', SUM(POPULATION) FROM
ONS_POPULATION_SUMMARY WHERE
SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY AGE_GROUP, IMD_GROUP, YEAR;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, IMD_GROUP, 'ALL', SEX, SUM(POPULATION) FROM
ONS_POPULATION_SUMMARY WHERE
SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY SEX, IMD_GROUP, YEAR;


INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, 'ALL', 'ALL', SEX, SUM(POPULATION) FROM
ONS_POPULATION_SUMMARY WHERE
SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL' AND IMD_GROUP NOT LIKE 'Q%'
GROUP BY SEX, YEAR;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, 'ALL', AGE_GROUP, 'A', SUM(POPULATION) FROM
ONS_POPULATION_SUMMARY WHERE
SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL' AND IMD_GROUP NOT LIKE 'Q%'
GROUP BY AGE_GROUP, YEAR;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, IMD_GROUP, 'ALL', 'A', SUM(POPULATION) FROM
ONS_POPULATION_SUMMARY WHERE
SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY IMD_GROUP, YEAR;

INSERT INTO ONS_POPULATION_SUMMARY
SELECT YEAR, 'ALL', 'ALL', 'A', SUM(POPULATION) FROM
ONS_POPULATION_SUMMARY WHERE
SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL' AND IMD_GROUP NOT LIKE 'Q%'
GROUP BY YEAR;

COMMIT;

-- check numbers add up
-- totals
SELECT * FROM
(SELECT YEAR, POPULATION AS TOTAL FROM ONS_POPULATION_SUMMARY 
WHERE IMD_GROUP='ALL' AND AGE_GROUP='ALL' AND SEX='A') A
INNER JOIN
(SELECT YEAR, SUM(POPULATION) AS TOTAL_DECILE FROM ONS_POPULATION_SUMMARY
WHERE IMD_GROUP LIKE 'D%' AND AGE_GROUP<>'ALL' AND SEX<>'A'
GROUP BY YEAR) D
ON A.YEAR = D.YEAR
INNER JOIN
(SELECT YEAR, SUM(POPULATION) TOTAL_QUINTILE FROM ONS_POPULATION_SUMMARY
WHERE IMD_GROUP LIKE 'Q%' AND AGE_GROUP<>'ALL' AND SEX<>'A'
GROUP BY YEAR)Q
ON Q.YEAR = D.YEAR
ORDER BY A.YEAR;


CREATE TABLE "ONS_POPULATION_WIDE" 
   (	"YEAR" NUMBER(*,0), 
	"SEX" VARCHAR2(1 BYTE), 
	"LSOA01CD" VARCHAR2(9 BYTE), 
	"AGE_0" NUMBER(*,0), 
	"AGE_1" NUMBER(*,0), 
	"AGE_2" NUMBER(*,0), 
	"AGE_3" NUMBER(*,0), 
	"AGE_4" NUMBER(*,0), 
	"AGE_5" NUMBER(*,0), 
	"AGE_6" NUMBER(*,0), 
	"AGE_7" NUMBER(*,0), 
	"AGE_8" NUMBER(*,0), 
	"AGE_9" NUMBER(*,0), 
	"AGE_10" NUMBER(*,0), 
	"AGE_11" NUMBER(*,0), 
	"AGE_12" NUMBER(*,0), 
	"AGE_13" NUMBER(*,0), 
	"AGE_14" NUMBER(*,0), 
	"AGE_15" NUMBER(*,0), 
	"AGE_16" NUMBER(*,0), 
	"AGE_17" NUMBER(*,0), 
	"AGE_18" NUMBER(*,0), 
	"AGE_19" NUMBER(*,0), 
	"AGE_20" NUMBER(*,0), 
	"AGE_21" NUMBER(*,0), 
	"AGE_22" NUMBER(*,0), 
	"AGE_23" NUMBER(*,0), 
	"AGE_24" NUMBER(*,0), 
	"AGE_25" NUMBER(*,0), 
	"AGE_26" NUMBER(*,0), 
	"AGE_27" NUMBER(*,0), 
	"AGE_28" NUMBER(*,0), 
	"AGE_29" NUMBER(*,0), 
	"AGE_30" NUMBER(*,0), 
	"AGE_31" NUMBER(*,0), 
	"AGE_32" NUMBER(*,0), 
	"AGE_33" NUMBER(*,0), 
	"AGE_34" NUMBER(*,0), 
	"AGE_35" NUMBER(*,0), 
	"AGE_36" NUMBER(*,0), 
	"AGE_37" NUMBER(*,0), 
	"AGE_38" NUMBER(*,0), 
	"AGE_39" NUMBER(*,0), 
	"AGE_40" NUMBER(*,0), 
	"AGE_41" NUMBER(*,0), 
	"AGE_42" NUMBER(*,0), 
	"AGE_43" NUMBER(*,0), 
	"AGE_44" NUMBER(*,0), 
	"AGE_45" NUMBER(*,0), 
	"AGE_46" NUMBER(*,0), 
	"AGE_47" NUMBER(*,0), 
	"AGE_48" NUMBER(*,0), 
	"AGE_49" NUMBER(*,0), 
	"AGE_50" NUMBER(*,0), 
	"AGE_51" NUMBER(*,0), 
	"AGE_52" NUMBER(*,0), 
	"AGE_53" NUMBER(*,0), 
	"AGE_54" NUMBER(*,0), 
	"AGE_55" NUMBER(*,0), 
	"AGE_56" NUMBER(*,0), 
	"AGE_57" NUMBER(*,0), 
	"AGE_58" NUMBER(*,0), 
	"AGE_59" NUMBER(*,0), 
	"AGE_60" NUMBER(*,0), 
	"AGE_61" NUMBER(*,0), 
	"AGE_62" NUMBER(*,0), 
	"AGE_63" NUMBER(*,0), 
	"AGE_64" NUMBER(*,0), 
	"AGE_65" NUMBER(*,0), 
	"AGE_66" NUMBER(*,0), 
	"AGE_67" NUMBER(*,0), 
	"AGE_68" NUMBER(*,0), 
	"AGE_69" NUMBER(*,0), 
	"AGE_70" NUMBER(*,0), 
	"AGE_71" NUMBER(*,0), 
	"AGE_72" NUMBER(*,0), 
	"AGE_73" NUMBER(*,0), 
	"AGE_74" NUMBER(*,0), 
	"AGE_75" NUMBER(*,0), 
	"AGE_76" NUMBER(*,0), 
	"AGE_77" NUMBER(*,0), 
	"AGE_78" NUMBER(*,0), 
	"AGE_79" NUMBER(*,0), 
	"AGE_80" NUMBER(*,0), 
	"AGE_81" NUMBER(*,0), 
	"AGE_82" NUMBER(*,0), 
	"AGE_83" NUMBER(*,0), 
	"AGE_84" NUMBER(*,0), 
	"AGE_85plus" NUMBER(*,0)
  ) TABLESPACE HEPI; 

CREATE TABLE ONS_POPULATION_LSOA_2011_2001 AS
SELECT year, lsoa01cd, age, sex, SUM(population*weight_2011_to_2001) AS population 
FROM 
ONS_POPULATION_LSOA_2011 pop
INNER JOIN 
LSOA_2001_2011_LOOKUP lkup
ON pop.lsoa11cd = lkup.lsoa11cd AND pop.year > 2011
GROUP BY lkup.lsoa01cd, pop.year, pop.age, pop.sex;


create or replace PROCEDURE INDICATOR_POPULATION_SUMMARY
AUTHID CURRENT_USER
AS 
BEGIN

INSERT INTO HEPI_INDICATORS
SELECT 'population', YEAR, 'Q'||TO_CHAR(QUINTILE), '0to4', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE <5
GROUP BY YEAR, QUINTILE, SEX;

INSERT INTO HEPI_INDICATORS
SELECT 'population', YEAR, 'Q'||TO_CHAR(QUINTILE), '5to14', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE > 4 AND POP.AGE < 15
GROUP BY YEAR, QUINTILE, SEX;

INSERT INTO HEPI_INDICATORS
SELECT 'population', YEAR, 'Q'||TO_CHAR(QUINTILE), '15to29', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE > 14 AND POP.AGE < 30
GROUP BY YEAR, QUINTILE, SEX;

INSERT INTO HEPI_INDICATORS
SELECT 'population', YEAR, 'Q'||TO_CHAR(QUINTILE), '30to49', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE > 29 AND POP.AGE < 50
GROUP BY YEAR, QUINTILE, SEX;

INSERT INTO HEPI_INDICATORS
SELECT 'population', YEAR, 'Q'||TO_CHAR(QUINTILE), '50to74', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE > 49 AND POP.AGE < 75
GROUP BY YEAR, QUINTILE, SEX;

INSERT INTO HEPI_INDICATORS
SELECT 'population', YEAR, 'Q'||TO_CHAR(QUINTILE), '75plus', SEX, SUM(POPULATION)
FROM
ONS_POPULATION POP
INNER JOIN 
IMD_2010 IMD
ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE > 74
GROUP BY YEAR, QUINTILE, SEX;

-- create aggregated indices
INSERT INTO HEPI_INDICATORS
SELECT 'population',  year, 'ALL', AGE_GROUP, SEX, SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS WHERE
INDICATOR = 'population' AND SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY YEAR, AGE_GROUP, SEX;

INSERT INTO HEPI_INDICATORS
SELECT 'population', year, IMD_GROUP, AGE_GROUP, 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS WHERE
INDICATOR = 'population' AND SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY AGE_GROUP, IMD_GROUP;

INSERT INTO HEPI_INDICATORS
SELECT 'population', year, IMD_GROUP, 'ALL', SEX, SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS WHERE
INDICATOR = 'population' AND SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY SEX, IMD_GROUP;

INSERT INTO HEPI_INDICATORS
SELECT 'population',  year, 'ALL', 'ALL', SEX, SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS WHERE
INDICATOR = 'population' AND SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY SEX;

INSERT INTO HEPI_INDICATORS
SELECT 'population', year, 'ALL', AGE_GROUP, 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS WHERE
INDICATOR = 'population' AND SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY AGE_GROUP;

INSERT INTO HEPI_INDICATORS
SELECT 'population', year, IMD_GROUP, 'ALL', 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS WHERE
INDICATOR = 'population' AND SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL'
GROUP BY IMD_GROUP;

INSERT INTO HEPI_INDICATORS
SELECT 'population', year, 'ALL', 'ALL', 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS WHERE
INDICATOR = 'population' AND SEX<>'A' AND AGE_GROUP<>'ALL' AND IMD_GROUP<>'ALL';

END INDICATOR_POPULATION_SUMMARY;