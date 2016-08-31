CREATE TABLE ONS_MORTALITY_WIDE ( 
death_id INTEGER,
DOD DATE,
sex VARCHAR2(1),
Underlying_cause VARCHAR2(4),
Mention_1 VARCHAR2(4),
Mention_2 VARCHAR2(4),
Mention_3 VARCHAR2(4),
Mention_4 VARCHAR2(4),
Mention_5 VARCHAR2(4),
Mention_6 VARCHAR2(4),
Mention_7 VARCHAR2(4),
Mention_8 VARCHAR2(4),
Mention_9 VARCHAR2(4),
Mention_10 VARCHAR2(4),
Mention_11 VARCHAR2(4),
Mention_12 VARCHAR2(4),
Mention_13 VARCHAR2(4),
Mention_14 VARCHAR2(4),
Mention_15 VARCHAR2(4),
lsoa01CD VARCHAR2(9),
lsoa11CD VARCHAR2(9),
age VARCHAR2(25),
min_age INTEGER,
max_age INTEGER,
hes_year INTEGER);

CREATE TABLE ONS_MORTALITY ( 
death_id INTEGER,
DOD DATE,
sex VARCHAR2(1),
lsoa01CD VARCHAR2(9),
lsoa11CD VARCHAR2(9),
min_age INTEGER,
max_age INTEGER,
hes_year INTEGER,
age_group VARCHAR2(10),
imd_quintile INTEGER);

CREATE TABLE ONS_MORTALITY_CAUSE ( 
death_id INTEGER,
cause VARCHAR2(4),
is_underlying NUMBER(1,0)
);

create or replace PROCEDURE NORMALISE_ONS_MORTALITY
AUTHID CURRENT_USER
AS 
BEGIN

INSERT INTO ONS_MORTALITY SELECT death_id, dod, sex, lsoa01CD, lsoa11CD, min_age, max_age, hes_year, 
CASE
WHEN max_age<5 THEN '0-4'
WHEN min_age>4 AND max_age<=15 THEN '5-15'
WHEN min_age>=15 AND max_age<=24 THEN '16-24'
WHEN min_age>=24 AND max_age<=39 THEN '25-39'
WHEN min_age>=39 AND max_age<=59 THEN '40-59'
WHEN min_age>=59 AND max_age<=74 THEN '60-74'
WHEN min_age>74 THEN '75+'
ELSE NULL
END age_group,
imd_quintile
FROM 
(SELECT mort.*, imd.quintile AS imd_quintile FROM ONS_MORTALITY_WIDE mort 
LEFT JOIN IMD_2010 imd
ON mort.lsoa01cd = imd.lsoa01cd);
 
INSERT INTO ONS_MORTALITY_CAUSE SELECT death_id, underlying_cause, 1 
FROM ONS_MORTALITY_WIDE
WHERE underlying_cause IS NOT NULL;

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_1 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_1 IS NOT NULL AND mention_1 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);


MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_2 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_2 IS NOT NULL AND mention_2 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_3 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_3 IS NOT NULL AND mention_3 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_4 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_4 IS NOT NULL AND mention_4 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_5 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_5 IS NOT NULL AND mention_5 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_6 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_6 IS NOT NULL AND mention_6 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_7 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_7 IS NOT NULL AND mention_7 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_8 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_8 IS NOT NULL AND mention_8 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_9 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_9 IS NOT NULL AND mention_9 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_10 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_10 IS NOT NULL AND mention_10 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_11 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_11 IS NOT NULL AND mention_11 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_12 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_12 IS NOT NULL AND mention_12 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_13 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_13 IS NOT NULL AND mention_13 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_14 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_14 IS NOT NULL AND mention_14 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

MERGE INTO ONS_MORTALITY_CAUSE a
USING (SELECT death_id, mention_15 as cause, 0 as is_underlying FROM ONS_MORTALITY_WIDE WHERE mention_15 IS NOT NULL AND mention_15 != underlying_cause) b
ON (a.death_id = b.death_id AND a.cause = b.cause)
WHEN NOT MATCHED THEN
  INSERT (death_id, cause, is_underlying)
  VALUES (b.death_id, b.cause, b.is_underlying);

 
END NORMALISE_ONS_MORTALITY;