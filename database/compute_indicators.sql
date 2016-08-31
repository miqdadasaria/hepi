create or replace PROCEDURE COMPUTE_INDICATOR(ind_num int)
AUTHID CURRENT_USER
AS
l_indicator_name varchar2(50);
BEGIN

CASE 
WHEN ind_num=1 THEN
  l_indicator_name := 'all_mortality';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, lsoa01cd, 
  'Q'||imd_quintile, 
  NVL(age_group,'U'), 
  NVL(sex,'U'), 
  COUNT(DISTINCT(mort.death_id)) 
  FROM 
  ONS_MORTALITY mort
  GROUP BY hes_year, sex, imd_quintile, age_group, lsoa01cd;
  COMMIT;
WHEN ind_num=2 THEN
  l_indicator_name := 'morbidity';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  year, 
  qof.lsoa01cd, 
  'Q'||quintile, 
  'ALL', 
  'A', 
  den+excnum 
  FROM
  QOF_LSOA qof
  INNER JOIN
  IMD_2010 imd
  ON qof.lsoa01cd = imd.lsoa01cd
  WHERE qmasname='smoke4';
  COMMIT;
WHEN ind_num=4 THEN
  l_indicator_name := 'gp_fte';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  year, 
  gp.lsoa01cd, 
  'Q'||quintile, 
  'ALL', 
  'A', 
  GP_FTE 
  FROM
  GP_LSOA_COUNT gp
  INNER JOIN
  IMD_2010 imd
  ON gp.lsoa01cd = imd.lsoa01cd;
  COMMIT;
WHEN ind_num=5 THEN
  l_indicator_name := 'PHIS';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  YEAR, 
  LSOA01CD, 
  'Q'||IMD_GROUP, 
  'ALL', 
  'A', 
  SUM(WEIGHT*POPA) AS INDICATOR_VALUE 
  FROM 
  (SELECT YEAR, LSOA01CD, INDNUM, WEIGHT, IMD_GROUP, NVL(SUM(NUM)/SUM(NVL(DEN,0) + NVL(EXCNUM,0)),0)*100 AS POPA FROM  
  (SELECT qof.*, phis.weight, imd.quintile AS IMD_GROUP FROM 
  QOF_LSOA qof
  INNER JOIN 
  PHIS_WEIGHTS phis
  ON qof.indnum = phis.indnum
  INNER JOIN 
  IMD_2010 imd
  ON qof.lsoa01cd = imd.lsoa01cd) x
  GROUP BY YEAR, INDNUM, WEIGHT, IMD_GROUP, LSOA01CD
  )
  GROUP BY YEAR, IMD_GROUP, LSOA01CD;
  COMMIT;
WHEN ind_num=6 THEN
  l_indicator_name := 'hospital_waiting_time';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  SUM(total_wait_days) 
  FROM 
  HES_WAITING_TIME w
  INNER JOIN IMD_2010 imd 
  ON w.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON w.extract_hesid = pat.extract_hesid
  WHERE total_wait_days IS NOT NULL
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=7 THEN  
  l_indicator_name := 'unplanned_hospitalisations';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  quintile, 
  age_group, 
  sex, 
  COUNT(DISTINCT(extract_hesid)) 
  FROM 
  (-- A
  SELECT av.extract_hesid, av.episode_number, sex, hes_year, soal, age_group, quintile
  FROM 
  HES_AVOIDABLE_HOSP av
  INNER JOIN
  HES_INPATIENT_EPISODE_DIAG d
  ON d.episode_number = av.episode_number
  AND d.is_primary = 1
  AND d.diag_code IN ('B180','B181')
  AND d.episode_number NOT IN (SELECT episode_number FROM HES_INPATIENT_EPISODE_DIAG WHERE diag_code LIKE 'D57%')
  
  UNION
  -- B, D, part of E, G, I
  SELECT av.extract_hesid, av.episode_number, sex, hes_year, soal, age_group, quintile
  FROM 
  HES_AVOIDABLE_HOSP av
  INNER JOIN
  HES_INPATIENT_EPISODE_DIAG d
  ON d.episode_number = av.episode_number
  WHERE 
  d.is_primary = 1
  AND(
  d.diag_code = 'J46X' 
  OR 
  d.diag_code LIKE 'J45%'
  OR
  d.diag_code LIKE 'E10%'
  OR
  d.diag_code LIKE 'E11%'
  OR
  d.diag_code LIKE 'E12%'
  OR
  d.diag_code LIKE 'E13%'
  OR
  d.diag_code LIKE 'E14%'
  OR
  d.diag_code LIKE 'J41%'
  OR
  d.diag_code LIKE 'J43%'
  OR
  d.diag_code LIKE 'J44%'
  OR
  d.diag_code = 'J42X'
  OR
  d.diag_code = 'J47X'
  OR
  d.diag_code LIKE 'D51%'
  OR
  d.diag_code LIKE 'D52%'
  OR
  d.diag_code = 'D501'
  OR
  d.diag_code = 'D508'
  OR
  d.diag_code = 'D509'
  OR
  d.diag_code LIKE 'G40%'
  OR
  d.diag_code LIKE 'G41%'
  OR
  d.diag_code LIKE 'I48%'
  OR
  d.diag_code LIKE 'F00%'
  OR
  d.diag_code LIKE 'F01%'
  OR
  d.diag_code LIKE 'F02%'
  OR
  d.diag_code LIKE 'F03%'
  )
  
  UNION
  -- C, H
  SELECT av.extract_hesid, av.episode_number, sex, hes_year, soal, age_group, quintile
  FROM 
  HES_AVOIDABLE_HOSP av
  INNER JOIN
  HES_INPATIENT_EPISODE_DIAG d
  ON d.episode_number = av.episode_number
  WHERE 
  d.is_primary = 1 
  AND
  (d.diag_code IN ('I110','J81X','I130','I10X','I119')
  OR d.diag_code LIKE 'J45%'
  )
  AND
  d.episode_number NOT IN 
  (SELECT episode_number FROM HES_INPATIENT_EPISODE_PROC WHERE substr(procedure_code,1,3) IN
  ('K00','K01','K02','K03','K04','K50','K52','K55','K56','K57','K60','K61','K66','K67','K68','K69','K71'))
  
  UNION
  -- E
  SELECT av.extract_hesid, av.episode_number, sex, hes_year, soal, age_group, quintile
  FROM 
  HES_AVOIDABLE_HOSP av
  INNER JOIN
  HES_INPATIENT_EPISODE_DIAG d
  ON d.episode_number = av.episode_number
  WHERE 
  d.is_primary = 1 
  AND d.diag_code LIKE 'J20%'
  AND d.episode_number IN (
  SELECT episode_number FROM HES_INPATIENT_EPISODE_DIAG
  WHERE diag_code LIKE 'J41%'
  OR 
  diag_code LIKE 'J42%'
  OR 
  diag_code LIKE 'J43%'
  OR 
  diag_code LIKE 'J44%'
  OR 
  diag_code LIKE 'J47%'
  )
  
  UNION
  -- F
  SELECT av.extract_hesid, av.episode_number, sex, hes_year, soal, age_group, quintile
  FROM 
  HES_AVOIDABLE_HOSP av
  INNER JOIN
  HES_INPATIENT_EPISODE_DIAG d
  ON d.episode_number = av.episode_number
  WHERE 
  d.is_primary = 1 
  AND 
  (d.diag_code LIKE 'I20%'
  OR 
  d.diag_code LIKE 'I25%')
  AND d.episode_number NOT IN (
  SELECT episode_number FROM HES_INPATIENT_EPISODE_PROC
  WHERE 
  procedure_code LIKE 'A%'
  OR
  procedure_code LIKE 'B%'
  OR
  procedure_code LIKE 'C%'
  OR
  procedure_code LIKE 'D%'
  OR
  procedure_code LIKE 'E%'
  OR
  procedure_code LIKE 'F%'
  OR
  procedure_code LIKE 'G%'
  OR
  procedure_code LIKE 'H%'
  OR
  procedure_code LIKE 'I%'
  OR
  procedure_code LIKE 'J%'
  OR
  procedure_code LIKE 'K%'
  OR
  procedure_code LIKE 'L%'
  OR
  procedure_code LIKE 'M%'
  OR
  procedure_code LIKE 'N%'
  OR
  procedure_code LIKE 'O%'
  OR
  procedure_code LIKE 'P%'
  OR
  procedure_code LIKE 'Q%'
  OR
  procedure_code LIKE 'R%'
  OR
  procedure_code LIKE 'S%'
  OR
  procedure_code LIKE 'T%'
  OR
  procedure_code LIKE 'V%'
  OR
  procedure_code LIKE 'W%'
  OR
  procedure_code LIKE 'X0%'
  OR
  procedure_code LIKE 'X1%'
  OR
  procedure_code LIKE 'X2%'
  OR
  procedure_code LIKE 'X4%'
  OR
  procedure_code LIKE 'X5%'
  ))
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;  
WHEN ind_num=8 THEN
  l_indicator_name := 'excess_hospital_stay';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(DISTINCT(ep.extract_hesid)) 
  FROM 
  HES_INPATIENT_EPISODE ep
  INNER JOIN
  (SELECT epikey FROM HES_INPATIENT_GROUPER WHERE fce_hrg NOT LIKE 'UZ%' AND fceexcessbeddays > 0) g
  ON ep.epikey = g.epikey
  INNER JOIN IMD_2010 imd 
  ON ep.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON ep.extract_hesid = pat.extract_hesid
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=9 THEN
  l_indicator_name := 'twelve_month_mortality';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  m.soal, 
  'Q'||imd_quintile, 
  NVL(age_group,'U'), 
  sex,
  COUNT(DISTINCT(m.extract_hesid)) 
  FROM 
  HES_MORTALITY_OPTIM m
  INNER JOIN 
  (SELECT extract_hesid, MAX(disdate) disdate 
  FROM HES_INPATIENT_CIPS 
  GROUP BY extract_hesid) ip
  ON m.extract_hesid = ip.extract_hesid AND m.dod<=(ip.disdate+365) 
  WHERE m.imd_quintile IS NOT NULL
  GROUP BY hes_year, sex, imd_quintile, age_group, soal;
  COMMIT;
WHEN ind_num=10 THEN
  l_indicator_name := 'amenable_mortality';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  lsoa01cd, 
  'Q'||imd_quintile, 
  NVL(age_group,'U'), 
  NVL(sex,'U'), 
  COUNT(DISTINCT(mort.death_id)) 
  FROM 
  ONS_MORTALITY mort
  INNER JOIN ONS_MORTALITY_CAUSE cause
  ON mort.death_id = cause.death_id AND cause.is_underlying=1
  INNER JOIN AMENABLE_MORTALITY_CODES amen
  ON amen.icd10_code = cause.cause
  GROUP BY hes_year, sex, imd_quintile, age_group, lsoa01cd;
  COMMIT;
WHEN ind_num=11 THEN
  l_indicator_name := 'cips_count_ind';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(DISTINCT(ip.extract_hesid)) 
  FROM 
  (SELECT c.extract_hesid, c.soal, c.hes_year, a.age_group FROM
  HES_INPATIENT_CIPS c
  INNER JOIN
  CIPS_AGE a
  ON c.cips_number = a.cips_number) ip
  INNER JOIN IMD_2010 imd 
  ON ip.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON ip.extract_hesid = pat.extract_hesid
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=12 THEN
  l_indicator_name := 'population';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '0-4', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE <5
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '5-15', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 5 AND 15
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '16-24', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 16 AND 24
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '25-39', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 25 AND 39
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '40-59', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 40 AND 59
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '60-74', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 60 AND 74
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '75+', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE >= 75
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  COMMIT;
WHEN ind_num=13 THEN
  l_indicator_name := 'cips_count_ind_wait';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(w.extract_hesid) 
  FROM 
  HES_WAITING_TIME w
  INNER JOIN IMD_2010 imd 
  ON w.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON w.extract_hesid = pat.extract_hesid
  WHERE total_wait_days IS NOT NULL
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=14 THEN
  l_indicator_name := 'cips_count_ind_group';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(DISTINCT(ep.extract_hesid)) 
  FROM 
  HES_INPATIENT_EPISODE ep
  INNER JOIN
  (SELECT epikey FROM HES_INPATIENT_GROUPER WHERE fce_hrg NOT LIKE 'UZ%') g
  ON ep.epikey = g.epikey
  INNER JOIN IMD_2010 imd 
  ON ep.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON ep.extract_hesid = pat.extract_hesid
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=15 THEN
  l_indicator_name := 'gp_op_waiting_time';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  SUM(GP_OP_DAYS) 
  FROM 
  HES_WAITING_TIME w
  INNER JOIN IMD_2010 imd 
  ON w.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON w.extract_hesid = pat.extract_hesid
  WHERE total_wait_days IS NOT NULL
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=16 THEN
  l_indicator_name := 'op_ref_waiting_time';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  SUM(op_ref_days) 
  FROM 
  HES_WAITING_TIME w
  INNER JOIN IMD_2010 imd 
  ON w.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON w.extract_hesid = pat.extract_hesid
  WHERE total_wait_days IS NOT NULL
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=17 THEN
  l_indicator_name := 'ref_ip_waiting_time';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  SUM(ref_ip_days) 
  FROM 
  HES_WAITING_TIME w
  INNER JOIN IMD_2010 imd 
  ON w.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON w.extract_hesid = pat.extract_hesid
  WHERE total_wait_days IS NOT NULL
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=18 THEN
  l_indicator_name := 'hospital_deaths';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name,
  HES_YEAR, 
  SOAL,
  'Q'||QUINTILE, 
  AGE_GROUP, 
  CASE pat.SEX WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(cips_number) hospital_deaths 
  FROM 
  (SELECT cips.cips_number, cips.extract_hesid, cips.dismeth, cips.soal, cips.hes_year, a.age_group FROM
  HES_INPATIENT_CIPS cips
  INNER JOIN
  CIPS_AGE a
  ON cips.cips_number = a.cips_number) c
  INNER JOIN
  IMD_2010 imd
  ON c.SOAL = imd.LSOA01CD
  INNER JOIN 
  HES_PATIENT pat
  ON c.EXTRACT_HESID = pat.EXTRACT_HESID
  WHERE c.dismeth=4
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=19 THEN
  l_indicator_name := 'inpatient_wait';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name,
  HES_YEAR,
  SOAL,
  'Q'||quintile,
  NVL(age_group,'U'),
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  SUM(elecdur) 
  FROM 
  (SELECT cips.cips_number, cips.extract_hesid, cips.elecdur, cips.admimeth, cips.soal, cips.hes_year, a.age_group FROM
  HES_INPATIENT_CIPS cips
  INNER JOIN
  CIPS_AGE a
  ON cips.cips_number = a.cips_number) c
  INNER JOIN IMD_2010 imd 
  ON c.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON c.extract_hesid = pat.extract_hesid
  WHERE 
  NVL(c.elecdur,366) < 366 AND c.admimeth BETWEEN 11 and 12
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=20 THEN
  l_indicator_name := 'cips_count_ind_inpatient_wait';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name,
  HES_YEAR,
  SOAL,
  'Q'||quintile,
  NVL(age_group,'U'),
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(DISTINCT(c.extract_hesid)) 
  FROM 
  (SELECT cips.cips_number, cips.extract_hesid, cips.elecdur, cips.admimeth, cips.soal, cips.hes_year, a.age_group FROM
  HES_INPATIENT_CIPS cips
  INNER JOIN
  CIPS_AGE a
  ON cips.cips_number = a.cips_number) c
  INNER JOIN IMD_2010 imd 
  ON c.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON c.extract_hesid = pat.extract_hesid
  WHERE 
  NVL(elecdur,366) < 366 AND admimeth BETWEEN 11 and 12
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=21 THEN
  l_indicator_name := 'hosp_discharge_failure';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  lsoa01cd, 
  imd_group, 
  NVL(age_group,'U'), 
  sex, 
  COUNT(extract_hesid) 
  FROM 
  HOSPITAL_DISCHARGE_FAILURE
  WHERE 
  discharge_failure = 1
  GROUP BY hes_year, sex, imd_group, age_group, lsoa01cd;
  COMMIT;
WHEN ind_num=22 THEN
  l_indicator_name := 'hosp_discharge_failure_no_death';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  lsoa01cd, 
  imd_group, 
  NVL(age_group,'U'), 
  sex, 
  COUNT(extract_hesid) 
  FROM 
  HOSPITAL_DISCHARGE_FAILURE
  WHERE 
  NVL(MAX_EMERGENCY_DATE,FIRST_HOSP) > FIRST_HOSP
  GROUP BY hes_year, sex, imd_group, age_group, lsoa01cd;
  COMMIT;
WHEN ind_num=23 THEN
  l_indicator_name := 'PHIS_REPORTED';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  YEAR, 
  LSOA01CD, 
  'Q'||IMD_GROUP, 
  'ALL', 
  'A', 
  SUM(WEIGHT*REPA) AS INDICATOR_VALUE 
  FROM 
  (SELECT YEAR, LSOA01CD, INDNUM, WEIGHT, IMD_GROUP, NVL(SUM(NUM)/SUM(NVL(DEN,0)),0)*100 AS REPA FROM  
  (SELECT qof.*, phis.weight, imd.quintile AS IMD_GROUP FROM 
  QOF_LSOA qof
  INNER JOIN 
  PHIS_WEIGHTS phis
  ON qof.indnum = phis.indnum
  INNER JOIN 
  IMD_2010 imd
  ON qof.lsoa01cd = imd.lsoa01cd) x
  GROUP BY YEAR, INDNUM, WEIGHT, IMD_GROUP, LSOA01CD
  )
  GROUP BY YEAR, IMD_GROUP, LSOA01CD;
  COMMIT;
WHEN ind_num=24 THEN
  l_indicator_name := 'population_under_75';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '0-4', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE <5
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '5-15', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 5 AND 15
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '16-24', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 16 AND 24
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '25-39', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 25 AND 39
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '40-59', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 40 AND 59
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT l_indicator_name, YEAR, POP.LSOA01CD, 'Q'||TO_CHAR(QUINTILE), '60-74', SEX, SUM(POPULATION)
  FROM
  ONS_POPULATION POP
  INNER JOIN 
  IMD_2010 IMD
  ON POP.LSOA01CD=IMD.LSOA01CD AND POP.AGE BETWEEN 60 AND 74
  GROUP BY YEAR, QUINTILE, SEX, POP.LSOA01CD;
  COMMIT;
WHEN ind_num=25 THEN
  l_indicator_name := 'chd_mortality';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  lsoa01cd, 
  'Q'||imd_quintile, 
  NVL(age_group,'U'), 
  NVL(sex,'U'), 
  COUNT(DISTINCT(mort.death_id)) 
  FROM 
  ONS_MORTALITY mort
  INNER JOIN ONS_MORTALITY_CAUSE cause
  ON mort.death_id = cause.death_id AND cause.is_underlying=1
  AND cause.cause IN ('I20', 'I200', 'I201', 'I208', 'I209', 
  'I21', 'I210', 'I211', 'I212', 'I213', 'I214', 'I219',	
  'I22', 'I220', 'I221', 'I228', 'I229', 
  'I23', 'I230', 'I231', 'I232', 'I233', 'I234', 'I235', 'I236', 'I238',	
  'I24', 'I240', 'I241', 'I248', 'I249',	
  'I25', 'I250', 'I251', 'I252', 'I253', 'I254', 'I255', 'I256', 'I258', 'I259')
  AND mort.max_age < 75
  GROUP BY hes_year, sex, imd_quintile, age_group, lsoa01cd;
  COMMIT;
WHEN ind_num=26 THEN
  l_indicator_name := 'diabetes_mortality';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  lsoa01cd, 
  'Q'||imd_quintile, 
  NVL(age_group,'U'), 
  NVL(sex,'U'), 
  COUNT(DISTINCT(mort.death_id)) 
  FROM 
  ONS_MORTALITY mort
  INNER JOIN ONS_MORTALITY_CAUSE cause
  ON mort.death_id = cause.death_id AND cause.is_underlying=1
  AND cause.cause IN ('E10',	'E100',	'E101',	'E102',	'E103',	'E104',	'E105',	'E106',	'E107',	'E108',	'E109',	
'E11',	'E110',	'E111',	'E112',	'E113',	'E114',	'E115',	'E116',	'E117',	'E118',	'E119',	
'E12',	'E120',	'E121',	'E122',	'E123',	'E124',	'E125',	'E126',	'E127',	'E128',	'E129',	
'E13',	'E130',	'E131',	'E132',	'E133',	'E134',	'E135',	'E136',	'E137',	'E138',	'E139',	
'E14',	'E140',	'E141',	'E142',	'E143',	'E144',	'E145',	'E146',	'E147',	'E148',	'E149')
  AND mort.max_age < 75
  GROUP BY hes_year, sex, imd_quintile, age_group, lsoa01cd;
  COMMIT;
WHEN ind_num=27 THEN
  l_indicator_name := 'chd_emergency_hosp';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(DISTINCT(ip.extract_hesid)) 
  FROM 
  (SELECT c.extract_hesid, c.soal, c.hes_year, a.age_group FROM
  HES_INPATIENT_CIPS c
  INNER JOIN
  CIPS_AGE a
  ON c.cips_number = a.cips_number
  AND c.admimeth BETWEEN 21 AND 28
  INNER JOIN HES_INPATIENT_EPISODE e
  ON c.cips_number = e.cips_number
  INNER JOIN HES_INPATIENT_EPISODE_DIAG d
  ON d.episode_number = e.episode_number AND d.is_primary=1 
  AND d.diag_code IN ('I20', 'I200', 'I201', 'I208', 'I209', 
  'I21', 'I210', 'I211', 'I212', 'I213', 'I214', 'I219',	
  'I22', 'I220', 'I221', 'I228', 'I229', 
  'I23', 'I230', 'I231', 'I232', 'I233', 'I234', 'I235', 'I236', 'I238',	
  'I24', 'I240', 'I241', 'I248', 'I249',	
  'I25', 'I250', 'I251', 'I252', 'I253', 'I254', 'I255', 'I256', 'I258', 'I259')
  ) ip
  INNER JOIN IMD_2010 imd 
  ON ip.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON ip.extract_hesid = pat.extract_hesid
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
WHEN ind_num=28 THEN
  l_indicator_name := 'diabetes_emergency_hosp';
  INSERT INTO HEPI_INDICATORS_LSOA
  SELECT 
  l_indicator_name, 
  hes_year, 
  soal, 
  'Q'||quintile, 
  NVL(age_group,'U'), 
  CASE sex WHEN 1 THEN 'M' WHEN 2 THEN 'F' ELSE 'U' END, 
  COUNT(DISTINCT(ip.extract_hesid)) 
  FROM 
  (SELECT c.extract_hesid, c.soal, c.hes_year, a.age_group FROM
  HES_INPATIENT_CIPS c
  INNER JOIN
  CIPS_AGE a
  ON c.cips_number = a.cips_number
  AND c.admimeth BETWEEN 21 AND 28
  INNER JOIN HES_INPATIENT_EPISODE e
  ON c.cips_number = e.cips_number
  INNER JOIN HES_INPATIENT_EPISODE_DIAG d
  ON d.episode_number = e.episode_number AND d.is_primary=1 
  AND d.diag_code IN ('E100', 'E101', 'E107', 'E108', 'E109',
'E110', 'E111', 'E117', 'E118', 'E119',
'E120', 'E121', 'E127', 'E128', 'E129',
'E130', 'E131', 'E137', 'E138', 'E139',
'E140', 'E141', 'E147', 'E148', 'E149',
'E162')
  ) ip
  INNER JOIN IMD_2010 imd 
  ON ip.soal = imd.lsoa01cd
  INNER JOIN HES_PATIENT pat
  ON ip.extract_hesid = pat.extract_hesid
  GROUP BY hes_year, sex, quintile, age_group, soal;
  COMMIT;
ELSE
  RETURN;
END CASE;

-- add in zero counts
MERGE INTO HEPI_INDICATORS_LSOA u
  USING (
    SELECT l_indicator_name AS indicator, year, lsoa01cd, 'Q'||quintile as imd_group, age_group, sex, 0 as indicator_value FROM 
    HEPI_INDICATORS_YEARS_LKUP y
    CROSS JOIN
    HEPI_INDICATORS_AGE_GROUP_LKUP a
    CROSS JOIN
    (
    SELECT 'M' AS SEX FROM DUAL
    UNION ALL
    SELECT 'F' AS SEX FROM DUAL
    UNION ALL
    SELECT 'U' AS SEX FROM DUAL
  ) s
  CROSS JOIN
  (SELECT lsoa01cd, quintile FROM IMD_2010) imd
  ) v
ON (u.indicator = v.indicator AND u.year = v.year AND u.lsoa01cd = v.lsoa01cd AND u.imd_group = v.imd_group AND u.age_group = v.age_group AND u.sex = v.sex)
  -- if matched then ignore
WHEN NOT MATCHED THEN
  INSERT (indicator, year, lsoa01cd, imd_group, age_group, sex, indicator_value)
  VALUES (v.indicator, v.year, v.lsoa01cd, v.imd_group, v.age_group, v.sex, v.indicator_value);
COMMIT;

-- create 2011 indicator
INSERT INTO HEPI_INDICATORS_LSOA_2011
SELECT 
lsoa11cd, 
indicator, 
year, 
SUM(lkup.weight_2001_TO_2011*indicator_value) indicator_value 
FROM 
(SELECT lsoa01cd, indicator, year, SUM(indicator_value) indicator_value 
FROM hepi_indicators_lsoa WHERE indicator=l_indicator_name GROUP BY lsoa01cd, indicator, year) ind
INNER JOIN
LSOA_2001_2011_LOOKUP lkup
ON ind.lsoa01cd=lkup.lsoa01cd
GROUP BY year, lsoa11cd, indicator;
COMMIT;

-- create aggregated indices
INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name, YEAR, IMD_GROUP, AGE_GROUP, SEX, SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name
GROUP BY YEAR, IMD_GROUP, AGE_GROUP, SEX;
COMMIT;

INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name,  YEAR, 'ALL', AGE_GROUP, SEX, SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name 
GROUP BY YEAR, AGE_GROUP, SEX;
COMMIT;

INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name, YEAR, IMD_GROUP, AGE_GROUP, 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name 
GROUP BY YEAR, AGE_GROUP, IMD_GROUP;
COMMIT;

INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name, YEAR, IMD_GROUP, 'ALL', SEX, SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name
GROUP BY YEAR, SEX, IMD_GROUP;
COMMIT;

INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name,  YEAR, 'ALL', 'ALL', SEX, SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name
GROUP BY YEAR, SEX;
COMMIT;

INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name, YEAR, 'ALL', AGE_GROUP, 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name
GROUP BY YEAR, AGE_GROUP;

INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name, YEAR, IMD_GROUP, 'ALL', 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name
GROUP BY YEAR, IMD_GROUP;

INSERT INTO HEPI_INDICATORS
SELECT l_indicator_name, YEAR, 'ALL', 'ALL', 'A', SUM(INDICATOR_VALUE) FROM
HEPI_INDICATORS_LSOA WHERE
INDICATOR = l_indicator_name
GROUP BY YEAR;

END COMPUTE_INDICATOR;