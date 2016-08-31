CREATE TABLE HES_INPATIENT_LOAD_20XX(
admidate  DATE,
elecdate  DATE,
admimeth  INTEGER,
admisorc  INTEGER,
elecdur  INTEGER,
opdate_1  DATE,
opdate_2  DATE,
opdate_3  DATE,
opdate_4  DATE,
opdate_5  DATE,
opdate_6  DATE,
opdate_7  DATE,
opdate_8  DATE,
opdate_9  DATE,
opdate_10  DATE,
opdate_11  DATE,
opdate_12  DATE,
opdate_13  DATE,
opdate_14  DATE,
opdate_15  DATE,
opdate_16  DATE,
opdate_17  DATE,
opdate_18  DATE,
opdate_19  DATE,
opdate_20  DATE,
opdate_21  DATE,
opdate_22  DATE,
opdate_23  DATE,
opdate_24  DATE,
diag_1  varchar2(4),
diag_2  varchar2(4),
diag_3  varchar2(4),
diag_4  varchar2(4),
diag_5  varchar2(4),
diag_6  varchar2(4),
diag_7  varchar2(4),
diag_8  varchar2(4),
diag_9  varchar2(4),
diag_10  varchar2(4),
diag_11  varchar2(4),
diag_12  varchar2(4),
diag_13  varchar2(4),
diag_14  varchar2(4),
diag_15  varchar2(4),
diag_16  varchar2(4),
diag_17  varchar2(4),
diag_18  varchar2(4),
diag_19  varchar2(4),
diag_20  varchar2(4),
mainspef  varchar2(4),
opertn_1  varchar2(4),
opertn_2  varchar2(4),
opertn_3  varchar2(4),
opertn_4  varchar2(4),
opertn_5  varchar2(4),
opertn_6  varchar2(4),
opertn_7  varchar2(4),
opertn_8  varchar2(4),
opertn_9  varchar2(4),
opertn_10  varchar2(4),
opertn_11  varchar2(4),
opertn_12  varchar2(4),
opertn_13  varchar2(4),
opertn_14  varchar2(4),
opertn_15  varchar2(4),
opertn_16  varchar2(4),
opertn_17  varchar2(4),
opertn_18  varchar2(4),
opertn_19  varchar2(4),
opertn_20  varchar2(4),
opertn_21  varchar2(4),
opertn_22  varchar2(4),
opertn_23  varchar2(4),
opertn_24  varchar2(4),
operstat  INTEGER,
classpat  INTEGER,
posopdur  INTEGER,
preopdur  INTEGER,
tretspef  varchar2(4),
disdate  DATE,
disdest  INTEGER,
disreadydate  DATE,
dismeth  INTEGER,
epiend  DATE,
epistart  DATE,
epidur  INTEGER,
epiorder  INTEGER,
epistat  INTEGER,
epitype  INTEGER,
provspno  varchar2(12),
soal  varchar2(10),
sitetret  varchar2(5),
hrglate  varchar2(5),
hrglate35  varchar2(5),
hrg40  varchar2(5),
purcode  varchar2(6),
procode  varchar2(6),
protype  varchar2(200),
admincat  INTEGER,
dob_mmyyyy  DATE,
ethnos  varchar2(1),
extract_hesid  varchar2(32),
sex  INTEGER,
pconsult  varchar2(16),
preferer  varchar2(16),
referorg  varchar2(16),
admistat  INTEGER,
epikey  INTEGER
) TABLESPACE HEPI;

CREATE TABLE HES_INPATIENT_LOAD_2000_2011 AS
	SELECT t.*, 0 AS TRANSIT, 0 AS EPI_CHE, 0 AS SPELL_CHE, 0 AS CIPS_CHE, 
	CASE COALESCE(hrglate35,hrglate,hrg40,'x') WHEN 'U01' THEN 1 WHEN 'U02' THEN 2 WHEN 'U04' THEN 4 WHEN 'U05' THEN 5 WHEN 'U07' THEN 7 WHEN 'U09' THEN 9 ELSE 0 END AS U,
	(CASE MAINSPEF WHEN NULL THEN 1 ELSE 0 END + CASE TRETSPEF WHEN NULL THEN 1 ELSE 0 END + CASE OPERTN_1 WHEN NULL THEN 1 ELSE 0 END + CASE EPIORDER WHEN NULL THEN 1 ELSE 0 END) AS ROW_QUALITY
	FROM 
  ( SELECT t2000.*, 2000 AS HES_YEAR FROM HES_INPATIENT_LOAD_2000 t2000
  UNION ALL
  SELECT t2001.*, 2001 AS HES_YEAR FROM HES_INPATIENT_LOAD_2001 t2001
  UNION ALL
  SELECT t2002.*, 2002 AS HES_YEAR FROM HES_INPATIENT_LOAD_2002 t2002
  UNION ALL
  SELECT t2003.*, 2003 AS HES_YEAR FROM HES_INPATIENT_LOAD_2003 t2003
  UNION ALL
  SELECT t2004.*, 2004 AS HES_YEAR FROM HES_INPATIENT_LOAD_2004 t2004
  UNION ALL
  SELECT t2005.*, 2005 AS HES_YEAR FROM HES_INPATIENT_LOAD_2005 t2005
  UNION ALL
  SELECT t2006.*, 2006 AS HES_YEAR FROM HES_INPATIENT_LOAD_2006 t2006
  UNION ALL
  SELECT t2007.*, 2007 AS HES_YEAR FROM HES_INPATIENT_LOAD_2007 t2007
  UNION ALL
  SELECT t2008.*, 2008 AS HES_YEAR FROM HES_INPATIENT_LOAD_2008 t2008
  UNION ALL
  SELECT t2009.*, 2009 AS HES_YEAR FROM HES_INPATIENT_LOAD_2009 t2009
  UNION ALL
  SELECT t2010.*, 2010 AS HES_YEAR FROM HES_INPATIENT_LOAD_2010 t2010
  UNION ALL
  SELECT t2011.*, 2011 AS HES_YEAR FROM HES_INPATIENT_LOAD_2011 t2011)  t 
	WHERE 
	EPISTAT=3 AND 
	ADMIDATE IS NOT NULL AND 
	PROCODE IS NOT NULL AND 
	EXTRACT_HESID IS NOT NULL AND 
	EPIKEY IS NOT NULL AND
	ADMIDATE <= NVL(DISDATE, ADMIDATE);
  
  COMMIT;
  
  UPDATE HES_INPATIENT_LOAD_2000_2011 SET TRANSIT = 1 WHERE ((ADMISORC<51 OR ADMISORC>53) AND ADMIMETH!=81) AND (DISDEST>=51 AND DISDEST<=53);
  COMMIT;

	UPDATE HES_INPATIENT_LOAD_2000_2011 SET TRANSIT = 3 WHERE ((ADMISORC>=51 OR ADMISORC<=53) AND ADMIMETH=81) AND (DISDEST<51 OR DISDEST>53);
  COMMIT;

	UPDATE HES_INPATIENT_LOAD_2000_2011 SET TRANSIT = 2 WHERE ((ADMISORC>=51 OR ADMISORC<=53) AND ADMIMETH=81) AND (DISDEST>=51 AND DISDEST<=53);
  COMMIT;

  -- drop duplicates ordered by quality keeping best and recreate a cleaned version of the original table
  CREATE TABLE HES_IP_LOAD_2000_2011_CLEAN AS
  SELECT * FROM
	(SELECT HES_INPATIENT_LOAD_2000_2011.*, ROW_NUMBER() OVER (PARTITION BY EXTRACT_HESID, EPISTART, EPIORDER, EPIEND, TRANSIT ORDER BY ADMIDATE, DISDATE, U, ROW_QUALITY) RN FROM HES_INPATIENT_LOAD_2000_2011) 
	WHERE RN=1;
  COMMIT;