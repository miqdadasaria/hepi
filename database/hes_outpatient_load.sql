DROP TABLE HES_OUTPATIENT_LOAD;
CREATE TABLE HES_OUTPATIENT_LOAD ( 
apptdate  DATE, 
attentype  INTEGER, 
attended  INTEGER, 
waiting  INTEGER,
firstatt INTEGER, 
dnadate  DATE, 
stafftyp  INTEGER, 
outcome  INTEGER, 
priority  INTEGER,
reqdate  DATE, 
servtype  INTEGER, 
refsourc  INTEGER, 
diag_01  varchar2(4), 
diag_02  varchar2(4), 
diag_03  varchar2(4), 
diag_04  varchar2(4), 
diag_05  varchar2(4), 
diag_06  varchar2(4), 
diag_07  varchar2(4), 
diag_08  varchar2(4), 
diag_09  varchar2(4), 
diag_10  varchar2(4), 
diag_11  varchar2(4), 
diag_12  varchar2(4),
opertn_01  varchar2(4), 
opertn_02  varchar2(4), 
opertn_03  varchar2(4), 
opertn_04  varchar2(4), 
opertn_05  varchar2(4), 
opertn_06  varchar2(4), 
opertn_07  varchar2(4), 
opertn_08  varchar2(4), 
opertn_09  varchar2(4),
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
mainspef  varchar2(4), 
tretspef  varchar2(4), 
soal  varchar2(10), 
sitetret  varchar2(10), 
purcode  varchar2(6), 
procode  varchar2(6), 
protype  varchar2(200), 
preferer  varchar2(16), 
admincat  INTEGER, 
dob  DATE, 
ethnos  varchar2(1),
extract_hesid  varchar2(32),  
sex  INTEGER,
pconsult  varchar2(16), 
referorg  varchar2(6), 
attendkey  NUMBER
) TABLESPACE HEPI; 

CREATE TABLE HES_OUTPATIENT_LOAD_2003_2011 AS
	SELECT t.*, 0 AS APPT_CHE, 0 AS POC_CHE, 
	(CASE MAINSPEF WHEN NULL THEN 1 ELSE 0 END + 
  CASE TRETSPEF WHEN NULL THEN 1 ELSE 0 END + 
  CASE reqdate WHEN NULL THEN 1 ELSE 0 END + 
  CASE refsourc WHEN NULL THEN 1 ELSE 0 END + 
  CASE priority WHEN NULL THEN 1 ELSE 0 END) AS ROW_QUALITY
	FROM 
  (
SELECT t2003.*, 2003 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2003 t2003
UNION ALL
SELECT t2004.*, 2004 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2004 t2004
UNION ALL
SELECT t2005.*, 2005 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2005 t2005
UNION ALL
SELECT t2006.*, 2006 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2006 t2006
UNION ALL
SELECT t2007.*, 2007 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2007 t2007
UNION ALL
SELECT t2008.*, 2008 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2008 t2008
UNION ALL
SELECT t2009.*, 2009 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2009 t2009
UNION ALL
SELECT t2010.*, 2010 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2010 t2010
UNION ALL
SELECT t2011.*, 2011 AS HES_YEAR FROM HES_OUTPATIENT_LOAD_2011 t2011)t 
	WHERE 
	APPTDATE IS NOT NULL AND 
	EXTRACT_HESID IS NOT NULL AND 
	APPTDATE >= NVL(REQDATE, APPTDATE);
  
  
CREATE TABLE HES_OP_LOAD_2003_2011_CLEAN AS
SELECT * FROM
  (SELECT HES_OUTPATIENT_LOAD_2003_2011.*, ROW_NUMBER() OVER (PARTITION BY EXTRACT_HESID, APPTDATE, NVL(FIRSTATT,0), NVL(PCONSULT,0), NVL(REQDATE,sysdate), NVL(OUTCOME,0) ORDER BY ROW_QUALITY) RN FROM HES_OUTPATIENT_LOAD_2003_2011) 
WHERE RN=1;
  
DROP TABLE HES_OUTPATIENT_LOAD_2003_2011 PURGE;