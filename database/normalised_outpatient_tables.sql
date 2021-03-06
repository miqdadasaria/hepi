DROP TABLE HES_OUTPATIENT_PERIOD_OF_CARE PURGE;
CREATE TABLE HES_OUTPATIENT_PERIOD_OF_CARE ( 
period_of_care_number INTEGER,
extract_hesid  varchar2(32), 
soal  varchar2(10), 
reqdate DATE,
firstapptdate DATE,
disdate DATE,
outcome  NUMBER(1,0),
refsourc  NUMBER(2,0), 
purcode  varchar2(6), 
preferer  varchar2(16), 
referorg  varchar2(6),
hes_year NUMBER(4,0),
age_group VARCHAR2(10) 
) TABLESPACE HEPI; 

DROP TABLE HES_OUTPATIENT_APPOINTMENT PURGE;
CREATE TABLE HES_OUTPATIENT_APPOINTMENT ( 
appointment_number INTEGER,
period_of_care_number INTEGER,
extract_hesid  varchar2(32), 
apptdate  DATE, 
apptorder NUMBER(4,0),
attentype  NUMBER(2,0), 
attended  NUMBER(1,0), 
firstatt NUMBER(1,0), 
dnadate  DATE, 
waiting  NUMBER(10,0),
stafftyp  NUMBER(2,0), 
outcome  NUMBER(1,0), 
priority  NUMBER(1,0),
servtype  NUMBER(1,0), 
mainspef  varchar2(4), 
tretspef  varchar2(4), 
soal  varchar2(10), 
sitetret  varchar2(10), 
procode  varchar2(6), 
protype  varchar2(200), 
pconsult  varchar2(16), 
admincat  NUMBER(2,0), 
attendkey NUMBER,
hes_year NUMBER(4,0),
age_group VARCHAR2(10)
) TABLESPACE HEPI; 

DROP TABLE HES_OUTPATIENT_APPT_DIAG PURGE;
CREATE TABLE HES_OUTPATIENT_APPT_DIAG(
appointment_number INTEGER,
diag_code  varchar2(4),
is_primary NUMBER(1,0)
);

DROP TABLE HES_OUTPATIENT_APPT_PROC PURGE;
CREATE TABLE HES_OUTPATIENT_APPT_PROC(
appointment_number INTEGER,
procedure_code  varchar2(4),
is_primary NUMBER(1,0)
);

CREATE INDEX POC_EXTRACT_HESID ON HES_OUTPATIENT_PERIOD_OF_CARE (EXTRACT_HESID);
CREATE INDEX POC_REQDATE ON HES_OUTPATIENT_PERIOD_OF_CARE (REQDATE);
CREATE BITMAP INDEX POC_OUTCOME ON HES_OUTPATIENT_PERIOD_OF_CARE (OUTCOME);
CREATE INDEX POC_firstapptdate ON HES_OUTPATIENT_PERIOD_OF_CARE (firstapptdate);
CREATE INDEX POC_DISDATE ON HES_OUTPATIENT_PERIOD_OF_CARE (DISDATE);
CREATE INDEX POC_SOAL ON HES_OUTPATIENT_PERIOD_OF_CARE (SOAL);
CREATE BITMAP INDEX POC_HES_YEAR ON HES_OUTPATIENT_PERIOD_OF_CARE (HES_YEAR);
CREATE BITMAP INDEX POC_AGE_GROUP ON HES_OUTPATIENT_PERIOD_OF_CARE (AGE_GROUP);


CREATE INDEX APPT_EXTRACT_HESID ON HES_OUTPATIENT_APPOINTMENT (EXTRACT_HESID);
CREATE INDEX APPT_POC_NUM ON HES_OUTPATIENT_APPOINTMENT (period_of_care_number);
CREATE INDEX APPT_DATE ON HES_OUTPATIENT_APPOINTMENT (APPTDATE);
CREATE INDEX DNA_DATE ON HES_OUTPATIENT_APPOINTMENT (DNADATE);
CREATE INDEX APPT_ORDER ON HES_OUTPATIENT_APPOINTMENT (APPTORDER);
CREATE BITMAP INDEX APPT_FIRSTATT ON HES_OUTPATIENT_APPOINTMENT (FIRSTATT);
CREATE BITMAP INDEX APPT_ATTENDED ON HES_OUTPATIENT_APPOINTMENT (ATTENDED);
CREATE BITMAP INDEX APPT_OUTCOME ON HES_OUTPATIENT_APPOINTMENT (OUTCOME);
CREATE BITMAP INDEX APPT_PRIORITY ON HES_OUTPATIENT_APPOINTMENT (PRIORITY);
CREATE INDEX APPT_SOAL ON HES_OUTPATIENT_APPOINTMENT (SOAL);
CREATE BITMAP INDEX APPT_HES_YEAR ON HES_OUTPATIENT_APPOINTMENT (HES_YEAR);
CREATE BITMAP INDEX APPT_AGE_GROUP ON HES_OUTPATIENT_APPOINTMENT (AGE_GROUP);


CREATE INDEX APPT_DIAG_APPT_NUMBER ON HES_OUTPATIENT_APPT_DIAG (appointment_number);
CREATE INDEX APPT_DIAG_ICD ON HES_OUTPATIENT_APPT_DIAG (DIAG_CODE);
CREATE BITMAP INDEX APPT_DIAG_PRIMARY ON HES_OUTPATIENT_APPT_DIAG (IS_PRIMARY);

CREATE INDEX APPT_PROC_APPT_NUMBER ON HES_OUTPATIENT_APPT_PROC (appointment_number);
CREATE INDEX APPT_PROC_OPCS ON HES_OUTPATIENT_APPT_PROC (PROCEDURE_CODE);
CREATE BITMAP INDEX APPT_PROC_PRIMARY ON HES_OUTPATIENT_APPT_PROC (IS_PRIMARY);