DROP TABLE HES_WAITING_TIME PURGE;
CREATE TABLE HES_WAITING_TIME ( 
extract_hesid  varchar2(32), 
cips_number INTEGER,
period_of_care_number INTEGER,
reqdate DATE,
firstapptdate DATE,
elecdate DATE,
admidate DATE,
gp_op_days INTEGER,
op_ref_days INTEGER,
ref_ip_days INTEGER,
total_wait_days INTEGER,
soal varchar2(10), 
hes_year NUMBER(4,0),
age_group VARCHAR2(10) 
) TABLESPACE HEPI;

CREATE INDEX WAITING_LSOA ON HEPI.HES_WAITING_TIME(SOAL);
CREATE BITMAP INDEX WAIT_AGE ON HEPI.HES_WAITING_TIME(AGE_GROUP);
CREATE BITMAP INDEX WAIT_YEAR ON HES_WAITING_TIME(HES_YEAR);


create or replace PROCEDURE CALCULATE_HES_WAIT
AUTHID CURRENT_USER
AS 
	CURSOR cips_cursor IS SELECT EXTRACT_HESID, CIPS_NUMBER, ELECDATE, ADMIDATE, SOAL, AGE_GROUP, HES_YEAR  FROM HES_INPATIENT_CIPS WHERE ADMIMETH BETWEEN 11 AND 13;
	current_wait HES_WAITING_TIME%ROWTYPE;
  match_found BOOLEAN;
	row_count INTEGER;
BEGIN
    row_count := 1;
    FOR curr_cips IN cips_cursor
    LOOP
        row_count := row_count + 1;
		-- commit every 10,000 iterations
		IF (row_count MOD 10000) = 0 THEN
      COMMIT;
		END IF;
		current_wait.extract_hesid := curr_cips.extract_hesid; 
		current_wait.cips_number := curr_cips.cips_number;
		current_wait.elecdate := curr_cips.elecdate;
		current_wait.admidate := curr_cips.admidate;
		current_wait.soal := curr_cips.soal;
		current_wait.hes_year := curr_cips.hes_year; 
		current_wait.age_group := curr_cips.age_group; 
	   
    match_found := FALSE;
    IF current_wait.elecdate IS NOT NULL THEN
			BEGIN
        SELECT PERIOD_OF_CARE_NUMBER, REQDATE, FIRSTAPPTDATE 
        INTO current_wait.period_of_care_number, current_wait.reqdate, current_wait.firstapptdate 
        FROM HES_OUTPATIENT_PERIOD_OF_CARE WHERE PERIOD_OF_CARE_NUMBER IN
        (SELECT PERIOD_OF_CARE_NUMBER FROM
        (SELECT APPTDATE, PERIOD_OF_CARE_NUMBER FROM HES_OUTPATIENT_APPOINTMENT 
        WHERE EXTRACT_HESID=current_wait.extract_hesid AND 
        APPTDATE <= current_wait.elecdate
        ORDER BY APPTDATE DESC)
        WHERE ROWNUM=1);
        match_found := TRUE;
      EXCEPTION
      WHEN NO_DATA_FOUND THEN
        match_found := FALSE;
      END;
		END IF;
	  
		IF current_wait.reqdate IS NOT NULL AND current_wait.firstapptdate IS NOT NULL THEN
			current_wait.gp_op_days := current_wait.firstapptdate-current_wait.reqdate;
		END IF;

		IF current_wait.elecdate IS NOT NULL AND current_wait.firstapptdate IS NOT NULL THEN
			current_wait.op_ref_days := current_wait.elecdate - current_wait.firstapptdate;
		END IF;

		IF current_wait.elecdate IS NOT NULL AND current_wait.admidate IS NOT NULL THEN
			current_wait.ref_ip_days := current_wait.admidate - current_wait.elecdate;
		END IF;

		IF current_wait.reqdate IS NOT NULL AND current_wait.admidate IS NOT NULL THEN
			current_wait.total_wait_days := current_wait.admidate - current_wait.reqdate;
		END IF;

    IF match_found THEN
      INSERT INTO hes_waiting_time VALUES current_wait;
    END IF;
	END LOOP;

END CALCULATE_HES_WAIT;