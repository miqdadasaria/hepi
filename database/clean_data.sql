CREATE OR REPLACE FUNCTION varcharToDate(string_date IN VARCHAR2)
  RETURN DATE DETERMINISTIC
AS
  temp_date DATE;
BEGIN
  
  CASE LENGTH(string_date) 
  WHEN 5 THEN
    temp_date := TO_DATE('0' || string_date, 'MMYYYY');
  WHEN 6 THEN
    temp_date := TO_DATE(string_date, 'MMYYYY');
  WHEN 7 THEN
    temp_date := TO_DATE('0' || string_date, 'DDMMYYYY');
  WHEN 8 THEN
    temp_date := TO_DATE(string_date, 'DDMMYYYY');
  WHEN 10 THEN
    IF INSTR(string_date, '-') = 5 THEN temp_date := TO_DATE(string_date, 'YYYY-MM-DD'); 
    ELSIF INSTR(string_date, '/') = 3 THEN temp_date :=  TO_DATE(string_date, 'DD/MM/YYYY');
    END IF;
  ELSE
    temp_date := NULL;
  END CASE;
  
  IF temp_date < TO_DATE('01/01/1800', 'DD/MM/YYYY') THEN temp_date:=NULL; END IF;
  
  RETURN temp_date;
END varcharToDate;

CREATE OR REPLACE PROCEDURE varcharToDateColumn(table_name IN VARCHAR2, column_name IN VARCHAR2)
AS
BEGIN
  EXECUTE IMMEDIATE 'ALTER TABLE ' || table_name || ' ADD (' || column_name || '_new DATE)';
  EXECUTE IMMEDIATE 'UPDATE ' || table_name || ' SET ' || column_name ||'_new = VARCHARTODATE(' || column_name ||')';
  EXECUTE IMMEDIATE 'ALTER TABLE ' || table_name || ' DROP COLUMN ' || column_name;
  EXECUTE IMMEDIATE 'ALTER TABLE ' || table_name || ' RENAME COLUMN ' || column_name || '_new TO ' || column_name;
END varcharToDateColumn;


CREATE OR REPLACE FUNCTION cleanA(dirty IN NUMBER)
  RETURN NUMBER DETERMINISTIC
AS
  clean NUMBER;
BEGIN
  
  CASE dirty 
  WHEN 98 THEN
    clean := NULL;
  WHEN 99 THEN
    clean := NULL;
  ELSE
    clean := dirty;
  END CASE;
  
  RETURN clean;
END cleanA;


CREATE OR REPLACE FUNCTION cleanB(dirty IN VARCHAR2)
  RETURN VARCHAR2 DETERMINISTIC
AS
  clean VARCHAR2(4);
BEGIN
  
  CASE LENGTH(NVL(dirty,'four')) 
  WHEN 4 THEN
    clean := dirty;
  ELSE
    clean := RTRIM(REPLACE(SUBSTR(dirty,1,4),'-'));
  END CASE;
  
  RETURN clean;
END cleanB;


CREATE OR REPLACE FUNCTION cleanC(dirty IN VARCHAR2)
  RETURN VARCHAR2 DETERMINISTIC
AS
  clean VARCHAR2(16);
BEGIN
  
IF dirty IS NOT NULL THEN
  clean := RTRIM(REPLACE(REPLACE(dirty, '&'),'-'));
END IF;

  RETURN clean;
END cleanC;


CREATE OR REPLACE FUNCTION cleanD(dirty IN NUMBER)
  RETURN NUMBER DETERMINISTIC
AS
  clean NUMBER;
BEGIN
  
  CASE dirty 
  WHEN 8 THEN
    clean := NULL;
  WHEN 9 THEN
    clean := NULL;
  ELSE
    clean := dirty;
  END CASE;
  
  RETURN clean;
END cleanD;

CREATE OR REPLACE FUNCTION cleanE(dirty IN VARCHAR2)
  RETURN VARCHAR2 DETERMINISTIC
AS
  clean VARCHAR2(16);
BEGIN
  
IF dirty IS NOT NULL THEN
  clean := RTRIM(REPLACE(REPLACE(dirty, 'X99998'),'X99999'));
END IF;

  RETURN clean;
END cleanE;

CREATE OR REPLACE FUNCTION cleanF(dirty IN VARCHAR2)
  RETURN VARCHAR2 DETERMINISTIC
AS
  clean VARCHAR2(1);
BEGIN
  
IF dirty IS NOT NULL THEN
  clean := RTRIM(REPLACE(REPLACE(REPLACE(UPPER(dirty), 'X'),'Z'),'9'));
END IF;

  RETURN clean;
END cleanF;

CREATE OR REPLACE FUNCTION cleanG(dirty IN NUMBER)
  RETURN NUMBER DETERMINISTIC
AS
  clean NUMBER;
BEGIN
  
  CASE dirty 
  WHEN 0 THEN
    clean := NULL;
  WHEN 3 THEN
    clean := NULL;
  WHEN 9 THEN
    clean := NULL;
  ELSE
    clean := dirty;
  END CASE;
  
  RETURN clean;
END cleanG;

----

CREATE OR REPLACE FUNCTION cleanH(dirty IN NUMBER)
  RETURN NUMBER DETERMINISTIC
AS
  clean NUMBER;
BEGIN
  
  CASE dirty 
  WHEN 0 THEN
    clean := NULL;
  WHEN 9 THEN
    clean := NULL;
  ELSE
    clean := dirty;
  END CASE;
  
  RETURN clean;
END cleanH;

CREATE OR REPLACE FUNCTION cleanI(dirty IN NUMBER)
  RETURN NUMBER DETERMINISTIC
AS
  clean NUMBER;
BEGIN
  
  CASE dirty 
  WHEN 13 THEN
    clean := NULL;
  ELSE
    clean := dirty;
  END CASE;
  
  RETURN clean;
END cleanI;

CREATE OR REPLACE FUNCTION cleanJ(dirty IN NUMBER)
  RETURN NUMBER DETERMINISTIC
AS
  clean NUMBER;
BEGIN
  
  CASE dirty 
  WHEN 8 THEN
    clean := NULL;
  WHEN 9 THEN
    clean := NULL;
  WHEN 99 THEN
    clean := NULL;
  ELSE
    clean := dirty;
  END CASE;
  
  RETURN clean;
END cleanJ;

CREATE OR REPLACE FUNCTION cleanK(dirty IN VARCHAR2)
  RETURN VARCHAR2 DETERMINISTIC
AS
  clean VARCHAR2(16);
BEGIN
  
IF dirty IS NOT NULL THEN
  clean := RTRIM(REPLACE(REPLACE(REPLACE(REPLACE(dirty,'-'),'&'), 'X99998'),'X99999'));
END IF;

  RETURN clean;
END cleanK;

CREATE OR REPLACE FUNCTION cleanL(dirty IN VARCHAR2)
  RETURN VARCHAR2 DETERMINISTIC
AS
  clean VARCHAR2(16);
BEGIN
  
IF dirty IS NOT NULL THEN
	IF UPPER(dirty) = 'Z99999999' THEN
		clean := NULL;
	ELSE
		clean := UPPER(dirty);
	END IF;
END IF;

  RETURN clean;
END cleanL;

CREATE OR REPLACE FUNCTION cleanM(dirty IN VARCHAR2)
  RETURN VARCHAR2 DETERMINISTIC
AS
  clean VARCHAR2(1);
BEGIN
  
IF dirty IS NOT NULL THEN
    clean := REPLACE(REPLACE(UPPER(SUBSTR(dirty,1,1)),'Z'),'X');
END IF;
  
  RETURN clean;
END cleanM;

-------------------------------
-- clean inpatient table
-------------------------------


  CREATE GLOBAL TEMPORARY TABLE "HEPI"."HES_INPATIENT_LOAD" 
   (	"ADMIDATE" DATE, 
	"ELECDATE" DATE, 
	"ADMIMETH" NUMBER(*,0), 
	"ADMISORC" NUMBER(*,0), 
	"ELECDUR" NUMBER(*,0), 
	"OPDATE_1" DATE, 
	"OPDATE_2" DATE, 
	"OPDATE_3" DATE, 
	"OPDATE_4" DATE, 
	"OPDATE_5" DATE, 
	"OPDATE_6" DATE, 
	"OPDATE_7" DATE, 
	"OPDATE_8" DATE, 
	"OPDATE_9" DATE, 
	"OPDATE_10" DATE, 
	"OPDATE_11" DATE, 
	"OPDATE_12" DATE, 
	"OPDATE_13" DATE, 
	"OPDATE_14" DATE, 
	"OPDATE_15" DATE, 
	"OPDATE_16" DATE, 
	"OPDATE_17" DATE, 
	"OPDATE_18" DATE, 
	"OPDATE_19" DATE, 
	"OPDATE_20" DATE, 
	"OPDATE_21" DATE, 
	"OPDATE_22" DATE, 
	"OPDATE_23" DATE, 
	"OPDATE_24" DATE, 
	"DIAG_1" VARCHAR2(4 BYTE), 
	"DIAG_2" VARCHAR2(4 BYTE), 
	"DIAG_3" VARCHAR2(4 BYTE), 
	"DIAG_4" VARCHAR2(4 BYTE), 
	"DIAG_5" VARCHAR2(4 BYTE), 
	"DIAG_6" VARCHAR2(4 BYTE), 
	"DIAG_7" VARCHAR2(4 BYTE), 
	"DIAG_8" VARCHAR2(4 BYTE), 
	"DIAG_9" VARCHAR2(4 BYTE), 
	"DIAG_10" VARCHAR2(4 BYTE), 
	"DIAG_11" VARCHAR2(4 BYTE), 
	"DIAG_12" VARCHAR2(4 BYTE), 
	"DIAG_13" VARCHAR2(4 BYTE), 
	"DIAG_14" VARCHAR2(4 BYTE), 
	"DIAG_15" VARCHAR2(4 BYTE), 
	"DIAG_16" VARCHAR2(4 BYTE), 
	"DIAG_17" VARCHAR2(4 BYTE), 
	"DIAG_18" VARCHAR2(4 BYTE), 
	"DIAG_19" VARCHAR2(4 BYTE), 
	"DIAG_20" VARCHAR2(4 BYTE), 
	"MAINSPEF" VARCHAR2(4 BYTE), 
	"OPERTN_1" VARCHAR2(4 BYTE), 
	"OPERTN_2" VARCHAR2(4 BYTE), 
	"OPERTN_3" VARCHAR2(4 BYTE), 
	"OPERTN_4" VARCHAR2(4 BYTE), 
	"OPERTN_5" VARCHAR2(4 BYTE), 
	"OPERTN_6" VARCHAR2(4 BYTE), 
	"OPERTN_7" VARCHAR2(4 BYTE), 
	"OPERTN_8" VARCHAR2(4 BYTE), 
	"OPERTN_9" VARCHAR2(4 BYTE), 
	"OPERTN_10" VARCHAR2(4 BYTE), 
	"OPERTN_11" VARCHAR2(4 BYTE), 
	"OPERTN_12" VARCHAR2(4 BYTE), 
	"OPERTN_13" VARCHAR2(4 BYTE), 
	"OPERTN_14" VARCHAR2(4 BYTE), 
	"OPERTN_15" VARCHAR2(4 BYTE), 
	"OPERTN_16" VARCHAR2(4 BYTE), 
	"OPERTN_17" VARCHAR2(4 BYTE), 
	"OPERTN_18" VARCHAR2(4 BYTE), 
	"OPERTN_19" VARCHAR2(4 BYTE), 
	"OPERTN_20" VARCHAR2(4 BYTE), 
	"OPERTN_21" VARCHAR2(4 BYTE), 
	"OPERTN_22" VARCHAR2(4 BYTE), 
	"OPERTN_23" VARCHAR2(4 BYTE), 
	"OPERTN_24" VARCHAR2(4 BYTE), 
	"OPERSTAT" NUMBER(*,0), 
	"CLASSPAT" NUMBER(*,0), 
	"POSOPDUR" NUMBER(*,0), 
	"PREOPDUR" NUMBER(*,0), 
	"TRETSPEF" VARCHAR2(4 BYTE), 
	"DISDATE" DATE, 
	"DISDEST" NUMBER(*,0), 
	"DISREADYDATE" DATE, 
	"DISMETH" NUMBER(*,0), 
	"EPIEND" DATE, 
	"EPISTART" DATE, 
	"EPIDUR" NUMBER(*,0), 
	"EPIORDER" NUMBER(*,0), 
	"EPISTAT" NUMBER(*,0), 
	"EPITYPE" NUMBER(*,0), 
	"PROVSPNO" VARCHAR2(12 BYTE), 
	"SOAL" VARCHAR2(10 BYTE), 
	"SITETRET" VARCHAR2(5 BYTE), 
	"HRGLATE" VARCHAR2(5 BYTE), 
	"HRGLATE35" VARCHAR2(5 BYTE), 
	"HRG40" VARCHAR2(5 BYTE), 
	"PURCODE" VARCHAR2(6 BYTE), 
	"PROCODE" VARCHAR2(6 BYTE), 
	"PROTYPE" VARCHAR2(200 BYTE), 
	"ADMINCAT" NUMBER(*,0), 
	"DOB_MMYYYY" DATE, 
	"ETHNOS" VARCHAR2(1 BYTE), 
	"EXTRACT_HESID" VARCHAR2(32 BYTE), 
	"SEX" NUMBER(*,0), 
	"PCONSULT" VARCHAR2(16 BYTE), 
	"PREFERER" VARCHAR2(16 BYTE), 
	"REFERORG" VARCHAR2(16 BYTE), 
	"ADMISTAT" NUMBER(*,0), 
	"EPIKEY" NUMBER(*,0), 
	"TRANSIT" NUMBER, 
	"EPI_CHE" NUMBER, 
	"SPELL_CHE" NUMBER, 
	"CIPS_CHE" NUMBER, 
	"U" NUMBER, 
	"ROW_QUALITY" NUMBER
   ) ON COMMIT PRESERVE ROWS ;


  CREATE GLOBAL TEMPORARY TABLE "HEPI"."HES_INPATIENT_LOAD_CLEAN" 
   (	"ADMIDATE" DATE, 
	"ELECDATE" DATE, 
	"ADMIMETH" NUMBER(*,0), 
	"ADMISORC" NUMBER(*,0), 
	"ELECDUR" NUMBER(*,0), 
	"OPDATE_1" DATE, 
	"OPDATE_2" DATE, 
	"OPDATE_3" DATE, 
	"OPDATE_4" DATE, 
	"OPDATE_5" DATE, 
	"OPDATE_6" DATE, 
	"OPDATE_7" DATE, 
	"OPDATE_8" DATE, 
	"OPDATE_9" DATE, 
	"OPDATE_10" DATE, 
	"OPDATE_11" DATE, 
	"OPDATE_12" DATE, 
	"OPDATE_13" DATE, 
	"OPDATE_14" DATE, 
	"OPDATE_15" DATE, 
	"OPDATE_16" DATE, 
	"OPDATE_17" DATE, 
	"OPDATE_18" DATE, 
	"OPDATE_19" DATE, 
	"OPDATE_20" DATE, 
	"OPDATE_21" DATE, 
	"OPDATE_22" DATE, 
	"OPDATE_23" DATE, 
	"OPDATE_24" DATE, 
	"DIAG_1" VARCHAR2(4 BYTE), 
	"DIAG_2" VARCHAR2(4 BYTE), 
	"DIAG_3" VARCHAR2(4 BYTE), 
	"DIAG_4" VARCHAR2(4 BYTE), 
	"DIAG_5" VARCHAR2(4 BYTE), 
	"DIAG_6" VARCHAR2(4 BYTE), 
	"DIAG_7" VARCHAR2(4 BYTE), 
	"DIAG_8" VARCHAR2(4 BYTE), 
	"DIAG_9" VARCHAR2(4 BYTE), 
	"DIAG_10" VARCHAR2(4 BYTE), 
	"DIAG_11" VARCHAR2(4 BYTE), 
	"DIAG_12" VARCHAR2(4 BYTE), 
	"DIAG_13" VARCHAR2(4 BYTE), 
	"DIAG_14" VARCHAR2(4 BYTE), 
	"DIAG_15" VARCHAR2(4 BYTE), 
	"DIAG_16" VARCHAR2(4 BYTE), 
	"DIAG_17" VARCHAR2(4 BYTE), 
	"DIAG_18" VARCHAR2(4 BYTE), 
	"DIAG_19" VARCHAR2(4 BYTE), 
	"DIAG_20" VARCHAR2(4 BYTE), 
	"MAINSPEF" VARCHAR2(4 BYTE), 
	"OPERTN_1" VARCHAR2(4 BYTE), 
	"OPERTN_2" VARCHAR2(4 BYTE), 
	"OPERTN_3" VARCHAR2(4 BYTE), 
	"OPERTN_4" VARCHAR2(4 BYTE), 
	"OPERTN_5" VARCHAR2(4 BYTE), 
	"OPERTN_6" VARCHAR2(4 BYTE), 
	"OPERTN_7" VARCHAR2(4 BYTE), 
	"OPERTN_8" VARCHAR2(4 BYTE), 
	"OPERTN_9" VARCHAR2(4 BYTE), 
	"OPERTN_10" VARCHAR2(4 BYTE), 
	"OPERTN_11" VARCHAR2(4 BYTE), 
	"OPERTN_12" VARCHAR2(4 BYTE), 
	"OPERTN_13" VARCHAR2(4 BYTE), 
	"OPERTN_14" VARCHAR2(4 BYTE), 
	"OPERTN_15" VARCHAR2(4 BYTE), 
	"OPERTN_16" VARCHAR2(4 BYTE), 
	"OPERTN_17" VARCHAR2(4 BYTE), 
	"OPERTN_18" VARCHAR2(4 BYTE), 
	"OPERTN_19" VARCHAR2(4 BYTE), 
	"OPERTN_20" VARCHAR2(4 BYTE), 
	"OPERTN_21" VARCHAR2(4 BYTE), 
	"OPERTN_22" VARCHAR2(4 BYTE), 
	"OPERTN_23" VARCHAR2(4 BYTE), 
	"OPERTN_24" VARCHAR2(4 BYTE), 
	"OPERSTAT" NUMBER(*,0), 
	"CLASSPAT" NUMBER(*,0), 
	"POSOPDUR" NUMBER(*,0), 
	"PREOPDUR" NUMBER(*,0), 
	"TRETSPEF" VARCHAR2(4 BYTE), 
	"DISDATE" DATE, 
	"DISDEST" NUMBER(*,0), 
	"DISREADYDATE" DATE, 
	"DISMETH" NUMBER(*,0), 
	"EPIEND" DATE, 
	"EPISTART" DATE, 
	"EPIDUR" NUMBER(*,0), 
	"EPIORDER" NUMBER(*,0), 
	"EPISTAT" NUMBER(*,0), 
	"EPITYPE" NUMBER(*,0), 
	"PROVSPNO" VARCHAR2(12 BYTE), 
	"SOAL" VARCHAR2(10 BYTE), 
	"SITETRET" VARCHAR2(5 BYTE), 
	"HRGLATE" VARCHAR2(5 BYTE), 
	"HRGLATE35" VARCHAR2(5 BYTE), 
	"HRG40" VARCHAR2(5 BYTE), 
	"PURCODE" VARCHAR2(6 BYTE), 
	"PROCODE" VARCHAR2(6 BYTE), 
	"PROTYPE" VARCHAR2(200 BYTE), 
	"ADMINCAT" NUMBER(*,0), 
	"DOB_MMYYYY" DATE, 
	"ETHNOS" VARCHAR2(1 BYTE), 
	"EXTRACT_HESID" VARCHAR2(32 BYTE), 
	"SEX" NUMBER(*,0), 
	"PCONSULT" VARCHAR2(16 BYTE), 
	"PREFERER" VARCHAR2(16 BYTE), 
	"REFERORG" VARCHAR2(16 BYTE), 
	"ADMISTAT" NUMBER(*,0), 
	"EPIKEY" NUMBER(*,0), 
	"TRANSIT" NUMBER, 
	"EPI_CHE" NUMBER, 
	"SPELL_CHE" NUMBER, 
	"CIPS_CHE" NUMBER, 
	"U" NUMBER, 
	"ROW_QUALITY" NUMBER, 
	"RN" NUMBER
   ) ON COMMIT DELETE ROWS ;


  CREATE GLOBAL TEMPORARY TABLE "HEPI"."PATIENT_TEMP" 
   (	"EXTRACT_HESID" VARCHAR2(32 BYTE), 
	"DOB" DATE, 
	"ETHNOS" VARCHAR2(1 BYTE), 
	"SEX" NUMBER(1,0)
   ) ON COMMIT PRESERVE ROWS ;


create or replace PROCEDURE NUMBER_SPELLS 
AUTHID CURRENT_USER
AS 
  TYPE episode_type IS RECORD(
  rid ROWID,
  extract_hesid VARCHAR2(32),
  procode3 VARCHAR2(3),
  admidate DATE,
  dismeth INTEGER,
  epistart DATE,
  epiend DATE
  );
  TYPE episode_cursor_type IS REF CURSOR;
  l_episode_number INTEGER;
  l_spell_number INTEGER;
  episode_cursor episode_cursor_type;
  current_episode episode_type;
  previous_episode episode_type;
BEGIN
  SELECT 
  NVL(MAX(episode_number),0)+1, NVL(MAX(spell_number),0)+1 
  INTO l_episode_number, l_spell_number
  FROM HES_INPATIENT_EPISODE;
  
  OPEN episode_cursor FOR 
  SELECT rowid AS rid, extract_hesid, substr(procode,1,3) AS procode3, admidate, dismeth, epistart, epiend FROM HES_INPATIENT_LOAD_CLEAN
   ORDER BY extract_hesid, epistart, epiorder, epiend, transit, epikey;
  
  FETCH episode_cursor INTO current_episode;
  UPDATE HES_INPATIENT_LOAD_CLEAN SET epi_che=l_episode_number, spell_che=l_spell_number WHERE rowid=current_episode.rid; 
  previous_episode := current_episode;
  l_episode_number := l_episode_number + 1;
  LOOP
    FETCH episode_cursor INTO current_episode;
    EXIT WHEN episode_cursor%NOTFOUND;
    IF current_episode.extract_hesid = previous_episode.extract_hesid
      AND current_episode.procode3 = previous_episode.procode3
      AND (current_episode.admidate = previous_episode.admidate OR
        ((previous_episode.dismeth > 5 AND current_episode.epistart = previous_episode.epiend) OR
        current_episode.epistart = previous_episode.epistart))
    THEN
      UPDATE HES_INPATIENT_LOAD_CLEAN SET epi_che=l_episode_number, spell_che=l_spell_number WHERE rowid=current_episode.rid; 
    ELSE
      l_spell_number := l_spell_number + 1;
      UPDATE HES_INPATIENT_LOAD_CLEAN SET epi_che=l_episode_number, spell_che=l_spell_number WHERE rowid=current_episode.rid; 
    END IF;
    previous_episode := current_episode;
    l_episode_number := l_episode_number + 1;
  END LOOP;
  CLOSE episode_cursor;
  DBMS_OUTPUT.PUT_LINE('episodes: ' || l_episode_number || ' spells: ' || l_spell_number);
END NUMBER_SPELLS;

create or replace PROCEDURE NUMBER_CIPS 
AUTHID CURRENT_USER
AS 
  TYPE episode_type IS RECORD(
  rid ROWID,
  spell_che INTEGER,
  extract_hesid VARCHAR2(32),
  admisorc INTEGER,
  admimeth INTEGER,
  disdest INTEGER,
  epistart DATE,
  epiend DATE
  );
  TYPE episode_cursor_type IS REF CURSOR;
  l_cips_number INTEGER;
  episode_cursor episode_cursor_type;
  current_episode episode_type;
  previous_episode episode_type;
BEGIN
  SELECT 
  NVL(MAX(cips_number),0)+1 
  INTO l_cips_number
  FROM HES_INPATIENT_EPISODE;
  
  OPEN episode_cursor FOR 
  SELECT rowid AS rid, spell_che, extract_hesid, admisorc, admimeth, disdest, epistart, epiend 
  FROM HES_INPATIENT_LOAD_CLEAN 
  ORDER BY extract_hesid, epistart, epiorder, epiend, transit, epikey;
  
  FETCH episode_cursor INTO current_episode;
  UPDATE HES_INPATIENT_LOAD_CLEAN SET cips_che=l_cips_number WHERE rowid=current_episode.rid; 
  previous_episode := current_episode;
  LOOP
    FETCH episode_cursor INTO current_episode;
    EXIT WHEN episode_cursor%NOTFOUND;
    IF current_episode.spell_che = previous_episode.spell_che OR
      (current_episode.extract_hesid = previous_episode.extract_hesid
      AND current_episode.epistart <= (2 + previous_episode.epiend)
      AND ( (previous_episode.disdest >= 51 AND previous_episode.disdest <= 53)
          OR (current_episode.admisorc >= 51 AND current_episode.admisorc <= 53)
          OR current_episode.admimeth=81 ) 
      )
    THEN
      UPDATE HES_INPATIENT_LOAD_CLEAN SET cips_che=l_cips_number WHERE rowid=current_episode.rid; 
    ELSE
      l_cips_number := l_cips_number + 1;
      UPDATE HES_INPATIENT_LOAD_CLEAN SET cips_che=l_cips_number WHERE rowid=current_episode.rid; 
    END IF;
    previous_episode := current_episode;
  END LOOP;
  CLOSE episode_cursor;
  DBMS_OUTPUT.PUT_LINE('cips: ' || l_cips_number);
END NUMBER_CIPS;

CREATE OR REPLACE PROCEDURE NORMALISE_HES_INPATIENT
AUTHID CURRENT_USER
AS 
  CURSOR episode_cursor IS SELECT * FROM HES_INPATIENT_LOAD_CLEAN ORDER BY extract_hesid, cips_che, spell_che, epi_che;
  current_patient HES_PATIENT%ROWTYPE;
  current_cips HES_INPATIENT_CIPS%ROWTYPE;
  current_spell HES_INPATIENT_SPELL%ROWTYPE;
  current_episode HES_INPATIENT_EPISODE%ROWTYPE;
  null_cips HES_INPATIENT_CIPS%ROWTYPE;
  null_spell HES_INPATIENT_SPELL%ROWTYPE;
  null_episode HES_INPATIENT_EPISODE%ROWTYPE;
  l_spell_number INTEGER;
  l_cips_number INTEGER;
BEGIN
    FOR curr_epi_w IN episode_cursor
    LOOP
      -- insert patient
      current_patient.extract_hesid := curr_epi_w.extract_hesid;
      current_patient.dob := curr_epi_w.dob_mmyyyy;
      current_patient.ethnos := curr_epi_w.ethnos;
      current_patient.sex := curr_epi_w.sex;
      INSERT INTO patient_temp VALUES current_patient;
      -- insert episode level data
      current_episode.episode_number := curr_epi_w.epi_che;
      current_episode.spell_number := curr_epi_w.spell_che;
      current_episode.cips_number := curr_epi_w.cips_che;
      current_episode.epistart := curr_epi_w.epistart;
      current_episode.epiend := curr_epi_w.epiend;
      current_episode.epidur := curr_epi_w.epidur;
      current_episode.epiorder := curr_epi_w.epiorder;
      current_episode.epitype := curr_epi_w.epitype;
      current_episode.mainspef := curr_epi_w.mainspef;
      current_episode.tretspef := curr_epi_w.tretspef;
      current_episode.operstat := curr_epi_w.operstat;
      current_episode.posopdur := curr_epi_w.posopdur;
      current_episode.preopdur := curr_epi_w.preopdur;
      current_episode.sitetret := curr_epi_w.sitetret;
      IF curr_epi_w.hrglate35 IS NOT NULL THEN
        current_episode.hrgcode := curr_epi_w.hrglate35;
        current_episode.hrgversion := '3.5';
      ELSIF curr_epi_w.hrglate IS NOT NULL THEN
        current_episode.hrgcode := curr_epi_w.hrglate;
        current_episode.hrgversion := '3.5';
      ELSIF curr_epi_w.hrg40 IS NOT NULL THEN
        current_episode.hrgcode := curr_epi_w.hrg40;
        current_episode.hrgversion := '4.0';      
      END IF;
      current_episode.pconsult := curr_epi_w.pconsult;
      current_episode.epikey := curr_epi_w.epikey;
      INSERT INTO HES_INPATIENT_EPISODE VALUES current_episode;
      current_episode := null_episode;
      -- insert all diagnosis and procedure codes
      IF curr_epi_w.diag_1 IS NOT NULL THEN
        INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_1, 1);
        IF curr_epi_w.diag_2 IS NOT NULL THEN
          INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_2, 0);
          IF curr_epi_w.diag_3 IS NOT NULL THEN
            INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_3, 0);
            IF curr_epi_w.diag_4 IS NOT NULL THEN
              INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_4, 0);
              IF curr_epi_w.diag_5 IS NOT NULL THEN
                INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_5, 0);
                IF curr_epi_w.diag_6 IS NOT NULL THEN
                  INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_6, 0);
                  IF curr_epi_w.diag_7 IS NOT NULL THEN
                    INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_7, 0);
                    IF curr_epi_w.diag_8 IS NOT NULL THEN
                      INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_8, 0);
                      IF curr_epi_w.diag_9 IS NOT NULL THEN
                        INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_9, 0);
                        IF curr_epi_w.diag_10 IS NOT NULL THEN
                          INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_10, 0);
                          IF curr_epi_w.diag_11 IS NOT NULL THEN
                            INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_11, 0);
                            IF curr_epi_w.diag_12 IS NOT NULL THEN
                              INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_12, 0);
                              IF curr_epi_w.diag_13 IS NOT NULL THEN
                                INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_13, 0);
                                IF curr_epi_w.diag_14 IS NOT NULL THEN
                                  INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_14, 0);
                                  IF curr_epi_w.diag_15 IS NOT NULL THEN
                                    INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_15, 0);
                                    IF curr_epi_w.diag_16 IS NOT NULL THEN
                                      INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_16, 0);
                                      IF curr_epi_w.diag_17 IS NOT NULL THEN
                                        INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_17, 0);
                                        IF curr_epi_w.diag_18 IS NOT NULL THEN
                                          INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_18, 0);
                                          IF curr_epi_w.diag_19 IS NOT NULL THEN
                                            INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_19, 0);
                                            IF curr_epi_w.diag_20 IS NOT NULL THEN
                                              INSERT INTO HES_INPATIENT_EPISODE_DIAG (episode_number, diag_code, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.diag_20, 0);
                                            END IF;
                                          END IF;
                                        END IF;
                                      END IF;
                                    END IF;
                                  END IF;
                                END IF;
                              END IF;
                            END IF;
                          END IF;
                        END IF;
                      END IF;
                    END IF;
                  END IF;
                END IF;
              END IF;
            END IF;
          END IF;
        END IF;
      END IF;

      IF curr_epi_w.opertn_1 IS NOT NULL THEN
        INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_1, curr_epi_w.opdate_1, 1);
        IF curr_epi_w.opertn_2 IS NOT NULL THEN
          INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_2, curr_epi_w.opdate_2, 0);
          IF curr_epi_w.opertn_3 IS NOT NULL THEN
            INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_3, curr_epi_w.opdate_3, 0);
            IF curr_epi_w.opertn_4 IS NOT NULL THEN
              INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_4, curr_epi_w.opdate_4, 0);
              IF curr_epi_w.opertn_5 IS NOT NULL THEN
                INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_5, curr_epi_w.opdate_5, 0);
                IF curr_epi_w.opertn_6 IS NOT NULL THEN
                  INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_6, curr_epi_w.opdate_6, 0);
                  IF curr_epi_w.opertn_7 IS NOT NULL THEN
                    INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_7, curr_epi_w.opdate_7, 0);
                    IF curr_epi_w.opertn_8 IS NOT NULL THEN
                      INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_8, curr_epi_w.opdate_8, 0);
                      IF curr_epi_w.opertn_9 IS NOT NULL THEN
                        INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_9, curr_epi_w.opdate_9, 0);
                        IF curr_epi_w.opertn_10 IS NOT NULL THEN
                          INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_10, curr_epi_w.opdate_10, 0);
                          IF curr_epi_w.opertn_11 IS NOT NULL THEN
                            INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_11, curr_epi_w.opdate_11, 0);
                            IF curr_epi_w.opertn_12 IS NOT NULL THEN
                              INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_12, curr_epi_w.opdate_12, 0);
                              IF curr_epi_w.opertn_13 IS NOT NULL THEN
                                INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_13, curr_epi_w.opdate_13, 0);
                                IF curr_epi_w.opertn_14 IS NOT NULL THEN
                                  INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_14, curr_epi_w.opdate_14, 0);
                                  IF curr_epi_w.opertn_15 IS NOT NULL THEN
                                    INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_15, curr_epi_w.opdate_15, 0);
                                    IF curr_epi_w.opertn_16 IS NOT NULL THEN
                                      INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_16, curr_epi_w.opdate_16, 0);
                                      IF curr_epi_w.opertn_17 IS NOT NULL THEN
                                        INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_17, curr_epi_w.opdate_17, 0);
                                        IF curr_epi_w.opertn_18 IS NOT NULL THEN
                                          INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_18, curr_epi_w.opdate_18, 0);
                                          IF curr_epi_w.opertn_19 IS NOT NULL THEN
                                            INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_19, curr_epi_w.opdate_19, 0);
                                            IF curr_epi_w.opertn_20 IS NOT NULL THEN
                                              INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_20, curr_epi_w.opdate_20, 0);
                                              IF curr_epi_w.opertn_21 IS NOT NULL THEN
                                                INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_21, curr_epi_w.opdate_21, 0);
                                                  IF curr_epi_w.opertn_22 IS NOT NULL THEN
                                                    INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_22, curr_epi_w.opdate_22, 0);
                                                    IF curr_epi_w.opertn_23 IS NOT NULL THEN
                                                      INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_23, curr_epi_w.opdate_23, 0);
                                                      IF curr_epi_w.opertn_24 IS NOT NULL THEN
                                                        INSERT INTO HES_INPATIENT_EPISODE_PROC (episode_number, procedure_code, procedure_date, is_primary) VALUES (curr_epi_w.epi_che, curr_epi_w.opertn_24, curr_epi_w.opdate_24, 0);
                                                      END IF;
                                                    END IF;
                                                  END IF;
                                              END IF;
                                            END IF;
                                          END IF;
                                        END IF;
                                      END IF;
                                    END IF;
                                  END IF;
                                END IF;
                              END IF;
                            END IF;
                          END IF;
                        END IF;
                      END IF;
                    END IF;
                  END IF;
                END IF;
              END IF;
            END IF;
          END IF;
        END IF;
      END IF;
  
      -- for first iteration set spells and cips
      IF l_spell_number IS NULL THEN
        l_spell_number := curr_epi_w.spell_che;
        l_cips_number := curr_epi_w.cips_che;
      END IF;  
  
      -- check if same spell, if changed insert spell level data, else update spell level data locally
      IF l_spell_number < curr_epi_w.spell_che THEN
        INSERT INTO HES_INPATIENT_SPELL VALUES current_spell;
        current_spell := null_spell;
      END IF;
 
     -- take most information only from the first episode
      IF current_spell.admidate IS NULL THEN
        l_spell_number := curr_epi_w.spell_che;
        current_spell.spell_number := curr_epi_w.spell_che;
        current_spell.cips_number := curr_epi_w.cips_che;
        current_spell.admidate := curr_epi_w.admidate;
        current_spell.admimeth := curr_epi_w.admimeth;
        current_spell.admisorc := curr_epi_w.admisorc;
        current_spell.admincat := curr_epi_w.admincat;
        current_spell.admistat := curr_epi_w.admistat;
        current_spell.classpat := curr_epi_w.classpat;
        current_spell.elecdate := curr_epi_w.elecdate;
        current_spell.elecdur := curr_epi_w.elecdur;
        current_spell.purcode := curr_epi_w.purcode;
        current_spell.procode := curr_epi_w.procode;
        current_spell.protype := curr_epi_w.protype;
        current_spell.preferer := curr_epi_w.preferer;
        current_spell.referorg := curr_epi_w.referorg;
      END IF;
      -- overwrite discharge information so we only keep information from final episode
      current_spell.disdate := curr_epi_w.disdate;
      current_spell.disdest := curr_epi_w.disdest;
      current_spell.disreadydate := curr_epi_w.disreadydate;
      current_spell.dismeth := curr_epi_w.dismeth;

      -- check if same CIPS, if changed insert CIPS level data
      IF l_cips_number < curr_epi_w.cips_che THEN
        INSERT INTO HES_INPATIENT_CIPS VALUES current_cips;
        current_cips := null_cips;
      END IF;
      -- take most information only from the first episode
      IF current_cips.admidate IS NULL THEN
        l_cips_number := curr_epi_w.cips_che;
        current_cips.extract_hesid := curr_epi_w.extract_hesid;
        current_cips.cips_number := curr_epi_w.cips_che;
        current_cips.admidate := curr_epi_w.admidate;
        current_cips.admimeth := curr_epi_w.admimeth;
        current_cips.admisorc := curr_epi_w.admisorc;
        current_cips.admincat := curr_epi_w.admincat;
        current_cips.admistat := curr_epi_w.admistat;
        current_cips.classpat := curr_epi_w.classpat;
        current_cips.elecdate := curr_epi_w.elecdate;
        current_cips.elecdur := curr_epi_w.elecdur;
        current_cips.soal := curr_epi_w.soal;
      END IF;
      -- overwrite discharge information so we only keep information from final episode
      current_cips.disdate := curr_epi_w.disdate;
      current_cips.disdest := curr_epi_w.disdest;
      current_cips.disreadydate := curr_epi_w.disreadydate;
      current_cips.dismeth := curr_epi_w.dismeth;

    END LOOP;

    -- save final spell and CIPS
    INSERT INTO HES_INPATIENT_SPELL VALUES current_spell;
    INSERT INTO HES_INPATIENT_CIPS VALUES current_cips;
    
    -- merge unique selection from patient_temp into the hes_patient table
    MERGE INTO HES_PATIENT a
    USING (SELECT extract_hesid, MIN(dob) dob, MIN(ethnos) ethnos, MIN(sex) sex FROM PATIENT_TEMP GROUP BY extract_hesid) b
    ON (a.extract_hesid = b.extract_hesid)
    WHEN MATCHED THEN
      UPDATE SET
        dob = b.dob,
        ethnos = b.ethnos,
        sex = b.sex
    WHEN NOT MATCHED THEN
      INSERT (extract_hesid, dob, ethnos, sex)
      VALUES (b.extract_hesid, b.dob, b.ethnos, b.sex);

      DBMS_OUTPUT.PUT_LINE('normalised spells: ' || l_spell_number || ' normalised CIPS: ' || l_cips_number);
END NORMALISE_HES_INPATIENT;

create or replace PROCEDURE cleanInpatientTable(table_name IN VARCHAR2, hrg_col_name IN VARCHAR2)
AUTHID CURRENT_USER
AS
BEGIN
  -- truncate temporary tables
  EXECUTE IMMEDIATE 'TRUNCATE TABLE HES_INPATIENT_LOAD';  
  EXECUTE IMMEDIATE 'TRUNCATE TABLE HES_INPATIENT_LOAD_CLEAN'; 
  EXECUTE IMMEDIATE 'TRUNCATE TABLE patient_temp';
  
	-- start by dropping nonsense records and creating row quality metrics and episode, cips and spells placeholders
  -- by selecting into a temporary table
  EXECUTE IMMEDIATE 'INSERT INTO HES_INPATIENT_LOAD
	SELECT t.*, 0 AS TRANSIT, 0 AS EPI_CHE, 0 AS SPELL_CHE, 0 AS CIPS_CHE, 
	CASE ' || hrg_col_name || ' WHEN ''U01'' THEN 1 WHEN ''U02'' THEN 2 WHEN ''U04'' THEN 4 WHEN ''U05'' THEN 5 WHEN ''U07'' THEN 7 WHEN ''U09'' THEN 9 ELSE 0 END AS U,
	(CASE MAINSPEF WHEN NULL THEN 1 ELSE 0 END + CASE TRETSPEF WHEN NULL THEN 1 ELSE 0 END + CASE OPERTN_1 WHEN NULL THEN 1 ELSE 0 END + CASE EPIORDER WHEN NULL THEN 1 ELSE 0 END) AS ROW_QUALITY
	FROM ' || table_name || ' t 
	WHERE 
	EPISTAT=3 AND 
	ADMIDATE IS NOT NULL AND 
	PROCODE IS NOT NULL AND 
	EXTRACT_HESID IS NOT NULL AND 
	EPIKEY IS NOT NULL AND
	ADMIDATE <= NVL(DISDATE, ADMIDATE)';   
  DBMS_OUTPUT.PUT_LINE('dropped bad data...');

	-- update transit variable
	UPDATE HES_INPATIENT_LOAD SET TRANSIT = 1 WHERE ((ADMISORC<51 OR ADMISORC>53) AND ADMIMETH!=81) AND (DISDEST>=51 AND DISDEST<=53);
	UPDATE HES_INPATIENT_LOAD SET TRANSIT = 3 WHERE ((ADMISORC>=51 OR ADMISORC<=53) AND ADMIMETH=81) AND (DISDEST<51 OR DISDEST>53);
	UPDATE HES_INPATIENT_LOAD SET TRANSIT = 2 WHERE ((ADMISORC>=51 OR ADMISORC<=53) AND ADMIMETH=81) AND (DISDEST>=51 AND DISDEST<=53);
  DBMS_OUTPUT.PUT_LINE('updated transit variables...');

  -- drop duplicates ordered by quality keeping best and recreate a cleaned version of the original table
  INSERT INTO HES_INPATIENT_LOAD_CLEAN 
  SELECT * FROM
	(SELECT HES_INPATIENT_LOAD.*, ROW_NUMBER() OVER (PARTITION BY EXTRACT_HESID, EPISTART, EPIORDER, EPIEND, TRANSIT ORDER BY ADMIDATE, DISDATE, U, ROW_QUALITY) RN FROM HES_INPATIENT_LOAD) 
	WHERE RN=1;
  DBMS_OUTPUT.PUT_LINE('dropped duplicates...');

   -- add spell numbers into the new table
  NUMBER_SPELLS();
  DBMS_OUTPUT.PUT_LINE('calculated spells...');
  -- add CIPS numbers into the new table
  NUMBER_CIPS();
  DBMS_OUTPUT.PUT_LINE('calculated CIPS...');
  -- normalise tables into patient, CIPS, spells, episode, diagnosis and procedures
  NORMALISE_HES_INPATIENT();
  DBMS_OUTPUT.PUT_LINE('normalised data structure...');
  
END cleanInpatientTable;

---------------------------------
-- load inpatient data
---------------------------------

  CREATE GLOBAL TEMPORARY TABLE "HEPI"."HES_OUTPATIENT_LOAD" 
   (	"APPTDATE" DATE, 
	"ATTENTYPE" NUMBER(*,0), 
	"ATTENDED" NUMBER(*,0), 
	"WAITING" NUMBER(*,0), 
	"FIRSTATT" NUMBER(*,0), 
	"DNADATE" DATE, 
	"STAFFTYP" NUMBER(*,0), 
	"OUTCOME" NUMBER(*,0), 
	"PRIORITY" NUMBER(*,0), 
	"REQDATE" DATE, 
	"SERVTYPE" NUMBER(*,0), 
	"REFSOURC" NUMBER(*,0), 
	"DIAG_01" VARCHAR2(4 BYTE), 
	"DIAG_02" VARCHAR2(4 BYTE), 
	"DIAG_03" VARCHAR2(4 BYTE), 
	"DIAG_04" VARCHAR2(4 BYTE), 
	"DIAG_05" VARCHAR2(4 BYTE), 
	"DIAG_06" VARCHAR2(4 BYTE), 
	"DIAG_07" VARCHAR2(4 BYTE), 
	"DIAG_08" VARCHAR2(4 BYTE), 
	"DIAG_09" VARCHAR2(4 BYTE), 
	"DIAG_10" VARCHAR2(4 BYTE), 
	"DIAG_11" VARCHAR2(4 BYTE), 
	"DIAG_12" VARCHAR2(4 BYTE), 
	"OPERTN_01" VARCHAR2(4 BYTE), 
	"OPERTN_02" VARCHAR2(4 BYTE), 
	"OPERTN_03" VARCHAR2(4 BYTE), 
	"OPERTN_04" VARCHAR2(4 BYTE), 
	"OPERTN_05" VARCHAR2(4 BYTE), 
	"OPERTN_06" VARCHAR2(4 BYTE), 
	"OPERTN_07" VARCHAR2(4 BYTE), 
	"OPERTN_08" VARCHAR2(4 BYTE), 
	"OPERTN_09" VARCHAR2(4 BYTE), 
	"OPERTN_10" VARCHAR2(4 BYTE), 
	"OPERTN_11" VARCHAR2(4 BYTE), 
	"OPERTN_12" VARCHAR2(4 BYTE), 
	"OPERTN_13" VARCHAR2(4 BYTE), 
	"OPERTN_14" VARCHAR2(4 BYTE), 
	"OPERTN_15" VARCHAR2(4 BYTE), 
	"OPERTN_16" VARCHAR2(4 BYTE), 
	"OPERTN_17" VARCHAR2(4 BYTE), 
	"OPERTN_18" VARCHAR2(4 BYTE), 
	"OPERTN_19" VARCHAR2(4 BYTE), 
	"OPERTN_20" VARCHAR2(4 BYTE), 
	"OPERTN_21" VARCHAR2(4 BYTE), 
	"OPERTN_22" VARCHAR2(4 BYTE), 
	"OPERTN_23" VARCHAR2(4 BYTE), 
	"OPERTN_24" VARCHAR2(4 BYTE), 
	"MAINSPEF" VARCHAR2(4 BYTE), 
	"TRETSPEF" VARCHAR2(4 BYTE), 
	"SOAL" VARCHAR2(10 BYTE), 
	"SITETRET" VARCHAR2(10 BYTE), 
	"PURCODE" VARCHAR2(6 BYTE), 
	"PROCODE" VARCHAR2(6 BYTE), 
	"PROTYPE" VARCHAR2(200 BYTE), 
	"PREFERER" VARCHAR2(16 BYTE), 
	"ADMINCAT" NUMBER(*,0), 
	"DOB" DATE, 
	"ETHNOS" VARCHAR2(1 BYTE), 
	"EXTRACT_HESID" VARCHAR2(32 BYTE), 
	"SEX" NUMBER(*,0), 
	"PCONSULT" VARCHAR2(16 BYTE), 
	"REFERORG" VARCHAR2(6 BYTE), 
	"ATTENDKEY" NUMBER, 
	"APPT_CHE" NUMBER, 
	"POC_CHE" NUMBER, 
	"ROW_QUALITY" NUMBER
   ) ON COMMIT PRESERVE ROWS ;

     CREATE GLOBAL TEMPORARY TABLE "HEPI"."HES_OUTPATIENT_LOAD_CLEAN" 
   (	"APPTDATE" DATE, 
	"ATTENTYPE" NUMBER(*,0), 
	"ATTENDED" NUMBER(*,0), 
	"WAITING" NUMBER(*,0), 
	"FIRSTATT" NUMBER(*,0), 
	"DNADATE" DATE, 
	"STAFFTYP" NUMBER(*,0), 
	"OUTCOME" NUMBER(*,0), 
	"PRIORITY" NUMBER(*,0), 
	"REQDATE" DATE, 
	"SERVTYPE" NUMBER(*,0), 
	"REFSOURC" NUMBER(*,0), 
	"DIAG_01" VARCHAR2(4 BYTE), 
	"DIAG_02" VARCHAR2(4 BYTE), 
	"DIAG_03" VARCHAR2(4 BYTE), 
	"DIAG_04" VARCHAR2(4 BYTE), 
	"DIAG_05" VARCHAR2(4 BYTE), 
	"DIAG_06" VARCHAR2(4 BYTE), 
	"DIAG_07" VARCHAR2(4 BYTE), 
	"DIAG_08" VARCHAR2(4 BYTE), 
	"DIAG_09" VARCHAR2(4 BYTE), 
	"DIAG_10" VARCHAR2(4 BYTE), 
	"DIAG_11" VARCHAR2(4 BYTE), 
	"DIAG_12" VARCHAR2(4 BYTE), 
	"OPERTN_01" VARCHAR2(4 BYTE), 
	"OPERTN_02" VARCHAR2(4 BYTE), 
	"OPERTN_03" VARCHAR2(4 BYTE), 
	"OPERTN_04" VARCHAR2(4 BYTE), 
	"OPERTN_05" VARCHAR2(4 BYTE), 
	"OPERTN_06" VARCHAR2(4 BYTE), 
	"OPERTN_07" VARCHAR2(4 BYTE), 
	"OPERTN_08" VARCHAR2(4 BYTE), 
	"OPERTN_09" VARCHAR2(4 BYTE), 
	"OPERTN_10" VARCHAR2(4 BYTE), 
	"OPERTN_11" VARCHAR2(4 BYTE), 
	"OPERTN_12" VARCHAR2(4 BYTE), 
	"OPERTN_13" VARCHAR2(4 BYTE), 
	"OPERTN_14" VARCHAR2(4 BYTE), 
	"OPERTN_15" VARCHAR2(4 BYTE), 
	"OPERTN_16" VARCHAR2(4 BYTE), 
	"OPERTN_17" VARCHAR2(4 BYTE), 
	"OPERTN_18" VARCHAR2(4 BYTE), 
	"OPERTN_19" VARCHAR2(4 BYTE), 
	"OPERTN_20" VARCHAR2(4 BYTE), 
	"OPERTN_21" VARCHAR2(4 BYTE), 
	"OPERTN_22" VARCHAR2(4 BYTE), 
	"OPERTN_23" VARCHAR2(4 BYTE), 
	"OPERTN_24" VARCHAR2(4 BYTE), 
	"MAINSPEF" VARCHAR2(4 BYTE), 
	"TRETSPEF" VARCHAR2(4 BYTE), 
	"SOAL" VARCHAR2(10 BYTE), 
	"SITETRET" VARCHAR2(10 BYTE), 
	"PURCODE" VARCHAR2(6 BYTE), 
	"PROCODE" VARCHAR2(6 BYTE), 
	"PROTYPE" VARCHAR2(200 BYTE), 
	"PREFERER" VARCHAR2(16 BYTE), 
	"ADMINCAT" NUMBER(*,0), 
	"DOB" DATE, 
	"ETHNOS" VARCHAR2(1 BYTE), 
	"EXTRACT_HESID" VARCHAR2(32 BYTE), 
	"SEX" NUMBER(*,0), 
	"PCONSULT" VARCHAR2(16 BYTE), 
	"REFERORG" VARCHAR2(6 BYTE), 
	"ATTENDKEY" NUMBER, 
	"APPT_CHE" NUMBER, 
	"POC_CHE" NUMBER, 
	"ROW_QUALITY" NUMBER,
	"RN" NUMBER
   ) ON COMMIT PRESERVE ROWS ;

   CREATE OR REPLACE PROCEDURE NUMBER_POC 
AUTHID CURRENT_USER
AS
  TYPE appt_type IS RECORD(
  rid ROWID,
  extract_hesid VARCHAR2(32),
  firstatt INTEGER,
  outcome INTEGER,
  reqdate DATE
  );
  TYPE appt_cursor_type IS REF CURSOR;
  l_appt_number INTEGER;
  l_poc_number INTEGER;
  appt_cursor appt_cursor_type;
  current_appt appt_type;
  previous_appt appt_type;
BEGIN
  SELECT 
  NVL(MAX(appointment_number),0)+1, NVL(MAX(period_of_care_number),0)+1 
  INTO l_appt_number, l_poc_number
  FROM HES_OUTPATIENT_APPOINTMENT;
  
  OPEN appt_cursor FOR 
    SELECT rowid AS rid, extract_hesid, firstatt, outcome, reqdate 
    FROM HES_OUTPATIENT_LOAD_CLEAN
    ORDER BY extract_hesid ASC, apptdate ASC, firstatt ASC, outcome DESC;
  
  FETCH appt_cursor INTO current_appt;
  UPDATE HES_OUTPATIENT_LOAD_CLEAN SET appt_che=l_appt_number, poc_che=l_poc_number WHERE rowid=current_appt.rid; 
  previous_appt := current_appt;
  l_appt_number := l_appt_number + 1;
  LOOP
    FETCH appt_cursor INTO current_appt;
    EXIT WHEN appt_cursor%NOTFOUND;
    
    IF current_appt.extract_hesid = previous_appt.extract_hesid
      AND NVL(current_appt.reqdate,sysdate) = NVL(previous_appt.reqdate,sysdate)
      AND 
      ((current_appt.firstatt IN (2,4)) 
      OR 
      (previous_appt.outcome > 1))
    THEN
      UPDATE HES_OUTPATIENT_LOAD_CLEAN SET appt_che=l_appt_number, poc_che=l_poc_number WHERE rowid=current_appt.rid; 
    ELSE
      l_poc_number := l_poc_number + 1;
      UPDATE HES_OUTPATIENT_LOAD_CLEAN SET appt_che=l_appt_number, poc_che=l_poc_number WHERE rowid=current_appt.rid; 
    END IF;
    previous_appt := current_appt;
    l_appt_number := l_appt_number + 1;
  END LOOP;
  CLOSE appt_cursor;
  
  DBMS_OUTPUT.PUT_LINE('appointments: ' || l_appt_number || ' periods of care: ' || l_poc_number);
END NUMBER_POC;

CREATE OR REPLACE PROCEDURE NORMALISE_HES_OUTPATIENT 
AUTHID CURRENT_USER
AS 
  CURSOR appt_cursor IS SELECT * FROM HES_OUTPATIENT_LOAD_CLEAN ORDER BY extract_hesid, poc_che, appt_che;
  current_patient HES_PATIENT%ROWTYPE;
  current_poc HES_OUTPATIENT_PERIOD_OF_CARE%ROWTYPE;
  current_appt HES_OUTPATIENT_APPOINTMENT%ROWTYPE;
  null_poc HES_OUTPATIENT_PERIOD_OF_CARE%ROWTYPE;
  null_appt HES_OUTPATIENT_APPOINTMENT%ROWTYPE;
  l_poc_number INTEGER;
  l_appt_number INTEGER;
BEGIN
    FOR current_appt_w IN appt_cursor
    LOOP
      -- insert patient
      current_patient.extract_hesid := current_appt_w.extract_hesid;
      current_patient.dob := current_appt_w.dob;
      current_patient.ethnos := current_appt_w.ethnos;
      current_patient.sex := current_appt_w.sex;
      INSERT INTO patient_temp VALUES current_patient;
      -- insert appointment level data
      current_appt.appointment_number := current_appt_w.appt_che;
      current_appt.period_of_care_number := current_appt_w.poc_che;
      current_appt.apptdate := current_appt_w.apptdate;
      current_appt.attentype := current_appt_w.attentype;
      current_appt.attended := current_appt_w.attended;
      current_appt.firstatt := current_appt_w.firstatt;
      current_appt.dnadate := current_appt_w.dnadate;
      current_appt.waiting := current_appt_w.waiting;
      current_appt.stafftyp := current_appt_w.stafftyp;
      current_appt.outcome := current_appt_w.outcome;
      current_appt.priority := current_appt_w.priority;
      current_appt.servtype := current_appt_w.servtype;
      current_appt.mainspef := current_appt_w.mainspef;
      current_appt.tretspef := current_appt_w.tretspef;
      current_appt.soal := current_appt_w.soal;
      current_appt.sitetret := current_appt_w.sitetret;
      current_appt.procode := current_appt_w.procode;
      current_appt.protype := current_appt_w.protype;
      current_appt.pconsult := current_appt_w.pconsult;
      current_appt.admincat := current_appt_w.admincat;
      current_appt.attendkey := current_appt_w.attendkey;
 
      INSERT INTO HES_OUTPATIENT_APPOINTMENT VALUES current_appt;
      current_appt := null_appt;
 
      -- for first iteration set POC number
      IF l_poc_number IS NULL THEN
        l_poc_number := current_appt_w.poc_che;
      END IF;  
  
      -- check if same POC, if changed insert POC level data, else update POC level data locally
      IF l_poc_number < current_appt_w.poc_che THEN
        INSERT INTO HES_OUTPATIENT_PERIOD_OF_CARE VALUES current_poc;
        current_poc := null_poc;
      END IF;
 
     -- take most information only from the first episode
      IF current_poc.period_of_care_number IS NULL THEN
        l_poc_number := current_appt_w.poc_che;
        current_poc.period_of_care_number := current_appt_w.poc_che;
        current_poc.extract_hesid := current_appt_w.extract_hesid;
        current_poc.reqdate := current_appt_w.reqdate;
        current_poc.firstapptdate := current_appt_w.apptdate;
        current_poc.refsourc := current_appt_w.refsourc;
        current_poc.purcode := current_appt_w.purcode;
        current_poc.preferer := current_appt_w.preferer;
        current_poc.referorg := current_appt_w.referorg;
      END IF;
      -- overwrite discharge information so we only keep information from final appointment
      current_poc.disdate := current_appt_w.apptdate;
      current_poc.outcome := current_appt_w.outcome;
      
      -- insert all diagnosis and procedure codes
      IF current_appt_w.diag_01 IS NOT NULL THEN
        INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_01, 1);
        IF current_appt_w.diag_02 IS NOT NULL THEN
          INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_02, 0);
          IF current_appt_w.diag_03 IS NOT NULL THEN
            INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_03, 0);
            IF current_appt_w.diag_04 IS NOT NULL THEN
              INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_04, 0);
              IF current_appt_w.diag_05 IS NOT NULL THEN
                INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_05, 0);
                IF current_appt_w.diag_06 IS NOT NULL THEN
                  INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_06, 0);
                  IF current_appt_w.diag_07 IS NOT NULL THEN
                    INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_07, 0);
                    IF current_appt_w.diag_08 IS NOT NULL THEN
                      INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_08, 0);
                      IF current_appt_w.diag_09 IS NOT NULL THEN
                        INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_09, 0);
                        IF current_appt_w.diag_10 IS NOT NULL THEN
                          INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_10, 0);
                          IF current_appt_w.diag_11 IS NOT NULL THEN
                            INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_11, 0);
                            IF current_appt_w.diag_12 IS NOT NULL THEN
                              INSERT INTO HES_OUTPATIENT_APPT_DIAG (appointment_number, diag_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.diag_12, 0);
                            END IF;
                          END IF;
                        END IF;
                      END IF;
                    END IF;
                  END IF;
                END IF;
              END IF;
            END IF;
          END IF;
        END IF;
      END IF;

      IF current_appt_w.opertn_01 IS NOT NULL THEN
        INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_01, 1);
        IF current_appt_w.opertn_02 IS NOT NULL THEN
          INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_02, 0);
          IF current_appt_w.opertn_03 IS NOT NULL THEN
            INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_03, 0);
            IF current_appt_w.opertn_04 IS NOT NULL THEN
              INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_04, 0);
              IF current_appt_w.opertn_05 IS NOT NULL THEN
                INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_05, 0);
                IF current_appt_w.opertn_06 IS NOT NULL THEN
                  INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_06, 0);
                  IF current_appt_w.opertn_07 IS NOT NULL THEN
                    INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_07, 0);
                    IF current_appt_w.opertn_08 IS NOT NULL THEN
                      INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_08, 0);
                      IF current_appt_w.opertn_09 IS NOT NULL THEN
                        INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_09, 0);
                        IF current_appt_w.opertn_10 IS NOT NULL THEN
                          INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_10, 0);
                          IF current_appt_w.opertn_11 IS NOT NULL THEN
                            INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_11, 0);
                            IF current_appt_w.opertn_12 IS NOT NULL THEN
                              INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_12, 0);
                              IF current_appt_w.opertn_13 IS NOT NULL THEN
                                INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_13, 0);
                                IF current_appt_w.opertn_14 IS NOT NULL THEN
                                  INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_14, 0);
                                  IF current_appt_w.opertn_15 IS NOT NULL THEN
                                    INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_15, 0);
                                    IF current_appt_w.opertn_16 IS NOT NULL THEN
                                      INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_16, 0);
                                      IF current_appt_w.opertn_17 IS NOT NULL THEN
                                        INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_17, 0);
                                        IF current_appt_w.opertn_18 IS NOT NULL THEN
                                          INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_18, 0);
                                          IF current_appt_w.opertn_19 IS NOT NULL THEN
                                            INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_19, 0);
                                            IF current_appt_w.opertn_20 IS NOT NULL THEN
                                              INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_20, 0);
                                              IF current_appt_w.opertn_21 IS NOT NULL THEN
                                                INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_21, 0);
                                                  IF current_appt_w.opertn_22 IS NOT NULL THEN
                                                    INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_22, 0);
                                                    IF current_appt_w.opertn_23 IS NOT NULL THEN
                                                      INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_23, 0);
                                                      IF current_appt_w.opertn_24 IS NOT NULL THEN
                                                        INSERT INTO HES_OUTPATIENT_APPT_PROC (appointment_number, procedure_code, is_primary) VALUES (current_appt_w.appt_che, current_appt_w.opertn_24, 0);
                                                      END IF;
                                                    END IF;
                                                  END IF;
                                              END IF;
                                            END IF;
                                          END IF;
                                        END IF;
                                      END IF;
                                    END IF;
                                  END IF;
                                END IF;
                              END IF;
                            END IF;
                          END IF;
                        END IF;
                      END IF;
                    END IF;
                  END IF;
                END IF;
              END IF;
            END IF;
          END IF;
        END IF;
      END IF;

    END LOOP;

    -- save final POC
    INSERT INTO HES_OUTPATIENT_PERIOD_OF_CARE VALUES current_poc;
    
    -- merge unique selection from patient_temp into the hes_patient table
    MERGE INTO HES_PATIENT a
    USING (SELECT extract_hesid, MIN(dob) dob, MIN(ethnos) ethnos, MIN(sex) sex FROM PATIENT_TEMP GROUP BY extract_hesid) b
    ON (a.extract_hesid = b.extract_hesid)
      -- if matched i.e. inpatient record already contains patient data 
      -- then ignore outpatient data as the data quality is often worse
    WHEN NOT MATCHED THEN
      INSERT (extract_hesid, dob, ethnos, sex)
      VALUES (b.extract_hesid, b.dob, b.ethnos, b.sex);

    DBMS_OUTPUT.PUT_LINE('normalised appointments: ' || l_appt_number || ' normalised perriods of care: ' || l_poc_number);

END NORMALISE_HES_OUTPATIENT;

CREATE OR REPLACE PROCEDURE CLEAN_OUTPATIENT_TABLE(table_name IN VARCHAR2) 
AUTHID CURRENT_USER
AS 
BEGIN
  -- truncate temporary tables
  EXECUTE IMMEDIATE 'TRUNCATE TABLE HES_OUTPATIENT_LOAD';  
  EXECUTE IMMEDIATE 'TRUNCATE TABLE HES_OUTPATIENT_LOAD_CLEAN'; 
  EXECUTE IMMEDIATE 'TRUNCATE TABLE patient_temp';

	-- start by dropping nonsense records and creating row quality metrics and episode, cips and spells placeholders
  -- by selecting into a temporary table
  EXECUTE IMMEDIATE 'INSERT INTO HES_OUTPATIENT_LOAD
	SELECT t.*, 0 AS APPT_CHE, 0 AS POC_CHE, 
	(CASE MAINSPEF WHEN NULL THEN 1 ELSE 0 END + 
  CASE TRETSPEF WHEN NULL THEN 1 ELSE 0 END + 
  CASE reqdate WHEN NULL THEN 1 ELSE 0 END + 
  CASE refsourc WHEN NULL THEN 1 ELSE 0 END + 
  CASE priority WHEN NULL THEN 1 ELSE 0 END) AS ROW_QUALITY
	FROM ' || table_name || ' t 
	WHERE 
	APPTDATE IS NOT NULL AND 
	PROCODE IS NOT NULL AND 
	EXTRACT_HESID IS NOT NULL AND 
	PCONSULT IS NOT NULL AND
	APPTDATE >= NVL(REQDATE, APPTDATE)';   
  DBMS_OUTPUT.PUT_LINE('dropped bad data...');

  -- drop duplicates ordered by quality keeping best and recreate a cleaned version of the original table
  INSERT INTO HES_OUTPATIENT_LOAD_CLEAN 
  SELECT * FROM
	(SELECT HES_OUTPATIENT_LOAD.*, ROW_NUMBER() OVER (PARTITION BY EXTRACT_HESID, APPTDATE, PCONSULT, NVL(REQDATE,sysdate), NVL(OUTCOME,0) ORDER BY ROW_QUALITY) RN FROM HES_OUTPATIENT_LOAD) 
	WHERE RN=1;
  DBMS_OUTPUT.PUT_LINE('dropped duplicates...');

  -- add period of care numbers into the new table
  NUMBER_POC();
  DBMS_OUTPUT.PUT_LINE('calculated POC...');
  -- normalise tables into patient, CIPS, spells, episode, diagnosis and procedures
  NORMALISE_HES_OUTPATIENT();
  DBMS_OUTPUT.PUT_LINE('normalised data structure...');

END CLEAN_OUTPATIENT_TABLE;