
CREATE TABLE QOF_PRACTICE(
 year NUMBER(4,0),
 practicecode VARCHAR2(8),
 indnum NUMBER(3,0),
 indlabel VARCHAR2(255),
 QMASname VARCHAR2(50),
 num INTEGER,
 den INTEGER, 
 perc NUMBER, 
 popa NUMBER, 
 excnum INTEGER, 
 icexc INTEGER, 
 lt INTEGER, 
 ut INTEGER, 
 points INTEGER,
 paRA NUMBER,
 paPA NUMBER
 ) TABLESPACE HEPI;

CREATE TABLE qof_lsoa AS
SELECT qof.year, a.lsoa AS lsoa01cd,  qof.indnum, qof.indlabel, qof.QMASname, 
SUM(a.lsoa_prop*qof.num) num,
SUM(a.lsoa_prop*qof.den) den,
SUM(a.lsoa_prop*qof.excnum) excnum,
qof.lt, qof.ut
FROM
QOF_PRACTICE qof
INNER JOIN
ADS_PROPORTIONS a
ON a.year=qof.year AND a.prcode=qof.practicecode
GROUP BY qof.year, a.lsoa,  qof.indnum, qof.indlabel, qof.QMASname, qof.lt, qof.ut;
