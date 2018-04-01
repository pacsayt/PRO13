-- f1dbkiur.sql
-- 1999.01.23
-- Az f1 adatbazis inicializalasa

-- A tablak torlese
-- A sequence-k eldobasa es letrehozasa

DELETE FROM leggyorsabb_kor ;
DELETE FROM befuto      ;
DELETE FROM rajtsorrend ;
DELETE FROM nagydijak   ;
DELETE FROM nevezesi_lista ;
DELETE FROM szezon      ;
DELETE FROM pontozas    ;
DELETE FROM vers_nev ;
DELETE FROM nemz_nev ;
DELETE FROM csapat_nev  ;
DELETE FROM motor_nev   ;
DELETE FROM nagydij_nev ;
DELETE FROM palya_nev   ;

DROP SEQUENCE seq_vnev  ;
DROP SEQUENCE seq_nnev  ;
DROP SEQUENCE seq_csnev ;
DROP SEQUENCE seq_mnev  ;
DROP SEQUENCE seq_ndnev ;
DROP SEQUENCE seq_pnev  ;
DROP SEQUENCE seq_pont  ;
DROP SEQUENCE seq_nvzl  ;

CREATE SEQUENCE seq_vnev  START WITH 1 MAXVALUE 4294967295 ;
CREATE SEQUENCE seq_nnev  START WITH 1 MAXVALUE 4294967295 ;
CREATE SEQUENCE seq_csnev START WITH 1 MAXVALUE 4294967295 ;
CREATE SEQUENCE seq_mnev  START WITH 1 MAXVALUE 4294967295 ;
CREATE SEQUENCE seq_ndnev START WITH 1 MAXVALUE 4294967295 ;
CREATE SEQUENCE seq_pnev  START WITH 1 MAXVALUE 4294967295 ;
CREATE SEQUENCE seq_pont  START WITH 1 MAXVALUE 4294967295 ;
CREATE SEQUENCE seq_nvzl  START WITH 1 MAXVALUE 4294967295 ;

-- Ez valoszinuleg felesleges, mert a DELETE utani elso
-- DDL utasitasnal automatikus COMMIT hajtodik vegre
COMMIT WORK ;