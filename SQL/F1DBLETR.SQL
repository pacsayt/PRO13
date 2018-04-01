-- f1dbletr.sql (korabban: tabletr.sql)
-- 1998.07.07
-- Az f1 adatbazis adatstrukturajanak letrehozasa

-- 2000.07.04 A rajtsorrend/befuto tablak mezoinek tipusa atalakitva VARCHAR2(16)
--            -ve, SQL-utasitasok altal kezelhetove.
--            Maga az osztaly tovabbra is bitekkel operal, az SQL_* fv.-ek
--            konvertalnak. Az egyes karakterpoziciok megfelelnek a bitpozicioknak.

-- 2000.03.02 A CONSTRAINT-ek a rajtsorrend/befuto, rajtsorrend/leggyorsabb_kor
--            tablak kozott megszuntetve, mert nem tudtam a rajtsorrend tabla
--            megjegyzes mezejenek allapotat itt figyelembe venni
--            (megj.!=16 torolt bejegyzesnek szamit)

-- 2000.01.18 uk_vs_nmz_cs_m - kiveve, mert arrol, hogy egy rajsorrendben
--            egyszerre csak egy rsz ill. versenyzo szerepeljen a rajts_insert
--            gondoskodik

-- 1999.12.19 Uj CONSTRAINT-ek hozzaadasa :
--            rajtsorrend     : uk_rs_sze_ndn_hlyz
--            befuto          : uk_bef_sze_ndn_nvzl, uk_bef_sze_ndn_hlyz
--            leggyorsabb_kor : uk_lgyk_sze_ndn

-- 1999.10.17 A nevezesi_lista tablahoz meg egy UNIQUE KEY
--            CONSTRAINT (uk_vs_nmz_cs_m) hozzaadva, biztositva
--            ezaltal, hogy egy nagydijon egy rajtszam mellett
--            csak egy azonos nevezesi lista sor szerepeljen

-- 1999.10.07 a szezon tablahoz 'chk_szez_ev' hozzaadva a
--            biztositando, hogy csak ervenyes F1 szezon
--            keruljon be az adatbazisba (szezon_eve>=1950)

-- 1999.04.17 nev tablak ..._nev mezokre UNIQUE KEY NOT NULL
--            lett huzva, hogy az adatbazis dontse el van-e
--            mar ilyen nev

-- 1999.02.28 pontozas tabla: PK( kulcs, helyezes) UK torolve
--            szezon -> pontozas FK megszunt ezaltal

DROP TABLE leggyorsabb_kor;
DROP TABLE befuto      ;
DROP TABLE rajtsorrend ;

DROP TABLE nagydijak   ;
DROP TABLE nevezesi_lista ;

DROP TABLE szezon      ;
DROP TABLE pontozas    ;

DROP TABLE vers_nev ;
DROP TABLE nemz_nev ;
DROP TABLE csapat_nev  ;
DROP TABLE motor_nev   ;
DROP TABLE nagydij_nev ;
DROP TABLE palya_nev   ;

DROP TABLE seq      ;

DROP SEQUENCE seq_vnev  ;
DROP SEQUENCE seq_nnev  ;
DROP SEQUENCE seq_csnev ;
DROP SEQUENCE seq_mnev  ;
DROP SEQUENCE seq_ndnev ;
DROP SEQUENCE seq_pnev  ;
DROP SEQUENCE seq_pont  ;
DROP SEQUENCE seq_nvzl  ;

-- A kulcsok azert NUMBER(10) tipusuak, mert ez fer be 32 bitbe
-- Lasd : CREATE SEQUENCE utasitas
-- ( unsigned int32 0..4294967295) 0 - fenntartva hibakezelesre (NULL ?)

-- A versenyzo neveket kulcsokkal osszerendelo tabla
CREATE TABLE vers_nev
(
  kulcs   NUMBER(10) ,
  vez_nev VARCHAR2(15) NOT NULL ,
  ker_nev VARCHAR2(15) NOT NULL ,
  CONSTRAINT pk_vers_nev PRIMARY KEY ( kulcs) ,
  CONSTRAINT uk_v_k_nev  UNIQUE ( vez_nev, ker_nev) 
) ;

CREATE SEQUENCE seq_vnev START WITH 1 MAXVALUE 4294967295 ;

-- A tablanevvel azonos oszlopnevek hasznalhatok - nem hasznalom ki...

-- A nemzetiseg neveket kulcsokkal osszerendelo tabla
CREATE TABLE nemz_nev
(
  kulcs   NUMBER(10)   ,
  nmz_nev VARCHAR2(3) ,
  CONSTRAINT pk_nemz_nev PRIMARY KEY ( kulcs) ,
  CONSTRAINT uk_nmz_nev  UNIQUE ( nmz_nev) ,
  CONSTRAINT chk_nemz_nev CHECK
  (
    LENGTH( nmz_nev ) = 3       AND
    nmz_nev BETWEEN 'A' AND 'Z'
  )
) ;

CREATE SEQUENCE seq_nnev START WITH 1 MAXVALUE 4294967295 ;

-- A csapat neveket kulcsokkal osszerendelo tabla
CREATE TABLE csapat_nev
(
  kulcs    NUMBER(10) ,
  csap_nev VARCHAR2(15) NOT NULL ,
  CONSTRAINT pk_csapat_nev PRIMARY KEY ( kulcs) ,
  CONSTRAINT uk_csap_nev  UNIQUE ( csap_nev)
) ;

CREATE SEQUENCE seq_csnev START WITH 1 MAXVALUE 4294967295 ;

-- A motor neveket kulcsokkal osszerendelo tabla
CREATE TABLE motor_nev
(
  kulcs   NUMBER(10) ,
  mot_nev VARCHAR2(15) NOT NULL ,
  CONSTRAINT pk_motor_nev PRIMARY KEY ( kulcs) ,
  CONSTRAINT uk_mot_nev  UNIQUE ( mot_nev)
) ;

CREATE SEQUENCE seq_mnev START WITH 1 MAXVALUE 4294967295 ;

-- A nagydij neveket kulcsokkal osszerendelo tabla
CREATE TABLE nagydij_nev
(
  kulcs  NUMBER(10) ,
  nd_nev VARCHAR2(15) NOT NULL ,
  CONSTRAINT pk_nagydij_nev PRIMARY KEY ( kulcs) ,
  CONSTRAINT uk_nd_nev  UNIQUE ( nd_nev)
) ;

CREATE SEQUENCE seq_ndnev START WITH 1 MAXVALUE 4294967295 ;

-- A palya neveket kulcsokkal osszerendelo tabla
CREATE TABLE palya_nev
(
  kulcs    NUMBER(10) ,
  paly_nev VARCHAR2(20) NOT NULL ,
  CONSTRAINT pk_palya_nev PRIMARY KEY ( kulcs) ,
  CONSTRAINT uk_paly_nev  UNIQUE ( paly_nev)
) ;

CREATE SEQUENCE seq_pnev START WITH 1 MAXVALUE 4294967295 ;

-- A pontozasokat tartalmazo tabla
CREATE TABLE pontozas
(
  kulcs    NUMBER(10) ,
  helyezes NUMBER(2)  ,
  pontszam NUMBER(2) NOT NULL ,
-- kulonben a szezon nem hivatkozhatna a kulcs-ra
  CONSTRAINT pk_pontozas PRIMARY KEY ( kulcs, helyezes)
) ;

CREATE SEQUENCE seq_pont START WITH 1 MAXVALUE 4294967295 ;

CREATE TABLE szezon
(
  szezon_eve     NUMBER(4) ,
  pontozas       NUMBER(10) ,
  edz_els_pont   NUMBER(2) NOT NULL ,
  leggy_kor_pont NUMBER(2) NOT NULL ,
  elso_legj_besz NUMBER(2) NOT NULL ,
  CONSTRAINT pk_szezon PRIMARY KEY ( szezon_eve),
  CONSTRAINT chk_szez_ev CHECK
  (
    szezon_eve >= 1950
  )
) ;

CREATE TABLE nagydijak
(
  szezon_eve     NUMBER(4) NOT NULL ,
  nagydij_nev    NUMBER(10) NOT NULL ,
  palya_nev      NUMBER(10) NOT NULL ,
  datum          DATE ,
--  CONSTRAINT fk_nd_szezon  FOREIGN KEY ( szezon_eve)  REFERENCES szezon( szezon_eve) ,
--  CONSTRAINT fk_nd_nd_nev  FOREIGN KEY ( nagydij_nev) REFERENCES nagydij_nev( kulcs) ,
--  CONSTRAINT fk_nd_ply_nev FOREIGN KEY ( palya_nev)   REFERENCES palya_nev( kulcs) ,
--  CONSTRAINT pk_nagydijak  PRIMARY KEY ( szezon_eve, nagydij_nev) helyett UNIQUE, NOT NULL
  CONSTRAINT uk_nagydijak  UNIQUE ( szezon_eve, nagydij_nev)
) ;

CREATE TABLE nevezesi_lista
(
  kulcs          NUMBER(10) ,
  rajtszam       NUMBER(3) ,
  versenyzo      NUMBER(10) ,
  nemzetiseg     NUMBER(10) ,
  csapat         NUMBER(10) ,
  motor          NUMBER(10) ,
  CONSTRAINT pk_nevezesi_lista  PRIMARY KEY ( kulcs) ,
--  CONSTRAINT fk_nvzl_vers_nev   FOREIGN KEY ( versenyzo)  REFERENCES vers_nev( kulcs) ,
--  CONSTRAINT fk_nvzl_nemz_nev   FOREIGN KEY ( nemzetiseg) REFERENCES nemz_nev( kulcs) ,
--  CONSTRAINT fk_nvzl_csapat_nev FOREIGN KEY ( csapat)     REFERENCES csapat_nev( kulcs) ,
--  CONSTRAINT fk_nvzl_motor_nev  FOREIGN KEY ( motor)      REFERENCES motor_nev( kulcs) , 
-- a kulcs ugyis UNIQUE, ezt nem veszem be: mi van, ha ket evben ua. a sor kell szerepeljen ?
-- semmi: akkor ugyanaz a nvzl kulcs szerepel majd...
  CONSTRAINT uk_nevezesi_lista  UNIQUE ( rajtszam, versenyzo, nemzetiseg, csapat, motor)
) ;

CREATE SEQUENCE seq_nvzl START WITH 1 MAXVALUE 4294967295 ;

CREATE TABLE rajtsorrend
(
  szezon_eve     NUMBER(4) ,
  nagydij_nev    NUMBER(10) ,
  nvzl_kulcs     NUMBER(10) ,
  helyezes       NUMBER(2) ,
  perc           NUMBER(2) ,
  masodperc      NUMBER(2) ,
  ezrdmsdp       NUMBER(3) ,
  megjegyzes     VARCHAR2(16) ,
--  CONSTRAINT fk_rs_nd FOREIGN KEY ( szezon_eve, nagydij_nev) REFERENCES nagydijak( szezon_eve, nagydij_nev) ,
  CONSTRAINT pk_rajtsorrend PRIMARY KEY ( szezon_eve, nagydij_nev, nvzl_kulcs)
) ;

CREATE TABLE befuto
(
  szezon_eve     NUMBER(4) ,
  nagydij_nev    NUMBER(10) ,
  nvzl_kulcs     NUMBER(10) ,
  helyezes       NUMBER(2) ,
  ora            NUMBER(1) ,
  perc           NUMBER(2) ,
  masodperc      NUMBER(2) ,
  ezrdmsdp       NUMBER(3) ,
  megjegyzes     VARCHAR2(16) ,
  CONSTRAINT pk_befuto PRIMARY KEY ( szezon_eve, nagydij_nev, nvzl_kulcs)
) ;

CREATE TABLE leggyorsabb_kor
(
  szezon_eve     NUMBER(4) ,
  nagydij_nev    NUMBER(10) ,
  nvzl_kulcs     NUMBER(10) ,
  korben         NUMBER(3) ,
  perc           NUMBER(2) ,
  masodperc      NUMBER(2) ,
  ezrdmsdp       NUMBER(3) ,
  atl_sebesseg   NUMBER(6,3) ,
  CONSTRAINT pk_leggy_kor PRIMARY KEY ( szezon_eve, nagydij_nev)
) ;

-- Ha kurzor erteket valtozoba akarom tolteni, kell egy tabla, melybol
-- pontosan 1 sor szelektalodik ki:
-- seq 'kamu' tabla, melyben csak 1 sor van
CREATE TABLE seq
(
  seq NUMBER(10)
) ;

-- Az integritast (pl. csak olyan kulcs szerepelhet a befuto tablaban,
-- mely a nevezesi_lista tablaban szerepel) biztosito CONSTRAINT-ek
-- kulon, mert definialaskor nem eszi meg

--Nem megy a pontozas PK( kulcs, helyezes) miatt:
--ORA-02270: no matching unique or primary key for this column-list
--ALTER TABLE szezon
--  ADD CONSTRAINT fk_szezon_pnt
--    FOREIGN KEY (pontozas)
--    REFERENCES pontozas( kulcs) ;

ALTER TABLE nagydijak
  ADD CONSTRAINT fk_nd_szezon
    FOREIGN KEY ( szezon_eve)
    REFERENCES szezon( szezon_eve) ;

ALTER TABLE nagydijak
  ADD CONSTRAINT fk_nd_nd_nev
    FOREIGN KEY ( nagydij_nev)
    REFERENCES nagydij_nev( kulcs) ;

ALTER TABLE nagydijak
  ADD CONSTRAINT fk_nd_ply_nev
    FOREIGN KEY ( palya_nev)   REFERENCES palya_nev( kulcs) ;

--ALTER TABLE nevezesi_lista 
--  ADD CONSTRAINT uk_vs_nmz_cs_m
--    UNIQUE ( versenyzo, nemzetiseg, csapat, motor) ;

ALTER TABLE nevezesi_lista
  ADD CONSTRAINT fk_nvzl_vers_nev
    FOREIGN KEY ( versenyzo)
    REFERENCES vers_nev( kulcs) ;

ALTER TABLE nevezesi_lista
  ADD CONSTRAINT fk_nvzl_nemz_nev
    FOREIGN KEY ( nemzetiseg)
    REFERENCES nemz_nev( kulcs) ;

ALTER TABLE nevezesi_lista
  ADD CONSTRAINT fk_nvzl_csapat_nev
    FOREIGN KEY ( csapat)
    REFERENCES csapat_nev( kulcs) ;

ALTER TABLE nevezesi_lista
  ADD CONSTRAINT fk_nvzl_motor_nev
    FOREIGN KEY ( motor)
    REFERENCES motor_nev( kulcs) ;

ALTER TABLE rajtsorrend
  ADD CONSTRAINT fk_rs_nd
    FOREIGN KEY ( szezon_eve, nagydij_nev)
    REFERENCES nagydijak( szezon_eve, nagydij_nev) ;

ALTER TABLE rajtsorrend
  ADD CONSTRAINT fk_rs_nvzl
    FOREIGN KEY ( nvzl_kulcs)
    REFERENCES nevezesi_lista( kulcs) ;

ALTER TABLE rajtsorrend
  ADD CONSTRAINT uk_rs_sze_ndn_hlyz
    UNIQUE ( szezon_eve, nagydij_nev, helyezes) ;

ALTER TABLE befuto
  ADD CONSTRAINT fk_bef_sze_ndn
    FOREIGN KEY ( szezon_eve, nagydij_nev)
    REFERENCES nagydijak( szezon_eve, nagydij_nev) ;

ALTER TABLE befuto
  ADD CONSTRAINT fk_bef_nvzl
    FOREIGN KEY ( nvzl_kulcs)
    REFERENCES nevezesi_lista( kulcs) ;

ALTER TABLE befuto
  ADD CONSTRAINT uk_bef_sze_ndn_hlyz
    UNIQUE ( szezon_eve, nagydij_nev, helyezes) ;

ALTER TABLE leggyorsabb_kor
  ADD CONSTRAINT fk_lggyk_sze_ndn
    FOREIGN KEY ( szezon_eve, nagydij_nev)
    REFERENCES nagydijak( szezon_eve, nagydij_nev) ;

ALTER TABLE leggyorsabb_kor
  ADD CONSTRAINT fk_lggyk_nvzl
    FOREIGN KEY ( nvzl_kulcs)
    REFERENCES nevezesi_lista( kulcs) ;

--ALTER TABLE leggyorsabb_kor utkozik a primary key-vel
--  ADD CONSTRAINT uk_lgyk_sze_ndn
--    UNIQUE ( szezon_eve, nagydij_nev) ;

-- nagydijak.datum mezo formatumanak beallitasa
ALTER SESSION SET NLS_DATE_FORMAT = "YYYY MM DD" ;

-- A kamu tablaban pontosan egy sornak kell lennie
INSERT INTO seq
  VALUES ( 1 ) ;
