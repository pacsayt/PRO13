-- szez_all_h.sql


-- Hasznalat :
-- 1. szez_pill_all_v/cs elkesziti az adott szezon pillanatnyi allasat versenyzo nevek szerint
--    (versenyzo-, nemzetiseg-, motornevekre vonatkozo lekerdezes eseten)
--    szez_pill_all_v abban kulobozik szez_pill_all_cs-tol, hogy szez_pill_all_v eseteben
--    n_max_ert_erdm eredmeny szamit be.
--    szez_pill_all_cs nevtipust is elfogad, mely alapjan kulonbozo nevek kulcsai
--    szerepelnek a vegeredmenyben
-- 2. szez_eredm_elhoz elhozza a megadott nevek a szezonban elert poziciojat
-- 3. a megadott nevek osszes beszamitott eredmenyet elhozza,
--    nev szerint megedva a nevezesilista kulcsot
  
CREATE OR REPLACE PACKAGE szez_all AS

  -- egyszeruen NUMBER, hogy jo legyen a versenyzo/csapat lekerdezesnel is
  TYPE t_nev_pont_sor IS RECORD
  (
    nev_kulcs   NUMBER(10) := 4294967296, -- A csokkeno rendezes miatt
    pont        NUMBER( 2) := 0,
    helyezes    NUMBER( 2) := 0,
    nvzl_kulcs  NUMBER(10) := 0,
    nagydij_nev NUMBER(10) := 0
  ) ;

  TYPE t_szez_all_sor IS RECORD
  (
    nev_kulcs   NUMBER(10) := 0,
    pont        NUMBER( 3) := 0
  ) ;

  TYPE t_nev_pont_tab IS TABLE OF t_nev_pont_sor
    INDEX BY BINARY_INTEGER ;

  TYPE t_szez_all_tab IS TABLE OF t_szez_all_sor
    INDEX BY BINARY_INTEGER ;

  -- Az eredmeny elhozasahoz hasznalt fuggveny kimenoparametereinek tipusa
  TYPE t_num10_array IS TABLE OF NUMBER(10)
    INDEX BY BINARY_INTEGER ;

  TYPE t_num3_array IS TABLE OF NUMBER(3)
    INDEX BY BINARY_INTEGER ;

  TYPE t_num2_array IS TABLE OF NUMBER(2)
    INDEX BY BINARY_INTEGER ;

  -- Az adott szezon versenyzok szerinti pillanatnyi allasa
  PROCEDURE szez_pill_all_v( n_szez_eve IN NUMBER) ;

  -- Az adott szezon csapatok, nemzetisegek, motorok szerinti pillanatnyi allasa
  PROCEDURE szez_pill_all_cs( n_szez_eve IN NUMBER, n_nevtipus IN NUMBER) ;

  -- A megadott nevek szezonben elert eredmenyeit adja vissza, a specifikus nev szerint
  -- Minden nevhez megad egy poziciot.Amelyik nem ert el semmit, az 0.
  -- Csak ismert meretu tombot lehet tarolt eljarasnak atadni:
  -- Egyszerre max. n_nev_tmb_mer nevet hoz el
  PROCEDURE szez_eredm_elhoz( n_nev_tab IN OUT t_num10_array, n_nev_tab_mer IN OUT NUMBER,
                              n_poz_tmb    OUT t_num2_array,  n_szz_pnt_tmb    OUT t_num3_array) ;

  -- A bemeno tombbel adott nevkulcsok altal elert es beszamitott eredmenyeket
  -- adja vissza. Csak a kimeno adatok tombje indexet jegyzi meg, a bemeno parameter
  -- nev kulcs tomb utolso felhasznalt poziciojat nem tarolom, *.pc feladata a nevek
  -- eloremozgatasa 
  -- Csak ismert meretu tombot lehet tarolt eljarasnak atadni:
  -- Egyszerre max. n_eredm_tmb_mer(konstans:szez_all_b.sql,dbglobal.pc) eredmenyt hoz el
  -- Az utolso menetben az elhozott adatok meretet szamitom (*.pc), nem kell visszaadni
  PROCEDURE besz_eredm_elhoz( n_nev_tab IN t_num10_array, n_nev_tab_mer IN NUMBER,
                              n_nev_kulcs_tab   OUT t_num10_array, n_pont_tab       OUT t_num2_array,
                              n_helyezes_tab    OUT t_num2_array , n_nvzl_kulcs_tab OUT t_num10_array,
                              n_nagydij_nev_tab OUT t_num10_array, n_ki_tab_mer     OUT NUMBER) ;

  PROCEDURE teszt( n_kezd IN NUMBER, n_veg IN NUMBER) ;

  -- A kovetkezo valtozokat az eredmeny elhozasa elott kerdezem le
  -- A szezon osszesitest tartalmazo tabla merete
  n_max_szez_idx  NUMBER := 0 ;
  -- A szezon osszes beszamitott eredmenyet tartalmazo tabla merete
  n_max_eredm_idx NUMBER := 0 ;

END szez_all ;
/

show errors
