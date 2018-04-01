-- szez_all.sql

-- Hasznalat :
-- 1. szez_pill_all_v/cs elkesziti az adott szezon pillanatnyi allasat versenyzo nevek szerint
--    (versenyzo-, nemzetiseg-, motornevekre vonatkozo lekerdezes eseten)
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

  TYPE t_nev_pont_tab IS TABLE OF t_nev_pont_sor ;
--    INDEX BY BINARY_INTEGER ;
  TYPE t_szez_all_tab IS TABLE OF t_szez_all_sor ;

  -- Az eredmeny elhozasahoz hasznalt fuggveny kimenoparametereinek tipusa
  TYPE t_num10_array IS TABLE OF NUMBER(10)
    INDEX BY BINARY_INTEGER ;

  TYPE t_num3_array IS TABLE OF NUMBER(3)
    INDEX BY BINARY_INTEGER ;

  TYPE t_num2_array IS TABLE OF NUMBER(2)
    INDEX BY BINARY_INTEGER ;

  -- Az adott szezon versenyzok szerinti pillanatnyi allasa
  PROCEDURE szez_pill_all_v( n_szez_eve IN NUMBER) ;

  -- Az adott szezon csapatok szerinti pillanatnyi allasa
  PROCEDURE szez_pill_all_cs( n_szez_eve IN NUMBER) ;

  -- A megadott nevek szezonben elert eredmenyeit adja vissza, a specifikus nev szerint
  -- Minden nevhez megad egy poziciot, amelyik nem ert el semmit, az 0.
  -- Csak ismert meretu tombot lehet tarolt eljarasnak atadni:
  -- Egyszerre max. n_nev_tmb_mer nevet hoz el
  PROCEDURE szez_eredm_elhoz( n_nev_tab IN OUT t_num10_array, n_nev_tab_mer IN NUMBER,
                              n_poz_tmb OUT t_num2_array,     n_szz_pnt_tmb OUT t_num3_array) ;

  -- A bemeno tombbel adott nevkulcsok altal elert es beszamitott eredmenyeket
  -- adja vissza.
  -- Csak ismert meretu tombot lehet tarolt eljarasnak atadni:
  -- Egyszerre max. n_eredm_tmb_mer eredmenyt hoz el
  PROCEDURE besz_eredm_elhoz( n_nev_tab IN t_num10_array, n_nev_tab_mer IN NUMBER,
                              nev_kulcs_tab   OUT t_num10_array, pont_tab       OUT t_num2_array,
                              helyezes_tab    OUT t_num2_array , nvzl_kulcs_tab OUT t_num10_array,
                              nagydij_nev_tab OUT t_num10_array,
                              n_tombok_merete OUT NUMBER) ;

  -- A kovetkezo valtozokat az eredmeny elhozasa elott kerdezem le
  -- A szezon osszesitest tartalmazo tabla merete
  n_max_szez_idx  NUMBER ;
  -- A szezon osszes beszamitott eredmenyet tartalmazo tabla merete
  n_max_eredm_idx NUMBER ;

END szez_all ;
/

show errors

CREATE OR REPLACE PACKAGE BODY szez_all AS

  nev_pont_tab   t_nev_pont_tab ;
  szez_all_tab   t_szez_all_tab ;

  -- A bemenoadatkent kapott/visszaadando nevek tombjenek merete
  -- (Tarolt eljarasnak csak fix meretu tomboket lehet megadni)
  n_nev_tmb_mer CONSTANT NUMBER := 20 ;

  n_eredm_tmb_mer CONSTANT NUMBER := 50 ;

  -- Elhozasnal ez a valtozo adja meg, hol tart eppen
  n_akt_nev_idx   NUMBER ;
  n_akt_eredm_idx NUMBER ;

-- max_ertekelt
  FUNCTION max_ertekelt( n_szez_eve IN NUMBER ) RETURN NUMBER AS
    
    n_max_ertekelt NUMBER ;

  BEGIN

    SELECT MAX( p.helyezes)
	  INTO n_max_ertekelt
	  FROM szezon sz, pontozas p
	  WHERE sz.szezon_eve = n_szez_eve AND
	        p.kulcs       = sz.pontozas ;

    RETURN n_max_ertekelt ;

  END max_ertekelt ;

-- helyez_osszeh
  -- A fuggveny a nev_pont_tab ket nevet hasonlitja ossze,
  -- melyiknek van tobb magasabb pontszamu eredmenye (a helyezes helyett[futgy. ? edzels])
  -- 1 : n_poz_egy > n_poz_ket, 0 : n_poz_egy > n_poz_ket -1 : n_poz_egy < n_poz_ket
  FUNCTION helyez_osszeh( n_kulcs_egy IN NUMBER, n_kulcs_ket IN NUMBER ) RETURN NUMBER AS

    n_poz_egy NUMBER ;
    n_poz_ket NUMBER ;

    n_relacio NUMBER ;

  BEGIN

    n_relacio := 0 ;

    -- Feltesszuk, hogy biztos vannak ilyen kulcsok
    WHILE nev_pont_tab(n_poz_egy).nev_kulcs != n_kulcs_egy LOOP
      n_poz_egy := n_poz_egy + 1 ;
    END LOOP ;

    WHILE nev_pont_tab(n_poz_ket).nev_kulcs != n_kulcs_ket LOOP
      n_poz_ket := n_poz_ket + 1 ;
    END LOOP ;

    WHILE n_poz_egy <= n_max_eredm_idx AND n_poz_ket <= n_max_eredm_idx AND
          nev_pont_tab(n_poz_egy).nev_kulcs = n_kulcs_egy AND nev_pont_tab(n_poz_ket).nev_kulcs = n_kulcs_ket LOOP

      IF nev_pont_tab(n_poz_egy).pont > nev_pont_tab(n_poz_ket).pont THEN
        RETURN 1 ;
      ELSE
        IF nev_pont_tab(n_poz_egy).pont < nev_pont_tab(n_poz_ket).pont THEN
          RETURN -1 ;
        END IF ;
      END IF ;

      n_poz_egy := n_poz_egy + 1 ;
      n_poz_ket := n_poz_ket + 1 ;
    END LOOP ;

    IF n_poz_egy <= n_max_eredm_idx AND nev_pont_tab(n_poz_egy).nev_kulcs = n_kulcs_egy THEN
      n_relacio := n_poz_egy + 1 ;
    END IF ;

    IF n_poz_ket <= n_max_eredm_idx AND nev_pont_tab(n_poz_ket).nev_kulcs = n_kulcs_ket THEN
      n_relacio := n_poz_ket - 1 ;
    END IF ;

    RETURN n_relacio ;

  END helyez_osszeh ;

-- szez_eredm_megh
-- A fv. osszeadja a (globalis) nev_pont_tab-ban szereplo nevenkenti eredmenyeket
-- a (globalis) szez_all_tab-ba, majd sorbarendezi a neveket 
-- ( A helyez_osszeh fv-t hivva : azonos pontszam eseten a tobb jobb helyezes dont)
  PROCEDURE szez_eredm_megh( n_eredm_szama IN NUMBER ) AS

    n_sz_idx   NUMBER ;
    n_sz2_idx  NUMBER ;
    n_ossz_ert NUMBER ;

    szez_all_sor t_szez_all_sor ;

  BEGIN

    -- Osszeadjuk a pontokat nev_pont_tab -> szez_all_tab
    n_sz_idx  := 0 ;
    szez_all_tab(0).nev_kulcs := nev_pont_tab(0).nev_kulcs ;
    szez_all_tab(0).pont      := nev_pont_tab(0).pont ;

    FOR n_pnt_idx IN 0 .. n_eredm_szama LOOP

      IF szez_all_tab(n_sz_idx).nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs THEN
        szez_all_tab(n_sz_idx).pont := szez_all_tab(n_sz_idx).pont + nev_pont_tab(n_pnt_idx).pont ;
      ELSE
        n_sz_idx := n_sz_idx + 1 ;
        szez_all_tab(n_sz_idx).pont := nev_pont_tab(n_pnt_idx).pont ;
      END IF ;

    END LOOP ;

    -- Sorbarendezzuk a neveket
    n_ossz_ert := n_sz_idx ;
--    szez_all_sor := szez_all_tab(n_szez_idx) ;
    FOR n_sz_idx IN 1 .. n_ossz_ert LOOP
      FOR n_sz2_idx IN 0 .. n_ossz_ert - n_sz_idx LOOP
        IF szez_all_tab(n_sz2_idx).pont < szez_all_tab(n_sz2_idx+1).pont THEN
          szez_all_sor := szez_all_tab(n_sz2_idx) ;
          szez_all_tab(n_sz2_idx) := szez_all_tab(n_sz2_idx+1) ;
          szez_all_tab(n_sz2_idx+1) := szez_all_sor ;
        ELSE
          IF szez_all_tab(n_sz2_idx).pont = szez_all_tab(n_sz2_idx+1).pont THEN
            IF helyez_osszeh( szez_all_tab(n_sz2_idx).nev_kulcs, szez_all_tab(n_sz2_idx+1).nev_kulcs) = -1 THEN
              szez_all_sor := szez_all_tab(n_sz2_idx) ;
              szez_all_tab(n_sz2_idx) := szez_all_tab(n_sz2_idx+1) ;
              szez_all_tab(n_sz2_idx+1) := szez_all_sor ;
            END IF ;
          END IF ;
        END IF ;
      END LOOP ;
    END LOOP ;

  END szez_eredm_megh ;


-- szez_pill_all_v
  PROCEDURE szez_pill_all_v( n_szez_eve IN NUMBER) AS

    CURSOR bef_cur( n_ev IN NUMBER, n_max_helyez IN NUMBER)
      RETURN t_nev_pont_sor IS
      SELECT n.versenyzo, p.pontszam, b.helyezes, n.kulcs, b.nagydij_nev
        FROM befuto b, nevezesi_lista n, szezon s, pontozas p
        WHERE b.szezon_eve  = n_ev         AND
              b.helyezes   <= n_max_helyez AND
              s.szezon_eve  = n_ev         AND
              p.kulcs       = s.pontozas   AND
              p.helyezes    = b.helyezes   AND
              n.kulcs       = b.nvzl_kulcs
              ORDER BY n.versenyzo, p.pontszam DESC ;

    CURSOR edzels_cur( n_ev IN NUMBER, n_edz_els_pnt IN NUMBER)
      RETURN t_nev_pont_sor IS
      SELECT  n.versenyzo, n_edz_els_pnt, r.helyezes, n.kulcs, r.nagydij_nev
        FROM rajtsorrend r, nevezesi_lista n
        WHERE r.szezon_eve  = n_ev         AND
              r.helyezes    = 1            AND
              n.kulcs       = r.nvzl_kulcs
              ORDER BY n.versenyzo DESC ;

    CURSOR lggykr_cur( n_ev IN NUMBER, n_lggykr_pnt IN NUMBER)
      RETURN t_nev_pont_sor IS
      SELECT  n.versenyzo, n_lggykr_pnt, 0, n.kulcs, l.nagydij_nev
        FROM leggyorsabb_kor l, nevezesi_lista n
        WHERE l.szezon_eve  = n_ev         AND
              n.kulcs       = l.nvzl_kulcs
              ORDER BY n.versenyzo DESC ;

    r_pont_sor   t_nev_pont_sor ;
    b_pont_sor   t_nev_pont_sor ;
    l_pont_sor   t_nev_pont_sor ;

    n_edz_els_pnt  NUMBER ;
    n_lggyk_pnt    NUMBER ;
    n_max_ertekelt NUMBER ;

    -- A ciklusban aktualis nev kulcsa
    n_akt_kulcs    NUMBER ;
    -- Az aktualis nevhez hany eredmeny lett kivalasztva
    n_kiv_eredm    NUMBER ;
    n_pnt_idx      NUMBER ;
    n_szez_idx     NUMBER ;
    n_szz2_idx     NUMBER ;

    -- Csak mindharom fajta eredmeny levizsgalasa utan tudjuk, melyik volt az
    -- alkalmas. Emiatt kell ez a valtozo, melyet minden tombbe irasnal
    -- aktualizalok, igy a vegen lehet tudni melyik fajta kerult a tombbe,
    -- melyikbol kell ujat hozni. 
    -- 'b' - befuto eredmeny, 'r' - edzeselsoseg, 'l' - leggyorsabb kor
    c_eredm_tip    CHAR   ;

  BEGIN

    -- Elhozasnal hol tart eppen : inicializalas
    n_akt_nev_idx   := 1 ;
    n_akt_eredm_idx := 1 ;

    -- Hany pont jar az edzeselsosegert az adott szezonban ?
    SELECT edz_els_pont
      INTO n_edz_els_pnt
      FROM szezon
      WHERE szezon_eve = n_szez_eve ;

    -- Hany pont jar a leggyorsabb korert az adott szezonban ?
    SELECT leggy_kor_pont
      INTO n_lggyk_pnt
      FROM szezon
      WHERE szezon_eve = n_szez_eve ;

    n_max_ertekelt := max_ertekelt( n_szez_eve) ;

    -- A befuto eredmenyert mindenkeppen jar pont, ez a kurzor mindig kell
    OPEN bef_cur( n_szez_eve, n_max_ertekelt) ;
    FETCH bef_cur INTO b_pont_sor ;

    -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
    nev_pont_tab(0) := b_pont_sor ;
    c_eredm_tip := 'b' ;

    -- Az eredmeny tomb kezdopoziciojara a legelso (legnagyobb) kulcsot
    -- a legnagyobb pontszammal tesszuk

    IF n_edz_els_pnt > 0 THEN
      OPEN edzels_cur( n_szez_eve, n_edz_els_pnt) ;
      FETCH edzels_cur INTO r_pont_sor ;

      IF r_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
        nev_pont_tab(0) := r_pont_sor ;
        c_eredm_tip := 'r' ;
      ELSE
        IF r_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(0).pont THEN
          nev_pont_tab(0) := r_pont_sor ;
          c_eredm_tip := 'r' ;
        END IF ;
      END IF ;

--      CLOSE edzels_cur ;
    END IF ;

    IF n_lggyk_pnt > 0 THEN
      OPEN lggykr_cur( n_szez_eve, n_lggyk_pnt) ;
      FETCH lggykr_cur INTO l_pont_sor ;

      IF l_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
        nev_pont_tab(0) := l_pont_sor ;
        c_eredm_tip := 'l' ;
      ELSE
        IF l_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(0).pont THEN
          nev_pont_tab(0) := l_pont_sor ;
          c_eredm_tip := 'l' ;
        END IF ;
      END IF ;

--      CLOSE lggykr_cur ;
    END IF ;

    -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
    IF c_eredm_tip = 'b' THEN
      FETCH bef_cur INTO b_pont_sor ;
    ELSE
      IF c_eredm_tip = 'r' THEN
        FETCH edzels_cur INTO r_pont_sor ;
      ELSE
        FETCH lggykr_cur INTO l_pont_sor ;
      END IF ;
    END IF ;

    n_kiv_eredm := 1 ;
    n_pnt_idx   := 1 ;
    LOOP
      -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
      c_eredm_tip := ' ' ;
      IF n_kiv_eredm < n_max_ertekelt THEN

        IF b_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs THEN
          nev_pont_tab(n_pnt_idx) := r_pont_sor ;
          c_eredm_tip := 'b' ;
        END IF ;

        IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
          nev_pont_tab(n_pnt_idx) := r_pont_sor ;
          c_eredm_tip := 'r' ;
        END IF ;

        IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
          nev_pont_tab(n_pnt_idx) := l_pont_sor ;
          c_eredm_tip := 'l' ;
        END IF ;

      END IF ;

      -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
      IF c_eredm_tip = 'b' THEN
        FETCH bef_cur INTO b_pont_sor ;
        n_pnt_idx := n_pnt_idx + 1 ;
      ELSE
        IF c_eredm_tip = 'r' THEN
          FETCH edzels_cur INTO r_pont_sor ;
          n_pnt_idx := n_pnt_idx + 1 ;
        ELSE
          IF c_eredm_tip = 'l' THEN
            FETCH lggykr_cur INTO l_pont_sor ;
            n_pnt_idx := n_pnt_idx + 1 ;
          ELSE
            -- c_eredm_tip = ' '
            -- Nincs tobb eredmeny ezzel a nev kulccsal vagy
            -- elertuk a maximalis ertekelheto eredmenyek szamat -> kovetkezo nev
            -- Valamilyen rendszer szerint vegig kell menni a neveken :
            -- legyen ez a novekvo kulcs
            
            -- Lepjuk at mindharom helyen a regi nevet !
            WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = b_pont_sor.nev_kulcs LOOP
              FETCH bef_cur INTO b_pont_sor ;
            END LOOP ;

            WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = r_pont_sor.nev_kulcs LOOP
              FETCH edzels_cur INTO r_pont_sor ;
            END LOOP ;

            WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = l_pont_sor.nev_kulcs LOOP
              FETCH lggykr_cur INTO l_pont_sor ;
            END LOOP ;
            
            -- Melyik a legnagyobb (nev_kulcs, pont) ?
            n_pnt_idx := n_pnt_idx + 1 ; -- ???
            nev_pont_tab(n_pnt_idx) := b_pont_sor ;
            c_eredm_tip := 'b' ;

            IF r_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
              nev_pont_tab(n_pnt_idx) := r_pont_sor ;
              c_eredm_tip := 'r' ;
            ELSE
              IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                nev_pont_tab(n_pnt_idx) := r_pont_sor ;
                c_eredm_tip := 'r' ;
              END IF ;
            END IF ;

            IF l_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
              nev_pont_tab(n_pnt_idx) := l_pont_sor ;
              c_eredm_tip := 'l' ;
            ELSE
              IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                nev_pont_tab(n_pnt_idx) := l_pont_sor ;
                c_eredm_tip := 'l' ;
              END IF ;
            END IF ;

            -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
            IF c_eredm_tip = 'b' THEN
              FETCH bef_cur INTO b_pont_sor ;
            ELSE
              IF c_eredm_tip = 'r' THEN
                FETCH edzels_cur INTO r_pont_sor ;
              ELSE
                FETCH lggykr_cur INTO l_pont_sor ;
              END IF ;
            END IF ;
             
          END IF ;
        END IF ;
      END IF ;

      EXIT WHEN bef_cur%NOTFOUND ;
    END LOOP ;

    CLOSE bef_cur    ;
    CLOSE edzels_cur ;
    CLOSE lggykr_cur ;

    n_max_eredm_idx := n_pnt_idx ;

    szez_eredm_megh( n_pnt_idx) ;

  END szez_pill_all_v ;


-- szez_pill_all_cs
  PROCEDURE szez_pill_all_cs( n_szez_eve IN NUMBER) AS

    CURSOR bef_cur( n_ev IN NUMBER, n_max_helyez IN NUMBER)
      RETURN t_nev_pont_sor IS
      SELECT n.csapat, p.pontszam, b.helyezes, n.kulcs, b.nagydij_nev
        FROM befuto b, nevezesi_lista n, szezon s, pontozas p
        WHERE b.szezon_eve  = n_ev         AND
              b.helyezes   <= n_max_helyez AND
              s.szezon_eve  = n_ev         AND
              p.kulcs       = s.pontozas   AND
              p.helyezes    = b.helyezes   AND
              n.kulcs       = b.nvzl_kulcs
              ORDER BY n.versenyzo DESC, p.pontszam DESC ;

    CURSOR edzels_cur( n_ev IN NUMBER, n_edz_els_pnt IN NUMBER)
      RETURN t_nev_pont_sor IS
      SELECT  n.csapat, n_edz_els_pnt, r.helyezes, n.kulcs, r.nagydij_nev
        FROM rajtsorrend r, nevezesi_lista n
        WHERE r.szezon_eve  = n_ev         AND
              r.helyezes    = 1            AND
              n.kulcs       = r.nvzl_kulcs
              ORDER BY n.versenyzo DESC ;

    CURSOR lggykr_cur( n_ev IN NUMBER, n_lggykr_pnt IN NUMBER)
      RETURN t_nev_pont_sor IS
      SELECT  n.csapat, n_lggykr_pnt, 0, n.kulcs, l.nagydij_nev
        FROM leggyorsabb_kor l, nevezesi_lista n
        WHERE l.szezon_eve  = n_ev         AND
              n.kulcs       = l.nvzl_kulcs
              ORDER BY n.versenyzo DESC ;

    r_pont_sor   t_nev_pont_sor ;
    b_pont_sor   t_nev_pont_sor ;
    l_pont_sor   t_nev_pont_sor ;

    n_edz_els_pnt  NUMBER ;
    n_lggyk_pnt    NUMBER ;
    n_max_ertekelt NUMBER ;

    -- A ciklusban aktualis nev kulcsa
    n_akt_kulcs    NUMBER ;
    -- Az aktualis nevhez hany eredmeny lett kivalasztva
    n_kiv_eredm    NUMBER ;
    n_pnt_idx      NUMBER ;
    n_szez_idx     NUMBER ;
    n_szz2_idx     NUMBER ;

    -- Csak mindharom fajta eredmeny levizsgalasa utan tudjuk, melyik volt az
    -- alkalmas. Emiatt kell ez a valtozo, melyet minden tombbe irasnal
    -- aktualizalok, igy a vegen lehet tudni melyik fajta kerult a tombbe,
    -- melyikbol kell ujat hozni. 
    -- 'b' - befuto eredmeny, 'r' - edzeselsoseg, 'l' - leggyorsabb kor
    c_eredm_tip    CHAR   ;

  BEGIN

    -- Elhozasnal hol tart eppen : inicializalas
    n_akt_nev_idx   := 1 ;
    n_akt_eredm_idx := 1 ;

    -- Hany pont jar az edzeselsosegert az adott szezonban ?
    SELECT edz_els_pont
      INTO n_edz_els_pnt
      FROM szezon
      WHERE szezon_eve = n_szez_eve ;

    -- Hany pont jar a leggyorsabb korert az adott szezonban ?
    SELECT leggy_kor_pont
      INTO n_lggyk_pnt
      FROM szezon
      WHERE szezon_eve = n_szez_eve ;

    n_max_ertekelt := max_ertekelt( n_szez_eve) ;

    -- A befuto eredmenyert mindenkeppen jar pont, ez a kurzor mindig kell
    OPEN bef_cur( n_szez_eve, n_max_ertekelt) ;
    FETCH bef_cur INTO b_pont_sor ;

    -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
    nev_pont_tab(0) := b_pont_sor ;
    c_eredm_tip := 'b' ;

    -- Az eredmeny tomb kezdopoziciojara a legelso (legnagyobb) kulcsot
    -- a legnagyobb pontszammal tesszuk

    IF n_edz_els_pnt > 0 THEN
      OPEN edzels_cur( n_szez_eve, n_edz_els_pnt) ;
      FETCH edzels_cur INTO r_pont_sor ;

      IF r_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
        nev_pont_tab(0) := r_pont_sor ;
        c_eredm_tip := 'r' ;
      ELSE
        IF r_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(0).pont THEN
          nev_pont_tab(0) := r_pont_sor ;
          c_eredm_tip := 'r' ;
        END IF ;
      END IF ;

--      CLOSE edzels_cur ;
    END IF ;

    IF n_lggyk_pnt > 0 THEN
      OPEN lggykr_cur( n_szez_eve, n_lggyk_pnt) ;
      FETCH lggykr_cur INTO l_pont_sor ;

      IF l_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
        nev_pont_tab(0) := l_pont_sor ;
        c_eredm_tip := 'l' ;
      ELSE
        IF l_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(0).pont THEN
          nev_pont_tab(0) := l_pont_sor ;
          c_eredm_tip := 'l' ;
        END IF ;
      END IF ;

--      CLOSE lggykr_cur ;
    END IF ;

    -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
    IF c_eredm_tip = 'b' THEN
      FETCH bef_cur INTO b_pont_sor ;
    ELSE
      IF c_eredm_tip = 'r' THEN
        FETCH edzels_cur INTO r_pont_sor ;
      ELSE
        FETCH lggykr_cur INTO l_pont_sor ;
      END IF ;
    END IF ;

    n_kiv_eredm := 1 ;
    n_pnt_idx   := 1 ;
    LOOP
      -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
      c_eredm_tip := ' ' ;
      IF n_kiv_eredm < n_max_ertekelt THEN

        IF b_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs THEN
          nev_pont_tab(n_pnt_idx) := r_pont_sor ;
          c_eredm_tip := 'b' ;
        END IF ;

        IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
          nev_pont_tab(n_pnt_idx) := r_pont_sor ;
          c_eredm_tip := 'r' ;
        END IF ;

        IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
          nev_pont_tab(n_pnt_idx) := l_pont_sor ;
          c_eredm_tip := 'l' ;
        END IF ;

      END IF ;

      -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
      IF c_eredm_tip = 'b' THEN
        FETCH bef_cur INTO b_pont_sor ;
        n_pnt_idx := n_pnt_idx + 1 ;
      ELSE
        IF c_eredm_tip = 'r' THEN
          FETCH edzels_cur INTO r_pont_sor ;
          n_pnt_idx := n_pnt_idx + 1 ;
        ELSE
          IF c_eredm_tip = 'l' THEN
            FETCH lggykr_cur INTO l_pont_sor ;
            n_pnt_idx := n_pnt_idx + 1 ;
          ELSE
            -- c_eredm_tip = ' '
            -- Nincs tobb eredmeny ezzel a nev kulccsal vagy
            -- elertuk a maximalis ertekelheto eredmenyek szamat -> kovetkezo nev
            -- Valamilyen rendszer szerint vegig kell menni a neveken :
            -- legyen ez a novekvo kulcs
            
            -- Lepjuk at mindharom helyen a regi nevet !
            WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = b_pont_sor.nev_kulcs LOOP
              FETCH bef_cur INTO b_pont_sor ;
            END LOOP ;

            WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = r_pont_sor.nev_kulcs LOOP
              FETCH edzels_cur INTO r_pont_sor ;
            END LOOP ;

            WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = l_pont_sor.nev_kulcs LOOP
              FETCH lggykr_cur INTO l_pont_sor ;
            END LOOP ;
            
            -- Melyik a legnagyobb (nev_kulcs, pont) ?
            n_pnt_idx := n_pnt_idx + 1 ; -- ???
            nev_pont_tab(n_pnt_idx) := b_pont_sor ;
            c_eredm_tip := 'b' ;

            IF r_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
              nev_pont_tab(n_pnt_idx) := r_pont_sor ;
              c_eredm_tip := 'r' ;
            ELSE
              IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                nev_pont_tab(n_pnt_idx) := r_pont_sor ;
                c_eredm_tip := 'r' ;
              END IF ;
            END IF ;

            IF l_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
              nev_pont_tab(n_pnt_idx) := l_pont_sor ;
              c_eredm_tip := 'l' ;
            ELSE
              IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                nev_pont_tab(n_pnt_idx) := l_pont_sor ;
                c_eredm_tip := 'l' ;
              END IF ;
            END IF ;

            -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
            IF c_eredm_tip = 'b' THEN
              FETCH bef_cur INTO b_pont_sor ;
            ELSE
              IF c_eredm_tip = 'r' THEN
                FETCH edzels_cur INTO r_pont_sor ;
              ELSE
                FETCH lggykr_cur INTO l_pont_sor ;
              END IF ;
            END IF ;
             
          END IF ;
        END IF ;
      END IF ;

      EXIT WHEN bef_cur%NOTFOUND ;
    END LOOP ;

    CLOSE bef_cur    ;
    CLOSE edzels_cur ;
    CLOSE lggykr_cur ;

    n_max_eredm_idx := n_pnt_idx ;

    szez_eredm_megh( n_pnt_idx) ;

  END szez_pill_all_cs ;

  PROCEDURE szez_eredm_elhoz( n_nev_tab IN OUT t_num10_array, n_nev_tab_mer IN NUMBER,
                              n_poz_tmb OUT    t_num2_array,  n_szz_pnt_tmb OUT t_num3_array) AS

    i NUMBER ;
    j NUMBER ;

  BEGIN

    -- n_nev_tab_mer = 0 eset jelzi, hogy mindent el kell hozni
    IF n_nev_tab_mer = 0 THEN

      i := n_akt_nev_idx ;

      WHILE i <= n_max_szez_idx AND (i-n_akt_nev_idx) < n_nev_tmb_mer LOOP

        n_nev_tab(i) := szez_all_tab(i).nev_kulcs ;
        n_poz_tmb(i) := i ;
        n_szz_pnt_tmb(i) := szez_all_tab(i).pont ;
      
        i := i + 1 ;

      END LOOP ;

      n_akt_nev_idx := i ;

    ELSE

      i := n_akt_nev_idx ;

      WHILE i <= n_nev_tab_mer AND (i-n_akt_nev_idx) < n_nev_tmb_mer LOOP

        FOR j IN 1..n_max_szez_idx LOOP

          IF n_nev_tab(i) = szez_all_tab(j).nev_kulcs THEN
            n_poz_tmb(i) := j ;
            n_szz_pnt_tmb(i) := szez_all_tab(j).pont ;
          END IF ;

        END LOOP ;

        i := i + 1 ;

      END LOOP ;

      n_akt_nev_idx := i ;

    END IF ;

    -- Ha elfogytak az elhozando adatok, az aktualis index visszaallitasa az
    -- elejere gondolva a kovetkezo elohzasra (besz_eredm_elhoz)
    IF i = n_nev_tab_mer THEN

      n_akt_nev_idx := 1 ;

    END IF ;

  END szez_eredm_elhoz ;


-- szez_eredm_elhoz
  PROCEDURE besz_eredm_elhoz( n_nev_tab IN t_num10_array, n_nev_tab_mer IN NUMBER,
                              nev_kulcs_tab OUT t_num10_array, pont_tab OUT t_num2_array,
                              helyezes_tab OUT t_num2_array, nvzl_kulcs_tab OUT t_num10_array,
                              nagydij_nev_tab OUT t_num10_array, n_tombok_merete OUT NUMBER) AS

    i NUMBER ;
    j NUMBER ;

  BEGIN

    n_tombok_merete := 0 ;

    -- Ha n_nev_tab_mer = 0, minden eredmenyt el kell hozni
    IF n_nev_tab_mer = 0 THEN

      j := n_akt_eredm_idx ;

      WHILE j <= n_max_eredm_idx AND n_tombok_merete < n_eredm_tmb_mer LOOP

        -- n_tombok_merete 0-ra inicializalva, elotte novelve, igy a
        -- visszaadott meret mindig helyes, a NESTED TABLE indexe 1-tol megy
        n_tombok_merete := n_tombok_merete + 1 ;

        nev_kulcs_tab(n_tombok_merete)   := nev_pont_tab(j).nev_kulcs   ;
        pont_tab(n_tombok_merete)        := nev_pont_tab(j).pont        ;
        helyezes_tab(n_tombok_merete)    := nev_pont_tab(j).helyezes    ;
        nvzl_kulcs_tab(n_tombok_merete)  := nev_pont_tab(j).nvzl_kulcs  ;
        nagydij_nev_tab(n_tombok_merete) := nev_pont_tab(j).nagydij_nev ;

        j := j + 1 ;

      END LOOP ;

      n_akt_eredm_idx := j ;
  
    ELSE

      j := n_akt_eredm_idx ;
      i := n_akt_nev_idx ;

      WHILE i <= n_nev_tab_mer AND n_tombok_merete <= n_eredm_tmb_mer LOOP

        WHILE j <= n_max_eredm_idx AND n_tombok_merete <= n_eredm_tmb_mer LOOP

          IF n_nev_tab(i) = nev_pont_tab(j).nev_kulcs THEN
            -- n_tombok_merete 0-ra inicializalva, elotte novelve, igy a
            -- visszaadott meret mindig helyes, a NESTED TABLE indexe 1-tol megy
            n_tombok_merete := n_tombok_merete + 1 ;

            nev_kulcs_tab(n_tombok_merete)   := nev_pont_tab(j).nev_kulcs   ;
            pont_tab(n_tombok_merete)        := nev_pont_tab(j).pont        ;
            helyezes_tab(n_tombok_merete)    := nev_pont_tab(j).helyezes    ;
            nvzl_kulcs_tab(n_tombok_merete)  := nev_pont_tab(j).nvzl_kulcs  ;
            nagydij_nev_tab(n_tombok_merete) := nev_pont_tab(j).nagydij_nev ;
          END IF ;

          j := j + 1 ;

        END LOOP ;

        i := i + 1 ;

      END LOOP ;

      n_akt_eredm_idx := j ;
      n_akt_nev_idx   := i ;

    END IF ;

  END besz_eredm_elhoz ;

END szez_all ;
/

show errors