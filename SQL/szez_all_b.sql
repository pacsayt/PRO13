CREATE OR REPLACE PACKAGE BODY szez_all AS
  -- Az egyes nevek osszes eredmenye
  nev_pont_tab   t_nev_pont_tab ;
  -- Az egyes nevek osszpontszama
  szez_all_tab   t_szez_all_tab ;
  -- A bemenoadatkent kapott/visszaadando nevek tombjenek merete
  -- (Tarolt eljarasnak csak fix meretu tomboket lehet megadni)
  -- l.: dbglobal.pc NEV_TMB_MER / EREDM_TMB_MER
  n_nev_tmb_mer CONSTANT NUMBER := 20 ;
  n_eredm_tmb_mer CONSTANT NUMBER := 50 ;
  -- Elhozasnal ez a valtozo adja meg, hol tart eppen
  n_akt_nev_idx   NUMBER ;
  n_akt_eredm_idx NUMBER ;

  -- bef, edzels, lggykr tipusu lekerdezesek kurzortipusa (azonos eredmeny)
  TYPE t_bel_cur IS REF CURSOR RETURN t_nev_pont_sor ;

-- kurz_megny : a szez_pill_all_cs-eljaras n_nevtipus (eNevtipus) fv-nyeben mas
--              SELECT-re nyitja a bemenoparameterkent kapott kurzorokat
  PROCEDURE bef_kurz_megny( n_nevtipus    IN     NUMBER,
                            n_ev          IN     NUMBER,
                            n_max_helyez  IN     NUMBER,
                            bef_cur       IN OUT t_bel_cur) AS
  BEGIN

    -- A megadott kurzor lehet nyitva, de az ujranyitas nem gond
    -- A DECODE-os resz a fele pontszam (1*) miatt van
    IF n_nevtipus = 0 THEN
      -- eVersNt : diszkv. versenyzo szerint
      OPEN bef_cur    FOR
        SELECT n.versenyzo,
               DECODE( SUBSTR(b.megjegyzes,13,1), '1', p.pontszam, '0', 2*p.pontszam),
               b.helyezes,
               n.kulcs,
               b.nagydij_nev
          FROM befuto b, nevezesi_lista n, szezon s, pontozas p
          WHERE b.szezon_eve  = n_ev         AND
                b.helyezes   <= n_max_helyez AND
                b.megjegyzes  LIKE ('_____________0__') AND
                s.szezon_eve  = n_ev         AND
                p.kulcs       = s.pontozas   AND
                p.helyezes    = b.helyezes   AND
                n.kulcs       = b.nvzl_kulcs
                ORDER BY n.versenyzo DESC, p.pontszam DESC ;

    ELSIF n_nevtipus = 1 THEN
      -- eNemzNt : diszkv. versenyzo szerint
      OPEN bef_cur    FOR
        SELECT n.nemzetiseg,
               DECODE( SUBSTR(b.megjegyzes,13,1), '1', p.pontszam, '0', 2*p.pontszam),
               b.helyezes,
               n.kulcs,
               b.nagydij_nev
          FROM befuto b, nevezesi_lista n, szezon s, pontozas p
          WHERE b.szezon_eve  = n_ev         AND
                b.helyezes   <= n_max_helyez AND
                b.megjegyzes  LIKE ('_____________0__') AND
                s.szezon_eve  = n_ev         AND
                p.kulcs       = s.pontozas   AND
                p.helyezes    = b.helyezes   AND
                n.kulcs       = b.nvzl_kulcs
                ORDER BY n.nemzetiseg DESC, p.pontszam DESC ;

    ELSIF n_nevtipus = 2 THEN
      -- eCsapNt : diszkv. csapat szerint
      OPEN bef_cur    FOR
        SELECT n.csapat,
               DECODE( SUBSTR(b.megjegyzes,13,1), '1', p.pontszam, '0', 2*p.pontszam),
               b.helyezes,
               n.kulcs,
               b.nagydij_nev
          FROM befuto b, nevezesi_lista n, szezon s, pontozas p
          WHERE b.szezon_eve  = n_ev         AND
                b.helyezes   <= n_max_helyez AND
                b.megjegyzes  LIKE ('______________0_') AND
                s.szezon_eve  = n_ev         AND
                p.kulcs       = s.pontozas   AND
                p.helyezes    = b.helyezes   AND
                n.kulcs       = b.nvzl_kulcs
                ORDER BY n.csapat DESC, p.pontszam DESC ;

    ELSIF n_nevtipus = 3 THEN
      -- eMotorNt : diszkv. csapat szerint
      OPEN bef_cur    FOR
        SELECT n.motor,
               DECODE( SUBSTR(b.megjegyzes,13,1), '1', p.pontszam, '0', 2*p.pontszam),
               b.helyezes,
               n.kulcs,
               b.nagydij_nev
          FROM befuto b, nevezesi_lista n, szezon s, pontozas p
          WHERE b.szezon_eve  = n_ev         AND
                b.helyezes   <= n_max_helyez AND
                b.megjegyzes  LIKE ('______________0_') AND
                s.szezon_eve  = n_ev         AND
                p.kulcs       = s.pontozas   AND
                p.helyezes    = b.helyezes   AND
                n.kulcs       = b.nvzl_kulcs
                ORDER BY n.motor DESC, p.pontszam DESC ;

    END IF ;

  END bef_kurz_megny ;

  PROCEDURE edzels_kurz_megny( n_nevtipus    IN     NUMBER,
                               n_ev          IN     NUMBER,
                               n_edz_els_pnt IN     NUMBER,
                               edzels_cur    IN OUT t_bel_cur) AS
  BEGIN

    -- A megadott kurzor lehet nyitva, de az ujranyitas nem gond
    IF n_nevtipus = 0 THEN
      -- eVersNt : diszkv. versenyzo szerint
      OPEN edzels_cur FOR
        SELECT  n.versenyzo, n_edz_els_pnt, r.helyezes, n.kulcs, r.nagydij_nev
          FROM rajtsorrend r, nevezesi_lista n
          WHERE r.szezon_eve  = n_ev         AND
                r.helyezes    = 1            AND
                r.megjegyzes  LIKE ('_____________0__') AND
                n.kulcs       = r.nvzl_kulcs
                ORDER BY n.versenyzo DESC ;

    ELSIF n_nevtipus = 1 THEN
      -- eNemzNt : diszkv. versenyzo szerint
      OPEN edzels_cur FOR
        SELECT  n.nemzetiseg, n_edz_els_pnt, r.helyezes, n.kulcs, r.nagydij_nev
          FROM rajtsorrend r, nevezesi_lista n
          WHERE r.szezon_eve  = n_ev         AND
                r.helyezes    = 1            AND
                r.megjegyzes  LIKE ('_____________0__') AND
                n.kulcs       = r.nvzl_kulcs
                ORDER BY n.nemzetiseg DESC ;

    ELSIF n_nevtipus = 2 THEN
      -- eCsapNt : diszkv. csapat szerint
      OPEN edzels_cur FOR
        SELECT  n.csapat, n_edz_els_pnt, r.helyezes, n.kulcs, r.nagydij_nev
          FROM rajtsorrend r, nevezesi_lista n
          WHERE r.szezon_eve  = n_ev         AND
                r.helyezes    = 1            AND
                r.megjegyzes  LIKE ('______________0_') AND
                n.kulcs       = r.nvzl_kulcs
                ORDER BY n.csapat DESC ;

    ELSIF n_nevtipus = 3 THEN
      -- eMotorNt : diszkv. csapat szerint
      OPEN edzels_cur FOR
        SELECT  n.motor, n_edz_els_pnt, r.helyezes, n.kulcs, r.nagydij_nev
          FROM rajtsorrend r, nevezesi_lista n
          WHERE r.szezon_eve  = n_ev         AND
                r.helyezes    = 1            AND
                r.megjegyzes  LIKE ('______________0_') AND
                n.kulcs       = r.nvzl_kulcs
                ORDER BY n.motor DESC ;

    END IF ;

  END edzels_kurz_megny ;

  PROCEDURE lggykr_kurz_megny( n_nevtipus    IN     NUMBER,
                               n_ev          IN     NUMBER,
                               n_lggykr_pnt  IN     NUMBER,
                               lggykr_cur    IN OUT t_bel_cur) AS
  BEGIN

    -- A megadott kurzor lehet nyitva, de az ujranyitas nem gond
    IF n_nevtipus = 0 THEN
      -- eVersNt : diszkv. versenyzo szerint

      OPEN lggykr_cur FOR
        SELECT  n.versenyzo, n_lggykr_pnt, 0, n.kulcs, l.nagydij_nev
          FROM leggyorsabb_kor l, nevezesi_lista n
          WHERE l.szezon_eve  = n_ev         AND
                n.kulcs       = l.nvzl_kulcs
                ORDER BY n.versenyzo DESC ;

    ELSIF n_nevtipus = 1 THEN
      -- eNemzNt : diszkv. versenyzo szerint

      OPEN lggykr_cur FOR
        SELECT  n.nemzetiseg, n_lggykr_pnt, 0, n.kulcs, l.nagydij_nev
          FROM leggyorsabb_kor l, nevezesi_lista n
          WHERE l.szezon_eve  = n_ev         AND
                n.kulcs       = l.nvzl_kulcs
                ORDER BY n.nemzetiseg DESC ;

    ELSIF n_nevtipus = 2 THEN
      -- eCsapNt : diszkv. csapat szerint
      OPEN lggykr_cur FOR
        SELECT  n.csapat, n_lggykr_pnt, 0, n.kulcs, l.nagydij_nev
          FROM leggyorsabb_kor l, nevezesi_lista n
          WHERE l.szezon_eve  = n_ev         AND
                n.kulcs       = l.nvzl_kulcs
                ORDER BY n.csapat DESC ;

    ELSIF n_nevtipus = 3 THEN
      -- eMotorNt : diszkv. csapat szerint
      OPEN lggykr_cur FOR
        SELECT  n.nemzetiseg, n_lggykr_pnt, 0, n.kulcs, l.nagydij_nev
          FROM leggyorsabb_kor l, nevezesi_lista n
          WHERE l.szezon_eve  = n_ev         AND
                n.kulcs       = l.nvzl_kulcs
                ORDER BY n.motor DESC ;

    END IF ;
  END lggykr_kurz_megny ;

-- max_ert_hlyz : Az elso hany befuto kap pontot
  FUNCTION max_ert_hlyz( n_szez_eve IN NUMBER ) RETURN NUMBER AS
    n_max_ert_hlyz NUMBER ;
  BEGIN
    SELECT MAX( p.helyezes) INTO n_max_ert_hlyz
      FROM szezon sz, pontozas p
      WHERE sz.szezon_eve = n_szez_eve AND
	          p.kulcs       = sz.pontozas ;
    RETURN n_max_ert_hlyz ;
  END max_ert_hlyz ;

-- max_ert_erdm : Hany legjobb eredmeny szamit be a vilagbajnoki osszesitesbe (csapatnal nem)
  FUNCTION max_ert_erdm( n_szez_eve IN NUMBER ) RETURN NUMBER AS
    n_max_ert_erdm NUMBER ;
  BEGIN
    SELECT elso_legj_besz INTO n_max_ert_erdm
      FROM szezon
      WHERE szezon_eve = n_szez_eve ;
    -- Ha a szezon.elso_legj_besz == 0, akkor az osszes eredmeny beszamit
    IF n_max_ert_erdm = 0 THEN
      SELECT COUNT(*) INTO n_max_ert_erdm
        FROM nagydijak
        WHERE szezon_eve = n_szez_eve ;
      -- Hogy a leggyorsabb koroket es az edzeselsosegekert jaro pontokat is
      -- figyelembe vegyuk : felso becsles az eredmenyek maximalis szamara :
      n_max_ert_erdm := 3*n_max_ert_erdm ;
    END IF ;
    RETURN n_max_ert_erdm ;
  END max_ert_erdm ;

-- helyez_osszeh
  -- A fuggveny a nev_pont_tab ket nevet hasonlitja ossze,
  -- melyiknek van tobb magasabb pontszamu eredmenye (a helyezes helyett[futgy. ? edzels])
  -- 1 : n_poz_egy > n_poz_ket, 0 : n_poz_egy > n_poz_ket -1 : n_poz_egy < n_poz_ket
  FUNCTION helyez_osszeh( n_kulcs_egy IN NUMBER, n_kulcs_ket IN NUMBER ) RETURN NUMBER AS
    n_poz_egy NUMBER := 0 ;
    n_poz_ket NUMBER := 0 ;
    n_relacio NUMBER := 0 ;
  BEGIN
-- dbms_output.put_line('helyez_osszeh: BEGIN') ;
--    n_poz_egy := 0 ;
--    n_poz_ket := 0 ;
--    n_relacio := 0 ;
    -- Feltesszuk, hogy biztos vannak ilyen kulcsok
    WHILE nev_pont_tab(n_poz_egy).nev_kulcs != n_kulcs_egy LOOP
      n_poz_egy := n_poz_egy + 1 ;
    END LOOP ;
    WHILE nev_pont_tab(n_poz_ket).nev_kulcs != n_kulcs_ket LOOP
      n_poz_ket := n_poz_ket + 1 ;
    END LOOP ;
    WHILE n_poz_egy < n_max_eredm_idx AND n_poz_ket < n_max_eredm_idx AND
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
/*
dbms_output.put_line('n_max_eredm_idx : ' || TO_CHAR(n_max_eredm_idx)) ;
dbms_output.put_line('n_poz_egy : ' || TO_CHAR(n_poz_egy)) ;
dbms_output.put_line('n_poz_ket : ' || TO_CHAR(n_poz_ket)) ;
dbms_output.put_line('helyez_osszeh : END') ;
*/
    IF n_poz_egy < n_max_eredm_idx AND nev_pont_tab(n_poz_egy).nev_kulcs = n_kulcs_egy THEN
      n_relacio := n_poz_egy + 1 ;
    END IF ;
    IF n_poz_ket < n_max_eredm_idx AND nev_pont_tab(n_poz_ket).nev_kulcs = n_kulcs_ket THEN
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
--    n_sz2_idx  NUMBER ;
    n_ossz_ert  NUMBER ;
    szez_all_sor t_szez_all_sor ;

f utl_file.file_type ;

  BEGIN
f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;
utl_file.put_line( f, 'szez_eredm_megh : BEGIN') ;
utl_file.put_line( f, 'n_eredm_szama (IN):' || TO_CHAR(n_eredm_szama)) ;

    -- Ha nincs beszamitando eredmeny, itt kiszall, mert kesobb 
    -- n_max_szez_idx = n_sz_idx + 1 (n_max_szez_idx meret, nem index tulajdonkeppen
    -- mert n_max_szez_idx == 0 -> 1 elem ? 0 elem ?
    IF n_eredm_szama = 0 THEN
      n_max_szez_idx := 0 ;
      RETURN ;
    END IF ;

    -- Osszeadjuk a pontokat nev_pont_tab -> szez_all_tab
    n_sz_idx  := 0 ;
--                                            \/ lehet NOTFOUND, ha egy sem volt !
    szez_all_tab(0).nev_kulcs := nev_pont_tab(0).nev_kulcs ;
    szez_all_tab(0).pont      := nev_pont_tab(0).pont ;
    -- Az indexek o alapuak
--    FOR n_pnt_idx IN 1 .. n_eredm_szama - 1 LOOP
    FOR n_pnt_idx IN 1 .. n_eredm_szama LOOP
utl_file.put_line( f, 'n_sz_idx:' || TO_CHAR( n_sz_idx)) ;
      IF szez_all_tab(n_sz_idx).nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs THEN
utl_file.put_line( f, 'n_sz_idx += n_pnt_idx ' || TO_CHAR( n_sz_idx) || ' += ' || TO_CHAR( n_pnt_idx)) ;
utl_file.put_line( f, 'nev_pont_tab(' || TO_CHAR( n_pnt_idx) || ').nev_kulcs:' || TO_CHAR( nev_pont_tab(n_pnt_idx).nev_kulcs)) ;
utl_file.put_line( f, 'nev_pont_tab(' || TO_CHAR( n_pnt_idx) || ').pont:' || TO_CHAR( nev_pont_tab(n_pnt_idx).pont)) ;
        szez_all_tab(n_sz_idx).pont := szez_all_tab(n_sz_idx).pont + nev_pont_tab(n_pnt_idx).pont ;
      ELSE
        n_sz_idx := n_sz_idx + 1 ;
utl_file.put_line( f, 'n_sz_idx <- n_pnt_idx ' || TO_CHAR( n_sz_idx) || ' <- ' || TO_CHAR( n_pnt_idx)) ;
utl_file.put_line( f, 'nev_pont_tab(' || TO_CHAR( n_pnt_idx) || ').nev_kulcs:' || TO_CHAR( nev_pont_tab(n_pnt_idx).nev_kulcs)) ;
utl_file.put_line( f, 'nev_pont_tab(' || TO_CHAR( n_pnt_idx) || ').pont:' || TO_CHAR( nev_pont_tab(n_pnt_idx).pont)) ;
        szez_all_tab(n_sz_idx).nev_kulcs := nev_pont_tab(n_pnt_idx).nev_kulcs ;
        szez_all_tab(n_sz_idx).pont      := nev_pont_tab(n_pnt_idx).pont      ;
      END IF ;
    END LOOP ;

-- dbms_output.put_line('n_sz_idx:'||TO_CHAR(n_sz_idx)) ;

    FOR n_pnt_idx IN 0 .. n_sz_idx LOOP
utl_file.put_line( f, 'n_pnt_idx '||TO_CHAR(n_pnt_idx)) ;
utl_file.put_line( f, 'nev_kulcs '||TO_CHAR(szez_all_tab(n_pnt_idx).nev_kulcs)) ;
utl_file.put_line( f, 'pont      '||TO_CHAR(szez_all_tab(n_pnt_idx).pont)) ;
utl_file.put_line( f, '---------------------') ;
    END LOOP ;

utl_file.put_line( f, 'Sorbarendezzuk a neveket') ;
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
    n_max_szez_idx := n_sz_idx + 1 ;
-- dbms_output.put_line('n_max_szez_idx : ' || TO_CHAR(n_max_szez_idx)) ;
/*
    FOR n_pnt_idx IN 0 .. n_sz_idx LOOP dbms_output.put_line('n_pnt_idx '||TO_CHAR(n_pnt_idx)) ;
dbms_output.put_line('nev_kulcs '||TO_CHAR(szez_all_tab(n_pnt_idx).nev_kulcs)) ;
dbms_output.put_line('---------------------') ;
    END LOOP ;
*/
utl_file.put_line( f, 'n_max_szez_idx : ' || TO_CHAR(n_max_szez_idx)) ;
-- dbms_output.put_line('szez_eredm_megh : END') ;
utl_file.put_line( f, 'szez_eredm_megh : END') ;
utl_file.fflush( f) ;
utl_file.fclose( f) ;
  END szez_eredm_megh ;

-- szez_pill_all_v
  PROCEDURE szez_pill_all_v( n_szez_eve IN NUMBER) AS

    -- Kurzorvaltozok, fix kurzorok helyett, hogy mindig csak annyit
    -- nyissunk meg, amennyit kell (OPEN-nel mindent elhoz)
    bef_cur    t_bel_cur ;
    edzels_cur t_bel_cur ;
    lggykr_cur t_bel_cur ;

    r_pont_sor   t_nev_pont_sor ;
    b_pont_sor   t_nev_pont_sor ;
    l_pont_sor   t_nev_pont_sor ;

    n_edz_els_pnt  NUMBER ;
    n_lggyk_pnt    NUMBER ;
    n_max_ert_hlyz NUMBER ;
    n_max_ert_erdm NUMBER ;
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
    -- 'b' - befuto eredmeny, 'r' - edzeselsoseg, 'l' - leggyorsabb kor, ' ' - nem lett adat elhozva
    c_eredm_tip    CHAR   ;
f utl_file.file_type ;
  BEGIN
f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;
utl_file.put_line( f, 'szez_pill_all_v : BEGIN') ;
-- dbms_output.enable(500000) ;
-- dbms_output.put_line('szez_pill_all_v : BEGIN') ;

    nev_pont_tab.DELETE ;
    szez_all_tab.DELETE ;
    -- Elhozasnal hol tart eppen : inicializalas
    n_akt_nev_idx   := 0 ;
    n_akt_eredm_idx := 0 ;

    -- Hany pont jar az edzeselsosegert az adott szezonban ?
    SELECT edz_els_pont
      INTO n_edz_els_pnt
      FROM szezon
      WHERE szezon_eve = n_szez_eve ;
utl_file.put_line( f, 'n_edz_els_pnt:' || TO_CHAR( n_edz_els_pnt)) ;

    -- Hany pont jar a leggyorsabb korert az adott szezonban ?
    SELECT leggy_kor_pont
      INTO n_lggyk_pnt
      FROM szezon
      WHERE szezon_eve = n_szez_eve ;
utl_file.put_line( f, 'n_lggyk_pnt:' || TO_CHAR( n_lggyk_pnt)) ;

    n_max_ert_hlyz := max_ert_hlyz( n_szez_eve) ;
utl_file.put_line( f, 'n_max_ert_hlyz:' || TO_CHAR( n_max_ert_hlyz)) ;

    c_eredm_tip := ' ' ;
    -- A befuto eredmenyert mindenkeppen jar pont, ez a kurzor mindig kell
    -- 0 - versenyzo nevtipus
    bef_kurz_megny( 0, n_szez_eve, n_max_ert_hlyz, bef_cur) ;

    FETCH bef_cur INTO b_pont_sor ;
    -- A FETCH nem kuld EXCEPTIONt, ha nincs tobb adat !
    IF bef_cur%FOUND THEN
      nev_pont_tab(0) := b_pont_sor ;
utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(0)||') <- b_pont_sor:') ;
utl_file.put_line( f, '__nev_kulcs:' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
utl_file.put_line( f, '__pont:'      || TO_CHAR(b_pont_sor.pont) ) ;
      c_eredm_tip := 'b' ;
    ELSE
      CLOSE bef_cur ;
    END IF ;
utl_file.put_line( f, 'bef_cur:') ;
utl_file.put_line( f, '__nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
-- utl_file.put_line( f, '__pont' || TO_CHAR(b_pont_sor.pont) ) ;
-- utl_file.put_line( f, '__helyezes' || TO_CHAR(b_pont_sor.helyezes) ) ;
-- utl_file.put_line( f, '__nvzl_kulcs' || TO_CHAR(b_pont_sor.nvzl_kulcs) ) ;
-- utl_file.put_line( f, '__nagydij_nev:' || TO_CHAR(b_pont_sor.nagydij_nev) ) ;
    -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
    -- Az eredmeny tomb kezdopoziciojara a legelso (legnagyobb) kulcsot
    -- a legnagyobb pontszammal tesszuk
    IF n_edz_els_pnt > 0 THEN
      -- 0 - versenyzo nevtipus
      edzels_kurz_megny( 0, n_szez_eve, n_edz_els_pnt, edzels_cur) ;

      FETCH edzels_cur INTO r_pont_sor ;
utl_file.put_line( f, 'FETCH edzels_cur:') ;
utl_file.put_line( f, '__nev_kulcs' || TO_CHAR(r_pont_sor.nev_kulcs) ) ;
      IF edzels_cur%FOUND THEN
        IF r_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
          nev_pont_tab(0) := r_pont_sor ;
          c_eredm_tip := 'r' ;
        ELSE
          IF r_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(0).pont THEN
            nev_pont_tab(0) := r_pont_sor ;
            c_eredm_tip := 'r' ;
          END IF ;
        END IF ;
      ELSE
        CLOSE edzels_cur ;
      END IF ;
    END IF ;

    IF n_lggyk_pnt > 0 THEN
      -- 0 - versenyzo nevtipus
      lggykr_kurz_megny( 0, n_szez_eve, n_lggyk_pnt, lggykr_cur) ;

      FETCH lggykr_cur INTO l_pont_sor ;
utl_file.put_line( f, 'FETCH lggykr_cur:') ;
utl_file.put_line( f, '__nev_kulcs' || TO_CHAR(l_pont_sor.nev_kulcs) ) ;
      IF lggykr_cur%FOUND THEN
        IF l_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
          nev_pont_tab(0) := l_pont_sor ;
          c_eredm_tip := 'l' ;
        ELSE
          IF l_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(0).pont THEN
            nev_pont_tab(0) := l_pont_sor ;
            c_eredm_tip := 'l' ;
          END IF ;
        END IF ;
      ELSE
        CLOSE lggykr_cur ;
      END IF ;
    END IF ;

    -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
    -- 'AND *_cur%ISOPEN' kiveve, mert kulonben c_eredm_tip = ' ' lenne
    IF c_eredm_tip = 'b' THEN
utl_file.put_line( f, 'FETCH bef_cur (uj elhoz)') ;
      FETCH bef_cur INTO b_pont_sor ;
utl_file.put_line( f, '__nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
      IF bef_cur%NOTFOUND THEN
        CLOSE bef_cur ;
      END IF ;
    ELSE
      IF c_eredm_tip = 'r' THEN
utl_file.put_line( f, 'FETCH edzels_cur (uj elhoz)') ;
        FETCH edzels_cur INTO r_pont_sor ;
        IF edzels_cur%NOTFOUND THEN
          CLOSE edzels_cur ;
        END IF ;
      ELSE
        IF c_eredm_tip = 'l' THEN
utl_file.put_line( f, 'FETCH lggykr_cur (uj elhoz)') ;
          FETCH lggykr_cur INTO l_pont_sor ;
          IF lggykr_cur%NOTFOUND THEN
            CLOSE lggykr_cur ;
          END IF ;
        ELSE
          -- Nem tudta az egyik fajtabol sem elhozni az elsot : ures szezon -> kiszall
          n_max_eredm_idx := 0 ;
utl_file.put_line( f, 'szez_pill_all_v : END') ;
          RETURN ;
        END IF ;
      END IF ;
    END IF ;

    -- n_max_eredm_idx-t ||-an futtatom n_pnt_idx-szel
    n_max_eredm_idx := 1 ;
    n_kiv_eredm     := 1 ;
    n_pnt_idx       := 1 ;

    n_max_ert_erdm := max_ert_erdm( n_szez_eve) ;
utl_file.put_line( f, 'n_max_ert_erdm:' || TO_CHAR(n_max_ert_erdm)) ;

    LOOP
      -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
      c_eredm_tip := ' ' ;
utl_file.put_line( f, 'n_kiv_eredm:'||TO_CHAR(n_kiv_eredm)||' < n_max_ert_erdm:'||TO_CHAR(n_max_ert_erdm)) ;
      IF n_kiv_eredm < n_max_ert_erdm THEN
utl_file.put_line( f, 'n_pnt_idx:' || TO_CHAR( n_pnt_idx)) ;
utl_file.put_line( f, 'b_pont_sor.nev_kulcs:' || TO_CHAR( b_pont_sor.nev_kulcs)) ;
utl_file.put_line( f, 'nev_pont_tab(n_pnt_idx-1).nev_kulcs:' || TO_CHAR( nev_pont_tab(n_pnt_idx-1).nev_kulcs)) ;
        -- Van-e ezzel a kulccsal meg feldolgozatlan bejegyzes (*_cur%ISOPEN)
        IF b_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs AND bef_cur%ISOPEN THEN
utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- b_pont_sor:') ;

utl_file.put_line( f, '__nev_kulcs:'   || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
utl_file.put_line( f, '__pont:'        || TO_CHAR(b_pont_sor.pont) ) ;
-- utl_file.put_line( f, '__helyezes:'    || TO_CHAR(b_pont_sor.helyezes) ) ;
-- utl_file.put_line( f, '__nvzl_kulcs:'  || TO_CHAR(b_pont_sor.nvzl_kulcs) ) ;
-- utl_file.put_line( f, '__nagydij_nev:' || TO_CHAR(b_pont_sor.nagydij_nev) ) ;

          nev_pont_tab(n_pnt_idx) := b_pont_sor ;
          c_eredm_tip := 'b' ;
        END IF ;
        -- Elso a *_pont_sor ervenyessegvizsgalata, mert kesobb az r_pont_sor-t
        -- mindenkeppen hasznalja osszehasonlitasra !
        -- (Azert igy, mert amikor ujat akar elhozni es nincs, lezarja.)
        IF edzels_cur%ISOPEN THEN
          IF c_eredm_tip = ' ' THEN
             -- Nincs uj elem, az elozohoz kell hasonlitani - akkor meg minek a pontot belevenni ?!?
            IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs THEN
utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- r_pont_sor') ;
              nev_pont_tab(n_pnt_idx) := r_pont_sor ;
              c_eredm_tip := 'r' ;
            END IF ;
          ELSE
             -- Uj eleme kerult a tombbe, ehhez kell hasonlitani
            IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- r_pont_sor') ;
              nev_pont_tab(n_pnt_idx) := r_pont_sor ;
              c_eredm_tip := 'r' ;
            END IF ;
          END IF ;
        END IF ;

        IF lggykr_cur%ISOPEN THEN
          IF c_eredm_tip = ' ' THEN
            -- Nincs uj elem, az elozohoz kell hasonlitani - akkor meg minek a pontot belevenni ?!?
            IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs THEN
utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- l_pont_sor') ;
              nev_pont_tab(n_pnt_idx) := l_pont_sor ;
              c_eredm_tip := 'l' ;
            END IF ;
          ELSE
            -- Uj elemet kerult a tombbe, ehhez kell hasonlitani
            IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- l_pont_sor') ;
              nev_pont_tab(n_pnt_idx) := l_pont_sor ;
              c_eredm_tip := 'l' ;
            END IF ;
          END IF ;
        END IF ;
      END IF ;

utl_file.put_line( f, 'Amelyik tenyleg bekerult, abbol ujat kell elhozni >' || c_eredm_tip || '<') ;

      -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
      -- 'AND *_cur%ISOPEN' kiveve, mert kulonben c_eredm_tip = ' ' lenne
      IF c_eredm_tip = 'b' THEN
        n_max_eredm_idx := n_max_eredm_idx + 1 ;
          n_pnt_idx   := n_pnt_idx + 1 ;
          n_kiv_eredm := n_kiv_eredm + 1 ;
utl_file.put_line( f, 'FETCH bef_cur (uj elhoz)') ;
        FETCH bef_cur INTO b_pont_sor ;
utl_file.put_line( f, 'FETCH(bef_cur):__nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
        IF bef_cur%NOTFOUND THEN
utl_file.put_line( f, 'CLOSE bef_cur') ;
          CLOSE bef_cur ;
        END IF ;
      ELSE
        IF c_eredm_tip = 'r' THEN
          n_max_eredm_idx := n_max_eredm_idx + 1 ;
          n_pnt_idx   := n_pnt_idx + 1 ;
          n_kiv_eredm := n_kiv_eredm + 1 ;
utl_file.put_line( f, 'FETCH edzels_cur') ;
          FETCH edzels_cur INTO r_pont_sor ;
          IF edzels_cur%NOTFOUND THEN
utl_file.put_line( f, 'CLOSE edzels_cur') ;
            CLOSE edzels_cur ;
          END IF ;
        ELSE
          IF c_eredm_tip = 'l' THEN
            n_max_eredm_idx := n_max_eredm_idx + 1 ;
              n_pnt_idx   := n_pnt_idx + 1 ;
              n_kiv_eredm := n_kiv_eredm + 1 ;
utl_file.put_line( f, 'FETCH lggykr_cur') ;
            FETCH lggykr_cur INTO l_pont_sor ;
            IF lggykr_cur%NOTFOUND THEN
utl_file.put_line( f, 'CLOSE lggykr_cur') ;
              CLOSE lggykr_cur ;
            END IF ;
          ELSE
            -- c_eredm_tip = ' '
            -- Nincs tobb eredmeny ezzel a nev kulccsal vagy
            -- elertuk a maximalis ertekelheto eredmenyek szamat -> kovetkezo nev
            -- Valamilyen rendszer szerint vegig kell menni a neveken :
            -- legyen ez a novekvo kulcs
-- utl_file.put_line( f, 'c_eredm_tip = " " ') ;
utl_file.put_line( f, 'n_pnt_idx:' || TO_CHAR( n_pnt_idx)) ;
            -- Lepjuk at mindharom helyen a regi nevet !
            -- NYITOTT KURZOR ? NINCS TOBB ADAT ?
utl_file.put_line( f, 'atlepes ?') ;
            IF bef_cur%ISOPEN THEN
utl_file.put_line( f, 'befuto eredmenybeli nevek atlepese') ;
              WHILE nev_pont_tab(n_pnt_idx-1).nev_kulcs = b_pont_sor.nev_kulcs LOOP
utl_file.put_line( f, 'atlep : __nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
                FETCH bef_cur INTO b_pont_sor ;
                EXIT WHEN bef_cur%NOTFOUND ;
              END LOOP ;
              IF bef_cur%NOTFOUND THEN
                CLOSE bef_cur ;
              END IF ;
            END IF ;

            IF edzels_cur%ISOPEN THEN
utl_file.put_line( f, 'rajtsorrend eredmenybeli nevek atlepese') ;
-- utl_file.put_line( f, 'r_pont_sor.nev_kulcs: ' || TO_CHAR(r_pont_sor.nev_kulcs)) ;
-- utl_file.put_line( f, 'nev_pont_tab(' || TO_CHAR(n_pnt_idx-1) || ').nev_kulcs: ' || TO_CHAR(nev_pont_tab(n_pnt_idx-1).nev_kulcs)) ;
-- utl_file.put_line( f, 'nev_pont_tab(' || TO_CHAR(n_pnt_idx) || ').nev_kulcs: ' || TO_CHAR(nev_pont_tab(n_pnt_idx).nev_kulcs)) ;
              WHILE nev_pont_tab(n_pnt_idx-1).nev_kulcs = r_pont_sor.nev_kulcs LOOP
utl_file.put_line( f, 'atlep : __nev_kulcs' || TO_CHAR(r_pont_sor.nev_kulcs) ) ;
                FETCH edzels_cur INTO r_pont_sor ;
                EXIT WHEN edzels_cur%NOTFOUND ;
              END LOOP ;
              IF edzels_cur%NOTFOUND THEN
                CLOSE edzels_cur ;
              END IF ;
            END IF ;

            IF lggykr_cur%ISOPEN THEN
utl_file.put_line( f, 'lggyk eredmenybeli nevek atlepese') ;
-- utl_file.put_line( f, 'nev_pont_tab(' || TO_CHAR(n_pnt_idx) || ').nev_kulcs: ' || TO_CHAR(nev_pont_tab(n_pnt_idx).nev_kulcs)) ;
              WHILE nev_pont_tab(n_pnt_idx-1).nev_kulcs = l_pont_sor.nev_kulcs LOOP
utl_file.put_line( f, 'atlep : __nev_kulcs' || TO_CHAR(l_pont_sor.nev_kulcs) ) ;
                FETCH lggykr_cur INTO l_pont_sor ;
                EXIT WHEN lggykr_cur%NOTFOUND ;
              END LOOP ;
              IF lggykr_cur%NOTFOUND THEN
                CLOSE lggykr_cur ;
              END IF ;
            END IF ;
            
            n_kiv_eredm := 0 ;

            -- Melyik a legnagyobb (nev_kulcs, pont) ?
            c_eredm_tip := ' ' ;
            -- VAN-E MEG ADAT ? b_pont_sor MAR EGYSZER FEL LETT HASZNALVA ?
            IF bef_cur%ISOPEN THEN
utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- b_pont_sor:') ;

utl_file.put_line( f, '__nev_kulcs:'   || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
utl_file.put_line( f, '__pont:'        || TO_CHAR(b_pont_sor.pont) ) ;
-- utl_file.put_line( f, '__helyezes:'    || TO_CHAR(b_pont_sor.helyezes) ) ;
-- utl_file.put_line( f, '__nvzl_kulcs:'  || TO_CHAR(b_pont_sor.nvzl_kulcs) ) ;
-- utl_file.put_line( f, '__nagydij_nev:' || TO_CHAR(b_pont_sor.nagydij_nev) ) ;
              nev_pont_tab(n_pnt_idx) := b_pont_sor ;
              c_eredm_tip := 'b' ;
            END IF ;
            -- JAR-E PONT EZERT EGYALTALAN ?
            IF edzels_cur%ISOPEN THEN
utl_file.put_line( f, 'edzels_cur%ISOPEN') ;
              IF r_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
                nev_pont_tab(n_pnt_idx) := r_pont_sor ;
                c_eredm_tip := 'r' ;
              ELSE
                IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                  nev_pont_tab(n_pnt_idx) := r_pont_sor ;
                  c_eredm_tip := 'r' ;
                END IF ;
              END IF ;
            END IF ;
            -- JAR-E PONT EZERT EGYALTALAN ?
            IF lggykr_cur%ISOPEN THEN
utl_file.put_line( f, 'lggykr_cur%ISOPEN') ;
              IF l_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
                nev_pont_tab(n_pnt_idx) := l_pont_sor ;
                c_eredm_tip := 'l' ;
              ELSE
                IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                  nev_pont_tab(n_pnt_idx) := l_pont_sor ;
                  c_eredm_tip := 'l' ;
                END IF ;
              END IF ;
            END IF ;

            -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
utl_file.put_line( f, 'Amelyik tenyleg bekerult, abbol ujat kell elhozni >' || c_eredm_tip || '<') ;
            IF c_eredm_tip = 'b' AND bef_cur%ISOPEN THEN
              n_max_eredm_idx := n_max_eredm_idx + 1 ;
                n_pnt_idx := n_pnt_idx + 1 ;
                n_kiv_eredm := n_kiv_eredm + 1 ;
utl_file.put_line( f, 'FETCH bef_cur (uj elhoz)') ;
              FETCH bef_cur INTO b_pont_sor ;
utl_file.put_line( f, '__nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
              IF bef_cur%NOTFOUND THEN
utl_file.put_line( f, 'CLOSE bef_cur') ;
                CLOSE bef_cur ;
              END IF ;
            ELSE
              IF c_eredm_tip = 'r' AND edzels_cur%ISOPEN THEN
                n_max_eredm_idx := n_max_eredm_idx + 1 ;
                  n_pnt_idx := n_pnt_idx + 1 ;
                  n_kiv_eredm := n_kiv_eredm + 1 ;
utl_file.put_line( f, 'FETCH edzels_cur (uj elhoz)') ;
                FETCH edzels_cur INTO r_pont_sor ;
                IF edzels_cur%NOTFOUND THEN
utl_file.put_line( f, 'CLOSE edzels_cur') ;
                  CLOSE edzels_cur ;
                END IF ;
              ELSE
                IF c_eredm_tip = 'l' AND lggykr_cur%ISOPEN THEN
                  n_max_eredm_idx := n_max_eredm_idx + 1 ;
                    n_pnt_idx := n_pnt_idx + 1 ;
                    n_kiv_eredm := n_kiv_eredm + 1 ;
utl_file.put_line( f, 'FETCH lggykr_cur (uj elhoz)') ;
                  FETCH lggykr_cur INTO l_pont_sor ;
                  IF lggykr_cur%NOTFOUND THEN
utl_file.put_line( f, 'CLOSE lggykr_cur') ;
                    CLOSE lggykr_cur ;
                  END IF ;
                ELSE
utl_file.put_line( f, '-- EXIT LOOP --------------------') ;
                  EXIT ; -- WHEN c_eredm_tip = ' '
                END IF ;
              END IF ;
            END IF ; -- c_eredm_tip = ...
            
          END IF ;
        END IF ;
      END IF ;
utl_file.put_line( f, '-- ciklusmag vege --------------------') ;
    END LOOP ;

    -- A nem nyitott kurzor 'lezarasa' hibat okoz
    IF bef_cur%ISOPEN THEN
      CLOSE bef_cur ;
    END IF ;
    IF edzels_cur%ISOPEN THEN
      CLOSE edzels_cur ;
    END IF ;
    IF lggykr_cur%ISOPEN THEN
      CLOSE lggykr_cur ;
    END IF ;

--dbms_output.put_line('n_max_eredm_idx : ' || TO_CHAR(n_max_eredm_idx)) ;
utl_file.put_line( f, 'n_max_eredm_idx:' || TO_CHAR( n_max_eredm_idx)) ;
utl_file.put_line( f, 'n_pnt_idx:' || TO_CHAR( n_pnt_idx)) ;
utl_file.fflush( f) ;
utl_file.fclose( f) ;

    szez_eredm_megh( n_pnt_idx - 1 ) ;

f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;
utl_file.put_line( f, 'szez_pill_all_v : END') ;
-- dbms_output.put_line( 'szez_pill_all_v : END') ;
utl_file.fflush( f) ;
utl_file.fclose( f) ;
  END szez_pill_all_v ;

-- szez_pill_all_cs
  PROCEDURE szez_pill_all_cs( n_szez_eve IN NUMBER, n_nevtipus IN NUMBER) AS

    bef_cur    t_bel_cur ;
    edzels_cur t_bel_cur ;
    lggykr_cur t_bel_cur ;

    r_pont_sor   t_nev_pont_sor ;
    b_pont_sor   t_nev_pont_sor ;
    l_pont_sor   t_nev_pont_sor ;

    n_edz_els_pnt  NUMBER ;
    n_lggyk_pnt    NUMBER ;
    n_max_ert_hlyz NUMBER ;
    n_max_ert_erdm NUMBER ;
    -- A ciklusban aktualis nev kulcsa
    n_akt_kulcs    NUMBER ;

    n_pnt_idx      NUMBER ;
    n_szez_idx     NUMBER ;
    n_szz2_idx     NUMBER ;
    -- Csak mindharom fajta eredmeny levizsgalasa utan tudjuk, melyik volt az
    -- alkalmas. Emiatt kell ez a valtozo, melyet minden tombbe irasnal
    -- aktualizalok, igy a vegen lehet tudni melyik fajta kerult a tombbe,
    -- melyikbol kell ujat hozni.
    -- 'b' - befuto eredmeny, 'r' - edzeselsoseg, 'l' - leggyorsabb kor
    c_eredm_tip    CHAR := ' ' ;
--f utl_file.file_type ;
  BEGIN
--f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;
-- utl_file.put_line( f, 'szez_pill_all_cs BEGIN') ;
    -- Elhozasnal hol tart eppen : inicializalas
    n_akt_nev_idx   := 0 ;
    n_akt_eredm_idx := 0 ;
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

    n_max_ert_hlyz := max_ert_hlyz( n_szez_eve) ;

    -- A befuto eredmenyert mindenkeppen jar pont, ez a kurzor mindig kell
    bef_kurz_megny( n_nevtipus, n_szez_eve, n_max_ert_hlyz, bef_cur) ;

    FETCH bef_cur INTO b_pont_sor ;
    IF bef_cur%FOUND THEN
      -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
      nev_pont_tab(0) := b_pont_sor ;
      c_eredm_tip := 'b' ;
    ELSE
      CLOSE bef_cur ;
    END IF ;

    -- Az eredmeny tomb kezdopoziciojara a legelso (legnagyobb) kulcsot
    -- a legnagyobb pontszammal tesszuk
    IF n_edz_els_pnt > 0 THEN

      edzels_kurz_megny( n_nevtipus, n_szez_eve, n_edz_els_pnt, edzels_cur) ;

      FETCH edzels_cur INTO r_pont_sor ;
      IF edzels_cur%FOUND THEN
        IF r_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
          nev_pont_tab(0) := r_pont_sor ;
          c_eredm_tip := 'r' ;
        ELSE
          IF r_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(0).pont THEN
            nev_pont_tab(0) := r_pont_sor ;
            c_eredm_tip := 'r' ;
          END IF ;
        END IF ;
      ELSE
        CLOSE edzels_cur ;
      END IF ;
    END IF ;

    IF n_lggyk_pnt > 0 THEN

      lggykr_kurz_megny( n_nevtipus, n_szez_eve, n_lggyk_pnt, lggykr_cur) ;

      FETCH lggykr_cur INTO l_pont_sor ;
      IF lggykr_cur%FOUND THEN
        IF l_pont_sor.nev_kulcs > nev_pont_tab(0).nev_kulcs THEN
          nev_pont_tab(0) := l_pont_sor ;
          c_eredm_tip := 'l' ;
        ELSE
          IF l_pont_sor.nev_kulcs = nev_pont_tab(0).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(0).pont THEN
            nev_pont_tab(0) := l_pont_sor ;
            c_eredm_tip := 'l' ;
          END IF ;
        END IF ;
      ELSE
        CLOSE lggykr_cur ;
      END IF ;
    END IF ;

    -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
    -- 'AND *_cur%ISOPEN' kiveve, mert kulonben c_eredm_tip = ' ' lenne
    IF c_eredm_tip = 'b' THEN
-- utl_file.put_line( f, 'FETCH bef_cur') ;
      FETCH bef_cur INTO b_pont_sor ;
-- utl_file.put_line( f, '__nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
      IF bef_cur%NOTFOUND THEN
        CLOSE bef_cur ;
      END IF ;
    ELSE
      IF c_eredm_tip = 'r' THEN
-- utl_file.put_line( f, 'FETCH edzels_cur') ;
        FETCH edzels_cur INTO r_pont_sor ;
        IF edzels_cur%NOTFOUND THEN
          CLOSE edzels_cur ;
        END IF ;
      ELSE
-- utl_file.put_line( f, 'FETCH lggykr_cur') ;
        IF c_eredm_tip = 'l' THEN
          FETCH lggykr_cur INTO l_pont_sor ;
          IF lggykr_cur%NOTFOUND THEN
            CLOSE lggykr_cur ;
          END IF ;
        ELSE
          -- Nem tudta az egyik fajtabol sem elhozni az elsot : ures szezon -> kiszall
          n_max_eredm_idx := 0 ;
-- utl_file.put_line( f, 'szez_pill_all_cs : END') ;
          RETURN ;
        END IF ;
      END IF ;
    END IF ;

    -- n_max_eredm_idx-t ||-an futtatom n_pnt_idx-szel
    n_max_eredm_idx := 1 ;
    n_pnt_idx := 1 ;

    LOOP
      -- Kikeressuk a legnagyobb nev kulcsot a legnagyobb pontszammal
      c_eredm_tip := ' ' ;
-- utl_file.put_line( f, 'c_eredm_tip:' || c_eredm_tip) ;
-- utl_file.put_line( f, 'n_pnt_idx:' || TO_CHAR( n_pnt_idx)) ;
-- utl_file.put_line( f, 'b_pont_sor.nev_kulcs:' || TO_CHAR( b_pont_sor.nev_kulcs)) ;
-- utl_file.put_line( f, 'nev_pont_tab(n_pnt_idx-1).nev_kulcs:' || TO_CHAR( nev_pont_tab(n_pnt_idx-1).nev_kulcs)) ;
        -- Van-e ezzel a kulccsal meg feldolgozatlan bejegyzes (*_cur%ISOPEN)
      IF b_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs AND bef_cur%ISOPEN THEN
-- utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- b_pont_sor:') ;

-- utl_file.put_line( f, '__nev_kulcs:'   || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
-- utl_file.put_line( f, '__pont:'        || TO_CHAR(b_pont_sor.pont) ) ;
-- utl_file.put_line( f, '__helyezes:'    || TO_CHAR(b_pont_sor.helyezes) ) ;
-- utl_file.put_line( f, '__nvzl_kulcs:'  || TO_CHAR(b_pont_sor.nvzl_kulcs) ) ;
-- utl_file.put_line( f, '__nagydij_nev:' || TO_CHAR(b_pont_sor.nagydij_nev) ) ;

        nev_pont_tab(n_pnt_idx) := b_pont_sor ;
        c_eredm_tip := 'b' ;
      END IF ;
      -- Elso a *_pont_sor ervenyessegvizsgalata, mert kesobb az r_pont_sor-t
      -- mindenkeppen hasznalja osszehasonlitasra !
      -- (Azert igy, mert amikor ujat akar elhozni es nincs, lezarja.)
      IF edzels_cur%ISOPEN THEN
        IF c_eredm_tip = ' ' THEN
          -- Nincs uj elem, az elozohoz kell hasonlitani
          IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx-1).pont THEN
            nev_pont_tab(n_pnt_idx) := r_pont_sor ;
            c_eredm_tip := 'r' ;
          END IF ;
        ELSE
          -- Uj elemet kerult a tombbe, ehhez kell hasonlitani
          IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
            nev_pont_tab(n_pnt_idx) := r_pont_sor ;
            c_eredm_tip := 'r' ;
          END IF ;
        END IF ;
      END IF ;
      IF lggykr_cur%ISOPEN THEN
        IF c_eredm_tip = ' ' THEN
          -- Nincs uj elem, az elozohoz kell hasonlitani
          IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx-1).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx-1).pont THEN
            nev_pont_tab(n_pnt_idx) := l_pont_sor ;
            c_eredm_tip := 'l' ;
          END IF ;
        ELSE
          -- Uj elemet kerult a tombbe, ehhez kell hasonlitani
          IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
            nev_pont_tab(n_pnt_idx) := l_pont_sor ;
            c_eredm_tip := 'l' ;
          END IF ;
        END IF ;
      END IF ;

-- utl_file.put_line( f, 'Amelyik tenyleg bekerult, abbol ujat kell elhozni >' || c_eredm_tip || '<') ;

      -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
      -- 'AND *_cur%ISOPEN' kiveve, mert kulonben c_eredm_tip = ' ' lenne
      IF c_eredm_tip = 'b' THEN
        n_max_eredm_idx := n_max_eredm_idx + 1 ;
          n_pnt_idx := n_pnt_idx + 1 ;
-- utl_file.put_line( f, 'FETCH bef_cur') ;
        FETCH bef_cur INTO b_pont_sor ;
-- utl_file.put_line( f, 'FETCH(bef_cur):__nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
        IF bef_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE bef_cur') ;
          CLOSE bef_cur ;
        END IF ;
      ELSE
        IF c_eredm_tip = 'r' THEN
          n_max_eredm_idx := n_max_eredm_idx + 1 ;
            n_pnt_idx := n_pnt_idx + 1 ;
-- utl_file.put_line( f, 'FETCH edzels_cur') ;
          FETCH edzels_cur INTO r_pont_sor ;
          IF edzels_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE edzels_cur') ;
            CLOSE edzels_cur ;
          END IF ;
        ELSE
-- utl_file.put_line( f, 'FETCH lggykr_cur') ;
          IF c_eredm_tip = 'l' THEN
            n_max_eredm_idx := n_max_eredm_idx + 1 ;
              n_pnt_idx := n_pnt_idx + 1 ;
            FETCH lggykr_cur INTO l_pont_sor ;
            IF lggykr_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE lggykr_cur') ;
              CLOSE lggykr_cur ;
            END IF ;
          ELSE
            -- c_eredm_tip = ' '
            -- Nincs tobb eredmeny ezzel a nev kulccsal vagy
            -- elertuk a maximalis ertekelheto eredmenyek szamat -> kovetkezo nev
            -- Valamilyen rendszer szerint vegig kell menni a neveken :
            -- legyen ez a novekvo kulcs
-- utl_file.put_line( f, 'c_eredm_tip = " " ') ;
-- utl_file.put_line( f, 'n_pnt_idx:' || TO_CHAR( n_pnt_idx)) ;
            -- Lepjuk at mindharom helyen a regi nevet !
            -- NYITOTT KURZOR ? NINCS TOBB ADAT ?
-- utl_file.put_line( f, 'atlepes ?') ;
            IF bef_cur%ISOPEN THEN
-- utl_file.put_line( f, 'befuto nevek atlepese:nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
              WHILE nev_pont_tab(n_pnt_idx-1).nev_kulcs = b_pont_sor.nev_kulcs LOOP
                FETCH bef_cur INTO b_pont_sor ;
                EXIT WHEN bef_cur%NOTFOUND ;
              END LOOP ;
              IF bef_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE bef_cur') ;
                CLOSE bef_cur ;
              END IF ;
            END IF ;

            IF edzels_cur%ISOPEN THEN
-- utl_file.put_line( f, 'edzels nevek atlepese:nev_kulcs' || TO_CHAR(r_pont_sor.nev_kulcs) ) ;
              WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = r_pont_sor.nev_kulcs LOOP
                FETCH edzels_cur INTO r_pont_sor ;
                EXIT WHEN edzels_cur%NOTFOUND ;
              END LOOP ;
              IF edzels_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE edzels_cur') ;
                CLOSE edzels_cur ;
              END IF ;
            END IF ;

            IF lggykr_cur%ISOPEN THEN
-- utl_file.put_line( f, 'lggykr nevek atlepese:nev_kulcs' || TO_CHAR(l_pont_sor.nev_kulcs) ) ;
              WHILE nev_pont_tab(n_pnt_idx).nev_kulcs = l_pont_sor.nev_kulcs LOOP
                FETCH lggykr_cur INTO l_pont_sor ;
                EXIT WHEN lggykr_cur%NOTFOUND ;
              END LOOP ;
              IF lggykr_cur%NOTFOUND THEN
                CLOSE lggykr_cur ;
              END IF ;
            END IF ;
            -- Melyik a legnagyobb (nev_kulcs, pont) ?
            c_eredm_tip := ' ' ;
            -- VAN-E MEG ADAT ? b_pont_sor MAR EGYSZER FEL LETT HASZNALVA ?
            IF bef_cur%ISOPEN THEN
-- utl_file.put_line( f, 'nev_pont_tab('||TO_CHAR(n_pnt_idx)||') <- b_pont_sor:') ;

-- utl_file.put_line( f, '__nev_kulcs:'   || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
-- utl_file.put_line( f, '__pont:'        || TO_CHAR(b_pont_sor.pont) ) ;
-- utl_file.put_line( f, '__helyezes:'    || TO_CHAR(b_pont_sor.helyezes) ) ;
-- utl_file.put_line( f, '__nvzl_kulcs:'  || TO_CHAR(b_pont_sor.nvzl_kulcs) ) ;
-- utl_file.put_line( f, '__nagydij_nev:' || TO_CHAR(b_pont_sor.nagydij_nev) ) ;

              nev_pont_tab(n_pnt_idx) := b_pont_sor ;
              c_eredm_tip := 'b' ;
            END IF ;
            -- JAR-E PONT EZERT EGYALTALAN ?
            IF edzels_cur%ISOPEN THEN
              IF r_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
                nev_pont_tab(n_pnt_idx) := r_pont_sor ;
                c_eredm_tip := 'r' ;
              ELSE
                IF r_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND r_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                  nev_pont_tab(n_pnt_idx) := r_pont_sor ;
                  c_eredm_tip := 'r' ;
                END IF ;
              END IF ;
            END IF ;
            -- JAR-E PONT EZERT EGYALTALAN ?
            IF lggykr_cur%ISOPEN THEN
              IF l_pont_sor.nev_kulcs > nev_pont_tab(n_pnt_idx).nev_kulcs THEN
                nev_pont_tab(n_pnt_idx) := l_pont_sor ;
                c_eredm_tip := 'l' ;
              ELSE
                IF l_pont_sor.nev_kulcs = nev_pont_tab(n_pnt_idx).nev_kulcs AND l_pont_sor.pont > nev_pont_tab(n_pnt_idx).pont THEN
                  nev_pont_tab(n_pnt_idx) := l_pont_sor ;
                  c_eredm_tip := 'l' ;
                END IF ;
              END IF ;
            END IF ;
            -- Amelyik tenyleg bekerult, abbol ujat kell elhozni
            IF c_eredm_tip = 'b' THEN
              n_max_eredm_idx := n_max_eredm_idx + 1 ;
                n_pnt_idx := n_pnt_idx + 1 ;
-- utl_file.put_line( f, 'FETCH bef_cur') ;
              FETCH bef_cur INTO b_pont_sor ;
-- utl_file.put_line( f, '__nev_kulcs' || TO_CHAR(b_pont_sor.nev_kulcs) ) ;
              IF bef_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE bef_cur') ;
                CLOSE bef_cur ;
              END IF ;
            ELSE
              IF c_eredm_tip = 'r' THEN
                n_max_eredm_idx := n_max_eredm_idx + 1 ;
                  n_pnt_idx := n_pnt_idx + 1 ;
-- utl_file.put_line( f, 'FETCH edzels_cur') ;
                FETCH edzels_cur INTO r_pont_sor ;
                IF edzels_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE edzels_cur') ;
                  CLOSE edzels_cur ;
                END IF ;
              ELSE
                IF c_eredm_tip = 'l' AND lggykr_cur%ISOPEN THEN
                  n_max_eredm_idx := n_max_eredm_idx + 1 ;
                    n_pnt_idx := n_pnt_idx + 1 ;
-- utl_file.put_line( f, 'FETCH lggykr_cur') ;
                  FETCH lggykr_cur INTO l_pont_sor ;
                  IF lggykr_cur%NOTFOUND THEN
-- utl_file.put_line( f, 'CLOSE lggykr_cur') ;
                    CLOSE lggykr_cur ;
                  END IF ;
                ELSE
-- utl_file.put_line( f, '-- EXIT LOOP --------------------') ;
                  EXIT ;
                END IF ;
              END IF ;
            END IF ; -- IF c_eredm_tip = 'b'

          END IF ;
        END IF ;
      END IF ;
-- utl_file.put_line( f, '-- ciklusmag vege --------------------') ;
    END LOOP ;

    -- A nem nyitott kurzor 'lezarasa' hibat okoz
    IF bef_cur%ISOPEN THEN
      CLOSE edzels_cur ;
    END IF ;
    IF edzels_cur%ISOPEN THEN
      CLOSE edzels_cur ;
    END IF ;
    IF lggykr_cur%ISOPEN THEN
      CLOSE lggykr_cur ;
    END IF ;

-- utl_file.put_line( f, 'n_max_eredm_idx:' || TO_CHAR( n_max_eredm_idx)) ;
-- utl_file.fflush( f) ;
-- utl_file.fclose( f) ;

  szez_eredm_megh( n_pnt_idx - 1 ) ;

--f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;
-- utl_file.put_line( f, 'szez_pill_all_cs END') ;
-- utl_file.fflush( f) ;
-- utl_file.fclose( f) ;

  END szez_pill_all_cs ;

-- szez_eredm_elhoz
-- A kimenotombok csak 1-tol indexelhetok !
  PROCEDURE szez_eredm_elhoz( n_nev_tab IN OUT t_num10_array, n_nev_tab_mer IN OUT NUMBER,
                              n_poz_tmb OUT    t_num2_array,  n_szz_pnt_tmb OUT    t_num3_array) AS
    i NUMBER NOT NULL := 1 ;
    j NUMBER NOT NULL := 0 ;
    n_kiv_nev_idx NUMBER NOT NULL := 1 ; 
    n_csere       NUMBER NOT NULL := 0 ;

--f utl_file.file_type ;
  BEGIN
--f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;

    -- n_nev_tab_mer = 0 eset jelzi, hogy mindent el kell hozni
    IF n_nev_tab_mer = 0 THEN
      i := 1 ;
      j := n_akt_nev_idx ;
--utl_file.put_line( f, 'n_akt_nev_idx:' || TO_CHAR(n_akt_nev_idx)) ;
      WHILE j < n_max_szez_idx AND i <= n_nev_tmb_mer LOOP
--utl_file.put_line( f, 'i:' || TO_CHAR(i)) ;
--utl_file.put_line( f, 'szez_all_tab(j).nev_kulcs:' || TO_CHAR(szez_all_tab(j).nev_kulcs)) ;
        n_nev_tab(i) := szez_all_tab(j).nev_kulcs ;
        n_poz_tmb(i) := j + 1 ;
        n_szz_pnt_tmb(i) := szez_all_tab(j).pont ;
        i := i + 1 ;
        j := j + 1 ;
      END LOOP ;
      n_akt_nev_idx := j ;

    ELSE

      n_kiv_nev_idx := 1 ;

      FOR j IN 0..n_max_szez_idx - 1 LOOP
--utl_file.put_line( f, 'j:' || TO_CHAR(j)) ;
        -- A kimeno tombok indexe 1-tol megy a masodik elhozasnal is !
        i := n_kiv_nev_idx ;
        WHILE i <= n_nev_tab_mer AND i <= n_nev_tmb_mer LOOP
--utl_file.put_line( f, 'i:' || TO_CHAR(i)) ;
          IF n_nev_tab(i) = szez_all_tab(j).nev_kulcs THEN
--utl_file.put_line( f, 'talalat(n_kiv_nev_idx)/csere(i): ' || TO_CHAR(n_kiv_nev_idx) || ' <-> ' || TO_CHAR(i)) ;
            n_csere                  := n_nev_tab(n_kiv_nev_idx) ;
            n_nev_tab(n_kiv_nev_idx) := n_nev_tab(i) ;
            n_nev_tab(i)             := n_csere ;

            n_poz_tmb(n_kiv_nev_idx)     := j + 1 ;
--utl_file.put_line( f, 'helyezes: ' || TO_CHAR(j+1)) ;

            n_szz_pnt_tmb(n_kiv_nev_idx) := szez_all_tab(j).pont ;
--utl_file.put_line( f, 'szez_all_tab(' || TO_CHAR(j) || ').pont:' || TO_CHAR(szez_all_tab(j).pont)) ;
            n_kiv_nev_idx := n_kiv_nev_idx + 1 ;

            -- Biztos nincs tobb, kar tovabb nezni : kov. iteracio
            EXIT ;
          END IF ;
          i := i + 1 ;
        END LOOP ;
      END LOOP ;

      n_nev_tab_mer := n_kiv_nev_idx - 1 ;
    END IF ;

    -- Ha elfogytak az elhozando adatok, az aktualis index visszaallitasa az
    -- elejere gondolva a kovetkezo elohzasra (besz_eredm_elhoz)
    IF i = n_nev_tab_mer THEN
      n_akt_nev_idx := 0 ;
    END IF ;

--utl_file.put_line( f, 'szez_eredm_elhoz : END') ;
--utl_file.fflush( f) ;
--utl_file.fclose( f) ;

  END szez_eredm_elhoz ;

-- szez_eredm_elhoz
  PROCEDURE besz_eredm_elhoz( n_nev_tab IN t_num10_array, n_nev_tab_mer IN NUMBER,
                              n_nev_kulcs_tab OUT t_num10_array, n_pont_tab OUT t_num2_array,
                              n_helyezes_tab OUT t_num2_array, n_nvzl_kulcs_tab OUT t_num10_array,
                              n_nagydij_nev_tab OUT t_num10_array, n_ki_tab_mer OUT NUMBER) AS

    -- A felhivo oldalon szamitom az aktualisan elhozott tombok meretet,
    -- ezert ezt a valtozot nem kell visszaadni
--    n_tombok_merete NUMBER ;
    i NUMBER NOT NULL := 1 ;
    j NUMBER ;
-- f utl_file.file_type ;
  BEGIN
-- f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;

-- utl_file.put_line( f, 'besz_eredm_elhoz : BEGIN') ;
-- utl_file.put_line( f, 'n_max_eredm_idx:' || TO_CHAR(n_max_eredm_idx)) ;
    n_ki_tab_mer := 0 ;
    -- Ha n_nev_tab_mer = 0, minden eredmenyt el kell hozni
    IF n_nev_tab_mer = 0 THEN
      j := n_akt_eredm_idx ;
      WHILE j < n_max_eredm_idx AND n_ki_tab_mer < n_eredm_tmb_mer LOOP
        -- n_ki_tab_mer 0-ra inicializalva, utana novelve, igy a
        -- visszaadott meret mindig helyes, a NESTED TABLE indexe 0-tol megy
-- utl_file.put_line( f, 'n_ki_tab_mer:' || TO_CHAR(n_ki_tab_mer)) ;
-- utl_file.put_line( f, 'j:' || TO_CHAR(j)) ;

-- utl_file.put_line( f, 'nev_kulcs:' || TO_CHAR(nev_pont_tab(j).nev_kulcs)) ;
-- utl_file.put_line( f, 'pont:' || TO_CHAR(nev_pont_tab(j).pont)) ;
-- utl_file.put_line( f, 'helyezes:' || TO_CHAR(nev_pont_tab(j).helyezes)) ;
-- utl_file.put_line( f, 'nvzl_kulcs:' || TO_CHAR(nev_pont_tab(j).nvzl_kulcs)) ;
-- utl_file.put_line( f, 'nagydij_nev:' || TO_CHAR(nev_pont_tab(j).nagydij_nev)) ;
-- utl_file.put_line( f, '---------------------') ;

        n_nev_kulcs_tab(n_ki_tab_mer+1)   := nev_pont_tab(j).nev_kulcs   ;
        n_pont_tab(n_ki_tab_mer+1)        := nev_pont_tab(j).pont        ;
        n_helyezes_tab(n_ki_tab_mer+1)    := nev_pont_tab(j).helyezes    ;
        n_nvzl_kulcs_tab(n_ki_tab_mer+1)  := nev_pont_tab(j).nvzl_kulcs  ;
        n_nagydij_nev_tab(n_ki_tab_mer+1) := nev_pont_tab(j).nagydij_nev ;

        -- Probaljuk meg a 0-s indexet !
        n_ki_tab_mer := n_ki_tab_mer + 1 ;
        j := j + 1 ;
      END LOOP ;
-- utl_file.put_line( f, 'n_ki_tab_mer:' || TO_CHAR(n_ki_tab_mer)) ;
      -- Ugy latszik n_ki_tab_mer nem lehet negativ szam +1 ???\/
--      n_ki_tab_mer    := j - n_akt_eredm_idx ; folosleges, a ciklusban novelve
      n_akt_eredm_idx := j ;
-- utl_file.put_line( f, 'n_akt_eredm_idx:' || TO_CHAR(n_akt_eredm_idx)) ;
    ELSE
-- utl_file.put_line( f, 'n_akt_eredm_idx  :' || TO_CHAR(n_akt_eredm_idx) ) ;
-- utl_file.put_line( f, 'n_akt_nev_idx    :' || TO_CHAR(n_akt_nev_idx)   ) ;
-- utl_file.put_line( f, 'n_nev_tab_mer    :' || TO_CHAR(n_nev_tab_mer)   ) ;
-- utl_file.put_line( f, 'n_ki_tab_mer     :' || TO_CHAR(n_ki_tab_mer)    ) ;
-- utl_file.put_line( f, 'n_eredm_tmb_mer  :' || TO_CHAR(n_eredm_tmb_mer) ) ;

      j := n_akt_eredm_idx ;
      WHILE i <= n_nev_tab_mer AND n_ki_tab_mer < n_eredm_tmb_mer LOOP
-- utl_file.put_line( f, 'i :' || TO_CHAR( i) ) ;
-- utl_file.put_line( f, 'n_ki_tab_mer     :' || TO_CHAR(n_ki_tab_mer)) ;
        -- Mind a bejovo kulcsok, mind a lekerdezes eredmenyei novekvo sorrendbe
        -- vannak rendezve, hogy tobb lepesben valo elhozas eseten egyszeeruen
        -- meg lehessen jegyezni, mi volt mar elhozva
-- utl_file.put_line( f, TO_CHAR(n_nev_tab(i)) || '? (WHILE elott) ' || TO_CHAR(nev_pont_tab(j).nev_kulcs)) ;
        WHILE j < n_max_eredm_idx AND n_ki_tab_mer < n_eredm_tmb_mer AND n_nev_tab(i) <= nev_pont_tab(j).nev_kulcs LOOP
-- utl_file.put_line( f, TO_CHAR(n_nev_tab(i)) || '?' || TO_CHAR(nev_pont_tab(j).nev_kulcs)) ;
          IF n_nev_tab(i) = nev_pont_tab(j).nev_kulcs THEN
            -- n_ki_tab_mer 0-ra inicializalva, elotte novelve, igy a
            -- visszaadott meret mindig helyes, a NESTED TABLE indexe 0-tol megy
            n_nev_kulcs_tab(n_ki_tab_mer+1)   := nev_pont_tab(j).nev_kulcs   ;
            n_pont_tab(n_ki_tab_mer+1)        := nev_pont_tab(j).pont        ;
            n_helyezes_tab(n_ki_tab_mer+1)    := nev_pont_tab(j).helyezes    ;
            n_nvzl_kulcs_tab(n_ki_tab_mer+1)  := nev_pont_tab(j).nvzl_kulcs  ;
            n_nagydij_nev_tab(n_ki_tab_mer+1) := nev_pont_tab(j).nagydij_nev ;

            -- Probaljuk meg a 0-s indexet !
            n_ki_tab_mer := n_ki_tab_mer + 1 ;
          END IF ;
          j := j + 1 ;
        END LOOP ;
        i := i + 1 ;
      END LOOP ;

      -- Az elhozatalt innen kell folytatni
      n_akt_eredm_idx := j ;
    END IF ;

-- utl_file.put_line( f, 'besz_eredm_elhoz : END') ;
-- utl_file.fflush( f) ;
-- utl_file.fclose( f) ;
  END besz_eredm_elhoz ;

  PROCEDURE teszt( n_kezd IN NUMBER, n_veg IN NUMBER) AS
    i NUMBER ;
--    f utl_file.file_type ;
  BEGIN
--    f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;
    dbms_output.put_line( 'teszt : BEGIN') ;

    FOR i IN n_kezd .. n_veg LOOP
      dbms_output.put_line( 'szez_all_tab' || TO_CHAR( i) || '.pont:' || TO_CHAR(szez_all_tab( i).pont)) ;
    END LOOP ;

    dbms_output.put_line( 'teszt : END') ;
--    utl_file.fflush( f) ;
--    utl_file.fclose( f) ;

  END teszt ;

END szez_all ;
/

show errors
