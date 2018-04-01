CREATE OR REPLACE PACKAGE forma1 AS

  PROCEDURE rsz_nd_bol_nvzl( rajtszam IN NUMBER, ev IN NUMBER, nagydij IN NUMBER, nvzl_kulcs OUT NUMBER) ;

  PROCEDURE rajts_insert( n_szezon_eve  IN NUMBER,
                          n_nagydij_nev IN NUMBER,
                          n_nvzl_kulcs  IN NUMBER,
                          n_helyezes    IN NUMBER,
                          n_perc        IN NUMBER,
                          n_masodperc   IN NUMBER,
                          n_ezrdmsdp    IN NUMBER,
                          c_megjegyzes  IN VARCHAR2) ;

  PROCEDURE befuto_insert( n_szezon_eve  IN NUMBER,
                           n_nagydij_nev IN NUMBER,
                           n_nvzl_kulcs  IN NUMBER,
                           n_helyezes    IN NUMBER,
                           n_ora         IN NUMBER,
                           n_perc        IN NUMBER,
                           n_masodperc   IN NUMBER,
                           n_ezrdmsdp    IN NUMBER,
                           c_megjegyzes  IN VARCHAR2) ;

  PROCEDURE rajts_torol( n_szez_eve IN NUMBER, n_nd_kulcs IN NUMBER) ;

  PROCEDURE rajts_update( n_szezon_eve      IN NUMBER,
                          n_nagydij_nev     IN NUMBER,
                          n_regi_hlyz       IN NUMBER,
                          n_regi_nvzl_kulcs IN NUMBER,
                          n_uj_hlyz         IN NUMBER,
                          n_uj_nvzl_kulcs   IN NUMBER,
                          n_perc            IN NUMBER,
                          n_masodperc       IN NUMBER,
                          n_ezrdmsdp        IN NUMBER,
                          c_megjegyzes      IN VARCHAR2) ;

  PROCEDURE befuto_update( n_szezon_eve      IN NUMBER,
                           n_nagydij_nev     IN NUMBER,
                           n_regi_hlyz       IN NUMBER,
                           n_regi_nvzl_kulcs IN NUMBER,
                           n_uj_hlyz         IN NUMBER,
                           n_uj_nvzl_kulcs   IN NUMBER,
                           n_ora             IN NUMBER,
                           n_perc            IN NUMBER,
                           n_masodperc       IN NUMBER,
                           n_ezrdmsdp        IN NUMBER,
                           c_megjegyzes      IN VARCHAR2) ;

  PROCEDURE rajts_delete( n_szezon_eve  IN NUMBER,
                          n_nagydij_nev IN NUMBER,
                          n_nvzl_kulcs  IN NUMBER,
                          n_helyezes    IN NUMBER) ;

END forma1 ;
/

CREATE OR REPLACE PACKAGE BODY forma1 AS

  CURSOR nvzl_keres_krz( rajtsz IN NUMBER, ev IN NUMBER, nagyd_datuma IN DATE) IS
    SELECT nl.kulcs, nd.datum
      FROM nagydijak nd, rajtsorrend rs, nevezesi_lista nl
      WHERE rs.szezon_eve  = ev AND
            nd.datum      <= nagyd_datuma AND
            nd.nagydij_nev = rs.nagydij_nev AND
            nl.rajtszam    = rajtsz AND
            rs.nvzl_kulcs  = nl.kulcs ;

  PROCEDURE rsz_nd_bol_nvzl( rajtszam IN NUMBER, ev IN NUMBER, nagydij IN NUMBER, nvzl_kulcs OUT NUMBER) AS

    nd_datum      DATE ;
    legn_nd_datum DATE ;

  BEGIN

--  set transaction read only : ORA-01453: SET TRANSACTION must be first statement of transaction ->
--  le kene zarni a fv. elejen a tranzakciot, amit eppen el akarok kerulni !
    SAVEPOINT fv_kezd_sp ;

    -- A Forma1 kezdetenek eve, a legkisebb datum, mely elofordulhat
    legn_nd_datum := TO_DATE( '1950-01-01', 'YYYY-MM-DD') ;

    SELECT datum
      INTO nd_datum
      FROM nagydijak
      WHERE szezon_eve  = ev AND
            nagydij_nev = nagydij ;

    FOR krz_rec IN nvzl_keres_krz( rajtszam, ev, nd_datum) LOOP

-- dbms_output.put_line(TO_CHAR( krz_rec.kulcs)) ;

      IF krz_rec.datum > legn_nd_datum THEN
        legn_nd_datum := krz_rec.datum ;
        nvzl_kulcs    := krz_rec.kulcs ;
      END IF ;

    END LOOP;

--  COMMIT TO fv_kezd_sp ; nem lehet savepointtal hasznalni !
    ROLLBACK TO fv_kezd_sp ;

  END rsz_nd_bol_nvzl ;

  PROCEDURE rajts_insert( n_szezon_eve  IN NUMBER,
                          n_nagydij_nev IN NUMBER,
                          n_nvzl_kulcs  IN NUMBER,
                          n_helyezes    IN NUMBER,
                          n_perc        IN NUMBER,
                          n_masodperc   IN NUMBER,
                          n_ezrdmsdp    IN NUMBER,
                          c_megjegyzes  IN VARCHAR2) AS

    n_utk_nvzl_kulcs nevezesi_lista.kulcs%TYPE      ;
    n_rajtszam       nevezesi_lista.rajtszam%TYPE   ;
    n_versenyzo      nevezesi_lista.versenyzo%TYPE  ;

    b_helyezes_utk   BOOLEAN DEFAULT FALSE ;

  BEGIN

    -- Nem parameterkent kapja az elj az uj rajtszam es versenyzo parametereket
    SELECT rajtszam, versenyzo
      INTO n_rajtszam, n_versenyzo
      FROM nevezesi_lista
      WHERE kulcs = n_nvzl_kulcs ;

--dbms_output.put_line( 'rsz : ' || TO_CHAR( n_rajtszam) || 'vers : ' || TO_CHAR( n_versenyzo)) ;

    -- Hogy a default EXCEPTION ne halassza el a vezerlest...
    BEGIN
      -- Szerepel-e a megadott versenyzo vagy rajtszam az adott futamon ?
      -- Ha az adatbazis hibatlan allapotban van, csak max. ket azonos rajtszam
      -- plusz versenyzot talalhat nem 16 megjegyzessel
      SELECT n.kulcs
        INTO n_utk_nvzl_kulcs
        FROM nevezesi_lista n, rajtsorrend r
        WHERE r.szezon_eve  = n_szezon_eve  AND
              r.nagydij_nev = n_nagydij_nev AND
              r.nvzl_kulcs  = n.kulcs       AND
              r.megjegyzes LIKE ('___________0____') AND
              ( n.rajtszam  = n_rajtszam OR
                n.versenyzo = n_versenyzo ) ;

--dbms_output.put_line( 'n_utk_nvzl_kulcs : ' || TO_CHAR( n_utk_nvzl_kulcs)) ;
--dbms_output.put_line( 'az versenyzo szerepel az adott futamon') ;

      -- Ha szerepel mar a versenyzo, nem veszi fel a sort
      RAISE_APPLICATION_ERROR( -20005, 'Szerepel mar a versenyzo !', FALSE) ;
--      RETURN ;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'n_utk_nvzl_kulcs : nem talalt ') ;
          NULL ;
        -- Elofordulhat, hogy az utkozo rajtszamhoz ill. versenyzohoz tartozo
        -- n_utk_nvzl_kulcs ket kulonbozo ertek !(ugysem adom egyiket sem vissza)
        WHEN TOO_MANY_ROWS THEN
--dbms_output.put_line( 'n_utk_nvzl_kulcs : tobb, mint egyet talalt ') ;
          RAISE_APPLICATION_ERROR( -20006, 'Rajtszam es versenyzo utkozes !', FALSE) ;
    END ;

    -- Hogy a default EXCEPTION ne halassza el a vezerlest...
    BEGIN
      -- Az esetlegesen meglevo nvzl celokat szolgalo bejegyzes atirasa
      DELETE FROM rajtsorrend
        WHERE szezon_eve = n_szezon_eve AND
              nvzl_kulcs = n_nvzl_kulcs AND
              megjegyzes LIKE ('___________1____') ;

--dbms_output.put_line( 'A nvzl celokat szolgalo bejegyzes torlese') ;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'Nincs nvzl celokat szolgalo bejegyzes') ;
          NULL ;
    END ;

    -- Hogy a default EXCEPTION ne halassza el a vezerlest...
    BEGIN
      -- A SELECT utan csak a helyezessel lehet utkozes
      -- Az uj sor beszurasa :
      INSERT INTO rajtsorrend
        VALUES( n_szezon_eve, n_nagydij_nev, n_nvzl_kulcs, n_helyezes,
                n_perc, n_masodperc, n_ezrdmsdp, c_megjegyzes) ;

--dbms_output.put_line( 'Az uj rajts bejegyzes beszurasa') ;

      EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN
--dbms_output.put_line( 'Az uj rajts bejegyzest nem sikerult beszurni') ;

          b_helyezes_utk := TRUE ;
    END ;

--dbms_output.put_line( 'SQLCODE = ' || TO_CHAR(SQLCODE)) ;

    -- Ha ezt megeszi (es mukodik) ... %DUP_VAL_ON_INDEX
    IF b_helyezes_utk = TRUE THEN

--dbms_output.put_line( 'a versenyzo szerepel az adott futamon') ;

      -- A beszurando helyezestol mindet egyel hatrebb
      -- Ha vannak kamu bejegyzesek a nevzl szamara, azokat is
      UPDATE rajtsorrend
        SET helyezes = helyezes + 1
        WHERE  szezon_eve =  n_szezon_eve  AND
               nagydij_nev=  n_nagydij_nev AND
               helyezes   >= n_helyezes ;

--dbms_output.put_line( 'Az utban levo bejegyzesek hatratolva') ;

      INSERT INTO rajtsorrend
        VALUES( n_szezon_eve, n_nagydij_nev, n_nvzl_kulcs, n_helyezes,
                n_perc, n_masodperc, n_ezrdmsdp, c_megjegyzes) ;

--dbms_output.put_line( 'Az uj rajtsorrend rekord beszurva') ;

    END IF ;

  END rajts_insert ;

  PROCEDURE befuto_insert( n_szezon_eve  IN NUMBER,
                           n_nagydij_nev IN NUMBER,
                           n_nvzl_kulcs  IN NUMBER,
                           n_helyezes    IN NUMBER,
                           n_ora         IN NUMBER,
                           n_perc        IN NUMBER,
                           n_masodperc   IN NUMBER,
                           n_ezrdmsdp    IN NUMBER,
                           c_megjegyzes  IN VARCHAR2) AS

    n_utk_nvzl_kulcs nevezesi_lista.kulcs%TYPE      ;
    n_rajtszam       nevezesi_lista.rajtszam%TYPE   ;
    n_versenyzo      nevezesi_lista.versenyzo%TYPE  ;

    c_kamu_megj      rajtsorrend.megjegyzes%TYPE ;

    b_helyezes_utk   BOOLEAN DEFAULT FALSE ;

  BEGIN

    -- Ha a beillesztendo befuto bejegyzes nem l., vagy a nvzl szamara,
    -- a default exception handling eljarassal kiszall
    SELECT megjegyzes
      INTO c_kamu_megj
      FROM rajtsorrend
      WHERE szezon_eve  = n_szezon_eve  AND
            nagydij_nev = n_nagydij_nev AND
            nvzl_kulcs  = n_nvzl_kulcs  AND
            megjegyzes LIKE ('___________0____') ;

    -- Nem parameterkent kapja az elj az uj rajtszam es versenyzo parametereket
    SELECT rajtszam, versenyzo
      INTO n_rajtszam, n_versenyzo
      FROM nevezesi_lista
      WHERE kulcs = n_nvzl_kulcs ;

--dbms_output.put_line( 'rsz : ' || TO_CHAR( n_rajtszam) || 'vers : ' || TO_CHAR( n_versenyzo)) ;

    -- Hogy a default EXCEPTION ne halassza el a vezerlest...
    BEGIN
      -- Szerepel-e a megadott versenyzo vagy rajtszam az adott befuton ?
      SELECT n.kulcs
        INTO n_utk_nvzl_kulcs
        FROM nevezesi_lista n, befuto b
        WHERE b.szezon_eve  = n_szezon_eve  AND
              b.nagydij_nev = n_nagydij_nev AND
              b.nvzl_kulcs  = n.kulcs       AND
              ( n.rajtszam  = n_rajtszam OR
                n.versenyzo = n_versenyzo ) ;

      -- Ha szerepel mar a versenyzo vagy a rajtszam, nem veszi fel a sort
--dbms_output.put_line( 'n_utk_nvzl_kulcs : ' || TO_CHAR( n_utk_nvzl_kulcs)) ;
--dbms_output.put_line( 'az versenyzo es/vagy rajtszam szerepel a befuton') ;

      RAISE_APPLICATION_ERROR( -20005, 'Szerepel mar a versenyzo !', FALSE) ;
--      RETURN ;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'n_utk_nvzl_kulcs : nem talalt ') ;
          NULL ;
        -- Elofordulhat, hogy az utkozo rajtszamhoz ill. versenyzohoz tartozo
        -- n_utk_nvzl_kulcs ket kulonbozo ertek !(ugysem adom egyiket sem vissza)
        WHEN TOO_MANY_ROWS THEN
--dbms_output.put_line( 'n_utk_nvzl_kulcs : tobb, mint egyet talalt ') ;
          RAISE_APPLICATION_ERROR( -20006, 'Rajtszam es versenyzo utkozes !', FALSE) ;
    END ;

    -- Hogy a default EXCEPTION ne halassza el a vezerlest...
    BEGIN
      -- A SELECT utan csak a helyezessel lehet utkozes
      -- Az uj sor beszurasa :
      INSERT INTO befuto
        VALUES( n_szezon_eve, n_nagydij_nev, n_nvzl_kulcs, n_helyezes,
                n_ora, n_perc, n_masodperc, n_ezrdmsdp, c_megjegyzes) ;

--dbms_output.put_line( 'Az uj bef bejegyzes beszurva') ;

      EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN

--dbms_output.put_line( 'Az uj bef bejegyzest nem sikerult beszurni') ;

          b_helyezes_utk := TRUE ;
    END ;


--dbms_output.put_line( 'SQLCODE = ' || TO_CHAR(SQLCODE)) ;

    -- Van mar ilyen helyezes, a tobbit hatrebbtolni ...
    IF b_helyezes_utk = TRUE THEN

--dbms_output.put_line( 'a versenyzo szerepel az adott futamon') ;

      -- A beszurando helyezestol mindet egyel hatrebb
      UPDATE befuto
        SET helyezes = helyezes + 1
        WHERE  szezon_eve =  n_szezon_eve  AND
               nagydij_nev=  n_nagydij_nev AND
               helyezes   >= n_helyezes ;

--dbms_output.put_line( 'Az utban levo bejegyzesek hatratolva') ;

      INSERT INTO befuto
        VALUES( n_szezon_eve, n_nagydij_nev, n_nvzl_kulcs, n_helyezes,
                n_ora, n_perc, n_masodperc, n_ezrdmsdp, c_megjegyzes) ;

--dbms_output.put_line( 'Az uj befuto rekord beszurva') ;
    END IF ;

  END befuto_insert ;


  PROCEDURE rajts_torol( n_szez_eve IN NUMBER, n_nd_kulcs IN NUMBER) AS

    -- a nagydijra nem kell feltetel, mert eleg, ha a szezonban elofordul a
    -- kerdeses nevezesi lista kulcs legalabb egyszer
    CURSOR nvzl_klcs_elof_szam( ev IN NUMBER) IS
      SELECT nvzl_kulcs, COUNT(nvzl_kulcs) kulcs_elof_szam
        FROM rajtsorrend
        WHERE szezon_eve  = ev
        GROUP BY nvzl_kulcs ;

    n_nvzl_kulcs nevezesi_lista.kulcs%TYPE ;

    n_alar_bej_szama NUMBER DEFAULT 0 ;

  BEGIN

    -- A befuto / leggyorsabb_kor tablak CONSTRAINT-jei helyett :
    -- van-e alarendelt bejegyzes a torlendo rajtsorrend barmely elemehez ?
    BEGIN

      SELECT COUNT(*)
        INTO n_alar_bej_szama
        FROM rajtsorrend r, befuto b, leggyorsabb_kor l
        WHERE r.megjegyzes LIKE ('___________0____') AND
              ((r.szezon_eve = b.szezon_eve  AND
                r.nagydij_nev= b.nagydij_nev AND
                r.nvzl_kulcs = b.nvzl_kulcs) OR
               (r.szezon_eve = l.szezon_eve  AND
                r.nagydij_nev= l.nagydij_nev AND
                r.nvzl_kulcs = l.nvzl_kulcs)) ;

      -- Vannak alarendelt bejegyzesek, ezert nem futott EXCEPTION-re
      RAISE_APPLICATION_ERROR( -20007, 'Vannak alarendelt bejegyzesek !', FALSE) ;

      EXCEPTION
        -- Lehet torolni a rajtsorrendet
        WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'a rajtsorrend egyetlen elemehez sincs alarendelt') ;
          NULL ;
    END ;

    FOR krz_rec IN nvzl_klcs_elof_szam( n_szez_eve) LOOP

--dbms_output.put_line( 'nvzl_klcs : ' || TO_CHAR(krz_rec.nvzl_kulcs) || '  elof.(db) : ' || TO_CHAR(krz_rec.kulcs_elof_szam)) ;

      IF krz_rec.kulcs_elof_szam > 1 THEN

        DELETE FROM rajtsorrend
          WHERE szezon_eve  = n_szez_eve AND
                nagydij_nev = n_nd_kulcs AND
                nvzl_kulcs  = krz_rec.nvzl_kulcs ;

      ELSE

        -- Ez a nevezesi lista kulcs csak egyszer fordul elo a szezonban,
        -- ezt nem szabad torolni, mert a levegoben logna, igy csak a
        -- nevezesi listaban jelenik majd meg
        UPDATE rajtsorrend
          SET megjegyzes = '___________1____' -- alaptip.h : NEM_KVALIF
          WHERE szezon_eve  = n_szez_eve AND
                nagydij_nev = n_nd_kulcs AND
                nvzl_kulcs  = krz_rec.nvzl_kulcs ;

      END IF ;

    END LOOP ;

  END rajts_torol ;


  PROCEDURE rajts_update( n_szezon_eve      IN NUMBER,
                          n_nagydij_nev     IN NUMBER,
                          n_regi_hlyz       IN NUMBER,
                          n_regi_nvzl_kulcs IN NUMBER,
                          n_uj_hlyz         IN NUMBER,
                          n_uj_nvzl_kulcs   IN NUMBER,
                          n_perc            IN NUMBER,
                          n_masodperc       IN NUMBER,
                          n_ezrdmsdp        IN NUMBER,
                          c_megjegyzes      IN VARCHAR2) AS

    INTCONSTR_VIOLATED EXCEPTION ;
    PRAGMA EXCEPTION_INIT( INTCONSTR_VIOLATED, -2292) ;

    n_rajtszam       nevezesi_lista.rajtszam%TYPE  ;
    n_versenyzo      nevezesi_lista.versenyzo%TYPE ;
    n_utk_nvzl_kulcs nevezesi_lista.kulcs%TYPE     ;

    n_bej_szama      NUMBER DEFAULT 0 ;
    n_alar_bej_szama NUMBER DEFAULT 0 ;

--f utl_file.file_type ;

  BEGIN

--f := utl_file.fopen( 'e:\', 'trace.txt', 'a') ;
--utl_file.put_line( f, 'rajts_update : BEGIN') ;

    IF n_regi_nvzl_kulcs != n_uj_nvzl_kulcs THEN

      -- Az uj nevezesi lista kulcshoz tartozo rajtszam, versenyzo
      -- meghatarozasa
      SELECT rajtszam, versenyzo
        INTO n_rajtszam, n_versenyzo
        FROM nevezesi_lista
        WHERE kulcs = n_uj_nvzl_kulcs ;

      -- Szerepel-e az uj versenyzo es/vagy rajtszam az adott rajtsorrenden ?
      BEGIN

        SELECT n.kulcs
          INTO n_utk_nvzl_kulcs
          FROM nevezesi_lista n, rajtsorrend r
          WHERE r.szezon_eve  = n_szezon_eve  AND
                r.nagydij_nev = n_nagydij_nev AND
                r.nvzl_kulcs  = n.kulcs       AND
                ( n.rajtszam  = n_rajtszam OR
                  n.versenyzo = n_versenyzo ) ;

        -- Van utkozes, ezert nem futott EXCEPTION-re. Sajat magat mindig megtalalja...
        IF n_utk_nvzl_kulcs != n_regi_nvzl_kulcs THEN
--utl_file.put_line( f, 'az uj rajtszam / versenyzo utkozik mas bejegyzessel') ;
          RAISE_APPLICATION_ERROR( -20009, 'Rajtszam es/vagy versenyzo utkozes !', FALSE) ;
        END IF ;

        EXCEPTION
          -- Lehet torolni a rajtsorrendet
          WHEN NO_DATA_FOUND THEN
--utl_file.put_line( f, 'a rajtsorrend egyetlen elemehez sincs alarendelt') ;
            NULL ;
          -- Elofordulhat, hogy az utkozo rajtszamhoz ill. versenyzohoz tartozo
          -- n_utk_nvzl_kulcs ket kulonbozo ertek !(ugysem adom egyiket sem vissza)
          WHEN TOO_MANY_ROWS THEN
--utl_file.put_line( f, 'n_utk_nvzl_kulcs : tobb, mint egyet talalt ') ;
            RAISE_APPLICATION_ERROR( -20006, 'Rajtszam es versenyzo utkozes !', FALSE) ;
      END ;

      -- Vannak-e alarendelt bejegyzesek a befuto/leggyorsabb_kor tablaban
      -- a modositando nevezesi lista kulcshoz
      -- r.megjegyzes LIKE ('___________1____') AND <- hiba belevenni !

      SELECT COUNT(*)
        INTO n_alar_bej_szama
        FROM befuto b
        WHERE b.szezon_eve  = n_szezon_eve  AND
              b.nagydij_nev = n_nagydij_nev AND
              b.nvzl_kulcs  = n_regi_nvzl_kulcs ;

      SELECT COUNT(*)
        INTO n_bej_szama
        FROM leggyorsabb_kor l
        WHERE l.szezon_eve  = n_szezon_eve  AND
              l.nagydij_nev = n_nagydij_nev AND
              l.nvzl_kulcs  = n_regi_nvzl_kulcs ;

      n_alar_bej_szama := n_alar_bej_szama + n_bej_szama ;

--utl_file.put_line( f, TO_CHAR(n_alar_bej_szama) || ' alarendelt elemet talalt') ;

      -- Ha nincsenek alarendelt bejegyzesek, a COUNT(*) miatt nem futott EXCEPTION-re
      -- Az alabbiak lehetsegesek, mert a CONSTRAINT a rajtsorrend <-> befuto valamint
      -- rajtsorrend <-> leggyorsabb_kor tablak kozt meg lett szuntetve
      IF n_alar_bej_szama > 0 THEN
        -- A nvzl valtozast a befutora is ki kell terjeszteni
        BEGIN
          UPDATE befuto
            SET nvzl_kulcs = n_uj_nvzl_kulcs
            WHERE szezon_eve = n_szezon_eve  AND
                  nagydij_nev= n_nagydij_nev AND
                  nvzl_kulcs = n_regi_nvzl_kulcs ;

          n_alar_bej_szama := n_alar_bej_szama - 1 ;

          EXCEPTION
          -- Lehet, hogy csak a leggyorsabb_kor-hoz volt ilyen kulccsal
          WHEN NO_DATA_FOUND THEN
--utl_file.put_line( f, 'a befutoban nem volt ilyen kulcs') ;
            NULL ;
        END ;

        -- Ez valoszinuleg gyorsabb, mint tenyleg vegrehajtani...
        IF n_alar_bej_szama > 0 THEN
          BEGIN
            -- A nvzl valtozast a leggyorsabb korre is ki kell terjeszteni
            UPDATE leggyorsabb_kor
              SET nvzl_kulcs = n_uj_nvzl_kulcs
              WHERE szezon_eve = n_szezon_eve  AND
                    nagydij_nev= n_nagydij_nev AND
                    nvzl_kulcs = n_regi_nvzl_kulcs ;

            EXCEPTION
            -- Lehet, hogy csak a befuto-hoz volt ilyen kulccsal
            WHEN NO_DATA_FOUND THEN
--utl_file.put_line( f, 'a befutoban nem volt ilyen kulcs') ;
              NULL ;
          END ;
        END IF ;

      END IF ;

--utl_file.put_line( f, 'a rajtsorrend egyetlen elemehez sincs alarendelt') ;
    END IF ;

    -- Kell-e rajtszam utkozes miatt tologatni ?
    IF n_regi_hlyz != n_uj_hlyz THEN
      -- nem lehet a regit minden tovabbi nelkul torolni, ha van alarendelt bejegyzes
      -- bar, ha megszuntetem a CONSTRAINT - et ugyis, mert pl. ez a tarolt eljaras
      -- fenntartja a konzisztens allapotot ...

      IF n_regi_hlyz < n_uj_hlyz THEN

        UPDATE rajtsorrend
          SET helyezes = DECODE( nvzl_kulcs, n_regi_nvzl_kulcs, n_uj_hlyz, helyezes - 1)
          WHERE szezon_eve =  n_szezon_eve  AND
                nagydij_nev=  n_nagydij_nev AND
                helyezes   >  n_regi_hlyz   AND
                helyezes   <= n_uj_hlyz ;

      ELSE

        UPDATE rajtsorrend
          SET helyezes = DECODE( nvzl_kulcs, n_regi_nvzl_kulcs, n_uj_hlyz, helyezes + 1)
          WHERE szezon_eve =  n_szezon_eve  AND
                nagydij_nev=  n_nagydij_nev AND
                helyezes   >= n_uj_hlyz     AND
                helyezes   <  n_regi_hlyz ;

      END IF ;

    END IF ;

    UPDATE rajtsorrend
      SET perc       = n_perc,
          masodperc  = n_masodperc,
          ezrdmsdp   = n_ezrdmsdp,
          megjegyzes = c_megjegyzes,
          nvzl_kulcs = n_uj_nvzl_kulcs
      WHERE szezon_eve = n_szezon_eve  AND
            nagydij_nev= n_nagydij_nev AND
            nvzl_kulcs = n_regi_nvzl_kulcs ;

    -- Vegul ki kell torolni a n_regi_nvzl_kulcs-csal adott
    -- rekordot a nevezesi_lista-bol, ha lehet
    -- (hogy ne maradjon a feluletrol elerhetetlen bejegyzes
    IF n_regi_nvzl_kulcs != n_uj_nvzl_kulcs THEN
      BEGIN

        DELETE FROM nevezesi_lista
          WHERE kulcs = n_regi_nvzl_kulcs ;

        EXCEPTION
        -- Lehet, hogy masutt mar hasznalva van, ha nem torolni kell
        WHEN INTCONSTR_VIOLATED THEN
--utl_file.put_line( f, 'a nevezesi_lista-bol nem sikerult torolni a regit') ;
          NULL ;
      END ;
    END IF ;

--utl_file.put_line( f, 'rajts_update : END') ;

--utl_file.fflush( f) ;
--utl_file.fclose( f) ;
  END rajts_update ;


  PROCEDURE befuto_update( n_szezon_eve      IN NUMBER,
                           n_nagydij_nev     IN NUMBER,
                           n_regi_hlyz       IN NUMBER,
                           n_regi_nvzl_kulcs IN NUMBER,
                           n_uj_hlyz         IN NUMBER,
                           n_uj_nvzl_kulcs   IN NUMBER,
                           n_ora             IN NUMBER,
                           n_perc            IN NUMBER,
                           n_masodperc       IN NUMBER,
                           n_ezrdmsdp        IN NUMBER,
                           c_megjegyzes      IN VARCHAR2) AS

    INTCONSTR_VIOLATED EXCEPTION ;
    PRAGMA EXCEPTION_INIT( INTCONSTR_VIOLATED, -2292) ;

    n_rajtszam       nevezesi_lista.rajtszam%TYPE  ;
    n_versenyzo      nevezesi_lista.versenyzo%TYPE ;
    n_kamu_nvz_klcs  nevezesi_lista.kulcs%TYPE     ;
    n_utk_nvzl_kulcs nevezesi_lista.kulcs%TYPE     ;

  BEGIN 

    IF n_regi_nvzl_kulcs != n_uj_nvzl_kulcs THEN
      -- A regi nevezesi lista kulcs atnevezese okoz-e utkozest a rajtsorrendben ?
      -- Az uj nevezesi lista kulcshoz tartozo rajtszam, versenyzo
      -- meghatarozasa
      SELECT rajtszam, versenyzo
        INTO n_rajtszam, n_versenyzo
        FROM nevezesi_lista
        WHERE kulcs = n_uj_nvzl_kulcs ;

      -- Szerepel-e az uj versenyzo es/vagy rajtszam az adott befutohoz tartozo rajtsorrenden ?
      -- Akar csak a nevezesi lista szamara
      BEGIN

        SELECT n.kulcs
          INTO n_utk_nvzl_kulcs
          FROM nevezesi_lista n, rajtsorrend r
          WHERE r.szezon_eve  = n_szezon_eve  AND
                r.nagydij_nev = n_nagydij_nev AND
                r.nvzl_kulcs  = n.kulcs       AND
                ( n.rajtszam  = n_rajtszam OR
                  n.versenyzo = n_versenyzo ) ;

        -- Van utkozes, ezert nem futott EXCEPTION-re. Sajat magat mindig megtalalja...
        IF n_utk_nvzl_kulcs != n_regi_nvzl_kulcs THEN
--dbms_output.put_line( 'az uj rajtszam / versenyzo utkozik mas bejegyzessel') ;
          RAISE_APPLICATION_ERROR( -20009, 'Rajtszam es/vagy versenyzo utkozes !', FALSE) ;
        END IF ;

        EXCEPTION
          -- Lehet modositani a rajtsorrend nvzl klcs-at
          WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'ez a nev/rsz. nem utkozik a rajtsorrend egyetlen elemevel sem') ;
            NULL ;
          -- Elofordulhat, hogy az utkozo rajtszamhoz ill. versenyzohoz tartozo
          -- n_utk_nvzl_kulcs ket kulonbozo ertek !(ugysem adom egyiket sem vissza)
          WHEN TOO_MANY_ROWS THEN
--dbms_output.put_line( 'n_utk_nvzl_kulcs : tobb, mint egyet talalt ') ;
            RAISE_APPLICATION_ERROR( -20006, 'Rajtszam es versenyzo utkozes a rs-ben !', FALSE) ;
      END ;
    END IF ;

    -- A nvzl valtozast a rajtsorrendre is ki kell terjeszteni
    BEGIN
      UPDATE rajtsorrend
        SET nvzl_kulcs = n_uj_nvzl_kulcs
        WHERE szezon_eve = n_szezon_eve  AND
              nagydij_nev= n_nagydij_nev AND
              nvzl_kulcs = n_regi_nvzl_kulcs ;

      EXCEPTION
      -- Inkonzisztencia (rs-,b+): ne szalljon ki emiatt a fv-bol
      WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'a rajtsorrendben nem volt ilyen kulcs : inkonzisztencia') ;
        NULL ;
    END ;

    -- A nvzl valtozast a leggyorsabb korre is ki kell terjeszteni
    BEGIN
      UPDATE leggyorsabb_kor
        SET nvzl_kulcs = n_uj_nvzl_kulcs
        WHERE szezon_eve = n_szezon_eve  AND
              nagydij_nev= n_nagydij_nev AND
              nvzl_kulcs = n_regi_nvzl_kulcs ;

      EXCEPTION
      -- Ez itt nem hiba : ne szalljon ki emiatt a fv-bol
      WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'a befutoban nem volt ilyen kulcs') ;
        NULL ;
    END ;

    -- Kell-e rajtszam utkozes miatt tologatni ?
    IF n_regi_hlyz != n_uj_hlyz THEN
      -- nem lehet a regit minden tovabbi nelkul torolni, ha van alarendelt bejegyzes
      -- bar, ha megszuntetem a CONSTRAINT - et ugyis, mert pl. ez a tarolt eljaras
      -- fenntartja a konzisztens allapotot ...

      IF n_regi_hlyz < n_uj_hlyz THEN

        UPDATE befuto
          SET helyezes = DECODE( nvzl_kulcs, n_regi_nvzl_kulcs, n_uj_hlyz, helyezes - 1)
          WHERE szezon_eve =  n_szezon_eve  AND
                nagydij_nev=  n_nagydij_nev AND
                helyezes   >  n_regi_hlyz   AND
                helyezes   <= n_uj_hlyz ;

      ELSE

        UPDATE befuto
          SET helyezes = DECODE( nvzl_kulcs, n_regi_nvzl_kulcs, n_uj_hlyz, helyezes + 1)
          WHERE szezon_eve =  n_szezon_eve  AND
                nagydij_nev=  n_nagydij_nev AND
                helyezes   >= n_uj_hlyz     AND
                helyezes   <  n_regi_hlyz ;

      END IF ;

    END IF ;

    UPDATE befuto
      SET ora        = n_ora,
          perc       = n_perc,
          masodperc  = n_masodperc,
          ezrdmsdp   = n_ezrdmsdp,
          megjegyzes = c_megjegyzes,
          nvzl_kulcs = n_uj_nvzl_kulcs
      WHERE szezon_eve = n_szezon_eve  AND
            nagydij_nev= n_nagydij_nev AND
            nvzl_kulcs = n_regi_nvzl_kulcs ;

    -- Vegul ki kell torolni a n_regi_nvzl_kulcs-csal adott
    -- rekordot a nevezesi_lista-bol, ha lehet
    -- (hogy ne maradjon a feluletrol elerhetetlen bejegyzes
    IF n_regi_nvzl_kulcs != n_uj_nvzl_kulcs THEN
      BEGIN

        DELETE FROM nevezesi_lista
          WHERE kulcs = n_regi_nvzl_kulcs ;

        EXCEPTION
        -- Lehet, hogy masutt mar hasznalva van, ha nem torolni kell
        WHEN INTCONSTR_VIOLATED THEN
--dbms_output.put_line( 'a nevezesi_lista-bol nem sikerult torolni a regit') ;
          NULL ;
      END ;
    END IF ;

  END befuto_update ;

  PROCEDURE rajts_delete( n_szezon_eve  IN NUMBER,
                          n_nagydij_nev IN NUMBER,
                          n_nvzl_kulcs  IN NUMBER,
                          n_helyezes    IN NUMBER) AS

    n_nvzl_elof_szam NUMBER ;

    n_kamu_hlyz   rajtsorrend.helyezes%TYPE     ;
    n_legmag_hlyz rajtsorrend.helyezes%TYPE     ;

  BEGIN
--dbms_output.put_line( 'rajts_delete BEGIN') ;
    -- Van-e alarendelt bejegyzes a befuto ill. leggyorsabb_kor tablaban ?
    BEGIN
      SELECT r.helyezes
        INTO n_kamu_hlyz
        FROM rajtsorrend r, befuto b, leggyorsabb_kor l
        WHERE r.megjegyzes LIKE ('___________1____') AND
              r.szezon_eve  = n_szezon_eve  AND
              r.nagydij_nev = n_nagydij_nev AND
              r.nvzl_kulcs  = n_nvzl_kulcs  AND
              ((r.szezon_eve = b.szezon_eve  AND
                r.nagydij_nev= b.nagydij_nev AND
                r.nvzl_kulcs = b.nvzl_kulcs) OR
               (r.szezon_eve = l.szezon_eve  AND
                r.nagydij_nev= l.nagydij_nev AND
                r.nvzl_kulcs = l.nvzl_kulcs)) ;

      -- Van alarendelt bejegyzes, ezert nem futott EXCEPTION-re
--dbms_output.put_line( 'a torlendo rajtsorrend bejegyzeshez tartoznak alarendeltek') ;
      RAISE_APPLICATION_ERROR( -20010, 'Van alarendelt bejegyzes !', FALSE) ;
--    RETURN ;

    EXCEPTION
      -- Lehet torolni a rajtsorrend bejegyzest (vagy a megjegyzest modositani)
      WHEN NO_DATA_FOUND THEN
--dbms_output.put_line( 'a torlendo rajtsorrend bejegyzeshez nincs alarendelt') ;
        NULL ;
      -- Elofordulhat, hogy az utkozo rajtszamhoz ill. versenyzohoz tartozo
      -- n_helyezes ket kulonbozo ertek !(ugysem adom egyiket sem vissza)
      WHEN TOO_MANY_ROWS THEN
--dbms_output.put_line( 'n_kamu_nvz_klcs : tobb, mint egyet talalt ') ;
        RAISE_APPLICATION_ERROR( -20006, 'Rajtszam es versenyzo utkozes !', FALSE) ;
    END ;

    -- Itt nem szamitok NO_DATA_FOUND - ra !
    SELECT COUNT(*)
      INTO n_nvzl_elof_szam
      FROM rajtsorrend
      WHERE szezon_eve = n_szezon_eve  AND
            nvzl_kulcs = n_nvzl_kulcs ;

--dbms_output.put_line( 'n_nvzl_elof_szam : ' || TO_CHAR(n_nvzl_elof_szam)) ;
    -- Ha a nevezesi lista tobb, mint egyszer elofordul,
    -- torlese nem eredmenyezi a nevzl-bol valo eltuneset
    IF n_nvzl_elof_szam > 1 THEN

--dbms_output.put_line( 'DELETE') ;
      -- Lehet torolni, marad meg bejegyzes a nevezesi lista szamara
      DELETE FROM rajtsorrend
        WHERE szezon_eve  = n_szezon_eve  AND
              nagydij_nev = n_nagydij_nev AND
              nvzl_kulcs  = n_nvzl_kulcs ;

--dbms_output.put_line( 'UPDATE') ;
      -- Mivel a r/b listaban nem lehetnek lyukak, elobbre kell csusztatni a torolt utan levoket */
      UPDATE rajtsorrend
        SET helyezes = helyezes -1
        WHERE szezon_eve  = n_szezon_eve  AND
              nagydij_nev = n_nagydij_nev AND
              helyezes    > n_helyezes ;

    ELSE

--dbms_output.put_line( 'DELETE FROM rajtsorrend') ;
      DELETE FROM rajtsorrend
        WHERE szezon_eve  = n_szezon_eve  AND
              nagydij_nev = n_nagydij_nev AND
              nvzl_kulcs  = n_nvzl_kulcs ;

--dbms_output.put_line( 'SELECT MAX') ;
      -- Mivel a rajtsorrend tablaban a szezon egy nagydijan egy
      -- helyezes csak egyszer fordulhat elo (uk_rs_sze_ndn_hlyz)
      -- meg kell keresni a legmagasabb ureset
      SELECT MAX( helyezes)
        INTO n_legmag_hlyz
        FROM rajtsorrend
        WHERE szezon_eve  = n_szezon_eve AND
              nagydij_nev = n_nagydij_nev ;

--dbms_output.put_line( 'n_legmag_hlyz:' || TO_CHAR(n_legmag_hlyz)) ;

--dbms_output.put_line( 'INSERT INTO rajtsorrend') ;
--dbms_output.put_line( 'n_szezon_eve:' || TO_CHAR(n_szezon_eve) || 'n_nagydij_nev:' || TO_CHAR(n_nagydij_nev)) ;
--dbms_output.put_line( 'n_nvzl_kulcs:' || TO_CHAR(n_nvzl_kulcs) || 'n_legmag_hlyz:' || TO_CHAR(n_legmag_hlyz)) ;
      -- NEM_KVALIF
      INSERT INTO rajtsorrend
        VALUES ( n_szezon_eve, n_nagydij_nev, n_nvzl_kulcs,
                 n_legmag_hlyz + 1, 0, 0, 0, '0000000000010000') ;

    END IF ;

--dbms_output.put_line( 'rajts_delete END') ;
  END rajts_delete ;

END forma1 ;
/

show errors ;

-- Hivasa PRO/C-bol :
--EXEC SQL EXECUTE
--begin
--forma1.rsz_nd_bol_nvzl( :rajtszam, :ev, :nagydij, :nvzl_kulcs) ;
--end;
--;

-- Hivasa SQL Plus-bol :
-- set serveroutput on
-- variable num number
-- execute forma1.rajts_insert( 1990, 1, 21, 1, 1, 23, 456, 0)

--    OPEN nvzl_keres_krz( rajtszam, ev, nd_datum) ;

--    <<nvzl_keres>>
--    LOOP

--      FETCH nvzl_keres_krz INTO ... ;

--      /* %FOUND, %ISOPEN, %NOTFOUND, %ROWCOUNT */
--      EXIT WHEN nvzl_keres_krz%NOTFOUND ;

--    END LOOP nvzl_keres ;

--    CLOSE nvzl_keres_krz ;

-- SET TRANSACTION READ ONLY; -> COMMIT WORK ;