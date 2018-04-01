
/* Result Sets Interface */
#ifndef SQL_CRSR
#  define SQL_CRSR
  struct sql_cursor
  {
    unsigned int curocn;
    void *ptr1;
    void *ptr2;
    unsigned long magic;
  };
  typedef struct sql_cursor sql_cursor;
  typedef struct sql_cursor SQL_CURSOR;
#endif /* SQL_CRSR */

/* Thread Safety */
typedef void * sql_context;
typedef void * SQL_CONTEXT;

/* Object support */
struct sqltvn
{
  unsigned char *tvnvsn; 
  unsigned short tvnvsnl; 
  unsigned char *tvnnm;
  unsigned short tvnnml; 
  unsigned char *tvnsnm;
  unsigned short tvnsnml;
};
typedef struct sqltvn sqltvn;

struct sqladts
{
  unsigned int adtvsn; 
  unsigned short adtmode; 
  unsigned short adtnum;  
  sqltvn adttvn[1];       
};
typedef struct sqladts sqladts;

static struct sqladts sqladt = {
  1,1,0,
};

/* Binding to PL/SQL Records */
struct sqltdss
{
  unsigned int tdsvsn; 
  unsigned short tdsnum; 
  unsigned char *tdsval[1]; 
};
typedef struct sqltdss sqltdss;
static struct sqltdss sqltds =
{
  1,
  0,
};

/* File name & Package Name */
struct sqlcxp
{
  unsigned short fillen;
           char  filnam[14];
};
static const struct sqlcxp sqlfpn =
{
    13,
    ".\\DBSZEZON.PC"
};


static unsigned long sqlctx = 522619;


static struct sqlexd {
   unsigned int   sqlvsn;
   unsigned int   arrsiz;
   unsigned int   iters;
   unsigned int   offset;
   unsigned short selerr;
   unsigned short sqlety;
   unsigned int   unused;
      const short *cud;
   unsigned char  *sqlest;
      const char  *stmt;
   sqladts *sqladtp;
   sqltdss *sqltdsp;
            void  **sqphsv;
   unsigned int   *sqphsl;
            int   *sqphss;
            void  **sqpind;
            int   *sqpins;
   unsigned int   *sqparm;
   unsigned int   **sqparc;
   unsigned short  *sqpadto;
   unsigned short  *sqptdso;
            void  *sqhstv[5];
   unsigned int   sqhstl[5];
            int   sqhsts[5];
            void  *sqindv[5];
            int   sqinds[5];
   unsigned int   sqharm[5];
   unsigned int   *sqharc[5];
   unsigned short  sqadto[5];
   unsigned short  sqtdso[5];
} sqlstm = {10,5};

// Prototypes
extern "C" {
  void sqlcxt (void **, unsigned long *,
               struct sqlexd *, const struct sqlcxp *);
  void sqlcx2t(void **, unsigned long *,
               struct sqlexd *, const struct sqlcxp *);
  void sqlbuft(void **, char *);
  void sqlgs2t(void **, char *);
  void sqlorat(void **, unsigned long *, void *);
}

// Forms Interface
static const int IAPSUCC = 0;
static const int IAPFAIL = 1403;
static const int IAPFTL  = 535;
extern "C" { void sqliem(char *, int *); }

 static const char *sq0002 = 
"select nagydij_nev ,palya_nev ,TO_CHAR(datum,'YYYY-MM-DD')  from nagydijak w\
here szezon_eve=:b0 order by datum            ";

typedef struct { unsigned short len; unsigned char arr[1]; } VARCHAR;
typedef struct { unsigned short len; unsigned char arr[1]; } varchar;

/* cud (compilation unit data) array */
static const short sqlcud0[] =
{10,4130,0,0,0,
5,0,0,1,116,0,4,58,0,0,5,1,0,1,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,1,68,0,0,
40,0,0,2,122,0,9,78,0,0,1,1,0,1,0,1,68,0,0,
59,0,0,2,0,0,13,86,0,0,3,0,0,1,0,2,68,0,0,2,68,0,0,2,5,0,0,
86,0,0,2,0,0,15,118,0,0,0,0,0,1,0,
101,0,0,3,0,0,29,120,0,0,0,0,0,1,0,
116,0,0,2,0,0,15,176,0,0,0,0,0,1,0,
131,0,0,4,0,0,29,178,0,0,0,0,0,1,0,
146,0,0,2,0,0,15,202,0,0,0,0,0,1,0,
161,0,0,5,0,0,29,208,0,0,0,0,0,1,0,
176,0,0,2,0,0,15,220,0,0,0,0,0,1,0,
191,0,0,6,47,0,3,249,0,0,5,5,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,
0,
226,0,0,7,107,0,5,297,0,0,5,5,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,
0,
261,0,0,8,44,0,2,335,0,0,1,1,0,1,0,1,68,0,0,
280,0,0,9,41,0,2,337,0,0,1,1,0,1,0,1,68,0,0,
299,0,0,2,0,0,15,353,0,0,0,0,0,1,0,
314,0,0,10,108,0,3,384,0,0,4,4,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,5,0,0,
345,0,0,11,66,0,2,422,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
368,0,0,12,126,0,5,472,0,0,5,5,0,1,0,1,68,0,0,1,68,0,0,1,5,0,0,1,68,0,0,1,68,0,
0,
};


// dbszezon.pc

#include "stdafx.h"

#include <stdio.h>
#include <string.h>

#include "resource.h"
#include "ensqlcam.h"

#include "alaptip.h"
#include "cnev.h"

#include "pontozas.h"
#include "cszezon.h"

#define SQLCA_STORAGE_CLASS extern
#undef  SQLCA_INIT

/* EXEC SQL INCLUDE sqlca ;
 */ 
/* Copyright (c) 1985,1986 by Oracle Corporation. */
 
/*
NAME
  SQLCA : SQL Communications Area.
FUNCTION
  Contains no code. Oracle fills in the SQLCA with status info
  during the execution of a SQL stmt.
NOTES
  If the symbol SQLCA_STORAGE_CLASS is defined, then the SQLCA
  will be defined to have this storage class. For example:
 
    #define SQLCA_STORAGE_CLASS extern
 
  will define the SQLCA as an extern.
 
  If the symbol SQLCA_INIT is defined, then the SQLCA will be
  statically initialized. Although this is not necessary in order
  to use the SQLCA, it is a good pgming practice not to have
  unitialized variables. However, some C compilers/OS's don't
  allow automatic variables to be init'd in this manner. Therefore,
  if you are INCLUDE'ing the SQLCA in a place where it would be
  an automatic AND your C compiler/OS doesn't allow this style
  of initialization, then SQLCA_INIT should be left undefined --
  all others can define SQLCA_INIT if they wish.

  If the symbol SQLCA_NONE is defined, then the SQLCA variable will
  not be defined at all.  The symbol SQLCA_NONE should not be defined
  in source modules that have embedded SQL.  However, source modules
  that have no embedded SQL, but need to manipulate a sqlca struct
  passed in as a parameter, can set the SQLCA_NONE symbol to avoid
  creation of an extraneous sqlca variable.
*/
 
#ifndef SQLCA
#define SQLCA 1
 
struct   sqlca
         {
         /* ub1 */ char    sqlcaid[8];
         /* b4  */ long    sqlabc;
         /* b4  */ long    sqlcode;
         struct
           {
           /* ub2 */ unsigned short sqlerrml;
           /* ub1 */ char           sqlerrmc[70];
           } sqlerrm;
         /* ub1 */ char    sqlerrp[8];
         /* b4  */ long    sqlerrd[6];
         /* ub1 */ char    sqlwarn[8];
         /* ub1 */ char    sqlext[8];
         };

#ifndef SQLCA_NONE 
#ifdef   SQLCA_STORAGE_CLASS
SQLCA_STORAGE_CLASS struct sqlca sqlca
#else
         struct sqlca sqlca
#endif
 
#ifdef  SQLCA_INIT
         = {
         {'S', 'Q', 'L', 'C', 'A', ' ', ' ', ' '},
         sizeof(struct sqlca),
         0,
         { 0, {0}},
         {'N', 'O', 'T', ' ', 'S', 'E', 'T', ' '},
         {0, 0, 0, 0, 0, 0},
         {0, 0, 0, 0, 0, 0, 0, 0},
         {0, 0, 0, 0, 0, 0, 0, 0}
         }
#endif
         ;
#endif
 
#endif
 
/* end SQLCA */


long CSzezon::SQL_SzezonBetolt( unsigned uiSzezon)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve ;
    unsigned n_pontozas   ;
    unsigned n_edz_els_pont   ;
    unsigned n_leggy_kor_pont ;
    unsigned n_elso_legj_besz ;

    struct sql_s_nagydij
    {
      unsigned n_nagydij_nev ;
      unsigned n_palya_nev   ;
      char     c_datum[13]   ;
    } nagydij_rekord[20] ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  int nFetch, i, j ;
  int nEv, nHonap, nNap ;
  int nCiklFelsHat ;
  SNagydij*  pNagydij   ;
  SNagydij** pNagydijak ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  // Ha nincs a kert futam, kiszall. Kesobb 'do break' van.
  /* EXEC SQL WHENEVER NOTFOUND GOTO sql_hiba ; */ 


  // Az ev megadasa a kurzor es a kovetkezo SELECT szamara :
  n_szezon_eve = uiSzezon ;

  // Szelektalas a szezon tablabol, a tobbi a nagydijak tablabol jon
  /* EXEC SQL SELECT pontozas, edz_els_pont, leggy_kor_pont, elso_legj_besz
               INTO :n_pontozas, :n_edz_els_pont, :n_leggy_kor_pont, :n_elso_legj_besz
             FROM szezon
             WHERE szezon_eve = :n_szezon_eve ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select pontozas ,edz_els_pont ,leggy_kor_pont ,elso_legj_be\
sz into :b0,:b1,:b2,:b3  from szezon where szezon_eve=:b4";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )5;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_pontozas;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_edz_els_pont;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_leggy_kor_pont;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&n_elso_legj_besz;
  sqlstm.sqhstl[3] = (unsigned int  )4;
  sqlstm.sqhsts[3] = (         int  )0;
  sqlstm.sqindv[3] = (         void  *)0;
  sqlstm.sqinds[3] = (         int  )0;
  sqlstm.sqharm[3] = (unsigned int  )0;
  sqlstm.sqadto[3] = (unsigned short )0;
  sqlstm.sqtdso[3] = (unsigned short )0;
  sqlstm.sqhstv[4] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[4] = (unsigned int  )4;
  sqlstm.sqhsts[4] = (         int  )0;
  sqlstm.sqindv[4] = (         void  *)0;
  sqlstm.sqinds[4] = (         int  )0;
  sqlstm.sqharm[4] = (unsigned int  )0;
  sqlstm.sqadto[4] = (unsigned short )0;
  sqlstm.sqtdso[4] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode == 1403) goto sql_hiba;
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  // Ha ide eljutott, van ilyen szezon :
  m_uiSzezon = uiSzezon  ;

  // A kiszelektalt ertekek tagvaltozokba irasa
  m_uiPontozas     = n_pontozas       ;
  m_ucEdzElsPont   = n_edz_els_pont   ;
  m_ucLeggyKorPont = n_leggy_kor_pont ;
  m_ucElsLgjBesz   = n_elso_legj_besz ;

  // A nagydijak kiszelektalasa
  /* EXEC SQL DECLARE select_nd_kurz CURSOR FOR
    SELECT nagydij_nev, palya_nev, TO_CHAR( datum, 'YYYY-MM-DD')
      FROM nagydijak
      WHERE szezon_eve = :n_szezon_eve ORDER BY datum ; */ 


  /* EXEC SQL OPEN select_nd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = sq0002;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )40;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  nFetch = 0 ;

  /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


  while ( TRUE )
  {
    /* EXEC SQL FETCH select_nd_kurz INTO :nagydij_rekord ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 5;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )20;
    sqlstm.offset = (unsigned int  )59;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&nagydij_rekord->n_nagydij_nev;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_nagydij);
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqharc[0] = (unsigned int   *)0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&nagydij_rekord->n_palya_nev;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_nagydij);
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqharc[1] = (unsigned int   *)0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)nagydij_rekord->c_datum;
    sqlstm.sqhstl[2] = (unsigned int  )13;
    sqlstm.sqhsts[2] = (         int  )sizeof(struct sql_s_nagydij);
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqharc[2] = (unsigned int   *)0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqphsv = sqlstm.sqhstv;
    sqlstm.sqphsl = sqlstm.sqhstl;
    sqlstm.sqphss = sqlstm.sqhsts;
    sqlstm.sqpind = sqlstm.sqindv;
    sqlstm.sqpins = sqlstm.sqinds;
    sqlstm.sqparm = sqlstm.sqharm;
    sqlstm.sqparc = sqlstm.sqharc;
    sqlstm.sqpadto = sqlstm.sqadto;
    sqlstm.sqptdso = sqlstm.sqtdso;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode == 1403) break;
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;
    pNagydijak = new SNagydij*[m_ucNagydSzam+nCiklFelsHat] ;

    if ( pNagydijak == NULL )
      return -1 ;

    // Atmasoljuk a regit az ideiglenes helyere
    memcpy( pNagydijak, m_pNagydijak, m_ucNagydSzam*sizeof(SNagydij*)) ;

    // Toroljuk a regi tombot
    delete [] m_pNagydijak ;

    // Ha nem hozott el mindent memoriafoglalasi hiba miatt, legyen tampont
    memset( pNagydijak+m_ucNagydSzam, 0, nCiklFelsHat*sizeof(SNagydij*)) ;

    for ( i = 0 ; i < nCiklFelsHat ; i++ )
    {
      if ( (pNagydij = new SNagydij()) == NULL )
      {
        // Hiba eseten az osztalyt kiuritve adja vissza
        for ( j = 0 ; j < i ; j++ )
          delete pNagydijak[j] ;

        delete [] pNagydijak ;

        m_pNagydijak  = NULL ;
        m_ucNagydSzam = 0 ;

        SQL_SQLCAFeltolt() ;

        /* EXEC SQL CLOSE select_nd_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 5;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )86;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}



        /* EXEC SQL COMMIT WORK ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 5;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )101;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}



        return -1 ;
      }

      // A nagydij feltoltese
      pNagydij->m_uiNagyd = nagydij_rekord[i].n_nagydij_nev ;
      pNagydij->m_uiPalya = nagydij_rekord[i].n_palya_nev   ;

      sscanf( nagydij_rekord[i].c_datum, "%d-%d-%d", &nEv, &nHonap, &nNap) ;

      pNagydij->m_sDatum.ev    = nEv    ;
      pNagydij->m_sDatum.honap = nHonap ;
      pNagydij->m_sDatum.nap   = nNap   ;

      pNagydijak[m_ucNagydSzam+i] = pNagydij ;
    }

    nFetch = sqlca.sqlerrd[2] ;
    m_pNagydijak = pNagydijak ;
    m_ucNagydSzam += nCiklFelsHat ;
  }

  nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;

  if ( nCiklFelsHat > 0 )
  {
    pNagydijak = new SNagydij*[m_ucNagydSzam+nCiklFelsHat] ;

    if ( pNagydijak == NULL )
      return -1 ;

    // Atmasoljuk a regit az ideiglenes helyere
    memcpy( pNagydijak, m_pNagydijak, m_ucNagydSzam*sizeof(SNagydij*)) ;

    // Toroljuk a regi tombot
    delete [] m_pNagydijak ;

    // Ha nem hozott el mindent memoriafoglalasi hiba miatt, legyen tampont
    memset( pNagydijak+m_ucNagydSzam, 0, nCiklFelsHat*sizeof(SNagydij*)) ;

    for ( i = 0 ; i < nCiklFelsHat ; i++ )
    {
      if ( (pNagydij = new SNagydij()) == NULL )
      {
        // Hiba eseten az osztalyt kiuritve adja vissza
        for ( j = 0 ; j < i ; j++ )
          delete pNagydijak[j] ;

        delete [] pNagydijak ;

        m_pNagydijak  = NULL ;
        m_ucNagydSzam = 0 ;

        SQL_SQLCAFeltolt() ;

        /* EXEC SQL CLOSE select_nd_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 5;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )116;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}



        /* EXEC SQL COMMIT WORK ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 5;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )131;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}



        return -1 ;
      }

      // A nagydij feltoltese
      pNagydij->m_uiNagyd = nagydij_rekord[i].n_nagydij_nev ;
      pNagydij->m_uiPalya = nagydij_rekord[i].n_palya_nev   ;

      sscanf( nagydij_rekord[i].c_datum, "%d-%d-%d", &nEv, &nHonap, &nNap) ;

      pNagydij->m_sDatum.ev    = nEv    ;
      pNagydij->m_sDatum.honap = nHonap ;
      pNagydij->m_sDatum.nap   = nNap   ;

      pNagydijak[m_ucNagydSzam+i] = pNagydij ;
    }

    m_pNagydijak   = pNagydijak ;
    m_ucNagydSzam += nCiklFelsHat ;
  }

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  /* EXEC SQL CLOSE select_nd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )146;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL COMMIT WORK ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )161;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL CLOSE select_nd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )176;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

long CSzezon::SQL_SzezINSERT()
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve ;

    unsigned n_pontozas       ;
    unsigned n_edz_els_pont   ;
    unsigned n_leggy_kor_pont ;
    unsigned n_els_lgj_besz   ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  n_szezon_eve = m_uiSzezon   ;

  n_pontozas       = m_uiPontozas     ;
  n_edz_els_pont   = m_ucEdzElsPont   ;
  n_leggy_kor_pont = m_ucLeggyKorPont ;
  n_els_lgj_besz   = m_ucElsLgjBesz   ;

  /* EXEC SQL INSERT INTO szezon
             VALUES ( :n_szezon_eve,
                      :n_pontozas,
                      :n_edz_els_pont,
                      :n_leggy_kor_pont,
                      :n_els_lgj_besz) ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "insert into szezon values (:b0,:b1,:b2,:b3,:b4)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )191;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_pontozas;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_edz_els_pont;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&n_leggy_kor_pont;
  sqlstm.sqhstl[3] = (unsigned int  )4;
  sqlstm.sqhsts[3] = (         int  )0;
  sqlstm.sqindv[3] = (         void  *)0;
  sqlstm.sqinds[3] = (         int  )0;
  sqlstm.sqharm[3] = (unsigned int  )0;
  sqlstm.sqadto[3] = (unsigned short )0;
  sqlstm.sqtdso[3] = (unsigned short )0;
  sqlstm.sqhstv[4] = (         void  *)&n_els_lgj_besz;
  sqlstm.sqhstl[4] = (unsigned int  )4;
  sqlstm.sqhsts[4] = (         int  )0;
  sqlstm.sqindv[4] = (         void  *)0;
  sqlstm.sqinds[4] = (         int  )0;
  sqlstm.sqharm[4] = (unsigned int  )0;
  sqlstm.sqadto[4] = (unsigned short )0;
  sqlstm.sqtdso[4] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CSzezon::SQL_SzezUPDATE()
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve ;

    unsigned n_pontozas       ;
    unsigned n_edz_els_pont   ;
    unsigned n_leggy_kor_pont ;
    unsigned n_elso_lgj_besz  ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  n_szezon_eve = m_uiSzezon   ;

  n_pontozas       = m_uiPontozas     ;
  n_edz_els_pont   = m_ucEdzElsPont   ;
  n_leggy_kor_pont = m_ucLeggyKorPont ;
  n_elso_lgj_besz  = m_ucElsLgjBesz   ;

  /* EXEC SQL UPDATE szezon
             SET pontozas       = :n_pontozas,
                 edz_els_pont   = :n_edz_els_pont,
                 leggy_kor_pont = :n_leggy_kor_pont,
                 elso_legj_besz = :n_elso_lgj_besz
             WHERE szezon_eve = :n_szezon_eve ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "update szezon  set pontozas=:b0,edz_els_pont=:b1,leggy_kor_\
pont=:b2,elso_legj_besz=:b3 where szezon_eve=:b4";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )226;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_pontozas;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_edz_els_pont;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_leggy_kor_pont;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&n_elso_lgj_besz;
  sqlstm.sqhstl[3] = (unsigned int  )4;
  sqlstm.sqhsts[3] = (         int  )0;
  sqlstm.sqindv[3] = (         void  *)0;
  sqlstm.sqinds[3] = (         int  )0;
  sqlstm.sqharm[3] = (unsigned int  )0;
  sqlstm.sqadto[3] = (unsigned short )0;
  sqlstm.sqtdso[3] = (unsigned short )0;
  sqlstm.sqhstv[4] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[4] = (unsigned int  )4;
  sqlstm.sqhsts[4] = (         int  )0;
  sqlstm.sqindv[4] = (         void  *)0;
  sqlstm.sqinds[4] = (         int  )0;
  sqlstm.sqharm[4] = (unsigned int  )0;
  sqlstm.sqadto[4] = (unsigned short )0;
  sqlstm.sqtdso[4] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CSzezon::SQL_SzezTorol()
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  n_szezon_eve = m_uiSzezon ;

  /* EXEC SQL DELETE FROM nagydijak WHERE szezon_eve = :n_szezon_eve ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "delete  from nagydijak  where szezon_eve=:b0";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )261;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  /* EXEC SQL DELETE FROM szezon WHERE szezon_eve = :n_szezon_eve ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "delete  from szezon  where szezon_eve=:b0";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )280;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL CLOSE select_nd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )299;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

long CSzezon::SQL_NagydINSERT( SNagydij& sUjNagydij)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve  ;
    unsigned n_nagydij_nev ;
    unsigned n_palya_nev   ;
    char     c_datum[13]   ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


/*  EXEC SQL WHENEVER NOTFOUND CONTINUE ; */

  n_szezon_eve  = m_uiSzezon ;
  n_nagydij_nev = sUjNagydij.m_uiNagyd ;
  n_palya_nev   = sUjNagydij.m_uiPalya ;

  sprintf( c_datum, "%d-%d-%d",
                    sUjNagydij.m_sDatum.ev,
                    sUjNagydij.m_sDatum.honap,
                    sUjNagydij.m_sDatum.nap ) ;

  /* EXEC SQL INSERT INTO nagydijak( szezon_eve, nagydij_nev, palya_nev, datum)
             VALUES( :n_szezon_eve, :n_nagydij_nev, :n_palya_nev, TO_DATE( :c_datum, 'YYYY-MM-DD') ) ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "insert into nagydijak(szezon_eve,nagydij_nev,palya_nev,datu\
m) values (:b0,:b1,:b2,TO_DATE(:b3,'YYYY-MM-DD'))";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )314;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_nagydij_nev;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_palya_nev;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)c_datum;
  sqlstm.sqhstl[3] = (unsigned int  )13;
  sqlstm.sqhsts[3] = (         int  )0;
  sqlstm.sqindv[3] = (         void  *)0;
  sqlstm.sqinds[3] = (         int  )0;
  sqlstm.sqharm[3] = (unsigned int  )0;
  sqlstm.sqadto[3] = (unsigned short )0;
  sqlstm.sqtdso[3] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

/*  EXEC SQL COMMIT WORK ; CSzezFelv::OnCancel stb. */

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CSzezon::SQL_NagydDELETE(unsigned uiNdKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve  ;
    unsigned n_nagydij_nev ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  n_szezon_eve  = m_uiSzezon ;
  n_nagydij_nev = uiNdKulcs  ;

  /* EXEC SQL DELETE FROM nagydijak
             WHERE szezon_eve = :n_szezon_eve AND nagydij_nev = :n_nagydij_nev ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "delete  from nagydijak  where (szezon_eve=:b0 and nagydij_n\
ev=:b1)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )345;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_nagydij_nev;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

/*  EXEC SQL COMMIT WORK ; CSzezFelv::OnCancel stb. */

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CSzezon::SQL_NagydUPDATE(unsigned uiNdKulcs, SNagydij& sUjNagydij)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve  ;
    unsigned n_regi_nd_nev ;

    unsigned n_nagydij_nev ;
    unsigned n_palya_nev   ;
    char     c_datum[13]   ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  n_szezon_eve  = m_uiSzezon ;
  n_regi_nd_nev = uiNdKulcs  ;

  n_nagydij_nev = sUjNagydij.m_uiNagyd ;
  n_palya_nev   = sUjNagydij.m_uiPalya ;

  sprintf( c_datum, "%d-%d-%d",
                    sUjNagydij.m_sDatum.ev,
                    sUjNagydij.m_sDatum.honap,
                    sUjNagydij.m_sDatum.nap ) ;

  /* EXEC SQL UPDATE nagydijak
             SET nagydij_nev=:n_nagydij_nev, palya_nev=:n_palya_nev, datum=TO_DATE(:c_datum,'YYYY-MM-DD')
               WHERE szezon_eve = :n_szezon_eve AND nagydij_nev=:n_regi_nd_nev ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 5;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "update nagydijak  set nagydij_nev=:b0,palya_nev=:b1,datum=T\
O_DATE(:b2,'YYYY-MM-DD') where (szezon_eve=:b3 and nagydij_nev=:b4)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )368;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_nagydij_nev;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_palya_nev;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)c_datum;
  sqlstm.sqhstl[2] = (unsigned int  )13;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[3] = (unsigned int  )4;
  sqlstm.sqhsts[3] = (         int  )0;
  sqlstm.sqindv[3] = (         void  *)0;
  sqlstm.sqinds[3] = (         int  )0;
  sqlstm.sqharm[3] = (unsigned int  )0;
  sqlstm.sqadto[3] = (unsigned short )0;
  sqlstm.sqtdso[3] = (unsigned short )0;
  sqlstm.sqhstv[4] = (         void  *)&n_regi_nd_nev;
  sqlstm.sqhstl[4] = (unsigned int  )4;
  sqlstm.sqhsts[4] = (         int  )0;
  sqlstm.sqindv[4] = (         void  *)0;
  sqlstm.sqinds[4] = (         int  )0;
  sqlstm.sqharm[4] = (unsigned int  )0;
  sqlstm.sqadto[4] = (unsigned short )0;
  sqlstm.sqtdso[4] = (unsigned short )0;
  sqlstm.sqphsv = sqlstm.sqhstv;
  sqlstm.sqphsl = sqlstm.sqhstl;
  sqlstm.sqphss = sqlstm.sqhsts;
  sqlstm.sqpind = sqlstm.sqindv;
  sqlstm.sqpins = sqlstm.sqinds;
  sqlstm.sqparm = sqlstm.sqharm;
  sqlstm.sqparc = sqlstm.sqharc;
  sqlstm.sqpadto = sqlstm.sqadto;
  sqlstm.sqptdso = sqlstm.sqtdso;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

/*  EXEC SQL COMMIT WORK ; CSzezFelv::OnCancel stb. */

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}