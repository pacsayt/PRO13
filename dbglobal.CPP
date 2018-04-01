
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
    ".\\dbglobal.PC"
};


static unsigned long sqlctx = 517387;


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
            void  *sqhstv[12];
   unsigned int   sqhstl[12];
            int   sqhsts[12];
            void  *sqindv[12];
            int   sqinds[12];
   unsigned int   sqharm[12];
   unsigned int   *sqharc[12];
   unsigned short  sqadto[12];
   unsigned short  sqtdso[12];
} sqlstm = {10,12};

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

typedef struct { unsigned short len; unsigned char arr[1]; } VARCHAR;
typedef struct { unsigned short len; unsigned char arr[1]; } varchar;

/* cud (compilation unit data) array */
static const short sqlcud0[] =
{10,4130,0,0,0,
5,0,0,1,0,0,29,61,0,0,0,0,0,1,0,
20,0,0,2,0,0,31,74,0,0,0,0,0,1,0,
35,0,0,3,29,0,17,98,0,0,0,0,0,1,0,
50,0,0,3,0,0,45,104,0,0,0,0,0,1,0,
65,0,0,3,0,0,13,114,0,0,1,0,0,1,0,2,68,0,0,
84,0,0,3,0,0,15,126,0,0,0,0,0,1,0,
99,0,0,4,0,0,29,128,0,0,0,0,0,1,0,
114,0,0,3,0,0,15,152,0,0,0,0,0,1,0,
129,0,0,5,0,0,29,154,0,0,0,0,0,1,0,
144,0,0,3,0,0,15,175,0,0,0,0,0,1,0,
159,0,0,6,0,0,29,181,0,0,0,0,0,1,0,
174,0,0,3,0,0,15,193,0,0,0,0,0,1,0,
189,0,0,7,137,0,17,235,0,0,0,0,0,1,0,
204,0,0,7,0,0,45,239,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
227,0,0,7,0,0,13,246,0,0,1,0,0,1,0,2,68,0,0,
246,0,0,7,0,0,15,301,0,0,0,0,0,1,0,
261,0,0,8,0,0,29,307,0,0,0,0,0,1,0,
276,0,0,7,0,0,15,319,0,0,0,0,0,1,0,
291,0,0,9,0,0,17,414,0,0,1,1,0,1,0,1,5,0,0,
310,0,0,9,0,0,45,429,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
333,0,0,9,0,0,13,435,0,0,10,0,0,1,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,
0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,
388,0,0,9,0,0,15,441,0,0,0,0,0,1,0,
403,0,0,9,0,0,15,470,0,0,0,0,0,1,0,
418,0,0,9,0,0,15,493,0,0,0,0,0,1,0,
433,0,0,10,0,0,29,500,0,0,0,0,0,1,0,
448,0,0,9,0,0,15,512,0,0,0,0,0,1,0,
463,0,0,11,0,0,17,595,0,0,1,1,0,1,0,1,5,0,0,
482,0,0,11,0,0,45,607,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
505,0,0,11,0,0,13,613,0,0,12,0,0,1,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,
0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,
568,0,0,11,0,0,15,619,0,0,0,0,0,1,0,
583,0,0,11,0,0,15,645,0,0,0,0,0,1,0,
598,0,0,11,0,0,15,665,0,0,0,0,0,1,0,
613,0,0,12,0,0,29,676,0,0,0,0,0,1,0,
628,0,0,11,0,0,15,688,0,0,0,0,0,1,0,
643,0,0,13,0,0,17,781,0,0,1,1,0,1,0,1,5,0,0,
662,0,0,14,312,0,17,787,0,0,0,0,0,1,0,
677,0,0,13,0,0,45,799,0,0,1,1,0,1,0,1,68,0,0,
696,0,0,13,0,0,13,803,0,0,5,0,0,1,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,
0,
731,0,0,14,0,0,45,818,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
754,0,0,14,0,0,13,822,0,0,2,0,0,1,0,2,68,0,0,2,5,0,0,
777,0,0,14,0,0,15,838,0,0,0,0,0,1,0,
792,0,0,13,0,0,15,839,0,0,0,0,0,1,0,
807,0,0,14,0,0,13,888,0,0,2,0,0,1,0,2,68,0,0,2,5,0,0,
830,0,0,14,0,0,15,902,0,0,0,0,0,1,0,
845,0,0,13,0,0,15,911,0,0,0,0,0,1,0,
860,0,0,13,0,0,15,917,0,0,0,0,0,1,0,
875,0,0,14,0,0,15,929,0,0,0,0,0,1,0,
890,0,0,13,0,0,15,930,0,0,0,0,0,1,0,
905,0,0,14,0,0,15,945,0,0,0,0,0,1,0,
920,0,0,13,0,0,15,946,0,0,0,0,0,1,0,
935,0,0,15,71,0,6,1022,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
958,0,0,16,56,0,6,1033,0,0,1,1,0,1,0,1,68,0,0,
977,0,0,17,113,0,6,1073,0,0,4,4,0,1,0,3,68,0,0,3,68,0,0,2,68,0,0,2,68,0,0,
1008,0,0,18,62,0,6,1208,0,0,1,1,0,1,0,2,68,0,0,
1027,0,0,19,113,0,6,1253,0,0,4,4,0,1,0,3,68,0,0,3,68,0,0,2,68,0,0,2,68,0,0,
1058,0,0,20,62,0,6,1300,0,0,1,1,0,1,0,2,68,0,0,
1077,0,0,21,161,0,6,1351,0,0,7,7,0,1,0,1,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,
68,0,0,2,68,0,0,2,68,0,0,
1120,0,0,22,179,0,6,1420,0,0,8,8,0,1,0,1,68,0,0,1,68,0,0,2,68,0,0,2,68,0,0,2,
68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,
};


/* dbglobal.pc */

#include "stdafx.h"

#include "ensqlcam.h"

#include "resource.h"
#include "alaptip.h"
#include "dbglobal.h"

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


/* l. szez_all.sql n_nev_klcs_tmb_mer */
/* const unsigned NEV_TMB_MER   = 20 ; */
#define NEV_TMB_MER   20
/* l. szez_all.sql n_eredm_tmb_mer    */
/*const unsigned EREDM_TMB_MER = 50 ;*/
#define EREDM_TMB_MER 50

/* Az SQL_AltLekerd fuggvenyben hasznalt lekerdezeseket e sztringtombokbol allitom ossze */
/* A "SELECT n.rajtszam, n.versenyzo, n.nemzetiseg, n.csapat, n.motor" resz kozos        */
/* A lekerdezes tipusa szerint :                                                         */
char* gl_pLek1Tmb[]   = {                            /* , b.helyezes */
                          {", b.szezon_eve, b.nagydij_nev, b.ora, b.perc, b.masodperc, b.ezrdmsdp, b.megjegyzes " \
                           "FROM nevezesi_lista n, befuto b " \
                           "WHERE " \
                           "b.szezon_eve >= :n_szez_eve  AND " \
                           "n.kulcs      = b.nvzl_kulcs AND " \
                           "b.helyezes   = 1 AND "} , /* , r.helyezes */
                          {", r.szezon_eve, r.nagydij_nev, 0, r.perc, r.masodperc, r.ezrdmsdp, r.megjegyzes " \
                           "FROM nevezesi_lista n, rajtsorrend r " \
                           "WHERE " \
                           "r.szezon_eve >= :n_szez_eve AND " \
                           "n.kulcs      = r.nvzl_kulcs AND " \
                           "r.helyezes = 1 AND "} ,
                          {", l.szezon_eve, l.nagydij_nev, l.perc, l.masodperc, l.ezrdmsdp " \
                           "FROM nevezesi_lista n, leggyorsabb_kor l " \
                           "WHERE " \
                           "l.szezon_eve >= :n_szez_eve  AND " \
                           "n.kulcs      = l.nvzl_kulcs AND "}
                        } ;
/* Az utolsonal ^ kellhet meg : (l.korben, l.atl_sebesseg)  nem lesznek ugysem kijelezve */

char* gl_pLek2Tmb[]   = {
                          {" n.versenyzo   = :n_nev_kulcs "} ,
                          {" n.nemzetiseg  = :n_nev_kulcs "} ,
                          {" n.csapat      = :n_nev_kulcs "} ,
                          {" n.motor       = :n_nev_kulcs "} ,
                          {" b.nagydij_nev = :n_nev_kulcs "} ,
                          {" r.nagydij_nev = :n_nev_kulcs "} ,
                          {" l.nagydij_nev = :n_nev_kulcs "}
                        } ;

long SQL_COMMIT()
{
  long l_SQLCODE ;

  /* EXEC SQL COMMIT WORK ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 0;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )5;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
} ;

long SQL_ROLLBACK()
{
  long l_SQLCODE ;

  /* EXEC SQL ROLLBACK WORK ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 0;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )20;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
} ;

long SQL_FelvSzez( unsigned** ppSzezTomb, unsigned* pSzezdb)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned flv_szez_tomb[FLV_SZEZ_BLK] ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  int       nElhozott ;
  unsigned* pIdglSzezTomb ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  /* EXEC SQL PREPARE sel_flv_szez_ut
           FROM "SELECT szezon_eve FROM szezon" ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 0;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "SELECT szezon_eve FROM szezon";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )35;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  /* Ha nics hiba az utasitasokban, johet az erdemi resz */
  /* EXEC SQL DECLARE sel_flv_szez_krz CURSOR FOR sel_flv_szez_ut ; */ 


  /* EXEC SQL OPEN sel_flv_szez_krz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 0;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )50;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  nElhozott = 0 ;
  *ppSzezTomb   = NULL ;
  pIdglSzezTomb = NULL ;

  /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


  while ( TRUE )
  {
    /* EXEC SQL FETCH sel_flv_szez_krz INTO :flv_szez_tomb ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 1;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )20;
    sqlstm.offset = (unsigned int  )65;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)flv_szez_tomb;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )4;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqharc[0] = (unsigned int   *)0;
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
    if (sqlca.sqlcode == 1403) break;
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    if ( ( pIdglSzezTomb = new unsigned[sqlca.sqlerrd[2]] ) == NULL )
    {
      if ( nElhozott > 0 )
        delete *ppSzezTomb ;

      *ppSzezTomb = NULL ;
      *pSzezdb  = 0 ;

      SQL_SQLCAFeltolt() ;

      /* EXEC SQL CLOSE sel_flv_szez_krz ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 1;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )1;
      sqlstm.offset = (unsigned int  )84;
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
      sqlstm.arrsiz = 1;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )1;
      sqlstm.offset = (unsigned int  )99;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
      if (sqlca.sqlcode < 0) goto sql_hiba;
}



      return -1 ;
    }

    memcpy( pIdglSzezTomb, *ppSzezTomb, nElhozott*sizeof( unsigned)) ;
    delete *ppSzezTomb ;
    *ppSzezTomb = pIdglSzezTomb ;
    memcpy( *ppSzezTomb+nElhozott, flv_szez_tomb, (sqlca.sqlerrd[2] - nElhozott)*sizeof( unsigned)) ;
    nElhozott = sqlca.sqlerrd[2] ;
  }

  if ( sqlca.sqlerrd[2] > nElhozott )
  {
    if ( ( pIdglSzezTomb = new unsigned[sqlca.sqlerrd[2]] ) == NULL )
    {
      if ( nElhozott > 0 )
        delete *ppSzezTomb ;

      *ppSzezTomb = NULL ;
      *pSzezdb  = 0 ;

      SQL_SQLCAFeltolt() ;

      /* EXEC SQL CLOSE sel_flv_szez_krz ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 1;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )1;
      sqlstm.offset = (unsigned int  )114;
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
      sqlstm.arrsiz = 1;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )1;
      sqlstm.offset = (unsigned int  )129;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
      if (sqlca.sqlcode < 0) goto sql_hiba;
}



      return -1 ;
    }

    if ( nElhozott > 0 )
    {
      /* Csak, ha a while-ciklus mar lefutott egyszer */
      memcpy( pIdglSzezTomb, *ppSzezTomb, nElhozott*sizeof( unsigned)) ;
      delete *ppSzezTomb ;
    }

    *ppSzezTomb = pIdglSzezTomb ;
    memcpy( *ppSzezTomb+nElhozott, flv_szez_tomb, (sqlca.sqlerrd[2] - nElhozott)*sizeof( unsigned)) ;
    nElhozott = sqlca.sqlerrd[2] ;
  }

  *pSzezdb = nElhozott ;

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  /* EXEC SQL CLOSE sel_flv_szez_krz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 1;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )144;
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
  sqlstm.arrsiz = 1;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )159;
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

  /* EXEC SQL CLOSE sel_flv_szez_krz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 1;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )174;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

long SQL_KulNvzlNdon( unsigned uiSzezEve, unsigned uiNagydKulcs,
                      unsigned** ppNvzlKulcsTomb, unsigned* pNvzldb)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szez_eve ;
    unsigned n_nd_kulcs ;

    /* a rajtsorrend tablaban nincsen ora mezo */
    struct sql_s_rsnvzl
    {
      unsigned n_nvz_kulcs  ;
    } rsnvzl_rekord[30] ;
    /* 30, hogy az esetek nagy reszeben egyszerre elhozzon mindent */
    /* Sok szezonban max 26 versenyzo allhatott rajthoz            */

  /* EXEC SQL END DECLARE SECTION ; */ 


  int nFetch ;
  int nCiklFelsHat, nMeret ;

  unsigned* pIdglTbl   ;
  unsigned* pVisszaTbl ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


  pIdglTbl   = NULL ;
  pVisszaTbl = NULL ;

  n_szez_eve = uiSzezEve ;
  n_nd_kulcs = uiNagydKulcs ;

  /* Mely nevezesi lista bejegyzes fordult elo az adott szezon */
  /* adott nagydijan (Nem mint nevezesi lista bejegyzes )      */
  /* EXEC SQL PREPARE proba_utas FROM "SELECT nvzl_kulcs FROM rajtsorrend WHERE szezon_eve  = :n_szez_eve AND nagydij_nev = :n_nd_kulcs AND megjegyzes LIKE ('___________0____')" ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 1;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "SELECT nvzl_kulcs FROM rajtsorrend WHERE szezon_eve  = :n_szez_eve AND nagydij_nev = :n_nd_kulcs AND megjegyzes LIKE ('___________0____')";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )189;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  /* EXEC SQL DECLARE proba_kurz CURSOR FOR proba_utas ; */ 


  /* EXEC SQL OPEN proba_kurz USING :n_szez_eve, :n_nd_kulcs ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 2;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )204;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_szez_eve;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_nd_kulcs;
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



  nMeret = 0 ;
  nFetch = 0 ;

  while ( TRUE )
  {
    /* EXEC SQL FETCH proba_kurz INTO :rsnvzl_rekord ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 2;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )30;
    sqlstm.offset = (unsigned int  )227;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&rsnvzl_rekord->n_nvz_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_rsnvzl);
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqharc[0] = (unsigned int   *)0;
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
    if (sqlca.sqlcode == 1403) break;
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    /* Ez cikluson belul mindig 30 */
    nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;
    pIdglTbl = new unsigned[nMeret+nCiklFelsHat] ;

    if ( pIdglTbl == NULL )
    {
      delete [] pVisszaTbl ;

      return -1 ;
    }

    /* Atmasoljuk a regit az ideiglenes helyere */
    memcpy( pIdglTbl, pVisszaTbl, nMeret*sizeof(unsigned)) ;

    memcpy( pIdglTbl+nMeret, rsnvzl_rekord, nCiklFelsHat*sizeof(unsigned)) ;

    /* Toroljuk a regi tombot */
    delete [] pVisszaTbl ;
    pVisszaTbl = NULL ;

    nFetch = sqlca.sqlerrd[2] ;
    pVisszaTbl = pIdglTbl ;
    nMeret += nCiklFelsHat ;
  }

  nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;

  if ( nCiklFelsHat > 0 )
  {
    pIdglTbl = new unsigned[nMeret+nCiklFelsHat] ;

    if ( pIdglTbl == NULL )
    {
      delete [] pVisszaTbl ;

      return -1 ;
    }

    /* Atmasoljuk a regit az ideiglenes helyere */
    memcpy( pIdglTbl, pVisszaTbl, nMeret*sizeof(unsigned)) ;
    memcpy( pIdglTbl+nMeret, rsnvzl_rekord, nCiklFelsHat*sizeof(unsigned)) ;

    /* Toroljuk a regi tombot */
    delete [] pVisszaTbl ;
    pVisszaTbl = NULL ;

    pVisszaTbl = pIdglTbl ;
    nMeret += nCiklFelsHat ;
  }

  *pNvzldb = nMeret ;
  *ppNvzlKulcsTomb = pVisszaTbl ;

  /* EXEC SQL CLOSE proba_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 2;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )246;
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
  sqlstm.arrsiz = 2;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )261;
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

  /* EXEC SQL CLOSE proba_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 2;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )276;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

/*
ID_LEKERD_VERSENYZNV ID_LEKERD_NEMZETISGNV ID_LEKERD_CSAPAT ID_LEKERD_MOTORNV
ID_LEKERD_NAGYDJNV palya nevekre nincs lekerdezes

IDC_LEK1_FUTGY_RB2 IDC_LEK1_EDZELS_RB3 IDC_LEK1_LEGGYKOR_RB4 IDC_LEK1_OSSZER_RB5
*/
/* A visszaszallitott adatok tipusanak kulonbozosege miatt kell */
/* SQL_LgGyKorAltLekerd SQL_RajtsBefuto                      */
long SQL_LeggyKor( eLekerdTipus enLekTip, eNevtipus enNevTip,
                   unsigned nSzezEve, unsigned* pNevKlcsTmb, unsigned nBemTmbMeret,
                   sLgGyKorLekErTip** ppLgGyKorLekEr, unsigned* pKiTmbMeret)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szez_eve ;
    unsigned n_nev_klcs ;

    char     s_lekerd[400]  ;

    struct sql_s_lggyk_lek_tip
    {
      unsigned n_rajtszam    ;
      unsigned n_versenyzo   ;
      unsigned n_nemzetiseg  ;
      unsigned n_csapat      ;
      unsigned n_motor       ;
      unsigned n_szezon_eve  ;
      unsigned n_nagydij_nev ;
      unsigned n_perc        ;
      unsigned n_masodperc   ;
      unsigned n_ezrdmsdperc ;
/*      unsigned n_megjegyzes  ; itt ilyen nincs ! */
/*      unsigned n_korben      ;
      float    f_atl_sebesseg  nem lesz ugysem kijelezve */
    } lggyk_lek_rekord[EREDM_TMB_MER] ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  unsigned nAktNevElh   ;
  unsigned nCiklFelsHat ;
  unsigned nNevKlcsIdx  ;

  sLgGyKorLekErTip* pLokLgGyKorLekEr ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


  /* Ha nagyobb, mint nulla felszabaditom a hivo fv-bol a kimeneti tombot */
  *pKiTmbMeret = 0 ;
  *ppLgGyKorLekEr = NULL ;

  /* Az osszes eredmenyt egyel magasabb szinten kell osszerakni */
  /* SQL_RajtsBefuto es SQL_LgGyKorAltLekerd                 */
  if ( enLekTip != eLeggyKor )
  {
    return -1 ;
  }

/* TRACE("\nSQL_LgGyKorAltLekerd enNevTip:%d enLekTip:%d\n", enNevTip, enLekTip) ;*/

  switch ( enNevTip )
  {
    case eNagydijNt :
      sprintf( s_lekerd,
               "SELECT n.rajtszam, n.versenyzo, n.nemzetiseg, n.csapat, n.motor %s %s",
               gl_pLek1Tmb[enLekTip],
               gl_pLek2Tmb[enNevTip+enLekTip] ) ;
      break ;
    case eVersNt  :
    case eNemzNt  :
    case eCsapNt  :
    case eMotorNt :
      sprintf( s_lekerd,
               "SELECT n.rajtszam, n.versenyzo, n.nemzetiseg, n.csapat, n.motor %s %s",
               gl_pLek1Tmb[enLekTip],
               gl_pLek2Tmb[enNevTip] ) ;
      break ;

    default :
      /* Nemletezo nevtipus : hiba */
      return -1 ;
  }

/*TRACE("\nSQL_LgGyKorAltLekerd s_lekerd:%s\n", s_lekerd) ;*/

  /* 4. tip. din. SQL (ism.len szamu parameter) ?                    */
  /* rem maceras es a fuggvenybol valo visszaadas sem problamamentes */

  /* EXEC SQL PREPARE leggy_kor_ut FROM :s_lekerd ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 2;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )291;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)s_lekerd;
  sqlstm.sqhstl[0] = (unsigned int  )400;
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



/*  n_bem_tmb_mer = nBemTmbMeret ; < ???*/

  /* EXEC SQL DECLARE leggy_kor_kurz CURSOR FOR leggy_kor_ut ; */ 


  n_szez_eve = nSzezEve ;

  for ( nNevKlcsIdx = 0 ; nNevKlcsIdx < nBemTmbMeret ; nNevKlcsIdx++ )
  {
    n_nev_klcs = pNevKlcsTmb[nNevKlcsIdx] ;

    /* Mivel minden kurzornyitasnal(nevnel) sqlca.sqlerrd[2] nullarol kezdodik */
    nAktNevElh = 0 ;

    /* EXEC SQL OPEN leggy_kor_kurz USING :n_szez_eve, :n_nev_klcs ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 2;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )310;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_szez_eve;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_nev_klcs;
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



    /* EXEC SQL WHENEVER NOTFOUND do break ; */ 
/* Es ha a for-bol is kiugrik ? */

    while ( TRUE )
    {
      /* EXEC SQL FETCH leggy_kor_kurz INTO :lggyk_lek_rekord ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 10;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )50;
      sqlstm.offset = (unsigned int  )333;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)&lggyk_lek_rekord->n_rajtszam;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )0;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&lggyk_lek_rekord->n_versenyzo;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[1] = (         void  *)0;
      sqlstm.sqinds[1] = (         int  )0;
      sqlstm.sqharm[1] = (unsigned int  )0;
      sqlstm.sqharc[1] = (unsigned int   *)0;
      sqlstm.sqadto[1] = (unsigned short )0;
      sqlstm.sqtdso[1] = (unsigned short )0;
      sqlstm.sqhstv[2] = (         void  *)&lggyk_lek_rekord->n_nemzetiseg;
      sqlstm.sqhstl[2] = (unsigned int  )4;
      sqlstm.sqhsts[2] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[2] = (         void  *)0;
      sqlstm.sqinds[2] = (         int  )0;
      sqlstm.sqharm[2] = (unsigned int  )0;
      sqlstm.sqharc[2] = (unsigned int   *)0;
      sqlstm.sqadto[2] = (unsigned short )0;
      sqlstm.sqtdso[2] = (unsigned short )0;
      sqlstm.sqhstv[3] = (         void  *)&lggyk_lek_rekord->n_csapat;
      sqlstm.sqhstl[3] = (unsigned int  )4;
      sqlstm.sqhsts[3] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[3] = (         void  *)0;
      sqlstm.sqinds[3] = (         int  )0;
      sqlstm.sqharm[3] = (unsigned int  )0;
      sqlstm.sqharc[3] = (unsigned int   *)0;
      sqlstm.sqadto[3] = (unsigned short )0;
      sqlstm.sqtdso[3] = (unsigned short )0;
      sqlstm.sqhstv[4] = (         void  *)&lggyk_lek_rekord->n_motor;
      sqlstm.sqhstl[4] = (unsigned int  )4;
      sqlstm.sqhsts[4] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[4] = (         void  *)0;
      sqlstm.sqinds[4] = (         int  )0;
      sqlstm.sqharm[4] = (unsigned int  )0;
      sqlstm.sqharc[4] = (unsigned int   *)0;
      sqlstm.sqadto[4] = (unsigned short )0;
      sqlstm.sqtdso[4] = (unsigned short )0;
      sqlstm.sqhstv[5] = (         void  *)&lggyk_lek_rekord->n_szezon_eve;
      sqlstm.sqhstl[5] = (unsigned int  )4;
      sqlstm.sqhsts[5] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[5] = (         void  *)0;
      sqlstm.sqinds[5] = (         int  )0;
      sqlstm.sqharm[5] = (unsigned int  )0;
      sqlstm.sqharc[5] = (unsigned int   *)0;
      sqlstm.sqadto[5] = (unsigned short )0;
      sqlstm.sqtdso[5] = (unsigned short )0;
      sqlstm.sqhstv[6] = (         void  *)&lggyk_lek_rekord->n_nagydij_nev;
      sqlstm.sqhstl[6] = (unsigned int  )4;
      sqlstm.sqhsts[6] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[6] = (         void  *)0;
      sqlstm.sqinds[6] = (         int  )0;
      sqlstm.sqharm[6] = (unsigned int  )0;
      sqlstm.sqharc[6] = (unsigned int   *)0;
      sqlstm.sqadto[6] = (unsigned short )0;
      sqlstm.sqtdso[6] = (unsigned short )0;
      sqlstm.sqhstv[7] = (         void  *)&lggyk_lek_rekord->n_perc;
      sqlstm.sqhstl[7] = (unsigned int  )4;
      sqlstm.sqhsts[7] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[7] = (         void  *)0;
      sqlstm.sqinds[7] = (         int  )0;
      sqlstm.sqharm[7] = (unsigned int  )0;
      sqlstm.sqharc[7] = (unsigned int   *)0;
      sqlstm.sqadto[7] = (unsigned short )0;
      sqlstm.sqtdso[7] = (unsigned short )0;
      sqlstm.sqhstv[8] = (         void  *)&lggyk_lek_rekord->n_masodperc;
      sqlstm.sqhstl[8] = (unsigned int  )4;
      sqlstm.sqhsts[8] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[8] = (         void  *)0;
      sqlstm.sqinds[8] = (         int  )0;
      sqlstm.sqharm[8] = (unsigned int  )0;
      sqlstm.sqharc[8] = (unsigned int   *)0;
      sqlstm.sqadto[8] = (unsigned short )0;
      sqlstm.sqtdso[8] = (unsigned short )0;
      sqlstm.sqhstv[9] = (         void  *)&lggyk_lek_rekord->n_ezrdmsdperc;
      sqlstm.sqhstl[9] = (unsigned int  )4;
      sqlstm.sqhsts[9] = (         int  )sizeof(struct sql_s_lggyk_lek_tip);
      sqlstm.sqindv[9] = (         void  *)0;
      sqlstm.sqinds[9] = (         int  )0;
      sqlstm.sqharm[9] = (unsigned int  )0;
      sqlstm.sqharc[9] = (unsigned int   *)0;
      sqlstm.sqadto[9] = (unsigned short )0;
      sqlstm.sqtdso[9] = (unsigned short )0;
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



      pLokLgGyKorLekEr = new sLgGyKorLekErTip[*pKiTmbMeret+sqlca.sqlerrd[2]] ;

      if ( pLokLgGyKorLekEr == NULL )
      {
        /* EXEC SQL CLOSE leggy_kor_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 10;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )388;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}



        *pKiTmbMeret = *pKiTmbMeret + nAktNevElh ;

        return -1 ;
      }

      /* Atmasoljuk a regit az ideiglenes helyere */
      memcpy( pLokLgGyKorLekEr, *ppLgGyKorLekEr, (*pKiTmbMeret+nAktNevElh)*sizeof(sLgGyKorLekErTip)) ;

      /* Hozzamasoljuk az ujonnan elhozottakat    */
      memcpy( pLokLgGyKorLekEr+*pKiTmbMeret+nAktNevElh, lggyk_lek_rekord, EREDM_TMB_MER*sizeof(sLgGyKorLekErTip)) ;

      /* Toroljuk a regi tombot */
      delete [] *ppLgGyKorLekEr ;

      *ppLgGyKorLekEr = pLokLgGyKorLekEr ;

      nAktNevElh = sqlca.sqlerrd[2] ;
    }

    nCiklFelsHat = sqlca.sqlerrd[2] - nAktNevElh ;
    if ( nCiklFelsHat > 0 )
    {
      pLokLgGyKorLekEr = new sLgGyKorLekErTip[*pKiTmbMeret+sqlca.sqlerrd[2]] ;

      /* A mar elhozottakat nem szabaditjuk fel, *pKiTmbMeret mindig aktualis */
      if ( pLokLgGyKorLekEr == NULL )
      {
        /* EXEC SQL CLOSE leggy_kor_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 10;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )403;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}



        *pKiTmbMeret = *pKiTmbMeret + nAktNevElh ;

        return -1 ;
      }

      /* Atmasoljuk a regit az ideiglenes helyere */
      memcpy( pLokLgGyKorLekEr, *ppLgGyKorLekEr, (*pKiTmbMeret+nAktNevElh)*sizeof(sLgGyKorLekErTip)) ;

      /* Hozzamasoljuk az ujonnan elhozottakat    */
      memcpy( pLokLgGyKorLekEr+*pKiTmbMeret+nAktNevElh, lggyk_lek_rekord, nCiklFelsHat*sizeof(sLgGyKorLekErTip)) ;

      /* Toroljuk a regi tombot */
      delete [] *ppLgGyKorLekEr ;

      *ppLgGyKorLekEr = pLokLgGyKorLekEr ;

      nAktNevElh = sqlca.sqlerrd[2] ;
    }

    *pKiTmbMeret = *pKiTmbMeret + nAktNevElh ;

    /* EXEC SQL CLOSE leggy_kor_kurz ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 10;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )418;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto sql_hiba;
}


  }

  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL COMMIT WORK ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 10;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )433;
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

  /* EXEC SQL CLOSE leggy_kor_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 10;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )448;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
} ;

long SQL_RajtsBefuto( eLekerdTipus enLekTip, eNevtipus enNevTip,
                      unsigned nSzezEve, unsigned* pNevKlcsTmb, unsigned nBemTmbMeret,
                      sRsBefLekErTip** ppRsBefLekEr, unsigned* pKiTmbMeret)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


/*    unsigned n_bem_tmb_mer ;*/

    unsigned n_szez_eve ;
    unsigned n_nev_klcs ;
    char      s_lekerd[400]  ;

    struct sql_s_rs_bef_lek_tip
    {
      unsigned n_rajtszam    ;
      unsigned n_versenyzo   ;
      unsigned n_nemzetiseg  ;
      unsigned n_csapat      ;
      unsigned n_motor       ;
      unsigned n_szezon_eve  ;
      unsigned n_nagydij_nev ;
/*      unsigned n_helyezes    ;*/
      unsigned n_ora         ;
      unsigned n_perc        ;
      unsigned n_masodperc   ;
      unsigned n_ezrdmsdperc ;
      unsigned n_megjegyzes  ;
    } rs_bef_lek_rekord[EREDM_TMB_MER] ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  unsigned nAktNevElh   ;
  unsigned nNevKlcsIdx  ;

  sRsBefLekErTip* pLokRsBefLekEr ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


  *pKiTmbMeret = 0 ;
  *ppRsBefLekEr = NULL ;

  /* Az osszes eredmenyt egyel magasabb szinten kell osszerakni */
  /* SQL_RajtsBefuto es SQL_LgGyKorAltLekerd                 */
  if ( enLekTip != eFutgy && enLekTip != eEdzEls )
  {
    return -1 ;
  }

/*TRACE("\nSQL_RajtsBefuto enNevTip:%d enLekTip:%d\n", enNevTip, enLekTip) ;*/

  switch ( enNevTip )
  {
    case eNagydijNt :
      sprintf( s_lekerd,
               "SELECT n.rajtszam, n.versenyzo, n.nemzetiseg, n.csapat, n.motor%s %s",
               gl_pLek1Tmb[enLekTip],
               gl_pLek2Tmb[enNevTip+enLekTip] ) ;
      break ;
    case eVersNt  :
    case eNemzNt  :
    case eCsapNt  :
    case eMotorNt :
      sprintf( s_lekerd,
               "SELECT n.rajtszam, n.versenyzo, n.nemzetiseg, n.csapat, n.motor%s %s",
               gl_pLek1Tmb[enLekTip],
               gl_pLek2Tmb[enNevTip] ) ;
      break ;

    default :
      /* Nemletezo nevtipus : hiba */
      return -1 ;
  }

/*TRACE("\ns_lekerd:%s\n", s_lekerd) ;*/

  /* EXEC SQL PREPARE alt_lek_ut FROM :s_lekerd ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 10;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )463;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)s_lekerd;
  sqlstm.sqhstl[0] = (unsigned int  )400;
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



  /* EXEC SQL DECLARE alt_lek_kurz2 CURSOR FOR alt_lek_ut ; */ 


  n_szez_eve = nSzezEve ;

  for ( nNevKlcsIdx = 0 ; nNevKlcsIdx < nBemTmbMeret ; nNevKlcsIdx++ )
  {
    n_nev_klcs = pNevKlcsTmb[nNevKlcsIdx] ;

    nAktNevElh = 0 ;

    /* EXEC SQL OPEN alt_lek_kurz2 USING :n_szez_eve, :n_nev_klcs ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 10;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )482;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_szez_eve;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_nev_klcs;
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



    /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


    while ( TRUE )
    {
      /* EXEC SQL FETCH alt_lek_kurz2 INTO :rs_bef_lek_rekord ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 12;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )50;
      sqlstm.offset = (unsigned int  )505;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)&rs_bef_lek_rekord->n_rajtszam;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )0;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&rs_bef_lek_rekord->n_versenyzo;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[1] = (         void  *)0;
      sqlstm.sqinds[1] = (         int  )0;
      sqlstm.sqharm[1] = (unsigned int  )0;
      sqlstm.sqharc[1] = (unsigned int   *)0;
      sqlstm.sqadto[1] = (unsigned short )0;
      sqlstm.sqtdso[1] = (unsigned short )0;
      sqlstm.sqhstv[2] = (         void  *)&rs_bef_lek_rekord->n_nemzetiseg;
      sqlstm.sqhstl[2] = (unsigned int  )4;
      sqlstm.sqhsts[2] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[2] = (         void  *)0;
      sqlstm.sqinds[2] = (         int  )0;
      sqlstm.sqharm[2] = (unsigned int  )0;
      sqlstm.sqharc[2] = (unsigned int   *)0;
      sqlstm.sqadto[2] = (unsigned short )0;
      sqlstm.sqtdso[2] = (unsigned short )0;
      sqlstm.sqhstv[3] = (         void  *)&rs_bef_lek_rekord->n_csapat;
      sqlstm.sqhstl[3] = (unsigned int  )4;
      sqlstm.sqhsts[3] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[3] = (         void  *)0;
      sqlstm.sqinds[3] = (         int  )0;
      sqlstm.sqharm[3] = (unsigned int  )0;
      sqlstm.sqharc[3] = (unsigned int   *)0;
      sqlstm.sqadto[3] = (unsigned short )0;
      sqlstm.sqtdso[3] = (unsigned short )0;
      sqlstm.sqhstv[4] = (         void  *)&rs_bef_lek_rekord->n_motor;
      sqlstm.sqhstl[4] = (unsigned int  )4;
      sqlstm.sqhsts[4] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[4] = (         void  *)0;
      sqlstm.sqinds[4] = (         int  )0;
      sqlstm.sqharm[4] = (unsigned int  )0;
      sqlstm.sqharc[4] = (unsigned int   *)0;
      sqlstm.sqadto[4] = (unsigned short )0;
      sqlstm.sqtdso[4] = (unsigned short )0;
      sqlstm.sqhstv[5] = (         void  *)&rs_bef_lek_rekord->n_szezon_eve;
      sqlstm.sqhstl[5] = (unsigned int  )4;
      sqlstm.sqhsts[5] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[5] = (         void  *)0;
      sqlstm.sqinds[5] = (         int  )0;
      sqlstm.sqharm[5] = (unsigned int  )0;
      sqlstm.sqharc[5] = (unsigned int   *)0;
      sqlstm.sqadto[5] = (unsigned short )0;
      sqlstm.sqtdso[5] = (unsigned short )0;
      sqlstm.sqhstv[6] = (         void  *)&rs_bef_lek_rekord->n_nagydij_nev;
      sqlstm.sqhstl[6] = (unsigned int  )4;
      sqlstm.sqhsts[6] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[6] = (         void  *)0;
      sqlstm.sqinds[6] = (         int  )0;
      sqlstm.sqharm[6] = (unsigned int  )0;
      sqlstm.sqharc[6] = (unsigned int   *)0;
      sqlstm.sqadto[6] = (unsigned short )0;
      sqlstm.sqtdso[6] = (unsigned short )0;
      sqlstm.sqhstv[7] = (         void  *)&rs_bef_lek_rekord->n_ora;
      sqlstm.sqhstl[7] = (unsigned int  )4;
      sqlstm.sqhsts[7] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[7] = (         void  *)0;
      sqlstm.sqinds[7] = (         int  )0;
      sqlstm.sqharm[7] = (unsigned int  )0;
      sqlstm.sqharc[7] = (unsigned int   *)0;
      sqlstm.sqadto[7] = (unsigned short )0;
      sqlstm.sqtdso[7] = (unsigned short )0;
      sqlstm.sqhstv[8] = (         void  *)&rs_bef_lek_rekord->n_perc;
      sqlstm.sqhstl[8] = (unsigned int  )4;
      sqlstm.sqhsts[8] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[8] = (         void  *)0;
      sqlstm.sqinds[8] = (         int  )0;
      sqlstm.sqharm[8] = (unsigned int  )0;
      sqlstm.sqharc[8] = (unsigned int   *)0;
      sqlstm.sqadto[8] = (unsigned short )0;
      sqlstm.sqtdso[8] = (unsigned short )0;
      sqlstm.sqhstv[9] = (         void  *)&rs_bef_lek_rekord->n_masodperc;
      sqlstm.sqhstl[9] = (unsigned int  )4;
      sqlstm.sqhsts[9] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[9] = (         void  *)0;
      sqlstm.sqinds[9] = (         int  )0;
      sqlstm.sqharm[9] = (unsigned int  )0;
      sqlstm.sqharc[9] = (unsigned int   *)0;
      sqlstm.sqadto[9] = (unsigned short )0;
      sqlstm.sqtdso[9] = (unsigned short )0;
      sqlstm.sqhstv[10] = (         void  *)&rs_bef_lek_rekord->n_ezrdmsdperc;
      sqlstm.sqhstl[10] = (unsigned int  )4;
      sqlstm.sqhsts[10] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[10] = (         void  *)0;
      sqlstm.sqinds[10] = (         int  )0;
      sqlstm.sqharm[10] = (unsigned int  )0;
      sqlstm.sqharc[10] = (unsigned int   *)0;
      sqlstm.sqadto[10] = (unsigned short )0;
      sqlstm.sqtdso[10] = (unsigned short )0;
      sqlstm.sqhstv[11] = (         void  *)&rs_bef_lek_rekord->n_megjegyzes;
      sqlstm.sqhstl[11] = (unsigned int  )4;
      sqlstm.sqhsts[11] = (         int  )sizeof(struct sql_s_rs_bef_lek_tip);
      sqlstm.sqindv[11] = (         void  *)0;
      sqlstm.sqinds[11] = (         int  )0;
      sqlstm.sqharm[11] = (unsigned int  )0;
      sqlstm.sqharc[11] = (unsigned int   *)0;
      sqlstm.sqadto[11] = (unsigned short )0;
      sqlstm.sqtdso[11] = (unsigned short )0;
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


/*>>>*/
      pLokRsBefLekEr = new sRsBefLekErTip[*pKiTmbMeret+sqlca.sqlerrd[2]] ;

      if ( pLokRsBefLekEr == NULL )
      {
        /* EXEC SQL CLOSE alt_lek_kurz2 ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )568;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}


        return -1 ;
      }

      /* Atmasoljuk a regit az ideiglenes helyere */
      memcpy( pLokRsBefLekEr, *ppRsBefLekEr, (*pKiTmbMeret+nAktNevElh)*sizeof(sRsBefLekErTip)) ;

      /* Hozzamasoljuk az ujonnan elhozottakat. Itt a meret mindig EREDM_TMB_MER */
      memcpy( pLokRsBefLekEr+*pKiTmbMeret+nAktNevElh, rs_bef_lek_rekord, EREDM_TMB_MER*sizeof(sRsBefLekErTip)) ;

      /* Toroljuk a regi tombot */
      delete [] *ppRsBefLekEr ;

      *ppRsBefLekEr = pLokRsBefLekEr ;

      nAktNevElh = sqlca.sqlerrd[2] ;
    }

/*>>>*/
    if ( sqlca.sqlerrd[2] - nAktNevElh > 0 )
    {
      pLokRsBefLekEr = new sRsBefLekErTip[*pKiTmbMeret+sqlca.sqlerrd[2]] ;

      /* A mar elhozottakat nem szabaditjuk fel, *pKiTmbMeret mindig aktualis */
      if ( pLokRsBefLekEr == NULL )
      {
        /* EXEC SQL CLOSE alt_lek_kurz2 ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )583;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}


        return -1 ;
      }

      /* Atmasoljuk a regit az ideiglenes helyere */
      memcpy( pLokRsBefLekEr, *ppRsBefLekEr, (*pKiTmbMeret+nAktNevElh)*sizeof(sRsBefLekErTip)) ;

      /* Hozzamasoljuk az ujonnan elhozottakat    */
      memcpy( pLokRsBefLekEr+*pKiTmbMeret+nAktNevElh,
              rs_bef_lek_rekord,
              (sqlca.sqlerrd[2] - nAktNevElh)*sizeof(sRsBefLekErTip)) ;

      /* Toroljuk a regi tombot */
      delete [] *ppRsBefLekEr ;

      *ppRsBefLekEr = pLokRsBefLekEr ;

      nAktNevElh = sqlca.sqlerrd[2] ;
    }

    /* EXEC SQL CLOSE alt_lek_kurz2 ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 12;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )598;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    *pKiTmbMeret = *pKiTmbMeret + nAktNevElh ;
  }

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL COMMIT WORK ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )613;
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

  /* EXEC SQL CLOSE alt_lek_kurz2 ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )628;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
} ;

#define HOST_TMB_MER 50

long SQL_HolVersKiVers( eNevtipus enNevTip,
                        unsigned nSzezEve, unsigned* pNevKlcsTmb, unsigned nBemTmbMeret,
                        sHolVKiVLekErTip** ppHolVKiVLekEr, unsigned* pKiTmbMeret)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned  n_szez_tol ;

    unsigned  n_nev_klcs ;

    char      s_nvzl_lekerd[300] ;

    /* A nevezesi kulcs kiveve a nvzl_klcs_tmb-bol, */
    /* mert masik kurzor   bemenoadata              */
    unsigned  nvzl_klcs_tmb[HOST_TMB_MER] ;
    
    /* Az eppen elhozott nevezesi lista elemek szamat adja meg a */
    /* kovetkezo kurzor megnyitasakor                            */
    unsigned n_elhozott_nvzl ;

    struct sql_s_nvzl_tip
    {
      unsigned n_versenyzo  ;
      unsigned n_nemzetiseg ;
      unsigned n_csapat     ;
      unsigned n_motor      ;
    } nvzl_tmb[HOST_TMB_MER] ;

    /* A nevezesi lista adatok nem elegendoek kell a nagydij es datum is */
    /* Ezeket kulon lekerdezessel hozom el                               */
    struct sql_s_nagyd_tip
    {
/*      unsigned n_nvzl_kulcs  ; ezt nem tudom a lekerdezessel elhozni */
      unsigned n_nagydij_nev ;
/*      unsigned n_palya_nev   ;*/
      char     c_datum[13]   ;
    } nagyd_rekord[HOST_TMB_MER] ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  sHolVKiVLekErTip* pLokHolVKiVLekEr ;

  unsigned nNvzlIdx     ;
  unsigned nOsszElhNvzl ;
  unsigned nNagydIdx    ;
  unsigned nOsszElhNd   ;
  unsigned nAktElhNd    ;
  unsigned nKiTmbFogl   ;
  unsigned nNevKlcsIdx  ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  nOsszElhNvzl    = 0 ;
  n_elhozott_nvzl = 0 ;
  nKiTmbFogl      = 0 ;
  pLokHolVKiVLekEr= NULL ; 
  *pKiTmbMeret    = 0 ;
  *ppHolVKiVLekEr = NULL ;

/*TRACE("\nSQL_HolVersKiVers enNevTip:%d\n", enNevTip) ;*/

  switch ( enNevTip )
  {
    case eVersNt  :
    case eNemzNt  :
    case eCsapNt  :
    case eMotorNt :
       sprintf( s_nvzl_lekerd,
               "SELECT kulcs, versenyzo, nemzetiseg, csapat, motor\n" \
               "FROM nevezesi_lista n\n" \
               "WHERE %s",
               gl_pLek2Tmb[enNevTip] ) ;
      break ;

    default :
      /* Hibas nevtipus */
      return -1 ;
  }

/*TRACE("\nSQL_HolVersKiVers s_nvzl_lekerd:%s\n", s_nvzl_lekerd) ;*/

  /* A nev kulcsokbol nevezesi lista adatokat  eloallito */
  /* lekerdezes elokeszitese                             */
  /* EXEC SQL PREPARE nvzl_lekerd_ut FROM :s_nvzl_lekerd ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )643;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)s_nvzl_lekerd;
  sqlstm.sqhstl[0] = (unsigned int  )300;
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



  /* EXEC SQL DECLARE nvzl_lekerd_kurz CURSOR FOR nvzl_lekerd_ut ; */ 


  /* A nevezesi lista kulcsokbol nagydij/palya neveket   */
  /* datumokat aloallito lekerdezes elokeszitese         */
  /* EXEC SQL PREPARE nagyd_lekerd_ut FROM
    "SELECT nagydij_nev, TO_CHAR(datum,'YYYY-MM-DD') FROM nagydijak WHERE datum IN (SELECT MIN(nd.datum) FROM rajtsorrend r, nevezesi_lista n, nagydijak nd WHERE r.nvzl_kulcs = :a AND r.szezon_eve >= :b AND nd.szezon_eve = r.szezon_eve AND nd.nagydij_nev=r.nagydij_nev AND n.kulcs=r.nvzl_kulcs GROUP BY(r.szezon_eve))" ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "SELECT nagydij_nev, TO_CHAR(datum,'YYYY-MM-DD') FROM nagydijak WHERE datum IN (SELECT MIN(nd.datum) FROM rajtsorrend r, nevezesi_lista n, nagydijak nd WHERE r.nvzl_kulcs = :a AND r.szezon_eve >= :b AND nd.szezon_eve = r.szezon_eve AND nd.nagydij_nev=r.nagydij_nev AND n.kulcs=r.nvzl_kulcs GROUP BY(r.szezon_eve))";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )662;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}


  /*, palya_nev*/

  /* EXEC SQL DECLARE nagyd_lekerd_kurz CURSOR FOR nagyd_lekerd_ut ; */ 


  n_szez_tol = nSzezEve ;

  for ( nNevKlcsIdx = 0 ; nNevKlcsIdx < nBemTmbMeret ; nNevKlcsIdx++ )
  {
    n_nev_klcs = pNevKlcsTmb[nNevKlcsIdx] ;

    /* EXEC SQL OPEN nvzl_lekerd_kurz USING :n_nev_klcs ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 12;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )677;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nev_klcs;
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



    while ( TRUE )
    {
      /* EXEC SQL FETCH nvzl_lekerd_kurz INTO :nvzl_klcs_tmb, :nvzl_tmb ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 12;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )50;
      sqlstm.offset = (unsigned int  )696;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)nvzl_klcs_tmb;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )4;
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )0;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&nvzl_tmb->n_versenyzo;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_nvzl_tip);
      sqlstm.sqindv[1] = (         void  *)0;
      sqlstm.sqinds[1] = (         int  )0;
      sqlstm.sqharm[1] = (unsigned int  )0;
      sqlstm.sqharc[1] = (unsigned int   *)0;
      sqlstm.sqadto[1] = (unsigned short )0;
      sqlstm.sqtdso[1] = (unsigned short )0;
      sqlstm.sqhstv[2] = (         void  *)&nvzl_tmb->n_nemzetiseg;
      sqlstm.sqhstl[2] = (unsigned int  )4;
      sqlstm.sqhsts[2] = (         int  )sizeof(struct sql_s_nvzl_tip);
      sqlstm.sqindv[2] = (         void  *)0;
      sqlstm.sqinds[2] = (         int  )0;
      sqlstm.sqharm[2] = (unsigned int  )0;
      sqlstm.sqharc[2] = (unsigned int   *)0;
      sqlstm.sqadto[2] = (unsigned short )0;
      sqlstm.sqtdso[2] = (unsigned short )0;
      sqlstm.sqhstv[3] = (         void  *)&nvzl_tmb->n_csapat;
      sqlstm.sqhstl[3] = (unsigned int  )4;
      sqlstm.sqhsts[3] = (         int  )sizeof(struct sql_s_nvzl_tip);
      sqlstm.sqindv[3] = (         void  *)0;
      sqlstm.sqinds[3] = (         int  )0;
      sqlstm.sqharm[3] = (unsigned int  )0;
      sqlstm.sqharc[3] = (unsigned int   *)0;
      sqlstm.sqadto[3] = (unsigned short )0;
      sqlstm.sqtdso[3] = (unsigned short )0;
      sqlstm.sqhstv[4] = (         void  *)&nvzl_tmb->n_motor;
      sqlstm.sqhstl[4] = (unsigned int  )4;
      sqlstm.sqhsts[4] = (         int  )sizeof(struct sql_s_nvzl_tip);
      sqlstm.sqindv[4] = (         void  *)0;
      sqlstm.sqinds[4] = (         int  )0;
      sqlstm.sqharm[4] = (unsigned int  )0;
      sqlstm.sqharc[4] = (unsigned int   *)0;
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



      /* n_elhozott_nvzl altalaban megegyezik HOST_TMB_MER-rel, kiveve az      */
      /* utolso menetet, ekkor fontos, hogy csak a tenyleges szammal dolgozzon */
/*TRACE("\nn_nev_klcs(%d) -> %d darab nvzl klcs-ot talalt\n", n_nev_klcs, sqlca.sqlerrd[2]) ;*/
      n_elhozott_nvzl = sqlca.sqlerrd[2] - nOsszElhNvzl ; /* ??? a masodik nevnel 0 az eredmeny ! */

      /* Az ujonnan elhozott nagydij eredmenyek osszefesulese a nevezesi lista adatokkal */
      for ( nNvzlIdx = 0 ; nNvzlIdx < n_elhozott_nvzl ; nNvzlIdx++ )
      {
        /* A nevezesi lista kulcsonkent nyitom meg a kurzort,mert nem tudom      */
        /* elhozni a nevezesi lista kulcsot a lekerdezesbol                      */

/*TRACE("\nnvzl_klcs_tmb[%d]:%d , n_szez_tol:%d\n", nNvzlIdx, nvzl_klcs_tmb[nNvzlIdx], n_szez_tol) ;*/

        /* EXEC SQL OPEN nagyd_lekerd_kurz USING :nvzl_klcs_tmb[nNvzlIdx], :n_szez_tol ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.stmt = "";
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )731;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlstm.sqhstv[0] = (         void  *)&nvzl_klcs_tmb[nNvzlIdx];
        sqlstm.sqhstl[0] = (unsigned int  )4;
        sqlstm.sqhsts[0] = (         int  )0;
        sqlstm.sqindv[0] = (         void  *)0;
        sqlstm.sqinds[0] = (         int  )0;
        sqlstm.sqharm[0] = (unsigned int  )0;
        sqlstm.sqadto[0] = (unsigned short )0;
        sqlstm.sqtdso[0] = (unsigned short )0;
        sqlstm.sqhstv[1] = (         void  *)&n_szez_tol;
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



/*      nOsszElhNd = 0 ;*/

        /* EXEC SQL FETCH nagyd_lekerd_kurz INTO :nagyd_rekord ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )50;
        sqlstm.offset = (unsigned int  )754;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlstm.sqhstv[0] = (         void  *)&nagyd_rekord->n_nagydij_nev;
        sqlstm.sqhstl[0] = (unsigned int  )4;
        sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_nagyd_tip);
        sqlstm.sqindv[0] = (         void  *)0;
        sqlstm.sqinds[0] = (         int  )0;
        sqlstm.sqharm[0] = (unsigned int  )0;
        sqlstm.sqharc[0] = (unsigned int   *)0;
        sqlstm.sqadto[0] = (unsigned short )0;
        sqlstm.sqtdso[0] = (unsigned short )0;
        sqlstm.sqhstv[1] = (         void  *)nagyd_rekord->c_datum;
        sqlstm.sqhstl[1] = (unsigned int  )13;
        sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_nagyd_tip);
        sqlstm.sqindv[1] = (         void  *)0;
        sqlstm.sqinds[1] = (         int  )0;
        sqlstm.sqharm[1] = (unsigned int  )0;
        sqlstm.sqharc[1] = (unsigned int   *)0;
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



        nAktElhNd = sqlca.sqlerrd[2] /*- 0 (nOsszElhNd)*/ ;

        /* Csak, ha hozott el uj elso nagydij adatot, lehet veluk tovabb dolgozni */
        while ( nAktElhNd > 0 )
        {
          /* Ha a tenylegesen elhozott adatok (*pKiTmbMeret) + a potencialisak(nAktElhNd) */
          /* nem fernek be a lefoglalt teruletre (nKiTmbFogl)                             */
          if ( *pKiTmbMeret + nAktElhNd > nKiTmbFogl )
          {
            pLokHolVKiVLekEr = new sHolVKiVLekErTip[nKiTmbFogl+nAktElhNd] ;

            if ( pLokHolVKiVLekEr == NULL )
            {
              /* A kurzorok lezarasa */
              /* EXEC SQL CLOSE nagyd_lekerd_kurz ; */ 

{
              struct sqlexd sqlstm;

              sqlstm.sqlvsn = 10;
              sqlstm.arrsiz = 12;
              sqlstm.sqladtp = &sqladt;
              sqlstm.sqltdsp = &sqltds;
              sqlstm.iters = (unsigned int  )1;
              sqlstm.offset = (unsigned int  )777;
              sqlstm.cud = sqlcud0;
              sqlstm.sqlest = (unsigned char  *)&sqlca;
              sqlstm.sqlety = (unsigned short)0;
              sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
              if (sqlca.sqlcode < 0) goto sql_hiba;
}


              /* EXEC SQL CLOSE nvzl_lekerd_kurz  ; */ 

{
              struct sqlexd sqlstm;

              sqlstm.sqlvsn = 10;
              sqlstm.arrsiz = 12;
              sqlstm.sqladtp = &sqladt;
              sqlstm.sqltdsp = &sqltds;
              sqlstm.iters = (unsigned int  )1;
              sqlstm.offset = (unsigned int  )792;
              sqlstm.cud = sqlcud0;
              sqlstm.sqlest = (unsigned char  *)&sqlca;
              sqlstm.sqlety = (unsigned short)0;
              sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
              if (sqlca.sqlcode < 0) goto sql_hiba;
}



              return -1 ;
            }

            nKiTmbFogl = nKiTmbFogl + nAktElhNd ;

            /* Az eddig beirtak atmasolasa */
            memcpy( pLokHolVKiVLekEr, *ppHolVKiVLekEr, (*pKiTmbMeret)*sizeof(sHolVKiVLekErTip)) ;
          }

          /* Egy nNvzlIdx-hez tobb nNagydIdx is jo lehet ! Ezert megy mindig vegig. */
          for ( nNagydIdx = 0 ; nNagydIdx < nAktElhNd ; nNagydIdx++ )
          {
            /* Ideiglenesen hasznaljuk a 'celtombot', az index csak akkor megy tovabb, */
            /* ha a bejegyzes kesobbi, mint a datumfeltetel */
            sscanf( nagyd_rekord[nNagydIdx].c_datum,/*<- "11-MAR-90"*/
                    "%d-%d-%d",
                    &(pLokHolVKiVLekEr+*pKiTmbMeret)->uiEv,
                    &(pLokHolVKiVLekEr+*pKiTmbMeret)->uiHonap,
                    &(pLokHolVKiVLekEr+*pKiTmbMeret)->uiNap) ;

            (pLokHolVKiVLekEr+*pKiTmbMeret)->uiVersenyzo  = nvzl_tmb[nNvzlIdx].n_versenyzo  ;
            (pLokHolVKiVLekEr+*pKiTmbMeret)->uiNemzetiseg = nvzl_tmb[nNvzlIdx].n_nemzetiseg ;
            (pLokHolVKiVLekEr+*pKiTmbMeret)->uiCsapat     = nvzl_tmb[nNvzlIdx].n_csapat     ;
            (pLokHolVKiVLekEr+*pKiTmbMeret)->uiMotor      = nvzl_tmb[nNvzlIdx].n_motor      ;
            (pLokHolVKiVLekEr+*pKiTmbMeret)->uiNagydijNev = nagyd_rekord[nNagydIdx].n_nagydij_nev ;

/*
TRACE("\nV:%d, N:%d, C:%d, M:%d, E:%d, H:%d, N:%d",
       nvzl_tmb[nNvzlIdx].n_versenyzo,
       nvzl_tmb[nNvzlIdx].n_nemzetiseg,
       nvzl_tmb[nNvzlIdx].n_csapat,
       nvzl_tmb[nNvzlIdx].n_motor,
       (pLokHolVKiVLekEr+*pKiTmbMeret)->uiEv,
       (pLokHolVKiVLekEr+*pKiTmbMeret)->uiHonap,
       (pLokHolVKiVLekEr+*pKiTmbMeret)->uiNap
     ) ;
*/

            (*pKiTmbMeret)++ ;
          }

          /* Tulajdonkeppen itt veszi eszre, hogy elfogytak az adatok */
          /* break helyett a ciklusvaltozot allitja                   */
          if ( nAktElhNd == HOST_TMB_MER )
          {
            nOsszElhNd = sqlca.sqlerrd[2] ;

            /* EXEC SQL FETCH nagyd_lekerd_kurz INTO :nagyd_rekord ; */ 

{
            struct sqlexd sqlstm;

            sqlstm.sqlvsn = 10;
            sqlstm.arrsiz = 12;
            sqlstm.sqladtp = &sqladt;
            sqlstm.sqltdsp = &sqltds;
            sqlstm.iters = (unsigned int  )50;
            sqlstm.offset = (unsigned int  )807;
            sqlstm.cud = sqlcud0;
            sqlstm.sqlest = (unsigned char  *)&sqlca;
            sqlstm.sqlety = (unsigned short)0;
            sqlstm.sqhstv[0] = (         void  *)&nagyd_rekord->n_nagydij_nev;
            sqlstm.sqhstl[0] = (unsigned int  )4;
            sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_nagyd_tip);
            sqlstm.sqindv[0] = (         void  *)0;
            sqlstm.sqinds[0] = (         int  )0;
            sqlstm.sqharm[0] = (unsigned int  )0;
            sqlstm.sqharc[0] = (unsigned int   *)0;
            sqlstm.sqadto[0] = (unsigned short )0;
            sqlstm.sqtdso[0] = (unsigned short )0;
            sqlstm.sqhstv[1] = (         void  *)nagyd_rekord->c_datum;
            sqlstm.sqhstl[1] = (unsigned int  )13;
            sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_nagyd_tip);
            sqlstm.sqindv[1] = (         void  *)0;
            sqlstm.sqinds[1] = (         int  )0;
            sqlstm.sqharm[1] = (unsigned int  )0;
            sqlstm.sqharc[1] = (unsigned int   *)0;
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



            nAktElhNd = sqlca.sqlerrd[2] - nOsszElhNd ;
          }
          else
          {
            nAktElhNd = 0 ;
          }
        }

        delete [] *ppHolVKiVLekEr ;
        *ppHolVKiVLekEr = pLokHolVKiVLekEr ;
/*      nNagydLekMer = nNagydLekMer + nAktElhNd ;*/

        /* EXEC SQL CLOSE nagyd_lekerd_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )830;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}


/*      break ;*/
      }

      nOsszElhNvzl = nOsszElhNvzl + n_elhozott_nvzl ;

      /* Ha eppen az utolso - nem teljes tombmeretu adagot dolgozta fel */
      if ( n_elhozott_nvzl < HOST_TMB_MER )
      {
        /* EXEC SQL CLOSE nvzl_lekerd_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )845;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}


        break ;
      }
    }

    /* A ket OPEN kozti lezaras az ANSI kompatibilitas miatt kell */
    /* EXEC SQL CLOSE nvzl_lekerd_kurz ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 12;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )860;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    /* Uj kurzor eseten megint 0-rol kell kezdeni,kulonben n_elhozott_nvzl negativ is lehet */
    nOsszElhNvzl = 0 ;
  } /* for a bejovo nevkulcsok tombjen */

/*  delete [] pLokHolVKiVLekEr ;*/

  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL CLOSE nagyd_lekerd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )875;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}


  /* EXEC SQL CLOSE nvzl_lekerd_kurz  ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )890;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  return l_SQLCODE ;

sql_hiba :

  /* A feltetelt tartalmazo valtozotombok felszabaditasa */
/*  delete [] pLokHolVKiVLekEr ; ??? */

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL CLOSE nagyd_lekerd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )905;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}


  /* EXEC SQL CLOSE nvzl_lekerd_kurz  ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 12;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )920;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
} ;

/* , unsigned* pKiTmbMeret nem hasznalatos. Helyette : sPillAllLekEr.uiOsszErTmbMer */
long SQL_PillAll( unsigned          nSzezon,       eNevtipus enNevTip,
                  unsigned*         pNevKlcsTmb,   unsigned  nNevTmbMeret,
                  sPillAllLekErTip* pPillAllLekEr)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    /* A tarolt eljarasok bemeno adatai */
    unsigned n_szez_eve ;
    unsigned n_nevtipus ;

    unsigned nev_klcs_tmb[NEV_TMB_MER] ;
    unsigned n_nev_klcs_tmb_mer ;

    /* A tarolt eljarasok kimeno adatai */
    unsigned nev_hlyz_tmb[NEV_TMB_MER] ;
    unsigned szez_pnt_tmb[NEV_TMB_MER] ;

    /* Az osszes beszamitott eredmeny szama */
    unsigned n_ossz_er_tmb_mer ;

    /* Az eppen elhozott beszamitott eredmenyek szama */
    unsigned n_ki_besz_tab_mer ;

    /* Az osszes eredmeny mezoit tartalmazo tombok */
    unsigned nev_eredm_tmb[EREDM_TMB_MER] ;
    unsigned pont_tab[EREDM_TMB_MER]      ;
    unsigned helyezes_tab[EREDM_TMB_MER]  ;
    unsigned nvzl_kulcs_tab[EREDM_TMB_MER];
    unsigned nd_nev_tab[EREDM_TMB_MER]    ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  unsigned nElhSzama ;
  unsigned i ;
  unsigned nElhIdx   ;

  unsigned nHosszRegi, nHosszUj;

  unsigned nBeszEredmMer   ;
  unsigned nUtElhNevIdx    ;

  /* A futam eredmenyek elhozatalanal csak a szezonban */
  /* eredmenyt elert neveket itt tarolom               */
  unsigned  nLokNevTmbMeret ;
  unsigned* pLokNevKlcsTmb ;

  unsigned* pINevEredmTmb  ;
  unsigned* pIPontTmb      ;
  unsigned* pIHelyezesTmb  ;
  unsigned* pINvzlKulcsTmb ;
  unsigned* pINdNevTmb     ;

  unsigned* pIdNevKlcsTmb ;
  unsigned* pIdNevHlyzTmb ;
  unsigned* pIdSzezPntTmb ;

  unsigned  nIdIdx, nIdx1, nIdx2 ;

  BOOL bMindElhoz ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  n_szez_eve = nSzezon  ;
  n_nevtipus = enNevTip ;

  if ( enNevTip == eNemzNt || enNevTip == eCsapNt || enNevTip == eMotorNt )
  {
    /* EXEC SQL EXECUTE
      BEGIN

        /o a nevek (csapat/versenyzo) szerinti eredmenyeket osszeallito szkript felhivasa o/
        szez_all.szez_pill_all_cs( :n_szez_eve, :n_nevtipus) ;

      END ;
    END-EXEC ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 12;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "begin szez_all . szez_pill_all_cs ( :n_szez_eve , :n_nevt\
ipus ) ; END ;";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )935;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_szez_eve;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_nevtipus;
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


  }
  else
  {
    /* EXEC SQL EXECUTE
      BEGIN

        /o a nevek (csapat/versenyzo) szerinti eredmenyeket osszeallito szkript felhivasa o/
        szez_all.szez_pill_all_v( :n_szez_eve) ;

      END ;
    END-EXEC ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 12;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "begin szez_all . szez_pill_all_v ( :n_szez_eve ) ; END ;";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )958;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_szez_eve;
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


  }

  /* ----------------------------------------------------------------------- */

  /* Vannak megadva elhozando nevek ? */
  if ( nNevTmbMeret > 0 )
  {
    bMindElhoz = FALSE ;

    /* Hany menetben tudjuk NEV_TMB_MER adagokban elhozni az eredmenyt ? */
    nElhSzama = nNevTmbMeret / NEV_TMB_MER + 1 ;
    nElhIdx = 0 ;

    for ( i = 0 ; i < nElhSzama ; i++ )
    {
      /* Nem mindig teljes meretu tombot kell mozgatni */
      if ( nNevTmbMeret > NEV_TMB_MER + nElhIdx)
      {
        n_nev_klcs_tmb_mer =  NEV_TMB_MER ;
      }
      else
      {
        n_nev_klcs_tmb_mer = nNevTmbMeret - nElhIdx ;
      }

      memcpy( nev_klcs_tmb, pNevKlcsTmb + nElhIdx, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
/*
for ( unsigned b = 0 ; b < n_nev_klcs_tmb_mer ; b++ )
{
  TRACE("\nnev_klcs_tmb[%d]:%d", b, nev_klcs_tmb[b]) ;
}
*/
      /* EXEC SQL EXECUTE
        BEGIN

          /o Az adott nevek hanyadikak lettek, hany ponttal o/
          szez_all.szez_eredm_elhoz( :nev_klcs_tmb, :n_nev_klcs_tmb_mer, :nev_hlyz_tmb, :szez_pnt_tmb) ;

        END ;
      END-EXEC ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 12;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.stmt = "begin szez_all . szez_eredm_elhoz ( :nev_klcs_tmb , :n_\
nev_klcs_tmb_mer , :nev_hlyz_tmb , :szez_pnt_tmb ) ; END ;";
      sqlstm.iters = (unsigned int  )1;
      sqlstm.offset = (unsigned int  )977;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)nev_klcs_tmb;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )4;
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )20;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&n_nev_klcs_tmb_mer;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )0;
      sqlstm.sqindv[1] = (         void  *)0;
      sqlstm.sqinds[1] = (         int  )0;
      sqlstm.sqharm[1] = (unsigned int  )0;
      sqlstm.sqadto[1] = (unsigned short )0;
      sqlstm.sqtdso[1] = (unsigned short )0;
      sqlstm.sqhstv[2] = (         void  *)nev_hlyz_tmb;
      sqlstm.sqhstl[2] = (unsigned int  )4;
      sqlstm.sqhsts[2] = (         int  )4;
      sqlstm.sqindv[2] = (         void  *)0;
      sqlstm.sqinds[2] = (         int  )0;
      sqlstm.sqharm[2] = (unsigned int  )20;
      sqlstm.sqharc[2] = (unsigned int   *)0;
      sqlstm.sqadto[2] = (unsigned short )0;
      sqlstm.sqtdso[2] = (unsigned short )0;
      sqlstm.sqhstv[3] = (         void  *)szez_pnt_tmb;
      sqlstm.sqhstl[3] = (unsigned int  )4;
      sqlstm.sqhsts[3] = (         int  )4;
      sqlstm.sqindv[3] = (         void  *)0;
      sqlstm.sqinds[3] = (         int  )0;
      sqlstm.sqharm[3] = (unsigned int  )20;
      sqlstm.sqharc[3] = (unsigned int   *)0;
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


/*
for ( unsigned a = 0 ; a < n_nev_klcs_tmb_mer ; a++ )
{
  TRACE("\na:%d nev_klcs_tmb:%d nev_hlyz_tmb:%d szez_pnt_tmb:%d", a, nev_klcs_tmb[a], nev_hlyz_tmb[a], szez_pnt_tmb[a]) ;
}
*/
      /* Helyfoglalas a kimenoadatoknak: Nem feltetlen ert el minden nev eredmenyt */
      /* A kivalasztottak kozul ezek ertek el eredmenyt */
      pIdNevKlcsTmb = new unsigned [pPillAllLekEr->uiSzezErTmbMeret+n_nev_klcs_tmb_mer] ;
      if ( pIdNevKlcsTmb == NULL )
      {
        return -1 ;
      }

      /* A szezon vegen elert helyezes  */
      pIdNevHlyzTmb = new unsigned [pPillAllLekEr->uiSzezErTmbMeret+n_nev_klcs_tmb_mer] ;
      if ( pIdNevHlyzTmb == NULL )
      {
        delete [] pIdNevKlcsTmb ;
        return -1 ;
      }

      /* A figyelembe vett pontok a szezon vegen */
      pIdSzezPntTmb = new unsigned [pPillAllLekEr->uiSzezErTmbMeret+n_nev_klcs_tmb_mer] ;
      if ( pIdSzezPntTmb == NULL )
      {
        delete [] pIdNevKlcsTmb ;
        delete [] pIdNevHlyzTmb ;
        return -1 ;
      }

      if ( i == 0 )
      {
        memcpy( pIdNevKlcsTmb, nev_klcs_tmb, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
        memcpy( pIdNevHlyzTmb, nev_hlyz_tmb, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
        memcpy( pIdSzezPntTmb, szez_pnt_tmb, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
      }
      else
      {
        nIdIdx = nIdx1 = nIdx2 = 0 ;

        while ( TRUE )
        {
          if ( nIdx1 < pPillAllLekEr->uiSzezErTmbMeret )
          {
            if ( nIdx2 < n_nev_klcs_tmb_mer )
            {
              /* Mindkettobol van meg */
              if ( pPillAllLekEr->pNevHlyzTmb[nIdx1] < nev_hlyz_tmb[nIdx2] )
              {
                pIdNevKlcsTmb[nIdIdx] = pPillAllLekEr->pNevKlcsTmb[nIdx1] ;
                pIdNevHlyzTmb[nIdIdx] = pPillAllLekEr->pNevHlyzTmb[nIdx1] ;
                pIdSzezPntTmb[nIdIdx] = pPillAllLekEr->pSzezPntTmb[nIdx1] ;

                nIdx1++ ;
              }
              else
              {
                pIdNevKlcsTmb[nIdIdx] = nev_klcs_tmb[nIdx2] ;
                pIdNevHlyzTmb[nIdIdx] = nev_hlyz_tmb[nIdx2] ;
                pIdSzezPntTmb[nIdIdx] = szez_pnt_tmb[nIdx2] ;

                nIdx2++ ;
              }
            }
            else
            {
              /* nev_hlyz_tmb elfogyott */
              pIdNevKlcsTmb[nIdIdx] = pPillAllLekEr->pNevKlcsTmb[nIdx1] ;
              pIdNevHlyzTmb[nIdIdx] = pPillAllLekEr->pNevHlyzTmb[nIdx1] ;
              pIdSzezPntTmb[nIdIdx] = pPillAllLekEr->pSzezPntTmb[nIdx1] ;

              nIdx1++ ;
            }
          }
          else
          {
            /* pPillAllLekEr->pNevHlyzTmb elfogyott */
            if ( nIdx2 < n_nev_klcs_tmb_mer )
            {
              pIdNevKlcsTmb[nIdIdx] = nev_klcs_tmb[nIdx2] ;
              pIdNevHlyzTmb[nIdIdx] = nev_hlyz_tmb[nIdx2] ;
              pIdSzezPntTmb[nIdIdx] = szez_pnt_tmb[nIdx2] ;

              nIdx2++ ;
            }
            else
            {
              /* Mindketto elfogyott -> KESZ, VEGE */
              break ;
            }
          }

          nIdIdx++ ;
        }
      }

      delete [] pPillAllLekEr->pNevKlcsTmb ;
      delete [] pPillAllLekEr->pNevHlyzTmb ;
      delete [] pPillAllLekEr->pSzezPntTmb ;

      pPillAllLekEr->pNevKlcsTmb = pIdNevKlcsTmb ;
      pPillAllLekEr->pNevHlyzTmb = pIdNevHlyzTmb ;
      pPillAllLekEr->pSzezPntTmb = pIdSzezPntTmb ;

      nElhIdx += NEV_TMB_MER ;
      pPillAllLekEr->uiSzezErTmbMeret += n_nev_klcs_tmb_mer ;
    }

/*
for ( unsigned ui = 0 ; ui < 18 ; ui++ )
{
  TRACE("\nui:%d nev_klcs_tmb:%d nev_hlyz_tmb:%d szez_pnt_tmb:%d",
        ui,
        pPillAllLekEr->pNevKlcsTmb[ui],
        pPillAllLekEr->pNevHlyzTmb[ui],
        pPillAllLekEr->pSzezPntTmb[ui]) ;
}
*/
  }
  else /* Minden eredmenyt elert nevet el kell hozni */
  {
    /* A feltetelul szolgalo valtozo elallitodik, es kesobb kell */
    bMindElhoz = TRUE ;

    /* Az adott szezonban eredmenyt elert nevek szama                  */
    /* Nem csak a feluleten kivalasztottake:minden nevet el kell hozni */
    /* EXEC SQL EXECUTE
      BEGIN

        :n_nev_klcs_tmb_mer := szez_all.n_max_szez_idx ;

      END ;
    END-EXEC ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 12;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "begin :n_nev_klcs_tmb_mer := szez_all . n_max_szez_idx ; \
END ;";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )1008;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nev_klcs_tmb_mer;
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



    if ( n_nev_klcs_tmb_mer > 0 )
    {
      pPillAllLekEr->uiSzezErTmbMeret = n_nev_klcs_tmb_mer ;

      /* Helyfoglalas a kimenoadatoknak */
      pPillAllLekEr->pNevKlcsTmb  = new unsigned [n_nev_klcs_tmb_mer] ;
      if ( pPillAllLekEr->pNevKlcsTmb  == NULL )
      {
        return -1 ;
      }

      /* A szezon vegen elert helyezes  */
      pPillAllLekEr->pNevHlyzTmb = new unsigned [n_nev_klcs_tmb_mer] ;
      if ( pPillAllLekEr->pNevHlyzTmb == NULL )
      {
        delete [] pPillAllLekEr->pNevKlcsTmb  ;
        return -1 ;
      }

      /* A figyelembe vett pontok a szezon vegen */
      pPillAllLekEr->pSzezPntTmb = new unsigned [n_nev_klcs_tmb_mer] ;
      if ( pPillAllLekEr->pSzezPntTmb == NULL )
      {
        delete [] pPillAllLekEr->pNevKlcsTmb ;
        delete [] pPillAllLekEr->pNevHlyzTmb ;
        return -1 ;
      }

      /* Hany menetben tudjuk NEV_TMB_MER adagokban elhozni az eredmenyt ? */
      nElhSzama = pPillAllLekEr->uiSzezErTmbMeret / NEV_TMB_MER + 1 ;
      nElhIdx = 0 ;

      for ( i = 0 ; i < nElhSzama ; i++ )
      {
        /* Ez jelzi, hogy a neveket is el kell hozni */
        n_nev_klcs_tmb_mer = 0 ;

        /* EXEC SQL EXECUTE
          BEGIN

            /o Az adott nevek hanyadikak lettek, hany ponttal o/
            szez_all.szez_eredm_elhoz( :nev_klcs_tmb, :n_nev_klcs_tmb_mer, :nev_hlyz_tmb, :szez_pnt_tmb) ;

          END ;
        END-EXEC ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.stmt = "begin szez_all . szez_eredm_elhoz ( :nev_klcs_tmb , :\
n_nev_klcs_tmb_mer , :nev_hlyz_tmb , :szez_pnt_tmb ) ; END ;";
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )1027;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlstm.sqhstv[0] = (         void  *)nev_klcs_tmb;
        sqlstm.sqhstl[0] = (unsigned int  )4;
        sqlstm.sqhsts[0] = (         int  )4;
        sqlstm.sqindv[0] = (         void  *)0;
        sqlstm.sqinds[0] = (         int  )0;
        sqlstm.sqharm[0] = (unsigned int  )20;
        sqlstm.sqharc[0] = (unsigned int   *)0;
        sqlstm.sqadto[0] = (unsigned short )0;
        sqlstm.sqtdso[0] = (unsigned short )0;
        sqlstm.sqhstv[1] = (         void  *)&n_nev_klcs_tmb_mer;
        sqlstm.sqhstl[1] = (unsigned int  )4;
        sqlstm.sqhsts[1] = (         int  )0;
        sqlstm.sqindv[1] = (         void  *)0;
        sqlstm.sqinds[1] = (         int  )0;
        sqlstm.sqharm[1] = (unsigned int  )0;
        sqlstm.sqadto[1] = (unsigned short )0;
        sqlstm.sqtdso[1] = (unsigned short )0;
        sqlstm.sqhstv[2] = (         void  *)nev_hlyz_tmb;
        sqlstm.sqhstl[2] = (unsigned int  )4;
        sqlstm.sqhsts[2] = (         int  )4;
        sqlstm.sqindv[2] = (         void  *)0;
        sqlstm.sqinds[2] = (         int  )0;
        sqlstm.sqharm[2] = (unsigned int  )20;
        sqlstm.sqharc[2] = (unsigned int   *)0;
        sqlstm.sqadto[2] = (unsigned short )0;
        sqlstm.sqtdso[2] = (unsigned short )0;
        sqlstm.sqhstv[3] = (         void  *)szez_pnt_tmb;
        sqlstm.sqhstl[3] = (unsigned int  )4;
        sqlstm.sqhsts[3] = (         int  )4;
        sqlstm.sqindv[3] = (         void  *)0;
        sqlstm.sqinds[3] = (         int  )0;
        sqlstm.sqharm[3] = (unsigned int  )20;
        sqlstm.sqharc[3] = (unsigned int   *)0;
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



        /* Nem mindig teljes meretu tombot kell mozgatni */
        if ( pPillAllLekEr->uiSzezErTmbMeret - nElhIdx > NEV_TMB_MER )
        {
          n_nev_klcs_tmb_mer =  NEV_TMB_MER ;
        }
        else
        {
          n_nev_klcs_tmb_mer = pPillAllLekEr->uiSzezErTmbMeret - nElhIdx ;
        }

        memcpy( pPillAllLekEr->pNevKlcsTmb + nElhIdx, nev_klcs_tmb, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
        memcpy( pPillAllLekEr->pNevHlyzTmb + nElhIdx, nev_hlyz_tmb, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
        memcpy( pPillAllLekEr->pSzezPntTmb + nElhIdx, szez_pnt_tmb, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;

        nElhIdx += n_nev_klcs_tmb_mer ;
      }

/*      pPillAllLekEr->uiSzezErTmbMeret = nElhIdx ; folosleges l. fent */
    }
/*
for ( unsigned ui = 0 ; ui < pPillAllLekEr->uiSzezErTmbMeret ; ui++ )
{
  TRACE("\nui:%d nev_klcs_tmb:%d nev_hlyz_tmb:%d szez_pnt_tmb:%d",
        ui,
        pPillAllLekEr->pNevKlcsTmb[ui],
        pPillAllLekEr->pNevHlyzTmb[ui],
        pPillAllLekEr->pSzezPntTmb[ui]) ;
}
*/
  }

  /* Nev kulcsok szerint rendezve van az eredmeny a szez_all package-ben,                */
  /* ha a parameterkent atadott nev kulcs tomb rendezett, nem kell minden nevre az       */
  /* eredmenyeket elolrol atnezni, meg lehet jegyezni az utolsonak vett eredmeny indexet */

  if ( bMindElhoz == TRUE )
  {
    /* besz_eredm_elhoz fuggveny kimenoadatai merete */
    /* EXEC SQL EXECUTE
      BEGIN

        :n_ossz_er_tmb_mer := szez_all.n_max_eredm_idx ;

      END ;
    END-EXEC ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 12;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "begin :n_ossz_er_tmb_mer := szez_all . n_max_eredm_idx ; \
END ;";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )1058;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_ossz_er_tmb_mer;
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



    pPillAllLekEr->uiOsszErTmbMer = n_ossz_er_tmb_mer ;

    if ( n_ossz_er_tmb_mer > 0 )
    {
      /* A beszamitott vilagbajnoki eredmenyek */
      pPillAllLekEr->pNevEredmTmb  = new unsigned [n_ossz_er_tmb_mer] ;
      pPillAllLekEr->pPontTmb      = new unsigned [n_ossz_er_tmb_mer] ;
      pPillAllLekEr->pHelyezesTmb  = new unsigned [n_ossz_er_tmb_mer] ;
      pPillAllLekEr->pNvzlKulcsTmb = new unsigned [n_ossz_er_tmb_mer] ;
      pPillAllLekEr->pNdNevTmb     = new unsigned [n_ossz_er_tmb_mer] ;

      if ( pPillAllLekEr->pNevEredmTmb  == NULL ||
           pPillAllLekEr->pPontTmb      == NULL ||
           pPillAllLekEr->pHelyezesTmb  == NULL ||
           pPillAllLekEr->pNvzlKulcsTmb == NULL ||
           pPillAllLekEr->pNdNevTmb     == NULL )
      {
        delete [] pPillAllLekEr->pNevEredmTmb  ;
        delete [] pPillAllLekEr->pPontTmb      ;
        delete [] pPillAllLekEr->pHelyezesTmb  ;
        delete [] pPillAllLekEr->pNvzlKulcsTmb ;
        delete [] pPillAllLekEr->pNdNevTmb     ;

        pPillAllLekEr->uiOsszErTmbMer = 0 ;

        return -1 ;
      }

      nElhSzama = pPillAllLekEr->uiOsszErTmbMer / EREDM_TMB_MER + 1 ;
      nElhIdx = 0 ;

      for ( i = 0 ; i < nElhSzama ; i++ )
      {
        /* Nem mindig teljes meretu tombot kell mozgatni */
        if ( pPillAllLekEr->uiOsszErTmbMer - nElhIdx > EREDM_TMB_MER )
        {
          n_ossz_er_tmb_mer = EREDM_TMB_MER ;
        }
        else
        {
          n_ossz_er_tmb_mer = pPillAllLekEr->uiOsszErTmbMer - nElhIdx ;
        }

        /* EXEC SQL EXECUTE
          BEGIN

        /o Az osszes beszamitott eredmeny elhozasa         o/
        szez_all.besz_eredm_elhoz( :nev_klcs_tmb,  0,
                                   :nev_eredm_tmb, :pont_tab,
                                   :helyezes_tab,  :nvzl_kulcs_tab,
                                   :nd_nev_tab,    :n_ki_besz_tab_mer) ;

          END ;
        END-EXEC ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 12;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.stmt = "begin szez_all . besz_eredm_elhoz ( :nev_klcs_tmb , 0\
 , :nev_eredm_tmb , :pont_tab , :helyezes_tab , :nvzl_kulcs_tab , :nd_nev_tab \
, :n_ki_besz_tab_mer ) ; END ;";
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )1077;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlstm.sqhstv[0] = (         void  *)nev_klcs_tmb;
        sqlstm.sqhstl[0] = (unsigned int  )4;
        sqlstm.sqhsts[0] = (         int  )4;
        sqlstm.sqindv[0] = (         void  *)0;
        sqlstm.sqinds[0] = (         int  )0;
        sqlstm.sqharm[0] = (unsigned int  )20;
        sqlstm.sqharc[0] = (unsigned int   *)0;
        sqlstm.sqadto[0] = (unsigned short )0;
        sqlstm.sqtdso[0] = (unsigned short )0;
        sqlstm.sqhstv[1] = (         void  *)nev_eredm_tmb;
        sqlstm.sqhstl[1] = (unsigned int  )4;
        sqlstm.sqhsts[1] = (         int  )4;
        sqlstm.sqindv[1] = (         void  *)0;
        sqlstm.sqinds[1] = (         int  )0;
        sqlstm.sqharm[1] = (unsigned int  )50;
        sqlstm.sqharc[1] = (unsigned int   *)0;
        sqlstm.sqadto[1] = (unsigned short )0;
        sqlstm.sqtdso[1] = (unsigned short )0;
        sqlstm.sqhstv[2] = (         void  *)pont_tab;
        sqlstm.sqhstl[2] = (unsigned int  )4;
        sqlstm.sqhsts[2] = (         int  )4;
        sqlstm.sqindv[2] = (         void  *)0;
        sqlstm.sqinds[2] = (         int  )0;
        sqlstm.sqharm[2] = (unsigned int  )50;
        sqlstm.sqharc[2] = (unsigned int   *)0;
        sqlstm.sqadto[2] = (unsigned short )0;
        sqlstm.sqtdso[2] = (unsigned short )0;
        sqlstm.sqhstv[3] = (         void  *)helyezes_tab;
        sqlstm.sqhstl[3] = (unsigned int  )4;
        sqlstm.sqhsts[3] = (         int  )4;
        sqlstm.sqindv[3] = (         void  *)0;
        sqlstm.sqinds[3] = (         int  )0;
        sqlstm.sqharm[3] = (unsigned int  )50;
        sqlstm.sqharc[3] = (unsigned int   *)0;
        sqlstm.sqadto[3] = (unsigned short )0;
        sqlstm.sqtdso[3] = (unsigned short )0;
        sqlstm.sqhstv[4] = (         void  *)nvzl_kulcs_tab;
        sqlstm.sqhstl[4] = (unsigned int  )4;
        sqlstm.sqhsts[4] = (         int  )4;
        sqlstm.sqindv[4] = (         void  *)0;
        sqlstm.sqinds[4] = (         int  )0;
        sqlstm.sqharm[4] = (unsigned int  )50;
        sqlstm.sqharc[4] = (unsigned int   *)0;
        sqlstm.sqadto[4] = (unsigned short )0;
        sqlstm.sqtdso[4] = (unsigned short )0;
        sqlstm.sqhstv[5] = (         void  *)nd_nev_tab;
        sqlstm.sqhstl[5] = (unsigned int  )4;
        sqlstm.sqhsts[5] = (         int  )4;
        sqlstm.sqindv[5] = (         void  *)0;
        sqlstm.sqinds[5] = (         int  )0;
        sqlstm.sqharm[5] = (unsigned int  )50;
        sqlstm.sqharc[5] = (unsigned int   *)0;
        sqlstm.sqadto[5] = (unsigned short )0;
        sqlstm.sqtdso[5] = (unsigned short )0;
        sqlstm.sqhstv[6] = (         void  *)&n_ki_besz_tab_mer;
        sqlstm.sqhstl[6] = (unsigned int  )4;
        sqlstm.sqhsts[6] = (         int  )0;
        sqlstm.sqindv[6] = (         void  *)0;
        sqlstm.sqinds[6] = (         int  )0;
        sqlstm.sqharm[6] = (unsigned int  )0;
        sqlstm.sqadto[6] = (unsigned short )0;
        sqlstm.sqtdso[6] = (unsigned short )0;
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



        memcpy( pPillAllLekEr->pNevEredmTmb + nElhIdx,  nev_eredm_tmb,  n_ossz_er_tmb_mer*sizeof(unsigned)) ;
        memcpy( pPillAllLekEr->pPontTmb + nElhIdx,      pont_tab,       n_ossz_er_tmb_mer*sizeof(unsigned)) ;
        memcpy( pPillAllLekEr->pHelyezesTmb + nElhIdx,  helyezes_tab,   n_ossz_er_tmb_mer*sizeof(unsigned)) ;
        memcpy( pPillAllLekEr->pNvzlKulcsTmb + nElhIdx, nvzl_kulcs_tab, n_ossz_er_tmb_mer*sizeof(unsigned)) ;
        memcpy( pPillAllLekEr->pNdNevTmb + nElhIdx,     nd_nev_tab,     n_ossz_er_tmb_mer*sizeof(unsigned)) ;

        nElhIdx += n_ossz_er_tmb_mer ;
      }
    } /* Csak ha van mit elhozni */
  }
  else
  {
    /* Nem lehet tudni, a kivalasztott nevekhez hany eredmeny tartozik ! */
    /* A szezon eredmenyek elhozasa kapcsan kiderult, hany nevhez van eredmeny */
    nLokNevTmbMeret = pPillAllLekEr->uiSzezErTmbMeret ;
    pLokNevKlcsTmb = new unsigned [nLokNevTmbMeret] ;
    if ( pLokNevKlcsTmb == NULL )
    {
      return -1 ;
    }

    memcpy( pLokNevKlcsTmb, pPillAllLekEr->pNevKlcsTmb, pPillAllLekEr->uiSzezErTmbMeret*sizeof(unsigned)) ;

    /* A kivalasztott nev kulcsok sorbarendezese, mivel az elhozott nev kulcsok   */
    /* is sorba vannak rendezve. Igy egyszeruen meg lehet a kimeno nev kulcs tomb */
    /* ismereteben, hogy a bemeno nev kulcs tombot mely kulcstol kell ismetelni.  */
    qsort( (void*)pLokNevKlcsTmb, nLokNevTmbMeret, sizeof(unsigned), KulcsOsszeh) ;

    nUtElhNevIdx  = 0 ;
    nBeszEredmMer = 0 ;

    if ( nLokNevTmbMeret - nUtElhNevIdx > NEV_TMB_MER )
    {
      memcpy( nev_klcs_tmb, pLokNevKlcsTmb, NEV_TMB_MER*sizeof(unsigned)) ;
      n_nev_klcs_tmb_mer = NEV_TMB_MER ;
/*      nUtElhNevIdx       = NEV_TMB_MER -1 ;*/
    }
    else
    {
      memcpy( nev_klcs_tmb, pLokNevKlcsTmb, nNevTmbMeret*sizeof(unsigned)) ;
      n_nev_klcs_tmb_mer = nLokNevTmbMeret - nBeszEredmMer ;
/*      nUtElhNevIdx       = n_nev_klcs_tmb_mer - 1 ;*/
    }

    /* Hogy belefusson a ciklusba */
    n_ki_besz_tab_mer = 1 ;
/*                                                \/ ??? */
    while ( n_ki_besz_tab_mer > 0 && nUtElhNevIdx < nNevTmbMeret )
    {
/*
for ( unsigned c = 0 ; c < n_nev_klcs_tmb_mer ; c++ )
{
TRACE( "\nidx:%d. nev_klcs_tmb:%d",
       c,
       nev_klcs_tmb[c]) ;
}
*/
      /* EXEC SQL EXECUTE
        BEGIN

          szez_all.besz_eredm_elhoz( :nev_klcs_tmb,  :n_nev_klcs_tmb_mer,
                                     :nev_eredm_tmb, :pont_tab,
                                     :helyezes_tab,  :nvzl_kulcs_tab,
                                     :nd_nev_tab,    :n_ki_besz_tab_mer) ;

        END ;
      END-EXEC ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 12;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.stmt = "begin szez_all . besz_eredm_elhoz ( :nev_klcs_tmb , :n_\
nev_klcs_tmb_mer , :nev_eredm_tmb , :pont_tab , :helyezes_tab , :nvzl_kulcs_ta\
b , :nd_nev_tab , :n_ki_besz_tab_mer ) ; END ;";
      sqlstm.iters = (unsigned int  )1;
      sqlstm.offset = (unsigned int  )1120;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)nev_klcs_tmb;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )4;
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )20;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&n_nev_klcs_tmb_mer;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )0;
      sqlstm.sqindv[1] = (         void  *)0;
      sqlstm.sqinds[1] = (         int  )0;
      sqlstm.sqharm[1] = (unsigned int  )0;
      sqlstm.sqadto[1] = (unsigned short )0;
      sqlstm.sqtdso[1] = (unsigned short )0;
      sqlstm.sqhstv[2] = (         void  *)nev_eredm_tmb;
      sqlstm.sqhstl[2] = (unsigned int  )4;
      sqlstm.sqhsts[2] = (         int  )4;
      sqlstm.sqindv[2] = (         void  *)0;
      sqlstm.sqinds[2] = (         int  )0;
      sqlstm.sqharm[2] = (unsigned int  )50;
      sqlstm.sqharc[2] = (unsigned int   *)0;
      sqlstm.sqadto[2] = (unsigned short )0;
      sqlstm.sqtdso[2] = (unsigned short )0;
      sqlstm.sqhstv[3] = (         void  *)pont_tab;
      sqlstm.sqhstl[3] = (unsigned int  )4;
      sqlstm.sqhsts[3] = (         int  )4;
      sqlstm.sqindv[3] = (         void  *)0;
      sqlstm.sqinds[3] = (         int  )0;
      sqlstm.sqharm[3] = (unsigned int  )50;
      sqlstm.sqharc[3] = (unsigned int   *)0;
      sqlstm.sqadto[3] = (unsigned short )0;
      sqlstm.sqtdso[3] = (unsigned short )0;
      sqlstm.sqhstv[4] = (         void  *)helyezes_tab;
      sqlstm.sqhstl[4] = (unsigned int  )4;
      sqlstm.sqhsts[4] = (         int  )4;
      sqlstm.sqindv[4] = (         void  *)0;
      sqlstm.sqinds[4] = (         int  )0;
      sqlstm.sqharm[4] = (unsigned int  )50;
      sqlstm.sqharc[4] = (unsigned int   *)0;
      sqlstm.sqadto[4] = (unsigned short )0;
      sqlstm.sqtdso[4] = (unsigned short )0;
      sqlstm.sqhstv[5] = (         void  *)nvzl_kulcs_tab;
      sqlstm.sqhstl[5] = (unsigned int  )4;
      sqlstm.sqhsts[5] = (         int  )4;
      sqlstm.sqindv[5] = (         void  *)0;
      sqlstm.sqinds[5] = (         int  )0;
      sqlstm.sqharm[5] = (unsigned int  )50;
      sqlstm.sqharc[5] = (unsigned int   *)0;
      sqlstm.sqadto[5] = (unsigned short )0;
      sqlstm.sqtdso[5] = (unsigned short )0;
      sqlstm.sqhstv[6] = (         void  *)nd_nev_tab;
      sqlstm.sqhstl[6] = (unsigned int  )4;
      sqlstm.sqhsts[6] = (         int  )4;
      sqlstm.sqindv[6] = (         void  *)0;
      sqlstm.sqinds[6] = (         int  )0;
      sqlstm.sqharm[6] = (unsigned int  )50;
      sqlstm.sqharc[6] = (unsigned int   *)0;
      sqlstm.sqadto[6] = (unsigned short )0;
      sqlstm.sqtdso[6] = (unsigned short )0;
      sqlstm.sqhstv[7] = (         void  *)&n_ki_besz_tab_mer;
      sqlstm.sqhstl[7] = (unsigned int  )4;
      sqlstm.sqhsts[7] = (         int  )0;
      sqlstm.sqindv[7] = (         void  *)0;
      sqlstm.sqinds[7] = (         int  )0;
      sqlstm.sqharm[7] = (unsigned int  )0;
      sqlstm.sqadto[7] = (unsigned short )0;
      sqlstm.sqtdso[7] = (unsigned short )0;
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


/*
TRACE( "\nbesz_eredm_elhoz--------------------------------\n") ;

for ( unsigned u = 0 ; u < n_ki_besz_tab_mer ; u++ )
{
TRACE( "\nidx:%d. nev:%d  pont:%d helyz:%d nvzl:%d nd:%d",
       u,
       nev_eredm_tmb[u],
       pont_tab[u],
       helyezes_tab[u],
       nvzl_kulcs_tab[u],
       nd_nev_tab[u]) ;
}
*/
      if ( n_ki_besz_tab_mer > 0 )
      {
        nBeszEredmMer = nBeszEredmMer + n_ki_besz_tab_mer ;

        /* Helyfoglalas az ujonnan elhozottakkal novelt eredmenytombnek */
        pINevEredmTmb  = new unsigned [nBeszEredmMer] ;
        pIPontTmb      = new unsigned [nBeszEredmMer] ;
        pIHelyezesTmb  = new unsigned [nBeszEredmMer] ;
        pINvzlKulcsTmb = new unsigned [nBeszEredmMer] ;
        pINdNevTmb     = new unsigned [nBeszEredmMer] ;

        if ( pINevEredmTmb  == NULL || pIPontTmb == NULL || pIHelyezesTmb  == NULL ||
             pINvzlKulcsTmb == NULL || pINdNevTmb == NULL )
        {
          delete [] pLokNevKlcsTmb ;

          delete [] pINevEredmTmb  ;
          delete [] pIPontTmb      ;
          delete [] pIHelyezesTmb  ;
          delete [] pINvzlKulcsTmb ;
          delete [] pINdNevTmb     ;

          return -1 ;
        }

        /* A regi eredmeny es az ujonnan elhozottak osszemasolasa */   
        i          = pPillAllLekEr->uiOsszErTmbMer  ;
        nHosszRegi = i*sizeof(unsigned) ; /*(nBeszEredmMer - n_ki_besz_tab_mer)*/
        nHosszUj   = n_ki_besz_tab_mer*sizeof(unsigned) ;

        memcpy( pINevEredmTmb,    pPillAllLekEr->pNevEredmTmb,  nHosszRegi) ;
        memcpy( pINevEredmTmb+i,  nev_eredm_tmb,                 nHosszUj) ;

        memcpy( pIPontTmb,        pPillAllLekEr->pPontTmb,      nHosszRegi) ;
        memcpy( pIPontTmb+i,      pont_tab,                     nHosszUj) ;

        memcpy( pIHelyezesTmb,    pPillAllLekEr->pHelyezesTmb,  nHosszRegi) ;
        memcpy( pIHelyezesTmb+i,  helyezes_tab,                 nHosszUj) ;

        memcpy( pINvzlKulcsTmb,   pPillAllLekEr->pNvzlKulcsTmb, nHosszRegi) ;
        memcpy( pINvzlKulcsTmb+i, nvzl_kulcs_tab,               nHosszUj) ;

        memcpy( pINdNevTmb,       pPillAllLekEr->pNdNevTmb,     nHosszRegi) ;
        memcpy( pINdNevTmb+i,     nd_nev_tab,                   nHosszUj) ;

        delete [] pPillAllLekEr->pNevEredmTmb  ;
        delete [] pPillAllLekEr->pPontTmb      ;
        delete [] pPillAllLekEr->pHelyezesTmb  ;
        delete [] pPillAllLekEr->pNvzlKulcsTmb ;
        delete [] pPillAllLekEr->pNdNevTmb     ;

        pPillAllLekEr->pNevEredmTmb  = pINevEredmTmb  ;
        pPillAllLekEr->pPontTmb      = pIPontTmb      ;
        pPillAllLekEr->pHelyezesTmb  = pIHelyezesTmb  ;
        pPillAllLekEr->pNvzlKulcsTmb = pINvzlKulcsTmb ;
        pPillAllLekEr->pNdNevTmb     = pINdNevTmb     ;

        /* A beszamitott eredmenyek szamanak beallitasa */
        pPillAllLekEr->uiOsszErTmbMer = nBeszEredmMer ;

        /* A nev_klcs_tmb eloremozgatasa ahhoz a nevhez, amelyhez az utolso */
        /* elhozott besz.er.tartozik Persze lehet az is, hogy meg az elso   */
        /* nevhez sem hozott el mindent akkor az elso marad az utolso       */
        /* A pLokNevKlcsTmb-rol itt feltetelezzuk, hogy novekvo sorrendben rendezett */
        /* Nem kell elolrol, ugyis csak innen adtam at \/ */
        while ( nUtElhNevIdx < nLokNevTmbMeret && *(pLokNevKlcsTmb+nUtElhNevIdx) != nev_eredm_tmb[n_ki_besz_tab_mer - 1] )
        {
          nUtElhNevIdx++ ;
        }

        if ( nLokNevTmbMeret > NEV_TMB_MER + nUtElhNevIdx )
        {
          memcpy( nev_klcs_tmb, pLokNevKlcsTmb+nUtElhNevIdx, NEV_TMB_MER*sizeof(unsigned)) ;
          n_nev_klcs_tmb_mer = NEV_TMB_MER ;
        }
        else
        {
          n_nev_klcs_tmb_mer = nLokNevTmbMeret - nUtElhNevIdx ;
          memcpy( nev_klcs_tmb, pLokNevKlcsTmb+nUtElhNevIdx, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
        }
      }
      else /* Ehhez a NEV_TMB_MER meretu nev kulcs reszhez nem volt eredmeny */
      {
        /* Ha ez(^) ugy esett meg, hogy a nev mar az utolso megadott volt, nincs */
        /* tobb elhozando eredmeny, ki kell szallni a ciklusbol                  */
        if ( nUtElhNevIdx == nLokNevTmbMeret - 1 )
        {
          break ;
        }

        if ( nLokNevTmbMeret - nUtElhNevIdx > NEV_TMB_MER )
        {
          memcpy( nev_klcs_tmb, pLokNevKlcsTmb+nUtElhNevIdx, NEV_TMB_MER*sizeof(unsigned)) ;
          n_nev_klcs_tmb_mer = NEV_TMB_MER ;
        }
        else
        {
          n_nev_klcs_tmb_mer = nLokNevTmbMeret - nUtElhNevIdx ;
          memcpy( nev_klcs_tmb, pLokNevKlcsTmb+nUtElhNevIdx, n_nev_klcs_tmb_mer*sizeof(unsigned)) ;
        }

        nUtElhNevIdx += n_nev_klcs_tmb_mer ;
      }
    }

    delete [] pLokNevKlcsTmb ;

/*    pPillAllLekEr->uiOsszErTmbMer = nUtElhNevIdx ; ??? */
  }

  /* ----------------------------------------------------------------------- */

  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :
/*
for ( unsigned ui = 0 ; ui < 18 ; ui++ )
{
  TRACE("\nui:%d nev_klcs_tmb:%d nev_hlyz_tmb:%d szez_pnt_tmb:%d", ui, nev_klcs_tmb[ui], nev_hlyz_tmb[ui], szez_pnt_tmb[ui]) ;
}
*/
  /* Tombok felszabaditasa ? */
  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
} ;

int KulcsOsszeh( const void* pKulcs1, const void* pKulcs2)
{
  if ( *((const unsigned*)pKulcs1) < *((const unsigned*)pKulcs2) )
  {
    return -1 ;
  }

  if ( *((const unsigned*)pKulcs1) > *((const unsigned*)pKulcs2) )
  {
    return 1 ;
  }

  return 0 ;
}
/*
mind :
   SELECT n.rajtszam, n.versenyzo, n.nemzetiseg, n.csapat, n.motor

IDC_LEK1_FUTGY_RB2    :
          b.szezon_eve, b.nagydij_nev, b.helyezes, b.ora, b.perc, b.masodperc, b.ezrdmsdperc (, r/b.megjegyzes)
     FROM nevezesi_lista n,
          befuto b,
     WHERE
          b.szezon_eve > :n_szez_eve  AND
          n.kulcs      = b.nvzl_kulcs AND
          b.helyezes   = 1 AND

IDC_LEK1_EDZELS_RB3   :
          r.szezon_eve, r.nagydij_nev, r.helyezes, r.perc, r.masodperc, r.ezrdmsdperc (, r/b.megjegyzes)
     FROM nevezesi_lista n,
          rajtsorrend r,
     WHERE
          r.szezon_eve > :n_szez_eve  AND
          n.kulcs      = r.nvzl_kulcs AND
          b.helyezes   = 1 AND

IDC_LEK1_LEGGYKOR_RB4 :
          l.szezon_eve, l.nagydij_nev, l.perc, l.masodperc (l.korben, l.atl_sebesseg)
     FROM nevezesi_lista n,
          leggyorsabb_kor l
     WHERE
          l.szezon_eve > :n_szez_eve  AND
          n.kulcs      = l .nvzl_kulcs AND

ID_LEKERD_VERSENYZNV  :                         kapcsolat kell meg a nevezesi_lista es r/b/l tablak kozott
         n.versenyzo   = :n_nev_kulcs AND


ID_LEKERD_NEMZETISGNV :
         n.nemzetiseg  = :n_nev_kulcs AND

ID_LEKERD_CSAPAT      :
         n.csapat      = :n_nev_kulcs AND

ID_LEKERD_MOTORNV     :
         n.motor       = :n_nev_kulcs AND

ID_LEKERD_NAGYDJNV    :
  IDC_LEK1_FUTGY_RB2    :
         b.nagydij_nev = :n_nev_kulcs AND

  IDC_LEK1_EDZELS_RB3   :
         r.nagydij_nev = :n_nev_kulcs AND

  IDC_LEK1_LEGGYKOR_RB4 :
         l.nagydij_nev = :n_nev_kulcs AND

nincs ilyen :
ID_LEKERD_PALYANV       :

*/
/*
for ( uiIdx = 0 ; uiIdx < nCiklFelsHat ; uiIdx++ )
{
  TRACE("\nR:%d, V:%d, N:%d, C:%d, M:%d, S:%d, N:%d, H:%d, O:%d, P:%d, M:%d, E:%d",
         rs_bef_lek_rekord[uiIdx].n_rajtszam,
         rs_bef_lek_rekord[uiIdx].n_versenyzo,
         rs_bef_lek_rekord[uiIdx].n_nemzetiseg,
         rs_bef_lek_rekord[uiIdx].n_csapat,
         rs_bef_lek_rekord[uiIdx].n_motor,
         rs_bef_lek_rekord[uiIdx].n_szezon_eve,
         rs_bef_lek_rekord[uiIdx].n_nagydij_nev,
//         rs_bef_lek_rekord[uiIdx].uiHelyezes,
         rs_bef_lek_rekord[uiIdx].n_ora,
         rs_bef_lek_rekord[uiIdx].n_perc,
         rs_bef_lek_rekord[uiIdx].n_masodperc,
         rs_bef_lek_rekord[uiIdx].n_ezrdmsdperc
      ) ;
}

for ( uiIdx = 0 ; uiIdx < nCiklFelsHat ; uiIdx++ )
{
  TRACE("\nR:%d, V:%d, N:%d, C:%d, M:%d, S:%d, N:%d, O:%d, P:%d, M:%d, E:%d",
         rs_bef_lek_rekord[uiIdx].n_rajtszam,
         rs_bef_lek_rekord[uiIdx].n_versenyzo,
         rs_bef_lek_rekord[uiIdx].n_nemzetiseg,
         rs_bef_lek_rekord[uiIdx].n_csapat,
         rs_bef_lek_rekord[uiIdx].n_motor,
         rs_bef_lek_rekord[uiIdx].n_szezon_eve,
         rs_bef_lek_rekord[uiIdx].n_nagydij_nev,
         rs_bef_lek_rekord[uiIdx].n_ora,
         rs_bef_lek_rekord[uiIdx].n_perc,
         rs_bef_lek_rekord[uiIdx].n_masodperc,
         rs_bef_lek_rekord[uiIdx].n_ezrdmsdperc
      ) ;
}
*/