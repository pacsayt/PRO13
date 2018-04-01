
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
    ".\\dbpontoz.PC"
};


static unsigned long sqlctx = 520923;


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
            void  *sqhstv[3];
   unsigned int   sqhstl[3];
            int   sqhsts[3];
            void  *sqindv[3];
            int   sqinds[3];
   unsigned int   sqharm[3];
   unsigned int   *sqharc[3];
   unsigned short  sqadto[3];
   unsigned short  sqtdso[3];
} sqlstm = {10,3};

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
5,0,0,1,46,0,17,40,0,0,0,0,0,1,0,
20,0,0,1,0,0,45,46,0,0,0,0,0,1,0,
35,0,0,1,0,0,13,54,0,0,3,0,0,1,0,2,68,0,0,2,68,0,0,2,68,0,0,
62,0,0,1,0,0,15,66,0,0,0,0,0,1,0,
77,0,0,2,0,0,29,68,0,0,0,0,0,1,0,
92,0,0,1,0,0,15,92,0,0,0,0,0,1,0,
107,0,0,3,0,0,29,94,0,0,0,0,0,1,0,
122,0,0,1,0,0,15,108,0,0,0,0,0,1,0,
137,0,0,4,0,0,29,114,0,0,0,0,0,1,0,
152,0,0,1,0,0,15,126,0,0,0,0,0,1,0,
167,0,0,5,44,0,4,151,0,0,1,0,0,1,0,2,68,0,0,
186,0,0,6,44,0,3,160,0,0,3,3,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,
213,0,0,7,47,0,4,195,0,0,1,1,0,1,0,1,68,0,0,
232,0,0,8,38,0,2,200,0,0,1,1,0,1,0,1,68,0,0,
251,0,0,9,92,0,17,236,0,0,0,0,0,1,0,
266,0,0,9,0,0,21,246,0,0,3,3,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,
};


// dbpontoz.pc

#include "stdafx.h"

//#include <string.h>

#include "ensqlcam.h"

//#include "alaptip.h"
#include "pontozas.h"

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


long CPontozas::SQL_PontBetolt()
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    struct sql_pont
    {
      unsigned n_kulcs ;
      unsigned n_helyezes ;
      unsigned n_pontszam ;
    } pontozas_tomb[2*MAX_ERTEKELT] ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  int       nFetch, i ;
  int       nCiklFelsHat ;
  pontadat* pPontAdat ;

  long l_SQLCODE  ;

/*  EXEC SQL SET TRANSACTION READ ONLY ; mert a vegen COMMIT/ROLLBACK van ! */

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  /* EXEC SQL PREPARE select_pont_utas
           FROM "SELECT kulcs, helyezes, pontszam FROM pontozas" ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 0;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "SELECT kulcs, helyezes, pontszam FROM pontozas";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )5;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  /* Ha nics hiba az utasitasokban, johet az erdemi resz */
  /* EXEC SQL DECLARE select_pont_kurz CURSOR FOR select_pont_utas ; */ 


  /* EXEC SQL OPEN select_pont_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 0;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )20;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  nFetch = 0 ;

  /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


  while ( TRUE )
  {
    /* EXEC SQL FETCH select_pont_kurz INTO :pontozas_tomb ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 3;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )20;
    sqlstm.offset = (unsigned int  )35;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&pontozas_tomb->n_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_pont);
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqharc[0] = (unsigned int   *)0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&pontozas_tomb->n_helyezes;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_pont);
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqharc[1] = (unsigned int   *)0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&pontozas_tomb->n_pontszam;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )sizeof(struct sql_pont);
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


/* EXEC SQL FOR :array_size FETCH */

    nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;
    for ( i = 0 ; i < nCiklFelsHat ; i++ )
    {
      if ( Lookup( pontozas_tomb[i].n_kulcs, pPontAdat) == FALSE )
      {
        if ( (pPontAdat = new pontadat()) == NULL )
        {
          SQL_SQLCAFeltolt() ;

          /* EXEC SQL CLOSE select_pont_kurz ; */ 

{
          struct sqlexd sqlstm;

          sqlstm.sqlvsn = 10;
          sqlstm.arrsiz = 3;
          sqlstm.sqladtp = &sqladt;
          sqlstm.sqltdsp = &sqltds;
          sqlstm.iters = (unsigned int  )1;
          sqlstm.offset = (unsigned int  )62;
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
          sqlstm.arrsiz = 3;
          sqlstm.sqladtp = &sqladt;
          sqlstm.sqltdsp = &sqltds;
          sqlstm.iters = (unsigned int  )1;
          sqlstm.offset = (unsigned int  )77;
          sqlstm.cud = sqlcud0;
          sqlstm.sqlest = (unsigned char  *)&sqlca;
          sqlstm.sqlety = (unsigned short)0;
          sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
          if (sqlca.sqlcode < 0) goto sql_hiba;
}



          return -1 ;
        }

        SetAt( pontozas_tomb[i].n_kulcs, pPontAdat) ;
      }

      /* A helyezesek, mint tombindexek 0-tol indulnak, mig az adatbazisban 1-tol */
      (*pPontAdat)[pontozas_tomb[i].n_helyezes-1] = pontozas_tomb[i].n_pontszam ;
    }

    nFetch = sqlca.sqlerrd[2] ;
  }

  nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;
  for ( i = 0 ; i < nCiklFelsHat ; i++ )
  {
    if ( Lookup( pontozas_tomb[i].n_kulcs, pPontAdat) == FALSE )
    {
      if ( (pPontAdat = new pontadat()) == NULL )
      {
        SQL_SQLCAFeltolt() ;

        /* EXEC SQL CLOSE select_pont_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 3;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )92;
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
        sqlstm.arrsiz = 3;
        sqlstm.sqladtp = &sqladt;
        sqlstm.sqltdsp = &sqltds;
        sqlstm.iters = (unsigned int  )1;
        sqlstm.offset = (unsigned int  )107;
        sqlstm.cud = sqlcud0;
        sqlstm.sqlest = (unsigned char  *)&sqlca;
        sqlstm.sqlety = (unsigned short)0;
        sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
        if (sqlca.sqlcode < 0) goto sql_hiba;
}



        return -1 ;
      }

      SetAt( pontozas_tomb[i].n_kulcs, pPontAdat) ;
    }

    /* A helyezesek, mint tombindexek 0-tol indulnak, mig az adatbazisban 1-tol */
    (*pPontAdat)[pontozas_tomb[i].n_helyezes-1] = pontozas_tomb[i].n_pontszam ;
  }

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  /* EXEC SQL CLOSE select_pont_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 3;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )122;
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
  sqlstm.arrsiz = 3;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )137;
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

  /* EXEC SQL CLOSE select_pont_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 3;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )152;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

long CPontozas::SQL_PontINSERT( pontadat& sUjPontAdat, unsigned* puiKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_kulcs ;

    struct sql_pont
    {
      unsigned n_kulcs ;
      unsigned n_helyezes ;
      unsigned n_pontszam ;
    } pontozas_tomb[MAX_ERTEKELT] ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  int i ;
  long l_SQLCODE  ;

  /* EXEC SQL WHENEVER SQLERROR goto sql_hiba ; */ 


  /* EXEC SQL SELECT seq_pont.NEXTVAL INTO :n_kulcs FROM seq ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 3;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select seq_pont.nextval  into :b0  from seq ";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )167;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_kulcs;
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



  for  ( i = 0 ; i < MAX_ERTEKELT ; i++ )
  {
    pontozas_tomb[i].n_kulcs    = n_kulcs ;
    pontozas_tomb[i].n_helyezes = i + 1 ;
    pontozas_tomb[i].n_pontszam = sUjPontAdat[i] ;
  }

  /* EXEC SQL INSERT INTO pontozas VALUES( :pontozas_tomb) ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 3;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "insert into pontozas values (:s1 ,:s2 ,:s3 )";
  sqlstm.iters = (unsigned int  )10;
  sqlstm.offset = (unsigned int  )186;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&pontozas_tomb->n_kulcs;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_pont);
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqharc[0] = (unsigned int   *)0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&pontozas_tomb->n_helyezes;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_pont);
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqharc[1] = (unsigned int   *)0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&pontozas_tomb->n_pontszam;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )sizeof(struct sql_pont);
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
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  *puiKulcs = n_kulcs ;

  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CPontozas::SQL_PontDELETE( unsigned uiKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_kulcs ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR goto sql_hiba ; */ 


  n_kulcs = uiKulcs ;

  /* EXEC SQL SELECT pontozas FROM szezon WHERE pontozas = :n_kulcs ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 3;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select pontozas  from szezon where pontozas=:b0";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )213;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_kulcs;
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



  /* Torles csak akkor, ha meg nem szerepel egy szezonban sem ! */
  if ( sqlca.sqlcode == 1403 )
  {
    /* EXEC SQL DELETE FROM pontozas WHERE kulcs = :n_kulcs ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 3;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "delete  from pontozas  where kulcs=:b0";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )232;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_kulcs;
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

  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CPontozas::SQL_PontUPDATE( unsigned uiKulcs, pontadat& sUjPontAdat)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_kulcs ;
    unsigned n_helyezes ;
    unsigned n_pontszam ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  int i ;
  long l_SQLCODE  ;

  /* EXEC SQL WHENEVER SQLERROR goto sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND goto sql_hiba ; */ 


  /* EXEC SQL PREPARE update_pont_utas
           FROM "UPDATE pontozas SET pontszam = :n_pontszam WHERE kulcs = :n_kulcs AND helyezes = :n_helyezes" ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 3;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "UPDATE pontozas SET pontszam = :n_pontszam WHERE kulcs = :n_kulcs AND helyezes = :n_helyezes";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )251;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  n_kulcs = uiKulcs ;
  for  ( i = 0 ; i < MAX_ERTEKELT ; i++ )
  {
    /* A helyezesek, mint tombindexek 0-tol indulnak, mig az adatbazisban 1-tol */
    n_helyezes = i + 1 ;
    n_pontszam = sUjPontAdat[i] ;

    /* EXEC SQL EXECUTE update_pont_utas USING :n_pontszam, :n_kulcs, :n_helyezes ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 3;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )266;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_pontszam;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_kulcs;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_helyezes;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )0;
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
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
    if (sqlca.sqlcode == 1403) goto sql_hiba;
    if (sqlca.sqlcode < 0) goto sql_hiba;
}


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