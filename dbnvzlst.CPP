
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
    ".\\dbnvzlst.PC"
};


static unsigned long sqlctx = 521835;


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
            void  *sqhstv[6];
   unsigned int   sqhstl[6];
            int   sqhsts[6];
            void  *sqindv[6];
            int   sqinds[6];
   unsigned int   sqharm[6];
   unsigned int   *sqharc[6];
   unsigned short  sqadto[6];
   unsigned short  sqtdso[6];
} sqlstm = {10,6};

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
"select n.kulcs ,n.rajtszam ,n.versenyzo ,n.nemzetiseg ,n.csapat ,n.motor  fr\
om nevezesi_lista n ,rajtsorrend r where (r.szezon_eve=:b0 and r.nvzl_kulcs=n.\
kulcs)           ";

 static const char *sq0007 = 
"select szezon_eve ,nvzl_kulcs  from rajtsorrend where szezon_eve=:b0        \
   ";

 static const char *sq0037 = 
"select nvzl.rajtszam ,nvzl.kulcs  from nagydijak nd ,nevezesi_lista nvzl ,ra\
jtsorrend rs where ((((rs.szezon_eve=:b0 and nd.szezon_eve=:b0) and rs.nvzl_ku\
lcs=nvzl.kulcs) and rs.nagydij_nev=nd.nagydij_nev) and nd.datum<=TO_DATE(:b2,'\
YYYY-MM-DD')) order by nvzl.rajtszam,nd.datum            ";

typedef struct { unsigned short len; unsigned char arr[1]; } VARCHAR;
typedef struct { unsigned short len; unsigned char arr[1]; } varchar;

/* cud (compilation unit data) array */
static const short sqlcud0[] =
{10,4130,0,0,0,
5,0,0,1,112,0,4,51,0,0,2,1,0,1,0,2,68,0,0,1,68,0,0,
28,0,0,2,171,0,9,74,0,0,1,1,0,1,0,1,68,0,0,
47,0,0,2,0,0,13,80,0,0,6,0,0,1,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,2,68,0,0,
2,68,0,0,
86,0,0,2,0,0,15,99,0,0,0,0,0,1,0,
101,0,0,3,0,0,29,101,0,0,0,0,0,1,0,
116,0,0,2,0,0,15,129,0,0,0,0,0,1,0,
131,0,0,4,0,0,29,131,0,0,0,0,0,1,0,
146,0,0,2,0,0,15,142,0,0,0,0,0,1,0,
161,0,0,5,0,0,29,148,0,0,0,0,0,1,0,
176,0,0,2,0,0,15,160,0,0,0,0,0,1,0,
191,0,0,6,105,0,4,197,0,0,2,1,0,1,0,2,68,0,0,1,68,0,0,
214,0,0,7,79,0,9,214,0,0,1,1,0,1,0,1,68,0,0,
233,0,0,8,23,0,1,216,0,0,0,0,0,1,0,
248,0,0,7,0,0,13,222,0,0,2,0,0,1,0,2,68,0,0,2,68,0,0,
271,0,0,9,69,0,2,226,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
294,0,0,10,45,0,2,231,0,0,1,1,0,1,0,1,68,0,0,
313,0,0,11,69,0,2,239,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
336,0,0,12,45,0,2,245,0,0,1,1,0,1,0,1,68,0,0,
355,0,0,7,0,0,15,250,0,0,0,0,0,1,0,
370,0,0,2,0,0,15,271,0,0,0,0,0,1,0,
385,0,0,13,26,0,43,275,0,0,0,0,0,1,0,
400,0,0,14,44,0,4,310,0,0,1,0,0,1,0,2,68,0,0,
419,0,0,15,64,0,3,321,0,0,6,6,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,
0,1,68,0,0,
458,0,0,16,135,0,4,331,0,0,6,5,0,1,0,2,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,
0,0,1,68,0,0,
497,0,0,17,85,0,4,342,0,0,3,2,0,1,0,2,68,0,0,1,68,0,0,1,68,0,0,
524,0,0,18,112,0,4,370,0,0,2,1,0,1,0,2,68,0,0,1,68,0,0,
547,0,0,19,93,0,4,387,0,0,3,2,0,1,0,2,68,0,0,1,68,0,0,1,68,0,0,
574,0,0,20,73,0,3,407,0,0,4,4,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,
605,0,0,21,108,0,2,451,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
628,0,0,22,44,0,2,466,0,0,1,1,0,1,0,1,68,0,0,
647,0,0,23,86,0,4,515,0,0,3,2,0,1,0,2,68,0,0,1,68,0,0,1,68,0,0,
674,0,0,24,24,0,1,526,0,0,0,0,0,1,0,
689,0,0,25,105,0,5,530,0,0,6,6,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,
0,0,1,68,0,0,
728,0,0,26,24,0,1,544,0,0,0,0,0,1,0,
743,0,0,27,44,0,4,549,0,0,1,0,0,1,0,2,68,0,0,
762,0,0,28,59,0,3,552,0,0,6,6,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,
0,1,68,0,0,
801,0,0,29,135,0,4,574,0,0,6,5,0,1,0,2,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,
0,0,1,68,0,0,
840,0,0,30,191,0,3,588,0,0,4,4,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,
871,0,0,31,75,0,5,601,0,0,3,3,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,
898,0,0,32,84,0,5,607,0,0,3,3,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,
925,0,0,33,67,0,2,613,0,0,2,2,0,1,0,1,68,0,0,1,68,0,0,
948,0,0,34,27,0,43,633,0,0,0,0,0,1,0,
963,0,0,35,44,0,2,657,0,0,1,1,0,1,0,1,68,0,0,
982,0,0,36,80,0,4,706,0,0,3,2,0,1,0,2,5,0,0,1,68,0,0,1,68,0,0,
1009,0,0,37,289,0,9,722,0,0,3,3,0,1,0,1,68,0,0,1,68,0,0,1,5,0,0,
1036,0,0,37,0,0,13,726,0,0,2,0,0,1,0,2,68,0,0,2,68,0,0,
1059,0,0,37,0,0,13,761,0,0,2,0,0,1,0,2,68,0,0,2,68,0,0,
1082,0,0,37,0,0,15,876,0,0,0,0,0,1,0,
1097,0,0,37,0,0,15,890,0,0,0,0,0,1,0,
1112,0,0,38,99,0,6,918,0,0,4,4,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,2,68,0,0,
1143,0,0,39,135,0,4,977,0,0,6,5,0,1,0,2,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,
0,0,1,68,0,0,
1182,0,0,40,44,0,4,990,0,0,1,0,0,1,0,2,68,0,0,
1201,0,0,41,59,0,3,992,0,0,6,6,0,1,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,0,0,1,68,
0,0,1,68,0,0,
};


// dbnvzlst.pc

#include "stdafx.h"

#include "ensqlcam.h"

#include "resource.h"

#include "alaptip.h"
#include "nevzlist.h"

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


long CNevezLista::SQL_NvzlstBetolt( unsigned uiSzezon)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve  ;
    unsigned n_ElsoNagydij ;

    struct sql_s_nvzlst
    {
      unsigned n_nvzl_kulcs ;
      unsigned n_rajtszam   ;
      unsigned n_vers_nev   ;
      unsigned n_nemz_nev   ;
      unsigned n_csapat_nev ;
      unsigned n_motor_nev  ;
    } nvzlst_rekord[200] ;
    /* 200, mert minden rajtsorrend bejegyzeshez kapunk egy elemet */

/*    char  c_kamu_datum[13] ;*/

  /* EXEC SQL END DECLARE SECTION ; */ 


  int i, nFetch, nCiklFelsHat ;
  CNvzSor* pCNvzSor ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  m_uiSzezon = uiSzezon ;

  /* Az ev megadasa a kurzor es a kovetkezo SELECT szamara : */
  n_szezon_eve = uiSzezon ;

  /* EXEC SQL SELECT nagydij_nev INTO :n_ElsoNagydij
             FROM nagydijak
             WHERE datum = (SELECT MIN( datum)
                              FROM nagydijak
                              WHERE szezon_eve = :n_szezon_eve) ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 2;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select nagydij_nev into :b0  from nagydijak where datum=(se\
lect min(datum)  from nagydijak where szezon_eve=:b1)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )5;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_ElsoNagydij;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
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



/*
  EXEC SQL SELECT SELECT MIN( datum), nagydij_nev
             INTO :c_kamu_datum, :n_ElsoNagydij
             FROM nagydijak
             WHERE szezon_eve = :n_szezon_eve ;
*/

  /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


  m_uiElsoNagydij = n_ElsoNagydij ;

  /* EXEC SQL DECLARE select_nvzl_kurz CURSOR FOR
    SELECT n.kulcs, n.rajtszam, n.versenyzo, n.nemzetiseg, n.csapat, n.motor
      FROM nevezesi_lista n, rajtsorrend r
      WHERE r.szezon_eve = :n_szezon_eve  AND
            r.nvzl_kulcs = n.kulcs ; */ 


  /* EXEC SQL OPEN select_nvzl_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 2;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = sq0002;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )28;
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

  while ( TRUE )
  {
    /* EXEC SQL FETCH select_nvzl_kurz INTO :nvzlst_rekord ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )200;
    sqlstm.offset = (unsigned int  )47;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&nvzlst_rekord->n_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_nvzlst);
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqharc[0] = (unsigned int   *)0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&nvzlst_rekord->n_rajtszam;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_nvzlst);
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqharc[1] = (unsigned int   *)0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&nvzlst_rekord->n_vers_nev;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )sizeof(struct sql_s_nvzlst);
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqharc[2] = (unsigned int   *)0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqhstv[3] = (         void  *)&nvzlst_rekord->n_nemz_nev;
    sqlstm.sqhstl[3] = (unsigned int  )4;
    sqlstm.sqhsts[3] = (         int  )sizeof(struct sql_s_nvzlst);
    sqlstm.sqindv[3] = (         void  *)0;
    sqlstm.sqinds[3] = (         int  )0;
    sqlstm.sqharm[3] = (unsigned int  )0;
    sqlstm.sqharc[3] = (unsigned int   *)0;
    sqlstm.sqadto[3] = (unsigned short )0;
    sqlstm.sqtdso[3] = (unsigned short )0;
    sqlstm.sqhstv[4] = (         void  *)&nvzlst_rekord->n_csapat_nev;
    sqlstm.sqhstl[4] = (unsigned int  )4;
    sqlstm.sqhsts[4] = (         int  )sizeof(struct sql_s_nvzlst);
    sqlstm.sqindv[4] = (         void  *)0;
    sqlstm.sqinds[4] = (         int  )0;
    sqlstm.sqharm[4] = (unsigned int  )0;
    sqlstm.sqharc[4] = (unsigned int   *)0;
    sqlstm.sqadto[4] = (unsigned short )0;
    sqlstm.sqtdso[4] = (unsigned short )0;
    sqlstm.sqhstv[5] = (         void  *)&nvzlst_rekord->n_motor_nev;
    sqlstm.sqhstl[5] = (unsigned int  )4;
    sqlstm.sqhsts[5] = (         int  )sizeof(struct sql_s_nvzlst);
    sqlstm.sqindv[5] = (         void  *)0;
    sqlstm.sqinds[5] = (         int  )0;
    sqlstm.sqharm[5] = (unsigned int  )0;
    sqlstm.sqharc[5] = (unsigned int   *)0;
    sqlstm.sqadto[5] = (unsigned short )0;
    sqlstm.sqtdso[5] = (unsigned short )0;
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



    /* Ez a cikluson belul mindig 200 ! */
    nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;

    for ( i = 0 ; i < nCiklFelsHat ; i++ )
    {
      if ( Lookup( nvzlst_rekord[i].n_nvzl_kulcs, pCNvzSor) == FALSE )
      {
        pCNvzSor = new CNvzSor( nvzlst_rekord[i].n_rajtszam,
                                nvzlst_rekord[i].n_vers_nev,
                                nvzlst_rekord[i].n_nemz_nev,
                                nvzlst_rekord[i].n_csapat_nev,
                                nvzlst_rekord[i].n_motor_nev) ;

        if ( pCNvzSor == NULL )
        {
          SQL_SQLCAFeltolt() ;

          /* EXEC SQL CLOSE select_nvzl_kurz ; */ 

{
          struct sqlexd sqlstm;

          sqlstm.sqlvsn = 10;
          sqlstm.arrsiz = 6;
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
          sqlstm.arrsiz = 6;
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

        SetAt( nvzlst_rekord[i].n_nvzl_kulcs, pCNvzSor) ;
      }
    }

    nFetch = sqlca.sqlerrd[2] ;
  }

  nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;

  for ( i = 0 ; i < nCiklFelsHat ; i++ )
  {
    if ( Lookup( nvzlst_rekord[i].n_nvzl_kulcs, pCNvzSor) == FALSE )
    {
      pCNvzSor = new CNvzSor( nvzlst_rekord[i].n_rajtszam,
                              nvzlst_rekord[i].n_vers_nev,
                              nvzlst_rekord[i].n_nemz_nev,
                              nvzlst_rekord[i].n_csapat_nev,
                              nvzlst_rekord[i].n_motor_nev) ;

      if ( pCNvzSor == NULL )
      {
        SQL_SQLCAFeltolt() ;

        /* EXEC SQL CLOSE select_nvzl_kurz ; */ 

{
        struct sqlexd sqlstm;

        sqlstm.sqlvsn = 10;
        sqlstm.arrsiz = 6;
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
        sqlstm.arrsiz = 6;
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

      SetAt( nvzlst_rekord[i].n_nvzl_kulcs, pCNvzSor) ;
    }
  }

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  /* EXEC SQL CLOSE select_nvzl_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
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
  sqlstm.arrsiz = 6;
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

  /* EXEC SQL CLOSE select_nvzl_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
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

long CNevezLista::SQL_NvzlstTorol()
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve ;
    unsigned n_rs_bej_db  ;
    unsigned n_torlendo_db;

    struct sql_s_trl_nvzkl
    {
      unsigned n_nvzl_kulcs ;
    } nvzkl_rekord[200] ;

    struct sql_s_trl_dat
    {
      unsigned n_szezon_eve ;
    } dat_rekord[200] ;

    /* 200, mert minden rajtsorrend bejegyzeshez kapunk egy elemet */
    /* Az n_szezon_eve majd tkeppen a DELETE-nel kell, ahol skalar */
    /* valtozot nem lehet megadni (tombbel valo torles)            */

  /* EXEC SQL END DECLARE SECTION ; */ 


  int nFetch ;
  long l_SQLCODE ;

  n_szezon_eve = m_uiSzezon ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  /* megjegyzes:alaptip.h : NEM_KVALIF */
  /* EXEC SQL SELECT COUNT(*) INTO :n_rs_bej_db
             FROM rajtsorrend
             WHERE szezon_eve =  :n_szezon_eve AND
                   megjegyzes LIKE ('___________0____') ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select count(*)  into :b0  from rajtsorrend where (szezon_e\
ve=:b1 and megjegyzes like '___________0____')";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )191;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_rs_bej_db;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
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



  /* EXEC SQL WHENEVER NOTFOUND DO break ; */ 


  if ( n_rs_bej_db == 0 )
  {
    /* A sima eset : nincs 'igazi' bejegyzes -> mind torolheto */
    /* Ha nincs tenyleges eredmeny, mindegyik nevezesi lista   */
    /* sorbol csak egy lehet (?)                               */
    /* EXEC SQL DECLARE szez_nvzl_kiv_krz CURSOR FOR
      SELECT szezon_eve, nvzl_kulcs
        FROM rajtsorrend
        WHERE szezon_eve = :n_szezon_eve ; */ 


    /* EXEC SQL OPEN szez_nvzl_kiv_krz ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = sq0007;
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )214;
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



    /* EXEC SQL SAVEPOINT nvzl_torol_sp ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "savepoint nvzl_torol_sp";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )233;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    nFetch = 0 ;

    while ( TRUE )
    {
      /* EXEC SQL FETCH szez_nvzl_kiv_krz INTO :dat_rekord, :nvzkl_rekord ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 6;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )200;
      sqlstm.offset = (unsigned int  )248;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)&dat_rekord->n_szezon_eve;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_trl_dat);
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )0;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&nvzkl_rekord->n_nvzl_kulcs;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_trl_nvzkl);
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
      if (sqlca.sqlcode == 1403) break;
      if (sqlca.sqlcode < 0) goto sql_hiba;
}



      nFetch = sqlca.sqlerrd[2] ;

      /* EXEC SQL DELETE FROM rajtsorrend
                 WHERE szezon_eve = :dat_rekord AND
                       nvzl_kulcs = :nvzkl_rekord ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 6;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.stmt = "delete  from rajtsorrend  where (szezon_eve=:s1  and nv\
zl_kulcs=:s2 )";
      sqlstm.iters = (unsigned int  )200;
      sqlstm.offset = (unsigned int  )271;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)&dat_rekord->n_szezon_eve;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_trl_dat);
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )0;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&nvzkl_rekord->n_nvzl_kulcs;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_trl_nvzkl);
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
      if (sqlca.sqlcode == 1403) break;
      if (sqlca.sqlcode < 0) goto sql_hiba;
}



      /* Allitolag ha valamely tombelemet nem talalja, az nem okoz hibat... */
      /* EXEC SQL DELETE FROM nevezesi_lista
                 WHERE kulcs = :nvzkl_rekord ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 6;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.stmt = "delete  from nevezesi_lista  where kulcs=:s1 ";
      sqlstm.iters = (unsigned int  )200;
      sqlstm.offset = (unsigned int  )294;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)&nvzkl_rekord->n_nvzl_kulcs;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_trl_nvzkl);
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


    }

    n_torlendo_db = sqlca.sqlerrd[2] - nFetch ;

    /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


    /* EXEC SQL FOR :n_torlendo_db
             DELETE
               FROM rajtsorrend
               WHERE szezon_eve = :dat_rekord   AND
                     nvzl_kulcs = :nvzkl_rekord ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "delete  from rajtsorrend  where (szezon_eve=:s1  and nvzl\
_kulcs=:s2 )";
    sqlstm.iters = (unsigned int  )n_torlendo_db;
    sqlstm.offset = (unsigned int  )313;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&dat_rekord->n_szezon_eve;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_trl_dat);
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqharc[0] = (unsigned int   *)0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&nvzkl_rekord->n_nvzl_kulcs;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_trl_nvzkl);
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



    /* EXEC SQL FOR :n_torlendo_db
             DELETE
               FROM nevezesi_lista
               WHERE kulcs = :nvzkl_rekord ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "delete  from nevezesi_lista  where kulcs=:s1 ";
    sqlstm.iters = (unsigned int  )n_torlendo_db;
    sqlstm.offset = (unsigned int  )336;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&nvzkl_rekord->n_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_trl_nvzkl);
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
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    /* EXEC SQL CLOSE szez_nvzl_kiv_krz ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )355;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    SQL_SQLCAFeltolt() ;

    l_SQLCODE = sqlca.sqlcode ;

/*    EXEC SQL COMMIT TO SAVEPOINT nvzl_torol_sp ; csak ROLLBACK-et lehet  ??? */

    return l_SQLCODE ;
  }

  return -1 ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL CLOSE select_nvzl_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )370;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  /* Ha a legelso SELECT-ben hiba van, ne szoljon bele a hianyzo */
  /* SAVEPOINT-bol szarmazo hiba                                 */
  /* EXEC SQL ROLLBACK TO SAVEPOINT nvzl_torol_sp ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "rollback  to nvzl_torol_sp";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )385;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

long CNevezLista::SQL_NvzlstINSERT( CNvzSor* pNvzSor, unsigned* pKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve ;

    unsigned n_elso_nd_kulcs ;
    unsigned n_nvzl_kulcs ;

    unsigned n_nvzl_elof_db ;
    short    ind_nvzl_elof_db ;

    unsigned n_legmag_hlyz ;

    struct sql_s_nvzlst
    {
      unsigned n_rajtszam   ;
      unsigned n_vers_nev   ;
      unsigned n_nemz_nev   ;
      unsigned n_csapat_nev ;
      unsigned n_motor_nev  ;
    } nvz_sor ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

/*  EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */
  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  /* EXEC SQL SELECT seq_nvzl.NEXTVAL INTO :n_nvzl_kulcs FROM seq ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select seq_nvzl.nextval  into :b0  from seq ";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )400;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
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
}



  n_szezon_eve = m_uiSzezon ;

  nvz_sor.n_rajtszam = pNvzSor->uiRajtszam  ;
  nvz_sor.n_vers_nev = pNvzSor->uiVersenyzo ;
  nvz_sor.n_nemz_nev = pNvzSor->uiNemz      ;
  nvz_sor.n_csapat_nev = pNvzSor->uiCsapat  ;
  nvz_sor.n_motor_nev = pNvzSor->uiMotor    ;

  /* A nevezesi lista sor felvetele */
  /* EXEC SQL INSERT INTO nevezesi_lista
             VALUES ( :n_nvzl_kulcs, :nvz_sor) ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "insert into nevezesi_lista values (:b0,:s1 ,:s2 ,:s3 ,:s4 ,\
:s5 )";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )419;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&nvz_sor.n_rajtszam;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&nvz_sor.n_vers_nev;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&nvz_sor.n_nemz_nev;
  sqlstm.sqhstl[3] = (unsigned int  )4;
  sqlstm.sqhsts[3] = (         int  )0;
  sqlstm.sqindv[3] = (         void  *)0;
  sqlstm.sqinds[3] = (         int  )0;
  sqlstm.sqharm[3] = (unsigned int  )0;
  sqlstm.sqadto[3] = (unsigned short )0;
  sqlstm.sqtdso[3] = (unsigned short )0;
  sqlstm.sqhstv[4] = (         void  *)&nvz_sor.n_csapat_nev;
  sqlstm.sqhstl[4] = (unsigned int  )4;
  sqlstm.sqhsts[4] = (         int  )0;
  sqlstm.sqindv[4] = (         void  *)0;
  sqlstm.sqinds[4] = (         int  )0;
  sqlstm.sqharm[4] = (unsigned int  )0;
  sqlstm.sqadto[4] = (unsigned short )0;
  sqlstm.sqtdso[4] = (unsigned short )0;
  sqlstm.sqhstv[5] = (         void  *)&nvz_sor.n_motor_nev;
  sqlstm.sqhstl[5] = (unsigned int  )4;
  sqlstm.sqhsts[5] = (         int  )0;
  sqlstm.sqindv[5] = (         void  *)0;
  sqlstm.sqinds[5] = (         int  )0;
  sqlstm.sqharm[5] = (unsigned int  )0;
  sqlstm.sqadto[5] = (unsigned short )0;
  sqlstm.sqtdso[5] = (unsigned short )0;
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
}



  /* Ha mar van ilyen nevezesi lista felveve, mi a kulcsa ? */
  if ( sqlca.sqlcode == -1 )
  {
    /* Az elozo INSERT ota le is torolhettek !                      */
    /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

    /* EXEC SQL WHENEVER NOTFOUND GOTO sql_hiba ; */ 


    /* EXEC SQL SELECT kulcs
               INTO :n_nvzl_kulcs
               FROM nevezesi_lista
               WHERE rajtszam  = :nvz_sor.n_rajtszam   AND
                     versenyzo = :nvz_sor.n_vers_nev   AND
                     nemzetiseg= :nvz_sor.n_nemz_nev   AND
                     csapat    = :nvz_sor.n_csapat_nev AND
                     motor     = :nvz_sor.n_motor_nev ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "select kulcs into :b0  from nevezesi_lista where ((((rajt\
szam=:b1 and versenyzo=:b2) and nemzetiseg=:b3) and csapat=:b4) and motor=:b5)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )458;
    sqlstm.selerr = (unsigned short)1;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&(nvz_sor.n_rajtszam);
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&(nvz_sor.n_vers_nev);
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )0;
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqhstv[3] = (         void  *)&(nvz_sor.n_nemz_nev);
    sqlstm.sqhstl[3] = (unsigned int  )4;
    sqlstm.sqhsts[3] = (         int  )0;
    sqlstm.sqindv[3] = (         void  *)0;
    sqlstm.sqinds[3] = (         int  )0;
    sqlstm.sqharm[3] = (unsigned int  )0;
    sqlstm.sqadto[3] = (unsigned short )0;
    sqlstm.sqtdso[3] = (unsigned short )0;
    sqlstm.sqhstv[4] = (         void  *)&(nvz_sor.n_csapat_nev);
    sqlstm.sqhstl[4] = (unsigned int  )4;
    sqlstm.sqhsts[4] = (         int  )0;
    sqlstm.sqindv[4] = (         void  *)0;
    sqlstm.sqinds[4] = (         int  )0;
    sqlstm.sqharm[4] = (unsigned int  )0;
    sqlstm.sqadto[4] = (unsigned short )0;
    sqlstm.sqtdso[4] = (unsigned short )0;
    sqlstm.sqhstv[5] = (         void  *)&(nvz_sor.n_motor_nev);
    sqlstm.sqhstl[5] = (unsigned int  )4;
    sqlstm.sqhsts[5] = (         int  )0;
    sqlstm.sqindv[5] = (         void  *)0;
    sqlstm.sqinds[5] = (         int  )0;
    sqlstm.sqharm[5] = (unsigned int  )0;
    sqlstm.sqadto[5] = (unsigned short )0;
    sqlstm.sqtdso[5] = (unsigned short )0;
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



    /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


    /* EXEC SQL SELECT COUNT(*)
               INTO :n_nvzl_elof_db
               FROM rajtsorrend
               WHERE szezon_eve = :n_szezon_eve AND
                     nvzl_kulcs = :n_nvzl_kulcs ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "select count(*)  into :b0  from rajtsorrend where (szezon\
_eve=:b1 and nvzl_kulcs=:b2)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )497;
    sqlstm.selerr = (unsigned short)1;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nvzl_elof_db;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_nvzl_kulcs;
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
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    /* Ez a nevezesi lista sor mar szerepel a szezonban !           */
    if ( n_nvzl_elof_db != 0 )
    {
      *pKulcs = n_nvzl_kulcs ;

      SQL_SQLCAFeltolt() ;

      l_SQLCODE = sqlca.sqlcode ;

      return l_SQLCODE ;
    }
  }

  /* Ha meg nem volt ilyen nevezesi lista sor felveve, nem          */
  /* szerepelhet a rajtsorrend tablaban sem                         */

  /* EXEC SQL WHENEVER NOTFOUND GOTO sql_hiba ; */ 


  /* Mert mindig az elso nagydij kulcsaval szerepel a bejegyzes     */
  /* a rajtsorrend tablaban a nevezesi lista meghatarozasahoz       */
  if ( m_uiElsoNagydij == 0 )
  {
    /* EXEC SQL SELECT nagydij_nev INTO :n_elso_nd_kulcs
               FROM nagydijak
               WHERE datum = (SELECT MIN( datum)
                                FROM nagydijak
                                WHERE szezon_eve = :n_szezon_eve) ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "select nagydij_nev into :b0  from nagydijak where datum=(\
select min(datum)  from nagydijak where szezon_eve=:b1)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )524;
    sqlstm.selerr = (unsigned short)1;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_elso_nd_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
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
    if (sqlca.sqlcode == 1403) goto sql_hiba;
    if (sqlca.sqlcode < 0) goto sql_hiba;
}



    m_uiElsoNagydij = n_elso_nd_kulcs ;
  }
  else
  {
    /* Az n_elso_nd_kulcs valtozot inicializalni kell !             */
    n_elso_nd_kulcs = m_uiElsoNagydij ;
  }

  /* Mivel a rajtsorrend tablaban a szezon egy nagydijan egy        */
  /* helyezes csak egyszer fordulhat elo (uk_rs_sze_ndn_hlyz)       */
  /* meg kell keresni a legmagasabb ureset                          */
  /* EXEC SQL SELECT MAX( helyezes)
             INTO :n_legmag_hlyz INDICATOR :ind_nvzl_elof_db
             FROM rajtsorrend
             WHERE szezon_eve  = :n_szezon_eve AND
                   nagydij_nev = :n_elso_nd_kulcs ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select max(helyezes) into :b0:b1  from rajtsorrend where (s\
zezon_eve=:b2 and nagydij_nev=:b3)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )547;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_legmag_hlyz;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)&ind_nvzl_elof_db;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_elso_nd_kulcs;
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



   if ( ind_nvzl_elof_db == -1 )
   {
     /* Meg egyetlen bejegyzes sincs a rajtsorrend tablaban (NULL)   */
     n_legmag_hlyz = 0 ;
   }

/* Ha sok probalkozas volt, lehet, hogy 99-nel nagyobb szam jon ki...*/
/* Itt lehetne meg masik nagydijjal probalkozni ekkor ...            */

  /* Az uj nevezesi lista kulccsal, a hozza tartozo elso nagydijjal  */
  /* de NEM_KVALIF megjegyzessel uj rajtsorrend bejegyzes(rajtsbef.h)*/
  /* n_elso_nd_kulcs mar korabban beallitva, a MAX( helyezes)-hez    */
  n_legmag_hlyz++ ;

  /* EXEC SQL INSERT INTO rajtsorrend
             VALUES ( :n_szezon_eve, :n_elso_nd_kulcs, :n_nvzl_kulcs,
                      :n_legmag_hlyz, 0, 0, 0,
                      '___________1____' ) ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "insert into rajtsorrend values (:b0,:b1,:b2,:b3,0,0,0,'____\
_______1____')";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )574;
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
  sqlstm.sqhstv[1] = (         void  *)&n_elso_nd_kulcs;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_nvzl_kulcs;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&n_legmag_hlyz;
  sqlstm.sqhstl[3] = (unsigned int  )4;
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
  if (sqlca.sqlcode == 1403) goto sql_hiba;
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  *pKulcs = n_nvzl_kulcs ;

  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CNevezLista::SQL_NvzlstDELETE( unsigned uiKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_nvzl_kulcs ;
    unsigned n_szezon_eve ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  n_nvzl_kulcs = uiKulcs    ;
  n_szezon_eve = m_uiSzezon ;

  /* Ha mar a feluletrol ez a kulcs erkezik, biztos van hozza legalabb   */
  /* rajtsorrend bejegyzes NEM_KVALIF megjegyzessel, e sorok torlese     */
  /* elott nincs ertelme a nevezesi_lista-bol torolni                    */
  /* EXEC SQL DELETE FROM rajtsorrend
             WHERE nvzl_kulcs = :n_nvzl_kulcs AND
                   szezon_eve = :n_szezon_eve AND
                   megjegyzes  LIKE ('___________1____') ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "delete  from rajtsorrend  where ((nvzl_kulcs=:b0 and szezon\
_eve=:b1) and megjegyzes like '___________1____')";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )605;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
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
}



  /* ha mas szezonban is elofordul a nevezes lista kulcs,akkor nem szabad*/
  /* (es nem is lehet) torolni a nevezesi_lista tablabol, hanem csak a   */
  /* rajtsorrend tablabol a bejegyzest. A NOTFOUND azt is jelentheti,    */
  /* hogy van tenyleges bejegyzes.                                       */
  if ( sqlca.sqlcode == 0 )
  {
    /* Ha a kovetkezo utasitas hibara fut, az azt jelenti, hogy mas      */
    /* szezonban vagy az adott szezonban kihagyassal elofordul a kerdeses*/
    /* nevezesi lista kulcs. Ez nem hiba, mert az adott szezonban nem    */
    /* jelenik meg tobbet ez a nvzl sor                                  */
    /* EXEC SQL DELETE FROM nevezesi_lista
               WHERE kulcs = :n_nvzl_kulcs ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "delete  from nevezesi_lista  where kulcs=:b0";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )628;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
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
}



    SQL_SQLCAFeltolt() ;

    return 0 ;
  }

  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CNevezLista::SQL_NvzlstUPDATE( CNvzSor* pNvzSor, unsigned* pKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szez_eve   ;
    unsigned n_nvzl_kulcs ;
    unsigned n_uj_nvzl_kulcs ;
 
    unsigned n_rajtszam   ;
    unsigned n_vers_nev   ;
    unsigned n_nemz_nev   ;
    unsigned n_csapat_nev ;
    unsigned n_motor_nev  ;

    unsigned n_darab ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  char cUPD_INS ;

  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  n_szez_eve   = m_uiSzezon ;
  n_nvzl_kulcs = *pKulcs ; 

  n_rajtszam   = pNvzSor->uiRajtszam  ;
  n_vers_nev   = pNvzSor->uiVersenyzo ;
  n_nemz_nev   = pNvzSor->uiNemz      ;
  n_csapat_nev = pNvzSor->uiCsapat    ;
  n_motor_nev  = pNvzSor->uiMotor     ;

  /* EXEC SQL SELECT COUNT(*) INTO :n_darab
             FROM rajtsorrend
             WHERE szezon_eve != :n_szez_eve AND
                   nvzl_kulcs  = :n_nvzl_kulcs ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select count(*)  into :b0  from rajtsorrend where (szezon_e\
ve<>:b1 and nvzl_kulcs=:b2)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )647;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_darab;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_szez_eve;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_nvzl_kulcs;
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
}



  l_SQLCODE = sqlca.sqlcode ;

  if ( sqlca.sqlcode == -1403 )
  {
    /* Csak az aktualis szezonban szerepel a valtoztatando nvzl sor */

    /* EXEC SQL SAVEPOINT uj_nvzl_sor_sp ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "savepoint uj_nvzl_sor_sp";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )674;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



    cUPD_INS = 'U' ;

    /* EXEC SQL UPDATE nevezesi_lista
               SET rajtszam   = :n_rajtszam,
                   versenyzo  = :n_vers_nev,
                   nemzetiseg = :n_nemz_nev,
                   csapat     = :n_csapat_nev,
                   motor      = :n_motor_nev
               WHERE kulcs = :n_nvzl_kulcs ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "update nevezesi_lista  set rajtszam=:b0,versenyzo=:b1,nem\
zetiseg=:b2,csapat=:b3,motor=:b4 where kulcs=:b5";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )689;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_rajtszam;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_vers_nev;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_nemz_nev;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )0;
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqhstv[3] = (         void  *)&n_csapat_nev;
    sqlstm.sqhstl[3] = (unsigned int  )4;
    sqlstm.sqhsts[3] = (         int  )0;
    sqlstm.sqindv[3] = (         void  *)0;
    sqlstm.sqinds[3] = (         int  )0;
    sqlstm.sqharm[3] = (unsigned int  )0;
    sqlstm.sqadto[3] = (unsigned short )0;
    sqlstm.sqtdso[3] = (unsigned short )0;
    sqlstm.sqhstv[4] = (         void  *)&n_motor_nev;
    sqlstm.sqhstl[4] = (unsigned int  )4;
    sqlstm.sqhsts[4] = (         int  )0;
    sqlstm.sqindv[4] = (         void  *)0;
    sqlstm.sqinds[4] = (         int  )0;
    sqlstm.sqharm[4] = (unsigned int  )0;
    sqlstm.sqadto[4] = (unsigned short )0;
    sqlstm.sqtdso[4] = (unsigned short )0;
    sqlstm.sqhstv[5] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[5] = (unsigned int  )4;
    sqlstm.sqhsts[5] = (         int  )0;
    sqlstm.sqindv[5] = (         void  *)0;
    sqlstm.sqinds[5] = (         int  )0;
    sqlstm.sqharm[5] = (unsigned int  )0;
    sqlstm.sqadto[5] = (unsigned short )0;
    sqlstm.sqtdso[5] = (unsigned short )0;
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
}


 
    l_SQLCODE = sqlca.sqlcode ;
  }
  else
  {
    /* Mas szezonban (is) szerepel a valtoztatando nvzl sor */

    /* EXEC SQL SAVEPOINT uj_nvzl_sor_sp ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "savepoint uj_nvzl_sor_sp";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )728;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



    cUPD_INS = 'I' ;

    /* Uj nevezesi lista kulcs */
    /* EXEC SQL SELECT seq_nvzl.NEXTVAL INTO :n_uj_nvzl_kulcs FROM seq ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "select seq_nvzl.nextval  into :b0  from seq ";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )743;
    sqlstm.selerr = (unsigned short)1;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_uj_nvzl_kulcs;
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
}



    /* Uj nevezesi lista sor felvetele az aktualis szezon szamara */
    /* EXEC SQL INSERT INTO nevezesi_lista
               VALUES ( :n_uj_nvzl_kulcs,
                        :n_rajtszam,
                        :n_vers_nev,
                        :n_nemz_nev,
                        :n_csapat_nev,
                        :n_motor_nev
                      ) ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "insert into nevezesi_lista values (:b0,:b1,:b2,:b3,:b4,:b\
5)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )762;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_uj_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_rajtszam;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_vers_nev;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )0;
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqhstv[3] = (         void  *)&n_nemz_nev;
    sqlstm.sqhstl[3] = (unsigned int  )4;
    sqlstm.sqhsts[3] = (         int  )0;
    sqlstm.sqindv[3] = (         void  *)0;
    sqlstm.sqinds[3] = (         int  )0;
    sqlstm.sqharm[3] = (unsigned int  )0;
    sqlstm.sqadto[3] = (unsigned short )0;
    sqlstm.sqtdso[3] = (unsigned short )0;
    sqlstm.sqhstv[4] = (         void  *)&n_csapat_nev;
    sqlstm.sqhstl[4] = (unsigned int  )4;
    sqlstm.sqhsts[4] = (         int  )0;
    sqlstm.sqindv[4] = (         void  *)0;
    sqlstm.sqinds[4] = (         int  )0;
    sqlstm.sqharm[4] = (unsigned int  )0;
    sqlstm.sqadto[4] = (unsigned short )0;
    sqlstm.sqtdso[4] = (unsigned short )0;
    sqlstm.sqhstv[5] = (         void  *)&n_motor_nev;
    sqlstm.sqhstl[5] = (unsigned int  )4;
    sqlstm.sqhsts[5] = (         int  )0;
    sqlstm.sqindv[5] = (         void  *)0;
    sqlstm.sqinds[5] = (         int  )0;
    sqlstm.sqharm[5] = (unsigned int  )0;
    sqlstm.sqadto[5] = (unsigned short )0;
    sqlstm.sqtdso[5] = (unsigned short )0;
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
}


  }

  /* Ennek a hatasa ugyis csak a kovetkezo EXEC SQL-nel ervenyesul */
  /* Mivel mindket agban van SAVEPOINT, mehet a 'goto sql_hiba_rlb'*/
  /* EXEC SQL WHENEVER SQLERROR goto sql_hiba_rlb ; */ 


  /* Ha mar van ilyen nevezesi lista felveve, mi a kulcsa ?        */
  if ( sqlca.sqlcode == -1 )
  {
    if ( cUPD_INS == 'U' )
    {
      cUPD_INS = 'H' ;
    }

    /* EXEC SQL SELECT kulcs INTO :n_uj_nvzl_kulcs FROM nevezesi_lista
               WHERE rajtszam  = :n_rajtszam   AND
                     versenyzo = :n_vers_nev   AND
                     nemzetiseg= :n_nemz_nev   AND
                     csapat    = :n_csapat_nev AND
                     motor     = :n_motor_nev ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "select kulcs into :b0  from nevezesi_lista where ((((rajt\
szam=:b1 and versenyzo=:b2) and nemzetiseg=:b3) and csapat=:b4) and motor=:b5)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )801;
    sqlstm.selerr = (unsigned short)1;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_uj_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_rajtszam;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_vers_nev;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )0;
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqhstv[3] = (         void  *)&n_nemz_nev;
    sqlstm.sqhstl[3] = (unsigned int  )4;
    sqlstm.sqhsts[3] = (         int  )0;
    sqlstm.sqindv[3] = (         void  *)0;
    sqlstm.sqinds[3] = (         int  )0;
    sqlstm.sqharm[3] = (unsigned int  )0;
    sqlstm.sqadto[3] = (unsigned short )0;
    sqlstm.sqtdso[3] = (unsigned short )0;
    sqlstm.sqhstv[4] = (         void  *)&n_csapat_nev;
    sqlstm.sqhstl[4] = (unsigned int  )4;
    sqlstm.sqhsts[4] = (         int  )0;
    sqlstm.sqindv[4] = (         void  *)0;
    sqlstm.sqinds[4] = (         int  )0;
    sqlstm.sqharm[4] = (unsigned int  )0;
    sqlstm.sqadto[4] = (unsigned short )0;
    sqlstm.sqtdso[4] = (unsigned short )0;
    sqlstm.sqhstv[5] = (         void  *)&n_motor_nev;
    sqlstm.sqhstl[5] = (unsigned int  )4;
    sqlstm.sqhsts[5] = (         int  )0;
    sqlstm.sqindv[5] = (         void  *)0;
    sqlstm.sqinds[5] = (         int  )0;
    sqlstm.sqharm[5] = (unsigned int  )0;
    sqlstm.sqadto[5] = (unsigned short )0;
    sqlstm.sqtdso[5] = (unsigned short )0;
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
    if (sqlca.sqlcode < 0) goto sql_hiba_rlb;
}


  }

  /* innentol sikeres INSERT eseten is szukseg van UPDATE-nel viszont nem !             */
  /* H - hibas UPDATE, mert a fenti SELECT eredmenye megsemmisitene az UPDATE-et        */
  if ( cUPD_INS == 'H' ||  cUPD_INS == 'I' )
  {
    /* Uj, parhuzamos rajtsorrend bejegyzes felvetele az uj nevzl kulccsal              */
    /* , hogy legyen mire mutasson az UPDATE eredmenye a befuto / leggyorsabb_kor -ben  */
    /* EXEC SQL INSERT INTO rajtsorrend
                    SELECT szezon_eve,
                           nagydij_nev,
                           DECODE( nvzl_kulcs, :n_nvzl_kulcs, :n_uj_nvzl_kulcs),
                           helyezes,
                           perc, masodperc, ezrdmsdp,
                           megjegyzes
                      FROM rajtsorrend
                      WHERE nvzl_kulcs = :n_nvzl_kulcs AND
                            szezon_eve = :n_szez_eve ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "insert into rajtsorrend select szezon_eve ,nagydij_nev ,D\
ECODE(nvzl_kulcs,:b0,:b1) ,helyezes ,perc ,masodperc ,ezrdmsdp ,megjegyzes  fr\
om rajtsorrend where (nvzl_kulcs=:b0 and szezon_eve=:b3)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )840;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_uj_nvzl_kulcs;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )0;
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqhstv[3] = (         void  *)&n_szez_eve;
    sqlstm.sqhstl[3] = (unsigned int  )4;
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
    if (sqlca.sqlcode < 0) goto sql_hiba_rlb;
}



    /* A befuto tabla modositasa. */
    /* Mehet, mert az elozo utasitas letrehozta a szulo elemet */
    /* EXEC SQL UPDATE befuto
               SET nvzl_kulcs = :n_uj_nvzl_kulcs
               WHERE nvzl_kulcs = :n_nvzl_kulcs AND
                     szezon_eve = :n_szez_eve ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "update befuto  set nvzl_kulcs=:b0 where (nvzl_kulcs=:b1 a\
nd szezon_eve=:b2)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )871;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_uj_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_szez_eve;
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
    if (sqlca.sqlcode < 0) goto sql_hiba_rlb;
}



    /* A leggyorsabb_kor tabla atallitasa */
    /* EXEC SQL UPDATE leggyorsabb_kor
               SET nvzl_kulcs = :n_uj_nvzl_kulcs
               WHERE nvzl_kulcs = :n_nvzl_kulcs AND
                     szezon_eve = :n_szez_eve ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "update leggyorsabb_kor  set nvzl_kulcs=:b0 where (nvzl_ku\
lcs=:b1 and szezon_eve=:b2)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )898;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_uj_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_szez_eve;
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
    if (sqlca.sqlcode < 0) goto sql_hiba_rlb;
}



    /* Mivel a gyerek elemek mar at lettek allitva, a torles mehet */
    /* EXEC SQL DELETE FROM rajtsorrend
               WHERE nvzl_kulcs = :n_nvzl_kulcs AND
                     szezon_eve = :n_szez_eve ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "delete  from rajtsorrend  where (nvzl_kulcs=:b0 and szezo\
n_eve=:b1)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )925;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_szez_eve;
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
    if (sqlca.sqlcode < 0) goto sql_hiba_rlb;
}



    *pKulcs = n_uj_nvzl_kulcs ;

    /* Innen tudja a felhivo fv., hogy a masodik parameterben adat jon vissza */
    l_SQLCODE = VE_UJ_NVZL_SOR ;
  }

  return l_SQLCODE ;

sql_hiba_rlb :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL ROLLBACK TO SAVEPOINT uj_nvzl_sor_sp ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "rollback  to uj_nvzl_sor_sp";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )948;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

long CNevezLista::SQL_NvzlKlcsDELETE( unsigned uiKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 

 
    unsigned n_nvzl_kulcs ;
 
  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND GOTO sql_hiba ; */ 


  n_nvzl_kulcs = uiKulcs ;

  /* Ha a kovetkezo utasitas hibara fut, az azt jelenti, hogy mas      */
  /* szezonban vagy az adott szezonban kihagyassal elofordul a kerdeses*/
  /* nevezesi lista kulcs. Ez nem hiba, mert az adott szezonban nem    */
  /* jelenik meg tobbet ez a nvzl sor                                  */
  /* EXEC SQL DELETE FROM nevezesi_lista
             WHERE kulcs = :n_nvzl_kulcs ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "delete  from nevezesi_lista  where kulcs=:b0";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )963;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
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
  if (sqlca.sqlcode == 1403) goto sql_hiba;
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CNevezLista::SQL_RszBolNvzlElk(unsigned uiNagydij)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve  ;
    unsigned n_nagydij ;

    char c_nagydij_datum[13] ;

    struct sql_s_rsz_nd
    {
      unsigned n_rajtszam   ;
      unsigned n_nvzl_kulcs ;
    } rsz_nd_rekord[600] ;
    /* 600, mert minden rajtsorrend bejegyzeshez kapunk egy elemet */
    /* egy szezonban korulbelul 500-550 elem varhato               */

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  int i ;
  int nFetch ;
  int nCiklFelsHat ;

  unsigned* pNvzlKlcs ;
  unsigned  uiElozoRsz, uiElozoNvzlKlcs ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 


  n_szezon_eve = m_uiSzezon ;
  n_nagydij    = uiNagydij  ;

  /* EXEC SQL SELECT datum INTO :c_nagydij_datum
             FROM nagydijak
             WHERE szezon_eve  = :n_szezon_eve AND
                   nagydij_nev = :n_nagydij ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select datum into :b0  from nagydijak where (szezon_eve=:b1\
 and nagydij_nev=:b2)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )982;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)c_nagydij_datum;
  sqlstm.sqhstl[0] = (unsigned int  )13;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_nagydij;
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



  /* nd.datum, */
  /* EXEC SQL DECLARE sel_rsz_nd_kurz  CURSOR FOR
    SELECT nvzl.rajtszam, nvzl.kulcs
      FROM nagydijak nd, nevezesi_lista nvzl, rajtsorrend rs
      WHERE rs.szezon_eve = :n_szezon_eve  AND
            nd.szezon_eve = :n_szezon_eve  AND
            rs.nvzl_kulcs = nvzl.kulcs     AND
            rs.nagydij_nev= nd.nagydij_nev AND  /o  \/ ??? o/
            nd.datum <= TO_DATE(:c_nagydij_datum, 'YYYY-MM-DD')
      ORDER BY nvzl.rajtszam, nd.datum ; */ 


  /* EXEC SQL OPEN sel_rsz_nd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = sq0037;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )1009;
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
  sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)c_nagydij_datum;
  sqlstm.sqhstl[2] = (unsigned int  )13;
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
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  nFetch = 0 ;

  /* EXEC SQL FETCH sel_rsz_nd_kurz INTO :rsz_nd_rekord ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )600;
  sqlstm.offset = (unsigned int  )1036;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&rsz_nd_rekord->n_rajtszam;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_rsz_nd);
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqharc[0] = (unsigned int   *)0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&rsz_nd_rekord->n_nvzl_kulcs;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_rsz_nd);
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
  if (sqlca.sqlcode == 1403) goto sql_hiba;
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  if ( sqlca.sqlcode == 0 )
  {
    nCiklFelsHat = sqlca.sqlerrd[2] - 2 ;

    for ( i = 0 ; i < nCiklFelsHat ; i++ )
    {
      if ( rsz_nd_rekord[i].n_rajtszam != rsz_nd_rekord[i+1].n_rajtszam )
      {
        pNvzlKlcs = new unsigned ;

        if ( pNvzlKlcs != NULL )
        {
          *pNvzlKlcs = rsz_nd_rekord[i].n_nvzl_kulcs ;
          m_cRszElsNdMap.SetAt( WORD(rsz_nd_rekord[i].n_rajtszam), pNvzlKlcs) ;
        }
        else
        {
          goto sql_hiba ;
        }
      }
    }

    /* EXEC SQL WHENEVER NOTFOUND do break ; */ 


    nFetch = sqlca.sqlerrd[2] ;

    while ( TRUE )
    {
      /* Biztositani kell az egyes FETCH-ekkel elhozott utolso es a kovetkezovel */
      /* elhozott eleje kozti atmenetet :                                        */
      uiElozoRsz = rsz_nd_rekord[i].n_rajtszam ;
      uiElozoNvzlKlcs = rsz_nd_rekord[i].n_nvzl_kulcs ;

      /* EXEC SQL FETCH sel_rsz_nd_kurz INTO :rsz_nd_rekord ; */ 

{
      struct sqlexd sqlstm;

      sqlstm.sqlvsn = 10;
      sqlstm.arrsiz = 6;
      sqlstm.sqladtp = &sqladt;
      sqlstm.sqltdsp = &sqltds;
      sqlstm.iters = (unsigned int  )600;
      sqlstm.offset = (unsigned int  )1059;
      sqlstm.cud = sqlcud0;
      sqlstm.sqlest = (unsigned char  *)&sqlca;
      sqlstm.sqlety = (unsigned short)0;
      sqlstm.sqhstv[0] = (         void  *)&rsz_nd_rekord->n_rajtszam;
      sqlstm.sqhstl[0] = (unsigned int  )4;
      sqlstm.sqhsts[0] = (         int  )sizeof(struct sql_s_rsz_nd);
      sqlstm.sqindv[0] = (         void  *)0;
      sqlstm.sqinds[0] = (         int  )0;
      sqlstm.sqharm[0] = (unsigned int  )0;
      sqlstm.sqharc[0] = (unsigned int   *)0;
      sqlstm.sqadto[0] = (unsigned short )0;
      sqlstm.sqtdso[0] = (unsigned short )0;
      sqlstm.sqhstv[1] = (         void  *)&rsz_nd_rekord->n_nvzl_kulcs;
      sqlstm.sqhstl[1] = (unsigned int  )4;
      sqlstm.sqhsts[1] = (         int  )sizeof(struct sql_s_rsz_nd);
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
      if (sqlca.sqlcode == 1403) break;
      if (sqlca.sqlcode < 0) goto sql_hiba;
}



      if ( uiElozoRsz != rsz_nd_rekord[0].n_rajtszam )
      {
        pNvzlKlcs = new unsigned ;

        if ( pNvzlKlcs != NULL )
        {
          *pNvzlKlcs = uiElozoNvzlKlcs ;
          m_cRszElsNdMap.SetAt( WORD( uiElozoRsz), pNvzlKlcs) ;
        }
        else
        {
          goto sql_hiba ;
        }
      }

      nCiklFelsHat = sqlca.sqlerrd[2] - nFetch - 2 ;

      /* i=0, mert a 0.-at is ossze kell hasonlitani az 1.-vel !                 */
      for ( i = 0 ; i < nCiklFelsHat ; i++)
      {
        if ( rsz_nd_rekord[i].n_rajtszam != rsz_nd_rekord[i+1].n_rajtszam )
        {
          pNvzlKlcs = new unsigned ;

          if ( pNvzlKlcs != NULL )
          {
            *pNvzlKlcs = rsz_nd_rekord[i].n_nvzl_kulcs ;
            m_cRszElsNdMap.SetAt( WORD(rsz_nd_rekord[i].n_rajtszam), pNvzlKlcs) ;
          }
          else
          {
            goto sql_hiba ;
          }
        }
      }

      nFetch = sqlca.sqlerrd[2] ;
    } /* while */

    /* Ide ugrik a while-ciklusbol, ha nem tud teljes tombnyire valot elhozni */
    if ( nFetch - sqlca.sqlerrd[2] != 0 )
    {
      if ( uiElozoRsz != rsz_nd_rekord[0].n_rajtszam )
      {
        pNvzlKlcs = new unsigned ;

        if ( pNvzlKlcs != NULL )
        {
          *pNvzlKlcs = uiElozoNvzlKlcs ;
          m_cRszElsNdMap.SetAt( WORD( uiElozoRsz), pNvzlKlcs) ;
        }
        else
        {
          goto sql_hiba ;
        }
      }
    }
    else
    {
      /* Ha nincs tobb, akkor az utolsot biztosan fel kell venni ! */
      pNvzlKlcs = new unsigned ;

      if ( pNvzlKlcs != NULL )
      {
        *pNvzlKlcs = uiElozoNvzlKlcs ;
        m_cRszElsNdMap.SetAt( WORD( uiElozoRsz), pNvzlKlcs) ;
      }
      else
      {
        goto sql_hiba ;
      }
    }
  } /* if teljes tomb eloszorre */

  nCiklFelsHat = sqlca.sqlerrd[2] - nFetch - 2 ;

  for ( i = 0 ; i < nCiklFelsHat ; i++)
  {
    if ( rsz_nd_rekord[i].n_rajtszam != rsz_nd_rekord[i+1].n_rajtszam )
    {
      pNvzlKlcs = new unsigned ;

      if ( pNvzlKlcs != NULL )
      {
        *pNvzlKlcs = rsz_nd_rekord[i].n_nvzl_kulcs ;
        m_cRszElsNdMap.SetAt( rsz_nd_rekord[i].n_rajtszam, pNvzlKlcs) ;
      }
      else
      {
        goto sql_hiba ;
      }
    }
  }

  /* Ha az elozo for-ciklus dolgozott, el kell varrni a veget */
  nCiklFelsHat = sqlca.sqlerrd[2] - nFetch ;

  if ( nCiklFelsHat != 0 )
  {
    /* Az utolsot biztosan fel kell venni ! */
    pNvzlKlcs = new unsigned ;

    if ( pNvzlKlcs != NULL )
    {
      *pNvzlKlcs = uiElozoNvzlKlcs ;
      m_cRszElsNdMap.SetAt( WORD( uiElozoRsz), pNvzlKlcs) ;
    }
    else
    {
      goto sql_hiba ;
    }
  }

  /* EXEC SQL CLOSE sel_rsz_nd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )1082;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  /* EXEC SQL CLOSE sel_rsz_nd_kurz ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )1097;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlcxt((void **)0, &sqlctx, &sqlstm, &sqlfpn);
}



  return l_SQLCODE ;
}

long CNevezLista::SQL_RszNdBolNvzl( unsigned uiRsz, unsigned uiNd, unsigned& uiNvzlstKulcs)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_rajtszam ;
    unsigned n_szezon_eve ;
    unsigned n_nagydij ;

    unsigned n_nvzl_kulcs ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND GOTO sql_hiba ; */ 


  n_rajtszam   = uiRsz      ;
  n_szezon_eve = m_uiSzezon ;
  n_nagydij    = uiNd ;

  n_nvzl_kulcs = 0 ;

  /* EXEC SQL EXECUTE
    BEGIN
      forma1.rsz_nd_bol_nvzl( :n_rajtszam, :n_szezon_eve, :n_nagydij, :n_nvzl_kulcs) ;
    END ;
  END-EXEC; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "begin forma1 . rsz_nd_bol_nvzl ( :n_rajtszam , :n_szezon_ev\
e , :n_nagydij , :n_nvzl_kulcs ) ; END ;";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )1112;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_rajtszam;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_szezon_eve;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_nagydij;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&n_nvzl_kulcs;
  sqlstm.sqhstl[3] = (unsigned int  )4;
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
  if (sqlca.sqlcode == 1403) goto sql_hiba;
  if (sqlca.sqlcode < 0) goto sql_hiba;
}



  uiNvzlstKulcs = n_nvzl_kulcs ;

  l_SQLCODE = sqlca.sqlcode ;

  SQL_SQLCAFeltolt() ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}

long CNevezLista::SQL_NvzlKulcsMeghat( unsigned& uiNvzlKulcs ,
                                       unsigned  uiNagydij   ,
                                       CNvzSor*  pNvzSor)
{
  /* EXEC SQL BEGIN DECLARE SECTION ; */ 


    unsigned n_szezon_eve ;
    unsigned n_nagydij ;

    unsigned n_rajtszam   ;
    unsigned n_vers_nev   ;
    unsigned n_nemz_nev   ;
    unsigned n_csapat_nev ;
    unsigned n_motor_nev  ;

    unsigned n_nvzl_kulcs ;

  /* EXEC SQL END DECLARE SECTION ; */ 


  long l_SQLCODE ;

  /* EXEC SQL WHENEVER SQLERROR GOTO sql_hiba ; */ 

  /* EXEC SQL WHENEVER NOTFOUND CONTINUE ; */ 


  n_szezon_eve = m_uiSzezon ;
  n_nagydij    = uiNagydij  ;

  n_rajtszam   = pNvzSor->uiRajtszam  ;
  n_vers_nev   = pNvzSor->uiVersenyzo ;
  n_nemz_nev   = pNvzSor->uiNemz      ;
  n_csapat_nev = pNvzSor->uiCsapat    ;
  n_motor_nev  = pNvzSor->uiMotor     ;

  /* Szerepel-e mar ez az osszeallitas, ha igen mi a kulcsa ? */
  /* EXEC SQL SELECT kulcs
             INTO :n_nvzl_kulcs
             FROM nevezesi_lista
             WHERE rajtszam = :n_rajtszam     AND
                   versenyzo = :n_vers_nev    AND
                   nemzetiseg = :n_nemz_nev   AND
                   csapat     = :n_csapat_nev AND
                   motor      = :n_motor_nev ; */ 

{
  struct sqlexd sqlstm;

  sqlstm.sqlvsn = 10;
  sqlstm.arrsiz = 6;
  sqlstm.sqladtp = &sqladt;
  sqlstm.sqltdsp = &sqltds;
  sqlstm.stmt = "select kulcs into :b0  from nevezesi_lista where ((((rajtsz\
am=:b1 and versenyzo=:b2) and nemzetiseg=:b3) and csapat=:b4) and motor=:b5)";
  sqlstm.iters = (unsigned int  )1;
  sqlstm.offset = (unsigned int  )1143;
  sqlstm.selerr = (unsigned short)1;
  sqlstm.cud = sqlcud0;
  sqlstm.sqlest = (unsigned char  *)&sqlca;
  sqlstm.sqlety = (unsigned short)0;
  sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
  sqlstm.sqhstl[0] = (unsigned int  )4;
  sqlstm.sqhsts[0] = (         int  )0;
  sqlstm.sqindv[0] = (         void  *)0;
  sqlstm.sqinds[0] = (         int  )0;
  sqlstm.sqharm[0] = (unsigned int  )0;
  sqlstm.sqadto[0] = (unsigned short )0;
  sqlstm.sqtdso[0] = (unsigned short )0;
  sqlstm.sqhstv[1] = (         void  *)&n_rajtszam;
  sqlstm.sqhstl[1] = (unsigned int  )4;
  sqlstm.sqhsts[1] = (         int  )0;
  sqlstm.sqindv[1] = (         void  *)0;
  sqlstm.sqinds[1] = (         int  )0;
  sqlstm.sqharm[1] = (unsigned int  )0;
  sqlstm.sqadto[1] = (unsigned short )0;
  sqlstm.sqtdso[1] = (unsigned short )0;
  sqlstm.sqhstv[2] = (         void  *)&n_vers_nev;
  sqlstm.sqhstl[2] = (unsigned int  )4;
  sqlstm.sqhsts[2] = (         int  )0;
  sqlstm.sqindv[2] = (         void  *)0;
  sqlstm.sqinds[2] = (         int  )0;
  sqlstm.sqharm[2] = (unsigned int  )0;
  sqlstm.sqadto[2] = (unsigned short )0;
  sqlstm.sqtdso[2] = (unsigned short )0;
  sqlstm.sqhstv[3] = (         void  *)&n_nemz_nev;
  sqlstm.sqhstl[3] = (unsigned int  )4;
  sqlstm.sqhsts[3] = (         int  )0;
  sqlstm.sqindv[3] = (         void  *)0;
  sqlstm.sqinds[3] = (         int  )0;
  sqlstm.sqharm[3] = (unsigned int  )0;
  sqlstm.sqadto[3] = (unsigned short )0;
  sqlstm.sqtdso[3] = (unsigned short )0;
  sqlstm.sqhstv[4] = (         void  *)&n_csapat_nev;
  sqlstm.sqhstl[4] = (unsigned int  )4;
  sqlstm.sqhsts[4] = (         int  )0;
  sqlstm.sqindv[4] = (         void  *)0;
  sqlstm.sqinds[4] = (         int  )0;
  sqlstm.sqharm[4] = (unsigned int  )0;
  sqlstm.sqadto[4] = (unsigned short )0;
  sqlstm.sqtdso[4] = (unsigned short )0;
  sqlstm.sqhstv[5] = (         void  *)&n_motor_nev;
  sqlstm.sqhstl[5] = (unsigned int  )4;
  sqlstm.sqhsts[5] = (         int  )0;
  sqlstm.sqindv[5] = (         void  *)0;
  sqlstm.sqinds[5] = (         int  )0;
  sqlstm.sqharm[5] = (unsigned int  )0;
  sqlstm.sqadto[5] = (unsigned short )0;
  sqlstm.sqtdso[5] = (unsigned short )0;
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



  /* Ha nincs, akkor lehet felvenni */
  if ( sqlca.sqlcode == 1403 )
  {
    /* Az uj bejegyzes uj kulcsa */
    /* EXEC SQL SELECT seq_nvzl.NEXTVAL INTO :n_nvzl_kulcs FROM seq ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "select seq_nvzl.nextval  into :b0  from seq ";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )1182;
    sqlstm.selerr = (unsigned short)1;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
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



    /* EXEC SQL INSERT INTO nevezesi_lista
               VALUES ( :n_nvzl_kulcs,
                        :n_rajtszam,
                        :n_vers_nev,
                        :n_nemz_nev,
                        :n_csapat_nev,
                        :n_motor_nev
                      ) ; */ 

{
    struct sqlexd sqlstm;

    sqlstm.sqlvsn = 10;
    sqlstm.arrsiz = 6;
    sqlstm.sqladtp = &sqladt;
    sqlstm.sqltdsp = &sqltds;
    sqlstm.stmt = "insert into nevezesi_lista values (:b0,:b1,:b2,:b3,:b4,:b\
5)";
    sqlstm.iters = (unsigned int  )1;
    sqlstm.offset = (unsigned int  )1201;
    sqlstm.cud = sqlcud0;
    sqlstm.sqlest = (unsigned char  *)&sqlca;
    sqlstm.sqlety = (unsigned short)0;
    sqlstm.sqhstv[0] = (         void  *)&n_nvzl_kulcs;
    sqlstm.sqhstl[0] = (unsigned int  )4;
    sqlstm.sqhsts[0] = (         int  )0;
    sqlstm.sqindv[0] = (         void  *)0;
    sqlstm.sqinds[0] = (         int  )0;
    sqlstm.sqharm[0] = (unsigned int  )0;
    sqlstm.sqadto[0] = (unsigned short )0;
    sqlstm.sqtdso[0] = (unsigned short )0;
    sqlstm.sqhstv[1] = (         void  *)&n_rajtszam;
    sqlstm.sqhstl[1] = (unsigned int  )4;
    sqlstm.sqhsts[1] = (         int  )0;
    sqlstm.sqindv[1] = (         void  *)0;
    sqlstm.sqinds[1] = (         int  )0;
    sqlstm.sqharm[1] = (unsigned int  )0;
    sqlstm.sqadto[1] = (unsigned short )0;
    sqlstm.sqtdso[1] = (unsigned short )0;
    sqlstm.sqhstv[2] = (         void  *)&n_vers_nev;
    sqlstm.sqhstl[2] = (unsigned int  )4;
    sqlstm.sqhsts[2] = (         int  )0;
    sqlstm.sqindv[2] = (         void  *)0;
    sqlstm.sqinds[2] = (         int  )0;
    sqlstm.sqharm[2] = (unsigned int  )0;
    sqlstm.sqadto[2] = (unsigned short )0;
    sqlstm.sqtdso[2] = (unsigned short )0;
    sqlstm.sqhstv[3] = (         void  *)&n_nemz_nev;
    sqlstm.sqhstl[3] = (unsigned int  )4;
    sqlstm.sqhsts[3] = (         int  )0;
    sqlstm.sqindv[3] = (         void  *)0;
    sqlstm.sqinds[3] = (         int  )0;
    sqlstm.sqharm[3] = (unsigned int  )0;
    sqlstm.sqadto[3] = (unsigned short )0;
    sqlstm.sqtdso[3] = (unsigned short )0;
    sqlstm.sqhstv[4] = (         void  *)&n_csapat_nev;
    sqlstm.sqhstl[4] = (unsigned int  )4;
    sqlstm.sqhsts[4] = (         int  )0;
    sqlstm.sqindv[4] = (         void  *)0;
    sqlstm.sqinds[4] = (         int  )0;
    sqlstm.sqharm[4] = (unsigned int  )0;
    sqlstm.sqadto[4] = (unsigned short )0;
    sqlstm.sqtdso[4] = (unsigned short )0;
    sqlstm.sqhstv[5] = (         void  *)&n_motor_nev;
    sqlstm.sqhstl[5] = (unsigned int  )4;
    sqlstm.sqhsts[5] = (         int  )0;
    sqlstm.sqindv[5] = (         void  *)0;
    sqlstm.sqinds[5] = (         int  )0;
    sqlstm.sqharm[5] = (unsigned int  )0;
    sqlstm.sqadto[5] = (unsigned short )0;
    sqlstm.sqtdso[5] = (unsigned short )0;
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



    uiNvzlKulcs = n_nvzl_kulcs ;

    SQL_SQLCAFeltolt() ;

    return 1 ;
  }

  uiNvzlKulcs = n_nvzl_kulcs ;

  l_SQLCODE = sqlca.sqlcode ;

  SQL_SQLCAFeltolt() ;

  return l_SQLCODE ;

sql_hiba :

  /* EXEC SQL WHENEVER SQLERROR CONTINUE ; */ 


  SQL_SQLCAFeltolt() ;

  l_SQLCODE = sqlca.sqlcode ;

  return l_SQLCODE ;
}
