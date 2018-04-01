// alaptip.h - altalanosan hasznalt adattipusok :

// define - ok a CRBBejegyzes::unsigned short m_nMegj szamara 
#define IDOEREDMENY      0x0
#define KORHATRANY       0x1
#define CSAPAT_DISZKV    0x2
#define VERSENYZO_DISZKV 0x4
#define FELE_PONT        0x8

// A teljes nvzl meghatarozasahoz kell, hogy a nem kvalifikaltak a rs-ben szerepeljenek
// Elofordul (nem define-ken) az f1packag.sql-fileban
#define NEM_KVALIF       0x0010

#define KIESESI_OK_2     0x0020
#define KIESESI_OK_3     0x0040
#define KIESESI_OK_4     0x0080
// ...
#define KIESESI_OK_12    0xf000

// G L O B A L I S  A D A T O K

// A lekerdezesi eredmenyek megjelenitesenel az egerkurzor egyes mezok felett
// letenek ellenorzesehez hasznalom (nyil/kezkurzor)
extern BOOL gl_bKezKurzorBe ;

extern int gl_dX, gl_dY ;

// vo.: szez_all_b.sql !
enum eNevtipus
{
  eVersNt     = 0 ,
  eNemzNt     = 1 ,
  eCsapNt     = 2 ,
  eMotorNt    = 3 ,
  eNagydijNt  = 4 ,
  ePalyaNt    = 5 ,
  ePontNt     = 6 ,
  eErvtlnNt   = 7
} ;

// Tabulalas a lekerdezesi eredmenyek mezoi szamara
enum eTabstopTip
{
  eVers     = 0 ,
  eNemz     = 1 ,
  eCsap     = 2 ,
  eMotor    = 3 ,
  ePalya    = 4 ,
  eDatum    = 5 ,
  eHelyez   = 6 ,
  ePAHelyez = 7 ,
  eIdoeredm = 8 ,
  ePont     = 9
} ;

/* A sorrendet a dbglobal.pc gl_pLek2Tmb kihasznalja */
enum eLekerdTipus
{
  eFutgy    = 0,
  eEdzEls   = 1,
  eLeggyKor = 2,
  eOsszEr   = 3,
  eHolvers  = 4,
  eVBPillAll= 5,
  eVBSzer   = 6,
  eErvtlnLt = 7
} ;

/* dbglobal.h : SQL_LgGyKorAltLekerd() */
struct sLgGyKorLekErTip
{
  sLgGyKorLekErTip()
  {
    nRajtszam    = 0 ;
    nVersenyzo   = 0 ;
    nNemzetiseg  = 0 ;
    nCsapat      = 0 ;
    nMotor       = 0 ;
    nSzezonEve   = 0 ;
    nNagydijNev  = 0 ;
    nPerc        = 0 ;
    nMasodperc   = 0 ;
    nEzrdmsdperc = 0 ;
  }

  unsigned nRajtszam;
  unsigned nVersenyzo;
  unsigned nNemzetiseg;
  unsigned nCsapat;
  unsigned nMotor;
  unsigned nSzezonEve;
  unsigned nNagydijNev;
  unsigned nPerc;
  unsigned nMasodperc;
  unsigned nEzrdmsdperc;
/*
  unsigned nKorben;
  float    fAtlSebesseg; nem lesz ugysem kijelezve */
} ;

/* dbglobal.h : SQL_RsBefAltLekerd()   */
struct sRsBefLekErTip
{
  sRsBefLekErTip()
  {
    uiRajtszam    = 0 ;
    uiVersenyzo   = 0 ;
    uiNemzetiseg  = 0 ;
    uiCsapat      = 0 ;
    uiMotor       = 0 ;
    uiSzezonEve   = 0 ;
    uiNagydijNev  = 0 ;
    uiOra         = 0 ;
    uiPerc        = 0 ;
    uiMasodperc   = 0 ;
    uiEzrdmsdperc = 0 ;
    uiMegjegyzes  = 0 ;
  }

  unsigned uiRajtszam;
  unsigned uiVersenyzo;
  unsigned uiNemzetiseg;
  unsigned uiCsapat;
  unsigned uiMotor;
  unsigned uiSzezonEve;
  unsigned uiNagydijNev;
/*  unsigned uiHelyezes;*/
  unsigned uiOra;
  unsigned uiPerc;
  unsigned uiMasodperc;
  unsigned uiEzrdmsdperc;
  unsigned uiMegjegyzes ;
} ;

/* dbglobal.h : SQL_HolVKiVLekerd()   */
struct sHolVKiVLekErTip
{
  sHolVKiVLekErTip()
  {
    uiVersenyzo  = 0 ;
    uiNemzetiseg = 0 ;
    uiCsapat     = 0 ;
    uiMotor      = 0 ;
    uiNagydijNev = 0 ;
    uiEv         = 0 ;
    uiHonap      = 0 ;
    uiNap        = 0 ;
  }

/*  unsigned uiRajtszam; */
  unsigned uiVersenyzo;
  unsigned uiNemzetiseg;
  unsigned uiCsapat;
  unsigned uiMotor;
/*  unsigned uiSzezonEve;*/
  unsigned uiNagydijNev;
/*  unsigned uiPalya ;*/
  unsigned uiEv;
  unsigned uiHonap;
  unsigned uiNap;
} ;

struct sPillAllLekErTip
{
  sPillAllLekErTip()
  {
    pNevKlcsTmb = NULL ;
    pNevHlyzTmb = NULL ;
    pSzezPntTmb = NULL ;
    uiSzezErTmbMeret = 0 ;

    pNevEredmTmb   = NULL ;
    pPontTmb       = NULL ;
    pHelyezesTmb   = NULL ;
    pNvzlKulcsTmb  = NULL ;
    pNdNevTmb      = NULL ;
    uiOsszErTmbMer = 0 ;
  }

  // A lenti tombok merete
  // != a kivalasztott nevek szamaval, ha a szezon pillanatnyi allasat kerdezi le
  // mert akkor 0 nev van kijelolve es az osszes ertekelt eredmenyt elhozza !
  unsigned  uiSzezErTmbMeret ;
  // Mindig elhozom a neveket :
  // 1. minden nevnel ugyis kell
  // 2. kiv nevek eseten nem mind er el eredmenyt
  unsigned* pNevKlcsTmb ;
  unsigned* pNevHlyzTmb ;
  unsigned* pSzezPntTmb ;

  // A beszamitott eredmenyek adatai
  unsigned  uiOsszErTmbMer ;

  unsigned* pNevEredmTmb   ;
  unsigned* pPontTmb       ;
  unsigned* pHelyezesTmb   ;
  unsigned* pNvzlKulcsTmb  ;
  unsigned* pNdNevTmb      ;

  void Felszabadit()
  {
    // A kimenoadatok felszabaditasa : remelem, amelyre nem tortent foglalas az NULL
    uiSzezErTmbMeret = 0 ;

    delete [] pNevKlcsTmb ;
    delete [] pNevHlyzTmb ;
    delete [] pSzezPntTmb ;

    pNevKlcsTmb = NULL ;
    pNevHlyzTmb = NULL ;
    pSzezPntTmb = NULL ;

    uiOsszErTmbMer = 0 ;

    delete [] pNevKlcsTmb   ;
    delete [] pNevEredmTmb  ;
    delete [] pPontTmb      ;
    delete [] pHelyezesTmb  ;
    delete [] pNvzlKulcsTmb ;
    delete [] pNdNevTmb     ;

    pNevKlcsTmb   = NULL ;
    pNevEredmTmb  = NULL ;
    pPontTmb      = NULL ;
    pHelyezesTmb  = NULL ;
    pNvzlKulcsTmb = NULL ;
    pNdNevTmb     = NULL ;
  }

  // Kiirja a struktura tartalmat 'TRACE'-szel
  void Trace() ;
} ;

extern char* gl_pNevTblTmb[8] ;
extern char* gl_pNevMezTmb[8] ;
extern char* gl_pSeqTmb[8] ;

extern char* gl_pLek1Tmb[3] ;
extern char* gl_pLek2Tmb[7] ;

// Nullazva a glTabsopTorol-ben
extern int gl_nTabstop[ePont+1] ;

extern inline void TabsopTorol() ;

extern void TabstopBeall( CDC* pDC, eTabstopTip eNevtipus) ;

extern void OsszTabstopBeall( CDC* pDC) ;

// A ID_FELVITEL_NVZLST-hez tartozo menupont elerhetoseget ezzel a valtozoval
// szabalyzom, hogy ne lehessen ket azonos dialogus ablak egyszerre a kepernyon
// l.
// extern BOOL gl_bFelvitelNvzlstAktiv ;

// L. mint fent ID_FELVITEL_RS_BEF
extern BOOL gl_bFelvitelRsBefAktiv ;

// Ha mar csatlakozva van, ne akarjon ujra ID_CSATL_ADATB
extern BOOL gl_bCsatlakozva ;

// Felhasznalja a CRsBefFelv, CLekerdDlg osztaly
enum eAblTipRsBef { RAJTS=IDD_RAJTS_FELV, BEFUTO=IDD_BEF_FELV } ;

#define ELSO_F1_SZEZ 1950

#define HIBA 65535

// A konyvtarstruktura sztringkonstansa
const CString sAlapkonyvtar("c:\\forma1\\") ;

// SDatum - az egyes nagydijak es nevek elofordulasi datumat tartalmazo struktura
// (esetleges muveletekkel)
struct SDatum
{
    SDatum(unsigned short ini_ev = 0, unsigned char  ini_honap = 0, unsigned char  ini_nap = 0)
    {
      ev    = ini_ev ;
      honap = ini_honap ;
      nap   = ini_nap ;
    } ;

    unsigned short ev ;
    unsigned char  honap ;
    unsigned char  nap ;

    BOOL operator  < (SDatum& Jobboldal) ;
    BOOL operator  > (SDatum& Jobboldal) ;
    BOOL operator == (SDatum& Jobboldal) ;

    BOOL operator >= (SDatum& Jobboldal) ;
    BOOL operator <= (SDatum& Jobboldal) ;
} ;