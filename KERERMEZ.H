// kerermez.h - a lekerdezesek eredmenye

// A CVBSzereplMezo-ben ilyen lepesekben tortenik a foglalas
#define HELY_BLOKKMERET 10
#define DAT_BLOKKMERET   5

// A kiemelt jelentosegu szintek neve
#define SZ_KEZDOSZINT 1
#define SZ_NEM_SZAMIT 0

#define KAPCSOLO      0xf0
// if ( kapcsol ) m_cKibe ^ KIBE
#define KIBE          0x0f
// if ( m_cKibe & BEKAPCS )
#define BEKAPCS       0x0f

// l. CBazKerEredm::AzMezok visszateresi ertekei (+CBazKerEredm::Kirajzol)
#define NEM_TART_HOZZA 0
#define HOZZA_TART     1
#define KOZOMBOS_SZINT 2

class CBazEredmMezo : public CObject
{
  DECLARE_SERIAL(CBazEredmMezo)
public :
  CBazEredmMezo() ;

public:
  char  m_cSzint ;
  unsigned short m_usKmltErt ;
  // Ettol a mezotol kezdve az alarendeltek az 'eger altal' ki vannak kapcsolva
  // 0 - ki , 1 - be
  char  m_cKiBe  ;
  // Minden egyes elem (mely mezoket tartalmaz) megprobalja kirajzolni magat
  // az elso szinttol az utolsoig. A mar -mas elemek- altal kirajzolt, kozos
  // mezok teves megjeleniteset akadalyozza meg ez a "BOOLean" adattag: 0, 1
  char  m_cKirajzol ;
  // A mezo kepernyon elfoglalt meretet adja meg, mely az eger es a mezo 
  // kolcsonos helyzenek tesztelesere szolgal (folotte van-e vagy sem).
  CRect m_cMezoRect ;
public:
//    virtual BOOL Kirajzol(CDC* pDC, int& nKezdY) ;
  virtual void Serialize(CArchive& ar) ;
  ~CBazEredmMezo() ;
} ;

class CNevEredmMezo : public CBazEredmMezo
{
  DECLARE_SERIAL(CNevEredmMezo)

public :
  CNevEredmMezo() ;

public:
  // m_cPalyaMez eseten a nagydij es a palyanak ket kulcsnyi hely kell
  // m_cPalyaMez : m_uiKulcs1 -> nagydijnev
  unsigned  m_uiKulcs1 ;
  // m_cPalyaMez : m_uiKulcs2 -> palya nev
  unsigned  m_uiKulcs2 ;

public :
  // A visszateresi ertek jelzi, hogy kirajzolta-e magat
//    BOOL Kirajzol(CDC* pDC, int& nKezdY) ;
  void Serialize(CArchive& ar) ;
  ~CNevEredmMezo() ;
} ;

class CDatEredmMezo : public CBazEredmMezo
{
  DECLARE_SERIAL(CDatEredmMezo)

public:
  CDatEredmMezo() ;

public:
  SDatum m_sDatum ;

public:
  void Serialize(CArchive& ar) ;

public:
  ~CDatEredmMezo() ;
} ;

class CIdoerEredmMezo : public CBazEredmMezo
{
  DECLARE_SERIAL(CIdoerEredmMezo)

public:
  CIdoerEredmMezo() ;

public:
  CIdoeredm m_cIdoeredm ;
  unsigned short m_usMegj ;

public:
  // m_usMegj fv.-ben korhatrany vagy idoeredmenykent irja ki 
  CString IdoerS() ;

  void Serialize(CArchive& ar) ;

public:
  ~CIdoerEredmMezo() ;
} ;

class CHelyezesMezo : public CBazEredmMezo
{
  DECLARE_SERIAL(CHelyezesMezo)
public :
  CHelyezesMezo() ;
//    CHelyezesMezo(unsigned char ucHelyezes) ;
public:
  unsigned char m_ucMeret ;
//    CVBSzerMezBej* m_pVBSzerTbl ;
  DWORD m_dwSajHlyzPnt ;
  // HIWORD: az adott, egyetlen helyezesert jaro pont-a fele pontszamok miatt
  // LOWORD: HI: n-ik helyezes(0xff-n!!!), LO: m db n-ik helyezes
  // A pontozas csokkeno sorrendben
  DWORD* m_pHelySzamTbl ;

public:
  // A helyezest '255-n' kent veszi fel a rendezes miatt !!!
  BOOL UjFelvesz(unsigned short usPontszam, unsigned char ucHelyezes) ;
  // A CPillAll, CVBSzerepl osztalyok UjFelvesz fv-e szamara
  // A CVBSzerepl::EredetiHelyz() kihasznalja, hogy a pontszam csak az eredeti
  // (nem kmlt) helyezesnel 1, igy a pontszam masolasa hiba vo.:BOOL parameter
  BOOL UjFelvesz(DWORD* pHelySzamTbl, unsigned char ucMeret, BOOL bPontMasol) ;
// A parameterkent atadott szamu legjobb befutoeredmeny szamit be.
// Az atadott parameter nem lehet 0 !
// unsigned short OsszpntKisz( unsigned char ucElsLegj) ;
  // A megadott helyezes darabszamat adja meg
  unsigned char HelyzSzam(unsigned char ucHelyezes) ;
  // A fv. a mezo 'eredeti' helyezeset adja meg CVBSzerepl osztalyban: pont=1
  // (az esetleges kmlt. helyezesek miatt)
  unsigned char SajatHelyz() ;
  WORD SajatPont()  ;
  void Kiurit() ;
  void Serialize(CArchive& ar) ;
  ~CHelyezesMezo() ;
} ;

// Bazisosztaly, melybol majd az egyes kulonbozo lekerdezesi tipusokat
// szarmaztatom le
class CBazKerEredm : public CObject
{
  DECLARE_SERIAL(CBazKerEredm)

public :
  // Default konstruktor a serialize kepesseghez
  CBazKerEredm() ;

public :
  // Az aktualis elem szintje, ahova be van akasztva. Pl. az ennel magasabb
  // szintu mezoket kell csak kijelezni, amikor ra kerul a sor.
  char m_cAktSzint ;

  // Az osztalyhoz tartozo kiirt adatokat befoglalo teglalap
  // (a kurzor vizsgalatahoz szukseges a view-en belul)
  CRect m_cTeglalap ;
  // Hogy ne csak csoportositani, hanem rendezni is lehessen
//  CBazKerEredm* m_pElozoElem ;
  // Az aktualis elemmel azonos szinten levo kovetkezo elem
  CBazKerEredm* m_pAzSzKovElem ;
  // Az aktualis  elemnel egyel alacsonyabb szinten levo kovetkezo 'gyerek' elem
  CBazKerEredm* m_pKovSzKovElem ;

public :
  // fuggvenyek, melyek a leszarmaztatott osztalyokban kulonbozokeppen lesznek
  // definialva
  virtual void UjFelvesz(CBazKerEredm* pUjElem) ;
  virtual void Kirajzol(CDC* pDC, int& nKezdY) ;
  virtual void ScrollMeret(int& x, int& y) ;
  virtual BOOL EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol) ;
  // A kovetkezo esetek megkulonboztetesere (kikapcsolt allapot mire vonatkozzon)
  // 1. versenyzo1 datum1 stb1   <->   2.versenyzo1 datum1 stb1
  //    versenyzo1 datum1 stb2           versenyzo1 datum2 stb2
  // A datum mezo kikapcsolasa 1. mindket stb mezore, mig 2. csak stb1-re vonatkozik
  // Az adott elem es a m_pKovSzKovElem mely szintig azonos
  virtual int  AzMezok(char cAktSzint) ;
  virtual void Felszabadit() ;
  virtual void Serialize(CArchive& ar) ;

public :
  ~CBazKerEredm() ;
} ;