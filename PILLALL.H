// pillall.h

// A pillanatnyi allasra vonatkozo lekerdesek eredmenyet tartalmazo osztaly
// Mellekszerepe a CNyVBOBej (vo.:CNyersVBOssz) osztalyokban nvzlst kulcs
// szerint elocsoportositott eredmenyeket az adott lekerdezesben aktualis
// nevfajta kulcsa (pl. motorhoz tartozhat akar egyidoben is tobb nvzlst
// kulcs) szerint atcsoportositva es rendezve a CVBSzerepl osztalynak atadni
class CPillAll : public CBazKerEredm
{
  DECLARE_SERIAL(CPillAll)
public :
  // Default konstruktor a serialize kepesseghez
  CPillAll() ;

public :
  CNevEredmMezo m_cVersMez ;
  CNevEredmMezo m_cNemzMez ;
  CNevEredmMezo m_cCsapMez ;
  CNevEredmMezo m_cMotorMez;
  CNevEredmMezo m_cPalyaMez;
  CDatEredmMezo m_cDatumMez;
  // Ennek van(tobb)ertelme, mint az CIdoerEredmMezo-nek
  CHelyezesMezo m_cHelyezMez ;
  // A pontozas mezo mindig a tenyleges pontozas duplajat tartalmazza
  // a unsigned short CBazEredmMezo::m_usKmltErt es fele pontszam nem teljes
  // lefutott futamhossz eseten (fele pont) ??? m_cPontMez <-> m_cHelyezMez
  CBazEredmMezo   m_cPontMez ;

public :

  void Feltolt( unsigned uiVers , unsigned uiNemz, unsigned uiCsap, unsigned uiMotor,
                unsigned uiNagyd, unsigned uiPalya, SDatum& sDatum,
                unsigned short usPontszam, unsigned char ucHelyezes,
                eNevtipus enNevtipus) ;

  // A m_cHelyezMez tartalmat hozzaadjuk a gyokerelemehez (az osszpontszamot
  // nehez lenne kiszamitani max. legj. != osszes futam esetben)
  void UjFelvesz(CPillAll* pUjElem) ;
  void Kirajzol(CDC* pDC, int& nKezdY) ;
  void ScrollMeret(int& x, int& y) ;

  // A fv. az teszteli, hogy az eger valamely ki/be kapcsolhato mezo folott
  // van-e, es ennek megfeleloen kapcsolja ki/be a mezo ala rendelteket
  // A FolotteVan es KiBeKapcsol fv-ek egybevonasa
  // talalat eseten
  // FALSE: a kurzor allapotat jelzo gl_bKezKurzorBe valtozot allitja
  // TRUE : ezen felul az alarendelt szinteket kapcsolja
  BOOL EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol) ;

  virtual int AzMezok(char cAktSzint) ;

  // A max ertekelt legjobb futam miatt sokkal egyszerubb az osszes adat
  // ismereteben elvegezni(CHelyezesMezo::CBazEredmMezo m_pHelySzamTbl).
  // A parameterkent atadott szamu legjobb befutoeredmeny vagy kevesebb szamit be.
  // ucElsLegj == 0 eseten mind beszamit (pl.a nevtipus != versenyzonev).
  void OsszpntKisz( unsigned char ucElsLegj) ;
  // Az adott helyezes darabszamat rekurzivan (1.m_pKovSzKovElem 2.m_pAzSzKovElem)
  // hatarozza meg.
  unsigned char HelyzSzam(unsigned char ucHelyezes) ;
  // Mivel a CVBSzerepl osztaly szamara az elofeldolgozas soran szukseges
  // a csoportositas sorrendjenek megvaltoztatasa (igaz, hogy csak a 'furt'
  // gyokerelemenel)
  void SzintMegad(char  cVersSzint,  char cNemzSzint, char cCsapSzint,
                  char cMotorSzint, char cPalyaSzint, char cPontSzint,
                  char cDatumSzint, char cIdoeredmSzint) ;

  // A CPillAll tomb qsort-tal valo rendezesehez kell.
  // Az OsszpntKisz fv elozetes felhivasat feltetelezi !
  friend int PAOsszehas(const void* pCPillAll1, const void* pCPillAll2) ;

  void Serialize(CArchive& ar) ;
  // Az Output ablakba kiirja az osztaly tartalmat
  void Trace() ;
  ~CPillAll() ;
} ;