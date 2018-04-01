// cnev.h : Az osztaly neveket es konstansokat rendel ossze, ezen felul 
//          megadja a nev adatbazisbeli elso es utolso elofordulasat. 
//
/////////////////////////////////////////////////////////////////////////////

// CString m_sNev - a tarolt nev

struct nevadat
{
  nevadat(const CString& sIniNev="")
  {
    m_sNev = sIniNev ;
  } ;

  CString m_sNev ;
} ;

// A leszarmaztatas CMapPtrToPtr-bol tortenik CMapWordToPtr helyett,
// mert az adatbazisbol kiolvasott MUMBER(10), unsigned nem fer bele
// a 16 bites WORD-be
class CNev : public CMapPtrToPtr
{
public:
  DECLARE_DYNCREATE(CNev)

  CNev() ;
  CNev( eNevtipus enIniNevtip, int nBlokkMeret ) ;
  // A const kulcsszot kivettem az argumentumlistabol , mert kulonben :
  // 'GetNextAssoc' : cannot convert 'this' pointer from 'const class ::CNev __far *' to 'class ::CNev __far *const '
  CNev( CNev& cMasol) ;

// Attributes
public:
  // Megis kell, mert az ablakban nevfelvetelkor ideiglenes indexeket veszi
  // lokalisan fel: adatforgalom merseklese, indexsporolas (CANCEL)
  // A tenyleges indexeket az SQL_NevINSERT() fv. osztja ki
//  unsigned  m_uiMaxIndex ;
  eNevtipus m_enNevtip ;

public:
// Operations
  inline BOOL Lookup(unsigned uiKulcs, nevadat*& pNevAdat)
  {
    return CMapPtrToPtr::Lookup((void*)uiKulcs, (void*&) pNevAdat) ;
  } ;
  inline void SetAt(unsigned uiKulcs, nevadat* pNevAdat)
  {
    CMapPtrToPtr::SetAt((void*)uiKulcs, (void*) pNevAdat) ;
  } ;
  inline void GetNextAssoc(POSITION& rKovPoz, unsigned& uiKulcs, nevadat*&  pNevAdat)
  {
    CMapPtrToPtr::GetNextAssoc(rKovPoz, (void*&)uiKulcs, (void*&) pNevAdat) ;
  } ;

  // Az importalas soran ennek a fv.-nek a feladata a beolvasott sztring
  // beazonositasa
  BOOL NevbolKulcs(const CString& sKeresett, unsigned& uiTalKulcs) ;

  void RemoveAll() ;
  BOOL RemoveKey(unsigned uiKulcs) ;

  // Csak jobbertekkent szerepelhet, mivel nem biztos, hogy a kulcs letezik.
  // Balertekkent ez esetben letrehozza az uj elemet.
  nevadat*& operator[](unsigned uiKulcs) ;
  // Egyenloseg operator (a felveteli ablakban ideiglenes valtozok ertekadasara)
  CNev& operator=(CNev& Jobboldal) ;

  // SQL fuggvenyek nem zarjak le a tranzakcio(kat)
  // (vo.: CNevFelv::OnOK ::OnCancel)
  // A Serialize fv. helyett betolt
  long SQL_NevBetolt() ;
  // A fv. megprobalja az adatbazisba beirni a megadott nevet,
  // ha sikerul, a kulcsot adja vissza
  long SQL_NevINSERT( CString& sUjNev, unsigned* puiKulcs) ;
  // A fv. torli a kulccsal megadott bejeggyzest
  // az adatbazisbol, ha sikerul a map-bol is
  long SQL_NevDELETE(unsigned uiKulcs) ;
  // Ld. mint fent, csak UPDATE-re (l. UK a nevekre!)
  long SQL_NevUPDATE(unsigned uiKulcs, CString& sUjNev) ;

// Implementation
public:
  virtual ~CNev() ;
  // Kiveve a felreertesek elkerulese vegett : valtoztatasok az adatbazisba
  // virtual void Serialize(CArchive& ar) ;

#ifdef _DEBUG
  virtual void AssertValid() const ;
  virtual void Dump(CDumpContext& dc) const ;
  // A fuggveny a parameterkent atadott file-ba dumpolja az osztalyt
  void CNevDump(CString sDumpFilenev) ;
#endif

} ;