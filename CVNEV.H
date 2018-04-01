// cvnev.h : Az osztaly versenyzo neveket es konstansokat rendel ossze, ezen  
//           felul megadja a nev adatbazisbeli elso es utolso elofordulasat. 
//
/////////////////////////////////////////////////////////////////////////////

// CString   sVezNev    - a tarolt vezeteknev
// CString   sKerNev    - a tarolt keresztnev

struct vnevadat
{
  vnevadat(const CString& sIniVezNev="", const CString& sIniKerNev="")
  {
    m_sVezNev = sIniVezNev ;
    m_sKerNev = sIniKerNev ;
  } ;

  CString m_sVezNev ;
  CString m_sKerNev ;
} ;

class CVNev : public CMapPtrToPtr
{
public:
  DECLARE_DYNCREATE(CVNev)

  CVNev() ;
  CVNev( int nBlokkMeret ) ;
  // A const kulcsszot kivettem az argumentumlistabol , mert kulonben :
  // 'GetNextAssoc' : cannot convert 'this' pointer from 'const class ::CVNev __far *' to 'class ::CVNev __far *const '
  CVNev( CVNev& cMasol) ;

// Attributes
public:

public:
// Operations
  inline BOOL Lookup(unsigned uiKulcs, vnevadat*& pVNevAdat)
  {
    return CMapPtrToPtr::Lookup((void*)uiKulcs, (void*&) pVNevAdat) ;
  } ;
  inline void SetAt(unsigned uiKulcs, vnevadat* pVNevAdat)
  {
    CMapPtrToPtr::SetAt((void*)uiKulcs, (void*) pVNevAdat) ;
  } ;
  inline void GetNextAssoc(POSITION& rKovPoz, unsigned& uiKulcs, vnevadat*&  rpVNevAdat)
  {
    CMapPtrToPtr::GetNextAssoc(rKovPoz, (void*&)uiKulcs, (void*&) rpVNevAdat) ;
  } ;

  // Az importalas soran ennek a fv.-nek a feladata a beolvasott sztring
  // beazonositasa
  BOOL NevbolKulcs(const CString & sKeresett, unsigned& uiTalKulcs) ;

  void RemoveAll() ;
  BOOL RemoveKey(unsigned uiKulcs) ;
  // Csak jobbertekkent szerepelhet, mivel nem biztos, hogy a kulcs letezik.
  // Balertekkent ez esetben letrehozza az uj elemet.
  vnevadat*& operator[](unsigned uiKulcs) ;
  // Egyenloseg operator (a felveteli ablakban ideiglenes valtozok ertekadasara)
  CVNev& operator=(CVNev& cJobboldal) ;
  // SQL fuggvenyek
  // A Serialize fv. helyett betolt
  long SQL_VNevBetolt() ;
  // A fv. megprobalja az adatbazisba beirni a megadott nevet,
  // ha sikerul, a kulcsot adja vissza
  long SQL_VNevINSERT( CString& sUjVezNev, CString& sUjKerNev, unsigned* puiKulcs) ;
  // A fv. torli a kulccsal megadott bejeggyzest
  // az adatbazisbol, ha sikerul a map-bol is
  long SQL_VNevDELETE(unsigned uiKulcs) ;
  // Ld. mint fent, csak UPDATE-re (l. UK a nevekre!)
  long SQL_VNevUPDATE(unsigned uiKulcs, CString& sUjVezNev, CString& sUjKerNev) ;

// Implementation
public:
  virtual ~CVNev() ;
  // Kiveve a felreertesek elkerulese vegett : valtoztatasok az adatbazisba
  // virtual void Serialize(CArchive& ar);

#ifdef _DEBUG
  virtual void AssertValid() const ;
  virtual void Dump(CDumpContext& dc) const ;
  // A fuggveny a parameterkent atadott file-ba dumpolja az osztalyt
  void CVNevDump(CString sDumpFilenev) ;
#endif
} ;