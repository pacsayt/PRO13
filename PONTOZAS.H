// pontozas.h - A pontozasokat tartalmazo tomb

// Az osztaly pontozasi kulcsokhoz rendel hozza a helyezesekhez (melyek a
// vektor indexei is egyben -1) rendelt pontozasi vektort

// AZ ALAPTIP.H FILE-BAN EZ MAR SZEREPEL !

// Az SQL fuggvenyek nincsenek arra felkeszitve, hogy ha ez megvaltozik,
// a valtoztatasokat atvigyek az adatbazisra is !

#define MAX_ERTEKELT 10

struct pontadat
{
  pontadat()
  {
    memset( m_pPontSor, 0, MAX_ERTEKELT) ;
  } ;

  pontadat(const pontadat& PontAdat)
  {
    memcpy( m_pPontSor, &PontAdat.m_pPontSor, sizeof(unsigned char)*MAX_ERTEKELT) ;
  } ;

  pontadat( pontadat* pPontAdat)
  {
    memcpy( m_pPontSor, pPontAdat->m_pPontSor, sizeof(unsigned char)*MAX_ERTEKELT) ;
  } ;

  pontadat& operator=(pontadat& sMasolando)
  {
    if ( this != &sMasolando )
    {
      memcpy( m_pPontSor, sMasolando.m_pPontSor, sizeof(unsigned char)*MAX_ERTEKELT) ;
    }

    return *this ;
  } ;

  unsigned char& operator[]( unsigned char uc_poz)
  {
    return m_pPontSor[uc_poz] ;
  } ;

  unsigned char m_pPontSor[MAX_ERTEKELT] ;

} ;

// A leszarmaztatas CMapPtrToPtr-bol tortenik CMapWordToPtr helyett,
// mert az adatbazisbol kiolvasott MUMBER(10), unsigned nem fer bele
// a 16 bites WORD-be.
// A NUMBER(10)-et szelektalom unsigned-ba, ezt viszont a map-ben void*-be

class CPontozas : public CMapPtrToPtr
{
  DECLARE_DYNCREATE( CPontozas)
public:  
  CPontozas() ;

  CPontozas( int nBlokkMeret ) ;
  // A const kulcsszot kivettem az argumentumlistabol , mert kulonben :
  // 'GetNextAssoc' : cannot convert 'this' pointer from 'const class ::CNev __far *' to 'class ::CNev __far *const '

  CPontozas( CPontozas& cMasol) ;

// Attributes
public:

public:
// Operations
  inline BOOL Lookup(unsigned uiKulcs, pontadat*& pPontAdat)
  {
    return CMapPtrToPtr::Lookup((void*)uiKulcs, (void*&) pPontAdat);
  } ;
  inline void SetAt(unsigned uiKulcs, pontadat* pPontAdat)
  {
    CMapPtrToPtr::SetAt((void*)uiKulcs, (void*) pPontAdat);
  } ;
  inline void GetNextAssoc(POSITION& rKovPoz, unsigned& uiKulcs, pontadat*&  pPontAdat)
  {
    CMapPtrToPtr::GetNextAssoc(rKovPoz, (void*&)uiKulcs, (void*&) pPontAdat) ;
  } ;

  void RemoveAll() ;
  BOOL RemoveKey(unsigned uiKulcs) ;

  // Csak jobbertekkent szerepelhet, mivel nem biztos, hogy a kulcs letezik.
  // Balertekkent ez esetben letrehozza az uj elemet.
  pontadat*& operator[](unsigned uiKulcs) ;
  // Egyenloseg operator (a felveteli ablakban ideiglenes valtozok ertekadasara)
  CPontozas& operator=(CPontozas& Jobboldal) ;

  // SQL fuggvenyek nem zarjak le a tranzakcio(kat)
  // (vo.: CNevFelv::OnOK ::OnCancel)
  // A Serialize fv. helyett betolt
  long SQL_PontBetolt() ;
  // A fv. megprobalja az adatbazisba beirni a megadott nevet,
  // ha sikerul, a kulcsot adja vissza
  long SQL_PontINSERT( pontadat& sUjPontAdat, unsigned* puiKulcs) ;
  // A fv. torli a kulccsal megadott bejeggyzest az adatbazisbol
  long SQL_PontDELETE( unsigned uiKulcs) ;
  // Ld. mint fent, csak UPDATE-re (l. UK a nevekre!)
  long SQL_PontUPDATE( unsigned uiKulcs, pontadat& sUjPontAdat) ;

// Implementation
public:
  virtual ~CPontozas() ;
  // Kiveve a felreertesek elkerulese vegett : valtoztatasok az adatbazisba
  // virtual void Serialize(CArchive& ar) ;

/*
#ifdef _DEBUG
  virtual void AssertValid() const ;
  virtual void Dump(CDumpContext& dc) const ;
  // A fuggveny a parameterkent atadott file-ba dumpolja az osztalyt
  void CPontDump(CString sDumpFilenev) ;
#endif
*/
} ;