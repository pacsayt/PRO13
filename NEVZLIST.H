// nevzlist.h : Az osztaly a versenyzo nevet, nemzetiseget, a csapat es a motor
//              nevet rendeli ossze a rajtszam(elsodleges kulcs) - versenyszam 
//              (masodlagos kulcs) fuggvenyeben
//
/////////////////////////////////////////////////////////////////////////////


const long VE_UJ_NVZL_SOR = 1 ;

// A keresesi maszkban a kozombos kulcs szerepet jatssza el.
// Mivel az extrem ertekek kozul a '0' biztos elofordul (1.), a masik vege a
// gyakorlatban valoszinuleg nem fog :
#define KOZOMBOS     65535
#define KOZOMBOS_RSZ   255

// Egy nevezesi sort tartalmazo osztaly
class CNvzSor : public CObject
{
public:
  // Default konstruktor
  CNvzSor() ;
  // 'CNvzSor' : multiple default constructors specified : unsigned uiElsoNagydIni,
  CNvzSor( unsigned uiRajtszamIni,
           unsigned uiVersIni,
           unsigned uiNemzIni,
           unsigned uiCsapIni,
           unsigned uiMotIni) ;

  // Nincs Serialize, eleg ez is
  DECLARE_DYNCREATE(CNvzSor)

public:
  // A nevezesi lista egy soraban levo nevek(elso verseny,versenyzo,nemzetiseg,
  // csapat,motor)-hez tartozo kulcsok (vo: C(V)Nev osztalyok)
//  unsigned uiElsoNagyd ;
  unsigned uiRajtszam  ;
  unsigned uiVersenyzo ;
  unsigned uiNemz   ;
  unsigned uiCsapat ;
  unsigned uiMotor  ;

public:
  // Egyenloseg operator - a default megoldas nem jo, mert kozombos
  // ertekek is lehetnek a maszkban
  BOOL operator==(CNvzSor& Jobboldal) ;
  // Ertekado operator (a felveteli ablakban ideiglenes valtozok ertekadasara)
  CNvzSor& operator=(CNvzSor& Jobboldal) ;

  void Trace() ;

public:
  ~CNvzSor() ;
} ;

class CNevezLista : public CMapPtrToPtr
{
public:

  CNevezLista() ;
  CNevezLista(unsigned uiInitSzezon) ;
  CNevezLista(int nBlokkMeret, unsigned uiInitSzezon) ;

  // A const kulcsszot kivettem az argumentumlistabol , mert kulonben :
  // 'GetNextAssoc' : cannot convert 'this' pointer from 'const class ::CNev __far *' to 'class ::CNev __far *const '
  CNevezLista( CNevezLista& cMasol) ;

  DECLARE_DYNCREATE(CNevezLista)

// Attributes
public:
  // A szezon eve, mely nevezesi listajat adatait az osztaly tartalmazza
  unsigned m_uiSzezon ;

  // Az elso nagydij nev kulcsa a szezonban, ezt irja be uj nevezesi lista sor
  // felvetelenel.
  unsigned m_uiElsoNagydij ;

protected:
  // Az adott futamon a rajtszam - nevezesi lista kulcs osszerendelest vegzi
  // L.: SQL_RszBolNvzlElk()
  CMapWordToPtr m_cRszElsNdMap ;

public:
// Muveletek
  // A lekerdezes dialogus ablakban a nvzlst meretere foglalok memoriat
  unsigned short Meret() ;

  void SetAt(unsigned uiKulcs, CNvzSor* pAdat)
  {
    CMapPtrToPtr::SetAt((void*)uiKulcs, (void*) pAdat) ;
  } ;

  inline BOOL Lookup(unsigned uiKulcs, CNvzSor*& pAdat)
  {
    return CMapPtrToPtr::Lookup((void*)uiKulcs, (void*&) pAdat) ;
  } ;
  
  inline void GetNextAssoc(POSITION& rKovPoz, unsigned& uiKulcs, CNvzSor*&  rpAdat)
  {
    CMapPtrToPtr::GetNextAssoc(rKovPoz, (void*&) uiKulcs, (void*&) rpAdat) ;
  } ;

  void RemoveAll() ;

  BOOL RemoveKey(unsigned uiKulcs) ;
  // Csak jobbertekkent szerepelhet, mivel nem biztos, hogy a kulcs letezik.
  // Balertekkent ez esetben letrehozza az uj elemet.
  CNvzSor*& operator[](unsigned uiKulcs) ;
  // Egyenloseg operator (a felveteli ablakban ideiglenes valtozok ertekadasara)
  CNevezLista& operator=(CNevezLista& Jobboldal) ;

//  BOOL RszNdBolNvzlstKlcs(unsigned uiRsz, unsigned uiNd, unsigned& uiNvzlstKulcs) ;
//  helyette : SQL_RszNdBolNvzl()

  // A m_uiSzezon tagvaltozot allitja
  long SQL_NvzlstBetolt( unsigned uiSzezon) ;
  
  // Ha csak CSAPAT_DISZKV | VERSENYZO_DISZKV bejegyzesek szerepelnek a rajtsorrend
  // tablaban torli azokat, es a nevezesi_lista-bal, azon sorokat, melyeket lehet
  // (nem szerepelnek mas szezonban
  long SQL_NvzlstTorol() ;
  
  // A fv. megprobalja az adatbazisba beirni a megadott nevezesi lista sort,
  // ha sikerul, a kulcsot adja vissza (az elso nagydijat veszi felvetelnel
  // a szezonbol - SQL_NvzlstBetolt)
  long SQL_NvzlstINSERT( CNvzSor* pNvzSor, unsigned* pKulcs) ;
  // Torli a CSAPAT_DISZKV|VERSENYZO_DISZKV bejegyzeseket a rajtsorrend tablabol
  // ha lehet a nevezesi listabol
  long SQL_NvzlstDELETE( unsigned uiKulcs) ;
  // Ha a modositando nevezesi lista sor csak az adott szezonban fordul elo,
  // UPDATE-eli, ha nem, uj lista sort vesz fel, a rajtsorrend tablaban uj
  // sort vesz fel a regi nevezesi lista kulcsuakkal parhuzamosan, ennek meg-
  // feleloen UPDATE-li a befuto es leggyorsabb_kor tablak nevezesi lista kulcsait,
  // majd torli a rajtsorrend tablabol.
  // Ha uj nevezesi lista sort vett fel, VE_UJ_NVZL_SOR a visszteresi ertek,
  // kulonben pKulcs nem valtozik
  long SQL_NvzlstUPDATE( CNvzSor* pNvzSor, unsigned* pKulcs) ;
  // A megadott nevezesi lista kulcsu bejegyzest megprobalja torolni a 
  // nevezesi_lista tablabol. A rajtsorrend tablaval nem foglalkozik.
  // Felhivasa : void CRsBefFelv::OnModosit()
  long SQL_NvzlKlcsDELETE( unsigned uiKulcs) ;
  // A fuggveny a megadott nagydijhoz meghatarozza melyik rajtszamhoz eppen
  // melyik nevezesi lista tartozik (CNvzSor::uiElsoNagyd )
  long SQL_RszBolNvzlElk(unsigned uiNagydij) ;

  long SQL_RszNdBolNvzl( unsigned uiRsz, unsigned uiNd, unsigned& uiNvzlstKulcs) ;

  // Megadja, hogy van-e az adott parameterekkel nevezesi lista sor,
  // ha nincs felveszi es az uj kulcsot adja vissza.
  // Ellentetben a SQL_NvzlstINSERT fv-nyel ez nem ir a rajtsorrend
  // tablaba, mert ugyis lesz bejegyzes, ha minden jol megy rs/bef felv eseten
  // 0 - mar volt, 1 - most vette fel, de a map-be es a rs-be be kell irni !
  long SQL_NvzlKulcsMeghat( unsigned& uiNvzlKulcs,
                            unsigned  uiNagydij  ,
                            CNvzSor*  pNvzSor ) ;

  virtual ~CNevezLista() ;

#ifdef _DEBUG
    virtual void AssertValid() const;
    virtual void Dump(CDumpContext& dc) const;
    // A fuggveny a parameterkent atadott file-ba irja az osztaly tartalmat
    // kulcsokkal ( a neveket nem helyettesiti be )
    void CNevezListaDump(CString sDumpFilenev) ;
#endif

} ;