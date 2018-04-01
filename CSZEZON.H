// cszezon.h
// A szezon adatait tartalmazo osztaly
//

// Ilyen lepesekben foglal helyet az m_pNagydijak szamara
#define ND_FOGL_BLK_MER 5

#define HIBA 65535 // LEHET, HOGY OSSZEAKADAS LESZ
// Ezzel a szammal inicializalom a kulcsot, ilyen biztos nincs,
// mert a SEQUENCE 1-tol indul
#define ERVTELEN_KULCS 0

struct SNagydij
{
  SNagydij():m_sDatum( 0, 0, 0)
  {
    m_uiNagyd = ERVTELEN_KULCS ;
    m_uiPalya = ERVTELEN_KULCS ;
  } ;

  // A nagydij nevehez, mint sztringhez tartozo kulcs (index)
  unsigned m_uiNagyd ;
  // A nagydijat megrendezo palya nevehez, mint sztringhez tartozo kulcs (index)
  unsigned m_uiPalya ;
  // A nagydij datuma
  SDatum m_sDatum ;
} ;

class CRajtsBef ;

class CSzezon : public CObject
{
  DECLARE_DYNCREATE( CSzezon)

public :
  CSzezon() ;

public :
  // A szezon eve, mely adatait az osztaly tartalmazza
  unsigned m_uiSzezon ;
  // A szezonban ervenyes pontozas
  unsigned m_uiPontozas ;
  // Az edzeselsosegert jaro pont
  unsigned char m_ucEdzElsPont ;
  // A futamgyozelemert jaro pont
  unsigned char m_ucLeggyKorPont ;
  // A legjobb ertekelt BEFUTOK szama (alapert 0: minden eredmeny beszamit)
  unsigned char m_ucElsLgjBesz ;

protected :
  // A nagydijakat (l. fent) tartalmazo dinamikus tomb (pointer)
  SNagydij** m_pNagydijak ;
  // SQL_RajtsBefBetolt utan nem blokk, hanem pont annyi van foglalva
  // amennyi kell, felvetel eseten az ujrafoglalas megy blokkosan
  unsigned char m_nBlokk ;

  // A nagydijak szama a szezonban
  unsigned char m_ucNagydSzam ;

public :
  // A szezonban levo nagydijak szamat adja meg
  inline unsigned char Meret() const
  {
    return m_ucNagydSzam ;
  } ;

  // Mivel m_pNagydijak protected lett :
  // Hibas index eseten mindegyik fv. 0-t ad vissza
  unsigned NagydKulcs( unsigned char ucIdx) ;
  unsigned PalyaKulcs( unsigned char ucIdx) ;
  // Hibas index eseten mindegyik fv. 0-t ad vissza
  unsigned short NagydEv( unsigned char ucIdx)    ;
  unsigned char  NagydHonap( unsigned char ucIdx) ;
  unsigned char  NagydNap( unsigned char ucIdx)   ;

  // Erre a fuggvenyre akkor van szukseg, amikor kiderul, hogy szezon/nagydij
  // INSERT/UPDATE nem sikerul, mert a szezon/egy nagydija torolve lett
  long SzezonFelvesz(unsigned char& ucFelvNd) ;

  // Az aktualis szezon torli az adatbazisbol (SQL_SzezTorol) es kiuriti az osztalyt
  // Kell ez a fv., mert mar meglevo szezont nem tudok ROLLBACK-kal torolni
  long SzezonTorol() ;

  // Uj NAGYDIJ felvetele, ha nem volt meg (a NAGYDIJ), a pUjNagyd->m_sDatum
  // alapjan szurja be a megfelelo helyre
  long UjFelv(SNagydij* pUjNagyd) ;

  // SQL fuggvenyek nem zarjak le a tranzakcio(kat)
  // A Serialize fv. helyett betolt : modositja az osztalyt
  long SQL_SzezonBetolt( unsigned uiSzezon) ;

  // A szezon tablaba irja be a szezon evet es a kulonbozo pontozasokat
  long SQL_SzezINSERT() ;

  // A szezon tablaba irja be a szezon evet es a kulonbozo pontozasokat
  long SQL_SzezUPDATE() ;

  // Torol az adatbazisban, de nem az osztalyban (szezon, nagydijak)
  // Mivel minden szezont egy tranzakcioban kezelek a CszezFelv ablak-
  // ban, a vegen COMMIT WORK-ot adhatok ki
  long SQL_SzezTorol() ;

  // Az INSERT DELETE UPDATE fv-ek nem modositjak az osztalyt

  // A fv. megprobalja az adatbazisba beirni a megadott nagydijat,
  // az osztaly evszamat veve alapul
  long SQL_NagydINSERT( SNagydij& sUjNagydij) ;
  // A fv. torli a nagydij kulccsal (es szezon evvel) megadott bejeggyzest
  long SQL_NagydDELETE( unsigned uiNdKulcs) ;
  // A fv. az adott nd kulcsu nagydijat magprobalja UPDATE-lni
  long SQL_NagydUPDATE( unsigned uiNdKulcs, SNagydij& sUjNagydij) ;

  // A fv. az adott nd kulcsu nagydijat magprobalja modositani
  // Mivel a regit modositja, nincs new delete, a bemenoadatnak
  // nem kell helyetfoglalni
  long Modosit(unsigned uiModNdKulcs, SNagydij& sUjNagydij) ;

  // A nagydij kulccsal megadott torli (0, ha minden ok)
  // Adatbazis + osztalytag
  long Torol(unsigned uiTorlendoNd) ;

  // A fuggveny az osztaly minden adattagjat torli (felszaabditja)
  // (Az adatbazist nem bantja)
  void Kiurit() ;

  // Az adott kulcsu nagydijhoz megadja a m_pNagydijak tombbeli
  // indexet. Ha nem talal ilyen nagydijat, m_ucNagydSzam jon vissza
  unsigned char NdBolIdx( unsigned uiNdKulcs ) ;
  // Operator, mely a megadott sorszamu futamra mutato pointert
  // adja vissza. Ha az index ervenytelen, NULL pointer jon vissza.
  SNagydij* operator[](unsigned char index) ;

  // Ertekado operator - kell, mert van dinamikus tag
  CSzezon& operator=(CSzezon& cJobboldal) ;

public :
  ~CSzezon() ;
#ifdef _DEBUG
  virtual void AssertValid() const ;
  virtual void Dump(CDumpContext& dc) const ;
  // Kiirja a CSzezon osztaly tartalmat kulcsokkal
  // ( a neveket nem helyettesiti be )
  void CSzezonDump(CString sDumpFilenev) ;
#endif

} ;