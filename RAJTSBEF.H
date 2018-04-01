// rajtsbef.h

// define - ok a CRBBejegyzes::unsigned short m_nMegj szamara -> alaptip.h
// A CRajtsBef osztaly ilyen lepesekben foglelja a m_pPozRszTbl nevu segedtombot
#define BLOKK_MERET 30

// Az osztaly a rajtsorrend/befuto file-ok egy bejegyzeset definialja

class CRBBejegyzes : public CObject
{
public:
  DECLARE_DYNCREATE(CRBBejegyzes)

  CRBBejegyzes() ;

  CRBBejegyzes(unsigned iniRajtszam, const CIdoeredm& iniIdoeredm, unsigned short iniMegj) ;

  CRBBejegyzes(unsigned iniRajtszam, const CIdoeredm& iniIdoeredm, char pszIniMegj[17]) ;

  CRBBejegyzes(const CRBBejegyzes& iniCRBejegyzes) ;

// Adattagok
private:

  unsigned  m_uiRajtszam ;
  CIdoeredm m_cIdoeredm  ;
  unsigned short m_usMegj ;

// Kezelofuggvenyek, operatorok
public:
  //
  CRBBejegyzes& operator=(const CRBBejegyzes& cJobbRBBej) ;

  inline void RajtszBeall( const unsigned iniRajtsz)
  { m_uiRajtszam = iniRajtsz ; } ;

  inline void IdoerBeall(const CIdoeredm& iniIdoeredm)
  { m_cIdoeredm = iniIdoeredm ; } ;

  inline void MegjBeall(unsigned short iniMegj)
  { m_usMegj = iniMegj ; } ;

  void MegjBeall(char iniMegj[16] ) ;

  inline unsigned Rajtsz() const
  { return m_uiRajtszam ; } ;

  inline CIdoeredm IdoerC() const
  { return m_cIdoeredm ; } ;

  CString IdoerS() const ;

  inline unsigned short Megj() const
  { return m_usMegj ; } ;

  void MegjBeallS(char pszMegj[17]) const ;

  ~CRBBejegyzes() ;
} ;

// Az osztaly a rajtsorrend/befuto file-okban szereplo leggyorsabb kor
// tarolasahoz szukseges adatstrukturat definialja

class CLeggyKor : public CObject
{
public:
  DECLARE_DYNCREATE(CLeggyKor)

  CLeggyKor() ;
  CLeggyKor(unsigned iniNvzlKulcs, unsigned char iniKorben,
            CIdoeredm iniIdoeredm, float iniAtlSeb ) ;
  CLeggyKor(const CLeggyKor& iniCLeggyKor) ;

// Adattagok
protected:

  unsigned m_uiNvzlKulcs ;
  unsigned char m_nKorben ;
  CIdoeredm m_cIdoeredm   ;
  // A leggyorsabb korben elert atlagsebesseg
  float m_fAtlSeb ;

public:
  CLeggyKor& operator=(CLeggyKor& cJobboldal) ;
  
  inline void NvzlBeall(unsigned uiNvzl)
  { m_uiNvzlKulcs = uiNvzl ; } ;

  inline void KorbenBeall(unsigned char iniKorben)
  { m_nKorben = iniKorben ; } ;

  inline void IdoerBeall( CIdoeredm& iniIdoeredm)
  { m_cIdoeredm = iniIdoeredm ; } ;

  inline void AtlSebBeall(float iniAtlSeb)
  { m_fAtlSeb = iniAtlSeb ; } ;

  inline unsigned Nvzl() const
  { return m_uiNvzlKulcs ; } ;

  inline unsigned char Korben() const
  { return m_nKorben ; } ;

  inline CIdoeredm Idoer() const
  { return m_cIdoeredm ; } ;

  CString IdoerS() const ;

  inline float AtlSeb() const
  { return m_fAtlSeb ; } ;

  // TRUE, ha az osztaly a default konstruktorbeli ertekeket tartalmezza
  BOOL Ures() ;

  // A default konstruktorbeli ertekeket allitja be
  void Kiurit() ;

  // Legyenek ennek az osztalynak is SQL_-fvnyei, ha mar kulon ablakban
  // olvasom be az adatait...

  // A Serialize fv. helyett betolt
  long SQL_LeggyKorBetolt( unsigned nSzezonEve, unsigned nNagydijNev) ;
  // A fv. megprobalja az adatbazisba beirni az osztaly adattagjait
  // a megadott parameterekkel
  long SQL_LeggyKorINSERT( unsigned nSzezonEve, unsigned nNagydijNev) ;
  // A fv. torli az osztaly adattagjaival es megadott parameterekkel
  // rendelkezo bejegyzest az adatbazisbol
  long SQL_LeggyKorDELETE( unsigned nSzezonEve, unsigned nNagydijNev) ;
  // Ld. mint fent, csak UPDATE-re
  long SQL_LeggyKorUPDATE( unsigned nSzezonEve, unsigned nNagydijNev) ;

  ~CLeggyKor() ;
} ;

// Az osztaly a rajtsorrend/befuto file-ok adatstrukturajat definialja

class CRajtsBef : public CObject
{
public:
  DECLARE_DYNCREATE(CRajtsBef)

  CRajtsBef() ;
  CRajtsBef(CRajtsBef& iniCRajtsBef) ;

// Adattagok
public:

  unsigned m_uiSzezEve ;
  unsigned m_uiNagydKulcs ;

  CLeggyKor m_cCLeggyKor ;

protected:

  unsigned char m_nMeret ;
  unsigned* m_pPzNdKlcsTbl ;

  CMapPtrToPtr  m_cEredmTabla ;

  // SQL_RajtsBefBetolt utan nem blokk, hanem pont annyi van foglalva
  // amennyi kell, felvetel eseten az ujrafoglalas megy blokkosan
  unsigned char m_nBlokk ;

// Kezelofuggvenyek, operatorok
public:
  // Az egyes bejegyzesek POZICIOJA kifele EGY ALAPU, mig belul NULLA ALAPU !
  inline unsigned char Meret() { return m_nMeret ; } ;

  // A megadott pozicioju (1 alapu) nevezesi lista kulcsot adja meg.
  // Ha balertekkent nincs ilyen indexu elem, 0xffff jon vissza egy lokalis
  // valtozoba elszall a semmibe az ertekadas.
  // & kiveve, mert nem t. mikor van baloldalon, nem modositja a map-et !!!
  unsigned operator[](unsigned char ucPoz) ;

  // Felvetel csak a lista belsejebe ill. pont a vegere lehetseges:
  // lyukak nem lehetnek !
  long Felvesz(eAblTipRsBef enRsBef,
               unsigned char ucPoz, unsigned uiNvzlKulcs,
               CRBBejegyzes* pUjBejegyzes ) ;

  // Elteres a Felvesz-hez kepest, hogy nem beszur, hanem felulir
  long Modosit(eAblTipRsBef enRsBef,
               unsigned char ucRegiPoz,
               unsigned char ucPoz,     unsigned uiNvzlKulcs,
               CRBBejegyzes* pUjBejegyzes ) ;

  long Torol( eAblTipRsBef enRsBef, unsigned char ucPoz ) ;

  // Az eredmeny 0 alapu ! Ha nincs talalat, a parametereket nem valtoztatja
  // RszPoz() ???
  BOOL KerRszPoz(unsigned char ucKerRsz,
                 unsigned char& ucTalPoz, CRBBejegyzes& cTalRBBej ) ;

  // A bemenet 1 alapu !
  BOOL KerPozRsz(unsigned char ucKerPoz, unsigned char& ucTalRsz, CRBBejegyzes& cTalRBBej ) ;

  // A copy - konstruktorban kell a poz -> rsz tomb
//  unsigned char TmbIdxRsz(unsigned char ucKerPoz) ; ???

  // Szinten ... miutan a tomb a teljes nvzlst kulcsot tartalmazza NULLA ALAPU
  unsigned TmbIdxKlcs(unsigned char ucKerPoz) ;

  // A copy - konstruktorban kell
  inline POSITION GetStartPosition()
  { return m_cEredmTabla.GetStartPosition() ; } ;

  // A copy - konstruktorban kell ez is ...
  inline void GetNextAssoc(POSITION& rKovPoz, unsigned& rKulcs, CRBBejegyzes*& pRBBej)
  { m_cEredmTabla.GetNextAssoc( rKovPoz, (void*&)rKulcs, (void*&) pRBBej) ; } ;

  inline BOOL Lookup(unsigned uiKulcs, CRBBejegyzes*& pRBBej)
  { return m_cEredmTabla.Lookup((void*)uiKulcs, (void*&) pRBBej) ; } ;

  // Megadja az atadott rsz poziciojat (1 alapu) az R/B-ben
  // KerRszPoz() ???
  BOOL RszPoz( unsigned char ucKerRsz, unsigned char& ucTalPoz) ;

  // Pl. uj betoltese elott torolni kell a regit (Csak az osztalyt erinti)
  void Kiurit() ;

  long SQL_RajtsBefBetolt(eAblTipRsBef enRsBef) ;

  // Kerdes, kell-e egyutt a ucPoz-t es a uiNvzlKulcs-ot egyszerre megadni ?
  // Beillesztes a m_pPzNdKlcsTbl-tombbe jelenleg az ORDER BY sorrendje szerint
  long SQL_RajtsBefINSERT(eAblTipRsBef enRsBef,
                          unsigned char ucPoz , unsigned uiNvzlKulcs,
                          const CRBBejegyzes* pUjBejegyzes) ;

  long SQL_RajtsBefDELETE(eAblTipRsBef enRsBef,
                           unsigned uiNvzlKulcs,
                           unsigned char ucPoz) ;

  // Az m_uiSzezEve, m_uiNagydKulcs parameterekkel meghatarozott
  // rajtsorrendet vagy befutot torli
  long SQL_RajtsBefDELETE(eAblTipRsBef enRsBef) ;

  long SQL_RajtsBefUPDATE(eAblTipRsBef enRsBef,
                          unsigned char ucRegiPoz, unsigned uiRegiNvzlKulcs,
                          unsigned char ucPoz, unsigned uiNvzlKulcs,
                          const CRBBejegyzes* pUjBejegyzes) ;

  // Az m_uiSzezEve, m_uiNagydKulcs rs/bef UPDATE-eli uiNvzlKulcs sor megj.et uiMegjegyzes-re
  long SQL_MegjUPDATE( eAblTipRsBef enRsBef, unsigned uiNvzlKulcs, unsigned uiMegjegyzes) ;

  // A fv.-t a CLegyKorFlv ablak fele pont valtozasanak a BEFUTO
  // minden bejegyzesere valo kiterjesztesere hasznalom
  // A MAP-ben levo megjegyzessel UPDATE-eli az adatbazist
  long SQL_FelePontBeall(eAblTipRsBef enRsBef, BOOL bFelePont) ;

  // A fv. a tagvaltozoval ( m_uiSzezEve, m_uiNagydKulcs ) adaott RAJTSORRENDBOL
  // kikeresi a kulonbozo rajtszamokat a Leggyorsabb kort beolvaso ablak szamara)
  long SQL_RajtsElofRajtsz( unsigned*& pRajtszTomb) ;

  ~CRajtsBef() ;
} ;