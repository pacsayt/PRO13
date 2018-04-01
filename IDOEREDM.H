// idoeredm.h

// Az osztaly a rajtsorrend/befuto file-okban elofordulo bejegyzesek
// idoeredmenyeit tarolja, ill. segedoperatorokat definial
//

class CIdoeredm : public CObject
{
public:
  DECLARE_DYNCREATE(CIdoeredm)

  // Default konstruktor a serialize kepesseghez
  CIdoeredm() ;
  // Ha a megadott kezdoertekek kivul esnek a termeszetes hatarokon,
  // az ertekadas 0-val hajtodik vegre
  CIdoeredm(char iniOra, char iniPerc, char iniMasodperc, short iniEzrdMsdp) ;
  CIdoeredm(const CIdoeredm& cJobbIdo) ;

// Adattagok
private:

  char m_cOra  ;
  char m_cPerc ;
  char m_cMasodperc ;
  short m_shEzrdMsdp ;

// Kezelofuggvenyek, operatorok
public:
  //  adattagok beallitasa lekerdezese ???
  BOOL IdoBeall(char iniOra, char iniPerc, char iniMasodperc, short iniEzrdMsdp) ;
  BOOL OraBeall(char iniOra) ;
  BOOL PercBeall(char iniPerc) ;
  BOOL MasodpercBeall(char iniMasodperc) ;
  BOOL EzrdMsdpBeall(short iniEzrdMsdp) ;  

  inline char  Ora() const { return m_cOra ; } ;
  inline char  Perc() const { return m_cPerc ; } ;
  inline char  Masodperc() const { return m_cMasodperc ; } ;
  inline short EzrdMsdp() const { return m_shEzrdMsdp ; } ;
   
  CString IdoerS() const ;

  // Operatorok
  BOOL operator<(const CIdoeredm& cJobbIdo) ;
  //
  BOOL operator>(const CIdoeredm& cJobbIdo) ;
  //
  BOOL operator==(const CIdoeredm& cJobbIdo) ;
  //
  BOOL operator<=(const CIdoeredm& cJobbIdo) ;
  //
  BOOL operator>=(const CIdoeredm& cJobbIdo) ;

  //
  CIdoeredm& operator=(const CIdoeredm& cJobbIdo) ;
  //
  CIdoeredm& operator+(const CIdoeredm& cJobbIdo) ;
  //
  CIdoeredm& operator-(const CIdoeredm& cJobbIdo) ;

public:
  ~CIdoeredm() ;

/*
#ifdef _DEBUG
    virtual void AssertValid() const;
    virtual void Dump(CDumpContext& dc) const;
#endif
*/
} ;