// befvkieg.h :
// A rajtsorrend/befuto felvetelenel felmerulo kiegeszito adatok
// (leggyorsabb kor, beszamit-e az eredmeny a r/b-ba) megadasara szolgalo ablak
//

// erebeszf.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CEreBeszFlv dialog

class CEreBeszFlv : public CDialog
{
// Construction
public:
  CEreBeszFlv(CWnd* pParent = NULL) ;

// Dialog Data
  //{{AFX_DATA(CEreBeszFlv)
    enum { IDD = IDD_EREDM_BESZ };
		// NOTE: the ClassWizard will add data members here
    BOOL m_bCsapat ;
    BOOL m_bVersenyzo ;
  //}}AFX_DATA

// Implementation
protected:
  virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support

  // Generated message map functions
  //{{AFX_MSG(CEreBeszFlv)
    virtual void OnCancel() ;
    virtual void OnOK() ;
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()
} ;

/////////////////////////////////////////////////////////////////////////////
// CLegyKorFlv dialog

class CLegyKorFlv : public CDialog
{
// Construction
public:
  // CRajtsBef* pAktRsBef helyett unsigned char ucNdKod
  CLegyKorFlv(unsigned uiSzezonEve, unsigned uiNdKulcs, CNevezLista* pAktNvzLst,
              CWnd* pParent = NULL) ;

// Dialog Data
  //{{AFX_DATA(CLegyKorFlv)
    enum { IDD = IDD_EGYEBEK };
    // A fele pontszam, mely minden versenyzore azonos kell legyen,
    // mivel a befutora vonatkozik, megis minden egyes R/B bejegyzes tartalmazza
    // az erre vonatkozo informaciot: az elso helyezettbol allapitom meg
    BOOL  m_bFelePont;
  //}}AFX_DATA

  // A felvitt versenyzo neveket tartalmazo lista
  // Az aktualis versenyzo nevek kigyujtesehez kell
  // ndkod + rsz parositasok probalgatasanal
  CNevezLista*  m_pAktNvzLst ;
  CVNev*        m_pVersenyzo ;
//  CIdoeredm*    m_pIdoeredm  ;

  // A versenyzo Cb-bol kivalasztott elemhez tartozo nvzl kulcs
  unsigned m_uiSzezonEve ;
  unsigned m_uiNdKulcs   ;
//  unsigned m_uiNvzlKulcs ;

  CLeggyKor m_cLeggyKor ;

  // A valtoztatasokat majd csak az ablak elhagyasakor irja az adatbazisba
  BOOL m_bLeggyKorValt ;
  // Ez a valtozo akadalyozza meg, hogy az inicializalaskor felhivodo
  // On...Chg-fv.-ek miatt a 'magvaltozott' legyen az ablak statusza
  BOOL m_bAblInit      ;

// Implementation
public:
//    void VersHozzaad(const CString& cVnev, WORD wNvzlKulcs) ;

protected:
  // A versenyzo nevek Cb-ba irasa miatt kell
  virtual BOOL OnInitDialog() ;

  virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support

  // Generated message map functions
  //{{AFX_MSG(CLegyKorFlv)
    virtual void OnOK() ;
    virtual void OnCancel() ;
    afx_msg void OnSelChgVrs() ;
    afx_msg void OnChgIdoerE() ;
    afx_msg void OnChgKorbenE() ;
    afx_msg void OnChgAtlSebE() ;
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()

  // Az idoeredmeny ablakbol beolvassa az idoeredmenyt/korhatranyt
  // A sztring ie: szam nemszam szam, korhatrany : nemszam szam nemszam (itt nem megengedett !)
  BOOL IdoEredmBeo() ;

  BOOL KorbenBeo() ;

  BOOL AtlSebBeo() ;
  
  // A fuggveny a versenyzo neveket tartalmazo CB-t tolti fel a nevezesi lista
  // alapjan:
  // - minden nev egyszer szerepelhet
  // - az aktualis nevezesi lista sor kulcsaval (ItemData)
  void VnevFeltolt() ;

  void HibaKiir(long nRC) ;
} ;
