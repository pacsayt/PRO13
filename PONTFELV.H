// pontfelv.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPontFelv dialog

class CPontFelv : public CDialog
{
// Construction
public:
  CPontFelv(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
  //{{AFX_DATA(CPontFelv)
    enum { IDD = IDD_PONT_FELV };
    // NOTE: the ClassWizard will add data members here
  //}}AFX_DATA

  // Adattag, melyet a konstruktorban a CWinApp osztalybeli pontozastablaval
  // inicializalunk (a mutatot masolom bele, nincs helyfoglalas).
  CPontozas* m_pPontozas ; 

  // A (vegulis globalis^) pontozasi tomb mentettsegi allapota
  BOOL m_bPontozMentve ;

  int m_nTabstopok[9] ;       

// Implementation
protected:
  // A dialogus ablak kepernyore kerulese elotti inicializalas:
  // a lista ablak feltoltese
  virtual BOOL OnInitDialog() ;
  // A fuggveny a pontadat unsigned char(BYTE) tipusat
  // alakitja CString tipusuva, a szamok koze cElv kerul . 
  void PontbolStr(pontadat* pPontAdat, CString& sCelStr, char cElv) ;
  // A fuggveny a szerkeszto ablakbol beolvasott CString-et 
  // alakitja pontadat tipusuva.
  // A visszateresi ertek a sikeresen atalakitott pontok szama
  unsigned char StrbolPont(CString sSztring, pontadat* pPontAdat) ;
  // A fuggveny a kovetkezo ervenyessegi vizsgalatot vegzi el a pontozason
  // - monoton csokkeno
  // - az elso elem nem nulla
  BOOL PontozEll( pontadat* pPontAdat) ;
  // A fuggveny a megadott stringrol levalasztja a felvezeto es zaro ' '-ket
  // Atveve a CNevfelv osztalybol 
  void UresLevagEV(CString& sMegvagando) ;
	   	
  virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support

  // Generated message map functions
  //{{AFX_MSG(CPontFelv)
  virtual void OnCancel();
  virtual void OnOK();
  afx_msg void OnDblclkList();
  afx_msg void OnFelvesz();
  afx_msg void OnModosit();
  afx_msg void OnTorol();
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()
} ;
