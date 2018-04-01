// vnevfelv.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CVNevFelv dialog

class CVNevFelv : public CDialog
{
// Construction
public:
  // standard constructor
  CVNevFelv( CWnd* pParent = NULL) ;

// Dialog Data
  //{{AFX_DATA(CVNevFelv)
    enum { IDD = IDD_VERSNEV_FELV } ;  //IDD_FELVITEL_VNEVEK
        // NOTE: the ClassWizard will add data members here
  //}}AFX_DATA

  // A Lb-ban kiszelektalt nevhez tartozo kulcsot tartalmazza
  unsigned m_uiKivNevKulcsa ;

  BOOL  m_bModositva ;

private:
// Belso adattagok
   // A konstruktorban adom at az aktualis CVNev osztalyt,
   // melyet le kell tarolni :
   CVNev* m_pCVNevAkt   ;

   // A tabstopok meretet megado 'tomb'
   int m_nTabstopok[1] ;

// Implementation
protected:
	// virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
    // A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
    // DoDataExchange fuggvenybol
    virtual BOOL OnInitDialog() ;

  // Generated message map functions
  //{{AFX_MSG(CVNevFelv)
    // A szerkeszto ablak tartalmat veszi fel a lista ablakba, ha meg nem szerepel
    afx_msg void OnFelvesz();
    // A lista ablakban szelektalt nevet irja at a szerkesztoablakba irt szoveggel
    afx_msg void OnAtnevez();
    // A lista ablakban szelektalt elemet torli
    afx_msg void OnTorol();
    // A vezeteknev szerkeszto ablak tartalma alapjan valtoztatja az aktualis kijelolest
    afx_msg void OnVezNevValt();
    // A lista ablak tartalmat hozza le a szerkesztoablakba
    afx_msg void OnDblclkList();

    // A kijelolt sor ItemData-janak visszairasa m_wKivNevKulcsa-ba :
    virtual void OnOK() ;
    virtual void OnCancel() ;
  //}}AFX_MSG
	DECLARE_MESSAGE_MAP()

  void UresLevagEV(CString& sMegvagando) ;
} ;