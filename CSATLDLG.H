// CsatlDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CCsatlDlg dialog

class CCsatlDlg : public CDialog
{
// Construction
public:
  CCsatlDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
  //{{AFX_DATA(CCsatlDlg)
    enum { IDD = IDD_CSATL_DLG };
    // NOTE: the ClassWizard will add data members here
  //}}AFX_DATA

  BOOL OnInitDialog() ;

// Overrides
  // ClassWizard generated virtual function overrides
  //{{AFX_VIRTUAL(CCsatlDlg)
protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
  //}}AFX_VIRTUAL

// Implementation
protected:

  // Generated message map functions
  // Hogy a DoModal fv. visszateresi ertekebol el lehessen donteni a csatlakozas
  // sikeresseget az OnCsatlDb az OnOK, OnCancel fveket hivja fel sqlcode fvben.
  //{{AFX_MSG(CCsatlDlg)
  afx_msg void OnCsatlDb() ;
  virtual void OnCancel()  ;
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()
} ;