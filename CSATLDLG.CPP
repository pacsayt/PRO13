// CsatlDlg.cpp : implementation file
//

#include "stdafx.h"
#include "resource.h"

#include "alaptip.h"
#include "ensqlcam.h"
#include "csatlako.h"

#include "csatldlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCsatlDlg dialog


CCsatlDlg::CCsatlDlg(CWnd* pParent /*=NULL*/) : CDialog(CCsatlDlg::IDD, pParent)
{
  //{{AFX_DATA_INIT(CCsatlDlg)
    // NOTE: the ClassWizard will add member initialization here
  //}}AFX_DATA_INIT
}

BOOL CCsatlDlg::OnInitDialog()
{
  CEdit *pNevEd    = (CEdit*) GetDlgItem(IDC_NEV_E)    ;
  CEdit *pJelszoEd = (CEdit*) GetDlgItem(IDC_JELSZO_E) ;

  CDialog::OnInitDialog() ;

  pNevEd->SetWindowText( "SCOTT") ;
  pJelszoEd->SetWindowText( "TIGER") ;

  return TRUE;  // return TRUE  unless you set the focus to a control
}

void CCsatlDlg::DoDataExchange(CDataExchange* pDX)
{
  CDialog::DoDataExchange(pDX);
  //{{AFX_DATA_MAP(CCsatlDlg)
    // NOTE: the ClassWizard will add DDX and DDV calls here
  //}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CCsatlDlg, CDialog)
  //{{AFX_MSG_MAP(CCsatlDlg)
    ON_BN_CLICKED(IDC_CSATL_DB, OnCsatlDb)
  //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCsatlDlg message handlers

void CCsatlDlg::OnCsatlDb() 
{
  CEdit *pNevEd    = (CEdit*) GetDlgItem(IDC_NEV_E)    ;
  CEdit *pJelszoEd = (CEdit*) GetDlgItem(IDC_JELSZO_E) ;

  long lSQLCODE = 0 ;
  char pszHibaSzov[75] ;
  CString cNev, cJelszo, sAblCim ;

  pNevEd->GetWindowText( cNev) ;
  pJelszoEd->GetWindowText( cJelszo) ;

  lSQLCODE = SQL_Csatlakozas( cNev, cJelszo) ;

  // A peldaprogramok a < 0 esettel foglalkoztak csak
  if ( lSQLCODE >= 0 )
  {
    gl_bCsatlakozva = TRUE ;

    CDialog::OnOK() ;
  }
  else
  {
    gl_bCsatlakozva = FALSE ;

    sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

    GetWindowText( sAblCim) ;

    MessageBox( pszHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) ;

    // Hogy le lehessen kerdezni a csatlakozas eredmenyet
    // a DoModal-nal
    CDialog::OnCancel() ;
  }
}

void CCsatlDlg::OnCancel() 
{
  gl_bCsatlakozva = FALSE ;
  
  CDialog::OnCancel();
}
