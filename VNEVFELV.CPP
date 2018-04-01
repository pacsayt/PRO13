// versnevf.cpp : implementation file
//

#include "resource.h"

#include "stdafx.h"

#include "alaptip.h"

#include "ensqlcam.h"
#include "dbglobal.h"

#include "cnev.h"
#include "cvnev.h"
#include "pontozas.h"
#include "proba13.h"

#include "vnevfelv.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVNevFelv dialog


CVNevFelv::CVNevFelv( CWnd* pParent /*=NULL*/)
	: CDialog(CVNevFelv::IDD, pParent)
{
  //{{AFX_DATA_INIT(CVNevFelv)
    // NOTE: the ClassWizard will add member initialization here
  //}}AFX_DATA_INIT
  m_bModositva  = FALSE ;
  m_uiKivNevKulcsa = 0 ;
 	m_pCVNevAkt = &((CProba13App*)AfxGetApp())->m_cVersenyzoNev ;
 	m_nTabstopok[0] = LOWORD(GetDialogBaseUnits())*6 ;
//-------------------------------------------------------------------------
//    m_pCVNevAkt->CVNevDump("c:\\forma1\\versenev.txt") ;
//-------------------------------------------------------------------------
}

BOOL CVNevFelv::OnInitDialog()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;
  CString   sSzov, sAblCim ;
  char      pszHibaSzov[75] ;
  long      nRC ;

  POSITION poz ;
  unsigned uiKulcs ;
  vnevadat* pVNevAdat ;

  CDialog::OnInitDialog() ;

  // A 4 ertek beallitasa :
  pListaAbl->SetTabStops( 1, m_nTabstopok) ;

  if ( sAblCim.LoadString(ID_FELVITEL_VERSENYZNV)  == FALSE )
    sAblCim = "Versenyzõ nevek felvitele." ;

  /* Aktualis allapotot lassunk */
  m_pCVNevAkt->RemoveAll() ;

  if ( (nRC = m_pCVNevAkt->SQL_VNevBetolt()) != 0 )
  {
    if ( nRC == -1 )
    {
      if ( sSzov.LoadString(IDS_MEMF_PRBL_SZOV)  == FALSE )
        sSzov = "IDS_MEMF_PRBL_SZOV" ;

      if ( MessageBox( sSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
        CDialog::OnCancel() ;// Ha nem sikerult memoriat foglalni
    }
    else // Ha nem memoriafoglalasi hiba, akkor SQL hiba
    {
      sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                  (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      if ( MessageBox( pszHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
        CDialog::OnCancel() ;// Ha nem sikerult memoriat foglalni
    }
  }

  poz = m_pCVNevAkt->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pCVNevAkt->GetNextAssoc( poz, uiKulcs, pVNevAdat) ;
    pListaAbl->SetItemData(pListaAbl->AddString(pVNevAdat->m_sVezNev+",\t"+pVNevAdat->m_sKerNev), uiKulcs) ;
  }

  // A lista ablak elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pListaAbl->SetCurSel(0) ;

	return TRUE ;
}

BEGIN_MESSAGE_MAP(CVNevFelv, CDialog)
	//{{AFX_MSG_MAP(CVNevFelv)
	ON_BN_CLICKED(IDC_VNFLV_DB, OnFelvesz)
	ON_BN_CLICKED(IDC_VNATN_B, OnAtnevez)
	ON_BN_CLICKED(IDC_VNTRL_B, OnTorol)
	ON_EN_CHANGE(IDC_VEZN_EDIT, OnVezNevValt)
	ON_LBN_DBLCLK(IDC_VN_LIST, OnDblclkList)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVNevFelv message handlers

void CVNevFelv::OnFelvesz()
{
	CString sBeoVezNev, sBeoKerNev ;
  CString sSzov, sAblCim ;
  char      pszSzov[101] ;

  unsigned  uiKulcs ;
//  POSITION  poz ;
  vnevadat* pVNevAdat = NULL ;

  int i ;

  CEdit* pVezNevAbl = (CEdit*) GetDlgItem(IDC_VEZN_EDIT) ;
  CEdit* pKerNevAbl = (CEdit*) GetDlgItem(IDC_KERN_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;

  // Beolvassuk a szoveget az editablakbol
  pVezNevAbl->GetWindowText(sBeoVezNev) ;
  pKerNevAbl->GetWindowText(sBeoKerNev) ;

  UresLevagEV( sBeoVezNev) ;
  UresLevagEV( sBeoKerNev) ;

  // Ugyan az adatbazis nem enne meg, megis sebesseg megfontolasok miatt
  if ( sBeoVezNev != "" && sBeoKerNev != "" )
  {
    GetWindowText( sAblCim) ;
    
    if ( m_pCVNevAkt->SQL_VNevINSERT( sBeoVezNev, sBeoKerNev, &uiKulcs) == 0 )
    {
      if ( (pVNevAdat = new vnevadat( sBeoVezNev, sBeoKerNev)) == NULL )
      {
        // Ez mar tuzoltas, itt nem vizsgalodunk
        m_pCVNevAkt->SQL_VNevDELETE( uiKulcs) ;

        if ( sSzov.LoadString(IDS_MEMF_PRBL_SZOV) == FALSE )
          sSzov = "IDS_MEMF_PRBL_SZOV" ;

        if ( MessageBox( sSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
          return ;// Ha nem sikerult memoriat foglalni
      }

      m_pCVNevAkt->SetAt( uiKulcs, pVNevAdat) ;

      m_bModositva = TRUE ;

      i = pListaAbl->AddString( sBeoVezNev+",\t"+sBeoKerNev) ;
      pListaAbl->SetItemData( i, uiKulcs) ;
      pListaAbl->SetCurSel(i) ;

      // Torli a szerkesztoablak tartalmat
      pVezNevAbl->SetWindowText("") ;
      pKerNevAbl->SetWindowText("") ;

      pVezNevAbl->SetFocus() ;
    }
    else // Nem sikerult uj nevet az adatbazisba felvenni (szerepel mar stb.)
    {
      sprintf(pszSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                              (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      if ( MessageBox( pszSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
        return ;// Ha nem sikerult memoriat foglalnia
    }
  }
}

void CVNevFelv::OnAtnevez()
{
  CEdit* pVezNevAbl = (CEdit*) GetDlgItem(IDC_VEZN_EDIT) ;
  CEdit* pKerNevAbl = (CEdit*) GetDlgItem(IDC_KERN_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;

  unsigned uiModKulcs ;

  int nListAblPoz ;

  CString sAblCim, sSzov ;
  CString sUjVezNev, sUjKerNev ;
  CString sRegiVezNev, sRegiKerNev ;
  char      pszSzov[101] ;
  vnevadat* pVNevAdat ;

  nListAblPoz = pListaAbl->GetCurSel() ;
  if ( (nListAblPoz = pListaAbl->GetCurSel()) != LB_ERR )
  {
    uiModKulcs  = (unsigned) pListaAbl->GetItemData(nListAblPoz) ;

    pVezNevAbl->GetWindowText(sUjVezNev) ;
    pKerNevAbl->GetWindowText(sUjKerNev) ;

    UresLevagEV(sUjVezNev) ;
    UresLevagEV(sUjKerNev) ;

    // Ugyan az adatbazis nem enne meg, megis sebesseg megfontolasok miatt
    if ( (sUjVezNev != "") && (sUjVezNev != "") )
    {
      if ( m_pCVNevAkt->Lookup( uiModKulcs, pVNevAdat) == TRUE )
      {
        GetWindowText( sAblCim) ;

        if ( sSzov.LoadString(IDS_ATVNEV_KRD_S)  == FALSE )
          sSzov = "Valóban át akarja nevezni\n %s %s -t\n %s %s -re ?" ;

        sprintf(pszSzov,
                (const char*)sSzov,
                (const char*)pVNevAdat->m_sVezNev,
                (const char*)pVNevAdat->m_sKerNev,
                (const char*)sUjVezNev,
                (const char*)sUjKerNev            ) ;

        if (IDYES==MessageBox( pszSzov, sAblCim,  MB_ICONQUESTION | MB_YESNO ))
        {// Atnevezes
          if ( m_pCVNevAkt->SQL_VNevUPDATE( uiModKulcs, sUjVezNev, sUjKerNev) == 0 )
          {
            pVNevAdat->m_sVezNev = sUjVezNev ;
            pVNevAdat->m_sKerNev = sUjKerNev ;

            sSzov = sUjVezNev + '\t' + sUjKerNev ;

            pListaAbl->DeleteString( nListAblPoz) ;

            nListAblPoz = pListaAbl->AddString( sSzov) ;

            pListaAbl->SetItemData( nListAblPoz, (DWORD)uiModKulcs) ;
            pListaAbl->SetCurSel( nListAblPoz) ;

            pVezNevAbl->SetWindowText("") ;
            pKerNevAbl->SetWindowText("") ;
            pVezNevAbl->SetFocus() ;

            m_bModositva = TRUE ;
          }
          else
          {
            sprintf(pszSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                    (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

            MessageBox( pszSzov, sAblCim, MB_OK | MB_ICONSTOP ) ;
          }
        }
      }
    }
  }
}

void CVNevFelv::OnTorol()
{
  CEdit* pVezNevAbl = (CEdit*) GetDlgItem(IDC_VEZN_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;

  CString   sSzov, sAblCim ;
  char      pszSzov[101] ;
  vnevadat* pVNevAdat = NULL ;
  unsigned  uiKulcs ;
  unsigned  nListAblPoz ;   	

  // A kivalasztott elem lehozatala
  nListAblPoz = pListaAbl->GetCurSel() ;

	if ( nListAblPoz != LB_ERR )
	{// Megprobaljuk megkeresni a kivalasztott elemet kulcs alapjan
    uiKulcs = (unsigned) pListaAbl->GetItemData(nListAblPoz) ;

    if ( m_pCVNevAkt->Lookup( uiKulcs, pVNevAdat) == TRUE )
    {
      // Megerosites
      GetWindowText( sAblCim) ;

      if ( sSzov.LoadString(IDS_TRL_MEGER_S)  == FALSE )
        sSzov = "IDS_TRL_MEGER_S" ;

      sSzov = sSzov + pVNevAdat->m_sVezNev + pVNevAdat->m_sKerNev ;

      if ( IDYES == MessageBox( sSzov, sAblCim, MB_ICONQUESTION | MB_YESNO ) )
      {
        if ( m_pCVNevAkt->SQL_VNevDELETE( uiKulcs) == 0 )
        {
          // Az elem torlese a map-bol :
          m_pCVNevAkt->RemoveKey( uiKulcs) ;

          pListaAbl->DeleteString( nListAblPoz) ;

          if ( nListAblPoz > 0 )
          {
            pListaAbl->SetCurSel( nListAblPoz-1) ;
          }
          else
          {
            pListaAbl->SetCurSel( nListAblPoz) ;
          }

          pVezNevAbl->SetFocus() ;
          m_bModositva = TRUE ;
        }
        else
        {
          sprintf( pszSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                   (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

          if ( MessageBox( pszSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
            return ;// Ha nem sikerult memoriat foglalnia
        }
      }
    }
  }
}

void CVNevFelv::OnVezNevValt()
{
  CEdit* pVezNevAbl = (CEdit*) GetDlgItem(IDC_VEZN_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;
	
  CString sBeoVezNev, sBeoKerNev ;

  pVezNevAbl->GetWindowText(sBeoVezNev) ;
	
  pListaAbl->SelectString(-1, sBeoVezNev) ;
}

void CVNevFelv::OnDblclkList()
{
  CEdit* pVezNevAbl = (CEdit*) GetDlgItem(IDC_VEZN_EDIT) ;
  CEdit* pKerNevAbl = (CEdit*) GetDlgItem(IDC_KERN_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;

  CString sListSzov ;
  CString sBeoVezNev, sBeoKerNev ;
  CString sAblCim ;

  vnevadat* pVNevAdat ;
  unsigned  uiKulcs   ;

  // Megkeressuk a kijelolt sorhoz tartozo kulcsot listablakbol
  uiKulcs = (unsigned) pListaAbl->GetItemData(pListaAbl->GetCurSel()) ;

  // A kulcs megkeresese a map-ben
  if ( m_pCVNevAkt->Lookup( uiKulcs, pVNevAdat) )
  {// Ha talalat volt
    // Kiirjuk a szoveget az editablakba
    pVezNevAbl->SetWindowText( pVNevAdat->m_sVezNev) ;
    pKerNevAbl->SetWindowText( pVNevAdat->m_sKerNev) ;
  }
  else
  {// A lista ablakbol kiolvasott kulcs nem szerepel a mapben : HIBA
    GetWindowText( sAblCim) ; 

    MessageBox("A lista ablakbol kiolvasott kulcs\nnem szerepel a mapben", sAblCim, MB_OK | MB_ICONSTOP ) ;
  }
}

void CVNevFelv::OnOK()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;

  int nKivNev ;
  
  char    pszHibaSzov[101] ;
  CString sAblCim ;

  if ( m_bModositva == TRUE )
  {
    if ( SQL_COMMIT() != 0 )
    {
      sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                  (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      GetWindowText( sAblCim) ;

      if ( MessageBox( pszHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
        CDialog::OnCancel() ;// Ha nem sikerult memoriat foglalni
    }
  }

  // A kijelolt (~ve maradt) listaablak bejegyzest beirni
  // A CRsBefFelv::OnImportal() fv. szamara
  if ( pListaAbl != NULL )
  {
    nKivNev = pListaAbl->GetCurSel() ;

    if ( nKivNev != LB_ERR )
    {
      m_uiKivNevKulcsa = (unsigned) pListaAbl->GetItemData( nKivNev) ;
    }
    else
    {
      // Ilyen ugysincs...
      m_uiKivNevKulcsa = 0 ;
    }
  }

  CDialog::OnOK() ;
}

void CVNevFelv::OnCancel()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_VN_LIST) ;

  int nKivNev ;

  long    nRC ;
  char    pszHibaSzov[101] ;

  CString sAblCim, sSzov ;

  if ( m_bModositva == TRUE )
  {
    GetWindowText( sAblCim) ;

    // Az aktualis nevlista megvaltozott. Kell-e menteni ?
    if ( sSzov.LoadString(IDS_VNEV_MEGV_S)  == FALSE )
      sSzov = "IDS_VNEV_MEGV_S" ;

    if ( IDYES == MessageBox( sSzov, sAblCim, MB_ICONQUESTION | MB_YESNO) )
    {
      if ( SQL_COMMIT() != 0 )
      {
        sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                    (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

        if ( MessageBox( pszHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
          CDialog::OnCancel() ;
       }
    }
    else
    {
      SQL_ROLLBACK() ;

      m_pCVNevAkt->RemoveAll() ;

      if ( (nRC = m_pCVNevAkt->SQL_VNevBetolt()) != 0 )
      {
        if ( nRC == -1 )
        {
          if ( sSzov.LoadString(IDS_MEMF_PRBL_SZOV)  == FALSE )
            sSzov = "IDS_MEMF_PRBL_SZOV" ;

          if ( MessageBox( sSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
            CDialog::OnCancel() ;
        }
        else // Ha nem memoriafoglalasi hiba, akkor SQL hiba
        {
          sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                      (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

          if ( MessageBox( pszHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
            CDialog::OnCancel() ;
        }
      }
    }
  }

  // A kijelolt (~ve maradt) listaablak bejegyzest beirni
  // A CRsBefFelv::OnImportal() fv. szamara
  if ( pListaAbl != NULL )
  {
    nKivNev = pListaAbl->GetCurSel() ;

    if ( nKivNev != LB_ERR )
    {
      m_uiKivNevKulcsa = (unsigned) pListaAbl->GetItemData( nKivNev) ;
    }
    else
    {
      // Ilyen ugysincs...
      m_uiKivNevKulcsa = 0 ;
    }
  }

  CDialog::OnCancel() ;
}
 
void CVNevFelv::UresLevagEV(CString& sMegvagando)
{
    int poz = 0 ;

    if ( sMegvagando.GetLength() != 0 )
    {// Ha a sztring nem ures csak akkor ...
      
      for (poz=0 ; ((poz+1) <= sMegvagando.GetLength()) && (sMegvagando[poz] == ' ') ; poz++ ) ;
      
      sMegvagando = sMegvagando.Right(sMegvagando.GetLength()-poz) ;    
      
      for (poz=sMegvagando.GetLength()-1 ; (poz >= 0) && (sMegvagando[poz] == ' ') ; poz--) ;
      
      sMegvagando = sMegvagando.Left(poz+1) ;
    }
}