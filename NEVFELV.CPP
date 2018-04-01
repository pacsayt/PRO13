// nevfelv.cpp : implementation file
//
#include "resource.h"  // main symbols

#include <stdio.h>

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"
#include "cnev.h"

#include "ensqlcam.h"
#include "dbglobal.h"

#include "cvnev.h"
#include "pontozas.h"
#include "proba13.h"

#include "nevfelv.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNevFelv dialog

CNevFelv::CNevFelv( eNevtipus eFelvNevtip, CWnd* pParent /*=NULL*/)
	: CDialog(CNevFelv::IDD, pParent)
{
   //{{AFX_DATA_INIT(CNevFelv)
     // NOTE: the ClassWizard (azaz en) will add member initialization here
     m_eFelvNevtip = eFelvNevtip ;
     m_bModositva  = FALSE ;
     m_uiKivNevKulcsa = 0 ;
     m_pCNevAkt    = NULL ;
   //}}AFX_DATA_INIT
}

BOOL CNevFelv::OnInitDialog()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;

  POSITION poz ;
  unsigned uiKulcs ;
  nevadat* pNevAdat ;

  CString  sHibaSzov, sSzov ;

  char     pszHibaSzov[75] ;
  long     nRC ;
  
  CDialog::OnInitDialog() ;

  switch ( m_eFelvNevtip )
  {
    case eNemzNt :
      if ( m_sAktNev.LoadString( IDS_NEMZETSGNV_SZOV) == FALSE )
        m_sAktNev = "IDS_NEMZETSGNV_SZOV" ;

      m_pCNevAkt = &((CProba13App*)AfxGetApp())->m_cNemzetisegNev ;

      break ;
    case eCsapNt :
      if ( m_sAktNev.LoadString( IDS_CSAPAT_SZOV) == FALSE )
        m_sAktNev = "IDS_CSAPAT_SZOV" ;

      m_pCNevAkt = &((CProba13App*)AfxGetApp())->m_cCsapatNev ;

      break ;
    case eMotorNt:
      if ( m_sAktNev.LoadString( IDS_MOTORNV_SZOV) == FALSE )
        m_sAktNev = "IDS_MOTORNV_SZOV" ;

      m_pCNevAkt = &((CProba13App*)AfxGetApp())->m_cMotorNev ;

      break ;
    case eNagydijNt :
      if ( m_sAktNev.LoadString( IDS_NAGYDJNV_SZOV) == FALSE )
        m_sAktNev = "IDS_NAGYDJNV_SZOV" ;

      m_pCNevAkt = &((CProba13App*)AfxGetApp())->m_cNagydijNev ;

      break ;
    case ePalyaNt:
      if ( m_sAktNev.LoadString( IDS_PALYANEV_SZOV) == FALSE )
        m_sAktNev = "IDS_PALYANEV_SZOV" ;

      m_pCNevAkt = &((CProba13App*)AfxGetApp())->m_cPalyaNev ;

      break ;
    default :
      if ( m_sAktNev.LoadString( IDS_ISMLEN_NEVT_SZOV) == FALSE )
        m_sAktNev = "IDS_ISMLEN_NEVT_SZOV" ;

      MessageBox( m_sAktNev, NULL, MB_OK | MB_ICONSTOP ) ;

      CDialog::OnCancel() ;

      return FALSE ;
  }

  // A sSzov valtozo masodik felhasznalasa
  if ( sSzov.LoadString(IDS_NEVEK_FELV_SZOV)  == FALSE )
    sSzov = "IDS_NEVEK_FELV_SZOV" ;

  // A dialogus ablak eltero feliratainak beallitasa
  SetWindowText( m_sAktNev + sSzov) ;

  // A group szovegenek beallitasa
  GetDlgItem(IDC_NSZOV_G)->SetWindowText(m_sAktNev) ;

  /* Aktualis allapotot lassunk */
  m_pCNevAkt->RemoveAll() ;

  if ( (nRC = m_pCNevAkt->SQL_NevBetolt()) != 0 )
  {
    if ( nRC == -1 )
    {
      if ( sSzov.LoadString(IDS_MEMF_PRBL_SZOV)  == FALSE )
        sSzov = "IDS_MEMF_PRBL_SZOV" ;

      if ( MessageBox( sSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
        CDialog::OnCancel() ;// Ha nem sikerult memoriat foglalni
    }
    else // Ha nem memoriafoglalasi hiba, akkor SQL hiba
    {
      sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                  (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      if ( MessageBox( pszHibaSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
        CDialog::OnCancel() ;// Ha nem sikerult memoriat foglalni
    }
  }

  poz = m_pCNevAkt->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pCNevAkt->GetNextAssoc( poz, uiKulcs, pNevAdat ) ;
    pListaAbl->SetItemData(pListaAbl->AddString(pNevAdat->m_sNev), uiKulcs) ;
  }

  // A lista ablak elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pListaAbl->SetCurSel(0) ;

//-------------------------------------------------------------------------
//    m_pCNevAkt->CNevDump("c:\\forma1\\nevdump.txt") ;
//-------------------------------------------------------------------------

  return TRUE ;
}

BEGIN_MESSAGE_MAP(CNevFelv, CDialog)
	//{{AFX_MSG_MAP(CNevFelv)
	ON_BN_CLICKED(IDC_NFLV_DB, OnFelvesz)
	ON_BN_CLICKED(IDC_NTRL_B, OnTorol)
	ON_EN_CHANGE(IDC_NFLV_EDIT, OnChangeFelvNevEdit)
	ON_LBN_DBLCLK(IDC_NLIST, OnDblclkList)
	ON_BN_CLICKED(IDC_NATN_B, OnAtnevez)
	//}}AFX_MSG_MAP                                     
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNevFelv message handlers

void CNevFelv::OnFelvesz()
{
  int i ;

  CString  sBeoSzov, sSzov ;
  char     pszSzov[101] ;

  unsigned uiKulcs ;
  nevadat* pNevAdat = NULL ;

  CEdit*    pSzerkAbl = (CEdit*) GetDlgItem(IDC_NFLV_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;

  if ( pSzerkAbl == NULL || pListaAbl == NULL )
    return ;

  // Beolvassuk a szoveget az editablakbol
  pSzerkAbl->GetWindowText( sBeoSzov) ;

  UresLevagEV( sBeoSzov) ;

  // Ugyan az adatbazis nem enne meg, megis sebesseg megfontolasok miatt
  if ( sBeoSzov != "" )
  {
    if ( m_pCNevAkt->SQL_NevINSERT( sBeoSzov, &uiKulcs) == 0 )
    {
      if ( (pNevAdat = new nevadat( sBeoSzov)) == NULL )
      {
        // Ez mar tuzoltas, itt nem vizsgalodunk
        m_pCNevAkt->SQL_NevDELETE( uiKulcs) ;

        if ( sSzov.LoadString(IDS_MEMF_PRBL_SZOV) == FALSE )
          sSzov = "IDS_MEMF_PRBL_SZOV" ;

        if ( MessageBox( sSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
          return ;
      }

      m_pCNevAkt->SetAt( uiKulcs, pNevAdat) ;

      m_bModositva = TRUE ;

      i = pListaAbl->AddString(sBeoSzov) ;
      pListaAbl->SetItemData( i, uiKulcs) ;
      pListaAbl->SetCurSel(i) ;

      // Torli a szerkesztoablak tartalmat
      pSzerkAbl->SetWindowText("") ;
      pSzerkAbl->SetFocus() ;
    }
    else // Nem sikerult uj nevet az adatbazisba felvenni (szerepel mar stb.)
    {
      sprintf(pszSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                              (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      if ( MessageBox( pszSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
        return ;
    }
  }// A beolvasott szoveg uressseget ellenorzo feltetel vege  
}

void CNevFelv::OnTorol()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;
  CEdit*    pSzerkAbl = (CEdit*) GetDlgItem(IDC_NFLV_EDIT) ;

  CString  sKivNev, sSzov ;
  char     pszSzov[101] ;

  nevadat* pNevAdat = NULL ;
  unsigned uiKulcs ;

  int      listpoz ;

  // A kivalasztott elem lehozatala
  listpoz = pListaAbl->GetCurSel() ;

  if ( listpoz != LB_ERR )
  {
    pListaAbl->GetText(listpoz, sKivNev) ;
    uiKulcs = (unsigned) pListaAbl->GetItemData(listpoz) ;

    if ( uiKulcs != LB_ERR )
    {
      if ( m_pCNevAkt->Lookup( uiKulcs, pNevAdat) == TRUE )
      {
        // Megerosites
        if ( sSzov.LoadString(IDS_TRL_MEGER_S)  == FALSE )
          sSzov = "IDS_TRL_MEGER_S" ;

        if (IDYES==MessageBox(sSzov+sKivNev,m_sAktNev,MB_ICONQUESTION|MB_YESNO))
        {
          if ( m_pCNevAkt->SQL_NevDELETE( uiKulcs) == 0 )
          {
            // Az elem torlese a map-bol : 
            m_pCNevAkt->RemoveKey( uiKulcs) ;

            pListaAbl->DeleteString( listpoz) ;

            if ( listpoz > 0 )
            {
              pListaAbl->SetCurSel( listpoz-1) ;
            }
            else
            {
              pListaAbl->SetCurSel( listpoz) ;
            }

            m_bModositva = TRUE ;

            pSzerkAbl->SetFocus() ;
          }
          else
          {
            sprintf(pszSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                    (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

            if ( MessageBox( pszSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
              return ;
          }
        }
      }
    }
  }
}

void CNevFelv::OnOK()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;

  int nKivNev ;

  char pszHibaSzov[101] ;

  if ( m_bModositva == TRUE )
  {
    if ( SQL_COMMIT() != 0 )
    {
      sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                  (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      if ( MessageBox( pszHibaSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
        CDialog::OnCancel() ;
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

void CNevFelv::OnCancel()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;

  int nKivNev ;

  long    nRC ;
  char    pszHibaSzov[101] ;
  CString sAblCim, sSzov ;

  if ( m_bModositva == TRUE )
  {
    GetWindowText( sAblCim) ;

    if ( sSzov.LoadString( IDS_NEV_MEGV) == FALSE )
      sSzov = "IDS_NEV_MEGV" ;

    // Az aktualis nevlista megvaltozott. Kell-e menteni ?
    if ( IDYES == MessageBox( sSzov, sAblCim, MB_ICONQUESTION | MB_YESNO) )
    {
      // Az aktualis nevezesi lista mentese
      if ( SQL_COMMIT() != 0 )
      {
        sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                    (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

        MessageBox( pszHibaSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) ;
      }
    }
    else
    {
      if ( SQL_ROLLBACK() != 0 )
      {
        sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                    (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

        MessageBox( pszHibaSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) ;
      }

      m_pCNevAkt->RemoveAll() ;

      if ( (nRC = m_pCNevAkt->SQL_NevBetolt()) != 0 )
      {
        if ( nRC == -1 )
        {
          if ( sSzov.LoadString(IDS_MEMF_PRBL_SZOV)  == FALSE )
            sSzov = "IDS_MEMF_PRBL_SZOV" ;

          if ( MessageBox( sSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
            CDialog::OnCancel() ;
        }
        else // Ha nem memoriafoglalasi hiba, akkor SQL hiba
        {
          sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                      (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

          if ( MessageBox( pszHibaSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) == 0 )
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

void CNevFelv::OnChangeFelvNevEdit()
{
	CString sBeoSzov ;

	CEdit* pSzerkAbl = (CEdit*) GetDlgItem(IDC_NFLV_EDIT) ;
	CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;

	pSzerkAbl->GetWindowText(sBeoSzov) ;
	pListaAbl->SelectString(-1, sBeoSzov) ;
}

void CNevFelv::OnDblclkList()
{
  CString sBeoSzov ;

  CEdit* pSzerkAbl = (CEdit*) GetDlgItem(IDC_NFLV_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;

  // Beolvassuk a szoveget a listablakbol
  pListaAbl->GetText(pListaAbl->GetCurSel(), sBeoSzov) ;

  // Kiirjuk a szoveget az editablakba
  pSzerkAbl->SetWindowText(sBeoSzov) ;

  pSzerkAbl->SetFocus() ;
}

void CNevFelv::OnAtnevez()
{
  CEdit* pSzerkAbl = (CEdit*) GetDlgItem(IDC_NFLV_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_NLIST) ;

  unsigned uiModKulcs ;
  int      nListAblPoz ;
  CString  sRegiNev, sUjNev ;
  CString  sSzov ;
  char     pszSzov[101] ;
  nevadat* pNevAdat ;

  if ( (nListAblPoz = pListaAbl->GetCurSel()) != LB_ERR )
  {
    uiModKulcs  = (unsigned) pListaAbl->GetItemData(nListAblPoz) ;

    pSzerkAbl->GetWindowText(sUjNev) ;
   	
   	UresLevagEV(sUjNev) ;
    
    // Ugyan az adatbazis nem enne meg, megis sebesseg megfontolasok miatt
    if ( sUjNev != "" )
    {
      if ( m_pCNevAkt->Lookup( uiModKulcs, pNevAdat) == TRUE )
      {
        if ( sSzov.LoadString(IDS_ATNEV_KRD_S)  == FALSE )
          sSzov = "Valóban át akarja nevezni\n %s -t\n %s -re ?" ;

        sprintf(pszSzov,(const char*)sSzov,(const char*)pNevAdat->m_sNev,(const char*)sUjNev) ;

        if (IDYES==MessageBox(pszSzov , m_sAktNev ,  MB_ICONQUESTION | MB_YESNO ))
        {// Atnevezes
          if ( m_pCNevAkt->SQL_NevUPDATE( uiModKulcs, sUjNev) == 0 )
          {
            pNevAdat->m_sNev = sUjNev ;

            pListaAbl->DeleteString( nListAblPoz) ;

            nListAblPoz = pListaAbl->AddString( sUjNev) ;

            pListaAbl->SetItemData( nListAblPoz, uiModKulcs) ;
            pListaAbl->SetCurSel( nListAblPoz) ;

            pSzerkAbl->SetWindowText("") ;
            pSzerkAbl->SetFocus() ;

            m_bModositva = TRUE ;
          }
          else
          {
            sprintf(pszSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                    (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

            MessageBox( pszSzov, m_sAktNev, MB_OK | MB_ICONSTOP ) ;
          }

        }
      }
    }
  } // A listaablak szelekico OK
}

void CNevFelv::UresLevagEV(CString& sMegvagando)
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