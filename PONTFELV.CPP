// pontfelv.cpp : implementation file
//

#include "stdafx.h"

#include "resource.h"

#include "ensqlcam.h"

#include "alaptip.h"
#include "dbglobal.h"
#include "pontozas.h"
#include "pontfelv.h"
#include "cnev.h"
#include "cvnev.h"
#include "nevfelv.h"
#include "vnevfelv.h"

#include "proba13.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPontFelv dialog

CPontFelv::CPontFelv(CWnd* pParent /*=NULL*/)
  : CDialog(CPontFelv::IDD, pParent)
{
  //{{AFX_DATA_INIT(CPontFelv)
    // NOTE: the ClassWizard will add member initialization here
  //}}AFX_DATA_INIT

  m_bPontozMentve = TRUE ;
  m_pPontozas = &((CProba13App*)AfxGetApp())->m_cPontozas ;

  for (int i=1 ; i<MAX_ERTEKELT ; i++)
    m_nTabstopok[i-1] = (int) (i*LOWORD(GetDialogBaseUnits())*1.6) ;	
}

void CPontFelv::DoDataExchange(CDataExchange* pDX)
{
  CDialog::DoDataExchange(pDX) ;
  //{{AFX_DATA_MAP(CPontFelv)
    // NOTE: the ClassWizard will add DDX and DDV calls here
  //}}AFX_DATA_MAP
}

BOOL CPontFelv:: OnInitDialog()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_P_LIST) ;

  POSITION  poz ;
  unsigned  uiKulcs ;
  pontadat* pPontAdat ;

  CString  sHibaSzov, sSzov ;

  char     pszHibaSzov[75] ;
  long     nRC ;

  CString sEgySor ;

  CDialog::OnInitDialog() ;

  // A group szovegenek beallitasa
  if ( sSzov.LoadString(IDS_PONTOZHF_S)  == FALSE )
    sSzov = "IDS_PONTOZHF_S" ;

  GetDlgItem(IDC_PONTOZHF_S)->SetWindowText( sSzov) ;

  // A dialogus ablak feliratainak beallitasa
  if ( sSzov.LoadString(IDS_PONT_FLV_S)  == FALSE )
    sSzov = "IDS_PONT_FLV_S" ;

  SetWindowText( sSzov) ;

  // A tabstopok beallitasa :                
  pListaAbl->SetTabStops( 9, m_nTabstopok) ;

  /* Aktualis allapotot lassunk */
  m_pPontozas->RemoveAll() ;

  if ( (nRC = m_pPontozas->SQL_PontBetolt()) != 0 )
  {
    if ( nRC == -1 )
    {
      if ( sHibaSzov.LoadString(IDS_MEMF_PRBL_SZOV)  == FALSE )
        sHibaSzov = "IDS_MEMF_PRBL_SZOV" ;

      if ( MessageBox( sHibaSzov, sSzov, MB_OK | MB_ICONSTOP ) == 0 ) ;

      CDialog::OnCancel() ;
    }
    else // Ha nem memoriafoglalasi hiba, akkor SQL hiba
    {
      sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                  (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      if ( MessageBox( pszHibaSzov, sSzov, MB_OK | MB_ICONSTOP ) == 0 ) ;

      CDialog::OnCancel() ;
    }
  }

  poz = m_pPontozas->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pPontozas->GetNextAssoc( poz, uiKulcs, pPontAdat) ;

    sEgySor = "" ;
    PontbolStr( pPontAdat, sEgySor, '\t') ;

    pListaAbl->SetItemData(pListaAbl->AddString( sEgySor), uiKulcs) ;
  }

  // A lista ablak elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pListaAbl->SetCurSel(0) ;

  return TRUE ;
}

BEGIN_MESSAGE_MAP(CPontFelv, CDialog)
	//{{AFX_MSG_MAP(CPontFelv)
	ON_LBN_DBLCLK(IDC_P_LIST, OnDblclkList)
	ON_BN_CLICKED(IDC_PFLV_DB, OnFelvesz)
	ON_BN_CLICKED(IDC_PMOD_B, OnModosit)
	ON_BN_CLICKED(IDC_PTRL_B, OnTorol)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPontFelv message handlers
void CPontFelv::OnCancel()
{
  long    nRC ;
  char    pszHibaSzov[101] ;
  CString sAblCim, sSzov ;

  if ( m_bPontozMentve == FALSE )
  {
    GetWindowText( sAblCim) ;

    if ( sSzov.LoadString( IDS_PNT_MGV_S) == FALSE )
      sSzov = "IDS_PNT_MGV_S" ;

    // Az aktualis pontozas megvaltozott. Kell-e menteni ?
    if ( IDYES == MessageBox( sSzov, sAblCim, MB_ICONQUESTION | MB_YESNO) )
    {
      SQL_COMMIT() ;
    }
    else
    {
      SQL_ROLLBACK() ;

      m_pPontozas->RemoveAll() ;

      if ( (nRC = m_pPontozas->SQL_PontBetolt()) != 0 )
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

  CDialog::OnCancel() ;
}

void CPontFelv::OnOK()
{
  CString sAblCim ;
  char    pszHibaSzov[101] ;

  if ( m_bPontozMentve == FALSE )
  {
    if ( SQL_COMMIT() != 0 )
    {
      sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                  (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

      if ( MessageBox( pszHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) == 0 )
        CDialog::OnCancel() ;
    }
  }

  CDialog::OnOK() ;
}

void CPontFelv::OnDblclkList()
{
  CEdit*    pSzerkAbl = (CEdit*) GetDlgItem(IDC_PFLV_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_P_LIST) ;

  CString sKivSor ;
  int     nKivSor ;

  unsigned  uiKulcs ;
  pontadat* pPontAdat ;

  // Meghatarozzuk a lista ablakban kivalasztott elem indexet
  nKivSor = pListaAbl->GetCurSel() ;

  if ( nKivSor != LB_ERR )
  {// Ha nincs semmi kiszelektalva azzal ne foglalkozzunk
    uiKulcs = pListaAbl->GetItemData(nKivSor) ; 

    sKivSor  = "" ;

    // Ha nem talalna, ures sort ir ki
    if ( m_pPontozas->Lookup( uiKulcs, pPontAdat) == TRUE )
    {
      PontbolStr( pPontAdat, sKivSor, ' ') ;
    }

    pSzerkAbl->SetWindowText(sKivSor) ;
  }
}

void CPontFelv::OnFelvesz()
{
  CEdit*    pSzerkAbl = (CEdit*) GetDlgItem(IDC_PFLV_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_P_LIST) ;

  CString sBeoSor ;
  int     nListAblPoz ;

  unsigned  uiKulcs   ;
  pontadat* pPontAdat ;

  CString sSzov ;
  char    pszSzov[101] ;

  unsigned char s = 1 , o = 0 ;

  // Beolvassuk a szoveget az editablakbol
  pSzerkAbl->GetWindowText(sBeoSor) ;

  UresLevagEV(sBeoSor) ;

  if ( sBeoSor != "" )
  {// Az UresLevagEV utan lehet "" is, ezzel ne foglalkozzunk
   // Tovabba ne akarjunk tobbet felvenni, mint amennyi befer ...
    if ( (pPontAdat = new pontadat()) == NULL )
    {
      if ( sSzov.LoadString(IDS_MEMF_PRBL_SZOV) == FALSE )
        sSzov = "IDS_MEMF_PRBL_SZOV" ;

      return ;
    }

    StrbolPont( sBeoSor, pPontAdat) ;

    if ( PontozEll(  pPontAdat) == TRUE )
    {
      if ( m_pPontozas->SQL_PontINSERT( *pPontAdat,  &uiKulcs) == 0 )
      {
        m_pPontozas->SetAt( uiKulcs, pPontAdat) ;

        PontbolStr( pPontAdat, sBeoSor, '\t') ;

        if ( (nListAblPoz = pListaAbl->AddString( sBeoSor)) != LB_ERR )
        {
          pListaAbl->SetItemData( nListAblPoz, uiKulcs) ;
          pListaAbl->SetCurSel( nListAblPoz) ;
        
          pSzerkAbl->SetWindowText("") ;
          pSzerkAbl->SetFocus() ;

          // A kivalasztott sor Modositasa miatt a pontozas mentetlen lett
          m_bPontozMentve = FALSE ;
        }
      }
      else // Nem sikerult uj nevet az adatbazisba felvenni
      {
        delete pPontAdat ;

        GetWindowText( sBeoSor) ;

        sprintf(pszSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

        MessageBox( pszSzov, sBeoSor, MB_OK | MB_ICONSTOP ) ;
      }
    }
    else
    {
      delete pPontAdat ;

      if ( sSzov.LoadString(IDS_PNT_SOR_NERV_S) == FALSE )
        sSzov = "IDS_PNT_SOR_NERV_S" ;

      GetWindowText( sBeoSor) ;

      MessageBox( sSzov, sBeoSor, MB_OK | MB_ICONEXCLAMATION ) ;

      // Az egesz sor kijelolese : DWORD felso szava -1, az also 0.
      pSzerkAbl->SetFocus() ;
      pSzerkAbl->SetSel( ((DWORD)((WORD)(0xffff)))<<16, FALSE ) ;
    }
  }
}

void CPontFelv::OnModosit()
{
  CEdit*    pSzerkAbl = (CEdit*) GetDlgItem(IDC_PFLV_EDIT) ;
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_P_LIST) ;

  unsigned  uiKulcs   ;
  pontadat  sUjPontAdat ;
  pontadat* pPontAdat ;

  CString sBeoSor , sRegiSor;
  int nKivSor ;

  // Beolvassuk a szoveget az editablakbol
  pSzerkAbl->GetWindowText( sBeoSor) ;
  // Meghatarozzuk a lista ablakban kivalasztott elem indexet
  nKivSor = pListaAbl->GetCurSel() ;

  UresLevagEV( sBeoSor) ;

  if ( (sBeoSor != "") && (nKivSor != LB_ERR) )
  {// Az UresLevagEV utan lehet "" is, ezzel ne foglalkozzunk
   // Vagy, ha nincs semmi kiszelektalva
    uiKulcs = pListaAbl->GetItemData( nKivSor) ;

    if ( m_pPontozas->Lookup( uiKulcs, pPontAdat) == TRUE )
    {
      StrbolPont( sBeoSor, &sUjPontAdat) ;

      if ( PontozEll( &sUjPontAdat) == TRUE )
      {
        if ( m_pPontozas->SQL_PontUPDATE( uiKulcs, sUjPontAdat) == 0 )
        {
          *pPontAdat = sUjPontAdat ;

          // A kivalasztott sor Modositasa miatt a pontozas mentetlen lett
          m_bPontozMentve = FALSE ;

          PontbolStr( pPontAdat, sBeoSor, '\t') ;

          // Listaablak : regi torol, uj felvesz !
          pListaAbl->DeleteString( nKivSor) ;

          if ( (nKivSor = pListaAbl->AddString( sBeoSor)) != LB_ERR )
          {
            pListaAbl->SetItemData( nKivSor, uiKulcs) ;
            pListaAbl->SetCurSel( nKivSor) ;
        
            pSzerkAbl->SetWindowText("") ;
            pSzerkAbl->SetFocus() ;
          }
        }
      }
    }
    else
    {
      if ( sBeoSor.LoadString(IDS_PNT_SOR_NERV_S) == FALSE )
        sBeoSor = "IDS_PNT_SOR_NERV_S" ;

      GetWindowText( sRegiSor) ;

      MessageBox( sBeoSor , sRegiSor , MB_OK | MB_ICONEXCLAMATION ) ;

      // Az egesz sor kijelolese : DWORD felso szava -1, az also 0.
      pSzerkAbl->SetSel( ((DWORD)((WORD)(0xffff)))<<16, FALSE ) ;
    }
  }
}

void CPontFelv::OnTorol()
{
  CListBox* pListaAbl = (CListBox*) GetDlgItem(IDC_P_LIST) ;

  int       nKivSor    ;

  pontadat* pPontAdat  ;
  unsigned  uiKivKulcs ;
  CString   sKivPontoz ;
  CString   sAblCim    ;
  CString   sHibaszov  ;

  // Meghatarozzuk a lista ablakban kivalasztott elem indexet
  nKivSor = pListaAbl->GetCurSel() ;

  if ( nKivSor != LB_ERR )
  {
    uiKivKulcs = pListaAbl->GetItemData( nKivSor) ;

    if ( m_pPontozas->SQL_PontDELETE( uiKivKulcs) == 0 )
    {
      // Sikeresen torolve az adatbazisbol : torles a mapben
      m_pPontozas->RemoveKey( uiKivKulcs) ;

      // Torles a lista ablakban :
      pListaAbl->DeleteString( nKivSor) ;

      // A torolt elem elotti kijelolese a lista ablakban
      if ( nKivSor > 0 )
      {
        pListaAbl->SetCurSel( nKivSor-1) ;
      }
      else
      {
        pListaAbl->SetCurSel( nKivSor) ;
      }

      m_bPontozMentve = FALSE ;
    }
    else
    {
      if ( m_pPontozas->Lookup( uiKivKulcs, pPontAdat) == TRUE )
      {
        PontbolStr( pPontAdat, sKivPontoz, ' ') ;

        GetWindowText( sAblCim) ;

        if ( sHibaszov.LoadString(IDS_N_TRLHETO_S) == FALSE )
          sHibaszov = "IDS_N_TRLHETO_S" ;

        MessageBox( sKivPontoz + sHibaszov,
                    sAblCim , MB_OK | MB_ICONEXCLAMATION ) ;
      }
    }
	}
}

// Kisegito fuggvenyek
BOOL CPontFelv::PontozEll( pontadat* pPontAdat)
{
  unsigned char i ;

  unsigned char debug1, debug2 ;

  if ( MAX_ERTEKELT > 1 )
  {
    i = 0 ;

    while ( i < MAX_ERTEKELT-1 )
    {
      debug1 = (*pPontAdat)[i]   ;
      debug2 = (*pPontAdat)[i+1] ;

      if ( debug1 < debug2 )
        return FALSE ;

      i++ ;
    }
  }

  if ( (*pPontAdat)[0] != 0 )
  {
    return TRUE ;
  }

  return FALSE ;
}

// CNevFelv osztalybol atveve
void CPontFelv::UresLevagEV(CString& sMegvagando)
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

void CPontFelv::PontbolStr(pontadat* pPontAdat, CString& sCelStr, char cElv)
{
  int i ;
  char pszPontszam[18] ;

  if ( pPontAdat == NULL )
    return ;

  sCelStr = "" ;

  for ( i = 0 ; i < MAX_ERTEKELT-1 ; i++)
  {
    sprintf( pszPontszam, "%d%c", (*pPontAdat)[i], cElv) ;
    sCelStr = sCelStr + pszPontszam ;
  }

  if ( i < MAX_ERTEKELT )
  {
    sprintf( pszPontszam, "%d", (*pPontAdat)[i]) ;
    sCelStr = sCelStr + pszPontszam ;
  }
}

unsigned char CPontFelv::StrbolPont(CString sSztring, pontadat* pPontAdat)
{
  int i = 0 ;
  int nHelyezes = 0 ;
  unsigned int uiAtmeneti = 0 ;
  int nHossz ;

  nHossz = sSztring.GetLength() ;

  if ( pPontAdat == NULL || nHossz  == 0 )
    return 0 ;

  // Nullazas-biztos, ami biztos
  memset( pPontAdat, '\0', MAX_ERTEKELT) ;

  nHelyezes = 0 ;
  uiAtmeneti = 0 ;

  while ( (i < nHossz) && (nHelyezes < MAX_ERTEKELT) )
  {
    // Ha ket szam kozt tobb,mint 1 elv kar van, azokat is atlepni
    while ( i < nHossz && ( sSztring[i] < '0' || '9' < sSztring[i] ) )
      i++ ;

    while ( i < nHossz && ( '0' <= sSztring[i] ) && ( sSztring[i] <= '9' ) )
    { // Egy helyiertekkel balra + uj szjegy
      uiAtmeneti = uiAtmeneti*10 ;

      // Ha nem kiseb, mint 255
      if ( uiAtmeneti > 255 )
        return i ;

      uiAtmeneti = uiAtmeneti + sSztring[i] - '0' ;
      i++ ;
    }

    if ( uiAtmeneti > 255 )
      return i ;

    (*pPontAdat)[nHelyezes] = uiAtmeneti ;

    uiAtmeneti = 0 ;
    nHelyezes++ ;
  }

  return i ;
}
