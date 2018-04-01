// szezndva.cpp : implementation file
//

#include "stdafx.h"

// A proba13.h miatt kell a kovetkezo negy include
#include "resource.h"
#include "alaptip.h"

#include "dbglobal.h"
#include "ensqlcam.h"

#include "cnev.h"
#include "cvnev.h"
#include "pontozas.h"

#include "cszezon.h"
#include "idoeredm.h"
#include "nevzlist.h"
#include "rajtsbef.h"
#include "impgyabl.h"
#include "rsbeffel.h"

#include "proba13.h"
#include "szezndva.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSzezNdVal dialog

CSzezNdVal::CSzezNdVal(CWnd* pParent /*=NULL*/)
	: CDialog(CSzezNdVal::IDD, pParent)
{
  // Ezt a mutatot nem konstruktorban adom at, mivel fixnek tekintheto :
  // nem kell visszairni OK-val valo kiszallasnal, a nevezes listat meg
  // kilepes elott mindig mentem ...
  m_pNagyd = &( ( (CProba13App*)AfxGetApp() )->m_cNagydijNev ) ;

  // Az amodalitas miatt
  Create( IDD, pParent) ;
}

// A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
// DoDataExchange fuggvenybol
BOOL CSzezNdVal::OnInitDialog()
{
  CDialog::OnInitDialog() ;

  CListBox* pSzezLb  = (CListBox*) GetDlgItem(IDC_FLV_SZEZ_LB) ;

  unsigned* pSzezTomb ;
  unsigned  nSzezdb   ;
  unsigned short i ;
  char pszSzezEve[10] ;

  long nRC ;

  // A rajtsorrend RadioButton beallitasa IDC_RAJTSORREND_R    IDC_BEFUTO_R
  //                az elso Rb         az utolso Rb  a bekapcsolando Rb
  CheckRadioButton( IDC_RAJTSORREND_R, IDC_BEFUTO_R, IDC_RAJTSORREND_R) ;

  //--------------------------------------------------------------------------
  // A felvitt szezonok evszamait tartalmazo Lb feltoltese

  pSzezTomb = NULL ;
  
  if ( (nRC=SQL_FelvSzez( &pSzezTomb, &nSzezdb)) != 0 )
  {
    HibaKiir( nRC) ;

    return FALSE ;
  }

  for ( i = 0 ; i < nSzezdb ; i++ )
  {
    sprintf( pszSzezEve, "%d", *(pSzezTomb+i)) ;
    pSzezLb->SetItemData( pSzezLb->AddString( pszSzezEve), (DWORD) *(pSzezTomb+i)) ;
  }

  delete [] pSzezTomb ;

  // A list box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
	pSzezLb->SetCurSel(0) ;

  // A kijelolt szezon nagydijainak megjelenitese
  OnSelchgFlvSzez() ;

  return TRUE ;
}

void CSzezNdVal::OnCancel()
{
  long nRC ;

  if ( ( nRC = SQL_ROLLBACK() ) != 0 )
  {
    HibaKiir( nRC) ;
  }

  // Ismet fel lehet hivni ezt az ablakot, mert nem lett CRsBefFelv felhivva
  gl_bFelvitelRsBefAktiv = TRUE ;

  DestroyWindow() ;
//  CDialog::OnCancel(); amodalis dialogusablak eseteben tilos meghivni
}

BEGIN_MESSAGE_MAP(CSzezNdVal, CDialog)
  //{{AFX_MSG_MAP(CSzezNdVal)
    ON_LBN_SELCHANGE(IDC_FLV_SZEZ_LB, OnSelchgFlvSzez)
    ON_BN_CLICKED(ID_SZERK_B, OnSzerk)
  //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSzezNdVal message handlers

void CSzezNdVal::OnSelchgFlvSzez()
{
  CListBox* pSzezLb  = (CListBox*) GetDlgItem(IDC_FLV_SZEZ_LB)  ;
  CListBox* pNagydLb = (CListBox*) GetDlgItem(IDC_NAGYDIJAK_LB) ;

  nevadat* pNevAdat ;
  unsigned uiNagydKulcs ;
  DWORD    dwItemData ;

  int      nKivSzez, i ;
  unsigned uiSzezon ;
  long     nRC ;
	CString  sKivSzez ;
  char     pszHibasNdKulcs[11] ;

	nKivSzez = pSzezLb->GetCurSel() ;

  // Ha van ervenyes kijeloles a szezonokat tartalmazo Lb-ban
  if ( nKivSzez != LB_ERR )
  {
    m_cAktSzez.Kiurit() ;

    uiSzezon = (unsigned) pSzezLb->GetItemData( nKivSzez) ;

    // Ha megtalalta a betoltendo szezont tartalmazo file-t : kiirja
    if ( (nRC = m_cAktSzez.SQL_SzezonBetolt( uiSzezon)) == 0 )
    {
      pNagydLb->ResetContent() ;

      nKivSzez = m_cAktSzez.Meret() ;

      for ( i = 0 ; i < nKivSzez ; i++ )
      {
        // A nagydij nevenek meghatarozasa
        uiNagydKulcs = m_cAktSzez.NagydKulcs( i) ;
        dwItemData   = DWORD( uiNagydKulcs) ;

        if ( m_pNagyd->Lookup( uiNagydKulcs, pNevAdat) )
        {
          // A sorhoz tartozo adatok beallitasa: "DWORD"
          pNagydLb->SetItemData( pNagydLb->AddString(pNevAdat->m_sNev), dwItemData) ;
        }
        else
        {
          // Nem talalt ilyen kulccsal bejegyzest, (megvaltozott a nagydij_nev
          // es a nagydijak tabla?) kiirja a hibas kulcsot
          sprintf( pszHibasNdKulcs, "%d", uiNagydKulcs) ;

          pNagydLb->SetItemData( pNagydLb->AddString(pszHibasNdKulcs), dwItemData) ;
        }
      }

      // Mindig kijeloli az elso elemet
      pNagydLb->SetFocus()   ;
      pNagydLb->SetCurSel(0) ;
    }
    else
    {
      HibaKiir( nRC) ;
    }
  }
}

void CSzezNdVal::OnSzerk()
{
  CListBox* pSzezLb  = (CListBox*) GetDlgItem(IDC_FLV_SZEZ_LB)  ;
  CListBox* pNagydLb = (CListBox*) GetDlgItem(IDC_NAGYDIJAK_LB) ;

  CRsBefFelv* pRajtsFelv ;
  int nKivSzez ;
  DWORD dwNdKulcs ;

  int nRajtsBef = GetCheckedRadioButton( IDC_RAJTSORREND_R, IDC_BEFUTO_R) ;

  // A LB adatanak lekerdezese: DWORD - nagydijnev kod
  nKivSzez = pNagydLb->GetCurSel() ;

  if ( nKivSzez != LB_ERR )
  {
    dwNdKulcs = pNagydLb->GetItemData( nKivSzez) ;

    // Visszaadja a bekapcsolt Rb ID-jet, ha nincs, 0-t
    switch( nRajtsBef )
    {
      // Rajtsorrend felvitele
      case IDC_RAJTSORREND_R :
      {
        pRajtsFelv = new CRsBefFelv( m_cAktSzez.m_uiSzezon, dwNdKulcs, RAJTS) ;

        if ( pRajtsFelv != NULL )
        {
          pRajtsFelv->Create( IDD_RAJTS_FELV, AfxGetMainWnd() ) ;
        }
        else
        {
          gl_bFelvitelRsBefAktiv = TRUE ;
        }

        break ;
      }

      // Befuto felvitele
      case IDC_BEFUTO_R      :
      {
        pRajtsFelv = new CRsBefFelv( m_cAktSzez.m_uiSzezon, dwNdKulcs, BEFUTO) ;

        if ( pRajtsFelv != NULL )
        {
          pRajtsFelv->Create( IDD_BEF_FELV, AfxGetMainWnd() ) ;
        }
        else
        {
          gl_bFelvitelRsBefAktiv = TRUE ;
        }

        break ;
      }
    }
  }
  else
  {
    gl_bFelvitelRsBefAktiv = TRUE ;
  }

  // Az amodalis dialogusablak megszuntetese
//  OnCancel() ; helyett (gl.valt. kez. miatt !)
  DestroyWindow() ;
}

void CSzezNdVal::PostNcDestroy()
{
  // A paredit.cpp-ben sem hivja a bazisosztaly azonos nevu fv.et...
  delete this ;
}

void CSzezNdVal::HibaKiir(long nRC)
{
  CString  sHibaSzov, sAblCim ;

  char pszHibaSzov[75] ;

  GetWindowText( sAblCim) ;

  if ( nRC == -1 )
  {
    if ( sHibaSzov.LoadString(IDS_MEMF_PRBL_SZOV) == FALSE )
      sHibaSzov = "IDS_MEMF_PRBL_SZOV" ;

    MessageBox( sHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) ;
  }
  else // Ha nem memoriafoglalasi hiba, akkor SQL hiba
  {
    sprintf(pszHibaSzov,"%.*s", (int)gl_sEnSQLCAm.sqlerrm.sqlerrml,
                                (const char *)gl_sEnSQLCAm.sqlerrm.sqlerrmc) ;

    MessageBox( pszHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) ;
  }
}
