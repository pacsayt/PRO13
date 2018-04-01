// lekerddl.cpp : implementation file
//

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"

#include "ensqlcam.h"
#include "dbglobal.h"

#include "pontozas.h"
#include "cnev.h"
#include "cvnev.h"
#include "proba13.h"

#include "cszezon.h"
#include "idoeredm.h"
#include "nevzlist.h"
#include "rajtsbef.h"
#include "impgyabl.h"
#include "rsbeffel.h"  // enum eAblTipRsBef

#include "kerermez.h"
#include "fgyeelgy.h"
#include "holvkiv.h"
#include "pillall.h"
#include "vbszerep.h"

#include "lkmgjvaz.h"
#include "lek1doc.h"
#include "lekerddl.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLekerdDlg dialog


CLekerdDlg::CLekerdDlg(eNevtipus enNevtipus, CWnd* pParent)
	: CDialog(CLekerdDlg::IDD, pParent)
{
  m_enNevtipus  = enNevtipus ;
  // Valamely enum erteket veheti csak fel ...
  m_enLekTip    = eFutgy     ;
  m_uiSzezontol = 0    ;
//  m_usSzezonig  = 0    ;
  m_nNevekSzama = 0    ;
  m_pCLekEr1Doc = NULL ;
  m_pVBSzerepl  = NULL ;

  switch ( enNevtipus )
  {
    case eVersNt    :
      m_pVNev = &((CProba13App*)AfxGetApp())->m_cVersenyzoNev ;
      m_pNev  = NULL ;
      // Versenyzo neveknel kell tabstop
      m_nTabstopok[0] = LOWORD(GetDialogBaseUnits())*6 ;
      break ;

    case eNemzNt    :
      m_pVNev = NULL ;
      m_pNev  = &((CProba13App*)AfxGetApp())->m_cNemzetisegNev ;
      break ;

    case eCsapNt    :
      m_pVNev = NULL ;
      m_pNev  = &((CProba13App*)AfxGetApp())->m_cCsapatNev ;
      break ;

    case eMotorNt   :
      m_pVNev = NULL ;
      m_pNev  = &((CProba13App*)AfxGetApp())->m_cMotorNev ;
      break ;

    case eNagydijNt :
      m_pVNev = NULL ;
      m_pNev  = &((CProba13App*)AfxGetApp())->m_cNagydijNev ;
      break ;

    default : //       hiba !
      m_pVNev = NULL ;
      m_pNev  = NULL ;
  }

  m_pKivNevek = NULL ;
}

BOOL CLekerdDlg::OnInitDialog()
{
  CDialog::OnInitDialog() ;

  CListBox * pNevLst = (CListBox* ) GetDlgItem(IDC_LEK1_NEVEK_LB) ;
  CComboBox* pSzezCb = (CComboBox*) GetDlgItem(IDC_LEK1_SZEZ_CB)  ;

  vnevadat* pVAdat ;
  nevadat*  pAdat  ;

  unsigned  nSzezdb ;
  unsigned* pFlvSzezTomb ;
  POSITION  poz   ;
  unsigned  uiKulcs ;

  unsigned short i ;
  CString sRBSzov  ;
  char pszEv[5]    ;

  long nRC ;

  // A lekerdezes fajtajat megado radiogombok feliratozasa
  sRBSzov.LoadString(IDS_VB_SZEREP_SZOV) ;
  GetDlgItem(IDC_LEK1_VBSZER_RB1)->SetWindowText(sRBSzov) ;

  sRBSzov.LoadString(IDS_FUT_GYOZ_SZOV) ;
  GetDlgItem(IDC_LEK1_FUTGY_RB2)->SetWindowText(sRBSzov) ;

  sRBSzov.LoadString(IDS_EDZ_ELS_SZOV) ;
  GetDlgItem(IDC_LEK1_EDZELS_RB3)->SetWindowText(sRBSzov) ;

  sRBSzov.LoadString(IDS_LEGGY_KOR_SZOV) ;
  GetDlgItem(IDC_LEK1_LEGGYKOR_RB4)->SetWindowText(sRBSzov) ;

  sRBSzov.LoadString(IDS_OSSZ_ER_SZOV) ;
  GetDlgItem(IDC_LEK1_OSSZER_RB5)->SetWindowText(sRBSzov) ;

  sRBSzov.LoadString(IDS_HOL_VERS_SZOV) ;
  GetDlgItem(IDC_LEK1_HOLVERS_RB6)->SetWindowText(sRBSzov) ;

  sRBSzov.LoadString(IDS_VB_PILL_ALL_SZOV) ;
  GetDlgItem(IDC_VBPILLALL_RB7)->SetWindowText(sRBSzov) ;

  // A felvitt szezonok evszamait tartalmazo Cb feltoltese
  nSzezdb = 0 ;
  pFlvSzezTomb = NULL ;

  nRC = SQL_FelvSzez( &pFlvSzezTomb, &nSzezdb) ;
  
  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;
  }

  i = 0 ;
  while ( i < nSzezdb )
  {
    sprintf( pszEv, "%d", pFlvSzezTomb[i]) ;
    pSzezCb->SetItemData(pSzezCb->AddString(pszEv), (DWORD) pFlvSzezTomb[i]) ;
    i++ ;
  }

  delete [] pFlvSzezTomb ;

  // Az esetleges RB deaktivizalas a lekerdezett nev fvben
  switch ( m_enNevtipus )
  {
    case eVersNt    :
      sRBSzov.LoadString(ID_LEKERD_VERSENYZNV) ;
      SetWindowText(sRBSzov) ;
      // A lekerdezes fajtajat megado radiogombok beallitasa
      //                az elso Rb         az utolso Rb  a bekapcsolando Rb
      CheckRadioButton( IDC_LEK1_VBSZER_RB1, IDC_VBPILLALL_RB7, IDC_LEK1_VBSZER_RB1) ;

      break ;

    case eNemzNt    :
      sRBSzov.LoadString(ID_LEKERD_NEMZETISGNV) ;
      SetWindowText(sRBSzov) ;
      // A lekerdezes fajtajat megado radiogombok beallitasa
      //                az elso Rb         az utolso Rb  a bekapcsolando Rb
      CheckRadioButton( IDC_LEK1_VBSZER_RB1, IDC_VBPILLALL_RB7, IDC_LEK1_VBSZER_RB1) ;
      break ;

    case eCsapNt    :
      sRBSzov.LoadString(ID_LEKERD_CSAPAT) ;
      SetWindowText(sRBSzov) ;
      // A lekerdezes fajtajat megado radiogombok beallitasa
      //                az elso Rb         az utolso Rb  a bekapcsolando Rb
      CheckRadioButton( IDC_LEK1_VBSZER_RB1, IDC_VBPILLALL_RB7, IDC_LEK1_VBSZER_RB1) ;
      break ;

    case eMotorNt   :
      sRBSzov.LoadString(ID_LEKERD_MOTORNV) ;
      SetWindowText(sRBSzov) ;
      // A lekerdezes fajtajat megado radiogombok beallitasa
      //                az elso Rb         az utolso Rb  a bekapcsolando Rb
      CheckRadioButton( IDC_LEK1_VBSZER_RB1, IDC_VBPILLALL_RB7, IDC_LEK1_VBSZER_RB1) ;
      break ;

    case eNagydijNt :
      sRBSzov.LoadString(ID_LEKERD_NAGYDJNV) ;
      SetWindowText(sRBSzov) ;
      GetDlgItem(IDC_LEK1_VBSZER_RB1)->EnableWindow(FALSE) ;
      GetDlgItem(IDC_LEK1_HOLVERS_RB6)->EnableWindow(FALSE) ;
      GetDlgItem(IDC_VBPILLALL_RB7)->EnableWindow(FALSE) ;
      // A lekerdezes fajtajat megado radiogombok beallitasa
      //                az elso Rb         az utolso Rb  a bekapcsolando Rb  
      CheckRadioButton( IDC_LEK1_VBSZER_RB1, IDC_VBPILLALL_RB7, IDC_LEK1_FUTGY_RB2) ; 
      break ;

    default : //       hiba ! (az osszes RB letiltva)
      GetDlgItem(IDC_LEK1_VBSZER_RB1)->EnableWindow(FALSE)  ;
      GetDlgItem(IDC_LEK1_FUTGY_RB2)->EnableWindow(FALSE)   ;
      GetDlgItem(IDC_LEK1_EDZELS_RB3)->EnableWindow(FALSE)  ;
      GetDlgItem(IDC_LEK1_LEGGYKOR_RB4)->EnableWindow(FALSE);
      GetDlgItem(IDC_LEK1_OSSZER_RB5)->EnableWindow(FALSE)  ;
      GetDlgItem(IDC_LEK1_HOLVERS_RB6)->EnableWindow(FALSE) ;
      GetDlgItem(IDC_VBPILLALL_RB7)->EnableWindow(FALSE)    ;
  }

  // A neveket tartalmazo LB feltoltese
  // - versenyzo nevekkel
  // - egyeb nevekkel
  if ( m_pVNev != NULL )
  {
    // A versenyzo neveknel kell tabstop :
    pNevLst->SetTabStops( 1, m_nTabstopok) ;

    poz = m_pVNev->GetStartPosition() ;
    while( poz != NULL)
    {
      m_pVNev->GetNextAssoc( poz, uiKulcs, pVAdat ) ;
      pNevLst->SetItemData(pNevLst->AddString(pVAdat->m_sVezNev+",\t"+pVAdat->m_sKerNev), uiKulcs) ;
    }
  }
  else
  {
    if ( m_pNev != NULL )
    {
      poz = m_pNev->GetStartPosition() ;
      while( poz != NULL)
      {
        m_pNev->GetNextAssoc( poz, uiKulcs, pAdat ) ;
        pNevLst->SetItemData(pNevLst->AddString(pAdat->m_sNev), uiKulcs) ;
      }
    }
    else
    {
       // Mindket mutato ures : hiba (l. konstr.)
    }
  }

  return TRUE ;
}

BEGIN_MESSAGE_MAP(CLekerdDlg, CDialog)
  //{{AFX_MSG_MAP(CLekerdDlg)
    ON_BN_CLICKED(IDC_KERESES_B, OnKereses)
    ON_BN_CLICKED(IDC_KILEPES_B, OnKilepes)
  //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CLekerdDlg message handlers

void CLekerdDlg::OnKereses()
{
//TRACE("\nCLekerdDlg::OnKereses : BEGIN") ;
  int nLekTip ;
  unsigned /*nTmbMeret,*/ nIdx ;
  CDocument*   pBiztonsagi ;
  CString      sDocNev("CLekEredm1Doc") ;
  CProba13App* pPro13App = (CProba13App*)AfxGetApp() ;

  unsigned* pFlvSzezTomb ;
  unsigned  nFlvSzezdb   ;

  long nRC ;

//    0 , ha nincs kiv
  nLekTip = GetCheckedRadioButton( IDC_LEK1_VBSZER_RB1, IDC_VBPILLALL_RB7) ;

  /* Az RB-azonositok atirasa enumma */
  switch ( nLekTip )
  {
    case IDC_LEK1_FUTGY_RB2 :
      m_enLekTip = eFutgy ;
      break ;
    case IDC_LEK1_EDZELS_RB3 :
      m_enLekTip = eEdzEls ;
      break ;
    case IDC_LEK1_LEGGYKOR_RB4 :
      m_enLekTip = eLeggyKor ;
      break ;
    case IDC_LEK1_OSSZER_RB5 :
      m_enLekTip = eOsszEr ;
      break ;
    case IDC_LEK1_HOLVERS_RB6 :
      m_enLekTip = eHolvers ;
      break ;
    case IDC_VBPILLALL_RB7 :
      m_enLekTip = eVBPillAll ;
      break ;
    case IDC_LEK1_VBSZER_RB1 :
      m_enLekTip = eVBSzer ;
      break ;
    default :
      return ;
  }

  /* Tagvaltozok implicit feltoltese : m_pKivNevek / m_nNevekSzama, m_uiSzezontol */
  NevKezdSzezBeo( m_uiSzezontol) ;

  // Ha NevKezdSzezBeo m_nNevekSzama-t 0-ra allitotta, az
  // 0 kivalasztott nevet, vagy hibat jelez
	if ( m_nNevekSzama != 0 && m_enLekTip != eVBPillAll )
	{
    switch ( nLekTip )
    {
      // Minegyik tagfuggveny az m_uiSzezontol tagvaltozot hasznalja
      case IDC_LEK1_HOLVERS_RB6 :
        HolVersLekerd() ;
        break ;
      case IDC_LEK1_VBSZER_RB1  :
        VBSzereplLekerd() ;
        break ;
      default :
        // A nagydij nevekre vonatkozo lekerdezesek is !
        AltLekerd() ;
    }

    // Az osszes szezonon valo vegigmenes utan rendezzuk a vb szerepleseket
    if ( m_enLekTip == eVBSzer && m_pVBSzerepl != NULL )
    {
      if ( m_pCLekEr1Doc == NULL )
      {
        pBiztonsagi = pPro13App->DocViewFrameLetr(sDocNev) ;

        if ( pBiztonsagi == NULL )
          return ;
 
        // Ellenorizzuk, hogy a megfelelo osztalytipust sikerult-e
        // letrehozni
        if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
        {
          m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;

          // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
          m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
          m_pCLekEr1Doc->DokuNevKiir() ;

          // Az eredmenyek hozzaadasa a dokumentumhoz
          nIdx = 0 ; 
          while ( m_pVBSzerepl[nIdx] != NULL )
          {
            m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( m_pVBSzerepl[nIdx]) ;
            nIdx++ ;
          }
        }
        else
        {// Ez valami fatalis hiba
          delete pBiztonsagi ;
          return ;
        }
      }

      delete [] m_pVBSzerepl ;
      m_pVBSzerepl  = NULL ;
    }
  } // m_nNevekSzama != 0
  else
  {
    if ( m_enLekTip == eVBPillAll && m_uiSzezontol != 0 )
    {
      pFlvSzezTomb = NULL ;

      nRC = SQL_FelvSzez( &pFlvSzezTomb, &nFlvSzezdb) ;

      if ( nRC == 0 )
      {
        nIdx = 0 ;

        while ( nIdx < nFlvSzezdb && pFlvSzezTomb[nIdx] != m_uiSzezontol )
        {
          nIdx++ ;
        }

        if ( nIdx < nFlvSzezdb && pFlvSzezTomb[nIdx] == m_uiSzezontol )
        {
          NvzLstBetolt(m_uiSzezontol) ; // A PillAllLekerd()-ben is

          SzezBetolt(m_uiSzezontol)   ; // A PillAllLekerd()-ben is

          // Nehany nvzlst fv helyes mukodesehez elengedhetetlenul szukseges
  	      // az aktualis szezon megadasa
          m_cAktNvzLst.m_uiSzezon = m_cAktSzez.m_uiSzezon ;

//          m_usSzezonig = m_uiSzezontol ;
          // Hogy a dokumentum nevenek beallitasanal ne legyen problema
          
          PillAllLekerd() ;
        }
      }
      else
      {
        HibaKiir( nRC) ;
      }

      // Lehet, hogy hiba eseten is tortent memoriafoglalas
      if ( nFlvSzezdb > 0 )
      {
        delete [] pFlvSzezTomb ;
      }
    }
  }

  // Mivel az CView::OnUpdate fv. eloszor a dokument ures allapotaban
  // hivodik meg, amikoris a scroll meretet meg nem t. megallapitani:
  if ( m_pCLekEr1Doc != NULL )
  {
    m_pCLekEr1Doc->UpdateAllViews( NULL) ;
    // A lekerdezes elkeszult, johet az ujabb
    m_pCLekEr1Doc = NULL ;
  }
//TRACE("\nCLekerdDlg::OnKereses : END") ;
}

void CLekerdDlg::OnKilepes()
{
//TRACE("\nCLekerdDlg::OnKilepes : BEGIN") ;
  if ( m_pKivNevek != NULL )
  {
    delete [] m_pKivNevek ;
  }

  // CDialog::OnCancel->CDialog::EndDialog barhonnet felhivhato.
  EndDialog(IDCANCEL) ;
//TRACE("\nCLekerdDlg::OnKilepes : END") ;
}

void CLekerdDlg::HolVersLekerd()
{
  CProba13App* pPro13App = (CProba13App*)AfxGetApp() ;

  CNvzSor    cNvzMaszk/*, *pNvzLstSor */ ;
//  SNagydij*  pNagydij ;
  CWordArray cTalalat ;
//  int nTalalSzama, nNevIdx, i ;
  CHolVKiV*  pCHolVKiV ;
  CDocument* pBiztonsagi ;
  CString    sDocNev     ;

  sHolVKiVLekErTip* pHolVKiVLekEr ;
  unsigned          nKiTmbMeret   ;

  unsigned uiIdx ;

  long nRC ;

  sDocNev = "CLekEredm1Doc" ;

//TRACE("\nCLekerdDlg::HolVersLekerd : BEGIN") ;

  // OnKereses()-fvben olvassa be a kijelolt neveket, szezon evet
  nRC = SQL_HolVersKiVers(  m_enNevtipus, m_uiSzezontol, m_pKivNevek, m_nNevekSzama,
                           &pHolVKiVLekEr, &nKiTmbMeret) ;
/*
for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
{
  TRACE("\nV:%d, N:%d, C:%d, M:%d, N:%d, E:%d, H:%d, N:%d",
         (pHolVKiVLekEr+uiIdx)->uiVersenyzo,
         (pHolVKiVLekEr+uiIdx)->uiNemzetiseg,
         (pHolVKiVLekEr+uiIdx)->uiCsapat,
         (pHolVKiVLekEr+uiIdx)->uiMotor,
         (pHolVKiVLekEr+uiIdx)->uiNagydijNev,
         (pHolVKiVLekEr+uiIdx)->uiEv,
         (pHolVKiVLekEr+uiIdx)->uiHonap,
         (pHolVKiVLekEr+uiIdx)->uiNap
      ) ;
}
*/
  if ( nRC == 0 )
  {
    // Hogy csak az elso iteracios lepesben tortenjen dok letrehozas
    if ( m_pCLekEr1Doc == NULL )
    {
      pBiztonsagi = pPro13App->DocViewFrameLetr( sDocNev) ;

      if ( pBiztonsagi == NULL )
      {
        // A kivalasztott neveket tartalmazo tomb nem kell mar
        delete [] m_pKivNevek ;
        m_pKivNevek   = NULL  ;
        m_nNevekSzama = 0     ;

        // Az SQL-fv altal visszaadott tomb sem kell mar
        delete [] pHolVKiVLekEr ;

        return ;
      }

      // Ez is csak akkor kell, ha eppen letrehoztuk a triumviratust
      if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
      {
        m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;
        // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
        m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
        m_pCLekEr1Doc->DokuNevKiir() ;
      }
      else
      {
        delete pBiztonsagi ;

        // A kivalasztott neveket tartalmazo tomb nem kell mar
        delete [] m_pKivNevek ;
        m_pKivNevek   = NULL  ;
        m_nNevekSzama = 0     ;

        // Az SQL-fv altal visszaadott tomb sem kell mar
        delete [] pHolVKiVLekEr ;

        return ;
      }
    }

    for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
    {
      pCHolVKiV = new CHolVKiV() ;

      if ( pCHolVKiV != NULL )
      {              
        pCHolVKiV->Feltolt( pHolVKiVLekEr+uiIdx, m_enNevtipus) ;
        m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( pCHolVKiV) ;
      }
    }

    delete [] pHolVKiVLekEr ;
  }
  else
  {
    // Lehet, hogy nem az elso memoriafoglalas futott hibara
    if ( nKiTmbMeret > 0 )
    {
      delete [] pHolVKiVLekEr ;
    }

    HibaKiir( nRC) ;
  }

  // A kivalasztott neveket tartalmazo tomb nem kell mar
  delete [] m_pKivNevek ;
  m_pKivNevek   = NULL  ;
  m_nNevekSzama = 0     ;
}

void CLekerdDlg::PillAllLekerd()
{
  CProba13App* pPro13App = (CProba13App*)AfxGetApp() ;

  CDocument* pBiztonsagi  ;
  CString    sDocNev      ;

  CNvzSor*   pNvzLstSor ;

  sPillAllLekErTip sPillAllLekEr ;
  CPillAll*  pPillAll       = NULL ;
  CPillAll*  pEgyNevPillAll = NULL ;
  CPillAll*  pKezdPillAll   = NULL ;
  CPillAll*  pElozodPillAll = NULL ;

  unsigned char ucNdIdx   ;
  SDatum        sNdDatum  ;

  unsigned      nNevekIdx   ;
  unsigned      nEredmIdx   ;
  unsigned      uiKivNevIdx ;

  long nRC ;

  sDocNev = "CLekEredm1Doc" ;

  sPillAllLekEr.uiSzezErTmbMeret = 0 ;
  sPillAllLekEr.uiOsszErTmbMer = 0 ;

  // A kivalasztott nevek megadasanak csak csapat- es vers. neveknel van ertelme
  // Mas esetekben a szez. veger. v. es cs. nevek alapjan megy, nvzl alapjan
  // valogatok e fv.-ben
  if ( m_enNevtipus == eNemzNt || m_enNevtipus == eCsapNt || m_enNevtipus == eMotorNt )
  {
    // A kivalasztott neveket, sorbarendezi az SQL_PillAll fv, mert igy konnyebb
    // valogatnia
    nRC = SQL_PillAll( m_uiSzezontol, m_enNevtipus, m_pKivNevek, m_nNevekSzama,
                       &sPillAllLekEr) ;
  }
  else
  {
    // Itt mindig versenyzo nevek szerint kell csoportositani a szez eredmenyeket
    nRC = SQL_PillAll( m_uiSzezontol,     eVersNt,  m_pKivNevek, m_nNevekSzama,
                       &sPillAllLekEr) ;
  }

sPillAllLekEr.Trace() ;

  // Lehet, hogy nem az elso memoriafoglalas futott hibara
  if ( nRC == 0 )
  {
    // A datum mezo feltoltesehez kell a szezon is
    // A nevezesi lista adatokat nem adja vissza az SQL tarolt eljaras,
    // ezeket a nevezesi listabol kell venni (elobb betolteni)
    if ( SzezBetolt( m_uiSzezontol) == FALSE || NvzLstBetolt( m_uiSzezontol) == FALSE )
    {
      // A kivalasztott neveket tartalmazo tomb felszabaditasa
      // Ugyan minden lekerdezes elott megprobalja, de ha nincs uj lekerdezes: mem.leak
      Felszabadit( sPillAllLekEr, &m_pKivNevek, &m_nNevekSzama) ;

      return ;
    }

    // A CPillAll-t nem hasznalom sorbarendezesre, a sPillAllLekEr.pNevHlyzTmb
    // sorbarendezett tombon vegigmenve adom hozza az egyes eredmenyeket
    for ( nNevekIdx = 0 ; nNevekIdx < sPillAllLekEr.uiSzezErTmbMeret ; nNevekIdx++ )
    {
/*
TRACE( "\nidx:%d. nev:%d helyez: %d  pont:%d",
       nNevekIdx+1,
       sPillAllLekEr.pNevKlcsTmb[nNevekIdx],
       sPillAllLekEr.pNevHlyzTmb[nNevekIdx],
       sPillAllLekEr.pSzezPntTmb[nNevekIdx]) ;
*/
      for ( nEredmIdx = 0 ; nEredmIdx < sPillAllLekEr.uiOsszErTmbMer ; nEredmIdx++ )
      {
/*
TRACE( "\nidx:%d. nev:%d  pont:%d helyz:%d nvzl:%d nd:%d",
       nEredmIdx,
       sPillAllLekEr.pNevEredmTmb[nEredmIdx],
       sPillAllLekEr.pPontTmb[nEredmIdx],
       sPillAllLekEr.pHelyezesTmb[nEredmIdx],
       sPillAllLekEr.pNvzlKulcsTmb[nEredmIdx],
       sPillAllLekEr.pNdNevTmb[nEredmIdx]) ;
*/
        if ( sPillAllLekEr.pNevKlcsTmb[nNevekIdx] == sPillAllLekEr.pNevEredmTmb[nEredmIdx] )
        {
          if ( m_cAktNvzLst.Lookup( sPillAllLekEr.pNvzlKulcsTmb[nEredmIdx], pNvzLstSor) == TRUE )
          {
            // Ez esetben a kivalasztas a nevezesi lista sor segitsegevel
            if ( m_nNevekSzama > 0 && (m_enNevtipus != eVersNt && m_enNevtipus != eCsapNt) )
            {
              // Itt elvileg csak eNemzNt, eMotorNt johet,
              // ezek azonos elbanasban reszesulnek
              if ( m_enNevtipus == eNemzNt )
              {
                uiKivNevIdx = 0 ;
                while ( uiKivNevIdx < m_nNevekSzama )
                {
                  if ( m_pKivNevek[uiKivNevIdx] == pNvzLstSor->uiNemz )
                  {
                    break ;
                  }
                  
                  uiKivNevIdx++ ;
                }
              }
              else
              {
                // Itt csak m_enNevtipus == eMotorNt nak szabadna elofordulnia
                uiKivNevIdx = 0 ;
                while ( uiKivNevIdx < m_nNevekSzama )
                {
                  if ( m_pKivNevek[uiKivNevIdx] == pNvzLstSor->uiMotor )
                  {
                    break ;
                  }
                  
                  uiKivNevIdx++ ;
                }
              }

              // Ez a nvzl kulcsban szereplo eNemzNt/eMotorNt nincs kivalasztva
              // Megy tovabb az eredmenyeken, ami onmagaban nem baj,
              // mert 1 szez er.-hez tobb futam er. is tartozhat, mas nvzl klcssal
              if ( uiKivNevIdx == m_nNevekSzama )
              {
                continue ;
              }
            }

            pPillAll = new CPillAll() ;

            if ( pPillAll == NULL )
            {
              Felszabadit( sPillAllLekEr, &m_pKivNevek, &m_nNevekSzama) ;

              if ( pEgyNevPillAll != NULL )
              {
                pEgyNevPillAll->Felszabadit() ;
                delete pEgyNevPillAll ;
              }

              if ( pPillAll != NULL )
              {
                delete pPillAll ;
              }

              return ;
            }

            // Az adott kulcsu nagydijhoz megadja a m_pNagydijak tombbeli
            // indexet. Ha nem talal ilyen nagydijat, m_ucNagydSzam jon vissza
            ucNdIdx = m_cAktSzez.NdBolIdx( sPillAllLekEr.pNdNevTmb[nEredmIdx]) ;
            if ( ucNdIdx != m_cAktSzez.Meret() )
            {
              sNdDatum = m_cAktSzez[ucNdIdx]->m_sDatum ;
            }
            else
            {
              sNdDatum = SDatum( m_uiSzezontol, 0, 0) ;
            }

            // Mert a lekerdezes \/ nem mindig versenyzo nevekre vonatkozik !
            pPillAll->Feltolt( pNvzLstSor->uiVersenyzo, pNvzLstSor->uiNemz, pNvzLstSor->uiCsapat, pNvzLstSor->uiMotor,
                               sPillAllLekEr.pNdNevTmb[nEredmIdx], 0, sNdDatum,
                               sPillAllLekEr.pPontTmb[nEredmIdx], sPillAllLekEr.pHelyezesTmb[nEredmIdx],
                               m_enNevtipus) ;

            if ( pEgyNevPillAll != NULL )
            {
              pPillAll->m_cVersMez.m_cKirajzol = 0 ;
              pEgyNevPillAll->UjFelvesz( pPillAll) ;
            }
            else
            {
              // Ezt irja ki ossz elert pontszamkent.
              // Korabban a CPillAll::OsszpntKisz allitotta (CLekerdDlg), most keszen jon
              pPillAll->m_cHelyezMez.m_usKmltErt = 2*sPillAllLekEr.pSzezPntTmb[nNevekIdx] ;
              pEgyNevPillAll = pPillAll ;
            }
          }
        }
      }

      // Valamiert new CPillAll() nelkul jut el ide, jollehet minden nevhez kene(?) eredmeny legyen ...
//      if ( pElozodPillAll != pEgyNevPillAll ) vagy ami (majdnem) ugyanaz
      if ( pEgyNevPillAll != NULL )
      {
        // Ehhez a nevhez nincs tobb eredmeny : felvetel az ablakba
        if ( pKezdPillAll != NULL )
        {
          pKezdPillAll->UjFelvesz( pEgyNevPillAll) ;
          pElozodPillAll = pEgyNevPillAll ;
        }
        else
        {
          pKezdPillAll = pEgyNevPillAll ;
          pElozodPillAll = pEgyNevPillAll ;
        }
      }

      pEgyNevPillAll = NULL ;
    }

    if ( m_pCLekEr1Doc == NULL )
    {
      pBiztonsagi = pPro13App->DocViewFrameLetr(sDocNev) ;

      if ( pBiztonsagi == NULL )
      {
        Felszabadit( sPillAllLekEr, &m_pKivNevek, &m_nNevekSzama) ;

        if ( pKezdPillAll != NULL )
        {
          pKezdPillAll->Felszabadit() ;
          delete pKezdPillAll ;
        }

        return ;
      }

      // Ellenorizzuk, hogy a megfelelo osztalytipust sikerult-e
      // letrehozni
      if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
      {
        m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;

        // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
        m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
        m_pCLekEr1Doc->DokuNevKiir() ;

        // A kivalasztott nevek felvetele a dokumentumba
        m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( pKezdPillAll) ;
      }
      else
      {
        // Ez valami fatalis hiba
        // A kivalasztott neveket tartalmazo tomb felszabaditasa
        // Ugyan minden lekerdezes elott megprobalja, de ha nincs uj lekerdezes: mem.leak
        Felszabadit( sPillAllLekEr, &m_pKivNevek, &m_nNevekSzama) ;
        pPillAll->Felszabadit() ;
        delete pPillAll ;

        delete pBiztonsagi ;
        return ;
      }
    }
  }
  else
  {
    HibaKiir( nRC) ;
  }

  // A kivalasztott neveket tartalmazo tomb felszabaditasa
  // Ugyan minden lekerdezes elott megprobalja, de ha nincs uj lekerdezes: mem.leak
  Felszabadit( sPillAllLekEr, &m_pKivNevek, &m_nNevekSzama) ;
}

// A fuggveny minden szezonra egyszer hivodik meg
void CLekerdDlg::VBSzereplLekerd()
{
  CString     sDocNev     ;
  CVBSzerepl* pVBSzerepl  ;
  CNvzSor*    pNvzLstSor ;

  sPillAllLekErTip sPillAllLekEr ;

  unsigned* pSzezTomb ;
  unsigned  nSzezdb   ;
  unsigned  i ;
  unsigned  nNevekIdx   ;
  unsigned  uiKivNevIdx ;
  unsigned  nEredmIdx   ;
  unsigned  nTmbMeret ;

  long      nRC ;

  sDocNev   = "CLekEredm1Doc" ;
  pSzezTomb = NULL ;

  nRC = SQL_FelvSzez( &pSzezTomb, &nSzezdb) ;

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;

    delete m_pKivNevek ;
    m_pKivNevek = NULL ;

    m_nNevekSzama = 0  ;

    return ;
  }

  // A kivalasztott szezontol kezdjuk a keresest
  i = 0 ;
  while ( i < nSzezdb && pSzezTomb[i] < m_uiSzezontol )
  {
    i++ ;
  }

  // Vegiglepkedes a felvitt szezonokon
  while ( i < nSzezdb )
  {
    nRC = SQL_PillAll( pSzezTomb[i], m_enNevtipus, m_pKivNevek, m_nNevekSzama,
                       &sPillAllLekEr) ;

TRACE("\n-szez_eve:%d----------KEZD\n", pSzezTomb[i]) ;
sPillAllLekEr.Trace() ;

    if ( nRC == 0 )
    {
      // A nevezesi lista adatokat nem adja vissza az SQL tarolt eljaras,
      // ezeket a nevezesi listabol kell venni (elobb betolteni)
      if ( NvzLstBetolt( pSzezTomb[i]) == TRUE )
      {
        for ( nNevekIdx = 0 ; nNevekIdx < sPillAllLekEr.uiSzezErTmbMeret ; nNevekIdx++ )
        {
          // A futam eredmenyeket tartalmazo reszben kell megkeresni a nevezesi
          // lista kulcsot
          for ( nEredmIdx = 0 ; nEredmIdx < sPillAllLekEr.uiOsszErTmbMer ; nEredmIdx++ )
          {
            if ( sPillAllLekEr.pNevKlcsTmb[nNevekIdx] == sPillAllLekEr.pNevEredmTmb[nEredmIdx] )
            {
              if ( m_cAktNvzLst.Lookup( sPillAllLekEr.pNvzlKulcsTmb[nEredmIdx], pNvzLstSor) == TRUE )
              {
                // Ez esetben a kivalasztas a nevezesi lista sor segitsegevel
                if ( m_nNevekSzama > 0 && (m_enNevtipus != eVersNt && m_enNevtipus != eCsapNt) )
                {
                  // Itt elvileg csak eNemzNt, eMotorNt johet,
                  // ezek azonos elbanasban reszesulnek
                  uiKivNevIdx = 0 ;
                  if ( m_enNevtipus == eNemzNt )
                  {
                    while ( uiKivNevIdx < m_nNevekSzama )
                    {
                      if ( m_pKivNevek[uiKivNevIdx] == pNvzLstSor->uiNemz )
                      {
                        break ;
                      }
                  
                      uiKivNevIdx++ ;
                    }
                  }
                  else
                  {
                    // Itt csak m_enNevtipus == eMotorNt nak szabadna elofordulnia
                    while ( uiKivNevIdx < m_nNevekSzama )
                    {
                      if ( m_pKivNevek[uiKivNevIdx] == pNvzLstSor->uiMotor )
                      {
                        break ;
                      }
                  
                      uiKivNevIdx++ ;
                    }
                  }

                  // Ez a nvzl kulcsban szereplo eNemzNt/eMotorNt nincs kivalasztva
                  // Megy tovabb az eredmenyeken, ami onmagaban nem baj,
                  // mert 1 szez er.-hez tobb futam er. is tartozhat, mas nvzl klcssal
                  if ( uiKivNevIdx == m_nNevekSzama )
                  {
                    continue ;
                  }
                }

                pVBSzerepl = new CVBSzerepl() ;

                if ( pVBSzerepl != NULL )
                {
                  pVBSzerepl->Feltolt( pNvzLstSor->uiVersenyzo,
                                       pNvzLstSor->uiNemz, pNvzLstSor->uiCsapat, pNvzLstSor->uiMotor,
                                       pSzezTomb[i],
                                       sPillAllLekEr.pSzezPntTmb[nNevekIdx], sPillAllLekEr.pNevHlyzTmb[nNevekIdx],
                                       m_enNevtipus) ;

                  VBSzerHozzaad( pVBSzerepl) ;

                  // Kulonben egy nevet ha a szezonban tobb nvzl klcs-csal
                  // szerepelt, tobbszor adna hozza !
                  break ;
                }
              }

              break ;
            }
          }
        }
      }
    } // SQL_PillAll : OK

    i++ ;
    sPillAllLekEr.Felszabadit() ;
  } // while - szezonok ciklusa

  nTmbMeret = 0 ;
  if ( m_pVBSzerepl != NULL )
  {
    while ( m_pVBSzerepl[nTmbMeret] != NULL )
    {
//m_pVBSzerepl[nTmbMeret]->Trace() ;
      nTmbMeret++ ;
    }
  }

  // A VB Szereples eredmenyeinek sorbarendezese
  qsort( (void*)m_pVBSzerepl, nTmbMeret, sizeof(CVBSzerepl*), VBOsszehas) ;

  // A kivalasztott neveket tartalmazo tomb felszabaditasa
  // Ugyan minden lekerdezes elott megprobalja, de ha nincs uj lekerdezes: mem.leak
  Felszabadit( sPillAllLekEr, &m_pKivNevek, &m_nNevekSzama) ;

  if ( nSzezdb > 0 )
  {
    nSzezdb = 0 ;

    delete [] pSzezTomb ;
    pSzezTomb = NULL ;
  }
}

void CLekerdDlg::AltLekerd()
{
  CProba13App*   pPro13App = (CProba13App*)AfxGetApp() ;

  CDocument*     pBiztonsagi  ;

  CFgyEeLgykOe*  pFgyEeLgykOe ;

  CString        sDocNev      ;

  sRsBefLekErTip*   pRsBefLekEr   ;
  sLgGyKorLekErTip* pLgGyKorLekEr ;
  unsigned          nKiTmbMeret   ;

  unsigned uiIdx ;

  long nRC ;

  sDocNev = "CLekEredm1Doc" ;

  switch ( m_enLekTip )
  {
    case eFutgy  :
    case eEdzEls :

      nRC = SQL_RajtsBefuto( m_enLekTip, m_enNevtipus,
                             m_uiSzezontol, m_pKivNevek, m_nNevekSzama,
                             &pRsBefLekEr, &nKiTmbMeret) ;
/*
for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
{
  TRACE("\nR:%d, V:%d, N:%d, C:%d, M:%d, S:%d, N:%d, H:%d, O:%d, P:%d, M:%d, E:%d",
         (pRsBefLekEr+uiIdx)->uiRajtszam,
         (pRsBefLekEr+uiIdx)->uiVersenyzo,
         (pRsBefLekEr+uiIdx)->uiNemzetiseg,
         (pRsBefLekEr+uiIdx)->uiCsapat,
         (pRsBefLekEr+uiIdx)->uiMotor,
         (pRsBefLekEr+uiIdx)->uiSzezonEve,
         (pRsBefLekEr+uiIdx)->uiNagydijNev,
         (pRsBefLekEr+uiIdx)->uiHelyezes,
         (pRsBefLekEr+uiIdx)->uiOra,
         (pRsBefLekEr+uiIdx)->uiPerc,
         (pRsBefLekEr+uiIdx)->uiMasodperc,
         (pRsBefLekEr+uiIdx)->uiEzrdmsdperc
      ) ;
}
*/
      if ( nRC == 0 )
      {
        // Hogy csak az elso iteracios lepesben tortenjen dok letrehozas
        if ( m_pCLekEr1Doc == NULL )
        {
          pBiztonsagi = pPro13App->DocViewFrameLetr(sDocNev) ;

          if ( pBiztonsagi == NULL )
          {
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pRsBefLekEr ;

            return ;
          }

          // Ez is csak akkor kell, ha eppen letrehoztuk a triumviratust
          if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
          {
            m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;
            // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
            m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
            m_pCLekEr1Doc->DokuNevKiir() ;
          }
          else
          {
            delete pBiztonsagi ;
      
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pRsBefLekEr ;

            return ;
          }
        }

        for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
        {
          pFgyEeLgykOe = new CFgyEeLgykOe() ;

          if ( pFgyEeLgykOe != NULL )
          {              
            pFgyEeLgykOe->Feltolt( pRsBefLekEr+uiIdx, m_enNevtipus) ;
            m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( pFgyEeLgykOe) ;
          }
        }

        delete [] pRsBefLekEr ;

      }
      else
      {
        // Lehet, hogy nem az elso memoriafoglalas futott hibara
        if ( nKiTmbMeret > 0 )
        {
          delete [] pRsBefLekEr ;
        }

        HibaKiir( nRC) ;
      }

      // A kivalasztott neveket tartalmazo tomb nem kell mar
      delete [] m_pKivNevek ;
      m_pKivNevek   = NULL  ;
      m_nNevekSzama = 0     ;

      break ;

    case eLeggyKor :

      nRC = SQL_LeggyKor( m_enLekTip, m_enNevtipus,
                          m_uiSzezontol, m_pKivNevek, m_nNevekSzama,
                          &pLgGyKorLekEr, &nKiTmbMeret) ;

      if ( nRC == 0 )
      {
        // Hogy csak az elso iteracios lepesben tortenjen dok letrehozas
        if ( m_pCLekEr1Doc == NULL )
        {
          pBiztonsagi = pPro13App->DocViewFrameLetr(sDocNev) ;

          if ( pBiztonsagi == NULL )
          {
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pLgGyKorLekEr ;

            return ;
          }

          // Ez is csak akkor kell, ha eppen letrehoztuk a triumviratust
          if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
          {
            m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;
            // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
            m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
            m_pCLekEr1Doc->DokuNevKiir() ;
          }
          else
          {
            delete pBiztonsagi ;
      
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pLgGyKorLekEr ;

            return ;
          }
        }

        for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
        {
          pFgyEeLgykOe = new CFgyEeLgykOe() ;

          if ( pFgyEeLgykOe != NULL )
          {              
            pFgyEeLgykOe->Feltolt( pLgGyKorLekEr+uiIdx, m_enNevtipus) ;
            m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( pFgyEeLgykOe) ;
          }
        }

        delete [] pLgGyKorLekEr ;

      }
      else
      {
        // Lehet, hogy nem az elso memoriafoglalas futott hibara
        if ( nKiTmbMeret > 0 )
        {
          delete [] pLgGyKorLekEr ;
        }

        HibaKiir( nRC) ;
      }

      // A kivalasztott neveket tartalmazo tomb nem kell mar
      delete [] m_pKivNevek ;
      m_pKivNevek   = NULL  ;
      m_nNevekSzama = 0     ;

      break ;

    case eOsszEr : // == eFutgy + eEdzEls + eLeggyKor
      // A futamgyozelmekre vonatkozo lekerdezes
      nRC = SQL_RajtsBefuto( eFutgy, m_enNevtipus,
                             m_uiSzezontol, m_pKivNevek, m_nNevekSzama,
                             &pRsBefLekEr, &nKiTmbMeret) ;

      if ( nRC == 0 )
      {
        // Hogy csak az elso iteracios lepesben tortenjen dok letrehozas
        if ( m_pCLekEr1Doc == NULL )
        {
          pBiztonsagi = pPro13App->DocViewFrameLetr(sDocNev) ;

          if ( pBiztonsagi == NULL )
          {
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pRsBefLekEr ;

            return ;
          }

          // Ez is csak akkor kell, ha eppen letrehoztuk a triumviratust
          if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
          {
            m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;
            // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
            m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
            m_pCLekEr1Doc->DokuNevKiir() ;
          }
          else
          {
            delete pBiztonsagi ;
      
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pRsBefLekEr ;

            return ;
          }
        }

        for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
        {
          pFgyEeLgykOe = new CFgyEeLgykOe() ;

          if ( pFgyEeLgykOe != NULL )
          {              
            pFgyEeLgykOe->Feltolt( pRsBefLekEr+uiIdx, m_enNevtipus) ;
            m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( pFgyEeLgykOe) ;
          }
        }

        delete [] pRsBefLekEr ;

      }
      else
      {
        // Lehet, hogy nem az elso memoriafoglalas futott hibara
        if ( nKiTmbMeret > 0 )
        {
          delete [] pRsBefLekEr ;
        }

        HibaKiir( nRC) ;
      }

      // A kivalasztott neveket tartalmazo tomb kell meg !
/*
      delete [] m_pKivNevek ;
      m_pKivNevek   = NULL  ;
      m_nNevekSzama = 0     ;
*/
      // Az edzeselsosegekre vonatkozo lekerdezes
      nRC = SQL_RajtsBefuto( eEdzEls, m_enNevtipus,
                             m_uiSzezontol, m_pKivNevek, m_nNevekSzama,
                             &pRsBefLekEr, &nKiTmbMeret) ;

      if ( nRC == 0 )
      {
        // Hogy csak az elso iteracios lepesben tortenjen dok letrehozas
        if ( m_pCLekEr1Doc == NULL )
        {
          pBiztonsagi = pPro13App->DocViewFrameLetr(sDocNev) ;

          if ( pBiztonsagi == NULL )
          {
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pRsBefLekEr ;

            return ;
          }

          // Ez is csak akkor kell, ha eppen letrehoztuk a triumviratust
          if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
          {
            m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;
            // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
            m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
            m_pCLekEr1Doc->DokuNevKiir() ;
          }
          else
          {
            delete pBiztonsagi ;
      
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pRsBefLekEr ;

            return ;
          }
        }

        for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
        {
          pFgyEeLgykOe = new CFgyEeLgykOe() ;

          if ( pFgyEeLgykOe != NULL )
          {              
            pFgyEeLgykOe->Feltolt( pRsBefLekEr+uiIdx, m_enNevtipus) ;
            m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( pFgyEeLgykOe) ;
          }
        }

        delete [] pRsBefLekEr ;

      }
      else
      {
        // Lehet, hogy nem az elso memoriafoglalas futott hibara
        if ( nKiTmbMeret > 0 )
        {
          delete [] pRsBefLekEr ;
        }

        HibaKiir( nRC) ;
      }

      // A leggyorsabb korokre vonatkozo lekerdezes
      nRC = SQL_LeggyKor( eLeggyKor, m_enNevtipus,
                          m_uiSzezontol, m_pKivNevek, m_nNevekSzama,
                          &pLgGyKorLekEr, &nKiTmbMeret) ;

      if ( nRC == 0 )
      {
        // Hogy csak az elso iteracios lepesben tortenjen dok letrehozas
        if ( m_pCLekEr1Doc == NULL )
        {
          pBiztonsagi = pPro13App->DocViewFrameLetr(sDocNev) ;

          if ( pBiztonsagi == NULL )
          {
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pLgGyKorLekEr ;

            return ;
          }

          // Ez is csak akkor kell, ha eppen letrehoztuk a triumviratust
          if ( pBiztonsagi->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
          {
            m_pCLekEr1Doc = (CLekEredm1Doc*) pBiztonsagi ;
            // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint   m_usSzezonig
            m_pCLekEr1Doc->TipSzezBeall( m_enNevtipus, m_enLekTip, m_uiSzezontol, m_uiSzezontol) ;
            m_pCLekEr1Doc->DokuNevKiir() ;
          }
          else
          {
            delete pBiztonsagi ;
      
            // A kivalasztott neveket tartalmazo tomb nem kell mar
            delete [] m_pKivNevek ;
            m_pKivNevek   = NULL  ;
            m_nNevekSzama = 0     ;

            // Az SQL-fv altal visszaadott tomb sem kell mar
            delete [] pLgGyKorLekEr ;

            return ;
          }
        }

        for ( uiIdx = 0 ; uiIdx < nKiTmbMeret ; uiIdx++ )
        {
          pFgyEeLgykOe = new CFgyEeLgykOe() ;

          if ( pFgyEeLgykOe != NULL )
          {              
            pFgyEeLgykOe->Feltolt( pLgGyKorLekEr+uiIdx, m_enNevtipus) ;
            m_pCLekEr1Doc->m_cLekErMegjVaz.UjFelvesz( pFgyEeLgykOe) ;
          }
        }

        delete [] pLgGyKorLekEr ;

      }
      else
      {
        // Lehet, hogy nem az elso memoriafoglalas futott hibara
        if ( nKiTmbMeret > 0 )
        {
          delete [] pLgGyKorLekEr ;
        }

        HibaKiir( nRC) ;
      }

      // A kivalasztott neveket tartalmazo tomb nem kell mar
      delete [] m_pKivNevek ;
      m_pKivNevek   = NULL  ;
      m_nNevekSzama = 0     ;

      break ;

    default :
      return ;
  }
}

int CLekerdDlg::NevKezdSzezBeo(unsigned& usSzezontol)
{
  CListBox * pNevLst  = (CListBox* ) GetDlgItem(IDC_LEK1_NEVEK_LB) ;
  CComboBox* pSzezCb  = (CComboBox*) GetDlgItem(IDC_LEK1_SZEZ_CB)  ;

  int*      pnKivNevIdx ;
  unsigned  i ;
  int       nKivElem ;

  pnKivNevIdx = NULL ;
  m_nNevekSzama = pNevLst->GetSelCount() ;

//  m_nNevekSzama = 18 ;

  // Lehet, hogy a keresest tobbszor akarom majd indithatni ugyanabbol az
  // ablakbol
  if ( m_pKivNevek != NULL )
  {
    delete [] m_pKivNevek ;
    m_pKivNevek   = NULL  ;
    m_nNevekSzama = 0     ;
  }

  if ( m_nNevekSzama != 0 && m_nNevekSzama != LB_ERR )
  {
    pnKivNevIdx = new int [m_nNevekSzama] ;

    // Ha sikerult  a foglalas
    if ( pnKivNevIdx != NULL )
    {
/*
      m_pKivNevek  = new unsigned[m_nNevekSzama] ;

   Az 1990-es szezon versenyzonev kulcsai.
   A vegeredmeny forditott sorrendjeben.

      m_pKivNevek[17] = 13 ;
      m_pKivNevek[16] = 14 ;
      m_pKivNevek[15] =  9 ;
      m_pKivNevek[14] = 11 ;
      m_pKivNevek[13] = 12 ;
      m_pKivNevek[12] = 31 ;
      m_pKivNevek[11] = 15 ;
      m_pKivNevek[10] = 64 ;
      m_pKivNevek[9] = 10 ;
      m_pKivNevek[8] =  1 ;
      m_pKivNevek[7] =  8 ;
      m_pKivNevek[6] = 25 ;
      m_pKivNevek[5] = 24 ;
      m_pKivNevek[4] = 51 ;
      m_pKivNevek[3] = 29 ;
      m_pKivNevek[2] =  5 ;
      m_pKivNevek[1] = 37 ;
      m_pKivNevek[0] =  7 ;
*/
      // Sikerult az indexek visszaadasa

      if ( pNevLst->GetSelItems(m_nNevekSzama, pnKivNevIdx) != LB_ERR )
      {
        m_pKivNevek  = new unsigned[m_nNevekSzama] ;

        if ( m_pKivNevek != NULL )
        {
          for ( i=0 ; i < m_nNevekSzama ; i++ )
          {
            m_pKivNevek[i] = unsigned(pNevLst->GetItemData(pnKivNevIdx[i])) ;
          }
        }
      }
    }
  }

  // Az indextomb nem kell mar
  if ( pnKivNevIdx != NULL )
    delete [] pnKivNevIdx ;

  // A muvelet meghiusult : rc (nKivNevekSzama) = 0
  if ( m_nNevekSzama == LB_ERR || m_pKivNevek == NULL )
  {
    // Indikalja, hogy valamilyen hiba miatt (^) nem jutott el legbelulre
    m_nNevekSzama = 0 ;
  }

  nKivElem = pSzezCb->GetCurSel() ;
  if ( nKivElem != CB_ERR )
  {
    m_uiSzezontol = unsigned(pSzezCb->GetItemData(nKivElem)) ;
  }
  else
  {
    m_uiSzezontol = 0 ;
  }

  return m_nNevekSzama ;
}

BOOL CLekerdDlg::NvzLstBetolt(unsigned uiBeoSzez)
{
  long nRC ;
/*
  if ( m_cAktNvzLst.m_uiSzezon != m_uiSzezontol )
  {
*/
    m_cAktNvzLst.RemoveAll() ;

    // A m_uiSzezon tagvaltozot allitja
    nRC = m_cAktNvzLst.SQL_NvzlstBetolt( uiBeoSzez/*m_uiSzezontol*/) ;
    if ( nRC != 0 )
    {
      HibaKiir( nRC) ;

      return FALSE ;
    }
/*
  }
*/
  // Minden rendben
  return TRUE ;
}

BOOL CLekerdDlg::SzezBetolt(unsigned uiBeoSzez)
{
  long nRC ;

  m_cAktSzez.Kiurit() ;

  nRC = m_cAktSzez.SQL_SzezonBetolt( uiBeoSzez) ;

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;

    return FALSE ;
  }

  // Minden rendben
  return TRUE ;
}

BOOL CLekerdDlg::RsBefBetolt(unsigned uiSzezon, unsigned uiNagydij,
                             eAblTipRsBef enRajtsBef)
{
  long nRC ;

  // Regi torol, majd uj betolt            
  m_cAktRsBef.Kiurit() ;
  m_cAktRsBef.m_uiSzezEve    = uiSzezon  ;
  m_cAktRsBef.m_uiNagydKulcs = uiNagydij ;

  nRC = m_cAktRsBef.SQL_RajtsBefBetolt( enRajtsBef) ;

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;

    return FALSE ;
  }

  return TRUE ;
}

BOOL CLekerdDlg::NevKulcsKeres( CPillAll** pCPillAllTmb, CNvzSor* pNvzLstSor,
                                unsigned short& usTombPoz )
{
  // Eleg a NULL-ra vizsgalni, mert vegig ki van nullazva - es ha teli van ?
  // Kizart, mert mindig egyel tobb helyet foglalok, mint a nvzl merete

  usTombPoz = 0 ;
  while ( pCPillAllTmb[usTombPoz] != NULL )
  {
    switch ( m_enNevtipus )
    {
      case eVers  :
        if ( pNvzLstSor->uiVersenyzo == pCPillAllTmb[usTombPoz]->m_cVersMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      case eNemz  :
        if ( pNvzLstSor->uiNemz == pCPillAllTmb[usTombPoz]->m_cNemzMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      case eCsap  :
        if ( pNvzLstSor->uiCsapat == pCPillAllTmb[usTombPoz]->m_cCsapMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      case eMotor :
        if ( pNvzLstSor->uiMotor == pCPillAllTmb[usTombPoz]->m_cMotorMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      default : // Minden egyeb (pl. a nagydijnev is) hiba !
                  ;
    } // switch : nevtipus

    usTombPoz++ ;
  } // while : melyik tombelemhez tartozik

  return FALSE ;
}

BOOL CLekerdDlg::NevKulcsKeres( CPillAll** pCPillAllTmb, unsigned uiNevKlcs,
                                unsigned short& usTombPoz )
{
  unsigned uiDebug = 0 ;
  // Eleg a NULL-ra vizsgalni, mert vegig ki van nullazva - es ha teli van ?
  // Kizart, mert mindig egyel tobb helyet foglalok, mint a nvzl merete

//  usTombPoz = 0 ; hogy ne mindig az elejerol kezdje !!!
  while ( pCPillAllTmb[usTombPoz] != NULL )
  {
    switch ( m_enNevtipus )
    {
      case eVers :
        if ( uiNevKlcs == pCPillAllTmb[usTombPoz]->m_cVersMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      case eNemz:
        uiDebug = pCPillAllTmb[usTombPoz]->m_cNemzMez.m_uiKulcs1 ;
        
        if ( uiNevKlcs == uiDebug )
        {
//TRACE("\nEgyezes : %d, usTombPoz: %d", uiNevKlcs, usTombPoz) ;
          return TRUE ;
        }

        break ;

      case eCsap     :
        if ( uiNevKlcs == pCPillAllTmb[usTombPoz]->m_cCsapMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      case eMotor    :
        if ( uiNevKlcs == pCPillAllTmb[usTombPoz]->m_cMotorMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      default : // Minden egyeb (pl. a nagydijnev is) hiba !
                  ;
    } // switch : nevtipus

    usTombPoz++ ;
  } // while : melyik tombelemhez tartozik

  return FALSE ;
}

BOOL CLekerdDlg::VersCsapKeres( CPillAll** pCPillAllTmb, CNvzSor* pNvzLstSor,
                                unsigned short& usTombPoz )
{
  usTombPoz = 0 ;
  while ( pCPillAllTmb[usTombPoz] != NULL )
  {
    switch ( m_enNevtipus )
    {// Ezen esetekben a szezon pillanatnyi allasat a versenyzo nevek alapjan 6+.
     // 
      case eVers  :
      case eNemz  :
      case eMotor :
        if ( pNvzLstSor->uiVersenyzo == pCPillAllTmb[usTombPoz]->m_cVersMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      case eCsap  :
        if ( pNvzLstSor->uiCsapat == pCPillAllTmb[usTombPoz]->m_cCsapMez.m_uiKulcs1 )
          return TRUE ;

        break ;

      default : // Minden egyeb (pl. a nagydijnev is) hiba !
                  ;
    } // switch : nevtipus

    usTombPoz++ ;
  } // while : melyik tombelemhez tartozik

  return FALSE ;
}

BOOL CLekerdDlg::VBSzerHozzaad(CVBSzerepl* pUjElem)
{
  BOOL bMegvan = FALSE ;
  unsigned short usTombPoz ;
  CVBSzerepl* pIdglVBSz = NULL ;

  if ( pUjElem != NULL && pUjElem->IsKindOf(RUNTIME_CLASS(CVBSzerepl)) )
  {

//pUjElem->Trace() ;

    // Ha nincs meg hely foglalva
    if ( m_pVBSzerepl == NULL )
    {                                          // +1 l. lent
      m_pVBSzerepl = new CVBSzerepl* [m_nNevekSzama+1] ;

      if ( m_pVBSzerepl == NULL )
        return FALSE ;

      // Kinullazzuk : kesobb epit erre a korulmenyre
      memset( m_pVBSzerepl, 0, sizeof(CVBSzerepl*)*(m_nNevekSzama+1) ) ;
    }

    // Eleg a NULL-ra vizsgalni, mert vegig ki van nullazva - es ha teli van ?
    // Kizart, mert mindig egyel tobb helyet foglal, mint a kiv nevek merete
    usTombPoz = 0 ;
    while ( m_pVBSzerepl[usTombPoz] != NULL && bMegvan == FALSE )
    {
      // Kihasznalom, hogy a lek. tipusa megegyezik a kezdoszinttel
      switch ( m_enNevtipus )
      {
        case eVers  :
          if ( pUjElem->m_cVersMez.m_uiKulcs1 == m_pVBSzerepl[usTombPoz]->m_cVersMez.m_uiKulcs1 )
            bMegvan = TRUE ;

          break ;

        case eNemz  :
          if ( pUjElem->m_cNemzMez.m_uiKulcs1 == m_pVBSzerepl[usTombPoz]->m_cNemzMez.m_uiKulcs1 )
            bMegvan = TRUE ;

          break ;

        case eCsap  :
          if ( pUjElem->m_cCsapMez.m_uiKulcs1 == m_pVBSzerepl[usTombPoz]->m_cCsapMez.m_uiKulcs1 )
            bMegvan = TRUE ;

          break ;

        case eMotor :
          if ( pUjElem->m_cMotorMez.m_uiKulcs1 == m_pVBSzerepl[usTombPoz]->m_cMotorMez.m_uiKulcs1 )
            bMegvan = TRUE ;

          break ;

        default : // Minden egyeb (pl. a nagydijnev is) hiba !
                  ;
      } // switch : nevtipus

      usTombPoz++ ;
    } // while : melyik tombelemhez tartozik

    if ( bMegvan == TRUE )
    {
      usTombPoz-- ;
      // Kell-e helyet cserelni ?
      if ( pUjElem->m_cHelyezMez.SajatHelyz() <
           m_pVBSzerepl[usTombPoz]->m_cHelyezMez.SajatHelyz() )
      {// Helycsere : az ujonnan felveendo 'van elobbre' !
        pIdglVBSz = m_pVBSzerepl[usTombPoz] ;

        // Ami az gyokerelemnel kov.sz., az az uj elembe felvetel utan nem az
        if ( pIdglVBSz->m_pKovSzKovElem )
        {
          switch ( pIdglVBSz->m_pKovSzKovElem->AzMezok( SZ_KEZDOSZINT+1) )
          {
            case NEM_TART_HOZZA :
              // pIdglVBSz->m_pAzSzKovElem != NULL esetre nem szabad a fv.t felhivni
              pIdglVBSz->m_pAzSzKovElem  = pIdglVBSz->m_pKovSzKovElem ;
              pIdglVBSz->m_pKovSzKovElem = NULL ;
              break ;
            case HOZZA_TART     :
              //                          \/ ha NULL az sem baj
              pIdglVBSz->m_pAzSzKovElem = pIdglVBSz->m_pKovSzKovElem->m_pAzSzKovElem ;
              pIdglVBSz->m_pKovSzKovElem->m_pAzSzKovElem = NULL ;
              break ;
            default :
              ;
          }
        }

        m_pVBSzerepl[usTombPoz] = pUjElem ;
        pUjElem->UjFelvesz( pIdglVBSz) ;
      }
      else
      {// Ha nem kell helyet cserelni, az uj elemet azert hozza kell adni !
        m_pVBSzerepl[usTombPoz]->UjFelvesz( pUjElem) ;
      }
    }
    else
    {
      m_pVBSzerepl[usTombPoz] = pUjElem ;
    }

    return TRUE ;
  } // if ( pUjElem != NULL...

  return FALSE ;
}

BOOL CLekerdDlg::KivNev( CPillAll* pCPillAll)
{
  unsigned i ;

  if ( pCPillAll == NULL )
    return FALSE ;

  for ( i = 0 ; i < m_nNevekSzama ; i++ )
  {
    switch ( m_enNevtipus )
    {
      case eVers  :
        if ( pCPillAll->m_cVersMez.m_uiKulcs1 == m_pKivNevek[i] )
          return TRUE ;
        break ;

      case eNemz  :
        if ( pCPillAll->m_cNemzMez.m_uiKulcs1 == m_pKivNevek[i] )
          return TRUE ;
        break ;

      case eCsap  :
        if ( pCPillAll->m_cCsapMez.m_uiKulcs1 == m_pKivNevek[i] )
          return TRUE ;
        break ;

      case eMotor :
        if ( pCPillAll->m_cMotorMez.m_uiKulcs1 == m_pKivNevek[i] )
          return TRUE ;
        break ;

      default : // Minden egyeb (pl. a nagydijnev is) hiba !
        return FALSE ;
    } // switch : nevtipus
  }

  return FALSE ;
}

void CLekerdDlg::Felszabadit( sPillAllLekErTip sPillAllLekEr,
                              unsigned** ppKivNevek, unsigned* pNevekSzama)
{
  sPillAllLekEr.Felszabadit() ;

  // A kivalasztott neveket tartalmazo tomb felszabaditasa
  // Ugyan minden lekerdezes elott megprobalja, de ha nincs uj lekerdezes: mem.leak
  delete [] *ppKivNevek ;
  *ppKivNevek  = NULL  ;
  *pNevekSzama = 0     ;
}

void CLekerdDlg::HibaKiir(long nRC)
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
