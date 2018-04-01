// szezfelv.cpp : implementation file
//

#include "stdafx.h"

#include "resource.h"

#include <stdlib.h>
#include <ctype.h>

#include "alaptip.h"

#include "ensqlcam.h"
#include "dbglobal.h"

#include "pontozas.h"
#include "cnev.h"
#include "cvnev.h"

#include "cszezon.h"
#include "proba13.h"

#include "szezfelv.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSzezFelv dialog

CSzezFelv::CSzezFelv( CWnd* pParent/*=NULL*/):CDialog(CSzezFelv::IDD, pParent)
{
  DWORD dwDlgBasUnit ;

  // GLOBALIS ADATOK :

  // Ezeket a mutatokat nem konstruktorban adom at, mivel fixnek tekinthetok :
  // nem kell oket visszairni OK-val valo kiszallasnal
  m_pPontozas = &((CProba13App*)AfxGetApp())->m_cPontozas ;
  m_pNagyd    = &( ( (CProba13App*)AfxGetApp() )->m_cNagydijNev ) ;
  m_pPalya    = &( ( (CProba13App*)AfxGetApp() )->m_cPalyaNev )   ;

  // LOKALIS ADATOK :
  m_bAktSzezonMentve = TRUE  ;

  dwDlgBasUnit = GetDialogBaseUnits() ;
/*
     int i ;
     for (i=1 ; i<3 ; i++) // lehet, hogy nem ciklus kell:nem nlotav+ csak ket elem
       m_nFutTbStp[i-1] = (int) i*LOWORD(dwDlgBasUnit)*11 ;
*/
  m_nFutTbStp[0] = 4 * 220 / LOWORD(dwDlgBasUnit) ;
  m_nFutTbStp[1] = 4 * 440 / LOWORD(dwDlgBasUnit) ;

  //{{AFX_DATA_INIT(CSzezFelv)
    // NOTE: the ClassWizard will add member initialization here
  //}}AFX_DATA_INIT
}

// Destruktor tesztcelokra
CSzezFelv::~CSzezFelv()
{

}

// A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
// DoDataExchange fuggvenybol
BOOL CSzezFelv::OnInitDialog()
{
  CComboBox* pSzezCb  = (CComboBox*) GetDlgItem(IDC_SZEZ_CB)   ;
  CComboBox* pPontCb  = (CComboBox*) GetDlgItem(IDC_PNT_CB)    ;
  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;
  CComboBox* pNagydCb = (CComboBox*) GetDlgItem(IDC_NAGYD_CB)  ;
  CComboBox* pPalyaCb = (CComboBox*) GetDlgItem(IDC_PLYA_CB)   ;

  CEdit* pPntEdzEEd      = (CEdit*) GetDlgItem(IDC_PNT_EDZE_E)    ;
  CEdit* pPntLgyKorEd    = (CEdit*) GetDlgItem(IDC_PNT_LGYKOR_E)  ;
  CEdit* pPntElsLgjBszEd = (CEdit*) GetDlgItem(IDC_ELS_LGJ_BESZ_E);

  unsigned  nSzezdb ;
  unsigned* pFlvSzezTomb ;

  CString   sHibaSzov, sSzov ;
  long      nRC ;

  unsigned  i ;
  int       nPntLBBeszPoz ;

  CString   sEgySor ;
  char      str[5] ;

  POSITION  poz ;
  unsigned  uiKulcs ;
  nevadat*  pNevAdat ;
  pontadat* pPontAdat ;

  CDialog::OnInitDialog() ;

  // Maga az inicializalas :
  //-------------------------------------------------------------------------
  // A dialogus ablak feliratainak beallitasa
  if ( sSzov.LoadString(IDS_SZEZFELV_S)  == FALSE )
    sSzov = "IDS_SZEZFELV_S" ;

  SetWindowText( sSzov) ;

  nSzezdb = 0 ;
  pFlvSzezTomb = NULL ;
  
  // A felvitt szezonok evszamait tartalmazo Cb feltoltese
  if ( (nRC = SQL_FelvSzez( &pFlvSzezTomb, &nSzezdb)) != 0 )
  {
    HibaKiir( nRC) ;
  }

  i = 0 ;
  while ( i < nSzezdb )
  {
    sprintf( str, "%d", pFlvSzezTomb[i]) ;
    pSzezCb->SetItemData(pSzezCb->AddString(str), (DWORD) pFlvSzezTomb[i]) ;
    i++ ;
  }

  delete [] pFlvSzezTomb ;

  //--------------------------------------------------------------------------
  // A pontozas lista feltoltese
  // A lista ablak feltoltese :

  poz = m_pPontozas->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pPontozas->GetNextAssoc( poz, uiKulcs, pPontAdat) ;

    sEgySor = "" ;

    // A COMBO BOX-NAL NEM ADHATO MEG TABSTOP
    PontbolStr( pPontAdat, sEgySor, CString("     ")) ;

    nPntLBBeszPoz = pPontCb->AddString( sEgySor)  ;
    pPontCb->SetItemData( nPntLBBeszPoz, uiKulcs) ;
  }

  //--------------------------------------------------------------------------
  // Az edzeselsosegert, leggyorsabb korert jaro pontok, a max ertekelt befuto
  // Mivel a SetWindowText automatikusan maga utan vonja OnChgPntEdzE (stb.)
  // hivasat, ami a szezon ervenytelenseget eredmenyezi, ezert:
  m_bInicializalas = TRUE ;
  pPntEdzEEd->SetWindowText( "0") ;
  pPntLgyKorEd->SetWindowText( "0") ;
  pPntElsLgjBszEd->SetWindowText( "0") ;
  m_bInicializalas = FALSE ;
  //--------------------------------------------------------------------------
	// A futamokat tartalmazo Lb tabstopjainak beallitasa
/*
	pFutLb->SetTabStops(2, m_nFutTbStp) ;
*/

  pFutLb->SendMessage(LB_SETTABSTOPS, 2, (LONG)(LPSTR)m_nFutTbStp) ;

  //--------------------------------------------------------------------------
  // A nagydij neveket tartalmazo Cb feltoltese
  poz = m_pNagyd->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pNagyd->GetNextAssoc( poz, uiKulcs, pNevAdat ) ;
    pNagydCb->SetItemData( pNagydCb->AddString( pNevAdat->m_sNev), uiKulcs) ;
  }

  // A lista ablak elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pNagydCb->SetCurSel(0) ;

  //--------------------------------------------------------------------------
  // A palya neveket tartalmazo Cb feltoltese
  poz = m_pPalya->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pPalya->GetNextAssoc( poz, uiKulcs, pNevAdat ) ;
    pPalyaCb->SetItemData( pPalyaCb->AddString( pNevAdat->m_sNev), uiKulcs) ;
  }

  // A combo box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pPalyaCb->SetCurSel(0) ;

  return TRUE ;
}

BEGIN_MESSAGE_MAP(CSzezFelv, CDialog)
  //{{AFX_MSG_MAP(CSzezFelv)
  ON_CBN_EDITCHANGE(IDC_SZEZ_CB, OnEdchgSzezCb)
  ON_CBN_SELCHANGE(IDC_SZEZ_CB, OnSelchgSzez)
  ON_LBN_DBLCLK(IDC_FTMK_LIST, OnDblclkFutamList)
  ON_BN_CLICKED(IDC_FELV_DB, OnFelv)
  ON_BN_CLICKED(IDC_MOD_B, OnMod)
  ON_BN_CLICKED(IDC_TRL_B, OnTorol)
  ON_CBN_SELCHANGE(IDC_PNT_CB, OnSelchgPont)
  ON_BN_CLICKED(IDC_SZFLV_B, OnSzezFelv)
  ON_BN_CLICKED(IDC_SZTRL_B, OnSzezTorol)
  ON_EN_CHANGE(IDC_PNT_EDZE_E, OnChgPntEdzE)
  ON_EN_CHANGE(IDC_PNT_LGYKOR_E, OnChgPntLgyKor)
  ON_EN_CHANGE(IDC_ELS_LGJ_BESZ_E, OnChgElsLgjBesz)
  //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSzezFelv message handlers

// A fuggveny igyekszik az m_cAktSzezon.m_usSzezon erteket aktualisan tartani,
// hogy az OnSelchgSzez fv. nel tudjuk melyik szezont kell esetleg menteni

void CSzezFelv::OnEdchgSzezCb()
{
  CComboBox* pSzezCb  = (CComboBox*) GetDlgItem(IDC_SZEZ_CB)   ;
  CComboBox* pPontCb  = (CComboBox*) GetDlgItem(IDC_PNT_CB)    ;

  CEdit* pPntEdzEEd      = (CEdit*) GetDlgItem(IDC_PNT_EDZE_E)    ;
  CEdit* pPntLgyKorEd    = (CEdit*) GetDlgItem(IDC_PNT_LGYKOR_E)  ;
  CEdit* pPntElsLgjBszEd = (CEdit*) GetDlgItem(IDC_ELS_LGJ_BESZ_E);

  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;

  CComboBox* pNagydCb = (CComboBox*) GetDlgItem(IDC_NAGYD_CB)  ;
  CComboBox* pPalyaCb = (CComboBox*) GetDlgItem(IDC_PLYA_CB)   ;
  CEdit* pDatEd       = (CEdit*) GetDlgItem(IDC_DAT_E)         ;

  CString sEvszam, sAblCim, sSzov ;
  char pszSegedStr[30] ;
  int nEvszam, nItemData ;
  long nRC ;
  unsigned char i = 0 ;

  // A "Szezon" szerkeszto ablak tartalmat hasznalja mentesnel,
  // ezt irja a m_cAktSzezon-ba ellenorzes (szintakszis, megvan-e...)
  pSzezCb->GetWindowText(sEvszam) ;

  // Mukodik : pl. "_123"->"" ; "1a23"->"1"
  sEvszam = sEvszam.SpanIncluding("0123456789") ;

  if ( sEvszam.GetLength() == 4 )
  {
    // A beirt sztring negyjegyu; lehet ervenyes evszam
    nEvszam = atoi( sEvszam ) ;

    // Ha a beirt szam kisebb, mint, ELSO_F1_SZEZ(1950), hibas a szezon evmegadas
    if ( nEvszam >= ELSO_F1_SZEZ )
    {
      if ( m_bAktSzezonMentve == FALSE )
      {
        // Mivel az aktualis tartalom nem volt mentve
        GetWindowText( sAblCim) ;

        if ( sSzov.LoadString(IDS_SZEZ_MEGV) == FALSE )
          sSzov = "IDS_SZEZ_MEGV" ;

        sprintf( pszSegedStr, "\n   %d", m_cAktSzezon.m_uiSzezon) ;

        if ( IDYES == MessageBox( sSzov+pszSegedStr, sAblCim, MB_ICONQUESTION|MB_YESNO) )
        {
          nRC = SQL_COMMIT() ;
          if ( nRC != 0 )
          {
            HibaKiir( nRC) ;

            return ;
          }
        }
        else
        {
          nRC = SQL_ROLLBACK() ;
          if ( nRC != 0 )
          {
            HibaKiir( nRC) ;

            return ;
          }
        }

        m_bAktSzezonMentve = TRUE ;
      }

      // Az ujonnan beirt, ervenyes evszam fel van-e mar veve ?
      // Lehetne DB alapjan, de igy gyorsabb !
      i = 0 ;
      nItemData = pSzezCb->GetItemData( i) ;
      while ( nItemData != CB_ERR && nItemData != nEvszam )
      {
        i++ ;
        nItemData = pSzezCb->GetItemData( i) ;
      }

      m_cAktSzezon.Kiurit() ;

      if ( nItemData == nEvszam )
      {
        // Mindenkeppen :
        m_bAktSzezonMentve = TRUE ;

        // Van mar ilyen szezon felveve
        // A kivalasztott szezon betoltese (es kiirasa)
        // Ha hiba van, kiirja maga (HibaKiir( nRC))
        if ( SzezonBetolt( (unsigned)nEvszam) == 0 )
          AktSzezKiir( 0) ;
      }
      else
      {
        // A mostmar helyes szezon evszam beirasa az aktualis szezont tartalmazo osztalyba
// Szezon felvetele az adatbazisba ?
        m_cAktSzezon.m_uiSzezon = (unsigned) nEvszam ;

        // Uj tranzakcio kezdete :
/*
        if ( m_cAktSzezon.SQL_SzezINSERT() != 0 )
        {
          m_bAktSzezonMentve = TRUE ;
          HibaKiir( nRC) ;
        }
        else
        {
          m_bAktSzezonMentve = FALSE ;
        }
*/
        // A pontozasok, elso legjobb eredmeny beszamit CEdit kontrolok torlese
        pPontCb->SetCurSel( -1) ;

        m_bInicializalas = TRUE ;
        pPntEdzEEd->SetWindowText( "0") ;
        pPntLgyKorEd->SetWindowText( "0") ;
        pPntElsLgjBszEd->SetWindowText( "0") ;
        m_bInicializalas = FALSE ;

        pFutLb->ResetContent() ;

        pNagydCb->SetCurSel( -1) ;
        pPalyaCb->SetCurSel( -1) ;
        pDatEd->SetWindowText("") ;
      }
    }
  }
}

void CSzezFelv::OnSelchgSzez()
{
  CComboBox* pSzezCb = (CComboBox*) GetDlgItem(IDC_SZEZ_CB) ;

  int      nCBPoz ;
  unsigned uiKivSzez ;

  long nRC ;

  CString  sAblCim, sSzov ;
  // Mivel az itoa fv. nem foglalkozik helyfoglalassal. Max 17 byte-ra van szuksege.
  char pszSegedStr[18] ;

  // A kivalsztott szezon betoltese
  nCBPoz = pSzezCb->GetCurSel() ;

  if ( nCBPoz == CB_ERR )
    return ;

//  uiKivSzez = pSzezCb->GetLBText( nCBPoz) ;

  if ( m_bAktSzezonMentve == FALSE )
  {
    // Mivel az aktualis tartalom nem volt mentve
    GetWindowText( sAblCim) ;

    if ( sSzov.LoadString(IDS_SZEZ_MEGV) == FALSE )
      sSzov = "IDS_SZEZ_MEGV" ;

    sprintf( pszSegedStr, "\n   %d", m_cAktSzezon.m_uiSzezon) ;

    if ( IDYES == MessageBox( sSzov+pszSegedStr, sAblCim, MB_ICONQUESTION|MB_YESNO) )
    {
      nRC = SQL_COMMIT() ;

      if ( nRC != 0 )
      {
        HibaKiir( nRC) ;

        return ;
      }
    }
    else
    {
      nRC = SQL_ROLLBACK() ;
      
      if ( nRC != 0 )
      {
        HibaKiir( nRC) ;

        return ;
      }
    }

    m_bAktSzezonMentve = TRUE ;
  }

// nem ertem, ez az eset hogy' fordulhat elo ...
  // Ha nincs felvitt szezon, a 'Szezon' Cb ures lehet, ilyenkor cKivSzez
  // ures, '.sze' nevu file-t nem tudja betolteni, ennek elkerulese vegett:
  pSzezCb->GetLBText( nCBPoz, sSzov) ;
  sSzov = sSzov.SpanIncluding("0123456789") ;

  if ( sSzov.GetLength() == 4 )
  {
    // A kivalasztott szezon betoltese (es kiirasa)
    m_cAktSzezon.Kiurit() ;

    uiKivSzez = atoi( sSzov) ;

    // Ha hiba van, kiirja maga (HibaKiir( nRC))
    SzezonBetolt( uiKivSzez) ;
    AktSzezKiir( 0) ;
  }
}

void CSzezFelv::OnDblclkFutamList()
{
  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;
  CComboBox* pNagydCb = (CComboBox*) GetDlgItem(IDC_NAGYD_CB)  ;
  CComboBox* pPalyaCb = (CComboBox*) GetDlgItem(IDC_PLYA_CB)   ;
  CEdit*     pDatEd   = (CEdit*)     GetDlgItem(IDC_DAT_E)     ;

  CString sKivSor ;

  // Mivel az itoa fv. nem foglalkozik helyfoglalassal. Max 17 byte-ra van szuksege.
  char pszSegedStr[20] ;
  int nKivSor ;

  unsigned char ucNdIdx ;
  unsigned uiNdKulcs   ;

  nevadat* pNevAdat  ;

  nKivSor = pFutLb->GetCurSel() ;

  // Ha nincs semmi kiszelektalva azzal ne foglalkozzunk
  if ( nKivSor != LB_ERR )
  {
    uiNdKulcs = pFutLb->GetItemData( nKivSor) ;

    // Szelektalas a Combo Boxbol csak akkor, ha megtalalta
    if ( m_pNagyd->Lookup( uiNdKulcs, pNevAdat) )
      pNagydCb->SelectString(-1, pNevAdat->m_sNev) ;

    // Lehetne talan az LB beli indexet a szezonra alkalmazni ?
    ucNdIdx = m_cAktSzezon.NdBolIdx( uiNdKulcs) ;

    // Szelektalas a Combo Boxbol csak akkor, ha megtalalta
    // m_cAktSzezon.m_pNagydijak[nKivSor]->m_uiPalya
    if ( m_pPalya->Lookup( m_cAktSzezon.PalyaKulcs( ucNdIdx), pNevAdat) )
      pPalyaCb->SelectString(-1, pNevAdat->m_sNev) ;

    // A datumot a kivalasztott sor szama alapjan keresi ki
    // az aktualis szezont tartalmazo osztalybol
    sprintf( pszSegedStr, "%d %d %d", m_cAktSzezon.NagydEv( ucNdIdx),
                                      m_cAktSzezon.NagydHonap( ucNdIdx),
                                      m_cAktSzezon.NagydNap( ucNdIdx) ) ;

    pDatEd->SetWindowText( pszSegedStr) ;
  }
}

void CSzezFelv::OnSelchgPont()
{
  CComboBox* pPontCb = (CComboBox*) GetDlgItem(IDC_PNT_CB) ;

  int nKivElem ;

  // Az aktualis szezonban levo pontozas azonnal kovesse a kepernyon levot
  nKivElem = pPontCb->GetCurSel() ;

  if ( nKivElem != CB_ERR )
  {
    m_cAktSzezon.m_uiPontozas = unsigned ( pPontCb->GetItemData(nKivElem) ) ;

    // A szezon a modositas miatt mentetlen lett:
    m_bAktSzezonMentve = FALSE ;
  }
}

void CSzezFelv::OnFelv()
{
  CComboBox* pNagydCb = (CComboBox*) GetDlgItem(IDC_NAGYD_CB)  ;
  CComboBox* pPalyaCb = (CComboBox*) GetDlgItem(IDC_PLYA_CB)   ;
  CEdit*     pDatEd   = (CEdit*)     GetDlgItem(IDC_DAT_E)     ;

  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;

  unsigned char  ucBeszIdx ;

  long nRC ;

  char pszSegedStr[20] ;
  CString sNagydij, sPalya, sEgySor ;

  nevadat* pNevAdat ;
  SNagydij* pUjNd = new SNagydij() ;

  if ( pUjNd == NULL )
    return ;

  if ( FutAdatBeo( pUjNd) )
  {
    nRC = m_cAktSzezon.UjFelv( pUjNd) ;
    if ( nRC == 0 )
    {
      ucBeszIdx = m_cAktSzezon.NdBolIdx( pUjNd->m_uiNagyd) ;

      // ucBeszIdx == usUjSzezMer ???
      if ( m_pNagyd->Lookup( pUjNd->m_uiNagyd, pNevAdat) == TRUE )
      {
        sNagydij = pNevAdat->m_sNev ;
      }
      else
      {
        sNagydij = "????" ;
      }

      if ( m_pPalya->Lookup( pUjNd->m_uiPalya, pNevAdat) == TRUE )
      {
        sPalya = pNevAdat->m_sNev ;
      }
      else
      {
        sPalya = "????" ;
      }

      sprintf( pszSegedStr, "%d %d %d", m_cAktSzezon.NagydEv( ucBeszIdx),
                                        m_cAktSzezon.NagydHonap( ucBeszIdx),
                                        m_cAktSzezon.NagydNap( ucBeszIdx) ) ;

      sEgySor = sNagydij + CString('\t') + sPalya + CString('\t') + pszSegedStr ;

      pFutLb->InsertString( ucBeszIdx, sEgySor) ;
      pFutLb->SetItemData( ucBeszIdx, (DWORD) pUjNd->m_uiNagyd) ;

      pDatEd->SetWindowText("") ;
      // A szezon a modositas miatt mentetlen lett:
      m_bAktSzezonMentve=FALSE ;
    }
    else
    {
      HibaKiir( nRC) ;
    }
  }
  else
  {// Hibas beolvasas eseten nem kell a lefoglalt osztaly: torlese
    delete pUjNd ;
  }
}

void CSzezFelv::OnMod()
{
  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;
  CComboBox* pNagydCb = (CComboBox*) GetDlgItem(IDC_NAGYD_CB)  ;
  CComboBox* pPalyaCb = (CComboBox*) GetDlgItem(IDC_PLYA_CB)   ;

  SNagydij sUjNagydij ;

  int nKivFut, nKivNd, nKivPa ;
  unsigned uiModNdKulcs, uiKivNdKulcs, uiKivPaKulcs ;

  long nRC ;

  // A kivalasztott futam :
  nKivFut = pFutLb->GetCurSel() ;

  // Mivel a FutAdatBeo fv nem engedi azonos nd. szerepleset,
  // modositasat sem, ezert :
  nKivNd = pNagydCb->GetCurSel() ;
  nKivPa = pPalyaCb->GetCurSel() ;

  // Az elvart helyeken van kivalasztott elem
  if ( nKivFut!=LB_ERR && nKivNd!=CB_ERR && nKivPa!=CB_ERR )
  {
    // Sikeres volt a beolvasas
    uiModNdKulcs = pFutLb->GetItemData( nKivFut)  ;
    uiKivNdKulcs = pNagydCb->GetItemData( nKivNd) ;
    uiKivPaKulcs = pPalyaCb->GetItemData( nKivPa) ;

    if ( DatumBeo( sUjNagydij.m_sDatum) == TRUE )
    {
      sUjNagydij.m_uiNagyd = uiKivNdKulcs ;
      sUjNagydij.m_uiPalya = uiKivPaKulcs ;

      // Nem kell new, mert a meglevot irja felul,az adatbazist is modositja
      nRC = m_cAktSzezon.Modosit( uiModNdKulcs, sUjNagydij) ;
      if ( nRC == 0 )
      {
        // A szezon a modositas miatt mentetlen lett:
        m_bAktSzezonMentve = FALSE ;

        AktSzezKiir( uiKivNdKulcs) ;
      }
      else
      {
        HibaKiir( nRC) ;
      }
    }
  }
}

void CSzezFelv::OnTorol()
{
  CListBox* pFutLb = (CListBox*) GetDlgItem(IDC_FTMK_LIST) ;
  CEdit*    pDatEd = (CEdit*)    GetDlgItem(IDC_DAT_E)     ;

  int nKiv ;
  unsigned uiKivNdKulcs ;
  long nRC ;

  nKiv = pFutLb->GetCurSel() ;

  if ( nKiv != LB_ERR )
  {
    // Ha van kiszelektalt elem
    uiKivNdKulcs = pFutLb->GetItemData( nKiv) ;

    nRC = m_cAktSzezon.Torol( uiKivNdKulcs) ;
    if ( nRC == 0 )
    {
      pFutLb->DeleteString( nKiv) ;

      pDatEd->SetWindowText("") ;
      // A szezon a modositas miatt mentetlen lett:
      m_bAktSzezonMentve=FALSE ;
    }
    else
    {
      // A torles meghiusult a nd.-hoz tartozo rajtsorrend/befuto miatt,
      // vagy a megadott index nemletezo elemre mutat ...
      HibaKiir( nRC) ;
    }
  }
}

void CSzezFelv::OnSzezFelv()
{
  CListBox*    pFutLb    = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;
  CComboBox*   pSzezCb   = (CComboBox*) GetDlgItem(IDC_SZEZ_CB)   ;

  CComboBox*   pPontCb   = (CComboBox*) GetDlgItem(IDC_PNT_CB)    ;
  
  CEdit* pPntEdzEEd      = (CEdit*) GetDlgItem(IDC_PNT_EDZE_E)    ;
  CEdit* pPntLgyKorEd    = (CEdit*) GetDlgItem(IDC_PNT_LGYKOR_E)  ;
  CEdit* pPntElsLgjBszEd = (CEdit*) GetDlgItem(IDC_ELS_LGJ_BESZ_E);

  long nRC ;
  CString sHibaSzov, sAblCim ;
  unsigned char ucFelvNdSzama ;
  char pszSegedStr[34] ;

  // Az aktualis szezon mentetlen:
  // Ezaltal ures szezont nem lehet felvenni
  if ( m_bAktSzezonMentve == FALSE )
  {
    // Mi van, ha az ablak tartalma szemantikailag hibas szz. ev, hianyzo pontozas)
    if ( EdzELgyKElsLgjBeo() == FALSE )
    {
      return ;
    }

    // Nincs pontozas megadva : hiba !
    if ( m_cAktSzezon.m_uiPontozas == 0 )
    {
      GetWindowText( sAblCim) ;

      if ( sHibaSzov.LoadString(IDS_NINCS_PONTOZAS_S) == FALSE )
        sHibaSzov = "IDS_NINCS_PONTOZAS_S" ;

      MessageBox( sHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) ;

      return ;
    }

    // Ha a kulonfele pontozasokat sikerult beolvasni,
    // most egyben irjuk be oket az adatbazisba, nem
    // kulon, midig, amikor valamelyik CEdit valtozik
//    if ( (nRC=m_cAktSzezon.SQL_SzezUPDATE()) != 0 )
    // OnOK-nal elvarom, hogy utana minden benne legyen az adatbazisban,
    // ezert nem UPDATE-tel inditok !

    nRC = m_cAktSzezon.SzezonFelvesz( ucFelvNdSzama) ;

    if ( nRC != 0 && nRC != -1 )
    {
      // -2290 : check constraint (CHK_SZEZ_EV) violated (1950<=szez_eve)
      if ( nRC == -2290 )
      {
        pSzezCb->SetFocus() ;
        pSzezCb->SetEditSel( (DWORD)0xffff0000, (BOOL)FALSE ) ;
      }
      else
      {
        HibaKiir( nRC) ;
      }

      return ;
    }

    nRC = SQL_COMMIT() ;

    if ( nRC != 0 )
    {
      HibaKiir( nRC) ;

      return ;
    }
  }

  // Hogy a kovetkezo szezon Cb-beli valtozas eseten ne akarja menteni a semmit
  m_bAktSzezonMentve = TRUE ;

  // Az iment mentett szezon hozzaadasa a felvett szezonok Cb-hoz
  // Ha meg nincs benne
  sprintf( pszSegedStr, "%d", m_cAktSzezon.m_uiSzezon) ;
  if (pSzezCb->FindStringExact( -1, pszSegedStr ) == CB_ERR )
  {
    // Hiba kereseskor: nincs meg; beir a listaba
    pSzezCb->AddString(pszSegedStr) ;
  }

  // A dialogus ablak mezoinek torlese
  pSzezCb->SetWindowText("") ;
  pFutLb->ResetContent() ;

  pPontCb->SetCurSel( -1) ;

  m_bInicializalas = TRUE ;

  pPntEdzEEd->SetWindowText("") ;
  pPntLgyKorEd->SetWindowText("") ;
  pPntElsLgjBszEd->SetWindowText("") ;

  m_bInicializalas = FALSE ;

  // Az ablak aktualis adattagjanak torlese
  m_cAktSzezon.Kiurit() ;

  m_bAktSzezonMentve = TRUE ;
}

// A szezonhoz tartozo nevezesi listat is ?
// A nevezesi lista bejegyzesek tkeppen rajtsorrend bejegyzesek is,
// igy ha leteznek, nem lehet a szezont sem torolni.
void CSzezFelv::OnSzezTorol()
{
  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;
  CEdit*     pDatEd   = (CEdit*)     GetDlgItem(IDC_DAT_E)     ;

  CComboBox* pSzezCb  = (CComboBox*) GetDlgItem(IDC_SZEZ_CB)   ;

  CEdit* pPntEdzEEd      = (CEdit*) GetDlgItem(IDC_PNT_EDZE_E)    ;
  CEdit* pPntLgyKorEd    = (CEdit*) GetDlgItem(IDC_PNT_LGYKOR_E)  ;
  CEdit* pPntElsLgjBszEd = (CEdit*) GetDlgItem(IDC_ELS_LGJ_BESZ_E);

  char pszEvszam[20] ;
  int  nKivSzez ;
  long nRC ;

  CString sSzov, sAblCim ;

  GetWindowText( sAblCim) ;

  sprintf( pszEvszam, "\n   %d", m_cAktSzezon.m_uiSzezon) ;

  if ( sSzov.LoadString(IDS_TRL_MEGER_S) == FALSE )
    sSzov = "IDS_TRL_MEGER_S" ;

  if (IDNO==MessageBox( sSzov+pszEvszam, sAblCim, MB_ICONQUESTION | MB_YESNO ))
  {
    return ;
  }

  // Mivel a SzezonTorol() nullazza a szezont, ki kellett hozni az 'if' ele
  sprintf( pszEvszam, "%d", m_cAktSzezon.m_uiSzezon) ;

  nRC = m_cAktSzezon.SzezonTorol() ;
  if ( nRC == 0 )
  {
    // A Cancel gomb erre nem fog vonatkozni, de nem lenne jo,
    // ha egy kovetkezo szezon modositasan vegrehajtott ROLLBACK
    // erre is vonatkozna...
    nRC = SQL_COMMIT() ;
    if ( nRC != 0 )
    {
      HibaKiir( nRC) ;
    }

    // A szezon torlese a szezonok comboboxbol
    nKivSzez = pSzezCb->FindString( -1, pszEvszam) ;
    pSzezCb->DeleteString( nKivSzez) ;

    // Az ablak torlese
    pFutLb->ResetContent() ;
    pDatEd->SetWindowText("") ;

    m_bInicializalas = TRUE ;

    pPntEdzEEd->SetWindowText("") ;
    pPntLgyKorEd->SetWindowText("") ;
    pPntElsLgjBszEd->SetWindowText("") ;

    m_bInicializalas = FALSE ;
  }
  else
  {
    HibaKiir( nRC) ;

    // Ha SQLNOTFOUND, az azt jelenti, hogy nem lett meg ervenyes ev megadva,
    // ezaltal nem kerult bejegyzes sem a szezon tablaba
    if ( nRC == 1403 )
    {
      pSzezCb->SetFocus() ;
      pSzezCb->SetEditSel( (DWORD)0xffff0000, (BOOL)FALSE ) ;
    }
  }
}

// Az OK gomb lenyomasakor (mentetlenseg eseten) megprobalja menteni
// az aktualis kepernyotaralmat
void CSzezFelv::OnOK()
{
  CComboBox* pSzezCb = (CComboBox*) GetDlgItem(IDC_SZEZ_CB) ;

  CString sAblCim, sHibaSzov ;

  unsigned char ucFelvNdSzama ;
  long nRC ;

  if ( m_bAktSzezonMentve == FALSE )
  {
    // Mi van, ha az ablak tartalma szemantikailag hibas szz. ev, hianyzo pontozas)
    if ( EdzELgyKElsLgjBeo() == FALSE )
    {
      return ;
    }

    // Nincs pontozas megadva : hiba !
    if ( m_cAktSzezon.m_uiPontozas == 0 )
    {
      GetWindowText( sAblCim) ;

      if ( sHibaSzov.LoadString(IDS_NINCS_PONTOZAS_S) == FALSE )
        sHibaSzov = "IDS_NINCS_PONTOZAS_S" ;

      MessageBox( sHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) ;

      return ;
    }

    // Ha a kulonfele pontozasokat sikerult beolvasni,
    // most egyben irjuk be oket az adatbazisba, nem
    // kulon, midig, amikor valamelyik CEdit valtozik

//    if ( (nRC=m_cAktSzezon.SQL_SzezUPDATE()) != 0 )
    // OnOK-nal elvarom, hogy utana minden benne legyen az adatbazisban,
    // ezert nem UPDATE-tel inditok !
    nRC = m_cAktSzezon.SzezonFelvesz( ucFelvNdSzama) ;

    if ( nRC != 0 && nRC != -1 )
    {
      // -2290 : check constraint (CHK_SZEZ_EV) violated (1950<=szez_eve)
      if ( nRC == -2290 )
      {
        pSzezCb->SetFocus() ;
        pSzezCb->SetEditSel( (DWORD)0xffff0000, (BOOL)FALSE ) ;
      }
      else
      {
        HibaKiir( nRC) ;
      }

      return ;
    }

    nRC = SQL_COMMIT() ;
    if ( nRC != 0 )
    {
      HibaKiir( nRC) ;
    }
  }

  CDialog::OnOK() ;
}

void CSzezFelv::OnCancel()
{
  CComboBox* pSzezCb = (CComboBox*) GetDlgItem(IDC_SZEZ_CB) ;
  CComboBox* pPontCb = (CComboBox*) GetDlgItem(IDC_PNT_CB) ;

  CString sAblCim, sHibaSzov ;
  char pszSegedStr[18] ;
  unsigned char ucFelvNdSzama ;

  long nRC ;

  if ( m_bAktSzezonMentve == FALSE )
  {
    GetWindowText( sAblCim) ;

    if ( sHibaSzov.LoadString(IDS_SZEZ_MEGV) == FALSE )
      sHibaSzov = "IDS_SZEZ_MEGV" ;

    sprintf( pszSegedStr, "\n   %d", m_cAktSzezon.m_uiSzezon) ;

    if ( IDYES == MessageBox( sHibaSzov+pszSegedStr, sAblCim, MB_ICONQUESTION|MB_YESNO) )
    {
      // Mi van, ha az ablak tartalma szemantikailag hibas (hianyzo pontozas)
      if ( EdzELgyKElsLgjBeo() == FALSE )
      {
        return ;
      }

      // Nincs pontozas megadva : hiba !
      if ( m_cAktSzezon.m_uiPontozas == 0 )
      {
        GetWindowText( sAblCim) ;

        if ( sHibaSzov.LoadString(IDS_NINCS_PONTOZAS_S) == FALSE )
          sHibaSzov = "IDS_NINCS_PONTOZAS_S" ;

        MessageBox( sHibaSzov, sAblCim, MB_OK | MB_ICONSTOP ) ;

        return ;
      }

      // Ha a kulonfele pontozasokat sikerult beolvasni,
      // most egyben irjuk be oket az adatbazisba, nem
      // kulon, midig, amikor valamelyik CEdit valtozik
//    if ( (nRC=m_cAktSzezon.SQL_SzezUPDATE()) != 0 )
      // OnOK-nal elvarom, hogy utana minden benne legyen az adatbazisban,
      // ezert nem UPDATE-tel inditok !
      nRC = m_cAktSzezon.SzezonFelvesz( ucFelvNdSzama) ;

      if ( nRC != 0 && nRC != -1 )
      {
        // -2290 : check constraint (CHK_SZEZ_EV) violated (1950<=szez_eve)
        if ( nRC == -2290 )
        {
          pSzezCb->SetFocus() ;
          pSzezCb->SetEditSel( (DWORD)0xffff0000, (BOOL)FALSE ) ;
        }
        else
        {
          HibaKiir( nRC) ;
        }

        return ;
      }

      nRC = SQL_COMMIT() ;

      if ( nRC == 0 )
      {
        m_bAktSzezonMentve = TRUE ;
      }
      else
      {
        HibaKiir( nRC) ;
      }
    }
    else
    {
      nRC = SQL_ROLLBACK() ;

      if ( nRC != 0 )
      {
        HibaKiir( nRC) ;
      }
    }
  }

  CDialog::OnCancel();
}

void CSzezFelv::OnChgPntEdzE()
{
  if ( m_bInicializalas == FALSE)
  {
    if ( EdzElsPontBeo() == TRUE )
    {
      m_bAktSzezonMentve = FALSE ;
    }
  }
}

void CSzezFelv::OnChgPntLgyKor()
{
  if ( m_bInicializalas == FALSE)
  {
    if ( LgyKorPontBeo() == TRUE )
    {
      m_bAktSzezonMentve = FALSE ;
    }
  }
}

void CSzezFelv::OnChgElsLgjBesz()
{
  if ( m_bInicializalas == FALSE )
  {
    if ( LegjBefSzamBeo() == TRUE )
    {
      m_bAktSzezonMentve = FALSE ;
    }
  }
}

BOOL CSzezFelv::FutAdatBeo(SNagydij* pBeolvasottFut)
{
  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;
  CComboBox* pNagydCb = (CComboBox*) GetDlgItem(IDC_NAGYD_CB)  ;
  CComboBox* pPalyaCb = (CComboBox*) GetDlgItem(IDC_PLYA_CB)   ;

  CString sNagydij, sPalya ;

  pNagydCb->GetLBText(pNagydCb->GetCurSel(), sNagydij) ;
  pPalyaCb->GetLBText(pPalyaCb->GetCurSel(), sPalya) ;

  if ( sNagydij=="" || sPalya=="" || DatumBeo(pBeolvasottFut->m_sDatum)==FALSE )
  {
    // Ha valamely ablak ures, vagy a datummegadas hibas
    return FALSE ;
  }
  else
  {
    // Az ablakok nem uresek: tartalmuk vizsgalata
    // Szerepelt-e mar ez a nagydij a szezonban
    if (pFutLb->SelectString(-1, sNagydij)!=LB_ERR)
    {
      return FALSE ;
    }

    // Beolvasas az ItemData alapjan
    pBeolvasottFut->m_uiNagyd=(unsigned)pNagydCb->GetItemData(pNagydCb->FindStringExact( -1, sNagydij)) ;
    pBeolvasottFut->m_uiPalya=(unsigned)pPalyaCb->GetItemData(pPalyaCb->FindStringExact( -1, sPalya)) ;

    return TRUE ;
  }

  // Ha ide eljut a vezerles, baj van ...
  return FALSE ;
}

long CSzezFelv::SzezonBetolt(unsigned uiBeoSzez)
{
  long nRC ;

  nRC = m_cAktSzezon.SQL_SzezonBetolt( uiBeoSzez) ;

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;
  }

//------------------------------------------------------------------------------
//    char pszDumpFile[31] ;
//    sprintf(pszDumpFile, "c:\\forma1\\%d.dmp", m_cAktSzezon.m_usSzezon) ;
//    m_cAktSzezon.CSzezonDump(CString(pszDumpFile)) ;
//------------------------------------------------------------------------------
  return nRC ;
}

//   0 eseten marad a kijeloles
void CSzezFelv::AktSzezKiir( unsigned uiUjKiv)
{
  CComboBox* pPontCb  = (CComboBox*) GetDlgItem(IDC_PNT_CB)    ;
  CListBox*  pFutLb   = (CListBox*)  GetDlgItem(IDC_FTMK_LIST) ;

  CEdit* pPntEdzEEd      = (CEdit*) GetDlgItem(IDC_PNT_EDZE_E)    ;
  CEdit* pPntLgyKorEd    = (CEdit*) GetDlgItem(IDC_PNT_LGYKOR_E)  ;
  CEdit* pPntElsLgjBszEd = (CEdit*) GetDlgItem(IDC_ELS_LGJ_BESZ_E);

  CString sPontozas, sNagyd, sPalya, sDatum, sEgySor ;
  unsigned char ucNdIdx, ucUjSzezMer ;
  nevadat* pNevAdat ;
  pontadat* pPontAdat ;
  unsigned uiNagydKulcs, uiPalyaKulcs ;

  // Mivel az itoa fv. nem foglalkozik helyfoglalassal. Max 17 byte-ra van szuksege.
  char pszSegedStr[20] ;
  // A kivalasztott pontozast tartalmazo szerkeszto ablakba
  // az uj szezonban kivalasztott ertek kiirasa. HA UJ A SZEZON, A 0. SORT (SZEREPELT-E) IRJA VISSZA
  if ( m_pPontozas->Lookup( m_cAktSzezon.m_uiPontozas, pPontAdat) == TRUE )
  {
    PontbolStr( pPontAdat, sPontozas, CString("     ")) ;
    pPontCb->SelectString( -1, sPontozas ) ;
  }

  //--------------------------------------------------------------------------
	// Az edzeselsosegert es leggyorsabb korert jaro pontok beirasa
  m_bInicializalas = TRUE ;

  sprintf(pszSegedStr, "%d", m_cAktSzezon.m_ucEdzElsPont) ;
  pPntEdzEEd->SetWindowText(pszSegedStr) ;
  sprintf(pszSegedStr, "%d", m_cAktSzezon.m_ucLeggyKorPont) ;
  pPntLgyKorEd->SetWindowText(pszSegedStr) ;
  sprintf(pszSegedStr, "%d", m_cAktSzezon.m_ucElsLgjBesz) ;
  pPntElsLgjBszEd->SetWindowText(pszSegedStr) ;

  m_bInicializalas = FALSE ;

  // Ha nem lett explicit megadva kijelolendo futam :
  if ( uiUjKiv == 0 )
  {
    uiUjKiv = (unsigned)pFutLb->GetItemData( pFutLb->GetCurSel()) ;
  }

  // A futamokat tartalmazo Lb feltoltese
  pFutLb->ResetContent() ;

  ucNdIdx = 0 ;

  ucUjSzezMer = m_cAktSzezon.Meret() ;

  while ( ucNdIdx < ucUjSzezMer )
  {
    // A nagydij nevenek meghatarozasa
    uiNagydKulcs = m_cAktSzezon.NagydKulcs( ucNdIdx) ;
    if ( m_pNagyd->Lookup(uiNagydKulcs, pNevAdat) == TRUE )
    {
      sNagyd = pNevAdat->m_sNev ;
    }
    else
    {
      sNagyd = "????" ;
    }

    uiPalyaKulcs=m_cAktSzezon.PalyaKulcs( ucNdIdx) ;
    if ( m_pPalya->Lookup( uiPalyaKulcs, pNevAdat) == TRUE )
    {
      sPalya = pNevAdat->m_sNev ;
    }
    else
    {
      sPalya = "????" ;
    }

    sprintf( pszSegedStr, "%d %d %d", m_cAktSzezon.NagydEv( ucNdIdx),
                                      m_cAktSzezon.NagydHonap( ucNdIdx),
                                      m_cAktSzezon.NagydNap( ucNdIdx) ) ;

    sEgySor=sNagyd+CString('\t')+sPalya+CString('\t')+pszSegedStr ;
    // A sorhoz tartozo adatok beallitasa: "DWORD" wNagydKulcs  wPalyaKulcs
    pFutLb->SetItemData( pFutLb->AddString( sEgySor), m_cAktSzezon.NagydKulcs( ucNdIdx)) ;

    ucNdIdx++ ;
  }

  //                -1
  if ( uiUjKiv != LB_ERR )
  {
    ucNdIdx = 0 ;
    while ( ucNdIdx < ucUjSzezMer )
    {
      if ( uiUjKiv == pFutLb->GetItemData( ucNdIdx) )
      {
        pFutLb->SetCurSel( ucNdIdx) ;
        return ;
      }

      ucNdIdx++ ;
    }
  }

  pFutLb->SetCurSel( 0) ;
}

BOOL CSzezFelv::DatumBeo(SDatum& sBeoDat)
{
  CEdit* pDatEd = (CEdit*) GetDlgItem(IDC_DAT_E) ;

  CString sBeoSztr, sEv, sHo, sNap ;
  int i, j ;

  GetDlgItem(IDC_DAT_E)->GetWindowText(sBeoSztr) ;

  if (sBeoSztr.GetLength==0)
  {
    // Nem olvastunk be semmit: toroljuk a datumot
    sBeoDat = SDatum( 0, 0, 0) ;

    // A datumot tartalmazo szerkeszto ablak tartalmanak kijelolese
    pDatEd->SetFocus() ;
    pDatEd->SetSel( (DWORD)0xffff0000 , (BOOL)FALSE ) ; // ((DWORD)((WORD)(0xffff)))<<16

    return FALSE ;
  }

  i=0; // A felvezeto nem szam karakterek kiszurese
  while ( i<sBeoSztr.GetLength() && !isdigit(sBeoSztr[i]) )
    i++ ;

  j=0;  // Az ev szamjegyeinek beolvasasa
  while (i<sBeoSztr.GetLength() && isdigit(sBeoSztr[i]) && j<4)
  {
    sEv=sEv+sBeoSztr[i] ;
    j++ ;
    i++ ;
  }

  // Az ev es a honap kozti esetleges nem szam karakter atlepese
  while ( i<sBeoSztr.GetLength() && !isdigit(sBeoSztr[i]) )
    i++ ;

  j=0;  // A honap szamjegyeinek beolvasasa
  while (i<sBeoSztr.GetLength() && isdigit(sBeoSztr[i]) && j<2)
  {
    sHo=sHo+sBeoSztr[i] ;
    j++ ;
    i++ ;
  }
  // Az honap es a nap kozti esetleges nem szam karakter atlepese
  while ( i<sBeoSztr.GetLength() && !isdigit(sBeoSztr[i]) )
    i++ ;

  j=0;  // A nap szamjegyeinek beolvasasa
  while ( i<sBeoSztr.GetLength() && isdigit(sBeoSztr[i]) && j<2 )
  {
    sNap = sNap+sBeoSztr[i] ;
    j++ ;
    i++ ;
  }

  // A beolvasasnal tett hosszmegkotesek biztositjak, hogy beferjenek a szamok
  sBeoDat = SDatum( (unsigned short)atol(sEv),
                    (unsigned char) atoi(sHo),
                    (unsigned char) atoi(sNap)) ;

  // Az evnel nem teszek tobb feltetelt, mert ugysem lehet 5 digites (l. fent)
  // Negativ szamok sem lehetnek, mert a - -t atugorja
  // Csak a szezon evel megegyezo evszamot fogadja el + stb.

  if ( sBeoDat.ev != m_cAktSzezon.m_uiSzezon || sBeoDat.ev<ELSO_F1_SZEZ ||
       sBeoDat.honap==0 || sBeoDat.honap>12 || sBeoDat.nap==0 || sBeoDat.nap>31 )
  {
    // A (hibas) datumot tartalmazo szerkeszto ablak tartalmanak kijelolese
    pDatEd->SetFocus() ;
    pDatEd->SetSel( (DWORD)0xffff0000 , (BOOL)FALSE ) ; // ((DWORD)((WORD)(0xffff)))<<16

    return FALSE ;
  }
  else
  {
    return TRUE ;
  }
  // Ha ide eljut a vezerles, baj van ...
  return FALSE ;
}

BOOL CSzezFelv::EdzELgyKElsLgjBeo()
{
  if ( EdzElsPontBeo() == TRUE )
  {
    if ( LgyKorPontBeo() == TRUE )
    {
      if ( LegjBefSzamBeo() == TRUE )
      {
        return TRUE ;
      }
    }
  }

  return FALSE ;
}

BOOL CSzezFelv::EdzElsPontBeo()
{
  CEdit* pPntEdzEEd = (CEdit*) GetDlgItem(IDC_PNT_EDZE_E) ;

  char pszSegedStr[18] ;
  int  i, nBeoErt ;

  // Az edzeselsosegert jaro pontok beolvasasa
  pPntEdzEEd->GetWindowText( pszSegedStr, 17) ;

  for ( i=0 ; pszSegedStr[i] != '\0' ; i++ )
  {
    if ( pszSegedStr[i] < '0' || '9' < pszSegedStr[i] )
    {
      pPntEdzEEd->SetFocus() ;
      pPntEdzEEd->SetSel( i, -1) ;
      return FALSE ;
    }
  }

  nBeoErt = atoi( pszSegedStr) ;

  if ( nBeoErt > 255 )
  {
    pPntEdzEEd->SetFocus() ;
    pPntEdzEEd->SetSel( -1, 0) ;
    return FALSE ;
  }

  m_cAktSzezon.m_ucEdzElsPont = (unsigned char) nBeoErt ;

  return TRUE ;
}

BOOL CSzezFelv::LgyKorPontBeo()
{
  CEdit* pPntLgyKorEd = (CEdit*) GetDlgItem(IDC_PNT_LGYKOR_E) ;

  char pszSegedStr[18] ;
  int  i, nBeoErt ;

  pPntLgyKorEd->GetWindowText(pszSegedStr, 17) ;

  for ( i=0 ; pszSegedStr[i] != '\0' ; i++ )
  {
    if ( pszSegedStr[i] < '0' || '9' < pszSegedStr[i] )
    {
      pPntLgyKorEd->SetFocus() ;
      pPntLgyKorEd->SetSel( i, -1) ;
      return FALSE ;
    }
  }

  nBeoErt = atoi(pszSegedStr) ;

  if ( nBeoErt > 255 )
  {
    pPntLgyKorEd->SetFocus() ;
    pPntLgyKorEd->SetSel( -1, 0) ;
    return FALSE ;
  }

  m_cAktSzezon.m_ucLeggyKorPont = (unsigned char) nBeoErt ;

  return TRUE ;
}

BOOL CSzezFelv::LegjBefSzamBeo()
{
  CEdit* pPntElsLgjBszEd = (CEdit*) GetDlgItem(IDC_ELS_LGJ_BESZ_E);

  char pszSegedStr[18] ;
  int  i, nBeoErt ;

  pPntElsLgjBszEd->GetWindowText(pszSegedStr, 17) ;

  for ( i=0 ; pszSegedStr[i] != '\0' ; i++ )
  {
    if ( pszSegedStr[i] < '0' || '9' < pszSegedStr[i] )
    {
      pPntElsLgjBszEd->SetFocus() ;
      pPntElsLgjBszEd->SetSel( i, -1) ;
      return FALSE ;
    }
  }

  nBeoErt = atoi(pszSegedStr) ;

  if ( nBeoErt > 255 )
  {
    pPntElsLgjBszEd->SetFocus() ;
    pPntElsLgjBszEd->SetSel( -1, 0) ;
    return FALSE ;
  }

  m_cAktSzezon.m_ucElsLgjBesz = (unsigned char) nBeoErt ;

  return TRUE ;
}

void CPontFelv(pontadat* pPontAdat, CString& sCelStr, char cElv)
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

void CSzezFelv::PontbolStr( pontadat* pPontAdat, CString& sCelStr, CString& sElv)
{
  int i ;

  // Mivel az itoa fv. nem foglalkozik helyfoglalassal. Max 17 byte-ra van szuksege.
  char pszPontszam[18] ;

  for ( i = 0 ; i < MAX_ERTEKELT - 1 ; i++ )
  {
    sprintf( pszPontszam, "%d", pPontAdat->m_pPontSor[i]) ;
    sCelStr = sCelStr + pszPontszam + sElv ;
  }

  if ( i < MAX_ERTEKELT )
  {
    sprintf( pszPontszam, "%d", pPontAdat->m_pPontSor[i]) ;
    sCelStr = sCelStr + pszPontszam ;
  }
}

void CSzezFelv::HibaKiir(long nRC)
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
