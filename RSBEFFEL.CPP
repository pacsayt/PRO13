// rsbeffel.cpp : implementation file
// Az R/B eredmenyek felvetelet vegzo osztaly.
// Az osztaly ket (meglehetosen hasonlo) dialogus ablak eroforrassal dolgozik
// a szezo + nagydij valasztasa ablakban aktiv radio button-nak megfeleloen.
// Az ablak eroforras azonositojat a konstruktorban adom at.

#include "stdafx.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

// A proba13.h miatt kell a kovetkezo negy include
#include "resource.h"
#include "alaptip.h"
#include "cnev.h"
#include "cvnev.h"
#include "pontozas.h"

#include "dbglobal.h"
#include "ensqlcam.h"

#include "nevfelv.h"
#include "vnevfelv.h"

#include "proba13.h"
#include "cszezon.h"
#include "idoeredm.h"
#include "nevzlist.h"
#include "rajtsbef.h"

#include "befvkieg.h"
#include "impgyabl.h"

#include "rsbeffel.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CRsBefFelv dialog

CRsBefFelv::CRsBefFelv( unsigned uiSzezEve, unsigned uiNdKulcs,
                        eAblTipRsBef aRajtsBef, CWnd* pParent /*=NULL*/)
	                                      : CDialog(aRajtsBef, pParent)
{
  DWORD dwDlgBasUnit ;
  long  nRC ;

  //{{AFX_DATA_INIT(CRsBefFelv)
    // NOTE: the ClassWizard will add member initialization here
  //}}AFX_DATA_INIT

  m_enRajtsBef = aRajtsBef ;

  m_cAktRsBef.m_uiSzezEve = uiSzezEve ;
  m_uiNdNevKulcs = uiNdKulcs ;
  m_cAktRsBef.m_uiNagydKulcs = uiNdKulcs ;

  m_pImportAbl  = NULL ;
  m_nImpKezdoSor= KEZDOSOR ;

  // Ezeket a mutatokat nem konstruktorban adom at, mivel fixnek tekinthetok :
  m_pVersenyzo= &( ( (CProba13App*)AfxGetApp() )->m_cVersenyzoNev ) ;
  m_pNemz     = &( ( (CProba13App*)AfxGetApp() )->m_cNemzetisegNev) ;
  m_pCsapat   = &( ( (CProba13App*)AfxGetApp() )->m_cCsapatNev    ) ;
  m_pMotor    = &( ( (CProba13App*)AfxGetApp() )->m_cMotorNev     ) ;

  // LOKALIS ADATOK :
  m_bAktNvzLstMentve = TRUE ;
  m_bAktRsBefMentve  = TRUE ;

  dwDlgBasUnit = GetDialogBaseUnits() ;

  m_nRBFelvTbStp[0] = 4 *  70  / LOWORD(dwDlgBasUnit) ;
  m_nRBFelvTbStp[1] = 4 * 160  / LOWORD(dwDlgBasUnit) ;
  m_nRBFelvTbStp[2] = 4 * 350  / LOWORD(dwDlgBasUnit) ;
  m_nRBFelvTbStp[3] = 4 * 440  / LOWORD(dwDlgBasUnit) ;
  m_nRBFelvTbStp[4] = 4 * 590  / LOWORD(dwDlgBasUnit) ;
  m_nRBFelvTbStp[5] = 4 * 740  / LOWORD(dwDlgBasUnit) ;

  //--------------------------------------------------------------------------
  // A megadott nagydij R/B betoltese lemezrol
  // m_uiSzezon beallitodik itt \/
  nRC = m_cAktNvzLst.SQL_NvzlstBetolt( uiSzezEve) ;

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;

    return ;
  }
}

BOOL CRsBefFelv::Create(UINT nIDTemplate, CWnd* pParentWnd )
{
  return CDialog::Create( nIDTemplate, pParentWnd) ;
}

// A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
// DoDataExchange fuggvenybol
BOOL CRsBefFelv::OnInitDialog()
{
  CDialog::OnInitDialog() ;

  CComboBox* pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZO_CB)  ;
  CComboBox* pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZETISEG_CB) ;
  CComboBox* pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)     ;
  CComboBox* pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)      ;

  CListBox * pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB) ;
  CStatic  * pNdSzov  = (CStatic*)   GetDlgItem(IDC_RB_NEV_S) ;

  char pszGrpBoxSzov[30] ;
  CRBBejegyzes* pRBBejegyzes ;

  CString sSzov ;

  // Valtozok a 'map'-eken valo vegiglepkedeshez
  POSITION  poz ;
  unsigned  uiKulcs ;
  nevadat*  pNdNevadat ;
  vnevadat* pVAdat ;

  long nRC ;

  // A fele pont miatt eloszor az r/b-t kell betolteni
  nRC = m_cAktRsBef.SQL_RajtsBefBetolt( m_enRajtsBef) ;

  if ( nRC != 0 && nRC != 1403 )
  {
    HibaKiir( nRC) ;

    return FALSE ;
  }

  if ( m_enRajtsBef == IDD_BEF_FELV )
  {
    // A leggyorsabb kor osztalytag felt
    nRC = m_cAktRsBef.m_cCLeggyKor.SQL_LeggyKorBetolt( m_cAktRsBef.m_uiSzezEve, m_cAktRsBef.m_uiNagydKulcs) ;

    if ( nRC != 0 && nRC != 1403 )
    {
      HibaKiir( nRC) ;

      return FALSE ;
    }
  }

  //--------------------------------------------------------------------------
  // A group szovegenek beallitasa - RsBef betoltese a konstruktorban
  // Formatum : 1997 - Hungary (Fele pont)
  if (((CProba13App*)AfxGetApp())->m_cNagydijNev.Lookup( m_uiNdNevKulcs, pNdNevadat))
  {
    // A fele pontszam:az elso helyezettbol allapitom meg(midre ua. kell legyen)
    poz = m_cAktRsBef.GetStartPosition() ;
    if ( poz != NULL )
    {
      m_cAktRsBef.GetNextAssoc( poz, uiKulcs, pRBBejegyzes) ;

      if ( (pRBBejegyzes->Megj()&FELE_PONT) == FELE_PONT )
      {
        if ( sSzov.LoadString( IDS_FELE_PONT_SZOV) == FALSE )
          sSzov = "IDS_FELE_PONT_SZOV" ;

        sprintf(pszGrpBoxSzov, "%d - %s - %s",
                m_cAktRsBef.m_uiSzezEve,
                (const char *)pNdNevadat->m_sNev,
                (const char *)sSzov) ;
	    }
	    else
	    {
        sprintf(pszGrpBoxSzov, "%d - %s",
	              m_cAktRsBef.m_uiSzezEve, (const char *)pNdNevadat->m_sNev) ;
	    }
    }
    else
    {
      sprintf(pszGrpBoxSzov, "%d - %s",
              m_cAktRsBef.m_uiSzezEve, (const char *)pNdNevadat->m_sNev) ;
    }

//      GetDlgItem(IDC_RB_NEV_S)->SetWindowText(pszGrpBoxSzov) ;
    pNdSzov->SetWindowText(pszGrpBoxSzov) ;
  }
  //--------------------------------------------------------------------------
	// A futamokat tartalmazo Lb tabstopjainak beallitasa
/*
	pEredmLb->SetTabStops(6, m_nRBFelvTbStp) ;
*/

  pEredmLb->SendMessage(LB_SETTABSTOPS, 6, (LONG)(LPSTR)m_nRBFelvTbStp) ;

  //--------------------------------------------------------------------------
  // A versenyzo neveket tartalmazo Cb feltoltese
  // Itt a vezeteknev kerul elore, mert a kereses billentyuzet segitsegevel
  // igy konnyebb

  poz = m_pVersenyzo->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pVersenyzo->GetNextAssoc( poz, uiKulcs, pVAdat ) ;
    pVzoCb->SetItemData(pVzoCb->AddString(pVAdat->m_sVezNev+", "+pVAdat->m_sKerNev), uiKulcs) ;
  }

  // A combo box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pVzoCb->SetCurSel(0) ;

  //--------------------------------------------------------------------------
  // A nemzetiseg neveket tartalmazo Cb feltoltese

  NevCbFeltolt( IDC_NEMZETISEG_CB, m_pNemz) ;

  //--------------------------------------------------------------------------
  // A csapat neveket tartalmazo Cb feltoltese

  NevCbFeltolt( IDC_CSAPAT_CB, m_pCsapat) ;

  //--------------------------------------------------------------------------
  // A motor neveket tartalmazo Cb feltoltese

  NevCbFeltolt( IDC_MOTOR_CB, m_pMotor) ;

  if ( m_ImportFile.m_hFile == CFile::hFileNull )
  {
    // A leggyorsabb kor kiirasa
    LegyKorKiir() ;

    // Az aktualis R/B kiirasa a listaablakba, az elso sor lesz kijelolve
    AktRBKiir( 0) ;
  }

  return TRUE ;
}

BEGIN_MESSAGE_MAP(CRsBefFelv, CDialog)
  //{{AFX_MSG_MAP(CRsBefFelv)
    ON_LBN_DBLCLK(IDC_EREDM_LB, OnDblclkErdmLst)
    ON_BN_CLICKED(IDC_FELVESZ_B, OnFelvesz)
    ON_BN_CLICKED(IDC_MODOSIT_B, OnModosit)
    ON_BN_CLICKED(IDC_TOROL_B, OnTorol)
    ON_BN_CLICKED(IDC_RB_FELVESZ_B, OnRbFelvesz)
    ON_BN_CLICKED(IDC_RB_TOROL_B, OnRbTorol)
    ON_BN_CLICKED(IDC_NEVEK_B, OnNevek)
    ON_BN_CLICKED(IDC_LEGGY_KOR_B, OnLeggyKor)
    ON_BN_CLICKED(IDC_BESZAMIT_B, OnBeszamit)
    ON_EN_CHANGE(IDC_RSZ_E, OnChangeRsz)
    ON_BN_CLICKED(IDC_IMPORTAL_B, OnImportal)
  //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRsBefFelv message handlers

void CRsBefFelv::OnDblclkErdmLst()
{
  CListBox  *pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB)      ;

  CEdit     *pHelyezEd = (CEdit*)    GetDlgItem(IDC_HELYEZES_E)    ;
  CEdit     *pRszEd    = (CEdit*)    GetDlgItem(IDC_RSZ_E)         ;

  CComboBox *pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZO_CB)  ;
  CComboBox *pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZETISEG_CB) ;
  CComboBox *pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)     ;
  CComboBox *pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)      ;

  CEdit     *pIdoEd   = (CEdit*)     GetDlgItem(IDC_IDOEREDMENY_E) ;

  DWORD    dwNvzLstSor ;
  unsigned uiKulcs ;
  CRBBejegyzes *pRBBej ;
  CNvzSor* pNvzSorAdat ;
  char pszSeged[21] ;
	int nKivSor ;

  nKivSor = pEredmLb->GetCurSel() ;

  // Ha nincs semmi kiszelektalva azzal ne foglalkozzunk
	if ( nKivSor != LB_ERR )
  {

    dwNvzLstSor = pEredmLb->GetItemData( nKivSor) ;
    uiKulcs     = (unsigned)dwNvzLstSor ;

    if ( m_cAktNvzLst.Lookup( uiKulcs, pNvzSorAdat) )
    {
      // A helyezes
      sprintf( pszSeged, "%d", nKivSor+1);
      pHelyezEd->SetWindowText(pszSeged) ;

      // A rajtszam            pRBBej->Rajtsz()
      sprintf( pszSeged, "%d", pNvzSorAdat->uiRajtszam) ;
      pRszEd->SetWindowText(pszSeged) ;

      // A Cb-ok megfelelo soranak kivalasztasa
	    CbSorKiv( pVzoCb  , DWORD(pNvzSorAdat->uiVersenyzo)) ;
	    CbSorKiv( pNemzCb , DWORD(pNvzSorAdat->uiNemz)) ;
	    CbSorKiv( pCsapCb , DWORD(pNvzSorAdat->uiCsapat)) ;
	    CbSorKiv( pMotorCb, DWORD(pNvzSorAdat->uiMotor)) ;

      // Az idoeredmeny kiirasa
	    if ( m_cAktRsBef.Lookup( uiKulcs, pRBBej) == TRUE )
      {
	      pIdoEd->SetWindowText( pRBBej->IdoerS()) ;
      }
      else
      {
        pIdoEd->SetWindowText( "???") ;
      }
    }
  }
}

void CRsBefFelv::OnFelvesz()
{
  unsigned uiHelyez ;
  CNvzSor* pNvzSor     ;
  CNvzSor* pKamuNvzSor ;
  CRBBejegyzes* pRBBej ;
  unsigned      uiNvzlKulcs ;

  long nRCNvzl, nRCRsBfFlv ;

  pNvzSor = new CNvzSor() ;
  pRBBej  = new CRBBejegyzes() ;

  if ( pNvzSor == NULL || pRBBej == NULL )
    return ;

  if ( RsBefSorBeo( uiHelyez, pNvzSor, pRBBej) == TRUE )
  {
    // A beirt sor szintaktikailag helyes,
    // Ez az ellenorzes itt kell, mert csak folytonosan lehet felvenni, a
    // Ugyan a CRajtsBef::Felvesz()-ben ellenorizve van, de e fv. hivasa
    // elott meg kell hivni -t, ami ezzel a vizsgalattal esetleg feleslegesse
    // elkerulheto
    if ( uiHelyez <= (unsigned)m_cAktRsBef.Meret()+1 )
    {
      // Van-e ilyen nevezesi lista sor ?
      // (Ha nem lenne, felveszi, de csak az adatbazisba, a map-be NEM.)
      nRCNvzl = m_cAktNvzLst.SQL_NvzlKulcsMeghat( uiNvzlKulcs, m_uiNdNevKulcs, pNvzSor) ;
      
      // 0 - OK, 1 - ujat vett fel
      if ( nRCNvzl < 0 )
      {
        HibaKiir( nRCNvzl) ;

        delete pNvzSor ;
        delete pRBBej  ;

        return ;
      }

      // Felvetel csak a lista belsejebe ill. pont a vegere lehetseges:
      // lyukak nem lehetnek !
      nRCRsBfFlv = m_cAktRsBef.Felvesz( m_enRajtsBef, uiHelyez, uiNvzlKulcs, pRBBej) ;

      if ( nRCRsBfFlv != 0 )
      {
        HibaKiir( nRCRsBfFlv) ;

        delete pNvzSor ;
        delete pRBBej  ;

        return ;
      }

      m_bAktRsBefMentve = FALSE ;

      // Mostmar minden rendben lezajlott,
      // Uj lehet a nevezesi lista kulcs :
      // az adatbazisban vagy a szezonban
      if ( nRCNvzl == 1 )
      {
        m_cAktNvzLst.SetAt( uiNvzlKulcs, pNvzSor) ;
        m_bAktNvzLstMentve = FALSE ;
      }
      else
      {
        // Ha a rs/bef ablakon keresztul veszunk fel nevezesi lista sort,
        // es az mar masik szezonban szerepel, m_cAktNvzLst ures AktRBKiir()
        // nem irja ki a sort, ezert :
        if ( m_cAktNvzLst.Lookup( uiNvzlKulcs, pKamuNvzSor) == FALSE )
        {
          m_cAktNvzLst.SetAt( uiNvzlKulcs, pNvzSor) ;
          m_bAktNvzLstMentve = FALSE ;
        }
        else
        {
          delete pNvzSor ;
        }
      }

      AktRBKiir( uiNvzlKulcs) ;

      // Van-e importalas folyamatban ?
      if ( m_nImpKezdoSor != KEZDOSOR )
      {
        CString cCimke, cAblSzov ;

        cCimke.LoadString( AFX_IDS_APP_TITLE) ;
        cAblSzov.LoadString( IDS_FOLY_IMP_S)  ;

        if(MessageBox(cAblSzov,cCimke,MB_YESNO|MB_ICONQUESTION)==IDYES)
        {
          OnImportal() ;
        }
        else
        {
          m_nImpKezdoSor = KEZDOSOR ;
        }
      }

      // A beolvasott elemek altal foglalt helyet nem kell felszabaditani
      return ;
    } // Korrekt-e a beirt helyezes
  } // RsBefSorBeo()

  delete pNvzSor ;
  delete pRBBej  ;
}

// A rajtszammal, versenyzonevvel nem foglalkozik, mert igy pl. egy versenyzo
// tobb rsz-mal indulna egy nagydijon.
void CRsBefFelv::OnModosit()
{
  CListBox * pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB) ;

  unsigned uiUjHelyez, uiRegiHelyez ;

  CNvzSor* pNvzSor     ;
  CNvzSor* pKamuNvzSor ;
  CRBBejegyzes* pUjBejegyzes ;

  unsigned uiRegiNvzlKulcs ;
  unsigned uiUjNvzlKulcs   ;

  int nKivSor ;

  long nRCNvzl, nRCUPDATE ;

  nKivSor = pEredmLb->GetCurSel() ;

  if ( nKivSor == LB_ERR || nKivSor > 255 )
    return ;

  uiRegiHelyez = (unsigned char) nKivSor + 1 ;
  uiRegiNvzlKulcs = pEredmLb->GetItemData( nKivSor) ;

  pNvzSor = new CNvzSor() ;
  pUjBejegyzes = new CRBBejegyzes() ;

  if ( pNvzSor == NULL || pUjBejegyzes == NULL )
    return ;

  if ( RsBefSorBeo( uiUjHelyez, pNvzSor, pUjBejegyzes) )
  {
    if ( uiUjHelyez <= (unsigned)m_cAktRsBef.Meret()+1 )
    {
      // Van-e ilyen nevezesi lista sor ?
      // (Ha nem lenne, felveszi, de csak az adatbazisba, a map-be NEM.)
      nRCNvzl = m_cAktNvzLst.SQL_NvzlKulcsMeghat( uiUjNvzlKulcs, m_uiNdNevKulcs, pNvzSor) ;

      // 0 - OK, 1 - ujat vett fel
      if ( nRCNvzl < 0 )
      {
        HibaKiir( nRCNvzl) ;

        delete pNvzSor ;
        delete pUjBejegyzes  ;

        return ;
      }

      nRCUPDATE = m_cAktRsBef.Modosit( m_enRajtsBef, uiRegiHelyez,
                                       uiUjHelyez, uiUjNvzlKulcs,
                                       pUjBejegyzes ) ;

      if ( nRCUPDATE == 0 )
      {
        // Nem volt utkozes sem versenyzovel, sem rajtszammal
        // Az uj nevezesi lista kulcsot fel lehet venni a map-be is
        if ( nRCNvzl == 1 )
        {
          m_cAktNvzLst.SetAt( uiUjNvzlKulcs, pNvzSor) ;
          m_bAktNvzLstMentve = FALSE ;
        }
        else
        {
          // Ha a rs/bef ablakon keresztul veszunk fel nevezesi lista sort,
          // es az mar masik szezonban szerepel, m_cAktNvzLst ures AktRBKiir()
          // nem irja ki a sort, ezert :
          if ( m_cAktNvzLst.Lookup( uiUjNvzlKulcs, pKamuNvzSor) == FALSE )
          {
            m_cAktNvzLst.SetAt( uiUjNvzlKulcs, pNvzSor) ;
            m_bAktNvzLstMentve = FALSE ;
          }
          else
          {
            delete pNvzSor ;
          }
        }

        m_bAktRsBefMentve = FALSE ;

        // A leggyorsabb kort is erintheti a modositas az adatbazisban (!)
        // m_cAktRsBef.Modosit()->SQL_RajtsBefUPDATE() Ezert :
        if ( uiRegiNvzlKulcs == m_cAktRsBef.m_cCLeggyKor.Nvzl() &&
             uiRegiNvzlKulcs != uiUjNvzlKulcs )
        {
          m_cAktRsBef.m_cCLeggyKor.NvzlBeall( uiUjNvzlKulcs) ;

          LegyKorKiir() ;
        }

        // Ezzel az LB ItemData is beallitodik
        AktRBKiir( uiUjNvzlKulcs) ;

        return ;
      }
      else
      {
        // mivel a hiba miatt a rajtsorrend-be nem kerul bele az ujonnan letrehozott
        // nevezesi lista sor, elerhetetlen bejegyzes kerult a nevezesi_lista tablaba !
        HibaKiir( nRCUPDATE) ;

        nRCNvzl = m_cAktNvzLst.SQL_NvzlKlcsDELETE( uiUjNvzlKulcs) ;

        HibaKiir( nRCNvzl) ;

        delete pNvzSor ;
        delete pUjBejegyzes  ;
      }

      return ;
    }//if ( RsBefSorBeo ???
  }

  delete pNvzSor ;
  delete pUjBejegyzes ;
}

void CRsBefFelv::OnTorol()
{
  CListBox* pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB) ;
  int       nKivSor ;
  unsigned  uiNvzlKulcs ;
  long      nRC ;

  nKivSor = pEredmLb->GetCurSel() ;

  // Csak, ha van kivalasztott elem
  if ( nKivSor != LB_ERR )
  {
    uiNvzlKulcs = (unsigned)pEredmLb->GetItemData( nKivSor) ;

    nRC = m_cAktRsBef.Torol( m_enRajtsBef, nKivSor+1) ;
    if ( nRC == 0 )
    {
      m_bAktRsBefMentve = FALSE ;

      // Ha a leggyorsabb korben is szerepelt a versenyzo :
      // nem foglalkozunk vele, mert
      // 1. a rajtsorrend bejegyzest nem lehet torolni
      // 2. befutonal szerepelhet lgy.korben, ha nem futott be

      AktRBKiir( 0) ;
    }
    else
    {
      HibaKiir( nRC) ;
    }
  }
}

// Ennek nincs tul sok gyakorlati jelentosege
void CRsBefFelv::OnRbFelvesz()
{
  CListBox  *pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB)      ;

  CEdit     *pHelyezEd = (CEdit*)    GetDlgItem(IDC_HELYEZES_E)    ;
  CEdit     *pRszEd    = (CEdit*)    GetDlgItem(IDC_RSZ_E)         ;

  CComboBox *pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZO_CB)  ;
  CComboBox *pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZETISEG_CB) ;
  CComboBox *pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)     ;
  CComboBox *pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)      ;

  CEdit     *pIdoEd   = (CEdit*)     GetDlgItem(IDC_IDOEREDMENY_E) ;

  long nRC ;

	if ( m_bAktNvzLstMentve == FALSE || m_bAktRsBefMentve == FALSE )
	{
    // Ne mentsen foloslegesen ...
    if ( (nRC=SQL_COMMIT()) != 0 )
    {
      HibaKiir( nRC) ;

      return ;
    }

    // A mentes utan a nevezesi lista mentett lett
    m_bAktRsBefMentve  = TRUE ;
    m_bAktNvzLstMentve = TRUE ;

    m_cAktRsBef.Kiurit() ;
  }

  pEredmLb->ResetContent() ;

  pHelyezEd->SetWindowText("") ;
  pRszEd->SetWindowText("") ;
  pRszEd->SetFocus() ;
  
  pVzoCb->SetCurSel( -1)   ;
  pNemzCb->SetCurSel( -1)  ;
  pCsapCb->SetCurSel( -1)  ;
  pMotorCb->SetCurSel( -1) ;
}

void CRsBefFelv::OnRbTorol()
{
  CListBox  *pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB)      ;

  CEdit     *pHelyezEd = (CEdit*)    GetDlgItem(IDC_HELYEZES_E)    ;
  CEdit     *pRszEd    = (CEdit*)    GetDlgItem(IDC_RSZ_E)         ;

  CComboBox *pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZO_CB)  ;
  CComboBox *pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZETISEG_CB) ;
  CComboBox *pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)     ;
  CComboBox *pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)      ;

  CEdit     *pIdoEd   = (CEdit*)     GetDlgItem(IDC_IDOEREDMENY_E) ;

  CStatic   *pLGyKor  = (CStatic*)   GetDlgItem(IDC_LEGGY_KOR) ;

  long nRC ;

/*
    if ( m_cAktRsBef.Meret() == 0 && m_cAktRsBef.m_cCLeggyKor.Ures() )
      return ;
*/
  nRC = m_cAktRsBef.SQL_RajtsBefDELETE( m_enRajtsBef) ;

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;

    return ;
  }

  // Az aktualis R/B torlese
  m_cAktRsBef.Kiurit() ;
  m_bAktRsBefMentve = TRUE ;

  // Inkabb, mint a *Kiir fuggvenyek :
  pEredmLb->ResetContent() ;

  pHelyezEd->SetWindowText("") ;
  pRszEd->SetWindowText("") ;
  pRszEd->SetFocus() ;
  
  pVzoCb->SetCurSel( -1)   ;
  pNemzCb->SetCurSel( -1)  ;
  pCsapCb->SetCurSel( -1)  ;
  pMotorCb->SetCurSel( -1) ;

  if ( pLGyKor != NULL )
  {
    pLGyKor->SetWindowText("") ;
  }
}

void CRsBefFelv::OnNevek()
{
// TRACE("\nCRsBefFelv::OnNevek()") ;
}
// mi t., ha a R/B felvetel soran a nvlst szezon modosul, de esetleg az R/B
// nem lett megsem felveve stb. ?
// A kettonek tkeppen egyutt kene valtoznia (kiveve egy R/B sor torlese,
// amikoris a nvzlst nem feltetlen modosul)

void CRsBefFelv::OnCancel()
{
  long nRC ;
  CString sAblCim, sSzov ;

  if ( m_bAktNvzLstMentve == FALSE || m_bAktRsBefMentve == FALSE )
  {
    GetWindowText( sAblCim) ;

    if ( m_enRajtsBef == RAJTS )
    {
      if ( sSzov.LoadString(IDS_RS_MEGV) != TRUE )
      {
        sSzov = "IDS_RS_MEGV" ;
      }
    }
    else
    {
      if ( sSzov.LoadString(IDS_BEF_MEGV) != TRUE )
      {
        sSzov = "IDS_BEF_MEGV" ;
      }
    }

    if ( IDYES==MessageBox( sSzov, sAblCim, MB_ICONQUESTION | MB_YESNO ) )
	  {
      nRC = SQL_COMMIT() ;
      if ( nRC != 0 )
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
/*
    char pszDumpFile[31] ;
    sprintf(pszDumpFile, "c:\\forma1\\%d.dmp", m_cAktSzez.m_usSzezon) ;
    m_cAktSzez.CSzezonDump(CString(pszDumpFile)) ;
*/

  // Ismet fel lehet hivni ezt az ablakot, mert nem lett CRsBefFelv felhivva
  gl_bFelvitelRsBefAktiv = TRUE ;

  // Ha a CImportGyAbl-ot kezzel becsukjak CRsBefFelv::On~ elott ne legyen baj
  if ( m_pImportAbl != NULL )
  {
    CMDIFrameWnd* pMainWnd = (CMDIFrameWnd*)AfxGetApp()->m_pMainWnd ;

    pMainWnd->MDIActivate( m_pImportAbl) ;

    if ( pMainWnd->GetActiveFrame() == m_pImportAbl )
      m_pImportAbl->MDIDestroy() ;
  }

  DestroyWindow() ;

//  CDialog::OnCancel(); amodalis dialogusablak eseteben tilos meghivni
}

void CRsBefFelv::OnOK()
{
  long nRC ;

  if ( m_bAktNvzLstMentve == FALSE || m_bAktRsBefMentve == FALSE )
  {
    // Csak, ha az aktualis rs/bef ill. a nevezesi lista mentetlen
    nRC=SQL_COMMIT() ;
    if ( nRC != 0 )
    {
      HibaKiir( nRC) ;
    }

    if ( m_pImportAbl != NULL )
    {
      CMDIFrameWnd* pMainWnd = (CMDIFrameWnd*)AfxGetApp()->m_pMainWnd ;

      pMainWnd->MDIActivate( m_pImportAbl) ;

      if ( pMainWnd->GetActiveFrame() == m_pImportAbl )
        m_pImportAbl->MDIDestroy() ;
    }
  }

  // Ismet fel lehet hivni ezt az ablakot, mert nem lett CRsBefFelv felhivva
  gl_bFelvitelRsBefAktiv = TRUE ;

  DestroyWindow() ;
}

void CRsBefFelv::OnLeggyKor()
{
  CStatic* pStatic = (CStatic*) GetDlgItem(IDC_RB_NEV_S) ;

  char pszGrpBoxSzov[30] ;
  CString cSzov ;

  POSITION poz ;
  unsigned uiKulcs ;
  unsigned char ucTalRsz  ;
  CRBBejegyzes  cTalRBBej ;
  CRBBejegyzes* pRBBej ;
  nevadat *pNdNevadat ;
  CString sSzov ;

  long nRC ;

  CLegyKorFlv cLKFDlg( m_cAktRsBef.m_uiSzezEve, m_uiNdNevKulcs, &m_cAktNvzLst, this) ;

  cLKFDlg.m_cLeggyKor = m_cAktRsBef.m_cCLeggyKor ;

  // A fele pontszam:az elso helyezettbol allapitom meg(mindre ua. kell legyen)
  if ( m_cAktRsBef.KerPozRsz( 1, ucTalRsz, cTalRBBej) == TRUE )
  {
    if ( (cTalRBBej.Megj()&FELE_PONT) == FELE_PONT )
      cLKFDlg.m_bFelePont = TRUE ;
  }

  if ( cLKFDlg.DoModal() == IDOK )
  {
    m_cAktRsBef.m_cCLeggyKor = cLKFDlg.m_cLeggyKor ;

    // A fele pontszam torleset / beallitasat figyelembe vesszuk
    poz = m_cAktRsBef.GetStartPosition() ;
    while( poz != NULL)
    {
      m_cAktRsBef.GetNextAssoc( poz, uiKulcs, pRBBej) ;

      if ( cLKFDlg.m_bFelePont == TRUE )
      {
        pRBBej->MegjBeall(pRBBej->Megj() | FELE_PONT) ;
      }
      else
      {
        pRBBej->MegjBeall(pRBBej->Megj() & ~FELE_PONT) ;
      }
    }

    // A fent modositott pRBBej->Megj() adatokat irja be az adatbazisba
    nRC = m_cAktRsBef.SQL_FelePontBeall( m_enRajtsBef, cLKFDlg.m_bFelePont) ;

    if ( nRC != 0 )
    {
      HibaKiir( nRC) ;
    }
    
    // A fele pontszam torleset / beallitasat kiirjuk
    if (((CProba13App*)AfxGetApp())->m_cNagydijNev.Lookup( m_uiNdNevKulcs, pNdNevadat))
    {
      if ( cLKFDlg.m_bFelePont == TRUE )
      {
        if ( sSzov.LoadString( IDS_FELE_PONT_SZOV) != TRUE )
        {
          sSzov = "IDS_FELE_PONT_SZOV" ;
        }

        sprintf(pszGrpBoxSzov, "%d - %s - %s", m_cAktRsBef.m_uiSzezEve,
                                               (const char *)pNdNevadat->m_sNev,
                                               (const char *)sSzov) ;
      }
      else
      {
        sprintf(pszGrpBoxSzov, "%d - %s",
                m_cAktRsBef.m_uiSzezEve, (const char *)pNdNevadat->m_sNev) ;
      }
    }

    if ( pStatic != NULL )
    {
      pStatic->SetWindowText( pszGrpBoxSzov) ;
    }

    // Nem t. eld., hogy volt-e valtozas, ezert :
    m_bAktRsBefMentve = FALSE ;

    LegyKorKiir() ;
  }
}

void CRsBefFelv::OnBeszamit()
{
  CListBox * pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB) ;

  int           nAktSor ;
  unsigned      uiRBKulcs ;
  CRBBejegyzes* pRBBej  ;
  unsigned      uiMegj  ;
  long          nRC     ;

  nAktSor = pEredmLb->GetCurSel() ;

  if ( nAktSor != LB_ERR )
  {
    CEreBeszFlv cErBeszDlg ;

    uiRBKulcs = (unsigned)pEredmLb->GetItemData(nAktSor) ;
    if ( !m_cAktRsBef.Lookup( uiRBKulcs, pRBBej) )
      return ;

    uiMegj = pRBBej->Megj() ;

    if ( uiMegj & CSAPAT_DISZKV )
    { 
      cErBeszDlg.m_bCsapat = FALSE ;
    }
    else
    {
      cErBeszDlg.m_bCsapat = TRUE ;
    }

    if ( uiMegj & VERSENYZO_DISZKV )
    { 
      cErBeszDlg.m_bVersenyzo = FALSE ;
    }
    else
    {
      cErBeszDlg.m_bVersenyzo = TRUE ;
    }

    if ( cErBeszDlg.DoModal() == IDOK )
    {
      if ( cErBeszDlg.m_bCsapat == TRUE )
      {
        //        bitek megforditva
        uiMegj &= CSAPAT_DISZKV ^ 0xffff ;
      }
      else
      {
        uiMegj |=  CSAPAT_DISZKV ;
      }

      if ( cErBeszDlg.m_bVersenyzo == TRUE )
      {
        //        bitek megforditva
        uiMegj &= VERSENYZO_DISZKV ^ 0xffff ;
      }
      else
      {
        uiMegj |=  VERSENYZO_DISZKV ;
      }

      if ( pRBBej->Megj() != uiMegj )
      {
        pRBBej->MegjBeall( uiMegj) ;

        nRC = m_cAktRsBef.SQL_MegjUPDATE( m_enRajtsBef, uiRBKulcs, uiMegj) ;

        if ( nRC != 0 )
        {
          HibaKiir( nRC) ;
        }
        else
        {
          m_bAktRsBefMentve = FALSE  ;
        }
      }
    }
  }
}

void CRsBefFelv::OnChangeRsz()
{
  CComboBox* pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZO_CB)  ;
  CComboBox* pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZETISEG_CB) ;
  CComboBox* pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)     ;
  CComboBox* pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)      ;
  CEdit*     pRszEd   = (CEdit*)     GetDlgItem(IDC_RSZ_E)         ;

  CString  sBeoRsz ;
  int      nBeoRsz ;
  unsigned uiRsz   ;
  unsigned uiNvzlstKulcs ;
  CNvzSor* pNvzLstSor ;

  long nRC ;
    
  pRszEd->GetWindowText(sBeoRsz) ;
  sBeoRsz = sBeoRsz.SpanIncluding("0123456789") ;

  if ( sBeoRsz.GetLength() > 0 )
  {
    // Szintaktikailag ervenyes a beirt rajtszam
    nBeoRsz = atoi((const char*) sBeoRsz) ;

    // A tipuskonverzio miatt... 0 <= helyett
    if ( -1 < nBeoRsz )
    {
      uiRsz = nBeoRsz ;
    }
    else
    {
      return ;
    }

    nRC = m_cAktNvzLst.SQL_RszNdBolNvzl( uiRsz, m_uiNdNevKulcs, uiNvzlstKulcs) ;

    // Ha van talalat, csak akkor tud felkinalni valamit
    if ( nRC == 0 )
    {
      if ( m_cAktNvzLst.Lookup( uiNvzlstKulcs, pNvzLstSor) == TRUE )
      {
        CbSorKiv( pVzoCb  , DWORD(pNvzLstSor->uiVersenyzo) ) ;
        CbSorKiv( pNemzCb , DWORD(pNvzLstSor->uiNemz)      ) ;
        CbSorKiv( pCsapCb , DWORD(pNvzLstSor->uiCsapat)    ) ;
        CbSorKiv( pMotorCb, DWORD(pNvzLstSor->uiMotor)     ) ;
      }
      else
      {
        // Ha nem talalja ezt a nevezesi lista kulcsot -> Cb-k torlese
        CbSorKiv( pVzoCb  , DWORD( 0) ) ;
        CbSorKiv( pNemzCb , DWORD( 0) ) ;
        CbSorKiv( pCsapCb , DWORD( 0) ) ;
        CbSorKiv( pMotorCb, DWORD( 0) ) ;
      }
    }
    else
    {
      // -1405 fetched column value is NULL
      if ( nRC == -1405 )
      {
        // Ha egyaltalan nem talal nevezesi lista kulcsot -> Cb-k torlese
        CbSorKiv( pVzoCb  , DWORD( 0) ) ;
        CbSorKiv( pNemzCb , DWORD( 0) ) ;
        CbSorKiv( pCsapCb , DWORD( 0) ) ;
        CbSorKiv( pMotorCb, DWORD( 0) ) ;
      }
      else
      {
        HibaKiir( nRC) ;
      }
    }
  }
}

void CRsBefFelv::OnImportal()
{
  CEdit     *pHelyezEd = (CEdit*)    GetDlgItem(IDC_HELYEZES_E)    ;
  CEdit     *pRszEd    = (CEdit*)    GetDlgItem(IDC_RSZ_E)         ;

  CComboBox *pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZO_CB)  ;
  CComboBox *pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZETISEG_CB) ;
  CComboBox *pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)     ;
  CComboBox *pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)      ;

  CEdit     *pIdoEd   = (CEdit*)     GetDlgItem(IDC_IDOEREDMENY_E) ;

  unsigned uiVnevKlcs ;   BOOL bVnevTal ;
  unsigned uiNemzKlcs ;   BOOL bNemzTal ;
  unsigned uiCsapKlcs ;   BOOL bCsapTal ;
  unsigned uiMotKlcs  ;   BOOL bMotTal  ;
                          BOOL bIdoerOk ;
  unsigned uiNvzLstKlcs ;
                          BOOL bRszOk   ;

  int  nRsz, nSorokSzama, nAktSor, nElvPoz ;
  unsigned uiRsz ;
  CNvzSor* pNvzLstSor ;
  CRBBejegyzes* pRBBej, RBBej ;
  CString sAktSor, sBeoIdoer, cDebug ;
  char pszSegedStr[18] ;

  long nRC ;

  if ( m_pImportAbl == NULL )
  {
    m_pImportAbl = new CImportGyAbl() ;

    if ( m_pImportAbl == NULL )
      return ;

    m_pImportAbl->LoadFrame( IDR_IMPORTALAS,
                             WS_OVERLAPPEDWINDOW|WS_VISIBLE|WS_VSCROLL|WS_MAXIMIZE|FWS_ADDTOTITLE,
                             AfxGetMainWnd() ) ;

    if ( ImportFileBeo() == FALSE )
      return ;
  }

  // A beolvasott adatokbol megprobalja kihamozni a neveket es eredmenyeket
  // A SAJAT EREDMENYFILE-JAIMRA VAN SPECIALIZALVA
  // (EGYEB FILE ESETEN UJRA KELL IRNI)

  nSorokSzama = m_pImportAbl->m_Sorok.GetUpperBound() ;

  for ( nAktSor = m_nImpKezdoSor ; nAktSor <= nSorokSzama ; nAktSor++ )
  {
    m_pImportAbl->SorInvertal( nAktSor) ;

    // Lokalis valt., hogy ne kelljen az operatort annyit hivogatni...
    sAktSor = m_pImportAbl->m_Sorok[nAktSor] ;

    if ( UresSor( sAktSor) )
      continue ;

    nRsz = atoi(m_pImportAbl->m_Sorok[nAktSor].Mid( 3, 2)) ;

    if ( nRsz > -1 )
    {
      uiRsz  = (unsigned) nRsz ;
      bRszOk = TRUE ;
    }
    else
    {
//      A rsz hibas, ra kell kerdezni a felhasznalonal
      bRszOk = FALSE ;
      pRszEd->SetFocus() ;
      pRszEd->SetSel( 0, -1) ;
    }

//TRACE("\n------------------------------") ;
    bVnevTal = m_pVersenyzo->NevbolKulcs(sAktSor.Mid( 6, 20), uiVnevKlcs) ;
/*
cDebug = bVnevTal ? "TALALTA" : "NEM TALALTA" ;
TRACE("\n%s : %d ", (const char*) sAktSor.Mid( 6, 20), wVnevKlcs) ;
TRACE(" %s",  (const char*) cDebug) ;
*/
    if ( bVnevTal == FALSE )
    {
      CVNevFelv VNevFelvAbl ;
      // Erdemes lenne esetleg a sztringet, amivel nem boldogult atadni az ablaknak
      VNevFelvAbl.DoModal() ;

      // Az esetlegesen ujonnan felvett nevet szeretnenk viszontlatni a ComboBox-ban
      if ( VNevFelvAbl.m_bModositva == TRUE )
        VNevCbFeltolt( IDC_VERSENYZO_CB, m_pVersenyzo) ;

      uiVnevKlcs = VNevFelvAbl.m_uiKivNevKulcsa ;
    }


    bNemzTal = m_pNemz->NevbolKulcs(sAktSor.Mid( 27, 3), uiNemzKlcs) ;
/*
cDebug = bNemzTal ? "TALALTA" : "NEM TALALTA" ;
TRACE("\n%s : %d", (const char*) sAktSor.Mid( 27, 3), wNemzKlcs) ;
TRACE(" %s",  (const char*) cDebug) ;
*/
    if ( bNemzTal == FALSE )
    {
      CNevFelv NevFelvAbl( eNemzNt) ;
      // Erdemes lenne esetleg a sztringet, amivel nem boldogult atadni az ablaknak
      NevFelvAbl.DoModal() ; 

      //Az esetlegesen ujonnan felvett nevet szeretnenk viszontlatni a ComboBox-ban
      if ( NevFelvAbl.m_bModositva == TRUE )
        NevCbFeltolt( IDC_NEMZETISEG_CB, m_pNemz) ;

      uiNemzKlcs = NevFelvAbl.m_uiKivNevKulcsa ;
    }

    nElvPoz = sAktSor.Mid( 34, 20).Find(" ") ;

    bCsapTal = m_pCsapat->NevbolKulcs(sAktSor.Mid( 34, nElvPoz), uiCsapKlcs) ;
/*
cDebug = bCsapTal ? "TALALTA" : "NEM TALALTA" ;
TRACE("\n%s : %d", (const char*) sAktSor.Mid( 34, nElvPoz), wCsapKlcs) ;
TRACE(" %s",  (const char*) cDebug) ;
*/
    if ( bCsapTal == FALSE )
    {
      CNevFelv NevFelvAbl( eCsapNt) ;
      // Erdemes lenne esetleg a sztringet, amivel nem boldogult atadni az ablaknak
      NevFelvAbl.DoModal() ;

      if ( NevFelvAbl.m_bModositva == TRUE )
        NevCbFeltolt( IDC_CSAPAT_CB, m_pCsapat) ;

      uiCsapKlcs = NevFelvAbl.m_uiKivNevKulcsa ;
    }

    bMotTal = m_pMotor->NevbolKulcs(sAktSor.Mid( 34+nElvPoz, 20-nElvPoz), uiMotKlcs) ;
//cDebug = bMotTal ? "TALALTA" : "NEM TALALTA" ;
//TRACE("\n%s : %d", (const char*) sAktSor.Mid( 34+nElvPoz, 20-nElvPoz), wMotKlcs) ;
//TRACE(" %s",  (const char*) cDebug) ;
    if ( bMotTal == FALSE )
    {
      CNevFelv NevFelvAbl( eMotorNt) ;
      // Erdemes lenne esetleg a sztringet, amivel nem boldogult atadni az ablaknak
      NevFelvAbl.DoModal() ;

      if ( NevFelvAbl.m_bModositva == TRUE )
        NevCbFeltolt( IDC_MOTOR_CB, m_pMotor) ;

      uiMotKlcs = NevFelvAbl.m_uiKivNevKulcsa ;
    }

    // IDOEREDMENY
    // ha a sor rovidebb, mint 56 megint ASSERTION FAILED lesz !
    if ( sAktSor.GetLength() >= 56 )
    {
      sBeoIdoer = sAktSor.Right( sAktSor.GetLength() - 54) ;

      bIdoerOk  = StrBolIdoEr( sBeoIdoer, &RBBej) ;
      if ( !bIdoerOk )
      {
        pIdoEd->SetWindowText( sBeoIdoer) ;
        pIdoEd->SetFocus() ;
        pIdoEd->SetSel( 0, -1) ;

        bIdoerOk = FALSE ;
      }
      else
      {
//TRACE("\nIdoeredm (visszaal.):%s", (const char*) RBBej.IdoerS()) ;
      }
    }
    else
    {
      pIdoEd->SetFocus() ;
      pIdoEd->SetSel( 0, -1) ;
      bIdoerOk = FALSE ;
    }

    // Ha van rajtszam, akkor lehet a nvzl-val probalkozni
    if ( bRszOk == TRUE )
    {
      nRC = m_cAktNvzLst.SQL_RszNdBolNvzl( uiRsz, m_uiNdNevKulcs, uiNvzLstKlcs) ;

      if ( nRC == 0 )
      { 
        // Ha mar talalt uiNvzLstKlcs-t, biztos, hogy van hozza pNvzLstSor
        m_cAktNvzLst.Lookup( uiNvzLstKlcs, pNvzLstSor) ;

        if ( bVnevTal && bNemzTal && bCsapTal && bMotTal )
        {
//TRACE("\nTalalt NvzLstSort") ;
          if ( pNvzLstSor->uiVersenyzo == uiVnevKlcs &&
               pNvzLstSor->uiNemz      == uiNemzKlcs &&
               pNvzLstSor->uiCsapat    == uiCsapKlcs &&
               pNvzLstSor->uiMotor     == uiMotKlcs )
          {
//TRACE("\nNvzLstSor egyezik a beolvasottakkal") ;
            if ( bIdoerOk == TRUE && bRszOk == TRUE )
            {
               pRBBej = new CRBBejegyzes( RBBej) ;
               if ( pRBBej == NULL )
                 return ;

              //  bRszOk == TRUE => nAktSor < 256 \/
              // Felvetel csak a lista belsejebe ill. pont a vegere lehetseges:
              // lyukak nem lehetnek !
              nRC = m_cAktRsBef.Felvesz( m_enRajtsBef,
                                        (unsigned char) nAktSor-1,
                                        uiNvzLstKlcs,
                                        pRBBej) ;

              if ( nRC == 0 )
              {
                AktRBKiir( (DWORD) uiNvzLstKlcs) ;

                continue ;
              }
              else
              {
                // Mivel pRBBej-t nem vette fel, fel kell szabaditani
                delete pRBBej ;

                HibaKiir( nRC) ;
              }
            }
          } // A talalt nevezesi lista sor nem egyezik a beolvasottakkal \/
        } // Nem talalt nevezesi lista sort \/
      }
      else
      {
        HibaKiir( nRC) ;
      }
    }// Nincs rajtszam => nem tud nevezesi listaval probalkozni \/

    _itoa( nAktSor-1, pszSegedStr, 10) ;
    pHelyezEd->SetWindowText( pszSegedStr) ;

    _itoa( uiRsz, pszSegedStr, 10) ;
    pRszEd->SetWindowText( pszSegedStr) ;

    CbSorKiv( pVzoCb, DWORD(uiVnevKlcs)) ;
    CbSorKiv( pNemzCb, DWORD(uiNemzKlcs)) ;
    CbSorKiv( pCsapCb, DWORD(uiCsapKlcs)) ;
    CbSorKiv( pMotorCb, DWORD(uiMotKlcs)) ;

    pIdoEd->SetWindowText( RBBej.IdoerS()) ;

    m_nImpKezdoSor = nAktSor + 1 ;

    // Hogy a felhasznalo valtozatathasson a felkinalt soron
    return ;
  } // for : vegig a sorokon

  m_nImpKezdoSor = KEZDOSOR ;
}

BOOL CRsBefFelv::ImportFileBeo()
{
  UINT uiBeoMer, uiIdx, uiTol, uiSorVeg, nDbgBeoSor = 0 ;

  const UINT uiBuffMer = 100 ; // unsigned int ? UINT 16 bit
  char buff[uiBuffMer] ;
  // ez ugyan problemas lehet, de ilyen hosszu v. ennel sokkal hosszabb sornak
  // ugysincs ertelme; nem valoszinu, hogy elofordul, ha igen levagja a tobbit
  char pszEgySor[255] ;
  char cRegiBuffUt = '\0' ;
  int nAktY = 0 ;

  CString cFileTip, sTeljesFilenev ;

  cFileTip.LoadString( IDS_MINDEN_FILE_TIP) ;
  // TRUE : File Open, FALSE : File Save As
  CFileDialog dlgLekerMent(TRUE, "*.*", NULL,
                  OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY,
                  (const char*) cFileTip);

  dlgLekerMent.m_ofn.lpstrInitialDir = sAlapkonyvtar ;

  if (dlgLekerMent.DoModal() != IDOK)
    return FALSE ;

  sTeljesFilenev = dlgLekerMent.GetPathName() ;

  if ( m_ImportFile.Open(sTeljesFilenev, CFile::modeRead) == 0 )
    return FALSE ;

  uiSorVeg = 0 ;

// TRACE("\nCRsBefFelv::ImportFileBeo()") ;

  while ( 0 != (uiBeoMer = m_ImportFile.Read( buff, uiBuffMer)) )
  {
    uiTol = 0 ;

    // Ha pl.: (regi blokk) 0d | 0a (uj blokk)
    uiIdx = 0 ;
    if ( buff[uiIdx] == 0x0a )
    {
      uiIdx++ ;

      if ( cRegiBuffUt == 0x0d )
      {
        // - 1 : felulirjuk a 0x0d-t
        pszEgySor[uiSorVeg - 1] = '\0' ;
        m_pImportAbl->m_Sorok.Add( pszEgySor) ;
        uiSorVeg = 0 ;

        // Atlepjuk a 0x0a-t
        uiTol = uiIdx ;
      }
    }

    while ( uiIdx < uiBeoMer - 1 )
    {
      if ( buff[uiIdx] == 0x0d && buff[uiIdx+1] == 0x0a )
      {
        if ( uiSorVeg + uiIdx - uiTol < 254 )
        {
          memcpy( pszEgySor+uiSorVeg, buff+uiTol, uiIdx-uiTol) ;

          uiSorVeg += uiIdx-uiTol ;

          pszEgySor[uiSorVeg] = '\0' ;

// TRACE("\n%d : %s", ++nDbgBeoSor, pszEgySor) ;

          m_pImportAbl->m_Sorok.Add( pszEgySor) ;
          uiSorVeg = 0 ;
        }
        else
        {
          // Bemasolunk annyit, amennyi max. befer
          memcpy( pszEgySor+uiSorVeg, buff+uiTol, 255-uiSorVeg-1) ;
          
          pszEgySor[255-1] = '\0' ;

// TRACE("\n%d : %s", ++nDbgBeoSor, pszEgySor) ;

          m_pImportAbl->m_Sorok.Add( pszEgySor) ;
          uiSorVeg = 0 ;
        }

        uiIdx += 2 ;
        uiTol = uiIdx ;
      }
      else
      {
        uiIdx++ ;
      }
    }

    // Eltarolom az utolso karaktert a bufferbol (regi blokk) 0d | 0a (uj blokk)
    cRegiBuffUt = buff[uiIdx] ;

    // A ENTER + UJ SOR -tol a buffer vegeig terjedo resz hozzaadasa
    if ( buff[uiIdx] != 0x0a )
      uiIdx++ ;

    if ( uiSorVeg + uiIdx - uiTol < 254 )
    {
      memcpy( pszEgySor+uiSorVeg, buff+uiTol, uiIdx-uiTol) ;

      uiSorVeg += uiIdx-uiTol ;
      uiTol = uiIdx + 2 ;

      pszEgySor[uiSorVeg] = '\0' ;
    }
    else
    {
      // Bemasolunk annyit, amennyi max. befer
      memcpy( pszEgySor+uiSorVeg, buff+uiTol, 255-uiSorVeg-1) ;

      pszEgySor[255-1] = '\0' ;
    }
  }

  m_ImportFile.Close() ;

  return TRUE ;
}

BOOL CRsBefFelv::NevCbFeltolt(int nComboBox, CNev* pKiirNev)
{
  POSITION poz     ;
  unsigned uiKulcs ;
  nevadat* pAdat   ;

  CComboBox* pComboBox = (CComboBox*) GetDlgItem( nComboBox) ;

  if ( pComboBox == NULL || pKiirNev == NULL )
    return FALSE ;

  pComboBox->ResetContent() ;

  poz = pKiirNev->GetStartPosition() ;
  while( poz != NULL)
  {
    pKiirNev->GetNextAssoc( poz, uiKulcs, pAdat ) ;
    pComboBox->SetItemData(pComboBox->AddString(pAdat->m_sNev), uiKulcs) ;
  }

  // A combo box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pComboBox->SetCurSel(0) ;

  return TRUE ;
}

BOOL CRsBefFelv::VNevCbFeltolt( int nComboBox, CVNev* pKiirVNev)
{
  POSITION  poz     ;
  unsigned  uiKulcs ;
  vnevadat* pVAdat  ;

  CComboBox* pComboBox = (CComboBox*) GetDlgItem( nComboBox) ;

  if ( pComboBox == NULL || pKiirVNev == NULL )
    return FALSE ;

  pComboBox->ResetContent() ;

  poz = pKiirVNev->GetStartPosition() ;
  while( poz != NULL)
  {
    pKiirVNev->GetNextAssoc( poz, uiKulcs, pVAdat ) ;
    pComboBox->SetItemData(pComboBox->AddString(pVAdat->m_sVezNev+", "+pVAdat->m_sKerNev), uiKulcs) ;
  }

  // A combo box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pComboBox->SetCurSel(0) ;

  return TRUE ;
}                                         

BOOL CRsBefFelv::CbSorKiv(CComboBox* pComboBox, DWORD dwKulcs)
{
  int nAktPoz ;
  int nElemszam ;

  nElemszam = pComboBox->GetCount() ;

  nAktPoz = 0 ;
  while ( nAktPoz < nElemszam )
  {
    if ( dwKulcs == pComboBox->GetItemData(nAktPoz) )
    {
      pComboBox->SetCurSel(nAktPoz) ;

      return TRUE ;
    }

    nAktPoz++ ;
  }

  // Torli a kivalasztast
  pComboBox->SetCurSel( -1) ;

  return FALSE ;
}

BOOL CRsBefFelv::RsBefSorBeo(unsigned& uiHelyez,
                             CNvzSor* pNvzSor, CRBBejegyzes* pRBBej)
{
  CEdit*     pHelyezEd = (CEdit*)     GetDlgItem(IDC_HELYEZES_E)    ;
  CEdit*     pRszEd    = (CEdit*)     GetDlgItem(IDC_RSZ_E)         ;

  CComboBox* pVzoCb    = (CComboBox*) GetDlgItem(IDC_VERSENYZO_CB)  ;
  CComboBox* pNemzCb   = (CComboBox*) GetDlgItem(IDC_NEMZETISEG_CB) ;
  CComboBox* pCsapCb   = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)     ;
  CComboBox* pMotorCb  = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)      ;

  CEdit*     pIdoEd    = (CEdit*)     GetDlgItem(IDC_IDOEREDMENY_E) ;

  POSITION      poz        ;
  unsigned      uiKulcs    ;
  CRBBejegyzes* pKamuRBBej ;

  CString sBeoHelyez, sBeoRsz, sBeoIdoer ;
  int nBeoErt, nKivElem ;

  // A helyezes beolvasasa
  pHelyezEd->GetWindowText(sBeoHelyez) ;
  sBeoHelyez = sBeoHelyez.SpanIncluding("0123456789") ;

  if ( sBeoHelyez.GetLength() > 0 )
  {
    // Szintaktikailag ervenyes a beirt helyezes
    nBeoErt = atoi((const char*) sBeoHelyez) ;

    // rajtsorrend.helyezes NUMBER(2)
    if ( 0 < nBeoErt && nBeoErt < 99 )
    {
      uiHelyez = (unsigned)nBeoErt ;
    }
    else
    {
      // A helyezes megadasa hibas nem tarolhato uc-ban: kijelolese
      pHelyezEd->SetFocus() ;
      pHelyezEd->SetSel( 0, -1 ) ;

      return FALSE ;
    }
  }
  else
  {
    // Nem lett ertelmes sztring megadva : hiba
    pHelyezEd->SetFocus() ;
    pHelyezEd->SetSel( 0, -1 ) ;

    return FALSE ;
  }

  // A rajtszam beolvasasa
  pRszEd->GetWindowText(sBeoRsz) ;
  sBeoRsz = sBeoRsz.SpanIncluding("0123456789") ;

  if ( sBeoRsz.GetLength() > 0 )
  {
    // Szintaktikailag ervenyes a beirt rajtszam
    nBeoErt = atoi((const char*) sBeoRsz) ;

    // nevezesi_lista.rajtszam NUMBER(3)
    if ( -1 < nBeoErt && nBeoErt < 256 )
    {
      pRBBej->RajtszBeall( (unsigned) nBeoErt) ;
      pNvzSor->uiRajtszam = (unsigned) nBeoErt ;
    }
    else
    {
      // A rajtszam megadasa hibas nem tarolhato uc-ban: kijelolese
      pRszEd->SetFocus() ;
      pRszEd->SetSel( 0, -1 ) ;

      return FALSE ;
    }
  }
  else
  {
    // Nem lett ertelmes sztring megadva : hiba
    pRszEd->SetFocus() ;
    pRszEd->SetSel( 0, -1 ) ;

    return FALSE ;
  }

  //-A-versenyzonev-beolvasasa----------------------------------------------
  nKivElem = pVzoCb->GetCurSel() ;
  if ( nKivElem == CB_ERR )
  {
    return FALSE ;
  }

  pNvzSor->uiVersenyzo = (unsigned)pVzoCb->GetItemData(nKivElem) ;
  //-A-versenyzo-nemzetisegenek-beolvasasa----------------------------------
  nKivElem = pNemzCb->GetCurSel() ;
  if ( nKivElem == CB_ERR )
  {
    return FALSE ;
  }

  pNvzSor->uiNemz = (unsigned)pNemzCb->GetItemData(nKivElem) ;
  //-A-csapat-beolvasasa----------------------------------------------------
  nKivElem = pCsapCb->GetCurSel() ;
  if ( nKivElem == CB_ERR )
  {
    return FALSE ;
  }

  pNvzSor->uiCsapat = (unsigned)pCsapCb->GetItemData(nKivElem) ;
  //-A-motor-beolvasasa-----------------------------------------------------
  nKivElem = pMotorCb->GetCurSel() ;
  if ( nKivElem == CB_ERR )
  {
    return FALSE ;
  }

  pNvzSor->uiMotor = (unsigned)pMotorCb->GetItemData(nKivElem) ;

  // Az idoeredmeny beolvasasa,
  // a megjegyzes meghatarozasa korabban felvett bejegyzesek alapjan
  pIdoEd->GetWindowText(sBeoIdoer) ;

  poz = m_cAktRsBef.GetStartPosition() ;
  if ( poz != NULL )
  {
    m_cAktRsBef.GetNextAssoc( poz, uiKulcs, pKamuRBBej) ;

    if ( (pKamuRBBej->Megj()&FELE_PONT) == FELE_PONT )
    {
      // Fele pontszam bekapcsol
      pRBBej->MegjBeall( pRBBej->Megj() | FELE_PONT) ;
    }
    else
    {
      // Fele pontszam kikapcsol
      pRBBej->MegjBeall( pRBBej->Megj() & ~FELE_PONT) ;
    }
  }

  if ( StrBolIdoEr( sBeoIdoer, pRBBej) )
  {
    return TRUE ;
  }

  // Az idoeredmenyt tartalmazo szerkeszto ablak tartalmanak kijelolese
  pIdoEd->SetFocus() ;
  pIdoEd->SetSel( (DWORD)0xffff0000 , (BOOL)FALSE ) ;

  return FALSE ;
}

BOOL CRsBefFelv::StrBolIdoEr(const CString & sBeoIdoer, CRBBejegyzes* pBeoRBBej)
{
  CString sOra, sPerc, sMasodp, sEzrMasodp ;
  // Nyolc szam eleg a mezohatarok (szam<->nem szam) megjegyzesere
  int pHatarok[] = {0, 0, 0, 0, 0, 0, 0, 0} ;
  int poz, hatkpoz ;
  int nOra, nPerc, nMasodp, nEzrMasodp ;
  CIdoeredm cLokIder ;

  // A mezohatarok megallapitasa
  poz = sBeoIdoer.GetLength()-1 ;
  hatkpoz = 7 ;
  if ( poz >= 0 )
  {
    // A while-ban folosleges a kov. feltetel : && !isdigit(sBeoIdoer[poz])
    while ( 0 <= poz && hatkpoz>=0 )
    {
      while ( 0 <= poz && !isdigit(sBeoIdoer[poz]) && hatkpoz>=0 )
      {
        poz-- ;
      }

      *(pHatarok+hatkpoz) = poz ;
      hatkpoz-- ;

      while ( 0 <= poz && isdigit(sBeoIdoer[poz]) && hatkpoz>=0 )
      {
        poz-- ;
      }

      *(pHatarok+hatkpoz) = poz ;
      hatkpoz-- ;
    }
  }

  // A beolvasott sztring szetszabdalasa
  sEzrMasodp = sBeoIdoer.Mid(*(pHatarok+6)+1, *(pHatarok+7) - *(pHatarok+6)) ;
  sMasodp    = sBeoIdoer.Mid(*(pHatarok+4)+1, *(pHatarok+5) - *(pHatarok+4)) ;
  sPerc      = sBeoIdoer.Mid(*(pHatarok+2)+1, *(pHatarok+3) - *(pHatarok+2)) ;
  sOra       = sBeoIdoer.Mid(*(pHatarok+0)+1, *(pHatarok+1) - *(pHatarok+0)) ;

  // Idoeredmeny vagy korhatrany ?
  // Idoeredmeny : ha legalabb az sEzrMasodp es az sMasodp sztringek nem uresek
  // Korhatrany  : ha pontosan az sMasodp nem ures
  // Hibas ertekmegadas : minden egyeb

  if ( sEzrMasodp.GetLength() != 0 )
  {
    if ( sMasodp.GetLength() != 0 )
    {
      // Idoeredmeny
      nEzrMasodp = atoi( (const char*) sEzrMasodp ) ;
      nMasodp    = atoi( (const char*) sMasodp ) ;

      // Ugyan negativ ertekek ugysem fordulhatnak elo (- nem szamjegy)...
      if ( -1000 < nEzrMasodp && nEzrMasodp < 1000 )
      { 
        cLokIder.EzrdMsdpBeall(nEzrMasodp) ;
      }
      else
      {
        // Ha a beolvasott szamertek nem megengedett tartomanyba esik
        return FALSE ;
      }

      // Ugyan negativ ertekek ugysem fordulhatnak elo (- nem szamjegy)...
      if ( -60 < nMasodp && nMasodp < 60 )
      {
        // Ha a beolvasott szamertek nem megengedett
        cLokIder.MasodpercBeall(nMasodp) ;
      }
      else
      {
        // Ha a beolvasott szamertek nem megengedett tartomanyba esik
        return FALSE ;
      }

      // A tobbit csak akkor, ha nem uresek (nullak), mert a cLokIder adattagjait
      // a default konstruktor ugyis 0-ra allitotta
      if ( sPerc.GetLength() != 0 )
      {
        nPerc = atoi( (const char*) sPerc ) ;
        // Ugyan negativ ertekek ugysem fordulhatnak elo (- nem szamjegy)...
        if ( -60 < nPerc && nPerc < 60 )
        {
          cLokIder.PercBeall(nPerc) ;
        }
        else
        {
          // Ha a beolvasott szamertek nem megengedett tartomanyba esik
          return FALSE ;
        }
      }

      if ( sOra.GetLength() != 0 )
      {
        nOra = atoi( (const char*) sOra ) ;
        // Ugyan negativ ertekek ugysem fordulhatnak elo (- nem szamjegy)...
        if ( -129 < nOra && nOra < 128 )
        {
          cLokIder.OraBeall(nOra) ;
        }
        else
        {
          // Ha a beolvasott szamertek nem megengedett tartomanyba esik
          return FALSE ;
        }
      }

      pBeoRBBej->IdoerBeall(cLokIder) ;

      // A megjegyzes beallitasa : ne rontson el semmit : a regit
      // es ne lehessen IDOEREDMENY es KORHATRANY egyszerre
      pBeoRBBej->MegjBeall((pBeoRBBej->Megj() | IDOEREDMENY) & ~KORHATRANY) ;
    }
    else
    {
      // Korhatrany
      nEzrMasodp = atoi( (const char*) sEzrMasodp ) ;

      // Ugyan negativ ertekek ugysem fordulhatnak elo (- nem szamjegy)...
      if ( cLokIder.EzrdMsdpBeall(nEzrMasodp) == FALSE)
      {
        // Ha a beolvasott szamertek nem megengedett tartomanyba esik
        return FALSE ;
      }

      pBeoRBBej->IdoerBeall(cLokIder) ;
      pBeoRBBej->MegjBeall((pBeoRBBej->Megj() | KORHATRANY) & ~IDOEREDMENY) ;
    }

    return TRUE ;
  }

  return FALSE ;
}

void CRsBefFelv::LegyKorKiir()
{
  CStatic* pLegyKorSt = (CStatic*) GetDlgItem(IDC_LGYK_EREDM_S) ;

  unsigned uiNvzlKulcs ;
  CNvzSor* pNvzSor ;
  nevadat* pAdat ;
  vnevadat* pVNevadat ;
  char pszSegedStr[18] ;
  CString  sEgySor ;

  if ( m_enRajtsBef == BEFUTO )
  {
    uiNvzlKulcs = m_cAktRsBef.m_cCLeggyKor.Nvzl() ;
    if ( !m_cAktNvzLst.Lookup( uiNvzlKulcs, pNvzSor) )
    {
      return ;
    }

    // A beillesztendo sor uresen kezed
//    sEgySor = "" ;

//    _itoa(LOBYTE(wNvzlKulcs), pszSegedStr, 10) ;
    sprintf( pszSegedStr, "%d", pNvzSor->uiRajtszam) ;
    sEgySor=pszSegedStr ; sEgySor=sEgySor+'\t' ;

    // A versenyzonev kikeresese es hozzaadasa
    if ( m_pVersenyzo->Lookup( pNvzSor->uiVersenyzo , pVNevadat) == TRUE )
    {
      sEgySor=sEgySor+pVNevadat->m_sKerNev+CString(' ')+pVNevadat->m_sVezNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"??? ???\t" ;
    }

    // A nemzetiseg kikeresese es hozzaadasa
    if ( m_pNemz->Lookup(pNvzSor->uiNemz, pAdat) == TRUE )
    {
      sEgySor=sEgySor+pAdat->m_sNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"???\t" ;
    }

    // A csapat kikeresese es hozzaadasa
    if ( m_pCsapat->Lookup(pNvzSor->uiCsapat, pAdat) == TRUE )
    {
      sEgySor=sEgySor+pAdat->m_sNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"???\t" ;
    }

    // A motor kikeresese es hozzaadasa
    if ( m_pMotor->Lookup(pNvzSor->uiMotor, pAdat) == TRUE )
    {
      sEgySor=sEgySor+pAdat->m_sNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"???\t" ;
    }

    sEgySor = sEgySor + m_cAktRsBef.m_cCLeggyKor.IdoerS() ;

    sprintf(pszSegedStr, "\t%.3f km/h", m_cAktRsBef.m_cCLeggyKor.AtlSeb() ) ;
    sEgySor = sEgySor + pszSegedStr ;
    pLegyKorSt->SetWindowText(sEgySor) ;
  }
}

void CRsBefFelv::AktRBKiir(DWORD dwUjKivRBSor)
{
  CListBox * pEredmLb = (CListBox* ) GetDlgItem(IDC_EREDM_LB) ;
  unsigned char nAktSor, ucRsBefMer ;
  CRBBejegyzes *pRBBej ;
  unsigned  uiNvzlKulcs ;
  CString   sEgySor ;
  CNvzSor*  pNvzSor ;
  nevadat*  pAdat ;
  vnevadat* pVNevadat ;

  // Mivel az itoa fv. nem foglalkozik helyfoglalassal. Max 17 byte-ra van szuksege.
  char pszSegedStr[18] ;

  //-A-nevezesi-listat-tartalmazo-Lb-torlese------------------------------
  if ( dwUjKivRBSor == 0 )
  {
    dwUjKivRBSor = pEredmLb->GetItemData(pEredmLb->GetCurSel()) ;
  }

  pEredmLb->ResetContent() ;

  //-A-nevezesi-listat-tartalmazo-Lb-feltoltese---------------------------
  ucRsBefMer = m_cAktRsBef.Meret() ;

  for ( nAktSor=1 ; nAktSor <= ucRsBefMer ; nAktSor++ )
  {
    uiNvzlKulcs = m_cAktRsBef[nAktSor] ;
    if ( !m_cAktNvzLst.Lookup(uiNvzlKulcs, pNvzSor) )
    {
      return ;
    }

    sprintf( pszSegedStr, "%d\t%d\t", nAktSor, pNvzSor->uiRajtszam) ;
    sEgySor=pszSegedStr ;

    // A versenyzonev kikeresese es hozzaadasa
    if ( m_pVersenyzo->Lookup( pNvzSor->uiVersenyzo , pVNevadat) == TRUE )
    {
      sEgySor=sEgySor+pVNevadat->m_sKerNev+CString(' ')+pVNevadat->m_sVezNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"??? ???\t" ;
    }

    // A nemzetiseg kikeresese es hozzaadasa
    if ( m_pNemz->Lookup(pNvzSor->uiNemz, pAdat) == TRUE )
    {
      sEgySor=sEgySor+pAdat->m_sNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"???\t" ;
    }

    // A csapat kikeresese es hozzaadasa
    if ( m_pCsapat->Lookup(pNvzSor->uiCsapat, pAdat) == TRUE )
    {
      sEgySor=sEgySor+pAdat->m_sNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"???\t" ;
    }

    // A motor kikeresese es hozzaadasa
    if ( m_pMotor->Lookup(pNvzSor->uiMotor, pAdat) == TRUE )
    {
      sEgySor=sEgySor+pAdat->m_sNev+'\t' ;
    }
    else
    {
      sEgySor=sEgySor+"???\t" ;
    }

    // Az (ido) eredmeny hozzaadasa
    if ( m_cAktRsBef.Lookup( uiNvzlKulcs, pRBBej) == TRUE )
    {
      sEgySor = sEgySor + pRBBej->IdoerS() ;
    }
    else
    {
      sEgySor = sEgySor + "???" ;
    }

    pEredmLb->SetItemData( pEredmLb->AddString(sEgySor), DWORD(uiNvzlKulcs)) ;
  }

  if ( dwUjKivRBSor != LB_ERR )
  {
    nAktSor=0;
    while ( nAktSor < ucRsBefMer )
    {
      if ( dwUjKivRBSor == pEredmLb->GetItemData( nAktSor) )
      {
        pEredmLb->SetCurSel( nAktSor) ;
        return ;
      }

      nAktSor++ ;
    }
  }

  pEredmLb->SetCurSel( 0) ;
}

BOOL CRsBefFelv::UresSor(CString& cSor)
{
  int i ;
  const char* pszCStringBele = (const char*) cSor ;

  for ( i=0 ; *(pszCStringBele+i) != '\0' && *(pszCStringBele+i) == ' ' ; i++ ) ;

  // Ha azert szallt ki, mert ENTER-t v. sztringlezaro karaktert talalt,
  // a sztring uresnek szamit
  if ( *(pszCStringBele+i) == '\0' || *(pszCStringBele+i) == '\n' )
  {
    return TRUE ;
  }

  return FALSE ;
}

void CRsBefFelv::HibaKiir(long nRC)
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

void CRsBefFelv::PostNcDestroy()
{ 
  // A paredit.cpp-ben sem hivja a bazisosztaly azonos nevu fv.et...
  delete this ;
}

CRsBefFelv::~CRsBefFelv()
{
  // A destruktor ezt ugyis megtenne
//  m_ImportFile.Close() ;
}