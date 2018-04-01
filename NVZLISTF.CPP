// nvzlistf.cpp : implementation file
//

#include "stdafx.h"
#include "resource.h"

#include "alaptip.h"
#include "dbglobal.h"
#include "ensqlcam.h"
#include "pontozas.h"
#include "cnev.h"
#include "cvnev.h"
#include "proba13.h"
#include "cszezon.h"
#include "nevzlist.h"
//#include "rendnvzl.h"
#include "nvzlistf.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNvzListFelv dialog

CNvzListFelv::CNvzListFelv(CWnd* pParent /*=NULL*/)
	: CDialog(CNvzListFelv::IDD, pParent)
{
  CProba13App* pProba13App = (CProba13App*)AfxGetApp() ;
  DWORD dwDlgBasUnit ;

  // Ezeket a mutatokat nem konstruktorban adom at, mivel fixnek tekinthetok :
  // nem kell oket visszairni OK-val valo kiszallasnal, a nevezes listat meg 
  // kilepes elott mindig mentem ...
  m_pNagyd    = &pProba13App->m_cNagydijNev   ;

  m_pVersenyzo= &pProba13App->m_cVersenyzoNev  ;
  m_pNemz     = &pProba13App->m_cNemzetisegNev ;
  m_pCsapat   = &pProba13App->m_cCsapatNev     ;
  m_pMotor    = &pProba13App->m_cMotorNev      ;

  // LOKALIS ADATOK :
  m_bAktNvzLstMentve = TRUE ;     

  // lehet, hogy nem ciklus kell:nem nlotav
  dwDlgBasUnit = GetDialogBaseUnits() ;

  m_nNvzLstTbStp[0] = 4 * 40  / LOWORD(dwDlgBasUnit) ;
  m_nNvzLstTbStp[1] = 4 * 220 / LOWORD(dwDlgBasUnit) ;
  m_nNvzLstTbStp[2] = 4 * 410 / LOWORD(dwDlgBasUnit) ;
  m_nNvzLstTbStp[3] = 4 * 500 / LOWORD(dwDlgBasUnit) ;
  m_nNvzLstTbStp[4] = 4 * 690 / LOWORD(dwDlgBasUnit) ;
}

// A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
// DoDataExchange fuggvenybol
BOOL CNvzListFelv::OnInitDialog()
{
  CDialog::OnInitDialog() ;

  CComboBox* pSzezCb  = (CComboBox*) GetDlgItem(IDC_NVZL_SZEZ_CB) ;
  CComboBox* pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZ_CB) ;
  CComboBox* pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZ_CB)     ;
  CComboBox* pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)   ;
  CComboBox* pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)    ;

  CListBox * pNvzLst  = (CListBox* ) GetDlgItem(IDC_NEVEZES_LIST);

  unsigned* pSzezTomb ;
  unsigned  nSzezdb, i ;
  char pszSzezEve[10] ;

  // Valtozok a 'map'-eken valo vegiglepkedeshez
  POSITION poz ;
  unsigned uiKulcs ;
  nevadat* pAdat ;
  vnevadat* pVAdat ;

  long nRC ;

  //--------------------------------------------------------------------------
  // A futamokat tartalmazo Lb tabstopjainak beallitasa
  pNvzLst->SendMessage(LB_SETTABSTOPS, 5, (LONG)(LPSTR)m_nNvzLstTbStp) ;

  // Maga az inicializalas :
  //--------------------------------------------------------------------------
  // A felvitt szezonok evszamait tartalmazo Cb feltoltese
  pSzezTomb = NULL ;
  
  if ( (nRC=SQL_FelvSzez( &pSzezTomb, &nSzezdb)) != 0 )
  {
    HibaKiir( nRC) ;

    return FALSE ;
  }

  for ( i = 0 ; i < nSzezdb ; i++ )
  {
    sprintf( pszSzezEve, "%d", *(pSzezTomb+i)) ;
    pSzezCb->SetItemData( pSzezCb->AddString( pszSzezEve), (DWORD) *(pSzezTomb+i)) ;
  }

  delete [] pSzezTomb ;

  //--------------------------------------------------------------------------
	// A nagydijakat tartalmazo Cb feltoltese

	// Mivel nincs 'aktualis szezon' nem toltom fel, majd csak a SzezCb valt.kor

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
  poz = m_pNemz->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pNemz->GetNextAssoc( poz, uiKulcs, pAdat ) ;
    pNemzCb->SetItemData(pNemzCb->AddString(pAdat->m_sNev), uiKulcs) ;
  }

  // A combo box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pNemzCb->SetCurSel(0) ;

  //--------------------------------------------------------------------------
  // A csapat neveket tartalmazo Cb feltoltese
  poz = m_pCsapat->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pCsapat->GetNextAssoc( poz, uiKulcs, pAdat ) ;
    pCsapCb->SetItemData(pCsapCb->AddString(pAdat->m_sNev), uiKulcs) ;
  }

  // A combo box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pCsapCb->SetCurSel(0) ;

  //--------------------------------------------------------------------------
  // A motor neveket tartalmazo Cb feltoltese

  poz = m_pMotor->GetStartPosition() ;
  while( poz != NULL)
  {
    m_pMotor->GetNextAssoc( poz, uiKulcs, pAdat ) ;
    pMotorCb->SetItemData(pMotorCb->AddString(pAdat->m_sNev), uiKulcs) ;
  }

  // A combo box elso soranak kijelolese (hogy mindig legyen 1 kijelolt)
  pMotorCb->SetCurSel(0) ;

	return TRUE ;
}

BEGIN_MESSAGE_MAP(CNvzListFelv, CDialog)
	//{{AFX_MSG_MAP(CNvzListFelv)
	ON_CBN_SELCHANGE(IDC_NVZL_SZEZ_CB, OnSelchgSzez)
	ON_LBN_DBLCLK(IDC_NEVEZES_LIST, OnDblclkNvzLst)
	ON_BN_CLICKED(IDC_FELV_DB, OnFelvesz)
	ON_BN_CLICKED(IDC_MOD_B, OnModosit)
	ON_BN_CLICKED(IDC_TRL_B, OnTorol)
	ON_BN_CLICKED(IDC_NVZLST_FLV_B, OnNvzListFelv)
	ON_BN_CLICKED(IDC_NVZLST_TRL_B, OnNvzListTorol)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNvzListFelv message handlers

// A fv az ujonnan szelektalt szezon nevezesi listajat irja az ablakba, ha az
// aktualis nevezesi lista mentetlen volt felkinalja menteset
// Tolti es menti az aktualis szezont, mivel a nagydijakhoztartozo m_wFile a
// nevezesi listaban valo szereples fveben valtozik.
void CNvzListFelv::OnSelchgSzez()
{
  CComboBox* pSzezCb  = (CComboBox*) GetDlgItem(IDC_NVZL_SZEZ_CB) ;
  CComboBox* pElsovCb = (CComboBox*) GetDlgItem(IDC_ELSOV_CB)     ;
  CListBox*  pNvzLstLb= (CListBox*)  GetDlgItem(IDC_NEVEZES_LIST) ;

  int nKivPoz ;

  // Hibauzenetek
  CString sAblCim, sSzov ;
  long nRC ;

//  pSzezCb->GetLBText( pSzezCb->GetCurSel(), cKivSzez) ;
  nKivPoz = pSzezCb->GetCurSel() ;

  if ( nKivPoz != CB_ERR )
  {
    if ( m_bAktNvzLstMentve == FALSE )
    {
      GetWindowText( sAblCim) ;

      if ( sSzov.LoadString( IDS_NVZL_MGV_S) == FALSE )
        sSzov = "IDS_NVZL_MGV_S" ;

      if ( IDYES==MessageBox( sSzov, sAblCim, MB_ICONQUESTION|MB_YESNO) )
	    {
        // Az aktualis nevezesi lista mentese
        if ( (nRC=SQL_COMMIT()) != 0 )
        {
          HibaKiir( nRC) ;

          return ;
        }
      }
      else
      {
        if ( (nRC=SQL_ROLLBACK()) != 0 )
        {
          HibaKiir( nRC) ;

          return ;
        }
      }

      m_bAktNvzLstMentve = TRUE ;
    }

	  // A nevezesi listat tartalmazo Lb torlese
	  pNvzLstLb->ResetContent() ;
	  //  hasonloan az m_cAktNvzLst -re is !
	  m_cAktNvzLst.RemoveAll() ;

    // m_uiSzezon beallitodik itt \/
    nRC = m_cAktNvzLst.SQL_NvzlstBetolt( (unsigned) pSzezCb->GetItemData( nKivPoz)) ;

    if ( nRC != 0 )
    {
      HibaKiir( nRC) ;

      return ;
    }

    // Az uj nvz lst sor kiiratasa a lista ablakba
    AktNvzLstKiir() ;

    // A regi szezon futamjainak torlese, az uj szezon futamjainak beirasa
    // az Elso Nagydij Cb-ba
//    pElsovCb->ResetContent() ;

//    A szezon nagydijaival fel kell tolteni az elso nd. ablakot ?!?
  }
}

void CNvzListFelv::OnDblclkNvzLst()
{
  CListBox*  pNvzLstLb= (CListBox*)  GetDlgItem(IDC_NEVEZES_LIST);

  CEdit*     pRajtszEd= (CEdit*)     GetDlgItem(IDC_RAJTSZ_E)    ;
  CComboBox* pElsovCb = (CComboBox*) GetDlgItem(IDC_ELSOV_CB)    ;
  CComboBox* pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZ_CB) ;
  CComboBox* pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZ_CB)     ;
  CComboBox* pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)   ;
  CComboBox* pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)    ;

  int nKivSor ;

  nKivSor = pNvzLstLb->GetCurSel() ;

  if ( nKivSor != LB_ERR )
  {
    // Ha nincs semmi kiszelektalva azzal ne foglalkozzunk
    CNvzSor* pNvzSorAdat ;
    nevadat* pAdat ;
    vnevadat* pVAdat ;
    unsigned uiKulcs ;
    char pszRajtsz[18] ;

    uiKulcs = pNvzLstLb->GetItemData(nKivSor) ;

    // Csak akkor probalkozzek a megfelelo Cb-kben valo keresessel, ha
    // az Lb-bol kinyert kulcsot megtalalja a nevezesi lista sorok kozt
    if ( m_cAktNvzLst.Lookup( uiKulcs, pNvzSorAdat) )
    {
      // A rajtszam kikeresese
      sprintf( pszRajtsz, "%d", pNvzSorAdat->uiRajtszam) ;
      pRajtszEd->SetWindowText(pszRajtsz) ;

      // A nagydij(verseny) kikeresese, ahol ez a kulonbozo sor eloszor szerepelt
//      if ( m_pNagyd->Lookup(pNvzSorAdat->wElsoVers, pAdat) )
//        pElsovCb->SelectString(-1, pAdat->m_sNev) ;

      // A versenyzo nevenek kikeresese
      if ( m_pVersenyzo->Lookup(pNvzSorAdat->uiVersenyzo, pVAdat) )
        pVzoCb->SelectString(-1, pVAdat->m_sVezNev+", "+pVAdat->m_sKerNev) ;

      // A versenyzo nemzetisegenek kikeresese
      if ( m_pNemz->Lookup(pNvzSorAdat->uiNemz, pAdat) )
        pNemzCb->SelectString(-1, pAdat->m_sNev) ;
          
      // A csapat kikeresese
      if ( m_pCsapat->Lookup(pNvzSorAdat->uiCsapat, pAdat) )
        pCsapCb->SelectString(-1, pAdat->m_sNev) ;
          
      // A versenyzo nemzetisegenek kikeresese
      if ( m_pMotor->Lookup(pNvzSorAdat->uiMotor, pAdat) )
        pMotorCb->SelectString(-1, pAdat->m_sNev) ;        
    }
  }      	
}

void CNvzListFelv::OnFelvesz()
{
  CNvzSor* pBeoNvzSor ;
  unsigned uiNvzlKulcs ;
  long nRC ;

  if ( (pBeoNvzSor = new CNvzSor) == NULL )
    return ;

  if ( NvzLstSorBeo( pBeoNvzSor/*, &Futamszam, */))
  {
    if ( (nRC=m_cAktNvzLst.SQL_NvzlstINSERT( pBeoNvzSor, &uiNvzlKulcs)) != 0 )
    {
      delete pBeoNvzSor ;

      HibaKiir( nRC) ;

      return ;
    }

    m_cAktNvzLst.SetAt( uiNvzlKulcs, pBeoNvzSor) ;

    // Az uj nvz lst sor kiiratasa a lista ablakba
    AktNvzLstKiir() ;

    // A felvetel utan a nevezesi lista mentetlen lett
    m_bAktNvzLstMentve = FALSE ;
  }
  else
  {
    delete pBeoNvzSor ;
  }
}

// A nevezesi lista sorban nem lehet a rajtszamot es az elso nagydijat minden
// tovabbi nelkul modositani: a r/b fileokban mar keszulhettek ilyen kulccsal
// bejegyzesek. Ezen megfontolasok miatt a rsz. es elsov. mezoket nem veszi
// figyelembe.
void CNvzListFelv::OnModosit()
{
  CListBox* pNvzLstLb = (CListBox*) GetDlgItem(IDC_NEVEZES_LIST) ;

  CNvzSor*  pModNvzSor, BeoNvzSor ;

  // A kivalasztott nevezesi lista sor :
  int  iKivNvzlst ;
  unsigned uiKivNvzlKulcs, uiUjNvzlKulcs ;

  long nRC ;

  iKivNvzlst = pNvzLstLb->GetCurSel() ;

  // Ha ures a nevezesi lista, vagy nincs semmi kiszelektalva ne csinaljon semmit
  // A nevezesi listaban kell legyen kivalasztott elem
  if ( iKivNvzlst != LB_ERR )
  {
	  // A nevezesi lista sorhoz tartozo kulcs meghatarozasa
	  uiKivNvzlKulcs = (unsigned) pNvzLstLb->GetItemData( iKivNvzlst) ;

	  // Csak, ha a beolvasas eredmenyes volt:
	  if ( NvzLstSorBeo( &BeoNvzSor))
	  {
      // A modositando nevezesi lista sor eloszedese
      if ( m_cAktNvzLst.Lookup( uiKivNvzlKulcs, pModNvzSor) == TRUE )
      {
        uiUjNvzlKulcs = uiKivNvzlKulcs ;

        nRC = m_cAktNvzLst.SQL_NvzlstUPDATE( &BeoNvzSor, &uiUjNvzlKulcs) ;

        switch ( nRC )
        {
          case VE_UJ_NVZL_SOR :
            // Mindenkeppen a szulo osztaly fuggvenyenek kell meghivodnia,
            // kulonben felszabaditja a pointer altal cimzett teruletet
            m_cAktNvzLst.CMapPtrToPtr::RemoveKey( (void*)uiKivNvzlKulcs) ;

            m_cAktNvzLst.SetAt( uiUjNvzlKulcs, pModNvzSor) ;

          case 0 :
            // A beolvasott adatok beirasa az elso nagydij kivetelevel
            pModNvzSor->uiVersenyzo= BeoNvzSor.uiVersenyzo ;
            pModNvzSor->uiNemz     = BeoNvzSor.uiNemz  ;
            pModNvzSor->uiCsapat   = BeoNvzSor.uiCsapat;
            pModNvzSor->uiMotor    = BeoNvzSor.uiMotor ;

            break ;

          default :
            // Ha SQL_NvzlstUPDATE hibakoddal ter vissza
            HibaKiir( nRC) ;

            return ;
        } ;

        // Az uj nvz lst sor kiiratasa a lista ablakba
        AktNvzLstKiir() ;

        // A felvetel utan a nevezesi lista mentetlen lett
        m_bAktNvzLstMentve = FALSE ;
      }
	  }
	}
}

void CNvzListFelv::OnTorol()
{
  CListBox*  pNvzLstLb = (CListBox*)  GetDlgItem(IDC_NEVEZES_LIST) ;

  int      iKivNvzlst ;
  unsigned uiKivNvzlKulcs ;

  long nRC ;

  iKivNvzlst = pNvzLstLb->GetCurSel() ;

  // Ha ures a nevezesi lista, vagy nincs semmi kiszelektalva ne csinaljon semmit
  if ( iKivNvzlst != LB_ERR )
  {
    // Ha van kiszelektalt elem
    uiKivNvzlKulcs = pNvzLstLb->GetItemData(iKivNvzlst) ;

    if ( (nRC = m_cAktNvzLst.SQL_NvzlstDELETE( uiKivNvzlKulcs)) == 0 )
    {
      // Torles a nevezesi listabol es a lista ablakbol
      m_cAktNvzLst.RemoveKey( uiKivNvzlKulcs) ;

      // Az uj nvz lst sor kiiratasa a lista ablakba
      AktNvzLstKiir() ;

      // A torles utan a nevezesi lista mentetlen lett
      m_bAktNvzLstMentve = FALSE ;
    }
    else
    {
      HibaKiir( nRC) ;
    }
  }
}

void CNvzListFelv::OnNvzListFelv()
{
  CListBox*  pNvzLstLb = (CListBox*)  GetDlgItem(IDC_NEVEZES_LIST) ;

  CComboBox* pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZ_CB) ;
  CComboBox* pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZ_CB)     ;
  CComboBox* pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)   ;
  CComboBox* pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)    ;

  long nRC ;

  if ( m_bAktNvzLstMentve == FALSE )
  {
    // Ne mentsen foloslegesen ...
    if ( (nRC=SQL_COMMIT()) != 0 )
    {
      HibaKiir( nRC) ;

      return ;
    }

    // A mentes utan a nevezesi lista mentett lett
    m_bAktNvzLstMentve = TRUE ;
  }

  m_cAktNvzLst.RemoveAll() ;

  pNvzLstLb->ResetContent();
  pVzoCb->SetCurSel( -1)   ;
  pNemzCb->SetCurSel( -1)  ;
  pCsapCb->SetCurSel( -1)  ;
  pMotorCb->SetCurSel( -1) ;
}

void CNvzListFelv::OnNvzListTorol()
{
  CComboBox* pSzezCb   = (CComboBox*) GetDlgItem(IDC_NVZL_SZEZ_CB) ;

  CListBox*  pNvzLstLb = (CListBox*)  GetDlgItem(IDC_NEVEZES_LIST) ;

  CComboBox* pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZ_CB) ;
  CComboBox* pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZ_CB)     ;
  CComboBox* pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)   ;
  CComboBox* pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)    ;

  int  nKijElem ;
  long nRC ;

  nRC = m_cAktNvzLst.SQL_NvzlstTorol() ;

  if ( nRC == 0 )
  {
    /* Az osztaly kiuritese */
    m_cAktNvzLst.RemoveAll() ;

    nKijElem = pSzezCb->GetCurSel() ;

    if ( nKijElem != CB_ERR )
    {
      pSzezCb->DeleteString( nKijElem) ;
    }

    pSzezCb->Clear() ;

    pNvzLstLb->ResetContent();
    pVzoCb->SetCurSel( -1)   ;
    pNemzCb->SetCurSel( -1)  ;
    pCsapCb->SetCurSel( -1)  ;
    pMotorCb->SetCurSel( -1) ;
  }
  else
  {
    HibaKiir( nRC) ;
  }

  // A torles utan az ures nevezesi lista 'mentett' lett
  // (Ne akarja az ureset szezonvaltas utan menteni)
  m_bAktNvzLstMentve = TRUE ;    
}

void CNvzListFelv::OnOK()
{
  long nRC ;

  if ( m_bAktNvzLstMentve == FALSE )
  {
    // Csak, ha az aktualis nevezesi lista mentetlen
    if ( (nRC=SQL_COMMIT()) != 0 )
    {
      HibaKiir( nRC) ;
    }
  }

	CDialog::OnOK() ;
}

void CNvzListFelv::OnCancel()
{
  long nRC ;
  CString sAblCim, sSzov ;

  if ( m_bAktNvzLstMentve==FALSE )
  {
    GetWindowText( sAblCim) ;

    if ( sSzov.LoadString( IDS_NVZL_MGV_S) == FALSE )
      sSzov = "IDS_NVZL_MGV_S" ;

    // Az aktualis nevlista megvaltozott. Kell-e menteni ?
    if ( IDYES == MessageBox( sSzov, sAblCim, MB_ICONQUESTION | MB_YESNO) )
    {
      if ( ( nRC = SQL_COMMIT() ) != 0 )
      {
        HibaKiir( nRC) ;
      }
    }
    else
    {
      if ( ( nRC = SQL_ROLLBACK() ) != 0 )
      {
        HibaKiir( nRC) ;
      }
    }
  }

	CDialog::OnCancel();
}

// Beolvas egy nevezesi lista sort a szerk ablakbol es a Cb-kbol,
// TRUE-val ter vissza, ha minde rendben, FALSE-szal, ha a szerk. ablak tar-
// talma hibas, vagy nincs a Cb-kban kiszelektalas
BOOL CNvzListFelv::NvzLstSorBeo( CNvzSor* pBeoNvzSor/*, unsigned* pElsoVers*/)
{
  CEdit*     pRajtszEd= (CEdit*)     GetDlgItem(IDC_RAJTSZ_E)    ;
  CComboBox* pElsovCb = (CComboBox*) GetDlgItem(IDC_ELSOV_CB)    ;
  CComboBox* pVzoCb   = (CComboBox*) GetDlgItem(IDC_VERSENYZ_CB) ;
  CComboBox* pNemzCb  = (CComboBox*) GetDlgItem(IDC_NEMZ_CB)     ;
  CComboBox* pCsapCb  = (CComboBox*) GetDlgItem(IDC_CSAPAT_CB)   ;
  CComboBox* pMotorCb = (CComboBox*) GetDlgItem(IDC_MOTOR_CB)    ;

  CString sBeoRszam ;
  int     nBeoRszam ;
  int     nKivElem  ;

  if ( pBeoNvzSor == NULL /*|| pElsoVers == NULL */)
    return FALSE ;

  // A rajtszam beolvasasa, ellenorzese
  pRajtszEd->GetWindowText( sBeoRszam) ;
  nBeoRszam = atoi( sBeoRszam) ;

  // Csak akkor megyunk tovabb, ha a rajtszam OK (0..MAX_RAJTSZAM)
  // es a sztring nem volt ures
  if ( sBeoRszam.GetLength() && nBeoRszam >= 0 && nBeoRszam <= MAX_RAJTSZAM )
  {
    // A rajtszam visszadasa
    pBeoNvzSor->uiRajtszam = nBeoRszam ;

    //-Az-elso-verseny-beolvasasa---------------------------------------------
/*
    nKivElem = pElsovCb->GetCurSel() ;
    if ( nKivElem == CB_ERR )
      return FALSE ;

    *pElsoVers = pElsovCb->GetItemData( nKivElem) ;
*/
    //-A-versenyzonev-beolvasasa----------------------------------------------
    nKivElem = pVzoCb->GetCurSel() ;
    if ( nKivElem == CB_ERR )
      return FALSE ;

    pBeoNvzSor->uiVersenyzo = pVzoCb->GetItemData( nKivElem) ;

    //-A-versenyzo-nemzetisegenek-beolvasasa----------------------------------
    nKivElem = pNemzCb->GetCurSel() ;
    if ( nKivElem == CB_ERR )
      return FALSE ;

    pBeoNvzSor->uiNemz = pNemzCb->GetItemData(nKivElem) ;

    //-A-csapat-beolvasasa----------------------------------------------------
    nKivElem = pCsapCb->GetCurSel() ;
    if ( nKivElem == CB_ERR )
      return FALSE ;

    pBeoNvzSor->uiCsapat = pCsapCb->GetItemData(nKivElem) ;

    //-A-motor-beolvasasa-----------------------------------------------------
    nKivElem = pMotorCb->GetCurSel() ;
    if ( nKivElem == CB_ERR )
      return FALSE ;

    pBeoNvzSor->uiMotor = pMotorCb->GetItemData(nKivElem) ;

    // Minden jo (ha jo a vege...)
    return TRUE ;
  }
  else
  {
    // A rajtszamot tartalmazo szerkeszto ablak tartalmanak kijelolese
    pRajtszEd->SetFocus() ;
    pRajtszEd->SetSel( (DWORD) 0xffff0000 , (BOOL) FALSE ) ;

    return FALSE ;    
  }

  return FALSE ;
} 

void CNvzListFelv::AktNvzLstKiir()
{
  CListBox* pNvzLstLb = (CListBox*) GetDlgItem( IDC_NEVEZES_LIST) ;

  unsigned short usNvzlMer, usIdx ;
  sRszNvzlBej*   pRszNvzlTmb ;

  unsigned  uiNvzlKulcs ;
  POSITION  poz ;
  CNvzSor*  pNvzLstAdat ;
  nevadat*  pAdat ;
  vnevadat* pVNevadat ;

  CString sHibaszov ;
  char pszEgySor[120] ;

  // A nevezesi listat tartalmazo Lb torlese
  pNvzLstLb->ResetContent() ;

  // -------------------------------------------------------------------------
  usNvzlMer = m_cAktNvzLst.Meret() ;

  if ( (pRszNvzlTmb = new sRszNvzlBej[usNvzlMer]) == NULL )
    return ;

  usIdx = 0 ;
  poz = m_cAktNvzLst.GetStartPosition() ;
  while( poz != NULL )
  {
    m_cAktNvzLst.GetNextAssoc( poz, uiNvzlKulcs, pNvzLstAdat) ;
    pRszNvzlTmb[usIdx].uiRajtszam  = pNvzLstAdat->uiRajtszam  ;
    pRszNvzlTmb[usIdx].uiNvzlKulcs = uiNvzlKulcs ;
    usIdx++ ;
  }

  qsort( (void*)pRszNvzlTmb, usNvzlMer, sizeof(sRszNvzlBej), NvzlOsszehas) ;
  // -------------------------------------------------------------------------

  // A rendezett nevezesi lista beirasa a lista ablakba
  for ( usIdx = 0 ; usIdx < usNvzlMer ; usIdx++)
  {
    uiNvzlKulcs = pRszNvzlTmb[usIdx].uiNvzlKulcs ;

    if ( m_cAktNvzLst.Lookup( uiNvzlKulcs, pNvzLstAdat) == TRUE )
    {
      // A beillesztendo sor uresen kezed
      sprintf( pszEgySor, "%u\t", pRszNvzlTmb[usIdx].uiRajtszam) ;

      // A versenyzonev kikeresese es hozzaadasa
      m_pVersenyzo->Lookup( pNvzLstAdat->uiVersenyzo, pVNevadat) ;
      sprintf( pszEgySor, "%s\t%s, %s", pszEgySor,
                                        (const char*)pVNevadat->m_sVezNev,
                                        (const char*)pVNevadat->m_sKerNev) ;

      // A nemzetiseg kikeresese es hozzaadasa
      m_pNemz->Lookup( pNvzLstAdat->uiNemz, pAdat) ;
      sprintf( pszEgySor, "%s\t%s", pszEgySor, (const char*)pAdat->m_sNev) ;

      // A csapat kikeresese es hozzaadasa
      m_pCsapat->Lookup(pNvzLstAdat->uiCsapat, pAdat) ;
      sprintf( pszEgySor, "%s\t%s", pszEgySor, (const char*)pAdat->m_sNev) ;    

      // A motor kikeresese es hozzaadasa
      m_pMotor->Lookup(pNvzLstAdat->uiMotor, pAdat) ;
      sprintf( pszEgySor, "%s\t%s", pszEgySor, (const char*)pAdat->m_sNev) ;        

      pNvzLstLb->SetItemData(pNvzLstLb->AddString( pszEgySor), uiNvzlKulcs) ;
    }
    else
    {
      // Ezt a nevezesi lista kulcsot nem talalta !
      if ( sHibaszov.LoadString( IDS_NVZL_NTAL_S) == FALSE )
        sHibaszov = "IDS_NVZL_NTAL_S" ;

      sprintf( pszEgySor, "%d", uiNvzlKulcs) ;
      pNvzLstLb->SetItemData(pNvzLstLb->AddString( sHibaszov+pszEgySor), uiNvzlKulcs) ;
    }
  }

  delete [] pRszNvzlTmb ;
}

void CNvzListFelv::HibaKiir(long nRC)
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

int NvzlOsszehas(const void* psRszNvzlBej1, const void* psRszNvzlBej2)
{
  sRszNvzlBej* p1 = (sRszNvzlBej*) psRszNvzlBej1 ;
  sRszNvzlBej* p2 = (sRszNvzlBej*) psRszNvzlBej2 ;

  if ( p1->uiRajtszam < p2->uiRajtszam )
    return -1 ;

  if ( p1->uiRajtszam > p2->uiRajtszam )
    return +1 ;

  return 0 ;
}