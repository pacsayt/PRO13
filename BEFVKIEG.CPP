// befvkieg.cpp :
// A rajtsorrend/befuto felvetelenel felmerulo kiegeszito adatok
// (leggyorsabb kor, beszamit-e az eredmeny a r/b-ba) megadasara szolgalo ablak
//

#include "stdafx.h"

#include <ctype.h>
#include <string.h>

//#include "dbglobal.h"
#include "ensqlcam.h"

// A proba13.h miatt kell a kovetkezo negy include :
#include "resource.h"
#include "alaptip.h"
#include "cnev.h"
#include "cvnev.h"
#include "pontozas.h"

#include "proba13.h"

#include "cszezon.h"
#include "nevzlist.h"
#include "idoeredm.h"
#include "rajtsbef.h"

#include "dbglobal.h"

#include "befvkieg.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEreBeszFlv dialog :
// Beszamit-e az adott eredmeny a versenyzo/csapat eredmenyebe

CEreBeszFlv::CEreBeszFlv(CWnd* pParent /*=NULL*/)
	: CDialog(CEreBeszFlv::IDD, pParent)
{
  //{{AFX_DATA_INIT(CEreBeszFlv)
    // Az alaphelyzet : mind a csapatok, mind a versenyzok versenyebe beszamit
    m_bCsapat = TRUE ;
    m_bVersenyzo = TRUE ;
  //}}AFX_DATA_INIT
}

void CEreBeszFlv::DoDataExchange(CDataExchange* pDX)
{
  CDialog::DoDataExchange(pDX);
  //{{AFX_DATA_MAP(CEreBeszFlv)
    DDX_Check(pDX, IDC_CSAPAT_CH, m_bCsapat);
    DDX_Check(pDX, IDC_VERSENYZO_CH, m_bVersenyzo);
  //}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CEreBeszFlv, CDialog)
  //{{AFX_MSG_MAP(CEreBeszFlv)
    // NOTE: the ClassWizard will add message map macros here
  //}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CEreBeszFlv::OnOK()
{
  CDialog::OnOK() ;
}

void CEreBeszFlv::OnCancel()
{
  CDialog::OnCancel() ;
}

/////////////////////////////////////////////////////////////////////////////
// CEreBeszFlv message handlers

/////////////////////////////////////////////////////////////////////////////
// CLegyKorFlv dialog :
// A leggyorsabb kor adatait felvevo dialogus ablak.
// (Az r/b foadatokat felvevo ablakba ez mar nem fert volna be.
// CRajtsBef* pAktRsBef helyett unsigned char ucNdKod
CLegyKorFlv::CLegyKorFlv(unsigned uiSzezonEve, unsigned uiNdKulcs, CNevezLista* pAktNvzLst,
                         CWnd* pParent /*=NULL*/) : CDialog(CLegyKorFlv::IDD, pParent)
{
  //{{AFX_DATA_INIT(CLegyKorFlv)
//    m_fAtlSeb = (float) 0 ;
//    m_nKorben = 0 ;
    m_bFelePont = FALSE ;
  //}}AFX_DATA_INIT

  m_bLeggyKorValt = FALSE ;
  m_bAblInit      = FALSE ;

  m_pVersenyzo = &( ( (CProba13App*) AfxGetApp() )->m_cVersenyzoNev ) ;

  m_uiSzezonEve = uiSzezonEve ;
  m_uiNdKulcs = uiNdKulcs  ;

  m_pAktNvzLst= pAktNvzLst ;
}

BOOL CLegyKorFlv::OnInitDialog()
{
  CDialog::OnInitDialog() ;

  CComboBox* pVzoCb  = (CComboBox*) GetDlgItem(IDC_LGGY_VERS_CB)  ;
  CEdit*     pIdoEd  = (CEdit*)     GetDlgItem(IDC_LEGGY_IDOER_E) ;
  CEdit*     pAtlsEd = (CEdit*)     GetDlgItem(IDC_ATLSEB_E)      ;
  CEdit*     pKorbEd = (CEdit*)     GetDlgItem(IDC_KORBEN_E)      ;

  char pszLokStr[50] ;

  m_bAblInit = TRUE ;

  // Az esetlegesen mar korabban felvitt idoeredmenyt ki kell irni
  pIdoEd->SetWindowText( (const char*)m_cLeggyKor.IdoerS())  ;

  sprintf( pszLokStr, "%.3f", m_cLeggyKor.AtlSeb()) ;
  pAtlsEd->SetWindowText( pszLokStr) ;

  sprintf( pszLokStr, "%d", m_cLeggyKor.Korben()) ;
  pKorbEd->SetWindowText( pszLokStr) ;

  VnevFeltolt() ;

  m_bAblInit = FALSE ;

  return TRUE ;
}

void CLegyKorFlv::DoDataExchange(CDataExchange* pDX)
{
  CDialog::DoDataExchange(pDX) ;

  //{{AFX_DATA_MAP(CLegyKorFlv)
/*
    DDX_Text(pDX, IDC_ATLSEB_E, m_cLeggyKor.AtlSeb()) ;
    DDV_MinMaxFloat(pDX, m_fAtlSeb, (float)0, (float)500) ;
    DDX_Text(pDX, IDC_KORBEN_E, m_cLeggyKor.Korben());
    DDV_MinMaxByte(pDX, m_nKorben, 0, 255);
*/
    DDX_Check(pDX, IDC_FELE_PONT_CHK, m_bFelePont);
  //}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CLegyKorFlv, CDialog)
  //{{AFX_MSG_MAP(CLegyKorFlv)
    ON_CBN_SELCHANGE(IDC_LGGY_VERS_CB, OnSelChgVrs)
    ON_EN_CHANGE(IDC_LEGGY_IDOER_E, OnChgIdoerE)
    ON_EN_CHANGE(IDC_KORBEN_E, OnChgKorbenE)
    ON_EN_CHANGE(IDC_ATLSEB_E, OnChgAtlSebE)
  //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CLegyKorFlv message handlers

void CLegyKorFlv::OnOK()
{
  CEdit* pIdoerEd  = (CEdit*) GetDlgItem(IDC_LEGGY_IDOER_E) ;
  CEdit* pKorbenEd = (CEdit*) GetDlgItem(IDC_KORBEN_E) ;
  CEdit* pAtlSebEd = (CEdit*) GetDlgItem(IDC_ATLSEB_E) ;

  long nRC ;

  // Az idoeredmeny kiolvasasa
  if ( IdoEredmBeo() == FALSE )
  {
    pIdoerEd->SetFocus() ;
    pIdoerEd->SetSel( 0, -1 ) ;
    return ;
  }

  if ( KorbenBeo() == FALSE )
  {
    pKorbenEd->SetFocus() ;
    pKorbenEd->SetSel( 0, -1 ) ;
    return ;
  }

  if ( AtlSebBeo() == FALSE )
  {
    pAtlSebEd->SetFocus() ;
    pAtlSebEd->SetSel( 0, -1 ) ;
    return ;
  }

  // Nem lehet tudni, hogy van-e mar adat felveve
  nRC = m_cLeggyKor.SQL_LeggyKorINSERT( m_uiSzezonEve, m_uiNdKulcs) ;

  if ( nRC == -1 )
  {
    nRC = m_cLeggyKor.SQL_LeggyKorUPDATE( m_uiSzezonEve, m_uiNdKulcs) ;
  }

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;

    return ;
  }

  CDialog::OnOK() ;
}

void CLegyKorFlv::OnCancel()
{
  long nRC ;
  CString sAblCim, sSzov ;

  if ( m_bLeggyKorValt == TRUE )
  {
    GetWindowText( sAblCim) ;

    if ( sSzov.LoadString( IDS_LGYK_MGV_S) != TRUE )
    {
      sSzov = "IDS_LGYK_MGV_S" ;
    }

    if ( IDYES==MessageBox( sSzov, sAblCim, MB_ICONQUESTION | MB_YESNO ) )
	  {
      // Nem lehet tudni, hogy van-e mar adat felveve
      nRC = m_cLeggyKor.SQL_LeggyKorINSERT( m_uiSzezonEve, m_uiNdKulcs) ;

      if ( nRC == -1 )
      {
        nRC = m_cLeggyKor.SQL_LeggyKorUPDATE( m_uiSzezonEve, m_uiNdKulcs) ;
      }

      if ( nRC != 0 )
      {
        HibaKiir( nRC) ;

        return ;
      }
    }
  }

  CDialog::OnCancel() ;
}

void CLegyKorFlv::OnSelChgVrs()
{
  CComboBox* pVzoCb = (CComboBox*) GetDlgItem(IDC_LGGY_VERS_CB) ;

  int nKivSor ;

  if ( m_bAblInit == FALSE )
  {
    nKivSor = pVzoCb->GetCurSel() ;

    if ( nKivSor != CB_ERR )
    {
      m_cLeggyKor.NvzlBeall( unsigned(pVzoCb->GetItemData( nKivSor))) ;
      m_bLeggyKorValt = TRUE ;
    }
  }
}

void CLegyKorFlv::OnChgIdoerE()
{
  if ( m_bAblInit == FALSE )
  {
    m_bLeggyKorValt = TRUE ;
  }
}

void CLegyKorFlv::OnChgKorbenE()
{
  if ( m_bAblInit == FALSE )
  {
    m_bLeggyKorValt = TRUE ;
  }
}

void CLegyKorFlv::OnChgAtlSebE()
{
  if ( m_bAblInit == FALSE )
  {
    m_bLeggyKorValt = TRUE ;
  }
}

BOOL CLegyKorFlv::IdoEredmBeo(/*CRBBejegyzes* pBeoIdoerdm*/)
{
  CEdit* pIdoEd = (CEdit*) GetDlgItem(IDC_LEGGY_IDOER_E) ;

  CString sBeoIdoer, sOra, sPerc, sMasodp, sEzrMasodp ;

  // Nyolc szam eleg a mezohatarok (szam<->nem szam) megjegyzesere
  int pHatarok[] = {0, 0, 0, 0, 0, 0, 0, 0} ;
  int poz, hatkpoz ;
  int nOra, nPerc, nMasodp, nEzrMasodp ;
  CIdoeredm cLokIder ;

  pIdoEd->GetWindowText(sBeoIdoer) ;

  // A mezohatarok megallapitasa
  poz = sBeoIdoer.GetLength()-1 ;
  hatkpoz = 7 ;

  if ( poz >= 0 )
  {
    while ( 0 <= poz && !isdigit(sBeoIdoer[poz]) && hatkpoz>=0 )
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
    {// Idoeredmeny
      nEzrMasodp = atoi( (const char*) sEzrMasodp ) ;
      nMasodp    = atoi( (const char*) sMasodp ) ;

      // Ugyan negativ ertekek ugysem fordulhatnak elo (- nem szamjegy)...
      if ( -1000 < nEzrMasodp && nEzrMasodp < 1000 )
      {
        cLokIder.EzrdMsdpBeall(nEzrMasodp) ;
      }
      else
      { // Ha a beolvasott szamertek nem megengedett tartomanyba esik
        return FALSE ;
      }

      // Ugyan negativ ertekek ugysem fordulhatnak elo (- nem szamjegy)...
      if ( -60 < nMasodp && nMasodp < 60 )
      { // Ha a beolvasott szamertek nem megengedett
        cLokIder.MasodpercBeall(nMasodp) ;
      }
      else
      { // Ha a beolvasott szamertek nem megengedett tartomanyba esik
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
        { // Ha a beolvasott szamertek nem megengedett tartomanyba esik
          return FALSE ;
        }
      }

      // A megjegyzes beallitasa
      m_cLeggyKor.IdoerBeall( cLokIder) ;

      m_bLeggyKorValt = TRUE ;

      return TRUE ;
    }

    return FALSE ;
  }

  return FALSE ;
}

BOOL CLegyKorFlv::KorbenBeo()
{
  CEdit* pKorbenEd = (CEdit*) GetDlgItem(IDC_KORBEN_E) ;

  int         nEleje, nVege, nSztrHossz ;
  const char* pszKorben ;
  CString sBeoKorben, sLokKorben ;

  pKorbenEd->GetWindowText( sBeoKorben) ;

  nSztrHossz = sBeoKorben.GetLength() ;

  if ( nSztrHossz == 0 )
    return FALSE ;

  pszKorben = (const char*) sBeoKorben ;

  // A keresendo sztring elejerol, vegerol leszedjuk a szokozt
  for ( nEleje=0 ; nEleje < nSztrHossz && *(pszKorben+nEleje) == ' ' ; nEleje++ ) ;

  // Csupa szokoz
  if ( nEleje == nSztrHossz )
    return FALSE ;
                      // \0
  for ( nVege=nSztrHossz-1 ; nVege >= 0 && *(pszKorben+nVege) == ' ' ; nVege-- ) ;

  // Csupa ' '
  if ( nEleje > nVege )
    return FALSE ;

  sBeoKorben = sBeoKorben.Mid( nEleje, nVege - nEleje + 1) ;
  nSztrHossz = sBeoKorben.GetLength() ;

  sBeoKorben = sBeoKorben.SpanIncluding( "0123456789" ) ;

  if ( sBeoKorben.GetLength() != nSztrHossz )
    return FALSE ;

  nEleje = atoi( (const char*) sBeoKorben) ;
  
  if ( 0 < nEleje && nEleje < 256 )
  {
    m_cLeggyKor.KorbenBeall( (unsigned char) nEleje) ;

    return TRUE ;
  }

  return FALSE ;
/*
  char pszBeoKorben[20] ;

  nSztrHossz = ::GetWindowText( pKorbenEd->m_hWnd, pszBeoKorben, 20) ;

  // Mindegy, hogy hiba miatt jott vissza 0, vagy mert nem irtak be semmit
  if ( nSztrHossz == 0  ) // && GetLastError() != 0
    return FALSE ;

  // A keresendo sztring elejerol, vegerol leszedjuk a szokozt
  for ( nEleje=0 ; nEleje < nSztrHossz && *(pszBeoKorben+nEleje) == ' ' ; nEleje++ ) ;

  // Csupa szokoz
  if ( nEleje == nSztrHossz )
    return FALSE ;
                      // \0
  for ( nVege=nSztrHossz-1 ; nVege >= 0 && *(pszBeoKorben+nVege) == ' ' ; nVege-- ) ;

  // Csupa ' '
  if ( nEleje == nVege )
    return FALSE ;

  // memcpy atlapolodas miatt
  memmove( pszBeoKorben, pszBeoKorben+nEleje, nVege-nEleje) ;

  pszBeoKorben[nVege] = '0' ;

  nVege = strspn( pszBeoKorben, "0123456789") ;

  // Ha a sztring belul tartalmazna mast, mint szamokat
  if ( nVege != (int)strlen( pszBeoKorben) )
    return FALSE ;
  
  nEleje = atoi( pszBeoKorben) ;
  
  if ( 0 < nEleje && nEleje < 256 )
  {
    m_cLeggyKor.KorbenBeall( (unsigned char) nEleje) ;

    return TRUE ;
  }

  return FALSE ;
*/
}

BOOL CLegyKorFlv::AtlSebBeo()
{
  CEdit* pAtlSebEd = (CEdit*) GetDlgItem(IDC_ATLSEB_E) ;

  int    nEleje, nVege, nSztrHossz ;
  double dAtlSeb ;
  char   pszBeoAtlSeb[20] ;

  nSztrHossz = ::GetWindowText( pAtlSebEd->m_hWnd, pszBeoAtlSeb, 20) ;

  // Mindegy, hogy hiba miatt jott vissza 0, vagy mert nem irtak be semmit
  if ( nSztrHossz == 0 /*&& GetLastError() != 0*/ )
    return FALSE ;

  // A keresendo sztring elejerol, vegerol leszedjuk a szokozt
  for ( nEleje=0 ; nEleje < nSztrHossz && *(pszBeoAtlSeb+nEleje) == ' ' ; nEleje++ ) ;

  // Csupa szokoz
  if ( nEleje == nSztrHossz )
    return FALSE ;
                      // \0
  for ( nVege=nSztrHossz-1 ; nVege >= 0 && *(pszBeoAtlSeb+nVege) == ' ' ; nVege-- ) ;

  // Csupa ' '
  if ( nEleje > nVege )
    return FALSE ;

  // memcpy atlapolodas miatt
  memmove( pszBeoAtlSeb, pszBeoAtlSeb+nEleje, nVege-nEleje) ;

  //           nVege + 1 ???  
  pszBeoAtlSeb[nVege+1] = '\0' ;

  nVege = strspn( pszBeoAtlSeb, ".0123456789") ;

  // Ha a sztring belul tartalmazna mast, mint szamokat
  if ( nVege != (int)strlen( pszBeoAtlSeb) )
    return FALSE ;
  
  dAtlSeb = atof( pszBeoAtlSeb) ;
  
  // E ket hatar kozt fogadom el valodinak a beirt sebesseget
  if ( 0.0 < dAtlSeb && dAtlSeb < 500.0 )
  {
    m_cLeggyKor.AtlSebBeall( (float)dAtlSeb) ;

    return TRUE ;
  }

  return FALSE ;
}

void CLegyKorFlv::VnevFeltolt()
{
  CComboBox* pVzoCb = (CComboBox*) GetDlgItem(IDC_LGGY_VERS_CB) ;

  int      nMeret, i;
  unsigned uiAktPoz ;

  CNvzSor* pNvzlSor   ;
  vnevadat *pVNevadat ;
  CString  sLokStr    ;
    
  unsigned  nTombMeret ;
  unsigned* pNvzlTomb  ;
  long nRC ;

  nTombMeret = 0 ;

  nRC = SQL_KulNvzlNdon( m_uiSzezonEve, m_uiNdKulcs, &pNvzlTomb, &nTombMeret) ;

  if ( nRC != 0 )
  {
    HibaKiir( nRC) ;
  }

  for ( uiAktPoz=0 ; uiAktPoz < nTombMeret ; uiAktPoz++ )
  {
    if ( m_pAktNvzLst->Lookup( pNvzlTomb[uiAktPoz], pNvzlSor) == TRUE )
    {
      if ( m_pVersenyzo->Lookup( pNvzlSor->uiVersenyzo, pVNevadat) == TRUE )
      {
        sLokStr = pVNevadat->m_sVezNev + CString(", ") + pVNevadat->m_sKerNev ;
        pVzoCb->SetItemData( pVzoCb->AddString(sLokStr), DWORD(pNvzlTomb[uiAktPoz]) ) ;
      }
    }
  }

  // A nvzl tomb nem kell mar
  if ( nTombMeret >= 0 )
  {
    delete [] pNvzlTomb ;
  }

  // A m_uiNvzlKulcs-ben levo nvzlst kulccsal azonositott v.-zo  kijelolese
  nMeret = pVzoCb->GetCount() ;

  if ( nMeret != CB_ERR )
  {
    for ( i = 0 ; i < nMeret ; i++ )
    {
      if ( m_cLeggyKor.Nvzl() == (unsigned)pVzoCb->GetItemData(i) )
      {
        pVzoCb->SetCurSel(i) ;
        break ;
      }
    }
  }
} 

void CLegyKorFlv::HibaKiir(long nRC)
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