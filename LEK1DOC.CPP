// lek1doc.cpp : implementation of the CLekEredm1Doc class
//

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"
#include "idoeredm.h"

#include "cnev.h"
#include "cvnev.h"
#include "pontozas.h"
//#include "nyvbossz.h"
#include "cszezon.h"
#include "nevzlist.h"
#include "rajtsbef.h"

// #include "kereredm.h"
#include "kerermez.h"
#include "fgyeelgy.h"
#include "holvkiv.h"
#include "pillall.h"
#include "vbszerep.h"

#include "lkmgjvaz.h"

#include "lek1doc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1Doc

IMPLEMENT_DYNCREATE(CLekEredm1Doc, CDocument)

BEGIN_MESSAGE_MAP(CLekEredm1Doc, CDocument)
    //{{AFX_MSG_MAP(CLekEredm1Doc)
    ON_COMMAND( IDC_LK_ER_MNT_B, OnMentes)
    ON_COMMAND( ID_FILE_MENTES, OnMentes)
    //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1Doc construction/destruction

CLekEredm1Doc::CLekEredm1Doc()
{
/*
    m_sSzoveg = "CLekEredm1Doc" ;
	m_bFolotte= FALSE ;
	m_bBeKi   = FALSE ;
    m_rTeglalap.SetRect( 0, 20, 100, 40) ;
*/
  m_enLekNevTip = eErvtlnNt ;
  m_enLekTip    = eErvtlnLt ;
  m_usSzezontol = 0 ;
  m_usSzezonig  = 0 ;

  m_cDokMeret = CSize( 50, 1000) ;
}

CLekEredm1Doc::~CLekEredm1Doc()
{
}

BOOL CLekEredm1Doc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}

BOOL CLekEredm1Doc::Betolt()
{
  CFileStatus statusz ;

  CString cFileTip, sTeljesFilenev ;
  cFileTip.LoadString( IDS_MNT_LEK_TIP) ;
  // TRUE : File Open, FALSE : File Save As
  CFileDialog dlgLekerMent(TRUE, "mlk", NULL,
                  OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY,
                  (const char*) cFileTip);

  dlgLekerMent.m_ofn.lpstrInitialDir = /*(const char*)*/ sAlapkonyvtar ;

  if (dlgLekerMent.DoModal() != IDOK)
    return FALSE ;

//-----------------------------------------------------------------------------
  sTeljesFilenev = dlgLekerMent.GetPathName() ;

  if (CFile::GetStatus(sTeljesFilenev, statusz))
  { // A file letezik: megnyitas olvasasra
    CFile fLekEr(sTeljesFilenev, CFile::modeRead) ;
    CArchive aLekEr(&fLekEr, CArchive::load) ;

    Serialize(aLekEr) ;

    aLekEr.Close() ;
    fLekEr.Close() ;
  }
  else
  { // A file nem letezik, vagy masutt: hibauzenet
    return FALSE ;
  }
//-----------------------------------------------------------------------------

  return TRUE ;
}
void CLekEredm1Doc::TipSzezBeall(eNevtipus enLekNevTip, eLekerdTipus enLekTip,
                                 unsigned short usSzezontol, unsigned short usSzezonig)
{
  m_enLekNevTip = enLekNevTip ;
  m_enLekTip    = enLekTip    ;
  m_usSzezontol = usSzezontol ;
  m_usSzezonig  = usSzezonig  ;
}

CString CLekEredm1Doc::DokuNev()
{
  char pszEv[15] ;
  CString sNev, sDokumentCim ;

  switch ( m_enLekNevTip )
  {
    case eVersNt    :
      if ( !sNev.LoadString(IDS_VERSENYZNV_SZOV) )
      {
        sNev = "IDS_VERSENYZNV_SZOV" ;
      }

      break ;

    case eNemzNt    :
      if ( !sNev.LoadString(IDS_NEMZETSGNV_SZOV) )
      {
        sNev = "IDS_NEMZETSGNV_SZOV" ;
      }

        break ;

    case eCsapNt    :
      if ( !sNev.LoadString(IDS_CSAPAT_SZOV) )
      {
        sNev = "IDS_CSAPAT_SZOV" ;
      }

      break ;

    case eMotorNt   :
      if ( !sNev.LoadString(IDS_MOTORNV_SZOV) )
      {
        sNev = "IDS_MOTORNV_SZOV" ;
      }

      break ;

    case eNagydijNt :
      if ( !sNev.LoadString(IDS_NAGYDJNV_SZOV) )
      {
        sNev = "IDS_NAGYDJNV_SZOV" ;
      }

      break ;

    default : // Minden egyeb (pl. a nagydijnev is) hiba !
      if ( !sNev.LoadString(IDS_ISMLEN_NEVT_SZOV ) )
      {
        sNev = "IDS_ISMLEN_NEVT_SZOV" ;
      }
  } // switch : nevtipus

  sDokumentCim = sNev + " " ;

  switch ( m_enLekTip )
  {
    case eVBSzer    :
      if ( !sNev.LoadString(IDS_VB_SZEREP_SZOV) )
      {
        sNev = "IDS_VB_SZEREP_SZOV" ;
      }

      break ;

    case eFutgy     :
      if ( !sNev.LoadString(IDS_FUT_GYOZ_SZOV) )
      {
        sNev = "IDS_FUT_GYOZ_SZOV" ;
      }

      break ;

    case eEdzEls    :
      if ( !sNev.LoadString(IDS_EDZ_ELS_SZOV) )
      {
        sNev = "IDS_EDZ_ELS_SZOV" ;
      }

      break ;

    case eLeggyKor  :
      if ( !sNev.LoadString(IDS_LEGGY_KOR_SZOV) )
      {
        sNev = "IDS_LEGGY_KOR_SZOV" ;
      }

      break ;

    case eOsszEr    :
      if ( !sNev.LoadString(IDS_OSSZ_ER_SZOV) )
      {
        sNev = "IDS_OSSZ_ER_SZOV" ;
      }

      break ;

    case eHolvers   :
      if ( !sNev.LoadString(IDS_HOL_VERS_SZOV) )
      {
        sNev = "IDS_HOL_VERS_SZOV" ;
      }

      break ;

    case eVBPillAll :
      if ( !sNev.LoadString(IDS_VB_PILL_ALL_SZOV) )
      {
        sNev = "IDS_VB_PILL_ALL_SZOV" ;
      }

      break ;

    default :
      if ( !sNev.LoadString(IDS_ISMLEN_LEK_SZOV ) )
      {
        sNev = "IDS_ISMLEN_LEK_SZOV" ;
      }
  }

  sDokumentCim += sNev ;

  sprintf( pszEv, " %d - %d", m_usSzezontol, m_usSzezonig) ;

  sDokumentCim += pszEv ;

  return sDokumentCim ;
}

void CLekEredm1Doc::DokuNevKiir()
{
  SetTitle( DokuNev()) ;
}

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1Doc serialization

void CLekEredm1Doc::Serialize(CArchive& ar)
{
  CDocument::Serialize(ar) ;

  m_cLekErMegjVaz.Serialize(ar) ;

  if (ar.IsStoring())
  {
    // An enumerator can be promoted to an integer value.
    // However, converting an integer to an enumerator requires an explicit cast,
    // and the results are not defined.

    ar << (int)m_enLekNevTip ;
    ar << (int)m_enLekTip    ;
/*
    ar.Write( &m_usLekNevtip, 2) ;
    ar.Write( &m_usLekTip,    2) ;
*/
    ar.Write( &m_usSzezontol, 2) ;
    ar.Write( &m_usSzezonig,  2) ;
  }
  else
  {
    ar >> (int&)m_enLekNevTip ;
    ar >> (int&)m_enLekTip    ;

/*
    ar.Read( &m_usLekNevtip, 2) ;
    ar.Read( &m_usLekTip,    2) ;
*/
    ar.Read( &m_usSzezontol, 2) ;
    ar.Read( &m_usSzezonig,  2) ;
  }
}

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1Doc diagnostics

#ifdef _DEBUG
void CLekEredm1Doc::AssertValid() const
{
	CDocument::AssertValid();
}

void CLekEredm1Doc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1Doc commands

void CLekEredm1Doc::OnMentes()
{
  CString cFileTip, sTeljesFilenev ;
  cFileTip.LoadString( IDS_MNT_LEK_TIP) ;
  // TRUE : File Open, FALSE : File Save As      
  CFileDialog dlgLekerMent(FALSE, "mlk", NULL,
                  OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY,
                  (const char*) cFileTip);

  dlgLekerMent.m_ofn.lpstrInitialDir = /*(const char*)*/ sAlapkonyvtar ;

  if (dlgLekerMent.DoModal() != IDOK)
    return ;
//-----------------------------------------------------------------------------
  sTeljesFilenev = dlgLekerMent.GetPathName() ;
/*
  if ( CFile::GetStatus(sTeljesFilenev, statusz) )
  { // rc != 0 => a file letezik
  }
*/

  CFile fLekEr(sTeljesFilenev, CFile::modeCreate | CFile::modeWrite) ;
  CArchive aLekEr(&fLekEr, CArchive::store) ;

  Serialize(aLekEr) ;
      
  aLekEr.Close() ;
  fLekEr.Close() ;            
//-----------------------------------------------------------------------------
}

/*
OPENFILENAME ofn;
char szDirName[256];
char szFile[256], szFileTitle[256];
UINT  i, cbString;
char  chReplace;    // string separator for szFilter
char  szFilter[256];
HFILE hf;

// Get the system directory name, and store in szDirName 

GetSystemDirectory(szDirName, sizeof(szDirName));
szFile[0] = '\0';

if ((cbString = LoadString(hinst, IDS_FILTERSTRING,
        szFilter, sizeof(szFilter))) == 0)
{
  ErrorHandler();
  return 0L;
}
// A '|'-k kicserelese '|'-re
chReplace = szFilter[cbString - 1]; // retrieve wildcard


for (i = 0; szFilter[i] != '\0'; i++)
{
  if (szFilter[i] == chReplace)
    szFilter[i] = '\0';
}

// Set all structure members to zero. 

memset(&ofn, 0, sizeof(OPENFILENAME));

ofn.lStructSize = sizeof(OPENFILENAME);
ofn.hwndOwner = hwnd;
ofn.lpstrFilter = szFilter;
ofn.nFilterIndex = 1;
ofn.lpstrFile= szFile;
ofn.nMaxFile = sizeof(szFile);
ofn.lpstrFileTitle = szFileTitle;
ofn.nMaxFileTitle = sizeof(szFileTitle);
ofn.lpstrInitialDir = szDirName;
ofn.Flags = OFN_SHOWHELP | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;


if (GetOpenFileName(&ofn)) {
    hf = _lopen(ofn.lpstrFile, OF_READ);
        .
        . // Perform file operations.
        .
}
else
    ErrorHandler();

The string referred to by the IDS_FILTERSTRING constant in the preceding example is defined as follows in the resource-definition file: 



STRINGTABLE
BEGIN
 IDS_FILTERSTRING  "Write Files(*.WRI)|*.wri|Word Files(*.DOC)|*.doc|"
END
*/