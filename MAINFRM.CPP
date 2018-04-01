// mainfrm.cpp : implementation of the CMainFrame class
//

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"

#include "csatlako.h"
#include "csatldlg.h"

#include "cnev.h"      // A neveket es kulcsokat osszerendelo adatstruktura
#include "cvnev.h"     // A versenyzo neveket es kulcsokat osszerendelo adatstruktura
#include "nevfelv.h"
#include "vnevfelv.h"

#include "pontozas.h"
#include "pontfelv.h"

#include "cszezon.h"
#include "nevzlist.h"

#include "idoeredm.h"
#include "rajtsbef.h"

#include "impgyabl.h"
#include "rsbeffel.h"

#include "kerermez.h"
#include "fgyeelgy.h"
#include "holvkiv.h"
#include "pillall.h"
#include "vbszerep.h"

#include "lkmgjvaz.h"
#include "lek1doc.h"

#include "szezfelv.h"
#include "nvzlistf.h"
#include "szezndva.h"
#include "lekerddl.h"

#include "proba13.h"

#include "mainfrm.h"


#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

IMPLEMENT_DYNAMIC(CMainFrame, CMDIFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_COMMAND(ID_FELVITEL_MOTORNV, OnFelvitelMotornv)
	ON_COMMAND(ID_FELVITEL_NAGYDJNV, OnFelvitelNagydjnv)
	ON_COMMAND(ID_FELVITEL_CSAPAT, OnFelvitelCsapat)
	ON_COMMAND(ID_FELVITEL_NEMZETISGNV, OnFelvitelNemzetisgnv)
	ON_COMMAND(ID_FELVITEL_PLYANV, OnFelvitelPlyanv)
	ON_COMMAND(ID_FELVITEL_VERSENYZNV, OnFelvitelVersenyznv)
  ON_COMMAND(ID_FELVITEL_PONTSZMOK, OnFelvitelPontszmok)
	ON_COMMAND(ID_FELVITEL_SZEZON, OnFelvitelSzezon)
	ON_COMMAND(ID_FELVITEL_NVZLST, OnFelvitelNvzlst)
	ON_COMMAND(ID_FELVITEL_RS_BEF, OnFelvitelSzezNdV)
	ON_COMMAND(ID_LEKERD_VERSENYZNV,OnLekerdVersenyznv)
	ON_COMMAND(ID_LEKERD_NEMZETISGNV, OnLekerdNemzetisgnv)
	ON_COMMAND(ID_LEKERD_CSAPAT, OnLekerdCsapat)
	ON_COMMAND(ID_LEKERD_MOTORNV, OnLekerdMotornv)
	ON_COMMAND(ID_LEKERD_NAGYDJNV, OnLekerdNagydjnv)
	ON_COMMAND(ID_FILE_BETOLTES, OnLekerdBetoltes)
	ON_UPDATE_COMMAND_UI(ID_FELVITEL_RS_BEF, OnUpdateFelvitelRsBef)
	ON_COMMAND(ID_CSATL_ADATB, OnCsatlAdatb)
	ON_UPDATE_COMMAND_UI(ID_CSATL_ADATB, OnUpdateCsatlAdatb)
	ON_COMMAND(ID_APP_EXIT, OnAppExit)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// arrays of IDs used to initialize control bars

// toolbar buttons - IDs are command buttons
static UINT BASED_CODE buttons[] =
{
  // same order as in the bitmap 'toolbar.bmp'
  
  ID_APP_EXIT,
    ID_SEPARATOR,
  ID_CSATL_ADATB,
    ID_SEPARATOR,
  ID_FILE_PRINT,
  ID_APP_ABOUT,
};

static UINT BASED_CODE indicators[] =
{
  ID_SEPARATOR,           // status line indicator
  ID_INDICATOR_CAPS,
  ID_INDICATOR_NUM,
  ID_INDICATOR_SCRL,
};

/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

CMainFrame::CMainFrame()
{
}

CMainFrame::~CMainFrame()
{
}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CMDIFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

	if (!m_wndToolBar.Create(this) ||
		!m_wndToolBar.LoadBitmap(IDR_MAINFRAME) ||
		!m_wndToolBar.SetButtons(buttons,
		  sizeof(buttons)/sizeof(UINT)))
	{
		TRACE("Failed to create toolbar\n");
		return -1;      // fail to create
	}

	if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE("Failed to create status bar\n");
		return -1;      // fail to create
	}

	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CMDIFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CMDIFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers

void CMainFrame::OnCsatlAdatb() 
{
  CCsatlDlg CsatlAbl ;

  // Allitja a gl_bCsatlakozva valtozot
  CsatlAbl.DoModal() ;
}

void CMainFrame::OnAppExit() 
{
  CProba13App* pProba13App = (CProba13App*)AfxGetApp() ;

  SQL_Kilepes() ;  

  //pProba13App->OnAppExit() ;
  SendMessage( WM_CLOSE) ; // CWnd::OnClose ?
}

void CMainFrame::OnFelvitelCsapat()
{
  CNevFelv NevFelvAbl( eCsapNt) ;

  NevFelvAbl.DoModal() ;
}

void CMainFrame::OnFelvitelMotornv()
{
  CNevFelv NevFelvAbl( eMotorNt) ;

  NevFelvAbl.DoModal() ;
}

void CMainFrame::OnFelvitelNagydjnv()
{
  CNevFelv NevFelvAbl( eNagydijNt) ;

  NevFelvAbl.DoModal() ;
}

void CMainFrame::OnFelvitelNemzetisgnv()
{
  CNevFelv NevFelvAbl( eNemzNt) ;

  NevFelvAbl.DoModal() ;
}

void CMainFrame::OnFelvitelPlyanv()
{
  CNevFelv NevFelvAbl( ePalyaNt) ;

  NevFelvAbl.DoModal() ;
}

void CMainFrame::OnFelvitelPontszmok()
{
  CPontFelv PontFelvAbl ;
  
  PontFelvAbl.DoModal() ;
}

void CMainFrame::OnFelvitelVersenyznv()
{
  CVNevFelv VNevFelvAbl ;

  VNevFelvAbl.DoModal() ;
}

void CMainFrame::OnFelvitelSzezon()
{
  CSzezFelv CSzezFelvAbl ;

  CSzezFelvAbl.DoModal() ;
}

// Mivel a memoriaban az adott nevezesi listak nincsenek bent, es modositas
// (uj felvetel) eseten maga az ablak gondoskodik a nevezesi lista es az azevi
// szezon menteserol, a dialogus ablak konstruktoranak nem adok at semmit.
void CMainFrame::OnFelvitelNvzlst()
{
  CNvzListFelv  cNvzLstFlvAbl ;

  cNvzLstFlvAbl.DoModal() ;
}

// A szezon + nagydij + r/b kivalasztasat biztosito dialogus ablak
// Mivel az utana kovetkezo (altala felhivott) ablakba ezek az informaciok
// mar nem fertek el.	
void CMainFrame::OnFelvitelSzezNdV()
{
  // A menuben kikapcsolom az ezt a fv-t meghivasat eredmenyezo menupontot
  gl_bFelvitelRsBefAktiv = FALSE ;

  new CSzezNdVal( this) ;
}

void CMainFrame::OnLekerdVersenyznv()
{
  CLekerdDlg cLekerdAbl( eVersNt) ;

  cLekerdAbl.DoModal() ;
}

void CMainFrame::OnLekerdNemzetisgnv()
{
  CLekerdDlg cLekerdAbl( eNemzNt) ;

  cLekerdAbl.DoModal() ;
}

void CMainFrame::OnLekerdCsapat()
{
  CLekerdDlg cLekerdAbl( eCsapNt) ;

  cLekerdAbl.DoModal() ;
}

void CMainFrame::OnLekerdMotornv()
{
  CLekerdDlg cLekerdAbl( eMotorNt) ;

  cLekerdAbl.DoModal() ;
}

void CMainFrame::OnLekerdNagydjnv()
{
  CLekerdDlg cLekerdAbl( eNagydijNt) ;

  cLekerdAbl.DoModal() ;
}

void CMainFrame::OnLekerdBetoltes()
{
/*
  CString      sDocNev("CLekEredm1Doc") ;
  CProba13App* pPro13App = (CProba13App*) AfxGetApp() ;
  CDocument*   pLekEr1Doc = pPro13App->DocViewFrameLetr(sDocNev) ;

  // Ellenorizzuk, hogy a megfelelo osztalytipust sikerult-e
  // letrehozni
  if ( pLekEr1Doc->IsKindOf(RUNTIME_CLASS(CLekEredm1Doc)) )
  {
    if ( ((CLekEredm1Doc*)pLekEr1Doc)->Betolt() == FALSE )
    {
      ((CLekEredm1Doc*)pLekEr1Doc)->OnCloseDocument() ;
      return ;
    }

    pLekEr1Doc->UpdateAllViews( NULL) ;

    // A dokumentum cimsoranak beallitasa az ablak tagvaltozoi szerint
    ((CLekEredm1Doc*)pLekEr1Doc)->DokuNevKiir() ;
  }
*/
}

/*
void CMainFrame::OnUpdateFelvitelNvzlst(CCmdUI* pCmdUI)
{
  TRACE("CMainFrame::OnUpdateFelvitelNvzlst") ;
}
*/
void CMainFrame::OnUpdateFelvitelRsBef(CCmdUI* pCmdUI)
{
  pCmdUI->Enable( gl_bFelvitelRsBefAktiv) ;
}

void CMainFrame::OnUpdateCsatlAdatb(CCmdUI* pCmdUI) 
{
  // ha TRUE ne probaljon meg megegyszer belepni
  pCmdUI->Enable( !gl_bCsatlakozva) ;
}