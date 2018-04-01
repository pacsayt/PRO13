// proba13.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"

#include "csatldlg.h"


#include "cnev.h"      // A neveket es kulcsokat osszerendelo adatstruktura
#include "cvnev.h"     // A versenyzo neveket es kulcsokat osszerendelo adatstruktura

#include "pontozas.h"

#include "cszezon.h"
#include "nevzlist.h"
#include "idoeredm.h"
#include "rajtsbef.h"

#include "kerermez.h"
#include "fgyeelgy.h"
#include "holvkiv.h"
#include "pillall.h"
#include "vbszerep.h"

#include "lkmgjvaz.h"
#include "lek1doc.h"
#include "lek1view.h"
#include "dlgbmdic.h"

#include "proba13.h"

#include "mainfrm.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProba13App

BEGIN_MESSAGE_MAP(CProba13App, CWinApp)
	//{{AFX_MSG_MAP(CProba13App)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
	// Standard print setup command
	ON_COMMAND(ID_FJL_PRINTSETUP, CWinApp::OnFilePrintSetup)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProba13App construction

CProba13App::CProba13App() : m_cCsapatNev(eCsapNt, 10),
                             m_cMotorNev(eMotorNt, 10),
                             m_cNagydijNev(eNagydijNt, 10),
                             m_cNemzetisegNev(eNemzNt, 10),
                             m_cPalyaNev(ePalyaNt, 10)
{
}

///////////////////////////////
// G L O B A L I S  A D A T O K
///////////////////////////////
// The one and only CProba13App object

CProba13App NEAR theApp;

/////////////////////////////////////////////////////////////////////////////
// CProba13App initialization

BOOL CProba13App::InitInstance()
{
  CCsatlDlg CsatlAbl ;
  
  CString sFileNev ;

  // Standard initialization
  // If you are not using these features and wish to reduce the size
  //  of your final executable, you should remove from the following
  //  the specific initialization routines you do not need.

  SetDialogBkColor();        // Set dialog background color to gray

//    LoadStdProfileSettings();  // Load standard INI file options (including MRU) VC++1.52-ben nem hiba

  // Register the application's document templates.  Document templates
  //  serve as the connection between documents, frame windows and views.

  CMultiDocTemplate* pDocTemplate;

  pDocTemplate = new CMultiDocTemplate(
    IDR_LEK_DOC_TEMPL_1,
    RUNTIME_CLASS(CLekEredm1Doc),
    RUNTIME_CLASS(CDlBarMDIChFrm), // sajat MDI child frame, dialog bar-ral
    RUNTIME_CLASS(CLekEredm1View));
  AddDocTemplate(pDocTemplate);

	SetDialogBkColor();        // Set dialog background color to gray
	LoadStdProfileSettings();  // Load standard INI file options (including MRU)

	// create main MDI Frame window
	CMainFrame* pMainFrame = new CMainFrame;
	if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
		return FALSE;
	m_pMainWnd = pMainFrame;

/*
	if (m_lpCmdLine[0] != '\0')
	{
		// TODO: add command line processing here
	}
*/

  // The main window has been initialized, so show and update it.
  pMainFrame->ShowWindow(m_nCmdShow);
  pMainFrame->UpdateWindow();


  // A nevek betoltese elott azert hozza is kell kapcsolodni az adatbazishoz
  // Allitja a gl_bCsatlakozva valtozot
  CsatlAbl.DoModal() ;

  if ( gl_bCsatlakozva == TRUE )
  {
    // A neveket es kulcsokat osszerendelo adatstrukturak (CNev)
    // feltoltese
    m_cCsapatNev.SQL_NevBetolt()     ;
    m_cMotorNev.SQL_NevBetolt()      ;
    m_cNagydijNev.SQL_NevBetolt()    ;
    m_cNemzetisegNev.SQL_NevBetolt() ;
    m_cPalyaNev.SQL_NevBetolt()      ;
    m_cVersenyzoNev.SQL_VNevBetolt() ;
    m_cPontozas.SQL_PontBetolt()     ;

    return TRUE ;
  }

  return FALSE ;
}

CDocument* CProba13App::DocViewFrameLetr(CString& sTemplNev)
{
  POSITION      poz ;
  CString       strTypeName ;
  CDocTemplate* pTemplate ;

  CDocument* pDoc ;

  CMDIChildWnd* pFrame ;

  BOOL bAutoDelete ;

  // Ures nevvel nem lesz, es kesobb hibat okozna
  // if ( strTypeName == sTemplNev
  // biztos, ami biztos: && pTemplate != NULL ) beteve
  if ( sTemplNev == "" )
  {
    return NULL ;
  }

//  POSITION poz = m_templateList.GetHeadPosition() ; VC++ 1.52
  poz = m_pDocManager->GetFirstDocTemplatePosition() ;
  // A megfelelo template kivalasztasa :
  // A sztring a *.rc file template kostrualasakor megadott azonositoju 
  // sztring eroforras elso \n-kel elvalasztott mezoje (windowTitle). 
  // (Talan ennek hasznalata zavar legkevesbe masutt.)
  while ( poz != NULL && strTypeName != sTemplNev )
  {
//    pTemplate = (CDocTemplate*)m_templateList.GetNext(poz) ;
    pTemplate = m_pDocManager->GetNextDocTemplate( poz) ;
    ASSERT(pTemplate->IsKindOf(RUNTIME_CLASS(CDocTemplate))) ;

    pTemplate->GetDocString(strTypeName, CDocTemplate::windowTitle) ;
//     TRACE("windowTitle : %s\n", (const char*) strTypeName) ;
  }

  // Egy uj dokumentum letrehozasa
  if ( strTypeName == sTemplNev && pTemplate != NULL )
  {
    pDoc = (CDocument*) pTemplate->CreateNewDocument() ;

    if ( pDoc == NULL )
    {
//       TRACE("CUjdokletDoc1 letrehozasa meghiusult !") ;
      return NULL ;
    }

    bAutoDelete = pDoc->m_bAutoDelete ;

    pDoc->m_bAutoDelete = FALSE ;

    pFrame = (CMDIChildWnd*) pTemplate->CreateNewFrame( pDoc, NULL) ;

    pDoc->m_bAutoDelete = bAutoDelete ;

    if ( pFrame == NULL )
    {
//       TRACE("CMDIChildWnd letrehozasa meghiusult !") ;
      delete pDoc ;
      return NULL ;
    }

    //                                MakeVisible \/
    pTemplate->InitialUpdateFrame( pFrame, pDoc, TRUE) ;

    return pDoc ;
  }

  return NULL ;
}

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

// Implementation
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//{{AFX_MSG(CAboutDlg)
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

// App command to run the dialog
void CProba13App::OnAppAbout()
{
	CAboutDlg aboutDlg;

  CMapStringToString sMap() ;
  CString cString1 ;
  CString cTalalat ;

  cString1 = "egy" ;
  sMap.SetAt( "1", cString1) ;

  cString1 = "ketto" ;
  sMap.SetAt( "2", cString1) ;

  cString1 = "harom" ;
  sMap.SetAt( "3", cString1) ;

	aboutDlg.DoModal();
}

/////////////////////////////////////////////////////////////////////////////
// CProba13App commands
