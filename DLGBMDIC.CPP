// dlgbmdic.cpp

#include "stdafx.h"
// #include "ujdoklet.h"

#include "resource.h"
#include "dlgbmdic.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDlBarMDIChFrm construction/destruction

IMPLEMENT_DYNCREATE(CDlBarMDIChFrm, CMDIChildWnd)

BEGIN_MESSAGE_MAP(CDlBarMDIChFrm, CMDIChildWnd)
	//{{AFX_MSG_MAP(CDlBarMDIChFrm)
   	ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CDlBarMDIChFrm::CDlBarMDIChFrm()
{
	// TODO: add construction code here
}

CDlBarMDIChFrm::~CDlBarMDIChFrm()
{
}

int CDlBarMDIChFrm::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
  if (CMDIChildWnd::OnCreate(lpCreateStruct) == -1)
    return -1 ;

  if (!m_wndDlgBar.Create(this, IDD_LEK_ER_MENT, CBRS_TOP, AFX_IDW_TOOLBAR))
  {
    TRACE("Failed to create DlgBar\n");
    return -1;      // fail to create
  }

  return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CDlBarMDIChFrm diagnostics

#ifdef _DEBUG
void CDlBarMDIChFrm::AssertValid() const
{
	CMDIChildWnd::AssertValid();
}

void CDlBarMDIChFrm::Dump(CDumpContext& dc) const
{
	CMDIChildWnd::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CDlBarMDIChFrm message handlers
