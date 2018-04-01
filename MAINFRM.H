// mainfrm.h : interface of the CMainFrame class
//
/////////////////////////////////////////////////////////////////////////////

class CMainFrame : public CMDIFrameWnd
{
	DECLARE_DYNAMIC(CMainFrame)
public:
	CMainFrame();

// Attributes
public:

// Operations
public:

// Implementation
protected:  // control bar embedded members
	CStatusBar  m_wndStatusBar;
	CToolBar    m_wndToolBar;

// Generated message map functions
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnFelvitelMotornv();
	afx_msg void OnFelvitelNagydjnv();
	afx_msg void OnFelvitelCsapat();
	afx_msg void OnFelvitelNemzetisgnv();
	afx_msg void OnFelvitelPlyanv();
	afx_msg void OnFelvitelVersenyznv();
	afx_msg void OnFelvitelPontszmok();
	afx_msg void OnFelvitelSzezon();
	afx_msg void OnFelvitelNvzlst();
	afx_msg void OnFelvitelSzezNdV();
	afx_msg void OnLekerdVersenyznv();  
	afx_msg void OnLekerdNemzetisgnv();
	afx_msg void OnLekerdCsapat();
	afx_msg void OnLekerdMotornv();
	afx_msg void OnLekerdNagydjnv();
	afx_msg void OnLekerdBetoltes();
	afx_msg void OnUpdateFelvitelRsBef(CCmdUI* pCmdUI);
	afx_msg void OnCsatlAdatb();
	afx_msg void OnUpdateCsatlAdatb(CCmdUI* pCmdUI);
	afx_msg void OnAppExit();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

public:
	virtual ~CMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif	
};