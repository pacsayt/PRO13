// dlgbmdic.h

class CDlBarMDIChFrm : public CMDIChildWnd
{
public:
    DECLARE_DYNCREATE(CDlBarMDIChFrm)

public: // create from serialization only
    CDlBarMDIChFrm() ;
    BOOL Create(LPCSTR lpszWindowName,
                DWORD dwStyle = WS_CHILD | WS_VISIBLE | WS_OVERLAPPEDWINDOW,
                const RECT& rect = rectDefault,
                CMDIFrameWnd* pParentWnd = NULL) ;

// Attributes
public:
    CDialogBar m_wndDlgBar ;

// Operations
public:

// Implementation
public:
	virtual ~CDlBarMDIChFrm();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Generated message map functions
protected:
	//{{AFX_MSG(CUjdokletView2)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
    afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct) ;
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
