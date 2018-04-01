// impgyabl.h : interface of the CUjdokletView class
//
/////////////////////////////////////////////////////////////////////////////

#define BETUMERET 10

class CImportGyAbl : public CMDIChildWnd
{
public : // create from serialization only
	CImportGyAbl() ;
	DECLARE_DYNCREATE(CImportGyAbl)

// Attributes
public:

    BOOL Create( LPCSTR lpszClassName, LPCSTR lpszWindowName,
                 DWORD dwStyle=WS_CHILD|WS_VISIBLE|WS_OVERLAPPEDWINDOW|WS_VSCROLL|WS_MAXIMIZE|FWS_ADDTOTITLE,
                 const RECT& rect = rectDefault, CMDIFrameWnd* pParentWnd = NULL,
                 CCreateContext* pContext = NULL ) ;

    virtual BOOL LoadFrame( UINT nIDResource = IDR_IMPORTALAS, DWORD dwDefaultStyle =
                            WS_CHILD|WS_OVERLAPPEDWINDOW|WS_VISIBLE|WS_VSCROLL|WS_MAXIMIZE|FWS_ADDTOTITLE,
                            CWnd* pParentWnd = NULL,
                            CCreateContext* pContext = NULL ) ;

public :
    CStringArray m_Sorok ;

protected :

    CFont m_cFont ;
    // GetTextMetrics GetClientRect
    int   m_nElsoSor     ;
    int   m_nSormagassag ;
    RECT  m_rKepernyoMer ;
    // Az invertaltan kiirando sor (1 db) = -1 : nincs
    int   m_nInverzSor   ;

    // message handlers
	//{{AFX_MSG(CImportGyAbl)
	afx_msg void OnPaint() ;
	afx_msg void OnSize(UINT nType, int cx, int cy) ;
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) ;
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

public :    
    // A megadott sort inverzen rajzolja ki
    BOOL SorInvertal( int nInvSor) ;

    // Mivel az ablakot new konstr.-ral hozom letre, ebben a fv.-ben adom ki
    // a this pointerre a delete-t
//    virtual void PostNcDestroy() ;

    ~CImportGyAbl() ;
} ;