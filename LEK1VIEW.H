// lek1view.h : interface of the CUjdokletView class
//
/////////////////////////////////////////////////////////////////////////////

class CLekEredm1View : public CScrollView
{
protected: // create from serialization only
	CLekEredm1View();
	DECLARE_DYNCREATE(CLekEredm1View)

// Attributes
public:
	CLekEredm1Doc* GetDocument();
protected:
   HCURSOR m_hRegiCursor ;

// Operations
public:

// Implementation
public:
    virtual void OnUpdate(CView* pKuldo, LPARAM lHint, CObject* pHint) ;
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view

protected:

	// Printing support
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
    int LekCimNyomt(CDC* pDC) ;

// Generated message map functions
protected:
	//{{AFX_MSG(CLekEredm1View)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
public:
	virtual ~CLekEredm1View();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
} ;

#ifndef _DEBUG  // debug version in ujdokvw.cpp
inline CLekEredm1Doc* CLekEredm1View::GetDocument()
   { return (CLekEredm1Doc*)m_pDocument; }
#endif

