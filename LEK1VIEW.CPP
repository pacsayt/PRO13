// lek1view.cpp : implementation of the CLekEredm1View class
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

#include "kerermez.h"
#include "fgyeelgy.h"
#include "holvkiv.h"
#include "pillall.h"
#include "vbszerep.h"

#include "lkmgjvaz.h"

#include "lek1doc.h"
#include "lek1view.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1View

IMPLEMENT_DYNCREATE(CLekEredm1View, CScrollView)

BEGIN_MESSAGE_MAP(CLekEredm1View, CScrollView)
	//{{AFX_MSG_MAP(CLekEredm1View)
	ON_WM_LBUTTONDOWN()
	ON_WM_MOUSEMOVE()
	ON_WM_SETCURSOR()
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND(ID_FJL_PRINT, CView::OnFilePrint)
	ON_COMMAND(ID_FJL_PRINTPREVIEW, CView::OnFilePrintPreview)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1View construction/destruction

CLekEredm1View::CLekEredm1View()
{
	// TODO: add construction code here
}

CLekEredm1View::~CLekEredm1View()
{
}

void CLekEredm1View::OnUpdate(CView* pKuldo, LPARAM lHint, CObject* pHint)
{
//TRACE("\nCLekEredm1View::OnUpdate : BEGIN") ;

  int x = 3, y = 100 ;

  GetDocument()->m_cLekErMegjVaz.ScrollMeret( x, y) ;

  SetScrollSizes( MM_TEXT, CSize( 8*x, 13*y)) ;

  // We can't interpret the hint, so assume that anything might
  // have been updated.
  Invalidate(TRUE) ;

//TRACE("\nCLekEredm1View::OnUpdate : END") ;
}
/////////////////////////////////////////////////////////////////////////////
// CLekEredm1View drawing

void CLekEredm1View::OnDraw(CDC* pDC)
{
//TRACE("\nCLekEredm1View::OnDraw : BEGIN") ;
//  CProba13App* pProba13App = (CProba13App*)AfxGetApp() ;

  CLekEredm1Doc* pDoc = GetDocument() ;
  ASSERT_VALID(pDoc) ;

  int nBkMode, nCimMagass = 0 ;
  // Lehet, hogy tagvaltozonak kene definialni
  int nKezdopY = 0 ;
  CSize cMeret ;

  CFont cFont, *pRegiFont ;
  unsigned int nBetumeret ;

  // A regi hatter uzemmod megjegyzese, atlatszo hatter beallitasa
  nBkMode = pDC->GetBkMode() ;
  pDC->SetBkMode(TRANSPARENT) ;

  if ( pDC->IsPrinting() )
  {
    nCimMagass = LekCimNyomt( pDC) ;
    nBetumeret = 50 ;
  }
  else
  {
    nBetumeret = 10 ;
  }

  cFont.CreateFont(nBetumeret, 0, 0, 0, 400, FALSE, FALSE, 0,
                   ANSI_CHARSET, OUT_DEFAULT_PRECIS,
                   CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                   DEFAULT_PITCH | FF_MODERN, "Courier") ; // Nem TrueType

  // Remelhetoleg nem zavar, hogy a kirajzolaskor nem allitgatom ide-oda
  pRegiFont = pDC->SelectObject( &cFont) ;

  // dX meghatarozasa az aktualis betumeret fv-eben ("XX" az etalon-sztring)
  cMeret = pDC->GetTextExtent( "Xy", 2) ;
  gl_dX = cMeret.cx ;
  gl_dY = cMeret.cy ;

  TabsopTorol() ;
  OsszTabstopBeall( pDC) ;

  nKezdopY += nCimMagass ;
  pDoc->m_cLekErMegjVaz.Kirajzol( pDC, nKezdopY) ;

  pDC->SelectObject( pRegiFont) ;

  pDC->SetBkMode(nBkMode) ;
//TRACE("\nCLekEredm1View::OnDraw : END") ;
}

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1View printing

BOOL CLekEredm1View::OnPreparePrinting(CPrintInfo* pInfo)
{
	// default preparation
	return DoPreparePrinting(pInfo);
}

void CLekEredm1View::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add extra initialization before printing
}

void CLekEredm1View::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add cleanup after printing
}

int CLekEredm1View::LekCimNyomt(CDC* pDC)
{
  CLekEredm1Doc* pDoc = GetDocument() ;
  ASSERT_VALID(pDoc) ;
  CString cDokuNev   ;
  CSize   cMeret ;

  CFont cFont, *pRegiFont ;

  cFont.CreateFont(100, 0, 0, 0, 400, FALSE, FALSE, 0,
                   ANSI_CHARSET, OUT_DEFAULT_PRECIS,
                   CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                   DEFAULT_PITCH | FF_MODERN, "Arial") ; // Nem TrueType

  pRegiFont = pDC->SelectObject( &cFont) ;

  cDokuNev = pDoc->DokuNev() ;
  pDC->TextOut( 0, 0, cDokuNev ) ;
  cMeret = pDC->GetTextExtent( cDokuNev, cDokuNev.GetLength()) ;
  
  pDC->MoveTo(0, cMeret.cy) ;
  pDC->LineTo(cMeret.cx, cMeret.cy) ;
  
  return cMeret.cy ;
}

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1View diagnostics

#ifdef _DEBUG
void CLekEredm1View::AssertValid() const
{
	CView::AssertValid();
}

void CLekEredm1View::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CLekEredm1Doc* CLekEredm1View::GetDocument() // non-debug version is inline
{                                                                
	return (CLekEredm1Doc*) m_pDocument ;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CLekEredm1View message handlers

void CLekEredm1View::OnLButtonDown(UINT nFlags, CPoint point)
{
//TRACE("\nCLekEredm1View::OnLButtonDown : BEGIN") ;

  CLekEredm1Doc* pDoc = GetDocument() ;

  // CScrollView changes the viewport origin and mapping mode.
  // It's necessary to convert the point from device coordinates
  // to logical coordinates, such as are stored in the document.
  CClientDC dc(this);
  OnPrepareDC(&dc);
  dc.DPtoLP(&point);

  if ( pDoc != NULL && pDoc->m_cLekErMegjVaz.m_pKezdopont != NULL )
  {
    if ( pDoc->m_cLekErMegjVaz.m_pKezdopont->EgerMozog( point, SZ_KEZDOSZINT, TRUE) )
    {// Az egerkurzor a teglalap felett van
      gl_bKezKurzorBe = TRUE ;

      pDoc->UpdateAllViews( NULL) ;
    }
    else
    {// Az egerkurzor nincs a teglalap felett
      gl_bKezKurzorBe = FALSE ;
    }
  }

  CView::OnLButtonDown(nFlags, point);
}

void CLekEredm1View::OnMouseMove(UINT nFlags, CPoint point)
{
  CLekEredm1Doc* pDoc = GetDocument() ;

  // CScrollView changes the viewport origin and mapping mode.
  // It's necessary to convert the point from device coordinates
  // to logical coordinates, such as are stored in the document.
  CClientDC dc(this);
  OnPrepareDC(&dc);
  dc.DPtoLP(&point);

  if ( pDoc != NULL && pDoc->m_cLekErMegjVaz.m_pKezdopont != NULL )
  {
    if ( pDoc->m_cLekErMegjVaz.m_pKezdopont->EgerMozog( point, SZ_KEZDOSZINT, FALSE) )
    {// Az egerkurzor a teglalap felett van
      gl_bKezKurzorBe = TRUE ;
      m_hRegiCursor = SetCursor(LoadCursor(AfxGetInstanceHandle(), MAKEINTRESOURCE(IDC_KEZ_CURSOR))) ;
    }
    else
    {// Az egerkurzor nincs a teglalap felett
      gl_bKezKurzorBe = FALSE ;

//    AfxGetApp()->LoadCursor(IDC_ARROW)  ;
      SetCursor(LoadCursor(NULL, IDC_ARROW)) ;
    }
  }
}

BOOL CLekEredm1View::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
  if ( pWnd == this )
  {
    if ( gl_bKezKurzorBe == TRUE )
    {
//        TRACE("SetCursor(LoadCursor(NULL, IDC_CROSS))\n") ;
//        m_hRegiCursor =
      SetCursor(LoadCursor(AfxGetInstanceHandle(), MAKEINTRESOURCE(IDC_KEZ_CURSOR))) ;
//        m_hRegiCursor = SetCursor(LoadCursor(AfxGetInstanceHandle(), "RES\\KEZ_CURS.CUR")) ;
//        m_hRegiCursor = SetCursor(LoadCursor(NULL, "RES\\KEZ_CURS.CUR")) ;
    }
    else
    {
//        SetCursor(m_hRegiCursor) ;
      SetCursor(LoadCursor(NULL, IDC_ARROW)) ;
    }

    // Mivel a SetCursor nem ad informaciot arrol, hogy a muvelet sikeres
    // volt-e, vagy sem, kenytelen vagyok 'TRUE'-val visszaterni. (NULL->
    // ha nem volt elozoleg kurzor
    return TRUE ;
  }

  return CView::OnSetCursor(pWnd, nHitTest, message) ;
}