// impgyabl.cpp : implementation of the CUjdokletView class
//
/////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "resource.h"
#include "impgyabl.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CImportGyAbl CMDIChildWnd

IMPLEMENT_DYNCREATE(CImportGyAbl, CMDIChildWnd)

BEGIN_MESSAGE_MAP(CImportGyAbl, CMDIChildWnd)
    //{{AFX_MSG_MAP(CImportGyAbl)
    ON_WM_PAINT()
	ON_WM_SIZE()
    ON_WM_VSCROLL()
    //}}AFX_MSG_MAP
    // Standard printing commands
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CImportGyAbl construction/destruction

CImportGyAbl::CImportGyAbl()
{
  m_cFont.CreateFont(BETUMERET, 0, 0, 0, 400, FALSE, FALSE, 0,
                     ANSI_CHARSET, OUT_DEFAULT_PRECIS,
                     CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                     DEFAULT_PITCH | FF_MODERN, "Courier") ; // Nem TrueType

//  m_rKepernyoMer = CFrameWnd::rectDefault ;

  m_nElsoSor   =  0 ;
  m_nInverzSor = -1 ;
}

BOOL CImportGyAbl::Create( LPCSTR lpszClassName, LPCSTR lpszWindowName,
                           DWORD dwStyle, const RECT& rect, CMDIFrameWnd* pParentWnd,
                           CCreateContext* pContext )
{
  return CMDIChildWnd::Create(lpszClassName, lpszWindowName,  dwStyle,
                                       rect,     pParentWnd, pContext) ;
}


BOOL CImportGyAbl::LoadFrame( UINT nIDResource , DWORD dwDefaultStyle ,
                              CWnd* pParentWnd, CCreateContext* pContext )
{
  BOOL bRC ;

  bRC = CMDIChildWnd::LoadFrame( nIDResource, dwDefaultStyle, pParentWnd, pContext) ;

  GetClientRect( &m_rKepernyoMer) ;

  return bRC ;
}

void CImportGyAbl::OnPaint()
{
  int nBkMode, nSorokSzama, nAktSor, nYPoz ;
  TEXTMETRIC tmSzovMer ;
  CFont* pRegiFont ;

  CPaintDC cPaintDC( this) ;

  // A regi hatter uzemmod megjegyzese, atlatszo hatter beallitasa
  nBkMode = cPaintDC.GetBkMode() ;
  cPaintDC.SetBkMode(TRANSPARENT) ;

  pRegiFont = cPaintDC.SelectObject( &m_cFont) ;

//  cPaintDC.SetTextAlign( TA_CENTER | TA_BASELINE) ;

  if ( cPaintDC.GetTextMetrics( &tmSzovMer) )
  {
    m_nSormagassag = tmSzovMer.tmHeight ;
  }
  else
  {
    m_nSormagassag = 20 ;
  }  

  nSorokSzama = m_Sorok.GetSize() ;

// TRACE("\nOnPaint()") ;
  // Mindig a kepernyo tetejerol kell \/ kezdeni 
  for ( nAktSor = m_nElsoSor, nYPoz = 0 ;
        nAktSor < nSorokSzama && nYPoz < m_rKepernyoMer.bottom ;
        nAktSor++, nYPoz = (nAktSor-m_nElsoSor)*m_nSormagassag )
  {
// TRACE("\n%d : %s", nAktSor, m_Sorok[nAktSor]) ;
    cPaintDC.TextOut( 0, nYPoz, m_Sorok[nAktSor]) ;
  }

  // Ha az invertalando sor a kepernyon lathato : a megfelelo terulet invertalasa
  if ( m_nElsoSor <= m_nInverzSor &&
       m_nInverzSor < m_rKepernyoMer.bottom/m_nSormagassag+m_nElsoSor)
  {
    nYPoz = (m_nInverzSor-m_nElsoSor)*m_nSormagassag ;
    cPaintDC.InvertRect(CRect(0, nYPoz, m_rKepernyoMer.right, nYPoz+m_nSormagassag)) ;
  }

  cPaintDC.SelectObject( pRegiFont) ;
  cPaintDC.SetBkMode( nBkMode) ;

// TRACE("\nOnPaint()") ; 
}

void CImportGyAbl::OnSize(UINT nType, int cx, int cy)
{
  CWnd::OnSize(nType, cx, cy);

// UpdateScrollSize

  m_rKepernyoMer.right  = cx ;
  m_rKepernyoMer.bottom = cy ; 
}

void CImportGyAbl::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
  int nIdx, nY ;
  int nKeperSorokSzama, nBkMode ;
  RECT rScrollMeret ;
  CBrush cBrush ;
  COLORREF crHattsz ;

  CFont*  pRegiFont ;
  CClientDC dc(this);

  crHattsz = GetSysColor( COLOR_WINDOW) ;
  cBrush.CreateSolidBrush( crHattsz) ;

  // A regi hatter uzemmod megjegyzese, atlatszo hatter beallitasa
  nBkMode = dc.GetBkMode() ;
  dc.SetBkMode( TRANSPARENT) ; // OPAQUE TRANSPARENT

  pRegiFont = dc.SelectObject( &m_cFont) ;

  nKeperSorokSzama = m_rKepernyoMer.bottom/m_nSormagassag ;

  switch ( nSBCode )
  {
    case SB_LINEDOWN :

      nIdx = m_nElsoSor + nKeperSorokSzama - 1 ;
      if ( nKeperSorokSzama < m_Sorok.GetSize() && nIdx < m_Sorok.GetSize() )
      {
        m_nElsoSor++ ;

        rScrollMeret = m_rKepernyoMer ;

        // Az utolso sor ugyis kikerul a kepernyorol
        rScrollMeret.top += m_nSormagassag ;
        rScrollMeret.bottom = nKeperSorokSzama*m_nSormagassag ;

        // A ScrollWindow az OnPaint meghivasat eredmenyezi
        dc.ScrollDC( 0, -m_nSormagassag, &rScrollMeret, NULL, NULL, NULL) ;
//        dc.InvertRect( &rScrollMeret) ; 

// TRACE("\n%d : %s", m_nElsoSor+nKeperSorokSzama, m_Sorok[m_nElsoSor+nKeperSorokSzama-1]) ;
        rScrollMeret = m_rKepernyoMer ;
        rScrollMeret.top = (nKeperSorokSzama-1)*m_nSormagassag ;
        dc.FillRect( &rScrollMeret, &cBrush) ;

        dc.TextOut(0, rScrollMeret.top, m_Sorok[nIdx]);

        // Ha az invertalando sor a kepernyon lathato : a megfelelo terulet invertalasa
        if ( nIdx == m_nInverzSor )
        {
          nY = (m_nInverzSor-m_nElsoSor)*m_nSormagassag ;
          dc.InvertRect(CRect(0, rScrollMeret.top,
                              m_rKepernyoMer.right, rScrollMeret.top+m_nSormagassag)) ;
        }

//TRACE("\nSOR LE : nIdx : %d", nIdx) ;
      }

      break ;

    case SB_LINEUP   :

      if ( m_nElsoSor > 0 )
      {
        m_nElsoSor-- ;

        rScrollMeret = m_rKepernyoMer ;

        // Az utolso sor ugyis kikerul a kepernyorol
//        rScrollMeret.top += m_nSormagassag ;
        rScrollMeret.bottom = nKeperSorokSzama*m_nSormagassag ;

        dc.ScrollDC( 0, m_nSormagassag, &rScrollMeret, NULL, NULL, NULL) ;

        rScrollMeret = m_rKepernyoMer ;
        rScrollMeret.bottom = m_nSormagassag ;
        dc.FillRect( &rScrollMeret, &cBrush) ;

        dc.TextOut(0, 0, m_Sorok[m_nElsoSor]);

        // Ha az kijelolt sor bekerul a kepernyore
        if ( m_nElsoSor == m_nInverzSor )
        {
          dc.InvertRect(CRect(0, 0, m_rKepernyoMer.right, m_nSormagassag)) ;
        }
//TRACE("\nSOR FEL : m_nElsoSor : %d", m_nElsoSor) ;
      }

      break ;

    case SB_PAGEUP   :
      break ;

    case SB_PAGEDOWN :
      break ;

/*  ebben a ket esetben van nPos-nak erteke, a scroll-box poziciojat tartalmazza
    lehet negativ es lehet int-re cast-olni
    SetScrollPos-zal be kell allitani a gomb poziciojat
    case SB_THUMBPOSITION :
      break ;

    case SB_THUBTRACK :
      break ;
*/
  }

  dc.SelectObject( pRegiFont) ;
  dc.SetBkMode( nBkMode) ;
}

BOOL CImportGyAbl::SorInvertal( int nInvSor)
{
  int nY ;
     // <= ?
  if ( nInvSor < 0 || nInvSor > m_Sorok.GetUpperBound() )
    return FALSE ;

  CPaintDC cPaintDC( this) ;

  // Ha a regi invertalando sor a kepernyon lathato :
  // a megfelelo terulet invertalasa (eltuntetese)
  if ( m_nElsoSor <= m_nInverzSor &&
       m_nInverzSor < m_nElsoSor + m_rKepernyoMer.bottom / m_nSormagassag )
  {
    nY = (m_nInverzSor-m_nElsoSor)*m_nSormagassag ;
    cPaintDC.InvertRect(CRect(0, nY, m_rKepernyoMer.right, nY+m_nSormagassag)) ;
  }

  m_nInverzSor = nInvSor ;

  if ( m_nElsoSor <= m_nInverzSor &&
       m_nInverzSor < m_nElsoSor + m_rKepernyoMer.bottom / m_nSormagassag )
  {
    nY = (m_nInverzSor-m_nElsoSor)*m_nSormagassag ;
    cPaintDC.InvertRect(CRect(0, nY, m_rKepernyoMer.right, nY+m_nSormagassag)) ;
  }

  return FALSE ;
}

CImportGyAbl::~CImportGyAbl()
{
}