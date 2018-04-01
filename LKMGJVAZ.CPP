// lkmgjvaz.cpp

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

IMPLEMENT_SERIAL(CLekErMegjVaz, CObject, 0)

CLekErMegjVaz::CLekErMegjVaz()
{
  m_pKezdopont = NULL ;
}

void CLekErMegjVaz::UjFelvesz(CFgyEeLgykOe* pUjElem)
{ // Ha mar egyszer a fv argumentuma adott vajon biztositja-e automatikusan
  // a kovetkezo feltetelt ???
  if ( pUjElem != NULL && pUjElem->IsKindOf(RUNTIME_CLASS(CFgyEeLgykOe)) )
  {
    // Ha az osztaly ures, egyszeruen csak fel kell venni az uj elemet
    if ( m_pKezdopont == NULL )
    {
      m_pKezdopont = pUjElem ;
    }
    else
    {
// ----------------------------------------------------------------------------    
      if ( pUjElem->m_cPontMez.m_cSzint == SZ_KEZDOSZINT )
      { 
        // Ha az uj elem a nagyobb, az eredeti ele kell beszurni
        if ( ((CFgyEeLgykOe*)pUjElem)->m_cPontMez.m_usKmltErt  >
             ((CFgyEeLgykOe*)m_pKezdopont)->m_cPontMez.m_usKmltErt )
        {
          CBazKerEredm* pTempKerEredm = m_pKezdopont ;

          m_pKezdopont = pUjElem ;
          pUjElem->m_pAzSzKovElem = pTempKerEredm ;
          // Az aktszint novelese felesleges, mert pontozas szintje alatt tovabbi
          // csoportositas nem lehetseges
          return ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          m_pKezdopont->UjFelvesz(pUjElem) ;
          return ;
        }
      }                        
// ----------------------------------------------------------------------------
      if ( pUjElem->m_cDatumMez.m_cSzint == SZ_KEZDOSZINT )
      {
        if ( pUjElem->m_cDatumMez.m_sDatum.ev >
             ((CFgyEeLgykOe*)m_pKezdopont)->m_cDatumMez.m_sDatum.ev )
        {
          CBazKerEredm* pTempKerEredm = m_pKezdopont ;

          m_pKezdopont = pUjElem ;
          pUjElem->m_pAzSzKovElem = pTempKerEredm ;
          return ;
        }
        else
        { // Jelen esetben fontos a <,>,= esetek megkulonboztetese, mivel
          // a datumnak kepesnek kell lennie csoportositasra is
          if ( pUjElem->m_cDatumMez.m_sDatum.ev ==
               ((CFgyEeLgykOe*)m_pKezdopont)->m_cDatumMez.m_sDatum.ev )
          {
            pUjElem->m_cAktSzint++ ;
            ((CFgyEeLgykOe*)m_pKezdopont)->m_cDatumMez.m_usKmltErt += 
                                              pUjElem->m_cDatumMez.m_usKmltErt ;

            if ( m_pKezdopont->m_pKovSzKovElem != NULL )
            {
              // A beljebb levo szintu elem foglalkozik vele tovabb
              m_pKezdopont->m_pKovSzKovElem->UjFelvesz(pUjElem) ;
            }
            else
            {
              m_pKezdopont->m_pKovSzKovElem = pUjElem ;
            }
          }
          else
          {
            if ( m_pKezdopont->m_pAzSzKovElem != NULL )
            { // Nincs szintnoveles
              m_pKezdopont->m_pAzSzKovElem->UjFelvesz(pUjElem) ;
            }
            else
            {
              m_pKezdopont->m_pAzSzKovElem = pUjElem ;
            }

            return ;
          }
        }
      }
// ----------------------------------------------------------------------------
      // Az osszes tobbi esetben csak csoportositas van, ahol nem kell az 
      // aktualis elem ele beszurni
      ((CFgyEeLgykOe*)m_pKezdopont)->UjFelvesz(pUjElem) ;
    } // m_pKezdopont != NULL
  }

  return ;
}

void CLekErMegjVaz::UjFelvesz(CHolVKiV* pUjElem)
{
  if ( pUjElem != NULL && pUjElem->IsKindOf(RUNTIME_CLASS(CHolVKiV)) )
  {
    // Ha az osztaly ures, egyszeruen csak fel kell venni az uj elemet
    if ( m_pKezdopont == NULL )
    {
      m_pKezdopont = pUjElem ;
    }
    else
    {
// ----------------------------------------------------------------------------
      if ( pUjElem->m_cDatumMez.m_cSzint == SZ_KEZDOSZINT )
      {
        if ( pUjElem->m_cDatumMez.m_sDatum.ev >
             ((CHolVKiV*)m_pKezdopont)->m_cDatumMez.m_sDatum.ev )
        {
          CBazKerEredm* pTempKerEredm = m_pKezdopont ;

          m_pKezdopont = pUjElem ;
          pUjElem->m_pAzSzKovElem = pTempKerEredm ;
//          cUjElem.m_cAktSzint++ ;
          return ;
        }
        else
        { // Jelen esetben fontos a <,>,= esetek megkulonboztetese, mivel
          // a datumnak kepesnek kell lennie csoportositasra is
          if ( pUjElem->m_cDatumMez.m_sDatum.ev ==
               ((CFgyEeLgykOe*)m_pKezdopont)->m_cDatumMez.m_sDatum.ev )
          {
            pUjElem->m_cAktSzint++ ;
            ((CHolVKiV*)m_pKezdopont)->m_cDatumMez.m_usKmltErt += 
                                              pUjElem->m_cDatumMez.m_usKmltErt ;

            if ( m_pKezdopont->m_pKovSzKovElem != NULL )
            {
              // A beljebb levo szintu elem foglalkozik vele tovabb
              m_pKezdopont->m_pKovSzKovElem->UjFelvesz(pUjElem) ;
            }
            else
            {
              m_pKezdopont->m_pKovSzKovElem = pUjElem ;
            }
          }
          else
          {
            if ( m_pKezdopont->m_pAzSzKovElem != NULL )
            { // Nincs szintnoveles
              m_pKezdopont->m_pAzSzKovElem->UjFelvesz(pUjElem) ;
            }
            else
            {
              m_pKezdopont->m_pAzSzKovElem = pUjElem ;
            }

            return ;
          }
        }
      }
// ----------------------------------------------------------------------------
      // Az osszes tobbi esetben csak csoportositas van, ahol nem kell az 
      // aktualis elem ele beszurni
      ((CHolVKiV*)m_pKezdopont)->UjFelvesz(pUjElem) ;
    } // m_pKezdopont != NULL
  }

  return ;
}

void CLekErMegjVaz::UjFelvesz(CPillAll* pUjElem)
{
  if ( pUjElem != NULL && pUjElem->IsKindOf(RUNTIME_CLASS(CPillAll)) )
  {
    // Ha az osztaly ures, egyszeruen csak fel kell venni az uj elemet
    if ( m_pKezdopont == NULL )
    {
      m_pKezdopont = pUjElem ;
    }
    else
    {
      // A rendezes a dialogusablakban tortenik, itt nem vegzunk semmilyet
      ((CPillAll*)m_pKezdopont)->UjFelvesz(pUjElem) ;
    }
  }
}
 
void CLekErMegjVaz::UjFelvesz(CVBSzerepl* pUjElem)
{
  BOOL bOsszetart = FALSE ;

  if ( pUjElem != NULL && pUjElem->IsKindOf(RUNTIME_CLASS(CVBSzerepl)) )
  {
    // Ha az osztaly ures, egyszeruen csak fel kell venni az uj elemet
    if ( m_pKezdopont == NULL )
    {
      m_pKezdopont = pUjElem ;
    }
    else
    {
      // Itt nem foglalkozunk rendezessel, mivel sokkal egyszerubb a CLekerdDlg::
      // VBSzereplLekerd()-ben tombben rendezni, majd egyenkent a dokumentbe
      // felvenni (csoportositas)
      if (((CVBSzerepl*)m_pKezdopont)->m_cVersMez.m_cSzint == SZ_KEZDOSZINT &&
          ((CVBSzerepl*)m_pKezdopont)->m_cVersMez.m_uiKulcs1==pUjElem->m_cVersMez.m_uiKulcs1)
      {
        bOsszetart = TRUE ;
      }
      else
      {
        if (((CVBSzerepl*)m_pKezdopont)->m_cNemzMez.m_cSzint == SZ_KEZDOSZINT &&
            ((CVBSzerepl*)m_pKezdopont)->m_cNemzMez.m_uiKulcs1 ==
            pUjElem->m_cNemzMez.m_uiKulcs1)
        {    
          bOsszetart = TRUE ;
        }
        else
        {
          if (((CVBSzerepl*)m_pKezdopont)->m_cCsapMez.m_cSzint == SZ_KEZDOSZINT &&
             ((CVBSzerepl*)m_pKezdopont)->m_cCsapMez.m_uiKulcs1 == 
             pUjElem->m_cCsapMez.m_uiKulcs1)
          {
            bOsszetart = TRUE ;
          }
          else
          { 
            if (((CVBSzerepl*)m_pKezdopont)->m_cMotorMez.m_cSzint == SZ_KEZDOSZINT &&
            ((CVBSzerepl*)m_pKezdopont)->m_cMotorMez.m_uiKulcs1==pUjElem->m_cMotorMez.m_uiKulcs1)
            {
              bOsszetart = TRUE ;
            }
          }
        }
      }
      // A helyezes szerinti csoportositas helycseret tehet szuksegesse
      if ( bOsszetart && 
           pUjElem->m_cHelyezMez.SajatHelyz() <
           ((CVBSzerepl*)m_pKezdopont)->m_cHelyezMez.SajatHelyz() )
      {// Helycsere : az ujonnan felveendo 'van elobbre' !
        pUjElem->m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)m_pKezdopont)->m_cHelyezMez.m_pHelySzamTbl,
                                         ((CVBSzerepl*)m_pKezdopont)->m_cHelyezMez.m_ucMeret, FALSE) ;
        // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?

      }

      ((CVBSzerepl*)m_pKezdopont)->UjFelvesz(pUjElem) ;
    } // m_pKezdopont != NULL
  }
}

void CLekErMegjVaz::Kirajzol(CDC* pDC,  int& nKezdY)
{
  if ( m_pKezdopont != NULL )
    m_pKezdopont->Kirajzol( pDC, nKezdY) ;
}

void CLekErMegjVaz::ScrollMeret(int& x, int& y)
{
  if ( m_pKezdopont != NULL )
  {
    x = 0 ;
    y = 0 ;
    m_pKezdopont->ScrollMeret( x, y) ;
  }
}

void CLekErMegjVaz::Serialize(CArchive& ar)
{
  CObject::Serialize(ar) ;

  if (ar.IsStoring())
  { // storing code
    ar << /* (CObject*) */ m_pKezdopont ;
  }
  else
  { // loading code
    ar >> /* (CObject*) */ m_pKezdopont ;
  }
}

CLekErMegjVaz::~CLekErMegjVaz()
{
  if ( m_pKezdopont != NULL )
  {
    m_pKezdopont->Felszabadit() ;

    delete m_pKezdopont ;
  }
}
