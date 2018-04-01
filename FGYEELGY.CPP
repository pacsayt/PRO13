// fgyeelgy.cpp

#include "stdafx.h"

#include "resource.h"

#include <stdlib.h>

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

#include "proba13.h"


IMPLEMENT_SERIAL(CFgyEeLgykOe, CBazKerEredm, 0)

CFgyEeLgykOe::CFgyEeLgykOe()
{ 
  m_cDatumMez.m_cKiBe = LOBYTE(KAPCSOLO | BEKAPCS) ;
}

void CFgyEeLgykOe::Feltolt(CNvzSor* pNvzLstSor, CObject* pRBBejLgyKor,
                           SNagydij* pNagydij, eNevtipus enNevtip)
{
  m_cVersMez.m_uiKulcs1 = pNvzLstSor->uiVersenyzo ;
  m_cNemzMez.m_uiKulcs1 = pNvzLstSor->uiNemz      ;
  m_cCsapMez.m_uiKulcs1 = pNvzLstSor->uiCsapat    ;
  m_cMotorMez.m_uiKulcs1 = pNvzLstSor->uiMotor    ;
  m_cNagydMez.m_uiKulcs1 = pNagydij->m_uiNagyd ;
  m_cNagydMez.m_uiKulcs2 = pNagydij->m_uiPalya ;

  m_cPontMez.m_usKmltErt = 1 ;
  m_cDatumMez.m_sDatum = pNagydij->m_sDatum ;

  // A datum mindig a masodik szinten van
  m_cDatumMez.m_cSzint = SZ_KEZDOSZINT + 1 ;
  m_cDatumMez.m_usKmltErt = 1 ;

  // El kell donteni, hogy a leggyorsabb kornek megfelelo hivassal allunk-e szemben
  if ( pRBBejLgyKor->IsKindOf(RUNTIME_CLASS(CLeggyKor)) )
  {
    m_cIdoeredmMez.m_cIdoeredm = ((CLeggyKor*)pRBBejLgyKor)->Idoer() ;
    // Tkeppen kellene uj bitet lefoglalni a leggyorsabb kornek
    m_cIdoeredmMez.m_usMegj = IDOEREDMENY ;
  }
  else
  {
    if ( pRBBejLgyKor->IsKindOf(RUNTIME_CLASS(CRBBejegyzes)) )
    {
      m_cIdoeredmMez.m_cIdoeredm = ((CRBBejegyzes*)pRBBejLgyKor)->IdoerC() ;
      m_cIdoeredmMez.m_usMegj    = ((CRBBejegyzes*)pRBBejLgyKor)->Megj()   ;
    }
  }

  // A szintek es kumulalt ertekek beallitasa a lek.nev. fv.ben
  switch ( enNevtip )
  {
    case eVersNt :
      m_cVersMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cVersMez.m_usKmltErt = 1 ;
      break ;

    case eNemzNt :
      m_cNemzMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cNemzMez.m_usKmltErt = 1 ;
      break ;

    case eCsapNt :
      m_cCsapMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cCsapMez.m_usKmltErt = 1 ;
      break ;

    case eMotorNt :
      m_cMotorMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cMotorMez.m_usKmltErt = 1 ;
      break ;

    case eNagydijNt :
      m_cNagydMez.m_cSzint  = SZ_KEZDOSZINT ;
      m_cNagydMez.m_usKmltErt = 1 ;
      break ;

    default :
      ;
  }
}

void CFgyEeLgykOe::Feltolt(sLgGyKorLekErTip* pLgGyKorLekEr, eNevtipus enNevtip)
{
  m_cVersMez.m_uiKulcs1 = pLgGyKorLekEr->nVersenyzo ;
  m_cNemzMez.m_uiKulcs1 = pLgGyKorLekEr->nNemzetiseg      ;
  m_cCsapMez.m_uiKulcs1 = pLgGyKorLekEr->nCsapat    ;
  m_cMotorMez.m_uiKulcs1 = pLgGyKorLekEr->nMotor    ;
  m_cNagydMez.m_uiKulcs1 = pLgGyKorLekEr->nNagydijNev ;
//  m_cNagydMez.m_uiKulcs2 = pLgGyKorLekEr->m_uiPalya ;

  m_cPontMez.m_usKmltErt = 1 ;
  m_cDatumMez.m_sDatum.ev    = pLgGyKorLekEr->nSzezonEve ;
  // Ezeket nem adja vissza az SQL-fuggveny
  m_cDatumMez.m_sDatum.honap = 0 ;
  m_cDatumMez.m_sDatum.nap   = 0 ;
  // A datum mindig a masodik szinten van
  m_cDatumMez.m_cSzint = SZ_KEZDOSZINT + 1 ;
  m_cDatumMez.m_usKmltErt = 1 ;

  // A leggyorsabb_kor tablaban nincs ora mezo \/
  m_cIdoeredmMez.m_cIdoeredm.IdoBeall((char)  0,
                                      (char)  pLgGyKorLekEr->nPerc,
                                      (char)  pLgGyKorLekEr->nMasodperc,
                                      (short) pLgGyKorLekEr->nEzrdmsdperc) ;

  // Itt mindig idoeredmenyrol van szo
  m_cIdoeredmMez.m_usMegj = IDOEREDMENY ;

  // A szintek es kumulalt ertekek beallitasa a lek.nev. fv.ben
  switch ( enNevtip )
  {
    case eVersNt :
      m_cVersMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cVersMez.m_usKmltErt = 1 ;
      break ;

    case eNemzNt :
      m_cNemzMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cNemzMez.m_usKmltErt = 1 ;
      break ;

    case eCsapNt :
      m_cCsapMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cCsapMez.m_usKmltErt = 1 ;
      break ;

    case eMotorNt :
      m_cMotorMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cMotorMez.m_usKmltErt = 1 ;
      break ;

    case eNagydijNt :
      m_cNagydMez.m_cSzint  = SZ_KEZDOSZINT ;
      m_cNagydMez.m_usKmltErt = 1 ;
      break ;

    default :
      ;
  }
}

void CFgyEeLgykOe::Feltolt(sRsBefLekErTip* pRsBefLekEr, eNevtipus enNevtip)
{
  m_cVersMez.m_uiKulcs1  = pRsBefLekEr->uiVersenyzo  ;
  m_cNemzMez.m_uiKulcs1  = pRsBefLekEr->uiNemzetiseg ;
  m_cCsapMez.m_uiKulcs1  = pRsBefLekEr->uiCsapat     ;
  m_cMotorMez.m_uiKulcs1 = pRsBefLekEr->uiMotor      ;
  m_cNagydMez.m_uiKulcs1 = pRsBefLekEr->uiNagydijNev ;

//  m_cNagydMez.m_uiKulcs2 = pLgGyKorLekEr->m_uiPalya ;

  m_cPontMez.m_usKmltErt  = 1 ;
  
  m_cDatumMez.m_sDatum.ev = pRsBefLekEr->uiSzezonEve ;
  // Ezeket nem adja vissza az SQL-fuggveny
  m_cDatumMez.m_sDatum.honap = 0 ;
  m_cDatumMez.m_sDatum.nap   = 0 ;
  // A datum mindig a masodik szinten van
  m_cDatumMez.m_cSzint = SZ_KEZDOSZINT + 1 ;
  m_cDatumMez.m_usKmltErt = 1 ;

  m_cIdoeredmMez.m_cIdoeredm.IdoBeall((char)  pRsBefLekEr->uiOra,
                                      (char)  pRsBefLekEr->uiPerc,
                                      (char)  pRsBefLekEr->uiMasodperc,
                                      (short) pRsBefLekEr->uiEzrdmsdperc) ;

  m_cIdoeredmMez.m_usMegj = IDOEREDMENY ;

  // A szintek es kumulalt ertekek beallitasa a lek.nev. fv.ben
  switch ( enNevtip )
  {
    case eVersNt :
      m_cVersMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cVersMez.m_usKmltErt = 1 ;
      break ;

    case eNemzNt :
      m_cNemzMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cNemzMez.m_usKmltErt = 1 ;
      break ;

    case eCsapNt :
      m_cCsapMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cCsapMez.m_usKmltErt = 1 ;
      break ;

    case eMotorNt :
      m_cMotorMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cMotorMez.m_usKmltErt = 1 ;
      break ;

    case eNagydijNt :
      m_cNagydMez.m_cSzint  = SZ_KEZDOSZINT ;
      m_cNagydMez.m_usKmltErt = 1 ;
      break ;

    default :
      ;
  }
}

void CFgyEeLgykOe::UjFelvesz(CFgyEeLgykOe* pUjElem)
{
  char cAktSzint ;

  if ( pUjElem->IsKindOf(RUNTIME_CLASS(CFgyEeLgykOe)) )
  {
    if ( pUjElem->m_cVersMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cVersMez.m_uiKulcs1 == m_cVersMez.m_uiKulcs1 )
      { 
        m_cVersMez.m_usKmltErt += pUjElem->m_cVersMez.m_usKmltErt ;
        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cVersMez.m_cKirajzol = 0 ;

        // Az ove az uj elem, felveszi
        if ( m_pKovSzKovElem == NULL )
        { 
          // Mivel az egyes elemek aktualis szintjuktol kezdve kirajzoljak magukat
          // tehat alanyulnak illetekessegi koruknek, az alattvaloknak a megfelelo
          // szintu elemek egyezese eseten nem szabad rajzolniuk
          for ( cAktSzint = m_cAktSzint+1 ; ; cAktSzint++ )
          {
            if ( m_cNemzMez.m_cSzint == cAktSzint &&
                 m_cNemzMez.m_uiKulcs1 == pUjElem->m_cNemzMez.m_uiKulcs1 )
            {
              pUjElem->m_cNemzMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cCsapMez.m_cSzint == cAktSzint &&
                 m_cCsapMez.m_uiKulcs1 == pUjElem->m_cCsapMez.m_uiKulcs1 )
            {
              pUjElem->m_cCsapMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cMotorMez.m_cSzint == cAktSzint &&
                 m_cMotorMez.m_uiKulcs1 == pUjElem->m_cMotorMez.m_uiKulcs1 )
            {
              pUjElem->m_cMotorMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cNagydMez.m_cSzint == cAktSzint &&
                 m_cNagydMez.m_uiKulcs1 == pUjElem->m_cNagydMez.m_uiKulcs1 )
            {
              pUjElem->m_cNagydMez.m_cKirajzol = 0 ;
              continue ;
            }
            // m_cPontMez ?!?
            if ( m_cDatumMez.m_cSzint == cAktSzint &&
                 m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
            {
              pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
              continue ;
            }
            break ;
          }
          m_pKovSzKovElem = pUjElem ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {// A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      
      return ;
    }
// ----------------------------------------------------------------------------    
    if ( pUjElem->m_cNemzMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cNemzMez.m_uiKulcs1 == m_cNemzMez.m_uiKulcs1 )
      { 
        m_cNemzMez.m_usKmltErt += pUjElem->m_cNemzMez.m_usKmltErt ;
        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cNemzMez.m_cKirajzol = 0 ;

        // Az ove az uj elem, felveszi
        if ( m_pKovSzKovElem == NULL )
        {
          for ( cAktSzint = m_cAktSzint+1 ; ; cAktSzint++ )
          {
            if ( m_cVersMez.m_cSzint == cAktSzint &&
                 m_cVersMez.m_uiKulcs1 == pUjElem->m_cVersMez.m_uiKulcs1 )
            {
              pUjElem->m_cVersMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cCsapMez.m_cSzint == cAktSzint &&
                 m_cCsapMez.m_uiKulcs1 == pUjElem->m_cCsapMez.m_uiKulcs1 )
            {
              pUjElem->m_cCsapMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cMotorMez.m_cSzint == cAktSzint &&
                 m_cMotorMez.m_uiKulcs1 == pUjElem->m_cMotorMez.m_uiKulcs1 )
            {
              pUjElem->m_cMotorMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cNagydMez.m_cSzint == cAktSzint &&
                 m_cNagydMez.m_uiKulcs1 == pUjElem->m_cNagydMez.m_uiKulcs1 )
            {
              pUjElem->m_cNagydMez.m_cKirajzol = 0 ;
              continue ;
            }
            // m_cPontMez ?!?
            if ( m_cDatumMez.m_cSzint == cAktSzint &&
                 m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
            {
              pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
              continue ;
            }
            break ;
          } 
          m_pKovSzKovElem = pUjElem ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {// A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      
      return ;
    }
// ----------------------------------------------------------------------------    
    if ( pUjElem->m_cCsapMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cCsapMez.m_uiKulcs1 == m_cCsapMez.m_uiKulcs1 )
      { 
        m_cCsapMez.m_usKmltErt += pUjElem->m_cCsapMez.m_usKmltErt ;
        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cCsapMez.m_cKirajzol = 0 ;

        // Az ove az uj elem, felveszi
        if ( m_pKovSzKovElem == NULL )
        {
          for ( cAktSzint = m_cAktSzint+1 ; ; cAktSzint++ )
          {
            if ( m_cVersMez.m_cSzint == cAktSzint &&
                 m_cVersMez.m_uiKulcs1 == pUjElem->m_cVersMez.m_uiKulcs1 )
            {
              pUjElem->m_cVersMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cNemzMez.m_cSzint == cAktSzint &&
                 m_cNemzMez.m_uiKulcs1 == pUjElem->m_cNemzMez.m_uiKulcs1 )
            {
              pUjElem->m_cCsapMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cMotorMez.m_cSzint == cAktSzint &&
                 m_cMotorMez.m_uiKulcs1 == pUjElem->m_cMotorMez.m_uiKulcs1 )
            {
              pUjElem->m_cMotorMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cNagydMez.m_cSzint == cAktSzint &&
                 m_cNagydMez.m_uiKulcs1 == pUjElem->m_cNagydMez.m_uiKulcs1 )
            {
              pUjElem->m_cNagydMez.m_cKirajzol = 0 ;
              continue ;
            }
            // m_cPontMez ?!?
            if ( m_cDatumMez.m_cSzint == cAktSzint &&
                 m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
            {
              pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
              continue ;
            }
            break ;
          }

          m_pKovSzKovElem = pUjElem ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {// A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }

      return ;
    }
// ----------------------------------------------------------------------------    
    if ( pUjElem->m_cMotorMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cMotorMez.m_uiKulcs1 == m_cMotorMez.m_uiKulcs1 )
      { 
        m_cMotorMez.m_usKmltErt += pUjElem->m_cMotorMez.m_usKmltErt ;
        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cMotorMez.m_cKirajzol = 0 ;

        // Az ove az uj elem, felveszi
        if ( m_pKovSzKovElem == NULL )
        {
          for ( cAktSzint = m_cAktSzint+1 ; ; cAktSzint++ )
          {
            if ( m_cNemzMez.m_cSzint == cAktSzint &&
                 m_cNemzMez.m_uiKulcs1 == pUjElem->m_cNemzMez.m_uiKulcs1 )
            {
              pUjElem->m_cNemzMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cCsapMez.m_cSzint == cAktSzint &&
                 m_cCsapMez.m_uiKulcs1 == pUjElem->m_cCsapMez.m_uiKulcs1 )
            {
              pUjElem->m_cCsapMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cVersMez.m_cSzint == cAktSzint &&
                 m_cVersMez.m_uiKulcs1 == pUjElem->m_cVersMez.m_uiKulcs1 )
            {
              pUjElem->m_cVersMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cNagydMez.m_cSzint == cAktSzint &&
                 m_cNagydMez.m_uiKulcs1 == pUjElem->m_cNagydMez.m_uiKulcs1 )
            {
              pUjElem->m_cNagydMez.m_cKirajzol = 0 ;
              continue ;
            }
            // m_cPontMez ?!?
            if ( m_cDatumMez.m_cSzint == cAktSzint &&
                 m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
            {
              pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
              continue ;
            }
            break ;
          } 
          m_pKovSzKovElem = pUjElem ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {// A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      
      return ;
    }
// ----------------------------------------------------------------------------    
    if ( pUjElem->m_cNagydMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cNagydMez.m_uiKulcs1 == m_cNagydMez.m_uiKulcs1 )
      { 
        m_cNagydMez.m_usKmltErt += pUjElem->m_cNagydMez.m_usKmltErt ;
        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cNagydMez.m_cKirajzol = 0 ;

        // Az ove az uj elem, felveszi
        if ( m_pKovSzKovElem == NULL )
        {
          for ( cAktSzint = m_cAktSzint+1 ; ; cAktSzint++ )
          {
            if ( m_cNemzMez.m_cSzint == cAktSzint &&
                 m_cNemzMez.m_uiKulcs1 == pUjElem->m_cNemzMez.m_uiKulcs1 )
            {
              pUjElem->m_cNemzMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cCsapMez.m_cSzint == cAktSzint &&
                 m_cCsapMez.m_uiKulcs1 == pUjElem->m_cCsapMez.m_uiKulcs1 )
            {
              pUjElem->m_cCsapMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cMotorMez.m_cSzint == cAktSzint &&
                 m_cMotorMez.m_uiKulcs1 == pUjElem->m_cMotorMez.m_uiKulcs1 )
            {
              pUjElem->m_cMotorMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cVersMez.m_cSzint == cAktSzint &&
                 m_cVersMez.m_uiKulcs1 == pUjElem->m_cVersMez.m_uiKulcs1 )
            {
              pUjElem->m_cVersMez.m_cKirajzol = 0 ;
              continue ;
            }
            // m_cPontMez ?!?
            if ( m_cDatumMez.m_cSzint == cAktSzint &&
                 m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
            {
              pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
              continue ;
            }
            break ;
          } 
          m_pKovSzKovElem = pUjElem ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {// A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CFgyEeLgykOe*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }

      return ;
    }
// ----------------------------------------------------------------------------    
    if ( pUjElem->m_cPontMez.m_cSzint == m_cAktSzint )
    {
      if ( m_pAzSzKovElem != NULL )
      { // Az ove az uj elem, felveszi
        // Hogy az if utasitasbeli cast-olas biztonsagos legyen 
        if ( !m_pAzSzKovElem->IsKindOf(RUNTIME_CLASS(CFgyEeLgykOe)) )
          return ;

        if ( pUjElem->m_cPontMez.m_usKmltErt > 
             ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cPontMez.m_usKmltErt )
        {
          CBazKerEredm* pTempKerEredm = m_pAzSzKovElem ;

          m_pAzSzKovElem = pUjElem ;
          pUjElem->m_pAzSzKovElem = pTempKerEredm ;
//          cUjElem.m_cAktSzint++ ;
          return ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          m_pAzSzKovElem->UjFelvesz(pUjElem) ;
          return ;
        }
      }
      
      return ;
    }
// ----------------------------------------------------------------------------
    if ( pUjElem->m_cDatumMez.m_cSzint == m_cAktSzint )
    {
      // A hozzatartozo elemet felveszi
      if ( pUjElem->m_cDatumMez.m_sDatum.ev == m_cDatumMez.m_sDatum.ev )
      {
        m_cDatumMez.m_usKmltErt += pUjElem->m_cDatumMez.m_usKmltErt ;
        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cDatumMez.m_cKirajzol = 0 ;

        if ( m_pKovSzKovElem != NULL )
        {
          ((CFgyEeLgykOe*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
        else
        {
          for ( cAktSzint = m_cAktSzint+1 ; ; cAktSzint++ )
          {
            if ( m_cNemzMez.m_cSzint == cAktSzint &&
                 m_cNemzMez.m_uiKulcs1 == pUjElem->m_cNemzMez.m_uiKulcs1 )
            {
              pUjElem->m_cNemzMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cCsapMez.m_cSzint == cAktSzint &&
                 m_cCsapMez.m_uiKulcs1 == pUjElem->m_cCsapMez.m_uiKulcs1 )
            {
              pUjElem->m_cCsapMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cMotorMez.m_cSzint == cAktSzint &&
                 m_cMotorMez.m_uiKulcs1 == pUjElem->m_cMotorMez.m_uiKulcs1 )
            {
              pUjElem->m_cMotorMez.m_cKirajzol = 0 ;
              continue ;
            }
            if ( m_cNagydMez.m_cSzint == cAktSzint &&
                 m_cNagydMez.m_uiKulcs1 == pUjElem->m_cNagydMez.m_uiKulcs1 )
            {
              pUjElem->m_cNagydMez.m_cKirajzol = 0 ;
              continue ;
            }
            // m_cPontMez ?!?
            if ( m_cVersMez.m_cSzint == cAktSzint &&
                 m_cVersMez.m_uiKulcs1 == pUjElem->m_cVersMez.m_uiKulcs1 )
            {
              pUjElem->m_cVersMez.m_cKirajzol = 0 ;
              continue ;
            }
            break ;
          } 
          m_pKovSzKovElem = pUjElem ;
        }
      }
      else
      { // Ha az uj elem nem egyenlo (l. fent) es nem is nagyobb, mint az aktualis
        // (l. elozo adattag) Ha nagyobb, mint a kovetkezo befuzes a ket elem koze
        if ( m_pAzSzKovElem != NULL )
        { // Hogy az if utasitasbeli cast-olas biztonsagos legyen 
          if ( !m_pAzSzKovElem->IsKindOf(RUNTIME_CLASS(CFgyEeLgykOe)) )
            return ;

          if ( pUjElem->m_cDatumMez.m_sDatum.ev <
               ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cDatumMez.m_sDatum.ev )
          { // Befuzes a ketto koze
            CBazKerEredm* pTempKerEredm = m_pAzSzKovElem ;

            m_pAzSzKovElem = pUjElem ;
            pUjElem->m_pAzSzKovElem = pTempKerEredm ;
          }
          else
          {
            ((CFgyEeLgykOe*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
        }
        else
        { // Mivel az uj elem nem nagyobb es egyenlo, mint az aktualis, es azonos
          // szinten nincs kovetkezo elem, ahol a > relaciot vizsgalni kene: felfuz
          m_pAzSzKovElem = pUjElem ;
        }
      }

      return ;
    }
// ----------------------------------------------------------------------------
    // Ha mar egyik mezo sem erzi magaenak ezt a szintet, szintnoveles nelkul
    // felvetetjuk a kovetkezo azonos szintuvel, aminek eredmenyekent lecsorog
    // az utolso helyre
    if ( m_pAzSzKovElem != NULL )
    { // Hogy az if utasitasbeli cast-olas biztonsagos legyen
      if ( !m_pAzSzKovElem->IsKindOf(RUNTIME_CLASS(CFgyEeLgykOe)) )
        return ;

      ((CFgyEeLgykOe*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
    }
    else
    {
      m_pAzSzKovElem = pUjElem ;
    }
  }
}
/*
void CFgyEeLgykOe::Kirajzol(CDC* pDC, int& nKezdY)
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  char sKmltErt[10] ;
  CString cSzoveg ;

  sprintf(sKmltErt, "%d", m_cDatumMez.m_sDatum.ev) ;
  cSzoveg = sKmltErt ;

  cPro13->m_cVersenyzoNev.Lookup(m_cVersMez.m_uiKulcs1, pVNevAdat) ;
  cSzoveg += pVNevAdat->m_sKerNev + " " + pVNevAdat->m_sVezNev + " " ;

  cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;
  cSzoveg += pNevAdat->m_sNev ;

  cPro13->m_cCsapatNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;
  cSzoveg += pNevAdat->m_sNev ;

  cPro13->m_cMotorNev.Lookup(m_cMotorMez.m_uiKulcs1, pNevAdat) ;
  cSzoveg += pNevAdat->m_sNev ;
          m_cNagydijNev
  cPro13->m_cNagydijNev.Lookup(m_cNagydMez.m_uiKulcs1, pNevAdat) ;
  cSzoveg += pNevAdat->m_sNev ;

TRACE("\n%s", (const char*) cSzoveg) ;

  if ( m_pKovSzKovElem != NULL )
    ((CFgyEeLgykOe*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;

  if ( m_pAzSzKovElem != NULL )
    ((CFgyEeLgykOe*)m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;

  return ;
}
*/
void CFgyEeLgykOe::Kirajzol(CDC* pDC, int& nKezdY)
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;
  int nX ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  CString cSzoveg ;
  char sKmltErt[100], cAktSzint ;
  CSize cMeret ;
  BOOL bAlacsSzntKi = FALSE ;

  for ( cAktSzint = m_cAktSzint ; ; cAktSzint++ )
  {
    // Sajat maga kirajzolasa | Ha meg nem rajzolta ki mas \/ elem ezt a mezot
    if ( m_cVersMez.m_cSzint == cAktSzint )
    {
      if ( m_cVersMez.m_cKirajzol )
      {
        cPro13->m_cVersenyzoNev.Lookup(m_cVersMez.m_uiKulcs1, pVNevAdat) ;
        cSzoveg = pVNevAdat->m_sKerNev + " " + pVNevAdat->m_sVezNev + " " ;
        // A versenyzonevvel egyut mindig kiirjuk a nemzetiseget is
        cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;
        cSzoveg += pNevAdat->m_sNev ;

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, " %d", m_cVersMez.m_usKmltErt) ;
        cSzoveg += sKmltErt ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( cSzoveg) ;
        m_cVersMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, cSzoveg) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cVersMez.m_cKiBe & BEKAPCS) )
      {// Nem rajzolunk tovabb magasabb szintet, kov sz kov elem
        bAlacsSzntKi = TRUE ;
        break ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cNemzMez.m_cSzint == cAktSzint )
    {
      if ( m_cNemzMez.m_cKirajzol )
      {
        cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;
        cSzoveg = pNevAdat->m_sNev ;

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, " %d", m_cNemzMez.m_usKmltErt) ;
        cSzoveg += sKmltErt ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( cSzoveg) ;
        m_cNemzMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, cSzoveg) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cNemzMez.m_cKiBe & BEKAPCS) )
      {// Nem rajzolunk tovabb magasabb szint, kov sz kov elem
        bAlacsSzntKi = TRUE ;
        break ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cCsapMez.m_cSzint == cAktSzint )
    {
      if ( m_cCsapMez.m_cKirajzol )
      {
        cPro13->m_cCsapatNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;
        cSzoveg = pNevAdat->m_sNev ;

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, " %d", m_cCsapMez.m_usKmltErt) ;
        cSzoveg += sKmltErt ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( cSzoveg) ;
        m_cCsapMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, cSzoveg) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cCsapMez.m_cKiBe & BEKAPCS) )
      {// Nem rajzolunk tovabb magasabb szint, kov sz kov elem
        bAlacsSzntKi = TRUE ;
        break ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cMotorMez.m_cSzint == cAktSzint )
    {
      if ( m_cMotorMez.m_cKirajzol )
      {
        cPro13->m_cMotorNev.Lookup(m_cMotorMez.m_uiKulcs1, pNevAdat) ;
        cSzoveg = pNevAdat->m_sNev ;

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, " %d", m_cMotorMez.m_usKmltErt) ;
        cSzoveg += sKmltErt ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( cSzoveg) ;
        m_cMotorMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, cSzoveg) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cMotorMez.m_cKiBe & BEKAPCS) )
      {// Nem rajzolunk tovabb magasabb szint, kov sz kov elem
        bAlacsSzntKi = TRUE ;
        break ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cNagydMez.m_cSzint == cAktSzint )
    {
      if ( m_cNagydMez.m_cKirajzol )
      {
        cPro13->m_cNagydijNev.Lookup(m_cNagydMez.m_uiKulcs1, pNevAdat) ;
        cSzoveg = pNevAdat->m_sNev ;

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, " %d", m_cNagydMez.m_usKmltErt) ;
        cSzoveg += sKmltErt ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( cSzoveg) ;
        m_cNagydMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, cSzoveg) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cNagydMez.m_cKiBe & BEKAPCS) )
      {// Nem rajzolunk tovabb magasabb szint, kov sz kov elem
        bAlacsSzntKi = TRUE ;
        break ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cPontMez.m_cSzint == cAktSzint )
    {
      if ( m_cPontMez.m_cKirajzol )
      {
        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, "%d", m_cPontMez.m_usKmltErt) ;
        cSzoveg = sKmltErt ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( cSzoveg) ;
        m_cPontMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, cSzoveg) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cPontMez.m_cKiBe & BEKAPCS) )
      {// Nem rajzolunk tovabb magasabb szint, kov sz kov elem
        bAlacsSzntKi = TRUE ;
        break ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cDatumMez.m_cSzint == cAktSzint )
    {
      if ( m_cDatumMez.m_cKirajzol )
      {
        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, "%d", m_cDatumMez.m_sDatum.ev) ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( sKmltErt, strlen(sKmltErt)) ;
        m_cDatumMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, sKmltErt, strlen(sKmltErt)) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cDatumMez.m_cKiBe & BEKAPCS) )
      {// Nem rajzolunk tovabb magasabb szint, kov sz kov elem
        bAlacsSzntKi = TRUE ;
        break ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    // Amikor ide er a vezerles, akkor a SZ_NEM_SZAMIT szintu tagok vannak soron
    // Mivel itt nincs mar mit csukogatni (alarendelt szinteket) nem vizsgalok
    // talalatot, igy meretmeghatarozas sem kell.

    nX = (cAktSzint+1)*gl_dX ;

    if ( m_cNagydMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cNagydijNev.Lookup(m_cNagydMez.m_uiKulcs1, pNevAdat) ;
      pDC->TextOut( nX, nKezdY, pNevAdat->m_sNev) ;
      nX += gl_nTabstop[ePalya] ;
    }

    if ( m_cVersMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cVersenyzoNev.Lookup(m_cVersMez.m_uiKulcs1, pVNevAdat) ;
      cSzoveg = pVNevAdat->m_sKerNev + " " + pVNevAdat->m_sVezNev ;

      pDC->TextOut( nX, nKezdY, cSzoveg) ;
      nX += gl_nTabstop[eVers] ;

      // A versenyzonevvel egyut kiirjuk a nemzetiseget is, ha az SZ_NEM_SZAMIT
      // szinten van (kulonben ketszer szerepelne)
      if ( m_cNemzMez.m_cSzint == SZ_NEM_SZAMIT )
      {
        cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;

        pDC->TextOut( nX, nKezdY, pNevAdat->m_sNev) ;
        nX += gl_nTabstop[eNemz] ;
      }
    }

    if ( m_cCsapMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cCsapatNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;

      pDC->TextOut( nX, nKezdY, pNevAdat->m_sNev) ;
      nX += gl_nTabstop[eCsap] ;
    }

    if ( m_cMotorMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cMotorNev.Lookup(m_cMotorMez.m_uiKulcs1, pNevAdat) ;

      pDC->TextOut( nX, nKezdY, pNevAdat->m_sNev) ;
      nX += gl_nTabstop[eMotor] ;
    }

    if ( m_cIdoeredmMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      pDC->TextOut( nX+
                    gl_nTabstop[eIdoeredm]-
                    pDC->GetTextExtent(m_cIdoeredmMez.IdoerS()).cx,
                    nKezdY, m_cIdoeredmMez.IdoerS()) ;
      nX += gl_nTabstop[eIdoeredm] ;
    }

    nKezdY += gl_dY ;

    // Ha az utolso mezok is ki lettek rajzolva az elembol: kilepes a ciklusbol
    break ;
  } // A szinteken vegiglepkedo for vege (az elem onmaga kirajzolasahoz)

  // A kovetkezo (alarendelt szintu) elem kirajzolasa
// TRACE("\n m_cAktSzint : %d", m_cAktSzint) ;
  if ( bAlacsSzntKi == FALSE )
  {
    if ( m_pKovSzKovElem != NULL )
    {
// TRACE("\n m_pKovSzKovElem") ;
      ((CFgyEeLgykOe*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;
    }
  }
  else
  {
    if ( m_pKovSzKovElem != NULL )
    {
      // nX ujrafelhasznalasa
      nX = ((CFgyEeLgykOe*)m_pKovSzKovElem)->AzMezok( m_cAktSzint+1) ;
      if ( nX == NEM_TART_HOZZA )
      {
// TRACE("\n m_pKovSzKovElem") ;
        ((CFgyEeLgykOe*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;
      }
      else
      {
        if ( nX == HOZZA_TART && m_pKovSzKovElem->m_pAzSzKovElem != NULL )
        {
// TRACE("\n m_pKovSzKovElem m_pAzSzKovElem") ;
          (((CFgyEeLgykOe*)m_pKovSzKovElem)->m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;
        }
      }
    }
  }

  // A kovetkezo (alarendelt szintu) elem kirajzolasa
  if ( m_pAzSzKovElem != NULL )
    ((CFgyEeLgykOe*)m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;

  return ;
}

BOOL CFgyEeLgykOe::EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol)
{
  char cSzntIdx ;

  if ( cEgerPoz.x < gl_dX*(cAktSzint+1) )
    return FALSE ;

  if ( m_cVersMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cVersMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cVersMez.m_cKirajzol &&
           cEgerPoz.y > ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cVersMez.m_cMezoRect.top )
      {
        return ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cVersMez.m_cKiBe & KAPCSOLO )
      {
        if ( m_cVersMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cVersMez.m_cKiBe ^= KIBE ;
          }

          return TRUE ;
        }
      }

      // Ennek az elemnek a hataskorebe(ez az elem v. alacsonyabb sz.) tartozik
      for ( cSzntIdx=cAktSzint+1 ; ; cSzntIdx++ )
      {
        if ( m_cNemzMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNemzMez.m_cKirajzol &&
               m_cNemzMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cNemzMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNemzMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cCsapMez.m_cSzint==cSzntIdx )
        {
          if ( m_cCsapMez.m_cKirajzol &&
               m_cCsapMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cCsapMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cCsapMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cMotorMez.m_cSzint==cSzntIdx )
        {
          if ( m_cMotorMez.m_cKirajzol &&
               m_cMotorMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cMotorMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cMotorMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cNagydMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNagydMez.m_cKirajzol &&
               m_cNagydMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cNagydMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNagydMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cPontMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPontMez.m_cKirajzol &&
               m_cPontMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cPontMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPontMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cDatumMez.m_cSzint==cSzntIdx )
        {
          if ( m_cDatumMez.m_cKirajzol &&
               m_cDatumMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cDatumMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cDatumMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cIdoeredmMez.m_cSzint==cSzntIdx )
        {
          if ( m_cIdoeredmMez.m_cKirajzol &&
               m_cIdoeredmMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cIdoeredmMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cIdoeredmMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL /*&& m_cVersMez.m_cKiBe & BEKAPCS*/ )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CFgyEeLgykOe*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }
      return FALSE ;
    }
  }
//------------------------------------------------------------------------------
  if ( m_cNemzMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cNemzMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cNemzMez.m_cKirajzol &&
           cEgerPoz.y > ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cNemzMez.m_cMezoRect.top )
      {
        return ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cNemzMez.m_cKiBe & KAPCSOLO  )
      {
        if ( m_cNemzMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cNemzMez.m_cKiBe ^= KIBE ;
          }

          return TRUE ;
        }
      }

      // Ennek az elemnek a hataskorebe(ez az elem v. alacsonyabb sz.) tartozik
      for ( cSzntIdx=cAktSzint+1 ; ; cSzntIdx++ )
      {
        if ( m_cVersMez.m_cSzint==cSzntIdx )
        {
          if ( m_cVersMez.m_cKirajzol &&
               m_cVersMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cVersMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cVersMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }
        if ( m_cCsapMez.m_cSzint==cSzntIdx )
        {
          if ( m_cCsapMez.m_cKirajzol &&
               m_cCsapMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cCsapMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cVersMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cMotorMez.m_cSzint==cSzntIdx )
        {
          if ( m_cMotorMez.m_cKirajzol &&
               m_cMotorMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cMotorMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cMotorMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cNagydMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNagydMez.m_cKirajzol &&
               m_cNagydMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNagydMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNagydMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cPontMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPontMez.m_cKirajzol &&
               m_cPontMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPontMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPontMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cDatumMez.m_cSzint==cSzntIdx )
        {
          if ( m_cDatumMez.m_cKirajzol &&
               m_cDatumMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cDatumMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cDatumMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cIdoeredmMez.m_cSzint==cSzntIdx )
        {
          if ( m_cIdoeredmMez.m_cKirajzol &&
               m_cIdoeredmMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cIdoeredmMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cIdoeredmMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cNemzMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CFgyEeLgykOe*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;

    }
  }
//------------------------------------------------------------------------------
  if ( m_cCsapMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cCsapMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cCsapMez.m_cKirajzol &&
           cEgerPoz.y > ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cCsapMez.m_cMezoRect.top )
      {
        return ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cCsapMez.m_cKiBe & KAPCSOLO  )
      {
        if ( m_cCsapMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cCsapMez.m_cKiBe ^= KIBE ;
          }

          return TRUE ;
        }
      }

      // Ennek az elemnek a hataskorebe(ez az elem v. alacsonyabb sz.) tartozik
      for ( cSzntIdx=cAktSzint+1 ; ; cSzntIdx++ )
      {
        if ( m_cNemzMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNemzMez.m_cKirajzol &&
               m_cNemzMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNemzMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNemzMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cVersMez.m_cSzint==cSzntIdx )
        {
          if ( m_cVersMez.m_cKirajzol &&
               m_cVersMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cVersMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cVersMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cMotorMez.m_cSzint==cSzntIdx )
        {
          if ( m_cMotorMez.m_cKirajzol &&
               m_cMotorMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cMotorMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cMotorMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cNagydMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNagydMez.m_cKirajzol &&
               m_cNagydMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNagydMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNagydMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cPontMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPontMez.m_cKirajzol &&
               m_cPontMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPontMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPontMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cDatumMez.m_cSzint==cSzntIdx )
        {
          if ( m_cDatumMez.m_cKirajzol &&
               m_cDatumMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cDatumMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cDatumMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cIdoeredmMez.m_cSzint==cSzntIdx )
        {
          if ( m_cIdoeredmMez.m_cKirajzol &&
               m_cIdoeredmMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cIdoeredmMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cIdoeredmMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cCsapMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CFgyEeLgykOe*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }

//------------------------------------------------------------------------------
  if ( m_cMotorMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cMotorMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cMotorMez.m_cKirajzol &&
           cEgerPoz.y > ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cMotorMez.m_cMezoRect.top )
      {
        return ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cMotorMez.m_cKiBe & KAPCSOLO  )
      {
        if ( m_cMotorMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cMotorMez.m_cKiBe ^= KIBE ;
          }

          return TRUE ;
        }
      }

      // Ennek az elemnek a hataskorebe(ez az elem v. alacsonyabb sz.) tartozik
      for ( cSzntIdx=cAktSzint+1 ; ; cSzntIdx++ )
      {
        if ( m_cNemzMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNemzMez.m_cKirajzol &&
               m_cNemzMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNemzMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNemzMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cCsapMez.m_cSzint==cSzntIdx )
        {
          if ( m_cCsapMez.m_cKirajzol &&
               m_cCsapMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cCsapMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cCsapMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cVersMez.m_cSzint==cSzntIdx )
        {
          if ( m_cVersMez.m_cKirajzol &&
               m_cVersMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cVersMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cVersMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cNagydMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNagydMez.m_cKirajzol &&
               m_cNagydMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNagydMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNagydMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cPontMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPontMez.m_cKirajzol &&
               m_cPontMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPontMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPontMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cDatumMez.m_cSzint==cSzntIdx )
        {
          if ( m_cDatumMez.m_cKirajzol &&
               m_cDatumMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cDatumMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cDatumMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cIdoeredmMez.m_cSzint==cSzntIdx )
        {
          if ( m_cIdoeredmMez.m_cKirajzol &&
               m_cIdoeredmMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cIdoeredmMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cIdoeredmMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cMotorMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CFgyEeLgykOe*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }
//------------------------------------------------------------------------------
  if ( m_cNagydMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cNagydMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cNagydMez.m_cKirajzol &&
           cEgerPoz.y > ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cNagydMez.m_cMezoRect.top )
      {
        return ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cNagydMez.m_cKiBe & KAPCSOLO  )
      {
        if ( m_cNagydMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cNagydMez.m_cKiBe ^= KIBE ;
          }

          return TRUE ;
        }
      }

      // Ennek az elemnek a hataskorebe(ez az elem v. alacsonyabb sz.) tartozik
      for ( cSzntIdx=cAktSzint+1 ; ; cSzntIdx++ )
      {
        if ( m_cNemzMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNemzMez.m_cKirajzol &&
               m_cNemzMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNemzMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNemzMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cCsapMez.m_cSzint==cSzntIdx )
        {
          if ( m_cCsapMez.m_cKirajzol &&
               m_cCsapMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cCsapMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cCsapMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cMotorMez.m_cSzint==cSzntIdx )
        {
          if ( m_cMotorMez.m_cKirajzol &&
               m_cMotorMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cMotorMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cMotorMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cVersMez.m_cSzint==cSzntIdx )
        {
          if ( m_cVersMez.m_cKirajzol &&
               m_cVersMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cVersMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cVersMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cPontMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPontMez.m_cKirajzol &&
               m_cPontMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPontMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPontMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cDatumMez.m_cSzint==cSzntIdx )
        {
          if ( m_cDatumMez.m_cKirajzol &&
               m_cDatumMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cDatumMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cDatumMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cIdoeredmMez.m_cSzint==cSzntIdx )
        {
          if ( m_cIdoeredmMez.m_cKirajzol &&
               m_cIdoeredmMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cIdoeredmMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cIdoeredmMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cNagydMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CFgyEeLgykOe*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }

//------------------------------------------------------------------------------
  if ( m_cDatumMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cDatumMez.m_cKirajzol )
    {

      if ( m_pAzSzKovElem != NULL &&
           ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cDatumMez.m_cKirajzol &&
           cEgerPoz.y > ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cDatumMez.m_cMezoRect.top )
      {
        return  ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cDatumMez.m_cKiBe & KAPCSOLO  )
      {
        if ( m_cDatumMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cDatumMez.m_cKiBe ^= KIBE ;
          }

          return TRUE ;
        }
      }

      // Ennek az elemnek a hataskorebe(ez az elem v. alacsonyabb sz.) tartozik
      for ( cSzntIdx=cAktSzint+1 ; ; cSzntIdx++ )
      {
        if ( m_cNemzMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNemzMez.m_cKirajzol &&
               m_cNemzMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNemzMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNemzMez.m_cKiBe ^= KIBE ;
              }
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cCsapMez.m_cSzint==cSzntIdx )
        {
          if ( m_cCsapMez.m_cKirajzol &&
               m_cCsapMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cCsapMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cCsapMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cMotorMez.m_cSzint==cSzntIdx )
        {
          if ( m_cMotorMez.m_cKirajzol &&
               m_cMotorMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cMotorMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cMotorMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cNagydMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNagydMez.m_cKirajzol &&
               m_cNagydMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNagydMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cNagydMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cPontMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPontMez.m_cKirajzol &&
               m_cPontMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPontMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPontMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cVersMez.m_cSzint==cSzntIdx )
        {
          if ( m_cVersMez.m_cKirajzol &&
               m_cVersMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cVersMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cVersMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cIdoeredmMez.m_cSzint==cSzntIdx )
        {
          if ( m_cIdoeredmMez.m_cKirajzol &&
               m_cIdoeredmMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cIdoeredmMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cIdoeredmMez.m_cKiBe ^= KIBE ;
              }

              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cDatumMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CFgyEeLgykOe*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }
//------------------------------------------------------------------------------
  if ( m_cIdoeredmMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cIdoeredmMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cIdoeredmMez.m_cKirajzol &&
           cEgerPoz.y > ((CFgyEeLgykOe*)m_pAzSzKovElem)->m_cIdoeredmMez.m_cMezoRect.top )
      {
        return ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cIdoeredmMez.m_cKiBe & KAPCSOLO  )
      {
        if ( m_cIdoeredmMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cIdoeredmMez.m_cKiBe ^= KIBE ;
          }

          return TRUE ;
        }
      }

      // Ennek az elemnek a hataskorebe(ez az elem v. alacsonyabb sz.) tartozik
      for ( cSzntIdx=cAktSzint+1 ; ; cSzntIdx++ )
      {
        if ( m_cNemzMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNemzMez.m_cKirajzol &&
               m_cNemzMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNemzMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cCsapMez.m_cSzint==cSzntIdx )
        {
          if ( m_cCsapMez.m_cKirajzol &&
               m_cCsapMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cCsapMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cMotorMez.m_cSzint==cSzntIdx )
        {
          if ( m_cMotorMez.m_cKirajzol &&
               m_cMotorMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cMotorMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cNagydMez.m_cSzint==cSzntIdx )
        {
          if ( m_cNagydMez.m_cKirajzol &&
               m_cNagydMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cNagydMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        if ( m_cPontMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPontMez.m_cKirajzol &&
               m_cPontMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPontMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cDatumMez.m_cSzint==cSzntIdx )
        {
          if ( m_cDatumMez.m_cKirajzol &&
               m_cDatumMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cDatumMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }  

          return FALSE ;
        }

        if ( m_cVersMez.m_cSzint==cSzntIdx )
        {
          if ( m_cVersMez.m_cKirajzol &&
               m_cVersMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cVersMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              return TRUE ;
            }

            // Lehet, hogy a tobbi szint is ki van rajzolva
            continue ;
          }

          return FALSE ;
        }

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cIdoeredmMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CFgyEeLgykOe*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }

  if ( m_pAzSzKovElem != NULL )
  {
    return ((CFgyEeLgykOe*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
  }

  return FALSE ;
}

int CFgyEeLgykOe::AzMezok(char cAktSzint)
{
  if ( m_cVersMez.m_cSzint == cAktSzint )
  {
    if ( m_cVersMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cNemzMez.m_cSzint == cAktSzint )
  {
    if ( m_cNemzMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cCsapMez.m_cSzint == cAktSzint )
  {
    if ( m_cCsapMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cMotorMez.m_cSzint == cAktSzint )
  {
    if ( m_cMotorMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cNagydMez.m_cSzint == cAktSzint )
  {
    if ( m_cNagydMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cPontMez.m_cSzint == cAktSzint )
  {
    if ( m_cPontMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cDatumMez.m_cSzint == cAktSzint )
  {
    if ( m_cDatumMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cIdoeredmMez.m_cSzint == cAktSzint )
  {
    if ( m_cIdoeredmMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  return KOZOMBOS_SZINT ;
}

void CFgyEeLgykOe::ScrollMeret(int& x, int& y)
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  int  nProbaX ;
  char cAktSzint ;

  for ( cAktSzint = m_cAktSzint ; ; cAktSzint++ )
  {
    if ( m_cVersMez.m_cSzint == cAktSzint )
    {
      if ( m_cVersMez.m_cKirajzol )
      {
        cPro13->m_cVersenyzoNev.Lookup(m_cVersMez.m_uiKulcs1, pVNevAdat) ;
        cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;

        // "m_sKerNev m_sVezNev m_sNev m_cVersMez.m_usKmltErt" <- fix 3
        nProbaX = pVNevAdat->m_sKerNev.GetLength() +
                  pVNevAdat->m_sVezNev.GetLength() + 2 +
                  pNevAdat->m_sNev.GetLength() + 3 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cNemzMez.m_cSzint == cAktSzint )
    {
      if ( m_cNemzMez.m_cKirajzol )
      {
        cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;

        // "m_sNev m_cNemzMez.m_usKmltErt" <- fix 3
        nProbaX = pNevAdat->m_sNev.GetLength() + 1 + 3 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cCsapMez.m_cSzint == cAktSzint )
    {
      if ( m_cCsapMez.m_cKirajzol )
      {
        cPro13->m_cCsapatNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;

        // "m_sNev m_cCsapatMez.m_usKmltErt" <- fix 3
        nProbaX = pNevAdat->m_sNev.GetLength() + 1 + 3 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cMotorMez.m_cSzint == cAktSzint )
    {
      if ( m_cMotorMez.m_cKirajzol )
      {
        cPro13->m_cMotorNev.Lookup(m_cMotorMez.m_uiKulcs1, pNevAdat) ;

        // "m_sNev m_cMotorMez.m_usKmltErt" <- fix 3
        nProbaX = pNevAdat->m_sNev.GetLength() + 1 + 3 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cNagydMez.m_cSzint == cAktSzint )
    {
      if ( m_cNagydMez.m_cKirajzol )
      {
        cPro13->m_cNagydijNev.Lookup(m_cNagydMez.m_uiKulcs1, pNevAdat) ;
        // "m_sNev m_cNagydMez.m_usKmltErt" <- fix 3
        nProbaX = pNevAdat->m_sNev.GetLength() + 1 + 3 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cPontMez.m_cSzint == cAktSzint )
    {
      if ( m_cPontMez.m_cKirajzol )
      {
        // "m_cPontMez.m_usKmltErt" <- fix 3
        nProbaX = 3 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    if ( m_cDatumMez.m_cSzint == cAktSzint )
    {
      if ( m_cDatumMez.m_cKirajzol )
      {
        // "eeee"
        nProbaX = 4 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    // Amikor ide er a vezerles, akkor a SZ_NEM_SZAMIT szintu tagok vannak soron
    nProbaX = 4*(cAktSzint+1) ;

    if ( m_cNagydMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cNagydijNev.Lookup(m_cNagydMez.m_uiKulcs1, pNevAdat) ;
      nProbaX += pNevAdat->m_sNev.GetLength() ;
    }

    if ( m_cVersMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cVersenyzoNev.Lookup(m_cVersMez.m_uiKulcs1, pVNevAdat) ;

      // "m_sKerNev m_sVezNev m_sNev m_cVersMez.m_usKmltErt" <- fix 3
      nProbaX += pVNevAdat->m_sKerNev.GetLength() +
                 pVNevAdat->m_sVezNev.GetLength() + 2 ;

      // A versenyzonevvel egyut kiirjuk a nemzetiseget is, ha az SZ_NEM_SZAMIT
      // szinten van (kulonben ketszer szerepelne)
      if ( m_cNemzMez.m_cSzint == SZ_NEM_SZAMIT )
      {
        cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;
        nProbaX += pNevAdat->m_sNev.GetLength() + 1 ;
      }
    }

    if ( m_cCsapMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cCsapatNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;
      nProbaX += pNevAdat->m_sNev.GetLength() + 1 ;
    }

    if ( m_cMotorMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      cPro13->m_cMotorNev.Lookup(m_cMotorMez.m_uiKulcs1, pNevAdat) ;
      nProbaX += pNevAdat->m_sNev.GetLength() + 1 ;
    }

    if ( m_cIdoeredmMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      nProbaX += m_cIdoeredmMez.m_cIdoeredm.IdoerS().GetLength() + 1 ;
    }

    if ( m_cPontMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      // "m_cPontMez.m_usKmltErt" <- fix 3
      nProbaX += 3 + 4*(cAktSzint+1) ;
    }

    if ( m_cDatumMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      // "eeee"
      nProbaX = 4 + 4*(cAktSzint+1) ;
    }

    if ( nProbaX > x )
      x = nProbaX ;

    y++ ;

    // Ha az utolso mezok is ki lettek rajzolva az elembol: kilepes a ciklusbol
    break ;
  } // A szinteken vegiglepkedo for vege (az elem onmaga kirajzolasahoz)

  // A kovetkezo (alarendelt szintu) elem
  if ( m_pKovSzKovElem != NULL )
    ((CFgyEeLgykOe*)m_pKovSzKovElem)->ScrollMeret( x, y) ;

  // A kovetkezo (alarendelt szintu) elem
  if ( m_pAzSzKovElem != NULL )
    ((CFgyEeLgykOe*)m_pAzSzKovElem)->ScrollMeret( x, y) ;

  return ;
}

void CFgyEeLgykOe::Serialize(CArchive& ar)
{
  CBazKerEredm::Serialize(ar) ;

  m_cVersMez.Serialize(ar) ;
  m_cNemzMez.Serialize(ar) ;
  m_cCsapMez.Serialize(ar) ; 
  m_cMotorMez.Serialize(ar);
  m_cNagydMez.Serialize(ar);
  m_cPontMez.Serialize(ar) ;
  m_cDatumMez.Serialize(ar);
  m_cIdoeredmMez.Serialize(ar) ;

  if (ar.IsStoring())
  {       /* ((CFgyEeLgykOe*) SOKKAL INKABB ! */
    ar << /*(CObject*)*/ m_pKovSzKovElem ;
    ar << /*(CObject*)*/ m_pAzSzKovElem  ;
  }
  else
  {
    ar >> /*(CObject*)*/ m_pKovSzKovElem ;
    ar >> /*(CObject*)*/ m_pAzSzKovElem  ;
  }
}

CFgyEeLgykOe::~CFgyEeLgykOe()
{
  // A bazisosztaly destruktoraba athelyezve, igy minden leszarmaztatott
  // osztalyban mukodni fog, nem kell kulon-kulon megirni
}
