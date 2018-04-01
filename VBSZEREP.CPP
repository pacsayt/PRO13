// vbszerep.cpp

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
#include "holvkiv.h"
#include "pillall.h"

#include "vbszerep.h"

#include "proba13.h"

IMPLEMENT_SERIAL(CVBSzerepl, CBazKerEredm, 0)

CVBSzerepl::CVBSzerepl()
{
  m_cHelyezMez.m_cKiBe = LOBYTE(KAPCSOLO | BEKAPCS) ;
}

int VBOsszehas(const void* pCVBSzerepl1, const void* pCVBSzerepl2)
{
  unsigned char uc, ucDarab1, ucDarab2 ;
  unsigned char ucMaxHlyz, ucIdx, uc1, uc2 ;
  
  CVBSzerepl* p1 = *((CVBSzerepl**) pCVBSzerepl1) ;
  CVBSzerepl* p2 = *((CVBSzerepl**) pCVBSzerepl2) ;

  uc1 = p1->m_cHelyezMez.m_ucMeret ;
  uc2 = p2->m_cHelyezMez.m_ucMeret ;

  // A max helyezes megallapitasa (0xff-ucHelyez van tarolva!)
  ucMaxHlyz = 0xff ;
  for ( uc = 0 ; uc < uc1 ; uc++ )
    if ( HIBYTE(LOWORD(p1->m_cHelyezMez.m_pHelySzamTbl[uc])) < ucMaxHlyz )
      ucMaxHlyz=HIBYTE(LOWORD(p1->m_cHelyezMez.m_pHelySzamTbl[uc])) ;

  for ( uc = 0 ; uc < uc2 ; uc++ )
    if ( HIBYTE(LOWORD(p2->m_cHelyezMez.m_pHelySzamTbl[uc])) < ucMaxHlyz )
      ucMaxHlyz = HIBYTE(LOWORD(p2->m_cHelyezMez.m_pHelySzamTbl[uc])) ;

  for ( uc = 0xff ; uc >= ucMaxHlyz ; uc-- )
  {
    ucDarab1 = ucDarab2 = 0 ;
    // Hany darab uc helyezes van ? ( p1)
    for ( ucIdx = 0 ; ucIdx < uc1 ; ucIdx++ )
    {
      if ( HIBYTE(LOWORD(p1->m_cHelyezMez.m_pHelySzamTbl[ucIdx])) == uc )
      {
        ucDarab1 += LOBYTE(LOWORD(p1->m_cHelyezMez.m_pHelySzamTbl[ucIdx])) ;
      }
    }
    // Hany darab uc helyezes van ? ( p2)
    for ( ucIdx = 0 ; ucIdx < uc2 ; ucIdx++ )
    {
      if ( HIBYTE(LOWORD(p2->m_cHelyezMez.m_pHelySzamTbl[ucIdx])) == uc )
      {
        ucDarab2 += LOBYTE(LOWORD(p2->m_cHelyezMez.m_pHelySzamTbl[ucIdx])) ;
      }
    }

    if ( ucDarab1 < ucDarab2 )
      return +1 ;

    if ( ucDarab1 > ucDarab2 )
      return -1 ;
  }

  return 0 ;
}

BOOL CVBSzerepl::operator < (CVBSzerepl& cJobboldal)
{
  int nEredm = VBOsszehas( this, &cJobboldal) ;

  if ( nEredm == -1 )
    return TRUE ;

  return FALSE ;
}

void CVBSzerepl::Feltolt( unsigned uiVers , unsigned uiNemz, unsigned uiCsap, unsigned uiMotor,
                          unsigned short usEv, 
                          unsigned short usPontszam, unsigned char ucHelyezes,
                          eNevtipus enNevtipus)
{
  m_cVersMez.m_uiKulcs1  = uiVers  ;
  m_cNemzMez.m_uiKulcs1  = uiNemz  ;
  m_cCsapMez.m_uiKulcs1  = uiCsap  ;
  m_cMotorMez.m_uiKulcs1 = uiMotor ;
  m_cDatumMez.m_sDatum.ev= usEv ;
  m_cHelyezMez.UjFelvesz( usPontszam, ucHelyezes) ;

  // A helyezes mindig a masodik szinten van
  m_cHelyezMez.m_cSzint = SZ_KEZDOSZINT + 1 ;

  // A szintek es kumulalt ertekek beallitasa a lek.nev. fv.ben
  switch ( enNevtipus )
  {
    case eVers  :
      m_cVersMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    case eNemz  :
      m_cNemzMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    case eCsap  :
      m_cCsapMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    case eMotor :
      m_cMotorMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    default :
            ;
  }
}

void CVBSzerepl::Feltolt( CPillAll* pCPillAll, unsigned char ucHelyezes, eNevtipus enNevtipus)
{
  m_cVersMez.m_uiKulcs1 = pCPillAll->m_cVersMez.m_uiKulcs1 ;
  m_cNemzMez.m_uiKulcs1 = pCPillAll->m_cNemzMez.m_uiKulcs1 ;
  m_cCsapMez.m_uiKulcs1 = pCPillAll->m_cCsapMez.m_uiKulcs1 ;
  m_cMotorMez.m_uiKulcs1= pCPillAll->m_cMotorMez.m_uiKulcs1;
  m_cDatumMez.m_sDatum = pCPillAll->m_cDatumMez.m_sDatum ;

  // A pontszam helyen allo 1 mutatja esetleges tovabbi eredmenyek kumulalasa
  // eseten (az eredeti helyezest kulon adattag tarolja)
  m_cHelyezMez.UjFelvesz( 1, ucHelyezes) ;
  // A helyezes mindig a masodik szinten van
  m_cHelyezMez.m_cSzint = SZ_KEZDOSZINT + 1 ;

  // A szintek es kumulalt ertekek beallitasa a lek.nev. fv.ben
  switch ( enNevtipus )
  {
    case eVers  :
      m_cVersMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    case eNemz  :
      m_cNemzMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    case eCsap  :
      m_cCsapMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    case eMotor :
      m_cMotorMez.m_cSzint = SZ_KEZDOSZINT ;
      break ;

    default :
            ;
  }
}

void CVBSzerepl::UjFelvesz(CVBSzerepl* pUjElem)
{
  char cAktSzint ;
  CVBSzerepl* pIdglVBSz ;

  if ( pUjElem->IsKindOf(RUNTIME_CLASS(CVBSzerepl)) )
  {
    if ( pUjElem->m_cVersMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cVersMez.m_uiKulcs1 == m_cVersMez.m_uiKulcs1 )
      { // Vegye csak fel, legalabb tudjuk, hany eredmeny volt osszesen,
        // jollehet nem feltetlen azonos az ossz indult szez. szamaval.
        m_cVersMez.m_usKmltErt += pUjElem->m_cVersMez.m_usKmltErt ;
        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cVersMez.m_cKirajzol = 0 ;

        // Az egyes versenyzok VBSzerepl-esek szerinti rendezesenel ugyis csak
        // a gyokerelem szamit (ez a fv. minden esetben meghivodik) 
        if ( m_cAktSzint == SZ_KEZDOSZINT )
          m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_pHelySzamTbl,
                                  ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_ucMeret, FALSE) ;

        // Az ove az uj elem, felveszi
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
          if (m_cHelyezMez.m_cSzint == cAktSzint &&
              m_cHelyezMez.SajatHelyz()==pUjElem->m_cHelyezMez.SajatHelyz())
          {
            pUjElem->m_cHelyezMez.m_cKirajzol = 0 ;
            continue ;
          }
          if ( m_cDatumMez.m_cSzint == cAktSzint &&
               m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
          {
            pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
            continue ;
          }
          break ;
        } 

        if ( m_pKovSzKovElem == NULL )
        {
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // Kell-e helyet cserelni ?
          if ( pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pKovSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
/*
            pUjElem->m_cHelyezMez.UjFelvesz(((CVBSzerepl*)m_pKovSzKovElem)->m_cHelyezMez.m_pHelySzamTbl,
                                            ((CVBSzerepl*)m_pKovSzKovElem)->m_cHelyezMez.m_ucMeret, FALSE) ;
*/
            // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?
            pIdglVBSz = ((CVBSzerepl*)m_pKovSzKovElem) ;
            m_pKovSzKovElem = pUjElem ;
//            pUjElem->m_pKovSzKovElem = pIdglVBSz ; // ? m_pAzSzKovElem ? pUjElem->UjFelvesz( pIdglVBSz)
            pUjElem->m_pAzSzKovElem = pIdglVBSz ;
          }
          else
          {
            // A beljebb levo szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
          } 
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        { // Biztos, hogy az azonos szintu kov elemnel is a versenyzo az elso
          // igy ezzel nem kell torodni
          if ( pUjElem->m_cVersMez.m_uiKulcs1 == ((CVBSzerepl*)m_pAzSzKovElem)->m_cVersMez.m_uiKulcs1 &&
               pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            pUjElem->m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_pHelySzamTbl,
                                             ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_ucMeret, FALSE) ;
            // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?
            pIdglVBSz = ((CVBSzerepl*)m_pAzSzKovElem) ;
            m_pAzSzKovElem = pUjElem ;
            pUjElem->m_pAzSzKovElem = pIdglVBSz ; // ?? l. fent
          
            // A beljebb levo szintu elem foglalkozik vele tovabb
//            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
          else
          {
            // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
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

        // Az egyes versenyzok VBSzerepl-esek szerinti rendezesenel ugyis csak
        // a gyokerelem szamit (ez a fv. minden esetben meghivodik) 
        if ( m_cAktSzint == SZ_KEZDOSZINT )
          m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_pHelySzamTbl,
                                  ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_ucMeret, FALSE) ;

        // Az ove az uj elem, felveszi
        for ( cAktSzint = m_cAktSzint+1 ; ; cAktSzint++ )
        {
          if ( m_cVersMez.m_cSzint == cAktSzint &&
               m_cVersMez.m_uiKulcs1 == pUjElem->m_cVersMez.m_uiKulcs1 )
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
          if (m_cHelyezMez.m_cSzint == cAktSzint &&
              m_cHelyezMez.SajatHelyz()==pUjElem->m_cHelyezMez.SajatHelyz())
          {
            pUjElem->m_cHelyezMez.m_cKirajzol = 0 ;
            continue ;
          }
          if ( m_cDatumMez.m_cSzint == cAktSzint &&
               m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
          {
            pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
            continue ;
          }
          break ;
        } 

        if ( m_pKovSzKovElem == NULL )
        {
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // Kell-e helyet cserelni ?
          if ( pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pKovSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            pIdglVBSz = ((CVBSzerepl*)m_pKovSzKovElem) ;
            m_pKovSzKovElem = pUjElem ;
            pUjElem->m_pAzSzKovElem = pIdglVBSz ;
          }
          else
          {
            // A beljebb levo szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
          }
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        { // Biztos, hogy az azonos szintu kov elemnel is a nemzetiseg az elso
          // igy ezzel nem kell torodni
          if ( pUjElem->m_cNemzMez.m_uiKulcs1 == ((CVBSzerepl*)m_pAzSzKovElem)->m_cNemzMez.m_uiKulcs1 &&
               pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            pUjElem->m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_pHelySzamTbl,
                                             ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_ucMeret, FALSE) ;
            // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?
            pIdglVBSz = ((CVBSzerepl*)m_pAzSzKovElem) ;
            m_pAzSzKovElem = pUjElem ;
            pUjElem->m_pAzSzKovElem = pIdglVBSz ;
          
            // A beljebb levo szintu elem foglalkozik vele tovabb
//            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
          else
          {
            // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
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

        // Az egyes versenyzok VBSzerepl-esek szerinti rendezesenel ugyis csak
        // a gyokerelem szamit (ez a fv. minden esetben meghivodik) 
        if ( m_cAktSzint == SZ_KEZDOSZINT )
          m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_pHelySzamTbl,
                                  ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_ucMeret, FALSE) ;

        // Az ove az uj elem, felveszi
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
          if (m_cHelyezMez.m_cSzint == cAktSzint &&
              m_cHelyezMez.SajatHelyz()==pUjElem->m_cHelyezMez.SajatHelyz())
          {
            pUjElem->m_cHelyezMez.m_cKirajzol = 0 ;
            continue ;
          }
          if ( m_cDatumMez.m_cSzint == cAktSzint &&
               m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
          {
            pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
            continue ;
          }
          break ;
        } 

        if ( m_pKovSzKovElem == NULL )
        {
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // Kell-e helyet cserelni ?
          if ( pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pKovSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            pIdglVBSz = ((CVBSzerepl*)m_pKovSzKovElem) ;
            m_pKovSzKovElem = pUjElem ;
            pUjElem->m_pAzSzKovElem = pIdglVBSz ;
          }
          else
          {
            // A beljebb levo szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
          } 
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        { // Biztos, hogy az azonos szintu kov elemnel is a csapat az elso
          // igy ezzel nem kell torodni
          if ( pUjElem->m_cCsapMez.m_uiKulcs1 == ((CVBSzerepl*)m_pAzSzKovElem)->m_cCsapMez.m_uiKulcs1 &&
               pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            pUjElem->m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_pHelySzamTbl,
                                             ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_ucMeret, FALSE) ;
            // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?
            pIdglVBSz = ((CVBSzerepl*)m_pAzSzKovElem) ;
            m_pAzSzKovElem = pUjElem ;
            pUjElem->m_pAzSzKovElem = pIdglVBSz ;
          
            // A beljebb levo szintu elem foglalkozik vele tovabb
//            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
          else
          {
            // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
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

        // Az egyes versenyzok VBSzerepl-esek szerinti rendezesenel ugyis csak
        // a gyokerelem szamit (ez a fv. minden esetben meghivodik) 
        if ( m_cAktSzint == SZ_KEZDOSZINT )
          m_cHelyezMez.UjFelvesz( ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_pHelySzamTbl,
                                  ((CVBSzerepl*)pUjElem)->m_cHelyezMez.m_ucMeret, FALSE) ;

        // Az ove az uj elem, felveszi
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
          if ( m_cHelyezMez.m_cSzint == cAktSzint &&
               m_cHelyezMez.SajatHelyz()==pUjElem->m_cHelyezMez.SajatHelyz())
          {
            pUjElem->m_cHelyezMez.m_cKirajzol = 0 ;
            continue ;
          }
          if ( m_cDatumMez.m_cSzint == cAktSzint &&
               m_cDatumMez.m_sDatum.ev == pUjElem->m_cDatumMez.m_sDatum.ev )
          {
            pUjElem->m_cDatumMez.m_cKirajzol = 0 ;
            continue ;
          }
          break ;
        } 
  
        if ( m_pKovSzKovElem == NULL )
        {
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // Kell-e helyet cserelni ?
          if ( pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pKovSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?
            pIdglVBSz = ((CVBSzerepl*)m_pKovSzKovElem) ;
            m_pKovSzKovElem = pUjElem ;
            pUjElem->UjFelvesz( pIdglVBSz) ;
          }
          else
          {
            // A beljebb levo szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
          }
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        { // Biztos, hogy az azonos szintu kov elemnel is a motor az elso
          // igy ezzel nem kell torodni
          if ( pUjElem->m_cMotorMez.m_uiKulcs1 == ((CVBSzerepl*)m_pAzSzKovElem)->m_cMotorMez.m_uiKulcs1 &&
               pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?
            pIdglVBSz = ((CVBSzerepl*)m_pAzSzKovElem) ;
            m_pAzSzKovElem = pUjElem ;
            pUjElem->UjFelvesz( pIdglVBSz) ;
          }
          else
          {
            // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
        }
      }
      
      return ;
    }
// ----------------------------------------------------------------------------    
// Mivel az elkepzelt rendeltetesszeru hasznalat eseten e mezo szintje alatta
// van a helyezes mezo szintjenek, itt nem foglalkozom a rendezessel
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
          ((CVBSzerepl*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
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
            if (m_cHelyezMez.m_cSzint == cAktSzint &&
                m_cHelyezMez.SajatHelyz()==pUjElem->m_cHelyezMez.SajatHelyz())
            {
              pUjElem->m_cHelyezMez.m_cKirajzol = 0 ;
              continue ;
            }
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
          if ( !m_pAzSzKovElem->IsKindOf(RUNTIME_CLASS(CVBSzerepl)) )
            return ;

          if ( pUjElem->m_cDatumMez.m_sDatum.ev >
               ((CVBSzerepl*)m_pAzSzKovElem)->m_cDatumMez.m_sDatum.ev )
          { // Befuzes a ketto koze
            CBazKerEredm* pTempKerEredm = m_pAzSzKovElem ;

            m_pAzSzKovElem = pUjElem ;
            pUjElem->m_pAzSzKovElem = pTempKerEredm ;
          }
          else
          {
            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
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
    if ( pUjElem->m_cHelyezMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cHelyezMez.SajatHelyz() == m_cHelyezMez.SajatHelyz() )
      {
        // ? minden helyezest hozzaad
        m_cHelyezMez.m_usKmltErt += pUjElem->m_cHelyezMez.m_usKmltErt ;
        // ! Az uj elem helyezeset (~eit) a megfelelo helyezeshez adja
        m_cHelyezMez.UjFelvesz(pUjElem->m_cHelyezMez.m_pHelySzamTbl,
                               pUjElem->m_cHelyezMez.m_ucMeret, FALSE) ;

        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cHelyezMez.m_cKirajzol = 0 ;

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
          ((CVBSzerepl*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        { // Biztos, hogy az azonos szintu kov elemnel is a helyezes az elso
          // igy ezzel nem kell torodni
          if ( pUjElem->m_cHelyezMez.SajatHelyz() <
               ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.SajatHelyz() )
          {// Helycsere : az ujonnan felveendo 'van elobbre' !
            // Az eredetibol vajon torolni kell-e a 'nem sajat' helyezeseket ?
            pIdglVBSz = ((CVBSzerepl*)m_pAzSzKovElem) ;
            m_pAzSzKovElem = pUjElem ;
            pUjElem->UjFelvesz( pIdglVBSz) ;
          }
          else
          {
            // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
            ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
          }
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
      if ( !m_pAzSzKovElem->IsKindOf(RUNTIME_CLASS(CVBSzerepl)) )
        return ;

      ((CVBSzerepl*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
    }
    else
    {
      m_pAzSzKovElem = pUjElem ;
    }
  }
}
/*
void CVBSzerepl::Kirajzol(CDC* pDC, int& nKezdY)
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  char sKmltErt[50] ;
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


//  for ( int nIdx = 0 ; nIdx < m_cHelyezMez.m_ucMeret ; nIdx++)

  sprintf(sKmltErt, "%d. helyezes: %d", m_cHelyezMez.SajatHelyz(),
                                        m_cHelyezMez.SajatPont()  ) ;
  cSzoveg += sKmltErt ;

//TRACE("\n%s", (const char*) cSzoveg) ;
  pDC->TextOut( 5, nKezdY, cSzoveg) ;

  nKezdY += gl_dY ;

  if ( m_pKovSzKovElem != NULL )
    ((CVBSzerepl*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;

  if ( m_pAzSzKovElem != NULL )
    ((CVBSzerepl*)m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;

  return ;
}
*/
void CVBSzerepl::Kirajzol(CDC* pDC, int& nKezdY)
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;
  int nX ;
  
  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  CString cSzoveg ;
  char sKmltErt[50], cAktSzint ;
  unsigned char ucEredHlyz ;
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

    if ( m_cHelyezMez.m_cSzint == cAktSzint )
    {
      if ( m_cHelyezMez.m_cKirajzol )
      {
        ucEredHlyz = m_cHelyezMez.SajatHelyz() ;

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, "%d. helyezes : %d",ucEredHlyz,
                m_cHelyezMez.HelyzSzam(ucEredHlyz)) ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( sKmltErt, strlen(sKmltErt)) ;
        m_cHelyezMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;
        pDC->TextOut( nX, nKezdY, sKmltErt, strlen(sKmltErt)) ;

        nKezdY += gl_dY ;
      }

      // Ha a mezo (es minden mezo alatta) ki van kapcsolva
      if ( !(m_cHelyezMez.m_cKiBe & BEKAPCS) )
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
    nX = (cAktSzint+1)*gl_dX ;

    sprintf(sKmltErt, "%d", m_cDatumMez.m_sDatum.ev) ;

    pDC->TextOut( nX, nKezdY, sKmltErt, strlen(sKmltErt)) ;
    nX += gl_nTabstop[eDatum] ;

    if ( m_cHelyezMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      ucEredHlyz = m_cHelyezMez.SajatHelyz() ;

      // A nevhez tartozo kumulalt ertek hozzaadasa
      sprintf(sKmltErt, "%d. helyezes : %d",ucEredHlyz,
              m_cHelyezMez.HelyzSzam(ucEredHlyz)) ;

      pDC->TextOut( nX, nKezdY, sKmltErt, strlen(sKmltErt)) ;
      nX += gl_nTabstop[eHelyez] ;
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
    }

// TRACE("\n(dX:%d,dY%d) %s", (cAktSzint+1)*dX, nKezdY, (const char*)cSzoveg) ;

    nKezdY += gl_dY ;

    // Ha az utolso mezok is ki lettek rajzolva az elembol: kilepes a ciklusbol
    break ;
  } // A szinteken vegiglepkedo for vege (az elem onmaga kirajzolasahoz)

// TRACE("\n m_cAktSzint : %d", m_cAktSzint) ;
  if ( bAlacsSzntKi == FALSE )
  {
    if ( m_pKovSzKovElem != NULL )
    {
// TRACE("\n m_pKovSzKovElem") ;
      ((CVBSzerepl*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;
    }
  }
  else
  {
    if ( m_pKovSzKovElem != NULL )
    {
      // nX ujrafelhasznalasa
      nX = ((CVBSzerepl*)m_pKovSzKovElem)->AzMezok( m_cAktSzint+1) ;
      if ( nX == NEM_TART_HOZZA )
      {
// TRACE("\n m_pKovSzKovElem") ;
        ((CVBSzerepl*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;
      }
      else
      {
        if ( nX == HOZZA_TART && m_pKovSzKovElem->m_pAzSzKovElem != NULL )
        {
// TRACE("\n m_pKovSzKovElem m_pAzSzKovElem") ;
          (((CVBSzerepl*)m_pKovSzKovElem)->m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;
        }
      }
    }
  }

  // A kovetkezo (alarendelt szintu) elem kirajzolasa
  if ( m_pAzSzKovElem != NULL )
    ((CVBSzerepl*)m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;

  return ;
}

void CVBSzerepl::ScrollMeret(int& x, int& y)
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

    if ( m_cHelyezMez.m_cSzint == cAktSzint )
    {
      if ( m_cHelyezMez.m_cKirajzol )
      {
                //"%d. helyezes : %d"
        nProbaX =  2 +    14     + 2 ;

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

    // m_cHelyezMez.m_cKirajzol
    if ( m_cHelyezMez.m_cSzint == SZ_NEM_SZAMIT )
    {
              //"%d. helyezes : %d"
      nProbaX +=  2 +    14     + 2 ;
    }

    if ( m_cDatumMez.m_cSzint == cAktSzint )
    {
      nProbaX += 4 + 4*(cAktSzint+1) ;
    }

    y++ ;

    break ;
  } // A szinteken vegiglepkedo for vege

  // A kovetkezo (alarendelt szintu) elem
  if ( m_pKovSzKovElem != NULL )
    ((CVBSzerepl*)m_pKovSzKovElem)->ScrollMeret( x, y) ;

  // A kovetkezo (alarendelt szintu) elem
  if ( m_pAzSzKovElem != NULL )
    ((CVBSzerepl*)m_pAzSzKovElem)->ScrollMeret( x, y) ;

  return ;
}

BOOL CVBSzerepl::EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol)
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
           ((CVBSzerepl*)m_pAzSzKovElem)->m_cVersMez.m_cKirajzol &&
           cEgerPoz.y > ((CVBSzerepl*)m_pAzSzKovElem)->m_cVersMez.m_cMezoRect.top )
      {
        return ((CVBSzerepl*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO )
          {
/*
TRACE("\nf:%d l:%d b:%d j:%d eger:(%d,%d)", m_cHelyezMez.m_cMezoRect.top,
                                            m_cHelyezMez.m_cMezoRect.bottom,
                                            m_cHelyezMez.m_cMezoRect.left,
                                            m_cHelyezMez.m_cMezoRect.right,
                                            cEgerPoz.x,
                                            cEgerPoz.y);
*/
            if ( m_cHelyezMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cHelyezMez.m_cKiBe ^= KIBE ;
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cVersMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CVBSzerepl*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CVBSzerepl*)m_pAzSzKovElem)->m_cNemzMez.m_cKirajzol &&
           cEgerPoz.y > ((CVBSzerepl*)m_pAzSzKovElem)->m_cNemzMez.m_cMezoRect.top )
      {
        return ((CVBSzerepl*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
/*
TRACE("\nf:%d l:%d b:%d j:%d eger:(%d,%d)", m_cHelyezMez.m_cMezoRect.top,
                                            m_cHelyezMez.m_cMezoRect.bottom,
                                            m_cHelyezMez.m_cMezoRect.left,
                                            m_cHelyezMez.m_cMezoRect.right,
                                            cEgerPoz.x,
                                            cEgerPoz.y);
*/
            if ( m_cHelyezMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cHelyezMez.m_cKiBe ^= KIBE ;
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cNemzMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CVBSzerepl*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CVBSzerepl*)m_pAzSzKovElem)->m_cCsapMez.m_cKirajzol &&
           cEgerPoz.y > ((CVBSzerepl*)m_pAzSzKovElem)->m_cCsapMez.m_cMezoRect.top )
      {
        return ((CVBSzerepl*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
/*
TRACE("\nf:%d l:%d b:%d j:%d eger:(%d,%d)", m_cHelyezMez.m_cMezoRect.top,
                                            m_cHelyezMez.m_cMezoRect.bottom,
                                            m_cHelyezMez.m_cMezoRect.left,
                                            m_cHelyezMez.m_cMezoRect.right,
                                            cEgerPoz.x,
                                            cEgerPoz.y);
*/
            if ( m_cHelyezMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cHelyezMez.m_cKiBe ^= KIBE ;
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cCsapMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CVBSzerepl*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CVBSzerepl*)m_pAzSzKovElem)->m_cMotorMez.m_cKirajzol &&
           cEgerPoz.y > ((CVBSzerepl*)m_pAzSzKovElem)->m_cMotorMez.m_cMezoRect.top )
      {
        return ((CVBSzerepl*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
/*
TRACE("\nf:%d l:%d b:%d j:%d eger:(%d,%d)", m_cHelyezMez.m_cMezoRect.top,
                                            m_cHelyezMez.m_cMezoRect.bottom,
                                            m_cHelyezMez.m_cMezoRect.left,
                                            m_cHelyezMez.m_cMezoRect.right,
                                            cEgerPoz.x,
                                            cEgerPoz.y);
*/
            if ( m_cHelyezMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cHelyezMez.m_cKiBe ^= KIBE ;
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cMotorMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CVBSzerepl*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CVBSzerepl*)m_pAzSzKovElem)->m_cDatumMez.m_cKirajzol &&
           cEgerPoz.y > ((CVBSzerepl*)m_pAzSzKovElem)->m_cDatumMez.m_cMezoRect.top )
      {
        return  ((CVBSzerepl*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
/*
TRACE("\nf:%d l:%d b:%d j:%d eger:(%d,%d)", m_cHelyezMez.m_cMezoRect.top,
                                            m_cHelyezMez.m_cMezoRect.bottom,
                                            m_cHelyezMez.m_cMezoRect.left,
                                            m_cHelyezMez.m_cMezoRect.right,
                                            cEgerPoz.x,
                                            cEgerPoz.y);
*/
            if ( m_cHelyezMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cHelyezMez.m_cKiBe ^= KIBE ;
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cDatumMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CVBSzerepl*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }

//------------------------------------------------------------------------------
  if ( m_cHelyezMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cDatumMez.m_cKirajzol )
    {

      if ( m_pAzSzKovElem != NULL &&
           ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_cKirajzol &&
           cEgerPoz.y > ((CVBSzerepl*)m_pAzSzKovElem)->m_cHelyezMez.m_cMezoRect.top )
      {
        return  ((CVBSzerepl*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cHelyezMez.m_cKiBe & KAPCSOLO  )
      {
/*
TRACE("\nf:%d l:%d b:%d j:%d eger:(%d,%d)", m_cHelyezMez.m_cMezoRect.top,
                                            m_cHelyezMez.m_cMezoRect.bottom,
                                            m_cHelyezMez.m_cMezoRect.left,
                                            m_cHelyezMez.m_cMezoRect.right,
                                            cEgerPoz.x,
                                            cEgerPoz.y);
*/
        if ( m_cHelyezMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cHelyezMez.m_cKiBe ^= KIBE ;
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cHelyezMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CVBSzerepl*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }

  if ( m_pAzSzKovElem != NULL )
  {
    return ((CVBSzerepl*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
  }

  return FALSE ;
}

int CVBSzerepl::AzMezok(char cAktSzint)
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

  if ( m_cHelyezMez.m_cSzint == cAktSzint )
  {
    if ( m_cHelyezMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cDatumMez.m_cSzint == cAktSzint )
  {
    if ( m_cDatumMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  return KOZOMBOS_SZINT ;
}

void CVBSzerepl::Serialize(CArchive& ar)
{
  CBazKerEredm::Serialize(ar) ;
 
  m_cVersMez.Serialize(ar) ;
  m_cNemzMez.Serialize(ar) ;
  m_cCsapMez.Serialize(ar) ; 
  m_cMotorMez.Serialize(ar);
  m_cHelyezMez.Serialize(ar);
  m_cDatumMez.Serialize(ar);

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

void CVBSzerepl::Trace()
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;

  unsigned char ucMIdx ;
  DWORD         dw     ;
 
  TRACE("\nm_cVersMez: ") ;
/*
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cVersMez.m_cSzint , m_cVersMez.m_usKmltErt, m_cVersMez.m_cKiBe, m_cVersMez.m_cKirajzol) ;
*/
  cPro13->m_cVersenyzoNev.Lookup(m_cVersMez.m_uiKulcs1, pVNevAdat) ;
  TRACE("%s, %s\n", (const char*)pVNevAdat->m_sVezNev, (const char*)pVNevAdat->m_sKerNev) ;

  TRACE("\nm_uiKulcs1:%d m_uiKulcs2:%d", m_cVersMez.m_uiKulcs1, m_cVersMez.m_uiKulcs2) ;

  TRACE("\nm_cNemzMez: ") ;
/*
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cNemzMez.m_cSzint , m_cNemzMez.m_usKmltErt, m_cNemzMez.m_cKiBe, m_cNemzMez.m_cKirajzol) ;
*/
  cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;
  TRACE("%s\n", (const char*)pNevAdat->m_sNev) ;
  TRACE("\nm_uiKulcs1:%d m_uiKulcs2:%d", m_cNemzMez.m_uiKulcs1, m_cNemzMez.m_uiKulcs2) ;

  TRACE("\nm_cCsapMez: ") ;
/*
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cCsapMez.m_cSzint , m_cCsapMez.m_usKmltErt, m_cCsapMez.m_cKiBe, m_cCsapMez.m_cKirajzol) ;
*/
  cPro13->m_cCsapatNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;
  TRACE("%s\n", (const char*)pNevAdat->m_sNev) ;
//  TRACE("\nm_uiKulcs1:%d m_wKulcs2:%d", m_cCsapMez.m_uiKulcs1, m_cCsapMez.m_wKulcs2) ;

  TRACE("\nm_cMotorMez: ") ;
/*
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cMotorMez.m_cSzint , m_cMotorMez.m_usKmltErt, m_cMotorMez.m_cKiBe, m_cMotorMez.m_cKirajzol) ;
  TRACE("\nm_uiKulcs1:%d m_wKulcs2:%d", m_cMotorMez.m_uiKulcs1, m_cMotorMez.m_wKulcs2) ;
*/
  TRACE("\nm_cDatumMez: ") ;
/*
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cDatumMez.m_cSzint , m_cDatumMez.m_usKmltErt, m_cDatumMez.m_cKiBe, m_cDatumMez.m_cKirajzol) ;
*/
  TRACE("ev:%d honap:%d nap:%d\n", m_cDatumMez.m_sDatum.ev, m_cDatumMez.m_sDatum.honap, m_cDatumMez.m_sDatum.nap) ;

  TRACE("\nm_cHelyezMez: ") ;

  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cHelyezMez.m_cSzint , m_cHelyezMez.m_usKmltErt, m_cHelyezMez.m_cKiBe, m_cHelyezMez.m_cKirajzol) ;

  TRACE("\nm_ucMeret:%d", m_cHelyezMez.m_ucMeret) ;
  for ( ucMIdx=0 ; ucMIdx < m_cHelyezMez.m_ucMeret ; ucMIdx++ )
  {
    dw = m_cHelyezMez.m_pHelySzamTbl[ucMIdx] ;
    TRACE("\n%d. pont:%d hely:%d darab:%d ", ucMIdx, HIWORD(dw), 0xff-HIBYTE(LOWORD(dw)), LOWORD(LOBYTE(dw)) ) ;
  }

  if ( m_pKovSzKovElem != NULL )
  {
    TRACE("\nm_pKovSzKovElem jon ---------------") ;
    ((CVBSzerepl*)m_pKovSzKovElem)->Trace() ;
  }

  if ( m_pAzSzKovElem != NULL )
  {
    TRACE("\nm_pAzSzKovElem jon ---------------") ;
    ((CVBSzerepl*)m_pAzSzKovElem)->Trace() ;
  }

}

CVBSzerepl::~CVBSzerepl()
{
}

