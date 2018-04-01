// pillall.cpp

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
#include "vbszerep.h"

#include "pillall.h"

#include "proba13.h"

IMPLEMENT_SERIAL(CPillAll, CBazKerEredm, 0)

CPillAll::CPillAll()
{
  // Hogy melyik a kapcsolomezo, itt meg nem tudni <-> CFgyEeLgykOe
  // vo.: Feltolt
}

void CPillAll::Feltolt( unsigned uiVers , unsigned uiNemz, unsigned uiCsap, unsigned uiMotor,
                        unsigned uiNagyd, unsigned uiPalya, SDatum& sDatum,
                        unsigned short usPontszam, unsigned char ucHelyezes,
                        eNevtipus enNevtipus)
{
/*
TRACE( "\nuiVers:%d. uiNemz:%d uiCsap: %d  uiMotor:%d uiNagyd:%d uiPalya:%d sDatum:%d %d %d",
       uiVers,
       uiNemz,
       uiCsap,
       uiMotor,
       uiNagyd,
       uiPalya,
       sDatum.ev,
       sDatum.honap,
       sDatum.nap) ;
*/
  m_cVersMez.m_uiKulcs1  = uiVers  ;
  m_cNemzMez.m_uiKulcs1  = uiNemz  ;
  m_cCsapMez.m_uiKulcs1  = uiCsap  ;
  m_cMotorMez.m_uiKulcs1 = uiMotor ;
  m_cPalyaMez.m_uiKulcs1 = uiNagyd ;
  m_cPalyaMez.m_uiKulcs2 = uiPalya ;
  m_cDatumMez.m_sDatum   = sDatum  ;
  m_cHelyezMez.UjFelvesz( usPontszam, ucHelyezes) ;
  m_cPontMez.m_usKmltErt = usPontszam ;

  // A szintek es kumulalt ertekek beallitasa a lek.nev. fv.ben
  switch ( enNevtipus )
  {
    case eVers :
      m_cVersMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cVersMez.m_usKmltErt = usPontszam ;
      m_cVersMez.m_cKiBe = LOBYTE(KAPCSOLO | BEKAPCS) ;
      break ;

    case eNemz:
      m_cNemzMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cNemzMez.m_usKmltErt = usPontszam ;
      m_cNemzMez.m_cKiBe = LOBYTE(KAPCSOLO | BEKAPCS) ;
      break ;

    case eCsap     :
      m_cCsapMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cCsapMez.m_usKmltErt = usPontszam ;
      m_cCsapMez.m_cKiBe = LOBYTE(KAPCSOLO | BEKAPCS) ;
      break ;

    case eMotor    :
      m_cMotorMez.m_cSzint = SZ_KEZDOSZINT ;
      m_cMotorMez.m_usKmltErt = usPontszam ;
      m_cMotorMez.m_cKiBe = LOBYTE(KAPCSOLO | BEKAPCS) ;
      break ;

    default : // hiba
      ;
  }
}

void CPillAll::UjFelvesz(CPillAll* pUjElem)
{
  char cAktSzint ;
//  CVBSzerepl* pIdglPillAll ;

  if ( pUjElem->IsKindOf(RUNTIME_CLASS(CPillAll)) )
  {
    if ( pUjElem->m_cVersMez.m_cSzint == m_cAktSzint )
    {
      if ( pUjElem->m_cVersMez.m_uiKulcs1 == m_cVersMez.m_uiKulcs1 )
      { // Vegye csak fel, legalabb tudjuk, hany eredmeny volt osszesen,
        // jollehet nem feltetlen azonos az ossz indult szez. szamaval.
        m_cVersMez.m_usKmltErt += pUjElem->m_cVersMez.m_usKmltErt ;

        // Az uj elem helyezeseinek kumulalasa
        m_cHelyezMez.UjFelvesz(pUjElem->m_cHelyezMez.m_pHelySzamTbl,
                               pUjElem->m_cHelyezMez.m_ucMeret, TRUE) ;

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
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // Kell-e helyet cserelni ? - NEM, nem keszulunk fel ra
          // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {
          // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
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

        // Az uj elem helyezeseinek kumulalasa
        m_cHelyezMez.UjFelvesz(pUjElem->m_cHelyezMez.m_pHelySzamTbl,
                               pUjElem->m_cHelyezMez.m_ucMeret, TRUE) ;

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
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {
          // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
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

        // Az uj elem helyezeseinek kumulalasa
        m_cHelyezMez.UjFelvesz(pUjElem->m_cHelyezMez.m_pHelySzamTbl,
                               pUjElem->m_cHelyezMez.m_ucMeret, TRUE) ;

        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cCsapMez.m_cKirajzol = 0 ;

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
            if ( m_cVersMez.m_cSzint == cAktSzint &&
                 m_cVersMez.m_uiKulcs1 == pUjElem->m_cVersMez.m_uiKulcs1 )
            {
              pUjElem->m_cVersMez.m_cKirajzol = 0 ;
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
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        {
          // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
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

        // Az uj elem helyezeseinek kumulalasa
        m_cHelyezMez.UjFelvesz(pUjElem->m_cHelyezMez.m_pHelySzamTbl,
                               pUjElem->m_cHelyezMez.m_ucMeret, TRUE) ;

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
          m_pKovSzKovElem = pUjElem ;
        }
        else // m_pKovSzKovElem != NULL \/
        { 
          // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
        }
      }
      else
      { // Nem az ove az uj elem -> azonos szinten megy tovabb
        if ( m_pAzSzKovElem == NULL )
        {
          m_pAzSzKovElem = pUjElem ;
        }
        else
        { 
          // A kovetkezo, azonos szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
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

        // Az uj elem helyezeseinek kumulalasa
        m_cHelyezMez.UjFelvesz(pUjElem->m_cHelyezMez.m_pHelySzamTbl,
                               pUjElem->m_cHelyezMez.m_ucMeret, TRUE) ;

        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cDatumMez.m_cKirajzol = 0 ;

        if ( m_pKovSzKovElem != NULL )
        {
          ((CPillAll*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
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
        {
          ((CPillAll*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
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
                               pUjElem->m_cHelyezMez.m_ucMeret, TRUE) ;

        pUjElem->m_cAktSzint++ ;
        // Ezt a mezot a felette levo szinten valamelyik elem mar kirajzolta
        pUjElem->m_cHelyezMez.m_cKirajzol = 0 ;

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
          m_pKovSzKovElem = pUjElem ;
        }
        else
        { // A beljebb levo szintu elem foglalkozik vele tovabb
          ((CPillAll*)m_pKovSzKovElem)->UjFelvesz(pUjElem) ;
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
          ((CPillAll*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
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
      if ( !m_pAzSzKovElem->IsKindOf(RUNTIME_CLASS(CPillAll)) )
        return ;

      ((CPillAll*)m_pAzSzKovElem)->UjFelvesz(pUjElem) ;
    }
    else
    {
      m_pAzSzKovElem = pUjElem ;
    }
  }
}

void CPillAll::Kirajzol(CDC* pDC, int& nKezdY)
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;
  int nX ;
  
  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  CString cSzoveg ;
  char sKmltErt[100], cAktSzint ;
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

        // A versenyzonevvel egyut mindig kiirjuk a nemzetiseget is
        cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf( sKmltErt, "%s %s %s %.1f", (const char*)pVNevAdat->m_sKerNev, 
                                          (const char*)pVNevAdat->m_sVezNev,
                                          (const char*)pNevAdat->m_sNev,
                                          m_cHelyezMez.m_usKmltErt/2.0) ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( sKmltErt, strlen(sKmltErt)) ;
        m_cVersMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;

// TRACE("\n%s", (const char*)cSzoveg) ;
        pDC->TextOut( nX, nKezdY, sKmltErt) ;

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

        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, "%s %.1f", (const char*)pNevAdat->m_sNev,
                                   m_cHelyezMez.m_usKmltErt/2.0) ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( sKmltErt, strlen(sKmltErt)) ;
        m_cNemzMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;

// TRACE("\n%s", (const char*)cSzoveg) ;

        pDC->TextOut( nX, nKezdY, sKmltErt) ;

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
//TRACE("\n%d : %s", m_cCsapMez.m_uiKulcs1, (const char*)pNevAdat->m_sNev) ;
        // A nevhez tartozo kumulalt ertek hozzaadasa
        sprintf(sKmltErt, "%s %.1f", (const char*)pNevAdat->m_sNev,
                                   m_cHelyezMez.m_usKmltErt/2.0) ;

        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( sKmltErt, strlen(sKmltErt)) ;
        m_cCsapMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;

        pDC->TextOut( nX, nKezdY, sKmltErt) ;

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
        sprintf(sKmltErt, "%s %.1f", (const char*)pNevAdat->m_sNev,
                                   m_cHelyezMez.m_usKmltErt/2.0) ;
        cSzoveg += sKmltErt ;
// TRACE("\n(dX:%d,dY%d) %s", (cAktSzint+1)*dX, nKezdY, (const char*)cSzoveg) ;

        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( sKmltErt, strlen(sKmltErt)) ;
        m_cMotorMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;

        pDC->TextOut( nX, nKezdY, sKmltErt) ;

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

// TRACE("\n(dX:%d,dY%d) %s", (cAktSzint+1)*dX, nKezdY, (const char*)cSzoveg) ;
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
// TRACE("\n(dX:%d,dY%d) %s", (cAktSzint+1)*dX, nKezdY, (const char*)cSzoveg) ;
 
        // A kiirt szoveg befoglalo mereteinek megallapitasa
        nX = (cAktSzint+1)*gl_dX ;
        cMeret = pDC->GetTextExtent( sKmltErt, strlen(sKmltErt)) ;
        m_cDatumMez.m_cMezoRect.SetRect( nX, nKezdY, nX+cMeret.cx, nKezdY+cMeret.cy) ;

        pDC->TextOut( nX, nKezdY, sKmltErt) ;

        nKezdY += gl_dY ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    nX = (cAktSzint+1)*gl_dX ;
  
    // Amikor ide er a vezerles, akkor a SZ_NEM_SZAMIT szintu tagok vannak soron
    sprintf(sKmltErt, "%d %2.d %2.d", m_cDatumMez.m_sDatum.ev,
                                      m_cDatumMez.m_sDatum.honap,
                                      m_cDatumMez.m_sDatum.nap) ;

    pDC->TextOut( nX, nKezdY, sKmltErt, strlen(sKmltErt)) ;
    nX += gl_nTabstop[eDatum] ;

    // m_cHelyezMez.m_cKirajzol
    if ( m_cHelyezMez.m_cSzint == SZ_NEM_SZAMIT )
    {
      // A nevhez tartozo kumulalt ertek hozzaadasa
      sprintf(sKmltErt, " %d. helyezes : %d", m_cHelyezMez.SajatHelyz(),
              m_cHelyezMez.HelyzSzam(ucEredHlyz)) ;

      pDC->TextOut( nX, nKezdY, sKmltErt, strlen(sKmltErt)) ;
      nX += gl_nTabstop[ePAHelyez] ;
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
      ((CPillAll*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;
    }
  }
  else
  {
    if ( m_pKovSzKovElem != NULL )
    {
      // nX ujrafelhasznalasa
      nX = ((CPillAll*)m_pKovSzKovElem)->AzMezok( m_cAktSzint+1) ;
      if ( nX == NEM_TART_HOZZA )
      {
// TRACE("\n m_pKovSzKovElem") ;
        ((CPillAll*)m_pKovSzKovElem)->Kirajzol( pDC, nKezdY) ;
      }
      else
      {
        if ( nX == HOZZA_TART && m_pKovSzKovElem->m_pAzSzKovElem != NULL )
        {
// TRACE("\n m_pKovSzKovElem m_pAzSzKovElem") ;
          (((CPillAll*)m_pKovSzKovElem)->m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;
        }
      }
    }
  }

  // A kovetkezo (alarendelt szintu) elem kirajzolasa
  if ( m_pAzSzKovElem != NULL )
    ((CPillAll*)m_pAzSzKovElem)->Kirajzol( pDC, nKezdY) ;

  return ;
}

void CPillAll::ScrollMeret(int& x, int& y)
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  int  nProbaX ;
  char cAktSzint ;

  for ( cAktSzint = m_cAktSzint ; ; cAktSzint++ )
  {
    // Sajat maga kirajzolasa | Ha meg nem rajzolta ki mas \/ elem ezt a mezot
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
        // "eeee hh nn"
        nProbaX = 10 + 4*(cAktSzint+1) ;

        if ( nProbaX > x )
          x = nProbaX ;

        y++ ;
      }

      // Johet a kovetkezo szint
      continue ;
    }

    // Amikor ide er a vezerles, akkor a SZ_NEM_SZAMIT szintu tagok vannak soron
    nProbaX = 4*(cAktSzint+1) ;

    // m_cHelyezMez.m_cKirajzol
    if ( m_cHelyezMez.m_cSzint == SZ_NEM_SZAMIT )
    {
              //"%d. helyezes : %d"
      nProbaX +=  2 +    14     + 2 ;
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

    y++ ;

    break ;
  } // A szinteken vegiglepkedo for vege

  // A kovetkezo (alarendelt szintu) elem
  if ( m_pKovSzKovElem != NULL )
    ((CPillAll*)m_pKovSzKovElem)->ScrollMeret( x, y) ;

  // A kovetkezo (alarendelt szintu) elem
  if ( m_pAzSzKovElem != NULL )
    ((CPillAll*)m_pAzSzKovElem)->ScrollMeret( x, y) ;

  return ;
}

BOOL CPillAll::EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol)
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
           ((CPillAll*)m_pAzSzKovElem)->m_cVersMez.m_cKirajzol &&
           cEgerPoz.y > ((CPillAll*)m_pAzSzKovElem)->m_cVersMez.m_cMezoRect.top )
      {
        return ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cPalyaMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPalyaMez.m_cKirajzol &&
               m_cPalyaMez.m_cKiBe & KAPCSOLO )
          {
            if ( m_cPalyaMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPalyaMez.m_cKiBe ^= KIBE ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO )
          {
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL /*&& m_cVersMez.m_cKiBe & BEKAPCS*/ )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CPillAll*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CPillAll*)m_pAzSzKovElem)->m_cNemzMez.m_cKirajzol &&
           cEgerPoz.y > ((CPillAll*)m_pAzSzKovElem)->m_cNemzMez.m_cMezoRect.top )
      {
        return ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cPalyaMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPalyaMez.m_cKirajzol &&
               m_cPalyaMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPalyaMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPalyaMez.m_cKiBe ^= KIBE ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cNemzMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CPillAll*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CPillAll*)m_pAzSzKovElem)->m_cCsapMez.m_cKirajzol &&
           cEgerPoz.y > ((CPillAll*)m_pAzSzKovElem)->m_cCsapMez.m_cMezoRect.top )
      {
        return ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cPalyaMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPalyaMez.m_cKirajzol &&
               m_cPalyaMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPalyaMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPalyaMez.m_cKiBe ^= KIBE ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cCsapMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CPillAll*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CPillAll*)m_pAzSzKovElem)->m_cMotorMez.m_cKirajzol &&
           cEgerPoz.y > ((CPillAll*)m_pAzSzKovElem)->m_cMotorMez.m_cMezoRect.top )
      {
        return ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cPalyaMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPalyaMez.m_cKirajzol &&
               m_cPalyaMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPalyaMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPalyaMez.m_cKiBe ^= KIBE ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cMotorMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CPillAll*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }
//------------------------------------------------------------------------------
  if ( m_cPalyaMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cPalyaMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CPillAll*)m_pAzSzKovElem)->m_cPalyaMez.m_cKirajzol &&
           cEgerPoz.y > ((CPillAll*)m_pAzSzKovElem)->m_cPalyaMez.m_cMezoRect.top )
      {
        return ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cPalyaMez.m_cKiBe & KAPCSOLO  )
      {
        if ( m_cPalyaMez.m_cMezoRect.PtInRect( cEgerPoz) )
        {
          if ( bKapcsol )
          {
            m_cPalyaMez.m_cKiBe ^= KIBE ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cPalyaMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CPillAll*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
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
           ((CPillAll*)m_pAzSzKovElem)->m_cDatumMez.m_cKirajzol &&
           cEgerPoz.y > ((CPillAll*)m_pAzSzKovElem)->m_cDatumMez.m_cMezoRect.top )
      {
        return  ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
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

        if ( m_cPalyaMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPalyaMez.m_cKirajzol &&
               m_cPalyaMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPalyaMez.m_cMezoRect.PtInRect( cEgerPoz) )
            {
              if ( bKapcsol )
              {
                m_cPalyaMez.m_cKiBe ^= KIBE ;
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

        if ( m_cHelyezMez.m_cSzint==cSzntIdx )
        {
          if ( m_cHelyezMez.m_cKirajzol &&
               m_cHelyezMez.m_cKiBe & KAPCSOLO  )
          {
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

        break ;
      } // for szintek

      if ( m_pKovSzKovElem != NULL && m_cDatumMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CPillAll*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }
//------------------------------------------------------------------------------
  if ( m_cHelyezMez.m_cSzint == cAktSzint )
  {
    // Ha nem kell kirajzolni, nincs a kepernyon, nem lehet az eger folotte...
    if ( m_cHelyezMez.m_cKirajzol )
    {
      if ( m_pAzSzKovElem != NULL &&
           ((CPillAll*)m_pAzSzKovElem)->m_cHelyezMez.m_cKirajzol &&
           cEgerPoz.y > ((CPillAll*)m_pAzSzKovElem)->m_cHelyezMez.m_cMezoRect.top )
      {
        return ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
      }

      if ( m_cHelyezMez.m_cKiBe & KAPCSOLO  )
      {
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

        if ( m_cPalyaMez.m_cSzint==cSzntIdx )
        {
          if ( m_cPalyaMez.m_cKirajzol &&
               m_cPalyaMez.m_cKiBe & KAPCSOLO  )
          {
            if ( m_cPalyaMez.m_cMezoRect.PtInRect( cEgerPoz) )
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

      if ( m_pKovSzKovElem != NULL && m_cHelyezMez.m_cKiBe & BEKAPCS )
      { // Mivel nem tudom, melyik mezo van a kovetkezo szinten
        return ((CPillAll*)m_pKovSzKovElem)->EgerMozog( cEgerPoz, cAktSzint+1, bKapcsol) ;
      }

      return FALSE ;
    }
  }

  if ( m_pAzSzKovElem != NULL )
  {
    return ((CPillAll*)m_pAzSzKovElem)->EgerMozog( cEgerPoz, cAktSzint, bKapcsol) ;
  }

  return FALSE ;
}

int CPillAll::AzMezok(char cAktSzint)
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

  if ( m_cPalyaMez.m_cSzint == cAktSzint )
  {
    if ( m_cPalyaMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cDatumMez.m_cSzint == cAktSzint )
  {
    if ( m_cDatumMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cHelyezMez.m_cSzint == cAktSzint )
  {
    if ( m_cHelyezMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  if ( m_cPontMez.m_cSzint == cAktSzint )
  {
    if ( m_cPontMez.m_cKirajzol )
      return NEM_TART_HOZZA ;

    return HOZZA_TART ;
  }

  return KOZOMBOS_SZINT ;
}

void CPillAll::OsszpntKisz( unsigned char ucElsLegj)
{
  int i ;
  unsigned char ucDarab ;

  m_cHelyezMez.m_usKmltErt = 0 ;

  if ( m_cHelyezMez.m_ucMeret == 0 )
    return ;

  if ( ucElsLegj != 0 )
  {
    i = 0 ;
    while ( ucElsLegj > 0 && i < m_cHelyezMez.m_ucMeret )
    {
      ucDarab = LOBYTE(LOWORD(m_cHelyezMez.m_pHelySzamTbl[i])) ;

      if ( ucDarab < ucElsLegj )
      {
        ucElsLegj -= ucDarab ;
//TRACE("\nplusz: %d (teljes) %d", ucDarab, HIWORD(m_cHelyezMez.m_pHelySzamTbl[i])*ucDarab) ;
        m_cHelyezMez.m_usKmltErt += HIWORD(m_cHelyezMez.m_pHelySzamTbl[i])*ucDarab ;
      }
      else
      {
//TRACE("\nplusz: %d (maradek) %d", ucElsLegj, HIWORD(m_cHelyezMez.m_pHelySzamTbl[i])*ucElsLegj) ;
        m_cHelyezMez.m_usKmltErt += HIWORD(m_cHelyezMez.m_pHelySzamTbl[i])*ucElsLegj ;
        ucElsLegj = 0 ;        
      }

      i++ ;
    }
  }
  else // Minden eredmeny beszamit: pl. csapat PillAll., VBOssz. eseten
  {
    i = 0 ;
    while ( i < m_cHelyezMez.m_ucMeret )
    {
      ucDarab = LOBYTE(LOWORD(m_cHelyezMez.m_pHelySzamTbl[i])) ;
//TRACE("\nplusz: %d (teljes) %d", ucDarab, HIWORD(m_cHelyezMez.m_pHelySzamTbl[i])*ucDarab) ;
      m_cHelyezMez.m_usKmltErt += HIWORD(m_cHelyezMez.m_pHelySzamTbl[i])*ucDarab ;
      i++ ;
    }
  }
}

unsigned char CPillAll::HelyzSzam(unsigned char ucHelyezes)
{
  unsigned char ucDarab ;

  ucDarab = m_cHelyezMez.HelyzSzam( ucHelyezes) ;

  if ( m_pKovSzKovElem != NULL )
  {
    ucDarab += ((CPillAll*)m_pKovSzKovElem)->HelyzSzam( ucHelyezes) ;
  }

  if ( m_pAzSzKovElem != NULL )
  {
    ucDarab += ((CPillAll*)m_pAzSzKovElem)->HelyzSzam( ucHelyezes) ;
  }

  return ucDarab ;
}

void CPillAll::SzintMegad(char  cVersSzint,  char cNemzSzint, char cCsapSzint,
                          char cMotorSzint, char cPalyaSzint, char cDatumSzint,
                          char cHelyezSzint, char cPontSzint)
{
  m_cVersMez.m_cSzint  = cVersSzint ;
  m_cNemzMez.m_cSzint  = cNemzSzint ;
  m_cCsapMez.m_cSzint  = cCsapSzint ; 
  m_cMotorMez.m_cSzint = cMotorSzint;
  m_cPalyaMez.m_cSzint = cPalyaSzint;
  m_cDatumMez.m_cSzint = cDatumSzint;
  m_cHelyezMez.m_cSzint= cHelyezSzint ;
  m_cPontMez.m_cSzint  = cPontSzint ;
}

// A CPillAll tomb qsort-tal valo rendezesehez kell
// Helyes mukodese feltetelezi a CPillAll::OsszpntKisz elozetes meghivasat !
int PAOsszehas(const void* pCPillAll1, const void* pCPillAll2)
{
  int i ;

  // Tipusellenorzes
  if ( !((*(CPillAll**)pCPillAll1)->IsKindOf(RUNTIME_CLASS(CPillAll))) ||
       !((*(CPillAll**)pCPillAll2)->IsKindOf(RUNTIME_CLASS(CPillAll))) )
  {// Hibas tipus : igy nem rendez tulajdonkeppen
    return 0 ;
  }

  if ( (*(CPillAll**)pCPillAll1)->m_cHelyezMez.m_usKmltErt <
       (*(CPillAll**)pCPillAll2)->m_cHelyezMez.m_usKmltErt )
  {
    return 1 ;
  }
  else
  {
    if ( (*(CPillAll**)pCPillAll1)->m_cHelyezMez.m_usKmltErt >
         (*(CPillAll**)pCPillAll2)->m_cHelyezMez.m_usKmltErt )
    {
      return -1 ;
    }
    else
    {// Pontegyezoseg eseten a tobb 1.,2.,... hely dont
      for ( i=1 ; i < MAX_ERTEKELT ; i++ )
      {
        if ( (*(CPillAll**)pCPillAll1)->m_cHelyezMez.HelyzSzam(i) <
             (*(CPillAll**)pCPillAll2)->m_cHelyezMez.HelyzSzam(i) )
        {
          return 1 ;
        }
        else
        {
          if ( (*(CPillAll**)pCPillAll1)->m_cHelyezMez.HelyzSzam(i) >
               (*(CPillAll**)pCPillAll2)->m_cHelyezMez.HelyzSzam(i) )
          {
            return -1 ;
          }
        }
      }

      return 0 ;
    }
  }

  return 0 ;
}

void CPillAll::Serialize(CArchive& ar)
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

void CPillAll::Trace()
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;

  unsigned char ucMIdx ;
  DWORD         dw     ;
 
  TRACE("\nm_cVersMez") ;
/*
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cVersMez.m_cSzint , m_cVersMez.m_usKmltErt, m_cVersMez.m_cKiBe, m_cVersMez.m_cKirajzol) ;
*/
  cPro13->m_cVersenyzoNev.Lookup(m_cVersMez.m_uiKulcs1, pVNevAdat) ;

  TRACE("\n%s, %s", (const char*)pVNevAdat->m_sVezNev, (const char*)pVNevAdat->m_sKerNev) ;
//  TRACE("\nm_uiKulcs1:%d m_uiKulcs2:%d", m_cVersMez.m_uiKulcs1, m_cVersMez.m_uiKulcs2) ;
/*
  TRACE("\nm_cNemzMez") ;

  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cNemzMez.m_cSzint , m_cNemzMez.m_usKmltErt, m_cNemzMez.m_cKiBe, m_cNemzMez.m_cKirajzol) ;

  cPro13->m_cNemzetisegNev.Lookup(m_cNemzMez.m_uiKulcs1, pNevAdat) ;
  TRACE("\n%s (%d)", (const char*)pNevAdat->m_sNev, m_cNemzMez.m_uiKulcs1) ;

  TRACE("\nm_uiKulcs1:%d m_uiKulcs2:%d", m_cNemzMez.m_uiKulcs1, m_cNemzMez.m_uiKulcs2) ;
*/
  TRACE("\nm_cCsapMez") ;

  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cCsapMez.m_cSzint , m_cCsapMez.m_usKmltErt, m_cCsapMez.m_cKiBe, m_cCsapMez.m_cKirajzol) ;
 
  cPro13->m_cCsapatNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;
  TRACE("\n%s", (const char*)pNevAdat->m_sNev) ;
  TRACE("\nm_uiKulcs1:%d m_uiKulcs2:%d", m_cCsapMez.m_uiKulcs1, m_cCsapMez.m_uiKulcs2) ;
/*
  TRACE("\nm_cMotorMez") ;
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cMotorMez.m_cSzint , m_cMotorMez.m_usKmltErt, m_cMotorMez.m_cKiBe, m_cMotorMez.m_cKirajzol) ;
  TRACE("\nm_uiKulcs1:%d m_uiKulcs2:%d", m_cMotorMez.m_uiKulcs1, m_cMotorMez.m_uiKulcs2) ;

  TRACE("\nm_cPalyaMez") ;
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cPalyaMez.m_cSzint , m_cPalyaMez.m_usKmltErt, m_cPalyaMez.m_cKiBe, m_cPalyaMez.m_cKirajzol) ;
  cPro13->m_cNagydijNev.Lookup(m_cCsapMez.m_uiKulcs1, pNevAdat) ;
  TRACE("\n%s", (const char*)pNevAdat->m_sNev) ;
  TRACE("\nm_uiKulcs1:%d m_uiKulcs2:%d", m_cPalyaMez.m_uiKulcs1, m_cPalyaMez.m_uiKulcs2) ;
*/
  TRACE("\nm_cDatumMez") ;
/*
  TRACE("\nm_cSzint:%d m_usKmltErt:%d m_cKiBe:%d m_cKirajzol:%d",
           m_cDatumMez.m_cSzint , m_cDatumMez.m_usKmltErt, m_cDatumMez.m_cKiBe, m_cDatumMez.m_cKirajzol) ;
*/
  TRACE("\nev:%d honap:%d nap:%d", m_cDatumMez.m_sDatum.ev, m_cDatumMez.m_sDatum.honap, m_cDatumMez.m_sDatum.nap) ;

  TRACE("\nm_cHelyezMez") ;
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
    ((CPillAll*)m_pKovSzKovElem)->Trace() ;
  }

  if ( m_pAzSzKovElem != NULL )
  {
    TRACE("\nm_pAzSzKovElem jon ---------------") ;
    ((CPillAll*)m_pAzSzKovElem)->Trace() ;
  }

}

CPillAll::~CPillAll()
{
}
