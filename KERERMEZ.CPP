// kerermez.cpp - a lekerdezesek eredmenye

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

#include "proba13.h"

IMPLEMENT_SERIAL(CBazEredmMezo, CObject, 0)

CBazEredmMezo::CBazEredmMezo()
{
  m_cSzint    = SZ_NEM_SZAMIT ;
  m_usKmltErt = 1 ; // 0 volt, de onmaga mar 1-nek szamit ! (?)
  m_cKiBe     = 1 ;
  m_cKirajzol = 1 ;
  m_cMezoRect.SetRectEmpty() ; 
}

void CBazEredmMezo::Serialize(CArchive& ar)
{
  CObject::Serialize(ar) ;	
	
  if (ar.IsStoring())
  {
    ar << (BYTE) m_cSzint ;
    ar << (WORD) m_usKmltErt ;
	  ar << (BYTE) m_cKiBe ;
	  ar << (BYTE) m_cKirajzol ;
	}
	else
	{
    ar >> (BYTE&) m_cSzint ;
    ar >> (WORD&) m_usKmltErt ;
    ar >> (BYTE&) m_cKiBe ;
    ar >> (BYTE&) m_cKirajzol ;
  }
}

CBazEredmMezo::~CBazEredmMezo()
{
}
// ----------------------------------------------------------
IMPLEMENT_SERIAL(CNevEredmMezo, CBazEredmMezo, 0)

CNevEredmMezo::CNevEredmMezo()
{
  m_uiKulcs1 = 0 ;
  m_uiKulcs2 = 0 ;
}

void CNevEredmMezo::Serialize(CArchive& ar)
{
  CBazEredmMezo::Serialize(ar) ;	

  if (ar.IsStoring())
  {
    ar << m_uiKulcs1 ;
    ar << m_uiKulcs2 ;
  }
  else
  {
    ar >> m_uiKulcs1 ;
    ar >> m_uiKulcs2 ;
  }
}

CNevEredmMezo::~CNevEredmMezo()
{
}

// ----------------------------------------------------------
IMPLEMENT_SERIAL(CDatEredmMezo, CBazEredmMezo, 0)

CDatEredmMezo::CDatEredmMezo()
{
}

void CDatEredmMezo::Serialize(CArchive& ar)
{
  CBazEredmMezo::Serialize(ar) ;	

  if (ar.IsStoring())
  {
    ar << m_sDatum.ev ;
    ar << m_sDatum.honap ;
    ar << m_sDatum.nap ;
	}
	else
	{
    ar >> m_sDatum.ev ;
    ar >> m_sDatum.honap ;
    ar >> m_sDatum.nap ;
  }
}

CDatEredmMezo::~CDatEredmMezo()
{
}

// ----------------------------------------------------------
IMPLEMENT_SERIAL(CIdoerEredmMezo, CBazEredmMezo, 1)

CIdoerEredmMezo::CIdoerEredmMezo()
{
}

CString CIdoerEredmMezo::IdoerS()
{
  char pIdoer[15] ;

  if ( (m_usMegj&0x0001) == IDOEREDMENY )
  {
    return m_cIdoeredm.IdoerS() ;
  }
  else
  {
    sprintf(pIdoer, "%d kör", m_cIdoeredm.EzrdMsdp()) ;
  }

  return CString(pIdoer) ;
}

void CIdoerEredmMezo::Serialize(CArchive& ar)
{
  CBazEredmMezo::Serialize(ar) ;	

  m_cIdoeredm.Serialize(ar) ;

  if( ar.IsStoring() )
  {
    ar.Write(&m_usMegj, 2) ;
  }
  else
  {
    ar.Read( &m_usMegj, 2) ;
  }
}

CIdoerEredmMezo::~CIdoerEredmMezo()
{
}

// ----------------------------------------------------------

IMPLEMENT_SERIAL(CHelyezesMezo, CBazEredmMezo, 0)

CHelyezesMezo::CHelyezesMezo()
{
  m_ucMeret = 0 ;
  m_dwSajHlyzPnt = 0 ;
  m_pHelySzamTbl = NULL ;
}

BOOL CHelyezesMezo::UjFelvesz( unsigned short usPontszam, unsigned char ucHelyezes)
{
  int i, j ;
  DWORD  dwUjElem ;
  DWORD* pIdglHelySzamTbl = NULL ;

  if ( m_ucMeret == 0 )
  {
    m_pHelySzamTbl = new DWORD [HELY_BLOKKMERET] ;

    if ( m_pHelySzamTbl == NULL )
      return FALSE  ;

    m_dwSajHlyzPnt=(DWORD(usPontszam)<<16) + (WORD(0xff-ucHelyezes)<<8|0x0001);
    m_pHelySzamTbl[0] = m_dwSajHlyzPnt ;
    m_ucMeret = 1 ;
  }
  else
  {
    // Azonos pontszamu kulonbozo helyezesu bejegyzesek megkulonboztetesere(fel p)
    // A helyezest 255-n tarolom, hogy pl. rendezesnel 1.>2. sit. legyen !!!
    dwUjElem = (DWORD(usPontszam)<<16) + (WORD(0xff-ucHelyezes)<<8) ;

    // Van-e ilyen pontszam(1 helyezesert), mert, ha igen biztos nem kell
    // uj blokkot foglalni...
    for ( i=0 ; i<m_ucMeret && (m_pHelySzamTbl[i]&0xffffff00) > dwUjElem ; i++ ) ;

    if ( i<m_ucMeret && (m_pHelySzamTbl[i]&0xffffff00) == dwUjElem )
    {// Csak hozzaadas, a merettel nem kell foglalkozni
      if ( LOBYTE(LOWORD(m_pHelySzamTbl[i])) < 255 )
      {
        m_pHelySzamTbl[i]++ ;
        return TRUE ;
      }

      return FALSE ;
    }
    else
    {// Nincs ilyen, uj hely kell a tombben

      // Kell-e uj blokkot foglalni ?
      if ( (m_ucMeret+1)%HELY_BLOKKMERET < m_ucMeret%HELY_BLOKKMERET )
      { // Atfordult: uj blokkot kell foglalni
        pIdglHelySzamTbl=new DWORD[((m_ucMeret+1)/HELY_BLOKKMERET+1)*HELY_BLOKKMERET];

        if ( pIdglHelySzamTbl == NULL )
          return FALSE ;

        for ( j=0 ; j < i ; j++ )
          pIdglHelySzamTbl[j] = m_pHelySzamTbl[j] ;

        pIdglHelySzamTbl[j] = (DWORD(usPontszam)<<16) + (WORD(0xff-ucHelyezes)<<8|0x0001) ;

        m_ucMeret++ ;
        for ( ; j < m_ucMeret ; j++ )
          pIdglHelySzamTbl[j] = m_pHelySzamTbl[j] ;

        delete [] m_pHelySzamTbl ;

        m_pHelySzamTbl = pIdglHelySzamTbl ;

        return TRUE ;
      }
      else
      { // Ha nem kell ujabb blokkot foglalni
        j=m_ucMeret ;
        while ( j > i )
        {
          m_pHelySzamTbl[j] = m_pHelySzamTbl[j-1] ;
          j-- ;
        }

        m_pHelySzamTbl[j] = dwUjElem + 1 ;
//        (DWORD(usPontszam)<<16) + (WORD(0xff-ucHelyezes)<<8|0x0001) ;
        m_ucMeret++ ;
      }
    }
  }

  return TRUE ;
}

BOOL CHelyezesMezo::UjFelvesz(DWORD* pHelySzamTbl, unsigned char ucMeret, BOOL bPontMasol)
{
  unsigned char i, j, k, ucHozzaadva ;
  unsigned short usTulcsVizsg ;
  unsigned int uiTablMer = 0 ;
  DWORD  dwUjElem ;
  DWORD* pIdglHelySzamTbl = NULL ;

  if ( pHelySzamTbl == NULL || ucMeret == 0 )
    return TRUE ;

  if ( m_ucMeret == 0 )
  {// Ebben az esetben a pontozasnal megjegyzett sajat helyezest is kell masolni
    uiTablMer = ((ucMeret+1)/HELY_BLOKKMERET+1)*HELY_BLOKKMERET ;
    m_pHelySzamTbl =new DWORD[uiTablMer] ;

    if ( m_pHelySzamTbl == NULL )
      return FALSE  ;

    for ( i=0 ; i < ucMeret ; ucMeret++ )
      m_pHelySzamTbl[i] = pHelySzamTbl[i] ;

    return TRUE ;
  }
  else
  {
    ucHozzaadva = 0 ;

    // Azon elemek hozzaadasa, melyek mar megvannak a celtombben
    for ( i = 0 ; i < m_ucMeret ; i++ )
    {
      for ( j = 0 ; j < ucMeret ; j++ )
      {
        // Ha a ponszam es a helyezes megegyezik
        if ( (m_pHelySzamTbl[i]&0xffffff00) == (pHelySzamTbl[j]&0xffffff00) )
        { // tulcsordulasvizsgalat 
          usTulcsVizsg = (unsigned short) ((m_pHelySzamTbl[i]&0x000000ff) +
                                           (pHelySzamTbl[j]&0x000000ff) ) ;

          if ( usTulcsVizsg < 256 )
          {
            m_pHelySzamTbl[i] += pHelySzamTbl[j]&0x000000ff ;
            ucHozzaadva++ ;
          }
          else
          {
            return FALSE ;
          }
        }
      }
    }

    // Mindet megtalalta es hozzaadta ?
    if ( ucHozzaadva != ucMeret )
    {
      // Kell-e uj blokkot foglalni ?
      //    ennyi maradt az eredetibol                 ennyi kell meg
      if ( HELY_BLOKKMERET-m_ucMeret%HELY_BLOKKMERET < ucMeret-ucHozzaadva )
      {
        uiTablMer = ((m_ucMeret+ucMeret-ucHozzaadva)/HELY_BLOKKMERET+1)*HELY_BLOKKMERET ;
        pIdglHelySzamTbl = new DWORD[uiTablMer];

        if ( pIdglHelySzamTbl == NULL )
          return FALSE ;
          
        memcpy( pIdglHelySzamTbl, m_pHelySzamTbl, m_ucMeret*sizeof(DWORD)) ;

        for ( j = 0 ; j < ucMeret ; j++ )
        {
          dwUjElem = pHelySzamTbl[j]&0xffffff00 ;
          for ( i = 0 ; i < m_ucMeret ; i++ )
          {
            if ( (pIdglHelySzamTbl[i]&0xffffff00) <= dwUjElem )
              break ;
          }

          // Ez nincs meg...
          if ( (pIdglHelySzamTbl[i]&0xffffff00) != dwUjElem )
          { // i-de kell az ujat beszurni
            for ( k = m_ucMeret ; k > i ; k-- )
              pIdglHelySzamTbl[k] = pIdglHelySzamTbl[k-1] ;
          }

          if ( bPontMasol )
          {
            pIdglHelySzamTbl[i] = pHelySzamTbl[j] ;
          }
          else
          {
            pIdglHelySzamTbl[i] = pHelySzamTbl[j]&0x0000ffff ;
          }

          m_ucMeret++ ;
        }
      
        delete m_pHelySzamTbl ;
        m_pHelySzamTbl = pIdglHelySzamTbl ;
      }
      else
      { // Ha maradt meg hozzaadando, de van meg eleg hely
        for ( j = 0 ; j < ucMeret ; j++ )
        {
          dwUjElem = pHelySzamTbl[j]&0xffffff00 ;// ???
          for ( i = 0 ; i < m_ucMeret ; i++ )
          {
            if ( (m_pHelySzamTbl[i]&0xffffff00) <= dwUjElem )// <= ???
              break ; // ha a vele = elem az utolso, azt hiszi: nincs meg !
          }

          // Ez nincs meg...
          if ( (m_pHelySzamTbl[i]&0xffffff00) != dwUjElem )
          { // i-de kell az ujat beszurni
            for ( k = m_ucMeret ; k > i ; k-- )
              m_pHelySzamTbl[k] = m_pHelySzamTbl[k-1] ;

            if ( bPontMasol )
            {
              m_pHelySzamTbl[i] = pHelySzamTbl[j] ;
            }
            else
            {
              m_pHelySzamTbl[i] = pHelySzamTbl[j]&0x0000ffff ;
            }

            m_ucMeret++ ;
          }
        }
      }
    }
  }

  return TRUE ;
}

/* A rekurziv kiszamitas nehezsege miatt (max. legj. besz.)
unsigned short CHelyezesMezo::OsszpntKisz( unsigned char ucElsLegj)
{
}
*/

unsigned char CHelyezesMezo::HelyzSzam(unsigned char ucHelyezes)
{
  unsigned char uc, ucDarab = 0, ucTaroltHelyezes ;

  ucTaroltHelyezes = 0xff - ucHelyezes ;

  for ( uc = 0 ; uc < m_ucMeret ; uc++ )
  {
    if ( HIBYTE(LOWORD(m_pHelySzamTbl[uc])) == ucTaroltHelyezes )
    {
      ucDarab += LOBYTE(LOWORD(m_pHelySzamTbl[uc])) ;
    }
  }

  return ucDarab ;
}

unsigned char CHelyezesMezo::SajatHelyz()
{
/*
  unsigned char uc ;

  for ( uc = 0 ; uc < m_ucMeret ; uc++ )
  {
    if ( HIWORD(m_pHelySzamTbl[uc]) == 1 )  
    {
      return 0xff - (unsigned char) ( (m_pHelySzamTbl[uc]&0x0000ff00)>>8 ) ;
    }
  }

  return 0 ;
*/
  return 0xff - HIBYTE(LOWORD(m_dwSajHlyzPnt)) ;
}

WORD CHelyezesMezo::SajatPont()
{
  return HIWORD(m_dwSajHlyzPnt) ;
}

void CHelyezesMezo::Kiurit()
{
  if ( m_ucMeret != 0 )
  {
    m_ucMeret = 0 ;
    m_dwSajHlyzPnt = 0 ;
    delete [] m_pHelySzamTbl ;
  }
}

void CHelyezesMezo::Serialize(CArchive& ar)
{
  unsigned char uc ;

  CBazEredmMezo::Serialize(ar) ;

  if (ar.IsStoring())
  {
    ar << (BYTE) m_ucMeret ;
    ar << m_dwSajHlyzPnt   ;

    for ( uc = 0 ; uc < m_ucMeret ; uc++ )
      ar << m_pHelySzamTbl[uc] ;
  }
  else
  {
    Kiurit() ;

    ar >> (BYTE&) m_ucMeret ;
    ar >> m_dwSajHlyzPnt ;

    m_pHelySzamTbl=new DWORD[((m_ucMeret+1)/HELY_BLOKKMERET+1)*HELY_BLOKKMERET];

    for ( uc = 0 ; uc < m_ucMeret ; uc++ )
      ar >> m_pHelySzamTbl[uc] ;
  }
}

CHelyezesMezo::~CHelyezesMezo()
{
  Kiurit() ;
}

// ----------------------------------------------------------

IMPLEMENT_SERIAL(CBazKerEredm, CObject, 0)

CBazKerEredm::CBazKerEredm()
{
  m_cAktSzint    = SZ_KEZDOSZINT ;
  m_pAzSzKovElem = NULL ;
  m_pKovSzKovElem= NULL ;
}
 
void CBazKerEredm::UjFelvesz(CBazKerEredm* pUjElem)
{
}

void CBazKerEredm::Kirajzol(CDC* pDC, int& nKezdY)
{
}

void CBazKerEredm::ScrollMeret(int& x, int& y)
{
}

BOOL CBazKerEredm::EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol)
{
  return FALSE ;
}

int CBazKerEredm::AzMezok(char cAktSzint)
{
  return NEM_TART_HOZZA ;
}

void CBazKerEredm::Felszabadit()
{
  if ( m_pKovSzKovElem != NULL )
  {
    m_pKovSzKovElem->Felszabadit() ;
    delete m_pKovSzKovElem ;
  }

  if ( m_pAzSzKovElem != NULL)
  {
    m_pAzSzKovElem->Felszabadit() ;
    delete m_pAzSzKovElem ;
  }
}

void CBazKerEredm::Serialize(CArchive& ar)
{
  CObject::Serialize(ar) ;	

  if (ar.IsStoring())
  {
    ar << (BYTE) m_cAktSzint ;
  }
  else
  {
    ar >> (BYTE&) m_cAktSzint ;
  }
}

CBazKerEredm::~CBazKerEredm()
{
//  Felszabadit() ;
}
