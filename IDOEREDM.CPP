// CIdoeredm.cpp

#include "stdafx.h"

#include "idoeredm.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE(CIdoeredm, CObject)

// Default konstruktor a serialize kepesseghez
CIdoeredm::CIdoeredm()
{
  m_cOra   = 0 ;
  m_cPerc  = 0 ;
  m_cMasodperc = 0 ;
  m_shEzrdMsdp = 0 ;
}

CIdoeredm::CIdoeredm(char iniOra, char iniPerc, char iniMasodperc, short iniEzrdMsdp)
{
  if ( iniPerc>-60       && iniPerc<60       &&
       iniMasodperc>-60  && iniMasodperc<60  &&
       iniEzrdMsdp>-1000 && iniEzrdMsdp<1000    )
  {
    m_cOra  = iniOra  ;
    m_cPerc = iniPerc ;
    m_cMasodperc = iniMasodperc ;
    m_shEzrdMsdp = iniEzrdMsdp  ;
  }
  else
  {
    m_cOra   = 0 ;
    m_cPerc  = 0 ;
    m_cMasodperc = 0 ;
    m_shEzrdMsdp  = 0 ;
  }
}

CIdoeredm::CIdoeredm(const CIdoeredm& cJobbIdo)
{
  m_cOra   = cJobbIdo.m_cOra  ;
  m_cPerc  = cJobbIdo.m_cPerc ;
  m_cMasodperc = cJobbIdo.m_cMasodperc ;
  m_shEzrdMsdp = cJobbIdo.m_shEzrdMsdp ;
}

BOOL CIdoeredm::IdoBeall(char iniOra, char iniPerc, char iniMasodperc, short iniEzrdMsdp)
{
  if ( iniPerc>-60       && iniPerc<60       &&
       iniMasodperc>-60  && iniMasodperc<60  &&
       iniEzrdMsdp>-1000 && iniEzrdMsdp<1000    )
  {
    m_cOra   = iniOra ;
    m_cPerc  = iniPerc ;
    m_cMasodperc = iniMasodperc ;
    m_shEzrdMsdp  = iniEzrdMsdp  ;
     
    return TRUE ;
  }
  else
  {
    m_cOra   = 0 ;
    m_cPerc  = 0 ;
    m_cMasodperc = 0 ;
    m_shEzrdMsdp  = 0 ;
  }

  return FALSE ;
}

BOOL CIdoeredm::OraBeall(char iniOra)
{
  m_cOra = iniOra ;
  return TRUE ;
} ;

BOOL CIdoeredm::PercBeall(char iniPerc)
{
  if ( iniPerc>-60 && iniPerc<60 )
  {
    m_cPerc = iniPerc ;
    return TRUE ;
  }

  return FALSE ;
}

BOOL CIdoeredm::MasodpercBeall(char iniMasodperc)
{
  if ( iniMasodperc>-60  && iniMasodperc<60 )
  {
    m_cMasodperc = iniMasodperc ;
    return TRUE ;
  }

  return FALSE ;
}

BOOL CIdoeredm::EzrdMsdpBeall(short iniEzrdMsdp)
{
  if ( iniEzrdMsdp>-1000 && iniEzrdMsdp<1000 )
  {
    m_shEzrdMsdp = iniEzrdMsdp  ;
    return TRUE ;
  }

  return FALSE ;
}

CString CIdoeredm::IdoerS() const
{
  char pIdoer[15] ;
  char pOra[3] ;     // oo
  char pPerc[3] ;    // pp
  char pMsdPerc[3] ; // mm
  char pEzrMp[6] ;   // eee

  sprintf(pOra,     "%d", m_cOra) ;
  sprintf(pPerc,    "%d", m_cPerc) ;
  sprintf(pMsdPerc, "%d", m_cMasodperc) ;
  sprintf(pEzrMp,   "%d", m_shEzrdMsdp) ;

  if ( m_shEzrdMsdp >99 )
  {
    sprintf(pEzrMp, "%d", m_shEzrdMsdp) ;
  }
  else
  {
    if ( m_shEzrdMsdp > 9 )
    {
      sprintf(pEzrMp, "0%d", m_shEzrdMsdp) ;
    }
    else
    {
      sprintf(pEzrMp, "00%d", m_shEzrdMsdp) ;
    }
  }
   
  if ( m_cPerc == 0 )
  {
    sprintf(pMsdPerc, "%d", m_cMasodperc) ;
  }
  else
  {
    if ( m_cMasodperc > 9 )
    {
      sprintf(pMsdPerc, "%d", m_cMasodperc) ;
    }
    else
    {
      sprintf(pMsdPerc, "0%d", m_cMasodperc) ;
    }
  }

  if ( m_cOra == 0 )
  {
    sprintf(pPerc, "%d", m_cPerc) ;
  }
  else
  {
    if ( m_cPerc > 9 )
    {
      sprintf(pPerc, "%d", m_cPerc) ;
    }
    else
    {
      sprintf(pPerc, "0%d", m_cPerc) ;
    }
  }

  if ( m_cOra == 0 )
  {
    if ( m_cPerc == 0 )
    {
      sprintf(pIdoer, "%s.%s''", pMsdPerc, pEzrMp) ;
    }
    else
    {
      sprintf(pIdoer, "%s'%s.%s''", pPerc, pMsdPerc, pEzrMp) ;
    }
  }
  else
  {
    sprintf(pIdoer, "%s:%s'%s.%s''", pOra, pPerc, pMsdPerc, pEzrMp) ;
  }

  return CString(pIdoer) ;
}

BOOL CIdoeredm::operator<(const CIdoeredm& cJobbIdo)
{
  if ( m_cOra < cJobbIdo.m_cOra )
  {
    return TRUE ;
  }
  else
  {
    if ( m_cOra == cJobbIdo.m_cOra )
    {
      if ( m_cPerc < cJobbIdo.m_cPerc )
      {
        return TRUE ;
      }
      else
      {
        if ( m_cPerc == cJobbIdo.m_cPerc )
        {
          if ( m_shEzrdMsdp < cJobbIdo.m_shEzrdMsdp )
          {
            return TRUE ;
          }
          else
          {
            return FALSE ;
          }
        }
        else
        {
          return FALSE ;
        }
      }
    }
    else
    {
      return FALSE ;
    }
  }

  return FALSE ;
}

BOOL CIdoeredm::operator>(const CIdoeredm& cJobbIdo)
{
  if ( m_cOra > cJobbIdo.m_cOra )
  {
    return TRUE ;
  }
  else
  {
    if ( m_cOra == cJobbIdo.m_cOra )
    {
      if ( m_cPerc > cJobbIdo.m_cPerc )
      {
        return TRUE ;
      }
      else
      {
        if ( m_cPerc == cJobbIdo.m_cPerc )
        {
          if ( m_shEzrdMsdp > cJobbIdo.m_shEzrdMsdp )
          {
            return TRUE ;
          }
          else
          {
            return FALSE ;
          }
        }
        else
        {
          return FALSE ;
        }
      }
    }
    else
    {
      return FALSE ;
    }
  }

  return FALSE ;
}

BOOL CIdoeredm::operator==(const CIdoeredm& cJobbIdo)
{
  if ( m_cOra == cJobbIdo.m_cOra && m_cPerc == cJobbIdo.m_cPerc &&
       m_cMasodperc == cJobbIdo.m_cMasodperc && m_shEzrdMsdp == cJobbIdo.m_shEzrdMsdp )
    return TRUE ;

  return FALSE ;
}

BOOL CIdoeredm::operator<=(const CIdoeredm& cJobbIdo)
{
  if ( *this < cJobbIdo || *this == cJobbIdo )
    return TRUE ;

  return FALSE ;
}

BOOL CIdoeredm::operator>=(const CIdoeredm& cJobbIdo)
{
  if ( *this > cJobbIdo || *this == cJobbIdo )
    return TRUE ;

  return FALSE ;
}

CIdoeredm& CIdoeredm::operator=(const CIdoeredm& cJobbIdo)
{
  m_cOra  = cJobbIdo.m_cOra  ;
  m_cPerc = cJobbIdo.m_cPerc ;
  m_cMasodperc = cJobbIdo.m_cMasodperc ;
  m_shEzrdMsdp = cJobbIdo.m_shEzrdMsdp ;

  return *this ;
}

CIdoeredm& CIdoeredm::operator+(const CIdoeredm& cJobbIdo)
{
  m_shEzrdMsdp = m_shEzrdMsdp + cJobbIdo.m_shEzrdMsdp ;
  m_cMasodperc = m_cMasodperc + cJobbIdo.m_cMasodperc + m_shEzrdMsdp/1000 ;
  m_cPerc = m_cPerc + cJobbIdo.m_cPerc + m_cMasodperc/60 ;
  m_cOra  = cJobbIdo.m_cOra + m_cPerc/60 ;

  m_shEzrdMsdp = m_shEzrdMsdp % 1000 ;
  m_cMasodperc = m_cMasodperc % 60   ;
  m_cPerc      = m_cPerc % 60        ;

  return *this ;
}

CIdoeredm& CIdoeredm::operator-(const CIdoeredm& cJobbIdo)
{
  m_shEzrdMsdp = m_shEzrdMsdp - cJobbIdo.m_shEzrdMsdp ;
  m_cMasodperc = m_cMasodperc - cJobbIdo.m_cMasodperc - m_shEzrdMsdp/1000 ;
  m_cPerc = m_cPerc - cJobbIdo.m_cPerc - m_cMasodperc/60 ;
  m_cOra  = cJobbIdo.m_cOra - m_cPerc/60 ;

  m_shEzrdMsdp = m_shEzrdMsdp % 1000 ;
  m_cMasodperc = m_cMasodperc % 60   ;
  m_cPerc      = m_cPerc % 60        ;

  return *this ;
}

CIdoeredm::~CIdoeredm()
{
}