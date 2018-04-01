// rajtsbef.cpp

#include "stdafx.h"

#include <memory.h>

#include "resource.h"
#include "alaptip.h"
#include "pontozas.h"

#include "cnev.h"
#include "cvnev.h"

#include "cszezon.h"

#include "nevzlist.h"

#include "idoeredm.h"

#include "rajtsbef.h"

#include "proba13.h" // kellhet hozza a rajtsbef.h

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE( CRBBejegyzes, CObject)

CRBBejegyzes::CRBBejegyzes() : m_cIdoeredm()
{
  m_usMegj = 0 ;
}

CRBBejegyzes::CRBBejegyzes( unsigned iniRajtszam, const CIdoeredm& iniIdoeredm, unsigned short iniMegj) :
                           m_cIdoeredm(iniIdoeredm)
{
  m_uiRajtszam = iniRajtszam ;
  m_usMegj = iniMegj ;
}

CRBBejegyzes::CRBBejegyzes( unsigned iniRajtszam, const CIdoeredm& iniIdoeredm, char pszIniMegj[17]) :
                           m_cIdoeredm(iniIdoeredm)
{
  m_uiRajtszam = iniRajtszam ;
  MegjBeall( pszIniMegj) ;
}

CRBBejegyzes::CRBBejegyzes(const CRBBejegyzes& iniCRBejegyzes) :
              m_cIdoeredm(iniCRBejegyzes.m_cIdoeredm)
{
  m_usMegj = iniCRBejegyzes.Megj() ;
}

CRBBejegyzes& CRBBejegyzes::operator=(const CRBBejegyzes& cJobbRBBej)
{
  m_cIdoeredm = cJobbRBBej.IdoerC() ;
  m_usMegj    = cJobbRBBej.Megj()   ;
  m_uiRajtszam = cJobbRBBej.Rajtsz() ;

  return *this ;
}

void CRBBejegyzes::MegjBeall(char pszIniMegj[17] )
{
  int i ;

  if ( pszIniMegj[0] == '1' )
  {
    m_usMegj = 1 ;
  }
  else
  {
    m_usMegj = 0 ;
  }

  for ( i = 1 ; i <= 15 ; i++ )
  {
    m_usMegj = m_usMegj << 1 ;
    if ( pszIniMegj[i] == '1' )
    {
      m_usMegj++ ;
    }
  }
}

void CRBBejegyzes::MegjBeallS(char pszMegj[17]) const
{
  int i ;
  unsigned short usBitMinta ;

  if ( pszMegj == NULL )
  {
    return ;
  }

  usBitMinta = 1 ;

  for ( i = 15 ; i >= 0 ; i-- )
  {
    if ( m_usMegj & usBitMinta )
    {
      pszMegj[i] = '1' ;
    }
    else
    {
      pszMegj[i] = '0' ;
    }

    usBitMinta = usBitMinta << 1 ;
  }

  pszMegj[16] = '\0' ;
}

CString CRBBejegyzes::IdoerS() const
{
  char pIdoer[15] ;

  if ( (m_usMegj&0x0001) == IDOEREDMENY )
  {
    return m_cIdoeredm.IdoerS() ;
  }
  else
  {
// Kiesesi okok ?
    sprintf(pIdoer, "%d kör", m_cIdoeredm.EzrdMsdp()) ;
  }

  return CString(pIdoer) ;
}

CRBBejegyzes::~CRBBejegyzes()
{
}

// - A - CLeggyKor - osztaly - tagfuggvenyei -----------------------------------

IMPLEMENT_DYNCREATE( CLeggyKor, CObject)

CLeggyKor::CLeggyKor() : m_cIdoeredm()
{
  // Nemletezo nevezesi lista kulcs
  m_uiNvzlKulcs = 0 ;
  m_nKorben = 0 ;
  m_fAtlSeb = (float) 0 ;
}

CLeggyKor::CLeggyKor(unsigned iniNvzlKulcs, unsigned char iniKorben,
                     CIdoeredm iniIdoeredm, float iniAtlSeb ) : 
                     m_cIdoeredm( iniIdoeredm )
{
  m_uiNvzlKulcs = iniNvzlKulcs ;
  m_nKorben = iniKorben ;
  m_fAtlSeb = iniAtlSeb ;
}

CLeggyKor::CLeggyKor(const CLeggyKor& iniCLeggyKor)
{
  m_uiNvzlKulcs = iniCLeggyKor.Nvzl() ;
  m_nKorben   = iniCLeggyKor.Korben() ;
  m_cIdoeredm = iniCLeggyKor.Idoer()  ;
  m_fAtlSeb   = iniCLeggyKor.AtlSeb() ;
}

CLeggyKor& CLeggyKor::operator=(CLeggyKor& cJobboldal)
{
  m_uiNvzlKulcs = cJobboldal.Nvzl() ;
  m_nKorben     = cJobboldal.Korben() ;
  m_cIdoeredm   = cJobboldal.Idoer()  ;
  m_fAtlSeb     = cJobboldal.AtlSeb() ;

  return *this ;
}

CString CLeggyKor::IdoerS() const
{
  return m_cIdoeredm.IdoerS() ;
}

BOOL CLeggyKor::Ures()
{
  return m_uiNvzlKulcs == 0 && m_nKorben == 0 && m_fAtlSeb == 0 ;
}

void CLeggyKor::Kiurit()
{
  // Nemletezo nevezesi lista kulcs
  m_uiNvzlKulcs = 0 ;
  m_nKorben = 0 ;
  m_fAtlSeb = (float) 0 ;
  m_cIdoeredm.IdoBeall( 0, 0, 0, 0) ;
}

CLeggyKor::~CLeggyKor()
{
}

// - A - CRajtsBef - osztaly - tagfuggvenyei -----------------------------------

IMPLEMENT_DYNCREATE( CRajtsBef, CObject)

CRajtsBef::CRajtsBef() : m_cEredmTabla()
{
  m_nMeret = 0 ;
  m_pPzNdKlcsTbl = NULL ;
  m_nBlokk = 0 ;
}

CRajtsBef::CRajtsBef(CRajtsBef& iniCRajtsBef)
{
  unsigned char uc ;

  POSITION poz ;
  unsigned uiKulcs ;
  CRBBejegyzes *pRBBejegyzes ;

  m_nMeret = iniCRajtsBef.m_nMeret ;

  if ( m_nMeret != 0 )
  {
    m_pPzNdKlcsTbl = new unsigned[m_nMeret] ;

    if ( m_pPzNdKlcsTbl == NULL )
      return ;

    for ( uc = 0 ; uc < m_nMeret ; uc++ )
    {
      m_pPzNdKlcsTbl[uc] = iniCRajtsBef.TmbIdxKlcs(uc) ;
    }

    poz = iniCRajtsBef.GetStartPosition() ;
    while( poz != NULL)
    {
      iniCRajtsBef.GetNextAssoc( poz, uiKulcs, pRBBejegyzes) ;

      pRBBejegyzes = new CRBBejegyzes() ;
      if ( pRBBejegyzes == NULL )
        return ;

      // Remelem a kulcs es tartalom nem keveredik el (GetNextAssoc !!!)
      m_cEredmTabla.SetAt( (void*)uiKulcs, (CObject*) pRBBejegyzes) ;
    }
  }
  else
  {
    m_pPzNdKlcsTbl = NULL ;
  }
}

unsigned CRajtsBef::operator[](unsigned char ucPoz)
{
  if ( ucPoz != 0 && ucPoz <= m_nMeret )
  {
    return *(m_pPzNdKlcsTbl+ucPoz-1) ;
  }

  // Ilyen kulcs ugysem lehet
  return 0 ;
}

long CRajtsBef::Felvesz(eAblTipRsBef enRsBef,
                        unsigned char ucPoz, unsigned uiNvzlKulcs,
                        CRBBejegyzes* pUjBejegyzes )
{
  CRBBejegyzes* pElsoRBBej ;
  unsigned char uc, ucToltelek ;
  unsigned* pIdPzNdKlcsTbl ;

  long nRC ;

  // Nem lehet felvenni uj elemet csak kozvetlen a vegere
  // A valosagban szerepelhetnek a rajtsorrendbol torolt,
  // de a nevezesi listaban szereplo elemmek magasabb
  // helyezesi szammal, ezeket is hatrebb tolja.
  if ( ucPoz > m_nMeret+1 || pUjBejegyzes == NULL )
    return -1 ;

  // Szerepel-e mar a felveendo rajtszam ?
  if ( RszPoz( pUjBejegyzes->Rajtsz(), ucToltelek) )
    return -1 ;

  // Az esetleges fele pontszamot ki kell terjeszteni az uj bejegyzesre
  if ( enRsBef == IDD_BEF_FELV && m_nMeret > 0 )
  {
    if ( Lookup( m_pPzNdKlcsTbl[0], pElsoRBBej) == FALSE )
      return -1 ;

    if ( (pElsoRBBej->Megj() & FELE_PONT) == FELE_PONT )
    {
      pUjBejegyzes->MegjBeall( pUjBejegyzes->Megj() | FELE_PONT ) ;
    }
    else
    {
      pUjBejegyzes->MegjBeall( pUjBejegyzes->Megj() & !FELE_PONT ) ;
    }
  }

  // Az INSERT fv. szamara biztositani kell a helyezes folytonossagat (l. fent)
  nRC = SQL_RajtsBefINSERT( enRsBef, ucPoz, uiNvzlKulcs, pUjBejegyzes) ;
  if ( nRC != 0 )
    return nRC ;

  // Kell-e uj blokkot foglalni
  if ( m_nMeret+1 > m_nBlokk*BLOKK_MERET )
  {
    pIdPzNdKlcsTbl = new unsigned[(m_nBlokk+1)*BLOKK_MERET] ;

    if ( pIdPzNdKlcsTbl == NULL )
      return -1 ;

    // Mivel ez mostmar teny :
    m_nBlokk++ ;

    // A regi tomb atmasolasa a megnovelt meretube
    memcpy( pIdPzNdKlcsTbl, m_pPzNdKlcsTbl, sizeof(unsigned)*m_nMeret) ;

    // A regi tomb mar megtette a magaet: torlese
    delete [] m_pPzNdKlcsTbl ;

    // Az atmasolt lesz az uj tomb
    m_pPzNdKlcsTbl = pIdPzNdKlcsTbl ;
  }

  m_nMeret++ ;

  // Az erintett elemek egy hellyel feljebbmasolasa
  for ( uc=m_nMeret-1 ; uc > ucPoz-1 ; uc-- )
    *(m_pPzNdKlcsTbl+uc) = *(m_pPzNdKlcsTbl+uc-1) ;

  *(m_pPzNdKlcsTbl+ucPoz-1) = uiNvzlKulcs ;

  m_cEredmTabla.SetAt( (void*)uiNvzlKulcs, (void*) pUjBejegyzes) ;

  return 0 ;
}

long CRajtsBef::Modosit( eAblTipRsBef enRsBef,
                         unsigned char ucRegiPoz,
                         unsigned char ucPoz,     unsigned uiNvzlKulcs,
                         CRBBejegyzes* pUjBejegyzes )
{
  unsigned uiPozKul ;
  unsigned uiRegiNvzlKulcs ;
  CRBBejegyzes* pRBBej ;

  long nRC ;

  // Ha az ucPoz nemletezo elemre mutat
  if ( ucPoz>m_nMeret || ucPoz==0 || ucRegiPoz>m_nMeret || ucRegiPoz==0 )
    return -1 ;

  uiRegiNvzlKulcs = *(m_pPzNdKlcsTbl+ucRegiPoz-1) ;

  // Ha nincs meg, az fatalis hiba
  if ( !m_cEredmTabla.Lookup((void*)uiRegiNvzlKulcs, (void*&) pRBBej) )
    return -1 ;

  // Az esetleges fele pontszamot ki kell terjeszteni az uj bejegyzesre
  // Itt a Felvesz()-szel ellentetben a modositando bejegyzesbol indulunk ki
  if ( enRsBef == IDD_BEF_FELV && m_nMeret > 0 )
  {
    if ( pRBBej->Megj() & FELE_PONT == FELE_PONT )
    {
      pUjBejegyzes->MegjBeall( pUjBejegyzes->Megj() | FELE_PONT ) ;
    }
    else
    {
      pUjBejegyzes->MegjBeall( pUjBejegyzes->Megj() & !FELE_PONT ) ;
    }
  }

  nRC = SQL_RajtsBefUPDATE( enRsBef, ucRegiPoz, uiRegiNvzlKulcs,
                            ucPoz,   uiNvzlKulcs, pUjBejegyzes) ;

  if ( nRC != 0 )
    return nRC ;

  // A modositando bejegyzes felszabaditasa
  delete pRBBej ;

  m_cEredmTabla.RemoveKey((void*)uiRegiNvzlKulcs) ;

  if ( ucRegiPoz == ucPoz )
  {
    *(m_pPzNdKlcsTbl+ucPoz-1) = uiNvzlKulcs ;
  }
  else
  {
    if ( ucRegiPoz < ucPoz )
    {
      uiPozKul = ucPoz - ucRegiPoz ;
    }
    else
    {
      uiPozKul = ucRegiPoz - ucPoz ;
    }

    memcpy( m_pPzNdKlcsTbl+ucRegiPoz-1, m_pPzNdKlcsTbl+ucPoz-1, sizeof(unsigned)*uiPozKul) ;
  }

  m_cEredmTabla.SetAt( (void*)uiNvzlKulcs, (void*) pUjBejegyzes) ;

  return 0 ;
}

long CRajtsBef::Torol( eAblTipRsBef enRsBef,
                       unsigned char ucPoz )
{
  unsigned      uiNvzlKulcs ;
  CRBBejegyzes  *pIdCRBBej  ;

  long nRC ;

  // Ha az ucPoz nemletezo elemre mutat
  if ( ucPoz>m_nMeret || ucPoz==0 )
    return -1 ;

  uiNvzlKulcs = *(m_pPzNdKlcsTbl+ucPoz-1) ;

  nRC = SQL_RajtsBefDELETE( enRsBef, uiNvzlKulcs, ucPoz) ;

  if ( nRC != 0 )
    return nRC ;

  if ( m_cEredmTabla.Lookup( (void*)uiNvzlKulcs, (void*&) pIdCRBBej) )
  {
    delete  pIdCRBBej ;
  }
  else
  {
    // Itt inkonzisztencia lep fel, mert a DELETE OK, vegrehajtva,
    // a map-ben viszont nem talalta
    return -1 ;
  }

  m_cEredmTabla.RemoveKey( (void*)*(m_pPzNdKlcsTbl+ucPoz-1)) ;

  memcpy( m_pPzNdKlcsTbl+ucPoz-1, m_pPzNdKlcsTbl+ucPoz, sizeof(unsigned)*(m_nMeret-ucPoz)) ;

  m_nMeret-- ;

  // A foglalt blokkok szamat nem csokkentem, ha lehetne sem
  return 0 ;
}

BOOL CRajtsBef::KerRszPoz(unsigned char ucKerRsz,
                          unsigned char& ucTalPoz, CRBBejegyzes& cTalRBBej )
{     
  unsigned char ucPoz ;
  CRBBejegyzes* pIdCRBBej ;

  ucPoz = 0 ;

  while ( ucPoz < m_nMeret )
  {
    if ( m_cEredmTabla.Lookup( (m_pPzNdKlcsTbl+ucPoz), (void*&)pIdCRBBej) == FALSE )
    {
      return FALSE ;
    }

    if ( pIdCRBBej->Rajtsz() == ucKerRsz )
    {
      ucTalPoz = ucPoz ;
      cTalRBBej = *pIdCRBBej ;

      return TRUE ;
    }

    ucPoz++ ;
  }

  return FALSE ;
}

BOOL CRajtsBef::KerPozRsz(unsigned char ucKerPoz, 
                          unsigned char& ucTalRsz, CRBBejegyzes& cTalRBBej )
{
  // ucPoz 0, ucKerPoz 1 alapu !
  unsigned char ucPoz ;
  CRBBejegyzes  *pIdCRBBej ;

  ucPoz = ucKerPoz - 1 ;
  if ( ucPoz < m_nMeret && ucPoz < ucKerPoz)
  {
    if ( !m_cEredmTabla.Lookup( (void*)m_pPzNdKlcsTbl[ucPoz], (void*&) pIdCRBBej) )
    {
      return FALSE ;
    }

    ucTalRsz  = pIdCRBBej->Rajtsz() ;
    cTalRBBej = *pIdCRBBej ;

    return TRUE ;
  }

  return FALSE ;
}
/*
unsigned char CRajtsBef::TmbIdxRsz(unsigned char ucKerPoz)
{
  CRBBejegyzes* pCRBBejegyzes ;

  if ( ucKerPoz < m_nMeret )
  {
    if ( m_cEredmTabla.Lookup( (void*)(m_pPzNdKlcsTbl+ucKerPoz), (void*&)pCRBBejegyzes) )
      return pCRBBejegyzes->Rajtsz() ;

    // Nem talalja a nevezesi lista kulcshoz tartozo bejegyzest
    return 255 ;
  }

  // Hibas index lett megadva
  return 255 ;
}
*/
unsigned CRajtsBef::TmbIdxKlcs(unsigned char ucKerPoz)
{
  if ( ucKerPoz  < m_nMeret )
  {
    return *(m_pPzNdKlcsTbl+ucKerPoz) ;
  }

  // Hibas index lett megadva : nemletezo nevezesi lista kulcs
  return 0 ;
}

BOOL CRajtsBef::RszPoz( unsigned char ucKerRsz, unsigned char& ucTalPoz)
{
  unsigned char ucPoz ;
  CRBBejegyzes* pIdCRBBej ;

  ucPoz = 0 ;

  while ( ucPoz < m_nMeret )
  {
    if ( m_cEredmTabla.Lookup( (m_pPzNdKlcsTbl+ucPoz), (void*&)pIdCRBBej) == FALSE )
    {
      return FALSE  ;
    }

    if ( pIdCRBBej->Rajtsz() == ucKerRsz )
    {
      ucTalPoz = ucPoz + 1 ;

      return TRUE ;
    }

    ucPoz++ ;
  }

  return FALSE ;
}

void CRajtsBef::Kiurit()
{
  POSITION poz ;
  unsigned uiKulcs ;
  CRBBejegyzes *pRBBejegyzes ;

  // Mivel a CRajtsBef::Torol m_pPzNdKlcsTbl-bol nem torol,
  // lehet, hogy m_nMeret==0, es m_pPzNdKlcsTbl!=NULL !
  if ( m_nBlokk > 0 )
  {
    delete [] m_pPzNdKlcsTbl ;
    m_pPzNdKlcsTbl = NULL ;
    m_nBlokk = 0 ;
  }

  if ( m_nMeret != 0 )
  {
    poz = m_cEredmTabla.GetStartPosition() ;
    while( poz != NULL)
    {
      m_cEredmTabla.GetNextAssoc( poz, (void*&)uiKulcs, (void*&)pRBBejegyzes) ;
      delete pRBBejegyzes ;
    }

    m_cEredmTabla.RemoveAll() ;

    m_nMeret = 0 ;
  }
}

CRajtsBef::~CRajtsBef()
{
  Kiurit() ;
}
