/////////////////////////////////////////////////////////////////////////////
// cszezon.cpp

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"
#include "pontozas.h"

#include "cszezon.h"

#include "nevzlist.h"

#include "idoeredm.h"
#include "rajtsbef.h"

#include "cnev.h"
#include "cvnev.h"

#include "proba13.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE( CSzezon, CObject)

/////////////////////////////////////////////////////////////////////////////
// CSzezon construction/destruction
CSzezon::CSzezon()
{
  m_uiSzezon    = 0 ;
  m_ucNagydSzam = 0 ;
  m_uiPontozas  = 0 ;
  m_ucEdzElsPont   = 0 ;
  m_ucLeggyKorPont = 0 ;
  m_ucElsLgjBesz   = 0 ;
  m_pNagydijak     = NULL ;
  m_nBlokk         = 0 ;

//  m_pNagydijak = new  SNagydij* [ND_FOGL_BLK_MER] ;
}

unsigned CSzezon::NagydKulcs( unsigned char ucIdx)
{
  if ( ucIdx < m_ucNagydSzam )
  {
    return m_pNagydijak[ucIdx]->m_uiNagyd ;
  }

  return 0 ;
}

unsigned CSzezon::PalyaKulcs( unsigned char ucIdx)
{
  if ( ucIdx < m_ucNagydSzam )
  {
    return m_pNagydijak[ucIdx]->m_uiPalya ;
  }

  return 0 ;
}

// Hibas index eseten mindegyik fv. 0-t ad vissza
unsigned short CSzezon::NagydEv( unsigned char ucIdx)
{
  if ( ucIdx < m_ucNagydSzam )
  {
    return m_pNagydijak[ucIdx]->m_sDatum.ev ;
  }

  return 0 ;
}

unsigned char CSzezon::NagydHonap( unsigned char ucIdx)
{
  if ( ucIdx < m_ucNagydSzam )
  {
    return m_pNagydijak[ucIdx]->m_sDatum.honap ;
  }

  return 0 ;
}

unsigned char CSzezon::NagydNap( unsigned char ucIdx)
{
  if ( ucIdx < m_ucNagydSzam )
  {
    return m_pNagydijak[ucIdx]->m_sDatum.nap ;
  }

  return 0 ;
}

long CSzezon::SzezonFelvesz(unsigned char& ucFelvNd)
{
  int i ;

  SNagydij** pNagyd ;

  long l_SQLCODE ;

  ucFelvNd = 0 ;

  l_SQLCODE = SQL_SzezINSERT() ;

  // Unique key (SZEZON.PK_SZEZON) voilated -1 : a szezon szerepel az adatbazisban
  // Hatha azonban valamely nagydij lett torolve
  if ( l_SQLCODE == 0 || l_SQLCODE == -1 )
  {
    // A szezon pontozasainak es beszamitott elso legjobb eredmenyeinek modositasa
    l_SQLCODE = SQL_SzezUPDATE() ;
    if ( l_SQLCODE != 0 )
    {
      return l_SQLCODE ;
    }

    i = 0 ;
    while( i < m_ucNagydSzam )
    {
      if ( m_pNagydijak[i] != NULL )
      {
        pNagyd = &(m_pNagydijak[i]) ;

        // ORA-00001: unique constraint (SCOTT.UK_NAGYDIJAK) violated
        l_SQLCODE = SQL_NagydINSERT( **pNagyd) ;
        if ( l_SQLCODE == 0 )
        {
          ucFelvNd++ ;
        }
      }

      i++ ;
    }
  }

  return l_SQLCODE ;
}

long CSzezon::SzezonTorol()
{
  long l_SQLCODE ;

  l_SQLCODE = SQL_SzezTorol() ;
  if ( l_SQLCODE == 0 )
  {
    Kiurit() ;
  }

  return l_SQLCODE ;
}

long CSzezon::UjFelv(SNagydij* pUjNagyd)
{
  long l_SQLCODE ;
  SNagydij**    pIdNagyd ;
  unsigned char ucBeszIdx ;
  unsigned char ucFelvNdSzama ;

  int i ;

  if ( pUjNagyd == NULL )
    return -1 ;

  // Megnezi, szerepel-e mar ez a NAGYDIJ
  // ha igen, nem veszi fel
  i=0 ;
  while( i < m_ucNagydSzam )
  {
    if (m_pNagydijak[i]->m_uiNagyd == pUjNagyd->m_uiNagyd)
      return -1 ;

    i++ ;
  }

  // Megprobalja felvenni az adatbazisba, a tombbe csak aztan szurjuk be
  l_SQLCODE = SQL_NagydINSERT( *pUjNagyd) ;

  if ( l_SQLCODE != 0 )
  { 
    // Ha a szezon eve nincs benn meg a szezon tablaban, fel kell venni,
    // kulonben nem lehet nagydijat felveni (ORA-02291)
    if ( l_SQLCODE == -2291 )
    {
      // Lehet, hogy a szezonban mar volt felveve nagydij, amikor toroltek
      l_SQLCODE = SzezonFelvesz( ucFelvNdSzama) ;

      // ORA-00001: unique constraint (SCOTT.UK_NAGYDIJAK/SCOTT.PK_SZEZON) violated
      // , mert lehet, hogy nem az osszes nagydijat toroltek (-1)
      if ( l_SQLCODE == 0 || l_SQLCODE == -1 )
      {
        if ( (l_SQLCODE = SQL_NagydINSERT( *pUjNagyd)) != 0 )
        {
          return l_SQLCODE ;
        }
      }
/*
      l_SQLCODE = SQL_SzezINSERT() ;

      // Mostmar biztos van ilyen szezon az adatbazisban !
      if ( l_SQLCODE != 0 )
      {
        l_SQLCODE = SQL_NagydINSERT( *pUjNagyd) ;
      }
      else
      {
        return l_SQLCODE ;
      }
*/
    }
    else
    {
      return l_SQLCODE ;
    }
  }

  if ( l_SQLCODE == 0 )
  {
    // Biztositani kell a szezonon belul, hogy a futamok datuma novekedjen az
    // index-szel, tehat az ujonnan felveendo datuma kesobbi legyen, mint az
    // utolsoe :
    for( ucBeszIdx = 0 ; ucBeszIdx < m_ucNagydSzam ; ucBeszIdx++ )
    {
      if ( pUjNagyd->m_sDatum < m_pNagydijak[ucBeszIdx]->m_sDatum )
        break ;
    }

    // Kell-e az uj elem miatt ujrafoglalni az m_pNagydijak tombot
    if ( m_ucNagydSzam+1 > m_nBlokk*ND_FOGL_BLK_MER )
    {
      pIdNagyd = new  SNagydij* [(m_nBlokk+1)*ND_FOGL_BLK_MER] ;

      if ( pIdNagyd == NULL )
      {
        return -1 ;
      }

      // Mivel ez mostmar teny :
      m_nBlokk++ ;

      memcpy( pIdNagyd, m_pNagydijak, ucBeszIdx*sizeof(SNagydij*)) ;

      pIdNagyd[ucBeszIdx] = pUjNagyd ;
      //                                               -1
      memcpy( pIdNagyd+1, m_pNagydijak, (m_ucNagydSzam-ucBeszIdx)*sizeof(SNagydij*)) ;

      // Ha ures, nincs lefoglalt terulet, hibat okoz a delete
      if ( m_ucNagydSzam )
        delete [] m_pNagydijak ;

      m_ucNagydSzam++ ;

      // A megnovelt tabla cimenek atmasolasa az eredeti helyere
      m_pNagydijak = pIdNagyd ;
    }
    else // Beszuras ujrafoglalas nelkul
    {
      //        1 alapu                  0 alapu
      for ( i = m_ucNagydSzam - 1 ; i >= ucBeszIdx ; i-- )
      {
        m_pNagydijak[i+1] = m_pNagydijak[i] ;
      }

      m_pNagydijak[ucBeszIdx] = pUjNagyd ;

      m_ucNagydSzam++ ;
    }
  }

  return l_SQLCODE ;
}

long CSzezon::Modosit(unsigned uiModNdKulcs, SNagydij& sUjNagydij)
{
  long l_SQLCODE ;
  unsigned char ucNdModIdx, ucNdBeszIdx ;
  unsigned char ucFelvNdSzama, ucIdx ;

  for ( ucNdModIdx = 0 ; ucNdModIdx < m_ucNagydSzam ; ucNdModIdx++ )
  {
    if ( m_pNagydijak[ucNdModIdx]->m_uiNagyd == sUjNagydij.m_uiNagyd )
      break ;
  }

  // Van ilyen nagydij egyaltalan ?
  if ( ucNdModIdx == m_ucNagydSzam )
    return -1 ;

  // Elobb az adatbazis, mert a tobbeli modositast nehez visszacsinalni,
  // ill. tombot kene foglalni stb.
  l_SQLCODE = SQL_NagydUPDATE( uiModNdKulcs, sUjNagydij) ;
  if ( l_SQLCODE != 0 )
  {
    // Ha a szezont/nagydijat valaki idokozben torolte volna az adatbazisbol
    if ( l_SQLCODE != -1403 ) // -1403 ?
    {
      return l_SQLCODE ;
    }

    // ORA-00001: unique constraint (SCOTT.UK_NAGYDIJAK/SCOTT.PK_SZEZON) violated
    l_SQLCODE = SzezonFelvesz( ucFelvNdSzama) ;

    // -1, mert lehet, hogy nem az osszes nagydijat toroltek
    if ( l_SQLCODE != 0 && l_SQLCODE != -1 )
    {
      return l_SQLCODE ;
    }

    l_SQLCODE = SQL_NagydUPDATE( uiModNdKulcs, sUjNagydij) ;
    if ( l_SQLCODE != 0 )
    {
      return l_SQLCODE ;
    }
  }

  // A datum esetleges valtoztatasaval modosul-e a nagydij helye a szezonban
  if ( m_pNagydijak[ucNdModIdx]->m_sDatum == sUjNagydij.m_sDatum )
  {
    *m_pNagydijak[ucNdModIdx] = sUjNagydij ;
  }
  else // A datum valtozasa miatt a nagydij pozicioja megvaltozhat
  {
    // A datumvaltozas miatti uj pozicio meghatarozasa
    for ( ucNdBeszIdx = 0 ; ucNdBeszIdx < m_ucNagydSzam ; ucNdBeszIdx++ )
    {
      if ( sUjNagydij.m_sDatum < m_pNagydijak[ucNdBeszIdx]->m_sDatum )
        break ;
    }

    if ( ucNdBeszIdx == ucNdModIdx )
    {
      *m_pNagydijak[ucNdModIdx] = sUjNagydij ;
    }
    else
    {
      if ( ucNdBeszIdx > ucNdModIdx )
      {
        // A tapasztalat szerint 1-el mindig nagyobb...
        ucNdBeszIdx-- ;
        
        for ( ucIdx = ucNdModIdx ; ucIdx < ucNdBeszIdx ; ucIdx++ )
        {
          *m_pNagydijak[ucIdx] = *m_pNagydijak[ucIdx+1] ;
        }

        *m_pNagydijak[ucNdBeszIdx] = sUjNagydij ;
      }
      else
      {
        for ( ucIdx = ucNdModIdx ; ucIdx > ucNdBeszIdx ; ucIdx-- )
        {
          *m_pNagydijak[ucIdx] = *m_pNagydijak[ucIdx-1] ;
        }

        *m_pNagydijak[ucNdBeszIdx] = sUjNagydij ;
      }
    }
  }

  // Minden rendben
  return l_SQLCODE ;
}

long CSzezon::Torol(unsigned uiTorlendoNd)
{     
  unsigned char ucNdIdx ;
  long          l_SQLCODE ;

  for ( ucNdIdx = 0 ; ucNdIdx < m_ucNagydSzam ; ucNdIdx++ )
  {
    if ( m_pNagydijak[ucNdIdx]->m_uiNagyd == uiTorlendoNd )
      break ;
  }

  // Van ilyen nagydij egyaltalan ?
  if ( ucNdIdx == m_ucNagydSzam )
    return -1 ;

  l_SQLCODE = SQL_NagydDELETE( uiTorlendoNd) ;
  if ( l_SQLCODE == 0 )
  {
    delete m_pNagydijak[ucNdIdx] ;

    // Ebben a verzioban nem foglalkozok minden egyes elem utan darabra
    // pontos helyfoglalassal, ugyis blokkonkent foglal, de igy elofordulhat,
    // hogy olyan esetben is ujrafoglal felvetelnel, amikor elferne
    for ( ; ucNdIdx < m_ucNagydSzam - 1 ; ucNdIdx++ )
      m_pNagydijak[ucNdIdx] = m_pNagydijak[ucNdIdx+1] ;

    m_ucNagydSzam-- ;
  }

  return l_SQLCODE ;
}

void CSzezon::Kiurit()
{
  // Hasonloan a destruktorhoz:
  unsigned char  ucNdIdx ;

  // Felszabaditja, amire mutat
  ucNdIdx = 0 ;

  while ( ucNdIdx < m_ucNagydSzam )
  {
    delete m_pNagydijak[ucNdIdx] ;

    ucNdIdx++ ;
  }

  // Felszabaditja magat a tombot
  // Ha ures, nincs lefoglalt terulet, hibat okoz a delete
//  if ( m_ucNagydSzam != 0 )
    delete [] m_pNagydijak ;

  // Ellentetben a destruktorral, itt a szabvanyos
  // adattagokat is "nullazni" kell
  m_pNagydijak  = NULL ;
  m_uiSzezon    = 0 ;
  m_uiPontozas  = 0 ;
  m_ucEdzElsPont   = 0 ;
  m_ucLeggyKorPont = 0 ;
  m_ucNagydSzam    = 0 ;
  m_ucElsLgjBesz   = 0 ;
}

unsigned char CSzezon::NdBolIdx( unsigned uiNdKulcs )
{
  unsigned char ucNdIdx ;

  for ( ucNdIdx = 0 ; ucNdIdx < m_ucNagydSzam ; ucNdIdx++ )
  {
    if ( m_pNagydijak[ucNdIdx]->m_uiNagyd == uiNdKulcs )
      break ;
  }

  return ucNdIdx ;
}

SNagydij* CSzezon::operator [] (unsigned char index)
{
    if ( index<m_ucNagydSzam )
    {
      return m_pNagydijak[index] ;
    }

    return NULL ;    
}

// Ertekado operator
CSzezon& CSzezon::operator=(CSzezon& cJobboldal)
{
  unsigned char uc=0 ;

  if (this != &cJobboldal)
  {
    // Az osztaly torlese (dinamikus tag !)
    Kiurit() ;

    m_uiSzezon    = cJobboldal.m_uiSzezon    ;
    m_uiPontozas  = cJobboldal.m_uiPontozas  ;
    m_ucNagydSzam = cJobboldal.m_ucNagydSzam ;

    m_pNagydijak  = new SNagydij* [m_ucNagydSzam] ;

    // Lehetne itt NULL-ra vizsgalni, de mit csinalnek akkor ?

    for ( uc=0 ; uc < m_ucNagydSzam ; uc++ )
    {
      m_pNagydijak[uc] = new SNagydij ;
      *(*this)[uc] = *(cJobboldal[uc]) ;
    }
  }

  return *this ;
}

// Mivel dinamikus adatot hasznalunk :
CSzezon::~CSzezon()
{
  unsigned char ucNdIdx ;

  // Felszabaditja, amire mutat
  ucNdIdx = 0 ;

  while ( ucNdIdx < m_ucNagydSzam )
  {
    delete m_pNagydijak[ucNdIdx] ;
    ucNdIdx++ ;
  }

  // Felszabaditja magat a tombot
  // Ha ures, nincs lefoglalt terulet, hibat okoz a delete
//  if ( m_ucNagydSzam )
    delete [] m_pNagydijak ;
}

/////////////////////////////////////////////////////////////////////////////
//  diagnostics

#ifdef _DEBUG
void CSzezon::AssertValid() const
{
	CObject::AssertValid();
}

void CSzezon::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);
}

void CSzezon::CSzezonDump(CString sDumpFilenev)
{
  char buffer[20] ;
  unsigned char i ;

  CFile fNev(sDumpFilenev, CFile::modeCreate | CFile::modeWrite) ;
  CArchive ar(&fNev, CArchive::store) ;

  ar << "A szezon eve: " ;
  _ultoa( m_uiSzezon, buffer, 10 ) ;
  ar << buffer ;

  ar << "\nPontozas: " ;
  _ultoa( m_uiPontozas, buffer, 10 ) ;
  ar << buffer ;

  ar << "\nA nagydijak szama: " ;
  _ultoa( m_ucNagydSzam, buffer, 10 ) ;
  ar << buffer ;

  ar << "\nA szezon nagydijai:\n" ;

  i=0 ;
  while( i < m_ucNagydSzam )
  {
    _ultoa( i, buffer, 10 ) ;

    ar << "\nA(z) " << buffer << ". nagydij:" ;

    ar << " nev: " ;
    _ultoa( m_pNagydijak[i]->m_uiNagyd, buffer, 10 ) ;
    ar << buffer ;

    ar << " palya:" ;
    _ultoa( m_pNagydijak[i]->m_uiPalya, buffer, 10 ) ;
    ar << buffer ;

    ar << " datum:" ;
    _ultoa( m_pNagydijak[i]->m_sDatum.ev, buffer, 10 ) ;
    ar << buffer << "(ev) " ;
    _ultoa( m_pNagydijak[i]->m_sDatum.honap, buffer, 10 ) ;
    ar << buffer << "(honap) " ;
    _ultoa( m_pNagydijak[i]->m_sDatum.nap, buffer, 10 ) ;
    ar << buffer << "(nap) " ;

    i++ ;
  }

  ar.Close() ;
  fNev.Close() ;
}
#endif //_DEBUG
