//  nevzlist.cpp : a CNevezLista  osztaly implementacioja
//

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"

#include "cszezon.h"

#include "nevzlist.h"

#include "cnev.h"
#include "cvnev.h"
#include "pontozas.h"
#include "proba13.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE( CNvzSor, CObject)

/////////////////////////////////////////////////////////////////////////////
// CNvzSor

// Default konstruktor a serialize kepesseghez
CNvzSor::CNvzSor()
{
}

CNvzSor::CNvzSor( unsigned uiRajtszamIni, unsigned uiVersIni, unsigned uiNemzIni, unsigned uiCsapIni, unsigned uiMotIni)
{
  uiRajtszam  = uiRajtszamIni ;
  uiVersenyzo = uiVersIni ;
  uiNemz      = uiNemzIni ;
  uiCsapat    = uiCsapIni ;
  uiMotor     = uiMotIni  ;
}

BOOL CNvzSor::operator==(CNvzSor& Jobboldal)
{
  // #define KOZOMBOS_RSZ   255
  if ( uiRajtszam == Jobboldal.uiRajtszam || Jobboldal.uiRajtszam == KOZOMBOS_RSZ )
  {
    if (uiVersenyzo == Jobboldal.uiVersenyzo || Jobboldal.uiVersenyzo == KOZOMBOS)
    {
      if (uiNemz == Jobboldal.uiNemz || Jobboldal.uiNemz == KOZOMBOS)
      {
        if (uiCsapat == Jobboldal.uiCsapat || Jobboldal.uiCsapat == KOZOMBOS)
        {
          if (uiMotor == Jobboldal.uiMotor || Jobboldal.uiMotor == KOZOMBOS)
          {
            return TRUE ;
          }
        }
      }
    }
  }

  return FALSE ;
}

CNvzSor& CNvzSor::operator=(CNvzSor& Jobboldal)
{
  uiVersenyzo = Jobboldal.uiVersenyzo ;
  uiNemz      = Jobboldal.uiNemz      ;
  uiCsapat    = Jobboldal.uiCsapat    ;
  uiMotor     = Jobboldal.uiMotor     ;

  return *this ;
}

void CNvzSor::Trace()
{
  CProba13App* cPro13 = (CProba13App*) AfxGetApp() ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  CString cSzoveg ;

  cPro13->m_cVersenyzoNev.Lookup( uiVersenyzo, pVNevAdat) ;
  cSzoveg += pVNevAdat->m_sKerNev + " " + pVNevAdat->m_sVezNev + " " ;

  cPro13->m_cNemzetisegNev.Lookup( uiNemz, pNevAdat) ;
  cSzoveg += pNevAdat->m_sNev ;

  cPro13->m_cCsapatNev.Lookup( uiCsapat, pNevAdat) ;
  cSzoveg += pNevAdat->m_sNev ;

  cPro13->m_cMotorNev.Lookup( uiMotor, pNevAdat) ;
  cSzoveg += pNevAdat->m_sNev ;

  TRACE("\n%s", (const char*) cSzoveg) ;
}

CNvzSor::~CNvzSor()
{
}

IMPLEMENT_DYNCREATE(CNevezLista , CMapPtrToPtr)

/////////////////////////////////////////////////////////////////////////////
// CNevezLista construction/destruction

CNevezLista::CNevezLista()
{
  m_uiSzezon = 0 ;
  m_uiElsoNagydij = 0 ;
}

CNevezLista::CNevezLista(unsigned uiInitSzezon)
{
  m_uiSzezon = uiInitSzezon ;
  m_uiElsoNagydij = 0 ;
}

CNevezLista::CNevezLista( int nBlokkMeret, unsigned uiInitSzezon) : CMapPtrToPtr( nBlokkMeret) , m_cRszElsNdMap( nBlokkMeret)
{
  m_uiSzezon = uiInitSzezon ;
  m_uiElsoNagydij = 0 ;
}

unsigned short CNevezLista::Meret()
{
  POSITION poz        ;
  unsigned uiNvzlKlcs  ;
  CNvzSor* pNvzLstSor ;
  unsigned short usMeret;

  usMeret = 0 ;

  poz = GetStartPosition() ;
  while ( poz != NULL )
  {
    GetNextAssoc( poz, uiNvzlKlcs, pNvzLstSor) ;
    usMeret++ ;
  }

  return usMeret ;
}

// A const kulcsszot kivettem az argumentumlistabol , mert kulonben :
// 'GetNextAssoc' : cannot convert 'this' pointer from 'const class ::CNev __far *' to 'class ::CNev __far *const '       
CNevezLista::CNevezLista( CNevezLista& cMasol )
{    		        
    // Az egyenloseg opereator meghivasa, ha mar egyszer van ...
    *this = cMasol ;
}

CNevezLista::~CNevezLista()
{
  POSITION  poz        ;
  WORD      wRajtszam ;
  unsigned* pNvzlKlcs  ;

  // Az objektum megszunesekor kulon figyelmet kell
  // forditani a mutatok altal cimzett terulet felszabaditasara
  RemoveAll() ;

  poz = m_cRszElsNdMap.GetStartPosition() ;
  while ( poz != NULL )
  {
    m_cRszElsNdMap.GetNextAssoc( poz, wRajtszam, (void*&)pNvzlKlcs) ;

    delete pNvzlKlcs ;
  }
}   

/////////////////////////////////////////////////////////////////////////////
//  kezelo fuggvenyek

// Mivel a tarolt pointer tipusa ismert, a RemoveAll fv-tol elvarhato,
// hogy necsak a mutatot, hanem azt, amire mutat is torolje :
void CNevezLista::RemoveAll()
{
  POSITION poz ;
  unsigned uiKulcs ;
  CNvzSor* pAdat ;

  m_uiSzezon = 0 ;
  m_uiElsoNagydij = 0 ;

  poz = GetStartPosition() ;
  while( poz != NULL)
  {
    GetNextAssoc( poz, uiKulcs, pAdat ) ;
    delete pAdat ;
  }

  CMapPtrToPtr::RemoveAll() ;
}

BOOL CNevezLista::RemoveKey(unsigned uiKulcs)
{
  CNvzSor* pAdat ;

  if ( Lookup( uiKulcs, pAdat) )
    delete pAdat ;

  return CMapPtrToPtr::RemoveKey((void*)uiKulcs) ;
}

CNvzSor*& CNevezLista::operator[](unsigned uiKulcs)
{
  return (CNvzSor*&) CMapPtrToPtr::operator[]((void*)uiKulcs) ;
}

CNevezLista& CNevezLista::operator=(CNevezLista& Jobboldal)
{
  unsigned uiKulcs ;
  WORD     wRajtszam ;
  POSITION poz   ;
  CNvzSor* pAdat ;
  unsigned* pUjNvzlKulcs ;
  unsigned* pRegiNvzlKulcs ;

  // Kulonben az eredmeny a celosztaly tokeletes torlese lenne
  if ( this != &Jobboldal )
  {
    // A biztonsag kedveer' toroljuk a celosztalyt...
    RemoveAll() ;

    m_uiSzezon = Jobboldal.m_uiSzezon ;

    poz = Jobboldal.GetStartPosition() ;
    while( poz != NULL )
    {
      Jobboldal.GetNextAssoc( poz, uiKulcs, pAdat) ;
	    SetAt( uiKulcs, new CNvzSor(pAdat->uiRajtszam,
                                  pAdat->uiVersenyzo,
                                  pAdat->uiNemz,
                                  pAdat->uiCsapat,
                                  pAdat->uiMotor) ) ;
    }
  }

  poz = Jobboldal.m_cRszElsNdMap.GetStartPosition() ;
  while( poz != NULL)
  {
    Jobboldal.m_cRszElsNdMap.GetNextAssoc( poz, wRajtszam, (void*&)pRegiNvzlKulcs) ;

    pUjNvzlKulcs  = new unsigned ;
    *pUjNvzlKulcs = *pRegiNvzlKulcs ;

    m_cRszElsNdMap.SetAt( wRajtszam, pUjNvzlKulcs ) ;
  }

  return *this ;
} ;

/////////////////////////////////////////////////////////////////////////////
//  diagnostics

#ifdef _DEBUG
void CNevezLista::AssertValid() const
{
	CMapPtrToPtr::AssertValid();
}

void CNevezLista::Dump(CDumpContext& dc) const
{
	CMapPtrToPtr::Dump(dc);
}

void CNevezLista::CNevezListaDump(CString sDumpFilenev)
{
  CProba13App* pProba13App = (CProba13App*) AfxGetApp() ;
  char buffer[51] ;

  nevadat*  pNevAdat  ;
  vnevadat* pVNevAdat ;
  unsigned  uiKulcs ;
  POSITION poz   ;
  CNvzSor* pNvzAdat ;

  CFile fNev(sDumpFilenev, CFile::modeCreate | CFile::modeWrite) ;
  CArchive ar(&fNev, CArchive::store) ;

  ar << "A szezon eve: " ;
  _ultoa( m_uiSzezon, buffer, 10 );
  ar << buffer ;

  poz = GetStartPosition() ;
  while( poz != NULL)
  {
    GetNextAssoc( poz, uiKulcs, pNvzAdat ) ;
    sprintf(buffer,"\nNvzlKlcs: %d\n", uiKulcs) ;

    ar << buffer ;
/*
    pProba13App->m_cPalyaNev.Lookup(pNvzAdat->wElsoVers, pNevAdat) ;
    sprintf(buffer,"%s(%d)", (const char*)pNevAdat->sNev,
                             pNvzAdat->wElsoVers) ;
    ar << buffer ;
*/
    pProba13App->m_cVersenyzoNev.Lookup(pNvzAdat->uiVersenyzo, pVNevAdat) ;

    sprintf(buffer,"%s %s(%d) ", (const char*)pVNevAdat->m_sKerNev,
                                 (const char*)pVNevAdat->m_sVezNev,
                                 pNvzAdat->uiVersenyzo ) ;
    ar << buffer ;

    pProba13App->m_cNemzetisegNev.Lookup(pNvzAdat->uiNemz, pNevAdat) ;
    sprintf(buffer,"%s(%d)", (const char*)pNevAdat->m_sNev,
                             pNvzAdat->uiNemz) ;
    ar << buffer ;

    pProba13App->m_cCsapatNev.Lookup(pNvzAdat->uiCsapat, pNevAdat) ;
    sprintf(buffer,"%s(%d)", (const char*)pNevAdat->m_sNev,
                             pNvzAdat->uiCsapat) ;
    ar << buffer ;

    pProba13App->m_cMotorNev.Lookup(pNvzAdat->uiMotor, pNevAdat) ;
    sprintf(buffer,"%s(%d)\n", (const char*)pNevAdat->m_sNev,
                             pNvzAdat->uiMotor) ;
    ar << buffer ;
  }

  ar.Close() ;
  fNev.Close() ;
}
#endif //_DEBUG