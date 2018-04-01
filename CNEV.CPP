//  cnev.cpp : a CNev  osztaly implementacioja
//

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"
#include "cnev.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE( CNev, CObject)

/////////////////////////////////////////////////////////////////////////////
// CNev construction/destruction

CNev::CNev()
{
  m_enNevtip = eErvtlnNt ;
}

CNev::CNev( eNevtipus enIniNevtip = eErvtlnNt, int nBlokkMeret = 10 ) : CMapPtrToPtr( nBlokkMeret )
{
  m_enNevtip = enIniNevtip ;
}

// A const kulcsszot kivettem az argumentumlistabol , mert kulonben :
// 'GetNextAssoc' : cannot convert 'this' pointer from 'const class ::CNev __far *' to 'class ::CNev __far *const '       
CNev::CNev( CNev& cMasol )
{
  // Az egyenloseg opereator meghivasa, ha mar egyszer van ...
  *this = cMasol ;
}

CNev::~CNev()
{// Az objektum megszunesekor kulon figyelmet kell
 // forditani a mutatok altal cimzett terulet felszabaditasara 
  RemoveAll() ;
}   

/////////////////////////////////////////////////////////////////////////////
//  kezelo fuggvenyek

BOOL CNev::NevbolKulcs(const CString & sKeresett, unsigned& uiTalKulcs)
{
  int nEleje, nVege, nSztrHossz = sKeresett.GetLength() ;
  unsigned uiKulcs ;
  nevadat* pNevAdat ;
  const char* pszKeresett = (const char*) sKeresett ;
  POSITION poz            = GetStartPosition() ;
  CString sLokKeresett ;

  if ( nSztrHossz == 0 || poz == NULL )
    return FALSE ;

  // A keresendo sztring elejerol, vegerol leszedjuk a szokozt
  for ( nEleje=0 ; nEleje < nSztrHossz && *(pszKeresett+nEleje) == ' ' ; nEleje++ ) ;

  // Csupa szokoz
  if ( nEleje == nSztrHossz )
    return FALSE ;
                      // \0
  for ( nVege=nSztrHossz-1 ; nVege >= 0 && *(pszKeresett+nVege) == ' ' ; nVege-- ) ;

  sLokKeresett = sKeresett.Mid( nEleje, nVege - nEleje + 1) ;

  while( poz != NULL)
  {
    GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

    if ( pNevAdat->m_sNev == sLokKeresett )
    {
      uiTalKulcs = uiKulcs ;

      return TRUE ;
    }
  }

  return FALSE ;
}

// Mivel a tarolt pointer tipusa ismert, a RemoveAll fv-tol elvarhato,
// hogy necsak a mutatot, hanem azt, amire mutat is torolje :
void CNev::RemoveAll()
{
  POSITION poz   ;
  unsigned uiKulcs  ;
  nevadat* pNevAdat ;

  poz = GetStartPosition() ;
  while( poz != NULL)
  {
    GetNextAssoc( poz, uiKulcs, pNevAdat ) ;
    delete pNevAdat ;
  }

  CMapPtrToPtr::RemoveAll() ;
}

BOOL CNev::RemoveKey(unsigned uiKulcs)
{
  nevadat* pNevAdat ;

  if(Lookup(uiKulcs, pNevAdat))
  {
    delete pNevAdat ;

    return CMapPtrToPtr::RemoveKey((void*)uiKulcs) ;
  }

  return FALSE ;
}

nevadat*& CNev::operator[](unsigned uiKulcs)
{
  return (nevadat*&) CMapPtrToPtr::operator[]((void*)uiKulcs) ;
}

CNev& CNev::operator=(CNev& Jobboldal)
{
  unsigned uiKulcs ;		
  POSITION poz   ;
  nevadat* pNevAdat ;
  nevadat* pNewAdat ;

  if ( this != &Jobboldal )
  {
    // A biztonsag kedveer' toroljuk a celosztalyt...
    RemoveAll() ;

    poz = Jobboldal.GetStartPosition() ;
    while( poz != NULL)
    {
      Jobboldal.GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

      if ( (pNewAdat = new nevadat(pNevAdat->m_sNev)) == NULL )
        return *this ;

      SetAt(uiKulcs, pNewAdat ) ;
    }
  }

  return *this ;
} ;

/////////////////////////////////////////////////////////////////////////////
//  serialization

/////////////////////////////////////////////////////////////////////////////
//  diagnostics

#ifdef _DEBUG
void CNev::AssertValid() const
{
	CMapPtrToPtr::AssertValid() ;
}

void CNev::Dump(CDumpContext& dc) const
{
	CMapPtrToPtr::Dump(dc) ;
}

void CNev::CNevDump(CString sDumpFilenev)
{
  char buffer[200] ;

  CFile fNev(sDumpFilenev, CFile::modeCreate | CFile::modeWrite) ;
  CArchive ar(&fNev, CArchive::store) ;

  int meret ;
  unsigned uiKulcs ;

  CMapPtrToPtr::Serialize(ar) ;

  if (ar.IsStoring())
  {
    meret = GetCount() ;
    sprintf( buffer, "Meret: %d", meret) ;
    ar <<  buffer ;

    nevadat* pNevAdat ;
    POSITION poz ;

    poz = GetStartPosition() ;
    while( poz != NULL)
    {
      GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

      sprintf( buffer, "\nKulcs: %d", uiKulcs) ;
      ar << buffer ;

      ar << "\tNev:" ;
      ar << pNevAdat->m_sNev ;
    }
  }

  ar.Close() ;
  fNev.Close() ;
}
#endif //_DEBUG