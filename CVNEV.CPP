//  cvnev.cpp : a CVNev  osztaly implementacioja
//

#include "stdafx.h"

#include "resource.h"
#include "alaptip.h"

#include "cvnev.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE( CVNev, CMapPtrToPtr)

/////////////////////////////////////////////////////////////////////////////
// CVNev construction/destruction

CVNev::CVNev() 
{
}

CVNev::CVNev( int nBlokkMeret ) : CMapPtrToPtr( nBlokkMeret )    
{
}

// A const kulcsszot kivettem az argumentumlistabol , mert kulonben :
// 'GetNextAssoc' : cannot convert 'this' pointer from 'const class ::CVNev __far *' to 'class ::CVNev __far *const '       
CVNev::CVNev( CVNev& cMasol)
{
  // Az egyenloseg opereator meghivasa, ha mar egyszer van ...
  *this = cMasol ;    
}

CVNev::~CVNev()
{// Az objektum megszunesekor kulon figyelmet kell
 // forditani a mutatok altal cimzett terulet felszabaditasara 
  RemoveAll() ;
}   

/////////////////////////////////////////////////////////////////////////////
//  kezelo fuggvenyek

BOOL CVNev::NevbolKulcs(const CString& sKeresett, unsigned& uiTalKulcs)
{
  unsigned  uiKulcs ;
  vnevadat* pVNevAdat ;
  POSITION  poz = GetStartPosition() ;

  if ( sKeresett.GetLength() == 0 || poz == NULL )
    return FALSE ;

  while( poz != NULL)
  {
    GetNextAssoc( poz, uiKulcs, pVNevAdat ) ;

    if ( sKeresett.Find( pVNevAdat->m_sVezNev) != -1 &&
         sKeresett.Find( pVNevAdat->m_sKerNev) != -1 )
    {
      uiTalKulcs = uiKulcs ;

      return TRUE ;
    }
  }

  return FALSE ;
}

// Mivel a tarolt pointer tipusa ismert, a RemoveAll fv-tol elvarhato,
// hogy necsak a mutatot, hanem azt, amire mutat is torolje :
void CVNev::RemoveAll()
{
  unsigned  uiKulcs ;
  vnevadat* pVNevAdat  ;
  POSITION  poz   ;

  poz = GetStartPosition() ;
  while( poz != NULL)
  {
    GetNextAssoc( poz, uiKulcs, pVNevAdat ) ;
    delete pVNevAdat ;
  }

  CMapPtrToPtr::RemoveAll() ;
}

BOOL CVNev::RemoveKey(unsigned uiKulcs)
{
  vnevadat* pVNevAdat ;

  if( Lookup(uiKulcs, pVNevAdat) )
    delete pVNevAdat ;

  return CMapPtrToPtr::RemoveKey((void*)uiKulcs) ;
}

vnevadat*& CVNev::operator[](unsigned uiKulcs)
{
  return (vnevadat*&) CMapPtrToPtr::operator[]((void*)uiKulcs) ;
}

CVNev& CVNev::operator=(CVNev& cJobboldal)
{
  unsigned  uiKulcs ;
	POSITION  poz     ;
  vnevadat* pVNevAdat ;
  vnevadat* pVNewAdat ;

  // A biztonsag kedveeer toroljuk a celosztalyt...
  RemoveAll() ;

  poz = cJobboldal.GetStartPosition() ;
  while( poz != NULL)
  {
    cJobboldal.GetNextAssoc( poz, uiKulcs, pVNevAdat ) ;

    pVNewAdat = new vnevadat(pVNevAdat->m_sVezNev, pVNevAdat->m_sKerNev) ;

    if ( pVNewAdat == NULL )
      return *this ;

    SetAt( uiKulcs, pVNewAdat ) ;
  }

  return *this ;
}

/////////////////////////////////////////////////////////////////////////////
//  diagnostics

#ifdef _DEBUG
void CVNev::AssertValid() const
{
	CMapPtrToPtr::AssertValid() ;
}

void CVNev::Dump(CDumpContext& dc) const
{
	CMapPtrToPtr::Dump(dc) ;
}

void CVNev::CVNevDump(CString sDumpFilenev)
{
  char buffer[200] ;

  CFile fNev(sDumpFilenev, CFile::modeCreate | CFile::modeWrite) ;
  CArchive ar(&fNev, CArchive::store) ;

  WORD meret ;
  unsigned uiKulcs ;
	CMapPtrToPtr::Serialize(ar) ;

	if (ar.IsStoring())
	{
	  meret = GetCount() ;
    sprintf( buffer, "Meret: %d", meret) ;
	  ar <<  buffer ;

	  POSITION poz ;
    vnevadat* pVNevAdat ;

    poz = GetStartPosition() ;
    while( poz != NULL)
    {
      GetNextAssoc( poz, uiKulcs, pVNevAdat ) ;

      sprintf( buffer, "\nKulcs: %d", uiKulcs) ;
      ar << buffer ;

      ar << "\tVezNev:" ;
      ar << pVNevAdat->m_sVezNev ;
      ar << "\tKerNev:" ;
      ar << pVNevAdat->m_sKerNev ;
    }
  }

  ar.Close() ;
  fNev.Close() ;
}
#endif //_DEBUG