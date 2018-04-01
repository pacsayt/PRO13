// pontozas.cpp - A pontozasokat tartalmazo tomb

#include "stdafx.h"

#include <memory.h>
#include "pontozas.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif



IMPLEMENT_DYNCREATE( CPontozas, CMapPtrToPtr)

CPontozas::CPontozas()
{
}

CPontozas::CPontozas( int nBlokkMeret = 10 ) : CMapPtrToPtr( nBlokkMeret)
{
}

// Mivel a tarolt pointer tipusa ismert, a RemoveAll fv-tol elvarhato,
// hogy necsak a mutatot, hanem azt, amire mutat is torolje :
void CPontozas::RemoveAll()
{
  POSITION poz   ;
  unsigned uiKulcs  ;
  pontadat* pPontAdat ;
    
  poz = GetStartPosition() ;
  while( poz != NULL)
  {
    GetNextAssoc( poz, uiKulcs, pPontAdat ) ;
    delete pPontAdat ;
  }

  CMapPtrToPtr::RemoveAll() ;
}

BOOL CPontozas::RemoveKey(unsigned uiKulcs)
{
  pontadat* pPontAdat ;

  if( Lookup(uiKulcs, pPontAdat) )
  {
    delete pPontAdat ;

    return CMapPtrToPtr::RemoveKey((void*)uiKulcs) ;
  }

  return FALSE ;
}

pontadat*& CPontozas::operator[](unsigned uiKulcs)
{
  return (pontadat*&) CMapPtrToPtr::operator[]((void*)uiKulcs) ;
}

CPontozas& CPontozas::operator=(CPontozas& Jobboldal)
{
  unsigned uiKulcs ;		
  POSITION poz   ;
  pontadat* pPontAdat ;
  pontadat* pUjPontAdat ;

  if ( this != &Jobboldal )
  {
    // A biztonsag kedveer' toroljuk a celosztalyt...
    RemoveAll() ;

    poz = Jobboldal.GetStartPosition() ;
    while( poz != NULL)
    {
      Jobboldal.GetNextAssoc( poz, uiKulcs, pPontAdat) ;

      if ( (pUjPontAdat = new pontadat( *pPontAdat)) == NULL )
        return *this ;

      SetAt( uiKulcs, pUjPontAdat) ;
    }
  }

  return *this ;
} ;

CPontozas::~CPontozas()
{// Az objektum megszunesekor kulon figyelmet kell
 // forditani a mutatok altal cimzett terulet felszabaditasara 
  RemoveAll() ;
}   

