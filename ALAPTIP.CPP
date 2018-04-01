// alaptip.cpp - altalanosan hasznalt adattipusok : 
//

#include "stdafx.h"

#include "resource.h"

#include "alaptip.h"

#include "pontozas.h"
#include "cnev.h"      // A neveket es kulcsokat osszerendelo adatstruktura
#include "cvnev.h"     // A versenyzo neveket es kulcsokat osszerendelo adatstruktura

#include "proba13.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

///////////////////////////////
// G L O B A L I S  A D A T O K
///////////////////////////////
// Deklaracio az alaptip.h-ban !

char* gl_pNevTblTmb[] = {
                          {"vers_nev"}   ,
                          {"nemz_nev"}   ,
                          {"csapat_nev"} ,
                          {"motor_nev"}  ,
                          {"nagydij_nev"},
                          {"palya_nev"}  ,
                          {"pontozas"}   ,
                          {"ervenytelen"}
                        } ;

char* gl_pNevMezTmb[] = {
                          {"vez_nev, ker_nev"}   ,
                          {"nmz_nev"}            ,
                          {"csap_nev"}           ,
                          {"mot_nev"}            ,
                          {"nd_nev"}             ,
                          {"paly_nev"}           ,
                          {"helyezes, pontszam"} ,
                          {"ervenytelen"}
                        } ;

char* gl_pSeqTmb[]    = {
                          {"seq_vnev"}   ,
                          {"seq_nnev"}   ,
                          {"seq_csnev"}  ,
                          {"seq_mnev"}   ,
                          {"seq_ndnev"}  ,
                          {"seq_pnev"}   ,
                          {"seq_pont"}   ,
                          {"ervenytelen"}
                        } ;

BOOL gl_bKezKurzorBe = FALSE ;

int gl_dX, gl_dY ;

// Nullazva a glTabsopTorol-ben
int gl_nTabstop[ePont+1] ;

BOOL gl_bFelvitelRsBefAktiv = TRUE ;

BOOL gl_bCsatlakozva = FALSE ;

void sPillAllLekErTip::Trace()
{
  unsigned uiIdx ;

  TRACE("\n uiSzezErTmbMeret : %d\n", uiSzezErTmbMeret) ;

  for ( uiIdx = 0 ; uiIdx < uiSzezErTmbMeret ; uiIdx++ )
  {
    TRACE("\n%d pNevKlcsTmb:%d, pNevHlyzTmb:%d, pSzezPntTmb:%d",
          uiIdx,
          pNevKlcsTmb[uiIdx],
          pNevHlyzTmb[uiIdx],
          pSzezPntTmb[uiIdx]) ;
  }

  TRACE("\n uiOsszErTmbMer : %d\n", uiOsszErTmbMer) ;

  for ( uiIdx = 0 ; uiIdx < uiOsszErTmbMer ; uiIdx++ )
  {
    TRACE("\n %d pNevEredmTmb:%d pPontTmb:%d, pHelyezesTmb:%d, pNvzlKulcsTmb:%d, pNdNevTmb:%d",
          uiIdx,
          pNevEredmTmb[uiIdx],
          pPontTmb[uiIdx],
          pHelyezesTmb[uiIdx],
          pNvzlKulcsTmb[uiIdx],
          pNdNevTmb[uiIdx]) ;
  }
}

inline void TabsopTorol()
{
  memset( ::gl_nTabstop, 0, sizeof(int)*(::ePont+1)) ;
}

void TabstopBeall( CDC* pDC, eTabstopTip eNevtipus)
{
  CProba13App* pProba13App = (CProba13App*)AfxGetApp() ;
  CString sOsszVNev ;
  CSize cMeret ;
  unsigned uiKulcs ;		
  POSITION poz ;
  vnevadat* pVNevAdat ;
  nevadat*  pNevAdat  ;
  int nTab ;
  CNev* pCNev ;

  if ( pDC == NULL )
    return ;

  switch ( eNevtipus )
  {
    case eVers :
      poz = pProba13App->m_cVersenyzoNev.GetStartPosition() ;
      while( poz != NULL)
      {
        pProba13App->m_cVersenyzoNev.GetNextAssoc( poz, uiKulcs, pVNevAdat ) ;

        sOsszVNev = pVNevAdat->m_sVezNev+pVNevAdat->m_sKerNev+" " ;
        nTab = pDC->GetTextExtent( sOsszVNev, sOsszVNev.GetLength()).cx;
        if ( nTab > gl_nTabstop[ eVers] )
          gl_nTabstop[ eVers] = nTab ;
      }

      return ;

    case eNemz :
      pCNev = &(pProba13App->m_cNemzetisegNev) ;
      break ;

    case eCsap :
      pCNev = &(pProba13App->m_cCsapatNev) ;
      break ;

    case eMotor:
      pCNev = &(pProba13App->m_cMotorNev) ;
      break ;

    case ePalya:
      pCNev = &(pProba13App->m_cPalyaNev) ;
      break ;

    case eDatum:
      cMeret = pDC->GetTextExtent( "1000 00 00", 10) ;
      ::gl_nTabstop[eDatum] = cMeret.cx ;
      return ;

    case eHelyez   :
      cMeret = pDC->GetTextExtent( "88", 2) ;
      ::gl_nTabstop[eHelyez] = cMeret.cx ;
      return ;

    case ePAHelyez :
      cMeret = pDC->GetTextExtent( " 88. helyezes : 88", 16) ;

    case eIdoeredm :
      cMeret = pDC->GetTextExtent( "2:59'69.999", 11) ;
      ::gl_nTabstop[eIdoeredm] = cMeret.cx ;
      return ;

    case ePont     :
      cMeret = pDC->GetTextExtent( "888", 3) ;
      ::gl_nTabstop[ePont] = cMeret.cx ;
      return ;
  }  

  poz = pCNev->GetStartPosition() ;
  while( poz != NULL)
  {
    pCNev->GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

    nTab = pDC->GetTextExtent( pNevAdat->m_sNev, pNevAdat->m_sNev.GetLength()).cx ;
    if ( nTab > gl_nTabstop[ eNevtipus] )
      gl_nTabstop[ eNevtipus] = nTab ;
  }
}

void OsszTabstopBeall( CDC* pDC)
{
  CProba13App* pProba13App = (CProba13App*)AfxGetApp() ;
  CString sOsszVNev ;
  CSize cMeret ;
  unsigned uiKulcs  ;
  POSITION poz ;
  vnevadat* pVNevAdat ;
  nevadat*  pNevAdat  ;
  int nTab, nSpace ;

  if ( pDC == NULL )
    return ;

  // Mindenutt hozza van adva egy elvalaszto space-nyi hely
  nSpace = pDC->GetTextExtent( " ", 1).cx;

  poz = pProba13App->m_cVersenyzoNev.GetStartPosition() ;
  while( poz != NULL)
  {
    pProba13App->m_cVersenyzoNev.GetNextAssoc( poz, uiKulcs, pVNevAdat ) ;

    sOsszVNev = pVNevAdat->m_sVezNev+pVNevAdat->m_sKerNev ;
    nTab = pDC->GetTextExtent( sOsszVNev, sOsszVNev.GetLength()).cx + 2*nSpace ;
    if ( nTab > gl_nTabstop[ eVers] )
      gl_nTabstop[ eVers] = nTab ;
  }

  poz = pProba13App->m_cNemzetisegNev.GetStartPosition() ;
  while( poz != NULL)
  {
    pProba13App->m_cNemzetisegNev.GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

    nTab = pDC->GetTextExtent( pNevAdat->m_sNev, pNevAdat->m_sNev.GetLength()).cx;
    nTab += nSpace ;

    if ( nTab > gl_nTabstop[ eNemz] )
      gl_nTabstop[ eNemz] = nTab ;
  }

  poz = pProba13App->m_cCsapatNev.GetStartPosition() ;
  while( poz != NULL)
  {
    pProba13App->m_cCsapatNev.GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

    nTab = pDC->GetTextExtent( pNevAdat->m_sNev, pNevAdat->m_sNev.GetLength()).cx;
    nTab += nSpace ;

    if ( nTab > gl_nTabstop[ eCsap] )
      gl_nTabstop[ eCsap] = nTab ;
  }

  poz = pProba13App->m_cMotorNev.GetStartPosition() ;
  while( poz != NULL)
  {
    pProba13App->m_cMotorNev.GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

    nTab = pDC->GetTextExtent( pNevAdat->m_sNev, pNevAdat->m_sNev.GetLength()+1).cx;
    nTab += nSpace ;

    if ( nTab > gl_nTabstop[ eMotor] )
      gl_nTabstop[ eMotor] = nTab ;
  }

  poz = pProba13App->m_cPalyaNev.GetStartPosition() ;
  while( poz != NULL)
  {
    pProba13App->m_cPalyaNev.GetNextAssoc( poz, uiKulcs, pNevAdat ) ;

    nTab = pDC->GetTextExtent( pNevAdat->m_sNev, pNevAdat->m_sNev.GetLength()+1).cx;
    nTab += nSpace ;

    if ( nTab > gl_nTabstop[ ePalya] )
      gl_nTabstop[ ePalya] = nTab ;
  }

  cMeret = pDC->GetTextExtent( "1000 00 00", 10) ;
  gl_nTabstop[eDatum] = cMeret.cx + nSpace ;

  cMeret = pDC->GetTextExtent( "88", 2) ;
  gl_nTabstop[eHelyez] = cMeret.cx + nSpace ;

  cMeret = pDC->GetTextExtent( " 88. helyezes : 88", 16) ;
  gl_nTabstop[ePAHelyez] = cMeret.cx + nSpace ;

  cMeret = pDC->GetTextExtent( "2:59'69.999", 11) ;
  gl_nTabstop[eIdoeredm] = cMeret.cx + nSpace ;

  cMeret = pDC->GetTextExtent( "888", 3) ;
  gl_nTabstop[ePont] = cMeret.cx + nSpace ;
}

// SDatum - az egyes nagydijak es nevek elofordulasi datumat tartalmazo struktura

BOOL SDatum::operator  < (SDatum& Jobboldal)
{
    if ( ev < Jobboldal.ev )
    {
      return TRUE ;
    }

    // Az evszam nem kisebb; vagy egyenlo vagy nagyobb
    if ( ev == Jobboldal.ev )
    {// Ebben az esetben a honapok jutnak szohoz ...
      if ( honap < Jobboldal.honap )
      {
        return TRUE ;
      }

      // Az honap nem kisebb; vagy egyenlo vagy nagyobb
      if ( honap == Jobboldal.honap )
      {// Ebben az esetben a napok jutnak szohoz ...

        if ( nap < Jobboldal.nap )
        {
          return TRUE ;
        }

        return FALSE ;
      }

      // Az honap nagyobb : hamis visszateresi ertek
      return FALSE ;
    }

    // Az evszam nagyobb : hamis visszateresi ertek
    return FALSE ;

}

BOOL SDatum::operator  > (SDatum& Jobboldal)
{
    if ( ev > Jobboldal.ev )
    {
      return TRUE ;
    }

    // Az evszam nem nagyobb; vagy egyenlo vagy kisebb
    if ( ev == Jobboldal.ev )
    {// Ebben az esetben a honapok jutnak szohoz ...
      if ( honap > Jobboldal.honap )
      {
        return TRUE ;
      }

      // Az honap nem nagyobb; vagy egyenlo vagy kisebb
      if ( honap == Jobboldal.honap )
      {// Ebben az esetben a napok jutnak szohoz ...

        if ( nap > Jobboldal.nap )
        {
          return TRUE ;
        }

        return FALSE ;
      }

      // Az honap kisebb : hamis visszateresi ertek
      return FALSE ;
    }

    // Az evszam kisebb : hamis visszateresi ertek
    return FALSE ;

}

BOOL SDatum::operator == (SDatum& Jobboldal)
{
    if ( ev==Jobboldal.ev && honap==Jobboldal.honap && nap==Jobboldal.nap)
      return TRUE ;

    return FALSE ;
}

BOOL SDatum::operator >= (SDatum& Jobboldal)
{
    if ( *this > Jobboldal || *this == Jobboldal )
      return TRUE ;
    
    return FALSE ;  
}

BOOL SDatum::operator <= (SDatum& Jobboldal)
{
    if ( *this < Jobboldal || *this == Jobboldal )
      return TRUE ;

    return FALSE ;
}
