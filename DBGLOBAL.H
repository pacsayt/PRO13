// dbglobal.h

// Az SQL_FelvSzez fv. mekkora leptekben foglaljon helyet
#define FLV_SZEZ_BLK 20

char* gl_pLek1Tmb[] ;
char* gl_pLek2Tmb[] ;

long SQL_COMMIT() ;

long SQL_ROLLBACK() ;

long SQL_FelvSzez( unsigned** ppSzezTomb, unsigned* pSzezdb) ;

/* Mely nevezesi lista bejegyzes fordult elo az adott szezon */
/* adott nagydijan (Nem mint nevezesi lista bejegyzes )      */
long SQL_KulNvzlNdon( unsigned uiSzezEve, unsigned uiNdKulcs,
                      unsigned** ppNvzlKulcsTomb, unsigned* pNvzldb) ;

/* Az Osszes eredmeny lekerdezest a SQL_LgGyKorAltLekerd, SQL_RsBefAltLekerd */
/* tobbszori felhivasaval erem el                                            */

/* Leggyorsabb korok                           */
/* Versenyzok, nemzetisegek, csapatok, motorok */
long SQL_LeggyKor( eLekerdTipus enLekTip, eNevtipus enNevTip,
                           unsigned nSzezEve, unsigned* pNevKlcsTmb, unsigned nBeTmbMeret,
                           sLgGyKorLekErTip** ppLgGyKorLekEr, unsigned* pKiTmbMeret) ;

/* Futamgyozelmek, Edzeselsosegek, Osszes eredmeny */
/* Versenyzok, nemzetisegek, csapatok, motorok     */
long SQL_RajtsBefuto( eLekerdTipus     enLekTip,     eNevtipus enNevtip,
                      unsigned         nSzezEve,     unsigned* pNevKlcsTmb, unsigned nBeTmbMeret,
                      sRsBefLekErTip** ppRsBefLekEr, unsigned* pKiTmbMeret) ;

long SQL_HolVersKiVers( eNevtipus          enNevTip,
                        unsigned           nSzezEve,       unsigned* pNevKlcsTmb, unsigned nBemTmbMeret,
                        sHolVKiVLekErTip** ppHolVKiVLekEr, unsigned* pKiTmbMeret) ;

long SQL_EdzEls(unsigned uiSzezonEve) ;

/*, unsigned* pKiTmbMeret*/
long SQL_PillAll( unsigned          nSzezon,       eNevtipus enNevTip,
                  unsigned*         pNevKlcsTmb,   unsigned  nBeKiTmbMeret,
                  sPillAllLekErTip* pPillAllLekEr) ;

/* Az SQL_PillAll-ban kell a nevkulcsok sorbarendezesehez */
int KulcsOsszeh( const void* pKulcs1, const void* pKulcs2) ;