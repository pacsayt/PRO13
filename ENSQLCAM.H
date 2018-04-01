// ensqlcam.h

// Definialok egy a szabvanyos sqlca-val azonos, de mas nevu strukturat,
// melyre, mint visszateresi ertekre kasztolom a szabvanyosat
/*
struct EnSQLCAm
*/
typedef struct
{
  char    sqlcaid[8] ; /* ub1 */ 
  long    sqlabc     ; /* b4  */
  long    sqlcode    ; /* b4  */
  struct
  {
    unsigned short sqlerrml ;    /* ub2 */
    char           sqlerrmc[70] ;/* ub1 */
  } sqlerrm ;
  char    sqlerrp[8] ; /* ub1 */
  long    sqlerrd[6] ; /* b4  */
  char    sqlwarn[8] ; /* ub1 */
  char    sqlext[8]  ; /* ub1 */
} EnSQLCAm ;

extern EnSQLCAm gl_sEnSQLCAm ;

// A fv. a CWinApp osztaly mellett globalisan definialt SQLCA valtozoba
// menti a parameterkent atadott sqlca-t, a hiba eseten vegrehajtott
// ROLLBACK WORK megsemmisito hatasat kikerulendo
extern void SQL_SQLCAFeltolt() ;

// Tomb, melyet az embedded SQL fv-t felhivo fv. hasznal
// Host valtozokent ugyanilyen adattagokkal van egy kulon tomb definialva
/*
typedef struct //sSzamok
{
  int    n_szam_tiz ;
  int    c_szam_ket_nn ;
  double d_szam_har_har;
} sSzamok ;
*/