// vbszerep.h

class CPillAll ;

// A VB szereplesek lekerdezes eredmenyeit tartalmazo,(es megjelenito)
// osztaly
class CVBSzerepl : public CBazKerEredm
{
    DECLARE_SERIAL(CVBSzerepl)

public :
    // Default konstruktor a serialize kepesseghez
    CVBSzerepl() ;

public:
    CNevEredmMezo m_cVersMez   ;
    CNevEredmMezo m_cNemzMez   ;
    CNevEredmMezo m_cCsapMez   ; 
    CNevEredmMezo m_cMotorMez  ;
    // Ami miatt az egesz van, azok a VB 1. 2. 3. ... helyek !
    CHelyezesMezo m_cHelyezMez ; // a helyezes fontos itt nem annyira a pontozas
    CDatEredmMezo m_cDatumMez  ;

public :
    BOOL operator < (CVBSzerepl& cJobboldal) ;
    // A CVBSzerepl tomb qsort-tal valo rendezesehez kell.
    // Az OsszpntKisz fv elozetes felhivasat feltetelezi !
    friend int VBOsszehas(const void* pCVBSzerepl1, const void* pCVBSzerepl2) ;

    void Feltolt( CPillAll* pCPillAll, unsigned char ucHelyezes, eNevtipus enNevtipus) ;
    // Feltoltes az SQL_PillAll adataival
    void Feltolt( unsigned uiVers , unsigned uiNemz, unsigned uiCsap, unsigned uiMotor,
                  unsigned short usEv, 
                  unsigned short usPontszam, unsigned char ucHelyezes,
                  eNevtipus enNevtipus) ;

    void UjFelvesz(CVBSzerepl* pUjElem) ;
    void Kirajzol(CDC* pDC, int& nKezdY) ;
    void ScrollMeret(int& x, int& y) ;
    BOOL EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol) ;
    virtual int AzMezok(char cAktSzint) ;
    void Serialize(CArchive& ar) ;
    void Trace() ;

public :
    ~CVBSzerepl() ;
} ;