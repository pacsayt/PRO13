// holvkiv.h

// A Hol versenyzett, ki versenyzett vele vonatkozo lekerdesek eredmenyet
// tartalmazo osztaly
class CHolVKiV : public CBazKerEredm
{
    DECLARE_SERIAL(CHolVKiV)

public :
    // Default konstruktor a serialize kepesseghez
    CHolVKiV() ;

//protected:
public:
    CNevEredmMezo   m_cVersMez ;
    CNevEredmMezo   m_cNemzMez ;
    CNevEredmMezo   m_cCsapMez ;
    CNevEredmMezo   m_cMotorMez;
    CDatEredmMezo   m_cDatumMez;
    CNevEredmMezo   m_cPalyaMez;

public:
    void Feltolt(CNvzSor* pNvzLstSor,
                 SNagydij* pNagydij, eNevtipus enNevtip) ;
    void Feltolt(sHolVKiVLekErTip* pHolVKiVLekEr, eNevtipus enNevtip) ;

    void UjFelvesz(CHolVKiV* pUjElem) ;
    void Kirajzol(CDC* pDC, int& nKezdY) ;
    void ScrollMeret(int& x, int& y) ;
    //vo.: CFgyEeLgykOe
    BOOL EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol) ;
    virtual int AzMezok(char cAktSzint) ;
    void Serialize(CArchive& ar) ;

    ~CHolVKiV() ;
} ;
