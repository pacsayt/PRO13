// fgyeelgy.h

// A Futamgyozelmekre, edzeselsosegekre, leggyorsab korokre, osszes eredmenyekre
// vonatkozo lekerdeseket tartalmazo osztaly
class CFgyEeLgykOe : public CBazKerEredm
{
    DECLARE_SERIAL(CFgyEeLgykOe)

public :
    // Default konstruktor a serialize kepesseghez
    CFgyEeLgykOe() ;

//protected:
public:
    CNevEredmMezo   m_cVersMez ;
    CNevEredmMezo   m_cNemzMez ;
    CNevEredmMezo   m_cCsapMez ; 
    CNevEredmMezo   m_cMotorMez;
    // m_uiKulcs1 - nagydij nev ; m_uiKulcs2 - palya nev (jelenleg nem kap erteket)
    CNevEredmMezo   m_cNagydMez;
    CBazEredmMezo   m_cPontMez ;
    CDatEredmMezo   m_cDatumMez;
    CIdoerEredmMezo m_cIdoeredmMez ; // m_cHelyezMez ; ???
    // az ossz eredmeny lekerdezesenel fontos a helyezes is !
//    CHelyezesMezo   m_cHelyezMez ;
    // Atlagsebesseg ?!? leggyorsabb koroket visszaado lekerdezeseknel
public:
    void Feltolt(CNvzSor* pNvzLstSor, CObject* pRBBejegyzes,
                 SNagydij* pNagydij, eNevtipus enNevtipus) ;
    void Feltolt(sLgGyKorLekErTip* pLgGyKorLekEr, eNevtipus enNevtip) ;
    void Feltolt(sRsBefLekErTip*   pRsBefLekEr,   eNevtipus enNevtip) ;

    void UjFelvesz(CFgyEeLgykOe* pUjElem) ;
    // Kirajzolja az aktualis elemet a megadott CDC-re a megadott
    // y koordinatatol. Az x-koordinatat az AktSzint*dx keplet adja.
    void Kirajzol(CDC* pDC, int& nKezdY) ;
    void ScrollMeret(int& x, int& y) ;
    // A fv. az teszteli, hogy az eger valamely ki/be kapcsolhato mezo folott
    // van-e, es ennek megfeleloen kapcsolja ki/be a mezo ala rendelteket
    // A FolotteVan es KiBeKapcsol fv-ek egybevonasa
    // talalat eseten
    // FALSE: a kurzor allapotat jelzo gl_bKezKurzorBe valtozot allitja
    // TRUE : ezen felul az alarendelt szinteket kapcsolja
    BOOL EgerMozog(CPoint cEgerPoz, char cAktSzint, BOOL bKapcsol) ;
    virtual int AzMezok(char cAktSzint) ;
    void Serialize(CArchive& ar) ;

    ~CFgyEeLgykOe() ;
} ;