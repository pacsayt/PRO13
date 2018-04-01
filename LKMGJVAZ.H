// lkmgjvaz.h - az adatszerkezet, melyre a lekerdezesi eredmenyek lesznek 
//              felaggatva, ez az adatszerkezet kezeli egyben oket pl. a
//              kepernyore valo rajzolaskor

class CLekErMegjVaz : public CObject
{
    DECLARE_SERIAL(CLekErMegjVaz)
public :
    // Default konstruktor a serialize kepesseghez
    CLekErMegjVaz() ;
    
// protected :
public :
    // Az osztaly altal tartalmazott lekerdezesi eredmenyek erre a mutatora 
    // vannak felfuzve
    CBazKerEredm* m_pKezdopont ;
public :
    // Kezelofuggvenyek
    void UjFelvesz(CFgyEeLgykOe* pUjElem) ;
    void UjFelvesz(CHolVKiV* pUjElem) ;
    void UjFelvesz(CPillAll* pUjElem) ;
    void UjFelvesz(CVBSzerepl* pUjElem) ;
    void Kirajzol(CDC* pDC,  int& nKezdY) ;

    // A scrollmeret beallitasahoz kell egy fv., mely CDC nelkul eredmenyt ad
    void ScrollMeret(int& x, int& y) ;
    void Serialize(CArchive& ar) ;

public :
    // Feladata a tarolt eredmenyek altal foglalt hely felszabaditasa
    ~CLekErMegjVaz() ;
} ;