// nvzlistf.h : header file
//

// AktNvzLstKiir fuggvenyben kell a rajtszamok felso korlatjanak 
#define MAX_RAJTSZAM 255

// A nevezesi lista elemeket tartalmazo CListBox-ba valo irashoz
// rendezett lista keszitesehez
struct sRszNvzlBej
{
  unsigned uiRajtszam  ;
  unsigned uiNvzlKulcs ;
} ;


/////////////////////////////////////////////////////////////////////////////
// CNvzListFelv dialog

class CNvzListFelv : public CDialog
{
// Construction
public:
  // standard constructor
  CNvzListFelv(CWnd* pParent = NULL) ; // CNevezLista* pNvzLst,

// A dialogus ablak adattagjai
private:
  // LOKALIS ADATOK : (
  // Az ablakban eppen szerkesztes alatt allo (aktualis) nevezesi lista tartalma
  CNevezLista m_cAktNvzLst ;

  // Az aktualis szezon, mivel a nagydijak kellenek, ill. hogy szerepel-e felvett
  // r/b 1..254 vagy 255 - szerepel nevezesi listaban
  CSzezon m_cAktSzez ;
  // Az aktualis nevezesi lista mentettsegi allapota
  BOOL m_bAktNvzLstMentve ;

  // GLOBALIS ADATOK : ( pProba13App-bol atveve )

  // A felvitt nagydijneveket tartalmazo lista
  CNev*      m_pNagyd ;
  // A felvitt versenyzo neveket tartalmazo lista
  CVNev*     m_pVersenyzo ;
  // A felvitt nemzetiseg neveket tartalmazo lista
  CNev*      m_pNemz ;
  // A felvitt csapat neveket tartalmazo lista
  CNev*      m_pCsapat ;
  // A felvitt motor neveket tartalmazo lista
  CNev*      m_pMotor ;

 	// A futamokat tartalmazo lista ablak tabstopjai
 	int m_nNvzLstTbStp[5] ;

	//{{AFX_DATA(CNvzListFelv)
	enum { IDD = IDD_NVZ_LIST_SZRK } ;
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Implementation
protected:
  // A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
  // DoDataExchange fuggvenybol
  virtual BOOL OnInitDialog() ;

	// Generated message map functions
	//{{AFX_MSG(CNvzListFelv)
	afx_msg void OnSelchgSzez();
	afx_msg void OnDblclkNvzLst();
	afx_msg void OnFelvesz();
	afx_msg void OnModosit();
	afx_msg void OnTorol();
	afx_msg void OnNvzListFelv();
	afx_msg void OnNvzListTorol();
	virtual void OnOK();
	virtual void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	
  // Az aktualis (a m_cAktNvzLst) nevezesi lista mentese, a versenyzo nev,
  // a nemzetiseg-, a csapat-, a motor nev elofordulasanak frissitese
//    void AktNvzLstMent() ;
    
    // A dialogus ablak osztaly szezon adattagjanak betoltese
//    void SzezBetolt(const CString& sBeoSzez) ;    
    // A dialogus ablak osztaly szezon adattagjanak mentese
//    void AktSzezMent() ;
  // 
	BOOL NvzLstSorBeo( CNvzSor* pBeoNvzSor/*, unsigned* pElsoVers*/) ;

	// A dialogus ablak osztaly nevezesi lista adattagjanak rendezett kiirasa a Lb-ba
	void AktNvzLstKiir() ;

  // Az aktualis SQL-hibat kiirja egy MessageBox-ba, ha nRC!=-1
  void HibaKiir(long nRC) ;

  // A fuggveny a qsort szamara hasonlit ossze ket sRszNvzlBej elemet
  friend int NvzlOsszehas(const void* psRszNvzlBej1, const void* psRszNvzlBej2) ;
 
} ;