// rsbeffel.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CRsBefFelv dialog

// Felhasznalja meg a CLekerdDlg osztaly is (lekerddl.~).
// enum eAblTipRsBef { RAJTS=IDD_RAJTS_FELV, BEFUTO=IDD_BEF_FELV } ;

#define KEZDOSOR 2

// A mainfraim.cpp-ben levo keresztbedefinialas feloldasara
class CRajtsBef ;

class CRsBefFelv : public CDialog
{
// Construction
public:
	CRsBefFelv( unsigned uiSzezEve, unsigned uiNdKulcs,
	            eAblTipRsBef aRajtsBef, CWnd* pParent = NULL) ;

  // Ennek a fv.-nek a hatasara kerul fel az ablak a kepernyore, es hivodik
  // meg az OnInitDialog.
  Create(UINT nIDTemplate, CWnd* pParentWnd = NULL) ;

// A dialogus ablak adattagjai
private:
  // LOKALIS ADATOK :
  // Rajtsorrendet vagy befutot szerkesztunk
  eAblTipRsBef m_enRajtsBef ;

  // Az ablakban eppen szerkesztes alatt allo (aktualis) R/B adatstruktura
  CRajtsBef m_cAktRsBef ;

  // Az ablakban eppen szerkesztes alatt allo (aktualis) R/B - hez tartozo
  // nevezesi lista
  CNevezLista m_cAktNvzLst ;

  // Az aktualis R/B adatstruktura mentettsegi allapota
  BOOL m_bAktRsBefMentve  ;

  // Az aktualis nevezesi lista mentettsegi allapota
  BOOL m_bAktNvzLstMentve ;

  // Ablak, melybe az importalt adatokat irja
  // (hogy lehessen latni, merre tart eppen,hol van baja)
  CImportGyAbl* m_pImportAbl ;

  // Az importalando file
  CFile m_ImportFile ;

  // Az importalalas kezdosora
  int m_nImpKezdoSor ;

  // GLOBALIS ADATOK : ( CProba13App-bol atveve )

  // A R/B evet m_cAktSzez.m_usSzezon tartalmazza ...
  // A szerkesztendo nagydij nevehez tartozo kulcs
  unsigned m_uiNdNevKulcs ;

  // A felvitt versenyzo neveket tartalmazo lista Cb
  CVNev* m_pVersenyzo ;
  // A felvitt nemzetiseg neveket tartalmazo lista Cb
  CNev*  m_pNemz ;
  // A felvitt csapat neveket tartalmazo lista Cb
  CNev*  m_pCsapat ;
  // A felvitt motor neveket tartalmazo lista Cb
  CNev*  m_pMotor ;

 	// Az eredmenyeket tartalmazo lista ablak tabstopjai
 	int m_nRBFelvTbStp[6] ;

// Dialog Data
	//{{AFX_DATA(CRsBefFelv)
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Implementation
protected:
  // A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
  // DoDataExchange fuggvenybol
  virtual BOOL OnInitDialog() ;

	// Generated message map functions
	//{{AFX_MSG(CRsBefFelv)
	afx_msg void OnDblclkErdmLst();
	afx_msg void OnFelvesz();
	afx_msg void OnModosit();
	afx_msg void OnTorol();
	// Ennek nincs tul sok gyakorlati jelentosege
  afx_msg void OnRbFelvesz();
	afx_msg void OnRbTorol();
  afx_msg void OnNevek() ;
	virtual void OnCancel();
	virtual void OnOK();
	afx_msg void OnLeggyKor();
	afx_msg void OnBeszamit();
	afx_msg void OnChangeRsz();
  afx_msg void OnImportal() ;
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

  // Beolvassa az importalando file tartalmat CImportGyAbl::m_Sorok-ba
  // OnImportal() -> ImportFileBeo()
  BOOL ImportFileBeo() ;

  // A fv. a megadott(resource ID) Cb-t tolti fel, a megadott tipusu nevvel
  // A fv.-t az OnInitDialog-bol es az Importal fv. hivja
  BOOL NevCbFeltolt(int nComboBox, CNev* pKiirNev) ;
  // L. mint fent
  BOOL VNevCbFeltolt( int nComboBox, CVNev* pKiirVNev ) ;

  // A megadott combo box egy sorat jeloli ki, melyhez adatkent
  // a megadott kulcs tartozik
  BOOL CbSorKiv(CComboBox* pComboBox, DWORD dwKulcs) ;

  // Az egy rsbf sorhoz tartozo adatokat olvassa be a szerk. es cboxokbol
  BOOL RsBefSorBeo(unsigned& uiHelyez,
                   CNvzSor* pNvzSor, CRBBejegyzes* pRBBej) ;

  // A megadott sztringet atalakitja idoeredmennye / korhatrannya
  // A sztring ie: szam nemszam szam, korhatrany : nemszam szam nemszam
  BOOL StrBolIdoEr(const CString & sBeoIdoer, CRBBejegyzes* cBeoIdoerdm) ;

	// Az (aktualis) leggyorsabb kor adatokat irja ki a static textbe
	void LegyKorKiir() ;

	// Az aktualis rajtsorrend/befuto eredmenyek kiirasa a Lb - ba
	// Az Lb-ben kiszelektalando sorhoz tartozo nvzlst kulcs, 0 : a regit jeloli ki
	void AktRBKiir(DWORD dwUjKivRBSor) ;

  // A fv. a R/B mentetlensege eseten hivando meg, mentes eseten
  // A fv. szukseg eseten torli a szezonhoz tartozo NyVBOsz-t (NyVBOszTorol())
  void NyVBOszLekezel() ;
  // Torli a szezonhoz tartozo NyVBOsz-t
  void NyVBOszTorol() ;

  // Importalas eseten baj, ha ures sorra fut
  BOOL UresSor(CString& cSor) ;

  // Mivel az ablakot new konstr.-ral hozom letre, ebben a fv.-ben adom ki
  // a this pointerre a delete-t
  virtual void PostNcDestroy() ;

  void HibaKiir(long nRC) ;
  
  // Pl. az importalando file bezarasa
  ~CRsBefFelv() ;
} ;