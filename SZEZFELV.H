// szezfelv.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// Hibakonstansok az AktSzezMent fuggveny szamara
#define H_OK             (int) 0
#define H_ROVID_SZAM     (int) 1
#define H_KIS_SZAM       (int) 2
#define H_NINCS_PONTOZAS (int) 3
#define H_FATALIS_HIBA   (int) -1

/////////////////////////////////////////////////////////////////////////////
// CSzezFelv dialog

class CSzezFelv : public CDialog
{
// Construction
public:
  CSzezFelv( CWnd* pParent = NULL) ;

// Dialog Data
private:
  // LOKALIS ADATOK : Az aktualis (2.);4.) Ed Lb-k ben levo) szezon adatai,
  // melyeket a dialogus ablak olvas be filebol/ a felhasznalotol, majd az
  // uj/modositott szezont az OnOK-ban visszairja
  CSzezon m_cAktSzezon ;

  // Az kepernyon levo szezon aktualis allapota mentve van-e
  BOOL m_bAktSzezonMentve ;

  // Mivel a CEdit kontrolok inicializalasakor is meghivodik a OnEdchg~ fv.
  // meg kell tudni kulonboztetni, hogy a felhasznalo, vagy maga a program
  // irt bele:
  BOOL m_bInicializalas ;

  // GLOBALIS ADATOK ( CProba13App-bol atveve )

  // A felvitt pontozasok
  CPontozas* m_pPontozas ;
  // A felvitt nagydijneveket tartalmazo lista Cb
  CNev*        m_pNagyd ;
  // A felvitt palyaneveket tartalmazo lista Cb
  CNev*        m_pPalya ;

    // A pontozasokat tartalmazo lista ablak tabstopjai
// 	int m_nPontTbStp[9] ;   A COMBO BOX-NAL NEM ADHATO MEG TABSTOP
 	// A futamokat tartalmazo lista ablak tabstopjai
 	int m_nFutTbStp[2] ;

	//{{AFX_DATA(CSzezFelv)
	enum { IDD = IDD_SZEZ_FELV };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Implementation
private:
    // A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
    // DoDataExchange fuggvenybol
    virtual BOOL OnInitDialog() ;

	// Generated message map functions
	//{{AFX_MSG(CSzezFelv)
	afx_msg void OnEdchgSzezCb();
	afx_msg void OnSelchgSzez();
	afx_msg void OnDblclkFutamList();
	afx_msg void OnSelchgNagyd();
	afx_msg void OnSelchgPalya();
	afx_msg void OnFelv();
	afx_msg void OnMod();
	afx_msg void OnTorol();
	afx_msg void OnSelchgPont();
	afx_msg void OnSzezFelv();
	afx_msg void OnSzezTorol();
	virtual void OnOK();
	virtual void OnCancel();
	afx_msg void OnChgPntEdzE();
	afx_msg void OnChgPntLgyKor();
	afx_msg void OnChgElsLgjBesz();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

  // A fuggveny egy futam adatait olvassa be a Nagydij(Cb)/Palya(Cb)/Datum/Ed
  // mezokbol, majd megvizsgalja a.) hogy a megadott nagydij szerepel-e mar a
  // szezonban b.) a datum szintakszisa helyes-e FALSE, ha a.) v. b.) felte-
  // tel miatt nem ervenyes a visszaadott eredmeny, vagy uresek a mezok.
  // (felhasznalja a DatumBeo fuggvenyt)
  BOOL FutAdatBeo(SNagydij* pBeolvasottFut) ;
  // A fuggveny a megadott evszamu szezont beolvassa filebol
  //(felhasznalja az UjSzezKiir fuggvenyt)
  long SzezonBetolt( unsigned uiBeoSzez) ;
  // A fuggveny az aktualis futam lista kiiratasat vegzi a Lb-ba
  // A masodik parameter a kijelolendo nagydij kulcsa
  void AktSzezKiir( unsigned uiUjKiv) ;
  // A fuggveny beolvassa a futam datumat a IDC_DAT_E szerkesztoablakbol,
  // ha sikerult helyes erteket beolvasni, TRUE-t ad vissza.
  BOOL DatumBeo(SDatum& sBeoDat) ;
  // A fuggveny az edzeselsosegert, a leggyorsabb korert jaro pontokat,
  // valamint a vegeredmenybe beszamito elso legjobb futamok szamat olvassa be
  BOOL EdzELgyKElsLgjBeo() ;

  BOOL EdzElsPontBeo() ;
  BOOL LgyKorPontBeo() ;
  BOOL LegjBefSzamBeo() ;

  // A fuggveny a pontozas tablak unsigned char(BYTE) tipusat
  // alakitja CString tipusuva, a szamok koze sElv kerul es nem char, mert Cb-nal
  // nem adhato meg tabulator. 
  void PontbolStr(pontadat* pPontAdat, CString& sCelStr, CString& sElv) ;
  // nRC==-1 -> memoria foglalasi hiba, kulonben a gl_sEnSQLCAm.sqlerrm.sqlerrmc
  // tartalmat irja ki egy MessageBox-ba
  void HibaKiir(long nRC) ;

public :
  // Destruktor tesztcelokra
  ~CSzezFelv() ;
} ;