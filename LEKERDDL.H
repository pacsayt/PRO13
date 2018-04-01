// lekerddl.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CLekerdDlg dialog

class CLekerdDlg : public CDialog
{
// Construction
public:
  // Mivel a dialogus ablak az osszes 'nevre' vonatkozo lekerdezest kiszolgalja
  // itt -a konstruktorban- adom meg, hogy melyik nevtipusrol van szo (menuid)
  CLekerdDlg(eNevtipus enNevtipus, CWnd* pParent = NULL) ;

  // Dialog Data
  //{{AFX_DATA(CLekerdDlg)
    enum { IDD = IDD_LEKERD_1 } ;
    // NOTE: the ClassWizard will add data members here
  //}}AFX_DATA

// Implementation
protected:
  // Adattagok     
  // Megadja, melyik nevre vonatkozo lekerdezest vegzi eppen az ablak
  // Azonos a menuazonositoval. A konstruktorban adando meg.
  eNevtipus m_enNevtipus ;
  // A lekerdezes tipusa - a GetCheckedRadioButton megfelelo visszat. erteke
  eLekerdTipus m_enLekTip ;
  // A szezon, melytol a keresest inditani kell
  unsigned m_uiSzezontol ;
  // A szezon, melyig a kereses terjed
//  unsigned m_uiSzezonig  ;    
  // Minden, ami nem versenyzonev
  CNev* m_pNev ;
  // Ha versenyzonevekre vonatkozik a lekerdezes ez ervenyes
  CVNev* m_pVNev ;
  // A tabstopok meretet megado 'tomb' (versenyzo neveknel kell)
  int m_nTabstopok[1] ;
  // Az eppen lekerdezes alatt allo szezon nevezesi listaja
  CNevezLista m_cAktNvzLst ;                                 
  // Az eppen lekerdezes alatt allo szezon, mivel a nagydijak kellenek,
  // ill. hogy szerepel-e felvett r/b 
  // 1..254 vagy 255 - szerepel nevezesi listaban
  CSzezon m_cAktSzez ;
  // Az ablakban eppen lekerdezes alatt allo (aktualis) R/B adatstruktura
  CRajtsBef m_cAktRsBef ;

  // A kivalasztott nevek kulcsait tartalmazo tomb
  unsigned* m_pKivNevek ;    
  // A lekerdezesben szereplo nevek szama
  unsigned  m_nNevekSzama ;

  // VB Szereplesek lekerdezese eseten ebbe a tombbe gyujti az eredmenyeket
  // a lekerdezesben szereplo nevekre csoportositva,majd rendezes utan adodnak
  // a dokumentumhoz, ahol csak nevek szerinti csoportositas folyik
  CVBSzerepl** m_pVBSzerepl ;
  // Az aktualis lekerdezeshez tartozo dokumentum (!=NULL az1. talalat utan)
  // Ha uj lekerdezes kezdodik, ki kell nullazni, nehogy egy regihez adjon 
  // tovabbiakat hozza (pl. CFgyEeLgykOe-hez CPillAll-t). Ez egy lek. ^ egy
  // lek. ablaknal automatikusan teljesul.
  CLekEredm1Doc* m_pCLekEr1Doc ;

  // A dialogus ablak kepernyore kerulese elotti inicializalas:
  // a lista ablak feltoltese
  virtual BOOL OnInitDialog() ;

  // Generated message map functions
  //{{AFX_MSG(CLekerdDlg)
    afx_msg void OnKereses() ;
    afx_msg void OnKilepes() ;
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()

  // A nagydijnevekre vonatkozo (szezononkenti) lekerdezest vegzo fv
  void NagydNevLekerd() ;
  // A hol versenyzett lekerdezest vegrehajto fv (globalis adatatvitel^ es \/)
  void HolVersLekerd() ;
  // A KIJELOLT szezon pillanatnyi allasa a kijelolt nevre vonatkoztatva
  void PillAllLekerd() ;
  // A vilagbajnoki szereplesekre vonatkozo lekerdezesek
  void VBSzereplLekerd() ;
  // A nem a fenti harom fv altal vegzett lekerdezest (altalanos eset) vegzo fv
  void AltLekerd() ;
  // A fuggveny a neveket es a 'kezdo' szezont olvassa be
  // rc : a kivalasztott elemek szama, 0 ha nem lett elem kivalasztva,
  // vagy egyeb (new, LB_ERR) hiba lepett fel
  // Tagvaltozok implicit feltoltese : m_pKivNevek / m_nNevekSzama, m_usSzezontol
  int NevKezdSzezBeo(unsigned& usSzezontol) ;
  // A fv. a parameterkent atadott nevkulcs tombben levo nevekre megadja
  // az elso es az utolso elofordulast szezonban
  // A lekerdezesnel az a legszukebb halmaz kell, melybe az osszes nev belefer
//  int ElsoUtElofSzez(unsigned short* pusSzezTol, unsigned short* pusSzezIg) ;

  // Vo: CNvzListFelv::~
  // Az m_uiSzezontol-al adott nevezesi listat tolti be az adatbazisbol

  BOOL NvzLstBetolt(unsigned uiBeoSzez) ;
  // A dialogus ablak osztaly szezon adattagjanak betoltese
  BOOL SzezBetolt(unsigned uiBeoSzez) ;
  // A tagvaltozokkal megadott evszamu, ezen belul kodu R/B-t tolti be
  BOOL RsBefBetolt(unsigned uiSzezon, unsigned uiNagydij, eAblTipRsBef enRajtsBef) ;
  // A VB szereples lekerdezese soran szukseg van megallapitani a CPillAll-
  // tombben melyik elemhez tartozik a megadott nevkulcs. TRUE : talalta.
  BOOL NevKulcsKeres( CPillAll** pCPillAllTmb, CNvzSor* pNvzLstSor,
                      unsigned short& usTombPoz ) ;
  // usTombPoz itt bekimenoadat!!!, az m_nNevtipus-su nevkulcsot adom at
  BOOL NevKulcsKeres( CPillAll** pCPillAllTmb, unsigned uiNevKlcs,
                      unsigned short& usTombPoz ) ;
  // A szezon pill.all.-at CPillAll-elemek mindig vagy versenyzo, vagy csapat
  // mezonkenti osszesitese adja. Ez utan szelektalok pl. nemzetiseg alapjan.
  // A fv-t a VBSzereplLekerd() hivja fel.
  BOOL VersCsapKeres( CPillAll** pCPillAllTmb, CNvzSor* pNvzLstSor,
                      unsigned short& usTombPoz ) ;
  // A fv-t a VBSzereplLekerd() hivja a kivalasztott nevek szezonbeli eredmenyet
  // hozzaadando
  BOOL VBSzerHozzaad(CVBSzerepl* pUjElem) ;
  // Megadja, hogy a parameterkent atadott CPillAll lekerdezesben szereplo tipusu
  // neve ki lett-e valasztva a dialogus ablakban (TRUE)
  BOOL KivNev( CPillAll* pCPillAll) ;
  // A fv. beallitja az ujonnan, a dialogusablakban letrehozott dokument
  // ablakanak a cimmet a m_nNevtipus, m_nLekTip, m_usSzezontol, m_usSzezonig
  // tagvaltozok alapjan
  void DokuNevBeall() ;

  // Az SQL_PillAll altal foglalt memoria felszabaditasa.
  // (Helyben olvashatatlanna teszi a programot)
  //                                \/ * ???
  void Felszabadit( sPillAllLekErTip sPillAllLekEr,
                    unsigned**       ppKivNevek, unsigned* pNevekSzama) ;

  // nRC==-1 -> memoria foglalasi hiba, kulonben a gl_sEnSQLCAm.sqlerrm.sqlerrmc
  // tartalmat irja ki egy MessageBox-ba
  void HibaKiir(long nRC) ;
} ;