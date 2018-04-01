// szezndva.h : header file
//
// Ez a dialogus ablak osztaly a rajtsorrend/befuto felvetele elottt jon fel,
// segitsegevel valaszthato ki, melyik szezon melyik nagydijat + r/b akarja
// az ember modositani/felvenni. A dialogus ablak a kivalasztott szez+nd+r/b
// alapjan hivja fel a kov dialogus ablakot, melyben a tenyleges szerkesztes
// elvegezheto.
// (Ez azert kerult kulon dialogus ablakba, mert a r/b felvevo dialogus
// ablakban nem volt mar hely.)

/////////////////////////////////////////////////////////////////////////////
// CSzezNdVal - szezon / nagydij kivalasztasara szolgalo dialogus ablak

class CSzezNdVal : public CDialog
{
// Construction
public :
  CSzezNdVal(CWnd* pParent = NULL) ;

// Dialog Data
  //{{AFX_DATA(CSzezNdVal)
    enum { IDD = IDD_SZEZ_ND_VAL_D };
    // NOTE: the ClassWizard will add data members here
  //}}AFX_DATA

protected :
  // GLOBALIS ADATOK : ( CProba13App-bol atveve )

  // Az eppen a Szezon Lb-ban kijelolt szezon adatait tartalmazo adattag
  // (Ezekbol jelenleg csak a nagydijak erdekesek)
  CSzezon   m_cAktSzez ;

  // A felvitt nagydijneveket tartalmazo lista Lb
  CNev*     m_pNagyd ;

// Implementation
protected:
  // A dialogus ablak mezoinek kezdeti feltoltese ide kerul a
  // DoDataExchange fuggvenybol
  virtual BOOL OnInitDialog() ;

  virtual void OnCancel() ;
  // Generated message map functions
  //{{AFX_MSG(CSzezNdVal)
    afx_msg void OnSelchgFlvSzez() ;
    afx_msg void OnSzerk() ;
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()

  void HibaKiir(long nRC) ;
  
  // Mivel az ablakot new konstr.-ral hozom letre, ebben a fv.-ben adom ki
  // a this pointerre a delete-t
  virtual void PostNcDestroy() ;
} ;