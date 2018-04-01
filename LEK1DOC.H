// lek1doc.h : interface of the CLekEredm1Doc class
//
/////////////////////////////////////////////////////////////////////////////

// Az CLekEredm1Doc osztaly a CLekEredm1View osztallyal egyutt egy negyszog
// alaku tartomanyban valo klikkelest tesztel, a talalatot szinvaltassal jelzi. 

class CLekEredm1Doc : public CDocument
{
  DECLARE_DYNCREATE(CLekEredm1Doc)

protected: // create from serialization only
  CLekEredm1Doc();

// Attributes
public:
  // Adattag, mely a lekerdezesben szereplo nevtipus sztring eroforras-
  // azonositojat tartalmazza. int ? unsigned short ?
  eNevtipus      m_enLekNevTip ;
  // Adattag, mely a lekerdezes tipus sztring eroforras-azonositojat tartalmazza
  eLekerdTipus   m_enLekTip ;
  // Az elso szezon, melyre a kereses vonatkozik
  unsigned short m_usSzezontol ;
  // Az utolso szezon, melyre a kereses vonatkozik
  unsigned short m_usSzezonig  ;    

  CLekErMegjVaz m_cLekErMegjVaz ;

  // A gorgeteshez szukseges a dokumentum meretenek ismerete
  CSize m_cDokMeret ;

  // Az egerkurzor a teglalap folott van
  BOOL  m_bFolotte ;

// Operations
public:
  BOOL OnNewDocument() ;
  // A fv. a 'File' kozos dialogus ablak segitsegevel betolti
  // az ott kivalasztott dokumentumot
  BOOL Betolt() ;
  // Ne a DokuNev/DokuNevKiir fuggvenyek allitsak be a tagvaltozokat, mert
  // igy ezeket a parametereket printelesnel vehesse tagvaltozobol
  void TipSzezBeall(eNevtipus usLekNevTip, eLekerdTipus usLekTip,
                    unsigned short usSzezontol, unsigned short usSzezonig) ;
  // Hogy printelesnel is megkapjam a cimet ketteszedtem a DokuNevKiir fv-t:
  CString DokuNev() ;
  // A fv. kiirja a tagvaltozok alapjan osszeallitott 'dokumentumnevet' a
  // dokumentum ablakanak cimsoraba.
  void DokuNevKiir() ;

// Implementation
public:
  virtual ~CLekEredm1Doc() ;
  virtual void Serialize(CArchive& ar) ;
#ifdef _DEBUG
  virtual void AssertValid() const;
  virtual void Dump(CDumpContext& dc) const;
#endif

protected:
//  virtual BOOL OnNewDocument();

// Generated message map functions
protected:
  //{{AFX_MSG(CUjdokletDoc)
  afx_msg void OnMentes() ;
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()
} ;