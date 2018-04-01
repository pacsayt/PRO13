// nevfelv.h : header file
//

#include "resource.h"  // main symbols

/////////////////////////////////////////////////////////////////////////////
// CNevFelv dialog

class CNevFelv : public CDialog
{
// Construction
public:
  // standard constructor  
	CNevFelv( eNevtipus eFelvNevtip, CWnd* pParent = NULL) ;

// Dialog Data
  //{{AFX_DATA(CNevFelv)
    enum { IDD = IDD_FELVITEL_NEVEK };
    // NOTE: the ClassWizard will add data members here
  //}}AFX_DATA

  // A Lb-ban kiszelektalt nevhez tartozo kulcsot tartalmazza
  unsigned m_uiKivNevKulcsa ;

  BOOL m_bModositva ;

private:
// Belso adattagok
  // Megadja, hogy az ablak eppen melyik nevtipussal foglalkozik eVers-nek
  // tulajdonkeppen itt nincsen ertelme, mert azt CVNevFelv csinalja
  eNevtipus m_eFelvNevtip ;
  // Mivel az osztalyt kulonbozo CNev osztalyokra akarom meghivni
  // a Csapat-, Motor-, Palya- (stb.) neveknek megfeleloen, az InitInstance fv.ben
  // masolom ide a megfelelo globalis CNev osztaly cimet
  CNev* m_pCNevAkt   ;
  // A kulonbozo neveket felvivo dialogus ablak kulonbozo felirata(i)
  CString m_sAktNev  ;

// Implementation
protected:
  // virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
    // A dialogus ablak mezoinek kezdeti feltoltese ide kerul a 
    // DoDataExchange fuggvenybol 
    virtual BOOL OnInitDialog() ;
  // Generated message map functions
  //{{AFX_MSG(CNevFelv)    
  // A szerkeszto ablak tartalmat veszi fel a lista ablakba, ha meg nem szerepel
  afx_msg void OnFelvesz();
  // A lista ablakban szelektalt elemet torli 
  afx_msg void OnTorol(); 
  //
  virtual void OnOK() ;
  virtual void OnCancel() ;
  // A szerkeszto ablak tartalma alapjan valtoztatja az aktualis kijelolest
  afx_msg void OnChangeFelvNevEdit();
  // A lista ablak tartalmat hozza le a szerkesztoablakba
  afx_msg void OnDblclkList();	
    // A lista ablakban szelektalt nevet irja at a szerkesztoablakba irt szoveggel
  afx_msg void OnAtnevez();
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()

  void UresLevagEV(CString& sMegvagando) ;
} ;
