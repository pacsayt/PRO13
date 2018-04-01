// proba13.h : main header file for the PROBA13application
//

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"  // main symbols

/////////////////////////////////////////////////////////////////////////////
// CProba13App:
// See proba13.cpp for the implementation of this class
//

class CProba13App : public CWinApp
{
public:
    CProba13App();

// Adattagok
    CNev  m_cCsapatNev     ;
    CNev  m_cMotorNev      ;
    CNev  m_cNagydijNev    ;
    CNev  m_cNemzetisegNev ;
    CNev  m_cPalyaNev      ;
    CVNev m_cVersenyzoNev  ; 
    CPontozas m_cPontozas  ;
    
public :
    
public:
// Overrides
   CDocument* DocViewFrameLetr(CString& sTemplNev) ;
    
   virtual BOOL InitInstance();

// Implementation
	//{{AFX_MSG(CProba13App)
	afx_msg void OnAppAbout();
	afx_msg void OnFilePrintSetup() ;
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
} ;