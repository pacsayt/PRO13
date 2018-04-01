# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=PROBA13 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to PROBA13 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "PROBA13 - Win32 Release" && "$(CFG)" !=\
 "PROBA13 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "PROBA13.MAK" CFG="PROBA13 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PROBA13 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "PROBA13 - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "PROBA13 - Win32 Debug"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

# PROP BASE Use_MFC 1
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\PROBA13.exe"

CLEAN : 
	-@erase "$(INTDIR)\ALAPTIP.OBJ"
	-@erase "$(INTDIR)\BEFVKIEG.OBJ"
	-@erase "$(INTDIR)\CNEV.OBJ"
	-@erase "$(INTDIR)\csatlako.obj"
	-@erase "$(INTDIR)\CSATLDLG.OBJ"
	-@erase "$(INTDIR)\CSZEZON.OBJ"
	-@erase "$(INTDIR)\CVNEV.OBJ"
	-@erase "$(INTDIR)\dbglobal.obj"
	-@erase "$(INTDIR)\dbnev.obj"
	-@erase "$(INTDIR)\dbnvzlst.obj"
	-@erase "$(INTDIR)\dbpontoz.obj"
	-@erase "$(INTDIR)\dbrsbef.obj"
	-@erase "$(INTDIR)\DBSZEZON.obj"
	-@erase "$(INTDIR)\dbvnev.obj"
	-@erase "$(INTDIR)\DLGBMDIC.OBJ"
	-@erase "$(INTDIR)\ensqlcam.obj"
	-@erase "$(INTDIR)\FGYEELGY.OBJ"
	-@erase "$(INTDIR)\HOLVKIV.OBJ"
	-@erase "$(INTDIR)\IDOEREDM.OBJ"
	-@erase "$(INTDIR)\IMPGYABL.OBJ"
	-@erase "$(INTDIR)\KERERMEZ.OBJ"
	-@erase "$(INTDIR)\LEK1DOC.OBJ"
	-@erase "$(INTDIR)\LEK1VIEW.OBJ"
	-@erase "$(INTDIR)\LEKERDDL.OBJ"
	-@erase "$(INTDIR)\LKMGJVAZ.OBJ"
	-@erase "$(INTDIR)\MAINFRM.OBJ"
	-@erase "$(INTDIR)\NEVFELV.OBJ"
	-@erase "$(INTDIR)\NEVZLIST.OBJ"
	-@erase "$(INTDIR)\NVZLISTF.OBJ"
	-@erase "$(INTDIR)\PILLALL.OBJ"
	-@erase "$(INTDIR)\PONTFELV.OBJ"
	-@erase "$(INTDIR)\pontozas.obj"
	-@erase "$(INTDIR)\PROBA13.OBJ"
	-@erase "$(INTDIR)\PROBA13.pch"
	-@erase "$(INTDIR)\PROBA13.res"
	-@erase "$(INTDIR)\RAJTSBEF.OBJ"
	-@erase "$(INTDIR)\RSBEFFEL.OBJ"
	-@erase "$(INTDIR)\STDAFX.OBJ"
	-@erase "$(INTDIR)\SZEZFELV.OBJ"
	-@erase "$(INTDIR)\SZEZNDVA.OBJ"
	-@erase "$(INTDIR)\VBSZEREP.OBJ"
	-@erase "$(INTDIR)\VNEVFELV.OBJ"
	-@erase "$(OUTDIR)\PROBA13.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /FR /Yu"STDAFX.H" /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Yu"STDAFX.H" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/PROBA13.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/PROBA13.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 oldnames.lib /nologo /stack:0x2800 /subsystem:windows /machine:IX86
# ADD LINK32 oldnames.lib /nologo /stack:0x2800 /subsystem:windows /machine:IX86
LINK32_FLAGS=oldnames.lib /nologo /stack:0x2800 /subsystem:windows\
 /incremental:no /pdb:"$(OUTDIR)/PROBA13.pdb" /machine:IX86 /def:".\PROBA13.DEF"\
 /out:"$(OUTDIR)/PROBA13.exe" 
DEF_FILE= \
	".\PROBA13.DEF"
LINK32_OBJS= \
	"$(INTDIR)\ALAPTIP.OBJ" \
	"$(INTDIR)\BEFVKIEG.OBJ" \
	"$(INTDIR)\CNEV.OBJ" \
	"$(INTDIR)\csatlako.obj" \
	"$(INTDIR)\CSATLDLG.OBJ" \
	"$(INTDIR)\CSZEZON.OBJ" \
	"$(INTDIR)\CVNEV.OBJ" \
	"$(INTDIR)\dbglobal.obj" \
	"$(INTDIR)\dbnev.obj" \
	"$(INTDIR)\dbnvzlst.obj" \
	"$(INTDIR)\dbpontoz.obj" \
	"$(INTDIR)\dbrsbef.obj" \
	"$(INTDIR)\DBSZEZON.obj" \
	"$(INTDIR)\dbvnev.obj" \
	"$(INTDIR)\DLGBMDIC.OBJ" \
	"$(INTDIR)\ensqlcam.obj" \
	"$(INTDIR)\FGYEELGY.OBJ" \
	"$(INTDIR)\HOLVKIV.OBJ" \
	"$(INTDIR)\IDOEREDM.OBJ" \
	"$(INTDIR)\IMPGYABL.OBJ" \
	"$(INTDIR)\KERERMEZ.OBJ" \
	"$(INTDIR)\LEK1DOC.OBJ" \
	"$(INTDIR)\LEK1VIEW.OBJ" \
	"$(INTDIR)\LEKERDDL.OBJ" \
	"$(INTDIR)\LKMGJVAZ.OBJ" \
	"$(INTDIR)\MAINFRM.OBJ" \
	"$(INTDIR)\NEVFELV.OBJ" \
	"$(INTDIR)\NEVZLIST.OBJ" \
	"$(INTDIR)\NVZLISTF.OBJ" \
	"$(INTDIR)\PILLALL.OBJ" \
	"$(INTDIR)\PONTFELV.OBJ" \
	"$(INTDIR)\pontozas.obj" \
	"$(INTDIR)\PROBA13.OBJ" \
	"$(INTDIR)\PROBA13.res" \
	"$(INTDIR)\RAJTSBEF.OBJ" \
	"$(INTDIR)\RSBEFFEL.OBJ" \
	"$(INTDIR)\STDAFX.OBJ" \
	"$(INTDIR)\SZEZFELV.OBJ" \
	"$(INTDIR)\SZEZNDVA.OBJ" \
	"$(INTDIR)\VBSZEREP.OBJ" \
	"$(INTDIR)\VNEVFELV.OBJ" \
	"e:\proc\PRO80\LIB\MSVC\SQLLIB80.LIB"

"$(OUTDIR)\PROBA13.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP BASE Use_MFC 1
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\PROBA13.exe" "$(OUTDIR)\PROBA13.bsc"

CLEAN : 
	-@erase "$(INTDIR)\ALAPTIP.OBJ"
	-@erase "$(INTDIR)\ALAPTIP.SBR"
	-@erase "$(INTDIR)\BEFVKIEG.OBJ"
	-@erase "$(INTDIR)\BEFVKIEG.SBR"
	-@erase "$(INTDIR)\CNEV.OBJ"
	-@erase "$(INTDIR)\CNEV.SBR"
	-@erase "$(INTDIR)\csatlako.obj"
	-@erase "$(INTDIR)\csatlako.sbr"
	-@erase "$(INTDIR)\CSATLDLG.OBJ"
	-@erase "$(INTDIR)\CSATLDLG.SBR"
	-@erase "$(INTDIR)\CSZEZON.OBJ"
	-@erase "$(INTDIR)\CSZEZON.SBR"
	-@erase "$(INTDIR)\CVNEV.OBJ"
	-@erase "$(INTDIR)\CVNEV.SBR"
	-@erase "$(INTDIR)\dbglobal.obj"
	-@erase "$(INTDIR)\dbglobal.sbr"
	-@erase "$(INTDIR)\dbnev.obj"
	-@erase "$(INTDIR)\dbnev.sbr"
	-@erase "$(INTDIR)\dbnvzlst.obj"
	-@erase "$(INTDIR)\dbnvzlst.sbr"
	-@erase "$(INTDIR)\dbpontoz.obj"
	-@erase "$(INTDIR)\dbpontoz.sbr"
	-@erase "$(INTDIR)\dbrsbef.obj"
	-@erase "$(INTDIR)\dbrsbef.sbr"
	-@erase "$(INTDIR)\DBSZEZON.obj"
	-@erase "$(INTDIR)\DBSZEZON.sbr"
	-@erase "$(INTDIR)\dbvnev.obj"
	-@erase "$(INTDIR)\dbvnev.sbr"
	-@erase "$(INTDIR)\DLGBMDIC.OBJ"
	-@erase "$(INTDIR)\DLGBMDIC.SBR"
	-@erase "$(INTDIR)\ensqlcam.obj"
	-@erase "$(INTDIR)\ensqlcam.sbr"
	-@erase "$(INTDIR)\FGYEELGY.OBJ"
	-@erase "$(INTDIR)\FGYEELGY.SBR"
	-@erase "$(INTDIR)\HOLVKIV.OBJ"
	-@erase "$(INTDIR)\HOLVKIV.SBR"
	-@erase "$(INTDIR)\IDOEREDM.OBJ"
	-@erase "$(INTDIR)\IDOEREDM.SBR"
	-@erase "$(INTDIR)\IMPGYABL.OBJ"
	-@erase "$(INTDIR)\IMPGYABL.SBR"
	-@erase "$(INTDIR)\KERERMEZ.OBJ"
	-@erase "$(INTDIR)\KERERMEZ.SBR"
	-@erase "$(INTDIR)\LEK1DOC.OBJ"
	-@erase "$(INTDIR)\LEK1DOC.SBR"
	-@erase "$(INTDIR)\LEK1VIEW.OBJ"
	-@erase "$(INTDIR)\LEK1VIEW.SBR"
	-@erase "$(INTDIR)\LEKERDDL.OBJ"
	-@erase "$(INTDIR)\LEKERDDL.SBR"
	-@erase "$(INTDIR)\LKMGJVAZ.OBJ"
	-@erase "$(INTDIR)\LKMGJVAZ.SBR"
	-@erase "$(INTDIR)\MAINFRM.OBJ"
	-@erase "$(INTDIR)\MAINFRM.SBR"
	-@erase "$(INTDIR)\NEVFELV.OBJ"
	-@erase "$(INTDIR)\NEVFELV.SBR"
	-@erase "$(INTDIR)\NEVZLIST.OBJ"
	-@erase "$(INTDIR)\NEVZLIST.SBR"
	-@erase "$(INTDIR)\NVZLISTF.OBJ"
	-@erase "$(INTDIR)\NVZLISTF.SBR"
	-@erase "$(INTDIR)\PILLALL.OBJ"
	-@erase "$(INTDIR)\PILLALL.SBR"
	-@erase "$(INTDIR)\PONTFELV.OBJ"
	-@erase "$(INTDIR)\PONTFELV.SBR"
	-@erase "$(INTDIR)\pontozas.obj"
	-@erase "$(INTDIR)\pontozas.sbr"
	-@erase "$(INTDIR)\PROBA13.OBJ"
	-@erase "$(INTDIR)\PROBA13.res"
	-@erase "$(INTDIR)\PROBA13.SBR"
	-@erase "$(INTDIR)\RAJTSBEF.OBJ"
	-@erase "$(INTDIR)\RAJTSBEF.SBR"
	-@erase "$(INTDIR)\RSBEFFEL.OBJ"
	-@erase "$(INTDIR)\RSBEFFEL.SBR"
	-@erase "$(INTDIR)\STDAFX.OBJ"
	-@erase "$(INTDIR)\STDAFX.SBR"
	-@erase "$(INTDIR)\SZEZFELV.OBJ"
	-@erase "$(INTDIR)\SZEZFELV.SBR"
	-@erase "$(INTDIR)\SZEZNDVA.OBJ"
	-@erase "$(INTDIR)\SZEZNDVA.SBR"
	-@erase "$(INTDIR)\VBSZEREP.OBJ"
	-@erase "$(INTDIR)\VBSZEREP.SBR"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\VNEVFELV.OBJ"
	-@erase "$(INTDIR)\VNEVFELV.SBR"
	-@erase "$(OUTDIR)\PROBA13.bsc"
	-@erase "$(OUTDIR)\PROBA13.exe"
	-@erase "$(OUTDIR)\PROBA13.map"
	-@erase ".\csatlako.cpp"
	-@erase ".\dbglobal.cpp"
	-@erase ".\dbnev.cpp"
	-@erase ".\dbnvzlst.cpp"
	-@erase ".\dbpontoz.cpp"
	-@erase ".\dbrsbef.cpp"
	-@erase ".\DBSZEZON.cpp"
	-@erase ".\dbvnev.cpp"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /Yu"STDAFX.H" /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fr /Yu"STDAFX.H" /c
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\Debug/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/PROBA13.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/PROBA13.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\ALAPTIP.SBR" \
	"$(INTDIR)\BEFVKIEG.SBR" \
	"$(INTDIR)\CNEV.SBR" \
	"$(INTDIR)\csatlako.sbr" \
	"$(INTDIR)\CSATLDLG.SBR" \
	"$(INTDIR)\CSZEZON.SBR" \
	"$(INTDIR)\CVNEV.SBR" \
	"$(INTDIR)\dbglobal.sbr" \
	"$(INTDIR)\dbnev.sbr" \
	"$(INTDIR)\dbnvzlst.sbr" \
	"$(INTDIR)\dbpontoz.sbr" \
	"$(INTDIR)\dbrsbef.sbr" \
	"$(INTDIR)\DBSZEZON.sbr" \
	"$(INTDIR)\dbvnev.sbr" \
	"$(INTDIR)\DLGBMDIC.SBR" \
	"$(INTDIR)\ensqlcam.sbr" \
	"$(INTDIR)\FGYEELGY.SBR" \
	"$(INTDIR)\HOLVKIV.SBR" \
	"$(INTDIR)\IDOEREDM.SBR" \
	"$(INTDIR)\IMPGYABL.SBR" \
	"$(INTDIR)\KERERMEZ.SBR" \
	"$(INTDIR)\LEK1DOC.SBR" \
	"$(INTDIR)\LEK1VIEW.SBR" \
	"$(INTDIR)\LEKERDDL.SBR" \
	"$(INTDIR)\LKMGJVAZ.SBR" \
	"$(INTDIR)\MAINFRM.SBR" \
	"$(INTDIR)\NEVFELV.SBR" \
	"$(INTDIR)\NEVZLIST.SBR" \
	"$(INTDIR)\NVZLISTF.SBR" \
	"$(INTDIR)\PILLALL.SBR" \
	"$(INTDIR)\PONTFELV.SBR" \
	"$(INTDIR)\pontozas.sbr" \
	"$(INTDIR)\PROBA13.SBR" \
	"$(INTDIR)\RAJTSBEF.SBR" \
	"$(INTDIR)\RSBEFFEL.SBR" \
	"$(INTDIR)\STDAFX.SBR" \
	"$(INTDIR)\SZEZFELV.SBR" \
	"$(INTDIR)\SZEZNDVA.SBR" \
	"$(INTDIR)\VBSZEREP.SBR" \
	"$(INTDIR)\VNEVFELV.SBR"

"$(OUTDIR)\PROBA13.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 oldnames.lib /nologo /stack:0xc000 /subsystem:windows /debug /machine:IX86
# ADD LINK32 oldnames.lib /nologo /stack:0xc000 /subsystem:windows /profile /map /debug /machine:IX86
LINK32_FLAGS=oldnames.lib /nologo /stack:0xc000 /subsystem:windows /profile\
 /map:"$(INTDIR)/PROBA13.map" /debug /machine:IX86 /def:".\PROBA13.DEF"\
 /out:"$(OUTDIR)/PROBA13.exe" 
DEF_FILE= \
	".\PROBA13.DEF"
LINK32_OBJS= \
	"$(INTDIR)\ALAPTIP.OBJ" \
	"$(INTDIR)\BEFVKIEG.OBJ" \
	"$(INTDIR)\CNEV.OBJ" \
	"$(INTDIR)\csatlako.obj" \
	"$(INTDIR)\CSATLDLG.OBJ" \
	"$(INTDIR)\CSZEZON.OBJ" \
	"$(INTDIR)\CVNEV.OBJ" \
	"$(INTDIR)\dbglobal.obj" \
	"$(INTDIR)\dbnev.obj" \
	"$(INTDIR)\dbnvzlst.obj" \
	"$(INTDIR)\dbpontoz.obj" \
	"$(INTDIR)\dbrsbef.obj" \
	"$(INTDIR)\DBSZEZON.obj" \
	"$(INTDIR)\dbvnev.obj" \
	"$(INTDIR)\DLGBMDIC.OBJ" \
	"$(INTDIR)\ensqlcam.obj" \
	"$(INTDIR)\FGYEELGY.OBJ" \
	"$(INTDIR)\HOLVKIV.OBJ" \
	"$(INTDIR)\IDOEREDM.OBJ" \
	"$(INTDIR)\IMPGYABL.OBJ" \
	"$(INTDIR)\KERERMEZ.OBJ" \
	"$(INTDIR)\LEK1DOC.OBJ" \
	"$(INTDIR)\LEK1VIEW.OBJ" \
	"$(INTDIR)\LEKERDDL.OBJ" \
	"$(INTDIR)\LKMGJVAZ.OBJ" \
	"$(INTDIR)\MAINFRM.OBJ" \
	"$(INTDIR)\NEVFELV.OBJ" \
	"$(INTDIR)\NEVZLIST.OBJ" \
	"$(INTDIR)\NVZLISTF.OBJ" \
	"$(INTDIR)\PILLALL.OBJ" \
	"$(INTDIR)\PONTFELV.OBJ" \
	"$(INTDIR)\pontozas.obj" \
	"$(INTDIR)\PROBA13.OBJ" \
	"$(INTDIR)\PROBA13.res" \
	"$(INTDIR)\RAJTSBEF.OBJ" \
	"$(INTDIR)\RSBEFFEL.OBJ" \
	"$(INTDIR)\STDAFX.OBJ" \
	"$(INTDIR)\SZEZFELV.OBJ" \
	"$(INTDIR)\SZEZNDVA.OBJ" \
	"$(INTDIR)\VBSZEREP.OBJ" \
	"$(INTDIR)\VNEVFELV.OBJ" \
	"e:\proc\PRO80\LIB\MSVC\SQLLIB80.LIB"

"$(OUTDIR)\PROBA13.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "PROBA13 - Win32 Release"
# Name "PROBA13 - Win32 Debug"

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\STDAFX.CPP
DEP_CPP_STDAF=\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

# ADD BASE CPP /Yc"STDAFX.H"
# ADD CPP /Yc"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fp"$(INTDIR)/PROBA13.pch" /Yc"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\STDAFX.OBJ" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\PROBA13.pch" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# ADD BASE CPP /Yc"STDAFX.H"
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\STDAFX.OBJ" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\STDAFX.SBR" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\MAINFRM.CPP
DEP_CPP_MAINF=\
	".\alaptip.h"\
	".\cnev.h"\
	".\csatlako.h"\
	".\CsatlDlg.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\fgyeelgy.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\IMPGYABL.H"\
	".\kerermez.h"\
	".\lek1doc.h"\
	".\lekerddl.h"\
	".\lkmgjvaz.h"\
	".\mainfrm.h"\
	".\nevfelv.h"\
	".\NEVZLIST.H"\
	".\nvzlistf.h"\
	".\pillall.h"\
	".\PONTFELV.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\RSBEFFEL.H"\
	".\stdafx.h"\
	".\szezfelv.h"\
	".\szezndva.h"\
	".\vbszerep.h"\
	".\vnevfelv.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\MAINFRM.OBJ" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\MAINFRM.OBJ" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\MAINFRM.SBR" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CNEV.CPP
DEP_CPP_CNEV_=\
	".\alaptip.h"\
	".\cnev.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\CNEV.OBJ" : $(SOURCE) $(DEP_CPP_CNEV_) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\CNEV.OBJ" : $(SOURCE) $(DEP_CPP_CNEV_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\CNEV.SBR" : $(SOURCE) $(DEP_CPP_CNEV_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CVNEV.CPP
DEP_CPP_CVNEV=\
	".\alaptip.h"\
	".\cvnev.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\CVNEV.OBJ" : $(SOURCE) $(DEP_CPP_CVNEV) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\CVNEV.OBJ" : $(SOURCE) $(DEP_CPP_CVNEV) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\CVNEV.SBR" : $(SOURCE) $(DEP_CPP_CVNEV) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\NEVFELV.CPP
DEP_CPP_NEVFE=\
	".\alaptip.h"\
	".\cnev.h"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\nevfelv.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\NEVFELV.OBJ" : $(SOURCE) $(DEP_CPP_NEVFE) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\NEVFELV.OBJ" : $(SOURCE) $(DEP_CPP_NEVFE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\NEVFELV.SBR" : $(SOURCE) $(DEP_CPP_NEVFE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\PONTFELV.CPP
DEP_CPP_PONTF=\
	".\alaptip.h"\
	".\cnev.h"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\nevfelv.h"\
	".\PONTFELV.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\stdafx.h"\
	".\vnevfelv.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\PONTFELV.OBJ" : $(SOURCE) $(DEP_CPP_PONTF) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\PONTFELV.OBJ" : $(SOURCE) $(DEP_CPP_PONTF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\PONTFELV.SBR" : $(SOURCE) $(DEP_CPP_PONTF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ALAPTIP.CPP
DEP_CPP_ALAPT=\
	".\alaptip.h"\
	".\cnev.h"\
	".\cvnev.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\ALAPTIP.OBJ" : $(SOURCE) $(DEP_CPP_ALAPT) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\ALAPTIP.OBJ" : $(SOURCE) $(DEP_CPP_ALAPT) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ALAPTIP.SBR" : $(SOURCE) $(DEP_CPP_ALAPT) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CSZEZON.CPP
DEP_CPP_CSZEZ=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\IDOEREDM.H"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\CSZEZON.OBJ" : $(SOURCE) $(DEP_CPP_CSZEZ) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\CSZEZON.OBJ" : $(SOURCE) $(DEP_CPP_CSZEZ) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\CSZEZON.SBR" : $(SOURCE) $(DEP_CPP_CSZEZ) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\SZEZFELV.CPP
DEP_CPP_SZEZF=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\stdafx.h"\
	".\szezfelv.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\SZEZFELV.OBJ" : $(SOURCE) $(DEP_CPP_SZEZF) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\SZEZFELV.OBJ" : $(SOURCE) $(DEP_CPP_SZEZF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\SZEZFELV.SBR" : $(SOURCE) $(DEP_CPP_SZEZF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\NVZLISTF.CPP
DEP_CPP_NVZLI=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\NEVZLIST.H"\
	".\nvzlistf.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\NVZLISTF.OBJ" : $(SOURCE) $(DEP_CPP_NVZLI) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\NVZLISTF.OBJ" : $(SOURCE) $(DEP_CPP_NVZLI) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\NVZLISTF.SBR" : $(SOURCE) $(DEP_CPP_NVZLI) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\NEVZLIST.CPP
DEP_CPP_NEVZL=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\NEVZLIST.OBJ" : $(SOURCE) $(DEP_CPP_NEVZL) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\NEVZLIST.OBJ" : $(SOURCE) $(DEP_CPP_NEVZL) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\NEVZLIST.SBR" : $(SOURCE) $(DEP_CPP_NEVZL) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\SZEZNDVA.CPP
DEP_CPP_SZEZN=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\IDOEREDM.H"\
	".\IMPGYABL.H"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\RSBEFFEL.H"\
	".\stdafx.h"\
	".\szezndva.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\SZEZNDVA.OBJ" : $(SOURCE) $(DEP_CPP_SZEZN) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\SZEZNDVA.OBJ" : $(SOURCE) $(DEP_CPP_SZEZN) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\SZEZNDVA.SBR" : $(SOURCE) $(DEP_CPP_SZEZN) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\VNEVFELV.CPP
DEP_CPP_VNEVF=\
	".\alaptip.h"\
	".\cnev.h"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\stdafx.h"\
	".\vnevfelv.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\VNEVFELV.OBJ" : $(SOURCE) $(DEP_CPP_VNEVF) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\VNEVFELV.OBJ" : $(SOURCE) $(DEP_CPP_VNEVF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\VNEVFELV.SBR" : $(SOURCE) $(DEP_CPP_VNEVF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\RAJTSBEF.CPP
DEP_CPP_RAJTS=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\IDOEREDM.H"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\RAJTSBEF.OBJ" : $(SOURCE) $(DEP_CPP_RAJTS) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\RAJTSBEF.OBJ" : $(SOURCE) $(DEP_CPP_RAJTS) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\RAJTSBEF.SBR" : $(SOURCE) $(DEP_CPP_RAJTS) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\IDOEREDM.CPP
DEP_CPP_IDOER=\
	".\IDOEREDM.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\IDOEREDM.OBJ" : $(SOURCE) $(DEP_CPP_IDOER) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\IDOEREDM.OBJ" : $(SOURCE) $(DEP_CPP_IDOER) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\IDOEREDM.SBR" : $(SOURCE) $(DEP_CPP_IDOER) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\RSBEFFEL.CPP
DEP_CPP_RSBEF=\
	".\alaptip.h"\
	".\BEFVKIEG.H"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\IDOEREDM.H"\
	".\IMPGYABL.H"\
	".\nevfelv.h"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\RSBEFFEL.H"\
	".\stdafx.h"\
	".\vnevfelv.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\RSBEFFEL.OBJ" : $(SOURCE) $(DEP_CPP_RSBEF) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\RSBEFFEL.OBJ" : $(SOURCE) $(DEP_CPP_RSBEF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\RSBEFFEL.SBR" : $(SOURCE) $(DEP_CPP_RSBEF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\BEFVKIEG.CPP
DEP_CPP_BEFVK=\
	".\alaptip.h"\
	".\BEFVKIEG.H"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\IDOEREDM.H"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\BEFVKIEG.OBJ" : $(SOURCE) $(DEP_CPP_BEFVK) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\BEFVKIEG.OBJ" : $(SOURCE) $(DEP_CPP_BEFVK) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\BEFVKIEG.SBR" : $(SOURCE) $(DEP_CPP_BEFVK) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\PROBA13.CPP
DEP_CPP_PROBA=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CsatlDlg.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\DLGBMDIC.H"\
	".\fgyeelgy.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\lek1doc.h"\
	".\lek1view.h"\
	".\lkmgjvaz.h"\
	".\mainfrm.h"\
	".\NEVZLIST.H"\
	".\pillall.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	".\vbszerep.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\PROBA13.OBJ" : $(SOURCE) $(DEP_CPP_PROBA) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\PROBA13.OBJ" : $(SOURCE) $(DEP_CPP_PROBA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\PROBA13.SBR" : $(SOURCE) $(DEP_CPP_PROBA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\PROBA13.RC
DEP_RSC_PROBA1=\
	".\RES\ICON1.ICO"\
	".\RES\KEZ_CURS.CUR"\
	".\RES\PROBA13.ICO"\
	".\res\proba13.rc2"\
	".\RES\PROBADOC.ICO"\
	".\RES\TOOLBAR.BMP"\
	

"$(INTDIR)\PROBA13.res" : $(SOURCE) $(DEP_RSC_PROBA1) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\LEKERDDL.CPP
DEP_CPP_LEKER=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\fgyeelgy.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\IMPGYABL.H"\
	".\kerermez.h"\
	".\lek1doc.h"\
	".\lekerddl.h"\
	".\lkmgjvaz.h"\
	".\NEVZLIST.H"\
	".\pillall.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\RSBEFFEL.H"\
	".\stdafx.h"\
	".\vbszerep.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\LEKERDDL.OBJ" : $(SOURCE) $(DEP_CPP_LEKER) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\LEKERDDL.OBJ" : $(SOURCE) $(DEP_CPP_LEKER) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\LEKERDDL.SBR" : $(SOURCE) $(DEP_CPP_LEKER) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\LKMGJVAZ.CPP
DEP_CPP_LKMGJ=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\fgyeelgy.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\lkmgjvaz.h"\
	".\NEVZLIST.H"\
	".\pillall.h"\
	".\PONTOZAS.H"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	".\vbszerep.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\LKMGJVAZ.OBJ" : $(SOURCE) $(DEP_CPP_LKMGJ) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\LKMGJVAZ.OBJ" : $(SOURCE) $(DEP_CPP_LKMGJ) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\LKMGJVAZ.SBR" : $(SOURCE) $(DEP_CPP_LKMGJ) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\LEK1DOC.CPP
DEP_CPP_LEK1D=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\fgyeelgy.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\lek1doc.h"\
	".\lkmgjvaz.h"\
	".\NEVZLIST.H"\
	".\pillall.h"\
	".\PONTOZAS.H"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	".\vbszerep.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\LEK1DOC.OBJ" : $(SOURCE) $(DEP_CPP_LEK1D) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\LEK1DOC.OBJ" : $(SOURCE) $(DEP_CPP_LEK1D) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\LEK1DOC.SBR" : $(SOURCE) $(DEP_CPP_LEK1D) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\LEK1VIEW.CPP
DEP_CPP_LEK1V=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\fgyeelgy.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\lek1doc.h"\
	".\lek1view.h"\
	".\lkmgjvaz.h"\
	".\NEVZLIST.H"\
	".\pillall.h"\
	".\PONTOZAS.H"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	".\vbszerep.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\LEK1VIEW.OBJ" : $(SOURCE) $(DEP_CPP_LEK1V) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\LEK1VIEW.OBJ" : $(SOURCE) $(DEP_CPP_LEK1V) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\LEK1VIEW.SBR" : $(SOURCE) $(DEP_CPP_LEK1V) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\DLGBMDIC.CPP
DEP_CPP_DLGBM=\
	".\DLGBMDIC.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\DLGBMDIC.OBJ" : $(SOURCE) $(DEP_CPP_DLGBM) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\DLGBMDIC.OBJ" : $(SOURCE) $(DEP_CPP_DLGBM) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\DLGBMDIC.SBR" : $(SOURCE) $(DEP_CPP_DLGBM) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\KERERMEZ.CPP
DEP_CPP_KERER=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\KERERMEZ.OBJ" : $(SOURCE) $(DEP_CPP_KERER) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\KERERMEZ.OBJ" : $(SOURCE) $(DEP_CPP_KERER) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\KERERMEZ.SBR" : $(SOURCE) $(DEP_CPP_KERER) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\FGYEELGY.CPP
DEP_CPP_FGYEE=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\fgyeelgy.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\FGYEELGY.OBJ" : $(SOURCE) $(DEP_CPP_FGYEE) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\FGYEELGY.OBJ" : $(SOURCE) $(DEP_CPP_FGYEE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\FGYEELGY.SBR" : $(SOURCE) $(DEP_CPP_FGYEE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\HOLVKIV.CPP
DEP_CPP_HOLVK=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\NEVZLIST.H"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\HOLVKIV.OBJ" : $(SOURCE) $(DEP_CPP_HOLVK) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\HOLVKIV.OBJ" : $(SOURCE) $(DEP_CPP_HOLVK) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\HOLVKIV.SBR" : $(SOURCE) $(DEP_CPP_HOLVK) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\PILLALL.CPP
DEP_CPP_PILLA=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\NEVZLIST.H"\
	".\pillall.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	".\vbszerep.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\PILLALL.OBJ" : $(SOURCE) $(DEP_CPP_PILLA) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\PILLALL.OBJ" : $(SOURCE) $(DEP_CPP_PILLA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\PILLALL.SBR" : $(SOURCE) $(DEP_CPP_PILLA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\VBSZEREP.CPP
DEP_CPP_VBSZE=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\cvnev.h"\
	".\holvkiv.h"\
	".\IDOEREDM.H"\
	".\kerermez.h"\
	".\NEVZLIST.H"\
	".\pillall.h"\
	".\PONTOZAS.H"\
	".\proba13.h"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	".\vbszerep.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\VBSZEREP.OBJ" : $(SOURCE) $(DEP_CPP_VBSZE) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\VBSZEREP.OBJ" : $(SOURCE) $(DEP_CPP_VBSZE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\VBSZEREP.SBR" : $(SOURCE) $(DEP_CPP_VBSZE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\IMPGYABL.CPP
DEP_CPP_IMPGY=\
	".\IMPGYABL.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\IMPGYABL.OBJ" : $(SOURCE) $(DEP_CPP_IMPGY) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# PROP Exclude_From_Build 0
# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\IMPGYABL.OBJ" : $(SOURCE) $(DEP_CPP_IMPGY) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\IMPGYABL.SBR" : $(SOURCE) $(DEP_CPP_IMPGY) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\PROBA13.DEF

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ensqlcam.cpp
DEP_CPP_ENSQL=\
	".\ensqlcam.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\ensqlcam.obj" : $(SOURCE) $(DEP_CPP_ENSQL) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\ensqlcam.obj" : $(SOURCE) $(DEP_CPP_ENSQL) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ensqlcam.sbr" : $(SOURCE) $(DEP_CPP_ENSQL) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\DBNEV.PC

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\DBNEV.PC
InputName=DBNEV

"$(ProjDir)\$(InputName).cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                                                        CHAR_MAP=STRING include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbnev.cpp
DEP_CPP_DBNEV=\
	".\alaptip.h"\
	".\cnev.h"\
	".\ensqlcam.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\dbnev.obj" : $(SOURCE) $(DEP_CPP_DBNEV) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\dbnev.obj" : $(SOURCE) $(DEP_CPP_DBNEV) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\dbnev.sbr" : $(SOURCE) $(DEP_CPP_DBNEV) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbvnev.pc

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\dbvnev.pc
InputName=dbvnev

"$(ProjDir)\$(InputName).cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                                                        CHAR_MAP=STRING include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbvnev.cpp
DEP_CPP_DBVNE=\
	".\alaptip.h"\
	".\cvnev.h"\
	".\ensqlcam.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\dbvnev.obj" : $(SOURCE) $(DEP_CPP_DBVNE) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\dbvnev.obj" : $(SOURCE) $(DEP_CPP_DBVNE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\dbvnev.sbr" : $(SOURCE) $(DEP_CPP_DBVNE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CSATLDLG.CPP
DEP_CPP_CSATL=\
	".\alaptip.h"\
	".\csatlako.h"\
	".\CsatlDlg.h"\
	".\ensqlcam.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\CSATLDLG.OBJ" : $(SOURCE) $(DEP_CPP_CSATL) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\CSATLDLG.OBJ" : $(SOURCE) $(DEP_CPP_CSATL) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\CSATLDLG.SBR" : $(SOURCE) $(DEP_CPP_CSATL) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CSATLAKO.PC

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\CSATLAKO.PC
InputName=CSATLAKO

"$(ProjDir)\$(InputName).CPP" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                                                        CHAR_MAP=STRING include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\csatlako.cpp
DEP_CPP_CSATLA=\
	".\ensqlcam.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

# ADD CPP /YX"STDAFX.H"

"$(INTDIR)\csatlako.obj" : $(SOURCE) $(DEP_CPP_CSATLA) "$(INTDIR)"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /YX"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\csatlako.obj" : $(SOURCE) $(DEP_CPP_CSATLA) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\csatlako.sbr" : $(SOURCE) $(DEP_CPP_CSATLA) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbglobal.pc

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\dbglobal.pc
InputName=dbglobal

"$(ProjDir)\$(InputName).cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                   CHAR_MAP=STRING SQLCHECK=SEMANTICS USERID=scott/tiger\
                                   include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbglobal.cpp
DEP_CPP_DBGLO=\
	".\alaptip.h"\
	".\dbglobal.h"\
	".\ensqlcam.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\dbglobal.obj" : $(SOURCE) $(DEP_CPP_DBGLO) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\dbglobal.obj" : $(SOURCE) $(DEP_CPP_DBGLO) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\dbglobal.sbr" : $(SOURCE) $(DEP_CPP_DBGLO) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=e:\PROC\PRO80\LIB\MSVC\SQLLIB80.LIB

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbpontoz.pc

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\dbpontoz.pc
InputName=dbpontoz

"$(ProjDir)\$(InputName).cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                                                        CHAR_MAP=STRING include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\pontozas.cpp
DEP_CPP_PONTO=\
	".\PONTOZAS.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\pontozas.obj" : $(SOURCE) $(DEP_CPP_PONTO) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\pontozas.obj" : $(SOURCE) $(DEP_CPP_PONTO) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\pontozas.sbr" : $(SOURCE) $(DEP_CPP_PONTO) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbpontoz.cpp
DEP_CPP_DBPON=\
	".\ensqlcam.h"\
	".\PONTOZAS.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\dbpontoz.obj" : $(SOURCE) $(DEP_CPP_DBPON) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\dbpontoz.obj" : $(SOURCE) $(DEP_CPP_DBPON) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\dbpontoz.sbr" : $(SOURCE) $(DEP_CPP_DBPON) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\DBSZEZON.PC

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\DBSZEZON.PC
InputName=DBSZEZON

"$(ProjDir)\$(InputName).cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                                                        CHAR_MAP=STRING include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\DBSZEZON.cpp
DEP_CPP_DBSZE=\
	".\alaptip.h"\
	".\cnev.h"\
	".\CSZEZON.H"\
	".\ensqlcam.h"\
	".\PONTOZAS.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\DBSZEZON.obj" : $(SOURCE) $(DEP_CPP_DBSZE) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# ADD CPP /YX"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/PROBA13.pch" /YX"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\DBSZEZON.obj" : $(SOURCE) $(DEP_CPP_DBSZE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\DBSZEZON.sbr" : $(SOURCE) $(DEP_CPP_DBSZE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbnvzlst.pc

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\dbnvzlst.pc
InputName=dbnvzlst

"$(ProjDir)\$(InputName).cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                                                        CHAR_MAP=STRING SQLCHECK=SEMANTICS USERID=scott/tiger\
                                                                        include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbnvzlst.cpp
DEP_CPP_DBNVZ=\
	".\alaptip.h"\
	".\ensqlcam.h"\
	".\NEVZLIST.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\dbnvzlst.obj" : $(SOURCE) $(DEP_CPP_DBNVZ) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\dbnvzlst.obj" : $(SOURCE) $(DEP_CPP_DBNVZ) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\dbnvzlst.sbr" : $(SOURCE) $(DEP_CPP_DBNVZ) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbrsbef.pc

!IF  "$(CFG)" == "PROBA13 - Win32 Release"

!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# Begin Custom Build
ProjDir=.
InputPath=.\dbrsbef.pc
InputName=dbrsbef

"$(ProjDir)\$(InputName).cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   e:\proc\bin\proc $(ProjDir)\$(InputName).PC CONFIG=procconf.h\
                                   CHAR_MAP=STRING SQLCHECK=SEMANTICS USERID=scott/tiger\
                                   include="$(MSDEVDIR)\mfc\include" include="$(MSDEVDIR)\include"

# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dbrsbef.cpp
DEP_CPP_DBRSB=\
	".\alaptip.h"\
	".\cnev.h"\
	".\cvnev.h"\
	".\ensqlcam.h"\
	".\IDOEREDM.H"\
	".\NEVZLIST.H"\
	".\RAJTSBEF.H"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "PROBA13 - Win32 Release"


"$(INTDIR)\dbrsbef.obj" : $(SOURCE) $(DEP_CPP_DBRSB) "$(INTDIR)"\
 "$(INTDIR)\PROBA13.pch"
   $(CPP) /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/PROBA13.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "PROBA13 - Win32 Debug"

# SUBTRACT CPP /YX /Yc /Yu

BuildCmds= \
	$(CPP) /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\dbrsbef.obj" : $(SOURCE) $(DEP_CPP_DBRSB) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\dbrsbef.sbr" : $(SOURCE) $(DEP_CPP_DBRSB) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
