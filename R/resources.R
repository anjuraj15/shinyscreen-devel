## Constants
FN_FTAB_BASE<-"ftable.base.csv"
FN_FTAB_PP<-"ftable.pp.csv"
FN_PP_OUT_PREF<-"PP.filetable"
FN_FTAB<-"ftable.csv"
FN_CMP_L<-"compounds.csv"
FN_LOC_SETID <-"setid.csv"
FN_COMP_TAB<-"comprehensive.csv"
MODEMAP<-list(pH="MpHp_mass",
              mH="MmHm_mass",
              pNH4="MpNH4_mass",
              pNa="MpNa_mass")

DEFAULT_RT_RANGE=c(NA,NA)

QANAMES <- c("MS1","MS2","Alignment","AboveNoise")
PLOT_DEF_TAGS<-NA
PLOT_DEF_SET<-NA

CEX<-0.75
RT_DIGITS=2
M_DIGITS=4
PAL="Dark2"

REST_TXT_INP<-c("fnTgtL",
                "fnUnkL",
                "fnSetId",
                "tagsInp",
                "confFileTabBase",
                "confFileTabProcInp",
                "confResFileTab")


GUI_TAB_TITLE<-c(conf="Config",
                 gen="Spectra Extraction and Automatic QA",
                 pres="Prescreening")


GUI_SIDE_TITLE<-GUI_TAB_TITLE
GUI_SIDE_TITLE[["gen"]]<-"Spectra Extraction"
