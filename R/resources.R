## Copyright (C) 2020 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.



## Constants
FN_FTAB_BASE<-"ftable.base.csv"
FN_FTAB_PP<-"ftable.pp.csv"
FN_PP_OUT_PREF<-"PP.filetable"
FN_FTAB_STATE<-"ftable_state.csv"
FN_FTAB_DEF_OUT<-"ftable.csv"
FN_CMP_L<-"compounds.csv"
FN_LOC_SETID <-"setid.csv"
FN_COMP_TAB<-"comprehensive.csv"
FN_SPEC<-"specdata.rds"
MODEMAP<-list(pH="MpHp_mass",
              mH="MmHm_mass",
              pNH4="MpNH4_mass",
              pNa="MpNa_mass")

TAG_DEF<-"unspecified"
TAG_DEF_DESC<-"Case"
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
                "tagsInp")

REST_TAB<-c("mzMLWork")


GUI_TAB_TITLE<-c(conf="Config",
                 gen="Spectra Extraction and Automatic QA",
                 pres="Prescreening")


GUI_SIDE_TITLE<-GUI_TAB_TITLE
GUI_SIDE_TITLE[["gen"]]<-"Spectra Extraction"

CHR_GRAM_X="retention time [min]"
CHR_GRAM_Y="intensity"

KEY_GLYPH='rect'

PLOT_MS1_LEG_TIT<-"peak retention time (MS1)"
PLOT_MS2_LEG_TIT<-"peak retention time (MS2)"


MS2_1ST_N<-5


