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




## Config defaults
CONF <- list(data=NA_character_,
             project=getwd(),
             compounds=list(known=NA_character_,
                            unknown=NA_character_,
                            sets=NA_character_))

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
FN_CONF <- "conf-state.yaml"
ADDUCTMAP <- RChemMass:::adducts$Name
names(ADDUCTMAP) <- apply(RChemMass:::adducts,1,function(row) {
    nm <- row[["Name"]]
    sgn <- row[["Charge"]]
    suff <- if (sgn > 0) "+" else if (sgn < 0) "-" else ""
    paste0("[",nm,"]",suff)
})

DISP_ADDUCTMAP <- c(c("UNSET"="UNSET_ADDUCT_ERROR"),ADDUCTMAP)
TAG_NA <- "::UNSET::"
SET_NA <- "::UNSET::"
TAG_DEF <- TAG_NA
TAG_DEF_DESC<-"Case"
DEFAULT_RT_RANGE=c(NA,NA)
DEFAULT_INT_RANGE=c(NA,NA)
DEFAULT_MZ_RANGE=c(NA,NA)

QANAMES <- c("MS1","MS2","Alignment","AboveNoise")
PLOT_DEF_TAGS<-NA
PLOT_DEF_SET<-NA

CEX<-0.75
RT_DIGITS=2
M_DIGITS=4
PAL="Dark2"

## REST_TXT_INP<-c("fnKnownL",
##                 "fnUnkL",
##                 "fnSetId",
##                 "tagsInp")

REST_TAB<-c("mzml")


GUI_TAB_TITLE<-c(conf="Config",
                 gen="Spectra Extraction and Automatic QA",
                 pres="Prescreening",
                 log="Log")


GUI_SIDE_TITLE<-GUI_TAB_TITLE
GUI_SIDE_TITLE[["gen"]]<-"Spectra Extraction"

CHR_GRAM_X="retention time [min]"
CHR_GRAM_Y="intensity"

KEY_GLYPH='rect'

PLOT_MS1_LEG_TIT<-"peak retention time (MS1)"
PLOT_MS2_LEG_TIT<-"peak retention time (MS2)"


MS2_1ST_N<-5

EXTR_MS2_DIR<-"MS2"
EXTR_MS2_FLAG<-file.path(EXTR_MS2_DIR,'.ms2.DONE')


FTAB_CHK_NONE<-'NONE'

FTAB_CHK_AUTO<-'AUTO'

FTAB_CHK_MANL<-'MANUAL'


MS1_ERR_COARSE<-0.5                     # Da
MS1_ERR_FINE<- 5                       # ppm
EIC_ERR <- 0.001                       # Da
RT_EXTR_ERR<-0.5                       # min
RT_SHIFT_ERR <- 0.5               # min

MS1_INT_THOLD <- 1e5
MS2_INT_THOLD <- 5000.

MS1_SN_FAC <- 3.0


## Shiny objects

NUM_INP_WIDTH="15%"

## Comprehensive table properties
COMP_NAME_MAP <- list(RT="rt")
## COMP_NAMES <-c("ID","mz","rt","adduct","set","origin","Name","SMILES")
COMP_NAME_FIRST <- c("ID","mz","rt","adduct","tag","set","Name","SMILES","Files","wd")

## File table properties
FTAB_KEY=c("set","tag","mz")
FTAB_NAMES=c("ID", "mz", "rt", "tag", "adduct", "set", "Name", "SMILES", "Files" , "wd","origin")


EMPTY_UNKNOWN <- dtable(ID=character(0),mz=numeric(0),RT=numeric(0),Name=character(0),CAS=character(0))
EMPTY_KNOWN <- dtable(ID=character(0),SMILES=character(0),RT=numeric(0),Name=character(0),CAS=character(0))

## Trivial data table
EMPTY_MZML <- dtable(Files=character(0),
                     tag=character(0),
                     adduct=character(0),
                     set=character(0))


FN_DATA_TAB <- "data-files.csv"


## Default number of concurrent workers
NO_WORKERS <- 2
