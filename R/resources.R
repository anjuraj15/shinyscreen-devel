## Copyright (C) 2020,2021,2022,2023 by University of Luxembourg

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
CONF = list(data=NA_character_,
             project=getwd(),
             compounds=list(lists=list(),
                            sets=NA_character_))

## Constants
FN_ENVOPTS="envopts.rds"
FN_SUMM_BASE="summ.base.csv"
FN_SUMM_PP="summ.pp.csv"
FN_PP_OUT_PREF="PP.filetable"
FN_SUMM_STATE="summ_state.csv"
FN_SUMM = "summ.csv"
FN_SUMM_DEF_OUT = FN_SUMM
FN_CMP_L="compounds.csv"
FN_COMP_TAB="comprehensive.csv"
FN_SPEC="specdata.rds"
FN_CONF = "conf-state.yaml"
FN_EXTR_STATE = "state_after_extraction.rds"
FN_STATE = "current-state.rds"
FN_GUI_STATE = "gui.rds"
.envp = new.env(parent = emptyenv())
data(adducts,package = "enviPat", envir = .envp)
data(isotopes,package = "enviPat", envir = .envp)
ADDUCTS = dtable(.envp$adducts)
ISOTOPES = dtable(.envp$isotopes)
.envp = NULL
ADDUCTMAP = ADDUCTS$Name
ADDUCTS$Name = the_ifelse(ADDUCTS$Charge>0,paste0("[",ADDUCTS$Name,"]+"),paste0("[",ADDUCTS$Name,"]-"))
## names(ADDUCTMAP) <- apply(ADDUCTS,1,function(row) {
##     nm <- row[["Name"]]
##     sgn <- row[["Charge"]]
##     suff <- if (sgn > 0) "+" else if (sgn < 0) "-" else ""
##     paste0("[",nm,"]",suff)
## })
## ADDUCTS$Name <- names(ADDUCTMAP)
DISP_ADDUCTS = ADDUCTS$Name
TAG_NA = "::UNSET::"
SET_NA = "::UNSET::"
TAG_DEF = TAG_NA
TAG_DEF_DESC="Case"
DEFAULT_RT_RANGE=c(NA,NA)
DEFAULT_INT_RANGE=c(NA,NA)
DEFAULT_MZ_RANGE=c(NA,NA)

## QANAMES = c("MS1","MS2","Alignment","AboveNoise")
PLOT_DEF_TAGS=NA
PLOT_DEF_SET=NA

CEX=0.75
RT_DIGITS=2
M_DIGITS=4
PAL="Dark2"

REST_TAB=c("mzml")


GUI_TAB_TITLE=c(conf="Config",
                 gen="Spectra Extraction and Automatic QA",
                 pres="Prescreening",
                 log="Log")


GUI_SIDE_TITLE=GUI_TAB_TITLE
GUI_SIDE_TITLE[["gen"]]="Spectra Extraction"

CHR_GRAM_X="retention time [min]"
CHR_GRAM_Y="intensity"

KEY_GLYPH='rect'

PLOT_MS1_LEG_TIT="peak retention time (MS1)"
PLOT_MS2_LEG_TIT="peak retention time (MS2)"


MS2_1ST_N=5

EXTR_MS2_DIR="MS2"
EXTR_MS2_FLAG=file.path(EXTR_MS2_DIR,'.ms2.DONE')


SUMM_CHK_NONE='NONE'

SUMM_CHK_AUTO='AUTO'

SUMM_CHK_MANL='MANUAL'


MS1_ERR_COARSE="0.5 Da"                     # Da
MS1_ERR_FINE= "5 ppm"                       # ppm
EIC_ERR = "0.001 Da"                       # Da
RT_EXTR_ERR="0.5 min"                       # min
RT_SHIFT_ERR = "0.5 min"               # min

MS1_INT_THOLD = 1e5
MS2_INT_THOLD = 2500.

MS1_SN_FAC = 3.0


## Shiny objects

NUM_INP_WIDTH=20
NUM_INP_HEIGHT="5%"



## Possible compound list fields
EMPTY_CMPD_LIST = dtable(ID=character(),
                         SMILES=character(),
                         Name=character(),
                         Formula=character(),
                         RT=numeric(),
                         mz=numeric(),
                         known=character(),
                         set=character(),
                         ORIG=character())
COMP_LIST_COLS = c("ID","Name","SMILES","Formula","RT","mz")
## Comprehensive table properties
COMP_NAME_MAP = list(RT="rt")
COMP_NAME_FIRST = c("ID","mz","rt","adduct","tag","set","Name","known","SMILES","Formula","file")



## Trivial data table
EMPTY_MZML = dtable(file=character(0),
                     tag=character(0),
                     adduct=character(0),
                     set=character(0))

FN_DATA_TAB = "data-files.csv"


## Default number of concurrent workers
NO_WORKERS = 2

## Input parameters for prescreening.
CONF_PRES_NUM = c("ms1_int_thresh","ms2_int_thresh","s2n")
CONF_PRES_TU = c("ret_time_shift_tol")


## Prescreening columns
QA_FLAGS = c("qa_pass",
              "qa_ms1_exists",
              "qa_ms2_exists",
              "qa_ms1_good_int",
              "qa_ms1_above_noise",
              "qa_ms2_near",
              "qa_ms2_good_int")

QABOX_VALS = c("MS1 exists"="qa_ms1_exists",
                "MS1 good intensity"="qa_ms1_good_int",
                "MS1 above noise"="qa_ms1_above_noise",
                "MS2 exists"="qa_ms2_exists",
                "MS2 good intensity"="qa_ms2_good_int",
                "MS2 no RT shift"="qa_ms2_near")


QA_NUM_REAL = c("ms1_int","ms1_rt","ms1_mean")

QA_NUM_INT = c("ms2_sel","ms1_rt_ind")

QA_COLS = c(QA_FLAGS,QA_NUM_REAL,QA_NUM_INT)

## MS2 spectral table columns
MS2_SPEC_COLS = c("adduct","tag","ID","CE","rt","file","spec","ms2_max_int")

## MS1 spectral table columns
MS1_SPEC_COLS = c("adduct","tag","ID","eicMS1","ms1_int","ms1_rt","ms1_mean")



## Default secondary indexing in the summary table
DEF_INDEX_SUMM = c("set", "-qa_pass", "-ms1_int", "adduct","-mz")

## Top-level directory to store the figures
FIG_TOPDIR = "figures"

REP_TOPDIR = "report"
## Figure filter
FIG_DEF_FILTER = ""

FIG_DEF_SUBSET = c("set","adduct","ID")


REPORT_AUTHOR = "Anonymous"
REPORT_TITLE = "Plots of EICs and MS2 Spectra"

## Select the most fundamental group of entries. Within this group,
## each ID is unique.
BASE_KEY = "precid"#c("adduct","tag","ID")
BASE_KEY_MS2 = c("precid","ce","scan")#c(BASE_KEY,"CE","scan")

FIG_DEF_CONF =list(grouping=list(group="adduct",
                                  plot="ID",
                                  label="tag"))


## Summary table properties.
SUMM_COLS=c("set",BASE_KEY_MS2,"mz","ms1_rt", "ms1_int", "ms2_rt", "ms2_int",
            "ms1_mean","ms2_sel",QA_FLAGS,"Name", "SMILES", "Formula", "known","Comments","file")

SUMM_KEY = c("set","ID","adduct","tag","precid","catid","scan")

PLOT_FEATURES = c("adduct",
                   "tag",
                   "ID")

## Empty summary table.
EMPTY_SUMM = data.table::data.table(set=character(0),
                                    adduct=character(0),
                                    tag=character(0),
                                    ID=character(0),
                                    CE=character(0),
                                    scan=character(0),
                                    mz=numeric(0),
                                    ms1_rt=numeric(0),
                                    ms1_int=numeric(0),
                                    ms2_rt=numeric(0),
                                    ms2_int=numeric(0),
                                    ms1_mean=numeric(0),
                                    ms2_sel=logical(0),
                                    qa_pass=logical(0),
                                    qa_ms1_exists=logical(0),
                                    qa_ms2_exists=logical(0),
                                    qa_ms1_good_int=logical(0),
                                    qa_ms1_above_noise=logical(0),
                                    qa_ms2_near=logical(0),
                                    qa_ms2_good_int=logical(0),
                                    Name=character(0),
                                    SMILES=character(0),
                                    Formula=character(0),
                                    known=character(0),
                                    Comments=character(0),
                                    file=character(0))

## Default sorting keys of spectra in the summary table
DEF_KEY_SUMM = c(BASE_KEY_MS2,"scan")


SUBSET_VALS = c(IGNORE="ignore",
                 GOOD="select good",
                 BAD="select bad")




## Empty comprehensive table.
EMPTY_COMP_TAB = dtable(ID=character(),
                         mz=numeric(),
                         rt=numeric(),
                         adduct=character(),
                         tag=character(),
                         set=character(),
                         Name=character(),
                         known=character(),
                         SMILES=character(),
                         Formula=character(),
                         file=character())


DEF_CONF_MISSING_PCS = "do_nothing"


## Significant digits for output
SIGNF_I = 3
SIGNF_MZ = 7
SIGNF_RT = 4


## Symbols to display T, or F
SYM_YES="\U002713"
SYM_NO="\U00274C"


CMPD_LIST_PATT = "((*.csv)|(*.csv.gz))$"
SET_LIST_PATT = CMPD_LIST_PATT
DFILES_LIST_PATT = ".*\\.mz[Mm][Ll]$"

CINDEX_BY = c("set","ID","adduct","tag")
CINDEX_COLS = c("mz", "ms1_rt","Name","qlt_ms1","qlt_ms2")
ARRANGE_CHOICES = c(nothing="nothing",
                     quality="quality",
                     set="set",
                     adduct="adduct",
                     mz="mz",
                     rt="rt",
                     id="ID")

PLOT_EIC_ASPECT = 0.75

## Each set should have it's different colourscheme, because of the
## possibility that each set connects to a different collection of
## files.
COLRDATA_KEY = "set"

## METFRAG

METFRAG_ADDUCTS = c("[M+H]+","[M+NH4]+","[M+Na]+","[M+K]+",
                    "[M+CH3OH+H]+","[M+ACN+H]+","[M+ACN+Na]+","[M+2ACN+H]+",
                    "[M-H]-","[M+Cl]-","[M+HCOO]-","[M+CH3COO]-","[M+]+","[M-]-")
METFRAG_ADDUCT_SWITCHES = c("[M+H]+"=1L,
                            "[M+NH4]+"=18L,
                            "[M+Na]+"=23L,
                            "[M+K]+"=39L,
                            "[M+CH3OH+H]+"=33L,
                            "[M+ACN+H]+"=42L,
                            "[M+ACN+Na]+"=64L,
                            "[M+2ACN+H]+"=83L,
                            "[M-H]-"=-1L,
                            "[M+Cl]-"=35L,
                            "[M+HCOO]-"=45L,
                            "[M+CH3COO]-"=49L,
                            "[M+]+"=0L,
                            "[M-]-"=0L)
                            
                            
                            
METFRAG_WRITER_CHOICES = c("CSV","PSV","XLS","ExtendedXLS","ExtendedFragmentXLS")
METFRAG_DEFAULT_WRITER = "CSV"
METFRAG_LOCAL_DATABASE_TYPE = c("LocalSDF","LocalPSV","LocalCSV")
METFRAG_REMOTE_DATABASE_TYPE = c("KEGG","PubChem","ExtendedPubChem")
METFRAG_DATABASE_TYPE = c(METFRAG_REMOTE_DATABASE_TYPE, METFRAG_LOCAL_DATABASE_TYPE)
METFRAG_DEFAULT_REMOTE_DATABASE_TYPE = "PubChem"
METFRAG_DEFAULT_DATABASE_TYPE = "LocalCSV"
METFRAG_PREPFLT_CHOICES = c("UnconnectedCompoundFilter","IsotopeFilter")
METFRAG_PREPFLT_DEFAULT = c("UnconnectedCompoundFilter","IsotopeFilter")
METFRAG_POSTPFLT_CHOICES = c("InChIKeyFilter")
METFRAG_POSTPFLT_DEFAULT = c("InChIKeyFilter")
METFRAG_DEFAULT_SCORES = list(FragmenterScore=1.0,OfflineIndividualMoNAScore=1.0)
METFRAG_INTRINSIC_SCORES = list("FragmenterScore",
                                "OfflineIndividualMoNAScore",
                                "AutomatedPeakFingerprintAnnotationScore",
                                "AutomatedLossFingerprintAnnotationScore")

## METFRAG_DEFAULT_WEIGHTS = "1,1"
METFRAG_DEFAULT_MAX_TREE_DEPTH = 2
METFRAG_SAMPLE_NAME = "sample"
METFRAG_DEFAULT_ABSMASSDEV = 1.E-3
METFRAG_DEFAULT_RELMASSDEV = 5
METFRAG_DB_SEARCH_RELDEV = 5

METFRAG_RESULT_READF = list(csv = function(file,...) data.table::fread(file=file,...),
                            xml = function(file,...) readxl::read_excel(path=file,...))

METFRAG_DEFAULT_PROC = 1L


## DATA MODEL
DB_CATALOGUE_KEY = c("set","tag","adduct","ID")
