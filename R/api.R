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


##' @export
run <- function(fn_conf) {
    conf <- read_conf(fn_conf)
    dir.create(conf$project,
               showWarnings = F,
               recursive = T)

    m <- new_state(conf=conf,
                   GUI=F)    
    withr::with_dir(new=conf$project,code = run_in_dir(m))
    return()
}


##' @export
run_in_dir <- function(m) {
    m <- load_inputs(m)
    m <- mk_comp_tab(m)
    m <- gen_base_ftab(m)
    invisible(m)
    
}

##' @export
gen_base_ftab <- function(m) {
    files <- add_wd_to_mzml(df=m$input$tab$mzml,wdir=m$conf$project)
    df <- gen_sup_ftab(files,m$out$tab$comp)
    tab2file(df,file.path(m$conf$project,FN_FTAB_BASE))
    m$out$tab$ftab <- df
    m
}

##' @export
load_compound_input <- function(m) {

    m$input$tab$known <- if (shiny::isTruthy(m$conf$compounds$known))
                             file2tab(m$conf$compounds$known) else EMPTY_KNOWN
    m$input$tab$unknown <- if (shiny::isTruthy(m$conf$compounds$unknown))
                               file2tab(m$conf$compounds$unknown) else EMPTY_UNKNOWN
    
    m$input$tab$setid <- read_setid(m$conf$compounds$sets,
                                    m$input$tab$known,
                                    m$input$tab$unknown)
    m
}

load_data_input <- function(m) {
    m$input$tab$mzml <- file2tab(m$conf$data)
    m

}

##' @export
load_inputs <- function(m) {
    m <- load_compound_input(m)
    m <- load_data_input(m)
    m
}

##' @export
mk_comp_tab <- function(m) {
    message("Started assembling the lists of knowns and unknowns into the `comprehensive' table.")
    setid <- m$input$tab$setid
    setkey(setid,set)
    mzml<- m$input$tab$mzml
    setkey(mzml,set)
    unk<-m$input$tab$unknown
    setkey(unk,ID)
    known<-m$input$tab$known
    setkey(known,ID)
    mzml[,`:=`(wd=sapply(Files,add_wd_to_mzml,m$conf$project))]
    assert(xor(nrow(unk)==0,nrow(known)==0),msg="No compound lists have been provided. At least one of the known, or unknown compound lists is required.")
    message("Begin generation of comp table.")
    ## knowns
    setidKnown<- mzml[setid[origin %in% "known"],.(tag,mode,ID,set,Files,wd),on="set",allow.cartesian=T]
    tab2file(tab=setidKnown,file="setidKnown.csv")
    compKnown <- known[setidKnown,on=c("ID"),allow.cartesian=T]
    setkey(compKnown,set,ID)
    tab2file(tab=compKnown,file="compKnown.csv")
    compKnown[,`:=`(mz=mapply(get_mz_from_smiles,SMILES,mode,USE.NAMES = F))]
    message("Generation of comp table: knowns done.")
    ## unknows
    setidUnk<-mzml[setid[origin %in% "unknown"],.(tag,mode,ID,set,Files,wd),on="set",allow.cartesian=T]
    compUnk <- unk[setidUnk,on="ID"]
    message("Generation of comp table: unknowns done.")
    df<-rbindlist(l=list(compKnown, compUnk),fill = T)
    setnames(df,names(COMP_NAME_MAP),
             function(o) COMP_NAME_MAP[[o]])
    setcolorder(df,COMP_NAME_FIRST)
    fn_out <- file.path(m$conf$project,FN_COMP_TAB)
    
    tab2file(tab=df,file=fn_out)
    message("Generation of comp table finished.")
    setkeyv(df,c("set","tag","mz"))
    m$out$tab$comp <- df
    m
}

##' @export
read_conf <- function(fn) {
    yaml::yaml.load_file(fn)
}
## read_conf <- function(fn_conf) {
##     assert(isThingFile(fn_conf),msg=paste("Unable to read the configuration file:", fn_conf))
##     conf <- yaml::yaml.load_file(fn_conf)
##     conf <- vrfy_conf(conf)
##     conf
## }



verify_compounds <- function(conf) {
    ## * Existence of input files

    fn_cmpd_known <- conf$compounds$known
    fn_cmpd_unk <- conf$compounds$unknown
    fn_cmpd_sets <- conf$compounds$sets

    ## ** Compound lists and sets

    assert(isThingFile(fn_cmpd_sets),
                            msg=paste("Cannot find the compound sets file:",fn_cmpd_sets))
    
    ## if (!is.null(fn_cmpd_known)) assert(isThingFile(fn_cmpd_known),
    ##                                                          msg=paste("Cannot find known compounds file:",fn_cmpd_known))        
    ## if (!is.null(fn_cmpd_unk)) assert(isThingFile(fn_cmpd_unk),
    ##                                                    msg=paste("Cannot find unknown compounds file:",fn_cmpd_unk))

    assert(xor(!isThingFile(fn_cmpd_known),!isThingFile(fn_cmpd_unk)),msg=paste("Both known and unknown compounds lists are missing."))

    ## * Data files
    df_sets <- file2tab(fn_cmpd_sets)
    all_sets<-unique(df_sets$set)

    ## ** Knowns
    if (isThingFile(fn_cmpd_unk)) {
        df_k <- file2tab(fn_cmpd_known)
        are_knowns_OK <- shiny::isTruthy(vald_comp_tab(df_k,fn_cmpd_known, checkSMILES=T, checkNames=T))
        assert(are_knowns_OK,msg='Aborted because known compounds table contained errors.')
    }

    ## ** Unknowns
    if (isThingFile(fn_cmpd_unk)) {
        df_u <- file2tab(fn_cmpd_unk)
        are_unknowns_OK <- shiny::isTruthy(vald_comp_tab(df_u,fn_cmpd_unk, checkSMILES=F, checkMz=T))
        assert(are_unknowns_OK, msg='Aborted because unknown compounds table contained errors.')
    }
    
    return(list(conf=conf,all_sets=all_sets))
}

verify_data_df <- function(mzml,all_sets) {
    no_files <- which(mzml[,!file.exists(Files)])
    no_modes <- which(mzml[,!(mode %in% names(MODEMAP))])
    no_sets <- which(mzml[,!(set %in% all_sets)])
    assert(length(no_files)==0,msg = paste("Non-existent data files at rows:",paste(no_files,collapse = ',')))
    assert(length(no_modes)==0,msg = paste("Unrecognised modes at rows:",paste(no_modes,collapse = ',')))
    assert(length(no_sets)==0,msg = paste("Unknown sets at rows:",paste(no_sets,collapse = ',')))
}

verify_data <- function(conf,all_sets) {
    ## * Existence of input files
    fn_data <- conf$data
    assert(isThingFile(fn_data),msg=paste("Data table does not exist:",fn_data))
    mzml <- file2tab(fn_data)
    verify_data_df(mzml=mzml,all_sets)
    return(conf)
}



##' @export
vrfy_conf <- function(conf) {
    ## * Existence of input files

    z <- verify_compounds(conf)
    conf <- z$conf
    all_sets <- z$all_sets
 
    verify_data(conf=conf,all_sets=all_sets)
    return(conf)
}






