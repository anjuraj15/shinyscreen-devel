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
    
    if (shiny::isTruthy(m$conf$compounds$known)) m$input$tab$known <- file2tab(m$conf$compounds$known)
    if (shiny::isTruthy(m$conf$compounds$unknown)) m$input$tab$unknown <- file2tab(m$conf$compounds$unknown)
    
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
    setkey(setid,set,ID)
    mzml<- m$input$tab$mzml
    setkey(mzml,set)
    
    setkey(m$input$tab$unknown,ID)
    setkey(m$input$tab$known,ID)
    unk<-m$input$tab$unknown
    known<-m$input$tab$known
    
    assert(xor(nrow(unk)==0,nrow(known)==0),msg="No compound lists have been provided. At least one of the known, or unknown compound lists is required.")
    message("Begin generation of comp table.")
    ## knowns
    setidKnown<- merge(mzml[,.(mode,tag,set)],setid[origin=="known",],allow.cartesian = T)
    compKnown <- setidKnown[known,on="ID"]
    compKnown[,`:=`(mz=mapply(get_mz_from_smiles,SMILES,mode,USE.NAMES = F))]
    message("Generation of comp table: knowns done.")

    ## unknows
    setidUnk<-merge(mzml[,.(mode,tag,set)],setid[origin=="unknown",],allow.cartesian = T)
    compUnk <- setidUnk[unk,on="ID"]
    message("Generation of comp table: unknowns done.")
    df<-rbindlist(l=list(compKnown, compUnk),fill = T)
    setnames(df,names(COMP_NAME_MAP),
             function(o) COMP_NAME_MAP[[o]])
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

    fn_data <- conf$data
    assert(isThingFile(fn_data),msg=paste("Data table does not exist:",fn_data))
    mzml <- file2tab(fn_data)
    
    no_files <- which(mzml[,!file.exists(Files)])
    no_modes <- which(mzml[,!(mode %in% names(MODEMAP))])
    no_sets <- which(mzml[,!(set %in% all_sets)])
    assert(length(no_files)==0,msg = paste("Non-existent data files at rows:",paste(no_files,collapse = ','), "of",fn_data))
    assert(length(no_modes)==0,msg = paste("Unrecognised modes at rows:",paste(no_modes,collapse = ','), "of", fn_data))
    assert(length(no_sets)==0,msg = paste("Unknown sets at rows:",paste(no_sets,collapse = ','),"of", fn_data))

    df_k <- file2tab(fn_cmpd_known)
    
    are_knowns_OK <- shiny::isTruthy(vald_comp_tab(df_k,fn_cmpd_known, checkSMILES=T, checkNames=T))
    assert(are_knowns_OK,msg='Aborted because known compounds table contained errors.')

    ## ** Unknowns
    if (!is.null(fn_cmpd_unk)) {
        df_u <- file2tab(fn_cmpd_unk)
        are_unknowns_OK <- shiny::isTruthy(vald_comp_tab(df_u,fn_cmpd_unk, checkSMILES=F, checkMz=T))
        assert(are_unknowns_OK, msg='Aborted because unknown compounds table contained errors.')
    }
    
    
    
    return(conf)
}


