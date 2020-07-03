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
    m <- new_state(fn_conf=fn_conf,
                   GUI=F)    
    dir.create(m$conf$project,
               showWarnings = F,
               recursive = T)
    m <- withr::with_dir(new=conf$project,code = run_in_dir(m))
    return(invisible(m))
}


##' @export
run_in_dir <- function(m) {
    m <- mk_tol_funcs(m)
    m <- load_inputs(m)
    m <- concurrency(m)
    m <- mk_comp_tab(m)
    
    invisible(m)
    
}



##' @export
load_compound_input <- function(m) {

    coll <- list()
    fields <- colnames(EMPTY_CMPD_LIST)
    fns <- m$conf$compounds$lists
    for (l in 1:length(fns)) {
        fn <- fns[[l]]
        fnfields <- colnames(fn)
        dt <- file2tab(fn, colClasses=c(ID="character",
                                        SMILES="character",
                                        Formula="character",
                                        Name="character",
                                        RT="numeric",
                                        mz="numeric"))
        verify_cmpd_l(dt=dt,fn=fn)
        nonexist <- setdiff(fnfields,fields)
        coll[[l]] <- if (length(nonexist)==0) dt else dt[,(nonexist) := NULL]
        coll[[l]]$ORIG <- fn
        
    }
    cmpds <- if (length(fns)>0) rbindlist(l=c(list(EMPTY_CMPD_LIST), coll), use.names = T, fill = T) else EMPTY_CMPD_LIST

    dups <- duplicated(cmpds$ID)
    dups <- dups | duplicated(cmpds$ID,fromLast = T)
    dupIDs <- cmpds$ID[dups]
    dupfns <- cmpds$ORIG[dups]

    msg <- ""
    for (fn in unique(dupfns)) {
        inds <- which(dupfns %in% fn)
        fndupID <- paste(dupIDs[inds], collapse = ',')
        msg <- paste(paste('Duplicate IDs', fndupID,'found in',fn),msg,sep = '\n')
    }

    assert(all(!dups), msg = msg)
    
    cmpds[,("known"):=.(the_ifelse(!is.na(SMILES),"structure",the_ifelse(!is.na(Formula),"formula","mz")))]
    m$input$tab$cmpds <- cmpds
    m$input$tab$setid <- read_setid(m$conf$compounds$sets,
                                    m$input$tab$cmpds)
    m
}

##' @export
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
    setid <- m$input$tab$setid
    setkey(setid,set)
    mzml<- m$input$tab$mzml
    setkey(mzml,set)
    cmpds<-m$input$tab$cmpds
    setkey(cmpds,ID)
    mzml[,`:=`(wd=sapply(Files,add_wd_to_mzml,m$conf$project))]
    assert(nrow(cmpds)>0,msg="No compound lists have been provided.")
    message("Begin generation of the comprehensive table.")
   
    comp <- cmpds[setid,on="ID"][mzml,.(tag,adduct,ID,RT,set,Name,Files,wd,SMILES,Formula,mz,known),on="set",allow.cartesian=T]
    tab2file(tab=comp,file=paste0("setidmerge",".csv"))
    setkey(comp,known,set,ID)

    ## Known structure.
    ## comp[,`:=`(mz=mapply(calc_mz_from_smiles,SMILES,adduct,ID,USE.NAMES = F))]
    comp[known=="structure",`:=`(mz=calc_mz_from_smiles(SMILES,adduct,ID))]

    ## Known formula.
    comp[known=="formula",`:=`(mz=calc_mz_from_formula(Formula,adduct,ID))]
    setnames(comp,names(COMP_NAME_MAP),
             function(o) COMP_NAME_MAP[[o]])
    setcolorder(comp,COMP_NAME_FIRST)
    fn_out <- file.path(m$conf$project,FN_COMP_TAB)
    tab2file(tab=comp,file=fn_out)
    message("Generation of comp table finished.")
    setkeyv(comp,c("set","tag","mz"))
    m$out$tab$comp <- comp
    m
}


verify_compounds <- function(conf) {
    ## * Existence of input files

    fns_cmpds <- conf$compounds$lists
    fn_cmpd_sets <- conf$compounds$sets

    ## ** Compound lists and sets

    assert(isThingFile(fn_cmpd_sets),
                            msg=paste("Cannot find the compound sets file:",fn_cmpd_sets))

    for (fn in fns_cmpds) {
        assert(isThingFile(fn), msg=paste("Cannot find compound list:",fn))
    }

    ## * Data files
    df_sets <- file2tab(fn_cmpd_sets)
    all_sets<-unique(df_sets$set)

    return(list(conf=conf,all_sets=all_sets))
}

verify_data_df <- function(mzml,all_sets) {
    no_files <- which(mzml[,!file.exists(Files)])
    no_adducts <- which(mzml[,!(adduct %in% names(ADDUCTMAP))])
    no_sets <- which(mzml[,!(set %in% all_sets)])
    assert(length(no_files)==0,msg = paste("Non-existent data files at rows:",paste(no_files,collapse = ',')))
    assert(length(no_adducts)==0,msg = paste("Unrecognised adducts at rows:",paste(no_adducts,collapse = ',')))
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

## @export
concurrency <- function(m) {
    ## Reads the concurrency entry in the config. It is optional, if
    ## not given, then it is up to the user to define the plan of the
    ## futures package. If present, it contains at least the `plan'
    ## specification. It can also contain `workers` entry specifying
    ## the number of workers. If that entry is absent, the default
    ## number of workers is NO_WORKERS from the resources.R.
    workers <- m$conf$concurrency$workers
    plan <- m$conf$concurrency$plan
    if (!is.null(plan)) {
        n <- if (!is.null(workers)) workers else NO_WORKERS
        if (!is.na(n)) future::plan(plan,workers=workers) else future::plan(plan)
        m$conf$concurrency$workers <- n

    } else {
        m$conf$concurrency$workers <- NA
        m$conf$concurrency$plan <- "user"
    }
    message("plan: ",m$conf$concurrency$plan)
    message("workers: ",m$conf$concurrency$workers)

    ## So we can actually debug.
    m$future <- if (!m$conf$debug)
                    future::future
                else {
                    message("Debug: futures evaluate as identity")
                    function(x,...) identity(x)
                }
    m
}

mk_tol_funcs <- function(m) {
    ## Depending on units given when the user specified the errors,
    ## generate functions that calculate errors given the concrete
    ## mass.

    ## Mass errors can be either in ppm, or Da.
    ## Time errors in min, or s.

    ## The mass error calculation functions and the retention time
    ## error in minutes are in m$extr$tol.
    
    grab <- function(entry,unit) {
        what <- paste0("\\<",unit,"\\>$")
        entry <- trimws(entry,which="both")
        if (grepl(what,entry))
            as.numeric(sub(paste0("^(.*)",unit),"\\1",entry)) else NA_real_
    }

    asgn_mz_err <- function (entry, msg) {
        eppm <- grab(entry,"ppm")
        eda <- grab(entry,"Da")
        shinyscreen:::assert(xor(is.na(eda), is.na(eppm)), msg = msg)
        if (is.na(eda)) Vectorize(function(mz) eppm*1e-6*mz, USE.NAMES = F) else Vectorize(function(mz) eda, USE.NAMES = F)
    }

    asgn_t_err <- function (entry, msg) {
        em <- grab(entry,"min")
        es <- grab(entry,"s")
        shinyscreen:::assert(xor(is.na(em), is.na(es)), msg = msg)
        if (is.na(em)) es/60. else em
        
        
    }

    m$extr$tol$coarse <- asgn_mz_err(m$conf$tolerance[["ms1 coarse"]], msg = "ms1 coarse error: Only ppm, or Da units allowed.")
    m$extr$tol$fine <- asgn_mz_err(m$conf$tolerance[["ms1 fine"]], msg = "ms1 fine error: Only ppm, or Da units allowed.")
    m$extr$tol$eic <- asgn_mz_err(m$conf$tolerance$eic, msg = "eic error: Only ppm, or Da units allowed.")

    m$extr$tol$rt <- asgn_t_err(m$conf$tolerance$rt, msg = "rt error: Only s(econds), or min(utes) allowed.")

    m
    
}

##' @export
extr_data <- function(m) {

    ## Reduce the comp table to only unique masses (this is because
    ## different sets can have same masses).
    m$out$tab$data <- m$out$tab$comp[,head(.SD,1),by=c('adduct','tag','ID')]
    files <- m$out$tab$data[,unique(Files)]
    allCEs <- do.call(c,args=lapply(files,function(fn) {
        z <- MSnbase::readMSData(files=fn,msLevel = c(1,2),mode="onDisk")
        unique(MSnbase::collisionEnergy(z),fromLast=T)
        
    }))
    allCEs <- unique(allCEs)
    allCEs <- allCEs[!is.na(allCEs)]
    cols <-paste('CE',allCEs,sep = '')
    vals <- rep(NA,length(cols))
    m$out$tab$data[,(cols) := .(rep(NA,.N))]
    files <- m$out$tab$data[,unique(Files)]
    m$extr$tmp <- lapply(files,function(fn) future::future(extract(fn=fn,
                                                                   tab=m$out$tab$data[,.(Files,mz,rt,ID)],
                                                                   err_ms1_eic=m$extr$tol$eic,
                                                                   err_coarse_fun=m$extr$tol$coarse,
                                                                   err_fine_fun=m$extr$tol$fine,
                                                                   err_rt=m$extr$tol$rt),
                                                           lazy = T))
    m
    
}
