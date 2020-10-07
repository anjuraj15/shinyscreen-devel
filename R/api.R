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
    m <- new_state(conf=conf,
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
    m <- extr_data(m)
    m <- prescreen(m)
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
    fn_out <- m$conf$fn_comp
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

#' @export
concurrency <- function(m) {
    ## Reads the concurrency entry in the config. It is optional, if
    ## not given, then it is up to the user to define the plan of the
    ## futures package. If present, it contains at least the `plan'
    ## specification. It can also contain `workers` entry specifying
    ## the number of workers. If that entry is absent, the default
    ## number of workers is NO_WORKERS from the resources.R.
    workers <- m$conf$concurrency$workers
    plan <- m$conf$concurrency$plan
    if (!is.null(plan) && plan!=user) {
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

#' @export
mk_tol_funcs <- function(m) {
    ## Depending on units given when the user specified the errors,
    ## generate functions that calculate errors given the concrete
    ## mass.

    ## Mass errors can be either in ppm, or Da.
    ## Time errors in min, or s.

    ## The mass error calculation functions and the retention time
    ## error in minutes are in m$extr$tol.

    ## TODO make these things compatible with futures.



    m$extr$tol$coarse <- gen_mz_err_f(m$conf$tolerance[["ms1 coarse"]],
                                      "ms1 coarse error: Only ppm, or Da units allowed."
                                      )


    m$extr$tol$fine <- gen_mz_err_f(m$conf$tolerance[["ms1 fine"]],
                                    "ms1 fine error: Only ppm, or Da units allowed.")

    m$extr$tol$eic <- gen_mz_err_f(m$conf$tolerance$eic,
                                   "eic error: Only ppm, or Da units allowed.")

 
    m$extr$tol$rt <- gen_rt_err(m$conf$tolerance$rt,
                                "rt error: Only s(econds), or min(utes) allowed.")
    
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
    ftags <- m$out$tab$data[,.(tag=unique(tag)),by=Files]
    futuref <- m$future
    tmp <- lapply(1:nrow(ftags),function(ii) {
        fn <- ftags[ii,Files]
        tag <- ftags[ii,tag]
        tab <- as.data.frame(data.table::copy(m$out$tab$data[,.(Files,adduct,mz,rt,ID)]))
        ## err_ms1_eic <- m$extr$tol$eic
        ## err_coarse_fun <- m$extr$tol$coarse
        ## err_fine_fun <- m$extr$tol$fine
        ## err_rt <- m$extr$tol$rt

        err_coarse <- m$conf$tolerance[["ms1 coarse"]]


        err_fine <- m$conf$tolerance[["ms1 fine"]]

        
        err_ms1_eic <- m$conf$tolerance$eic 
        
        
        err_rt <- m$conf$tolerance$rt
                                   
        x <- futuref(extract(fn=fn,
                             tab=tab,
                             err_ms1_eic=err_ms1_eic,
                             err_coarse = err_coarse,
                             err_fine= err_fine,
                             err_rt= err_rt),
                     lazy = T)

        x

    })

    msk <- sapply(tmp,future::resolved)
    curr_done <- which(msk)
    
    for (x in curr_done) {
        message("Done extraction for ", unique(future::value(tmp[[x]])$Files))
    }
    while (!all(msk)) {
        msk <- sapply(tmp,future::resolved)
        newly_done <- which(msk)
        for (x in setdiff(newly_done,curr_done)) {
            message("Done extraction for ", unique(future::value(tmp[[x]])$Files))
        }
        Sys.sleep(0.5)
        curr_done <- newly_done
    }
    ztmp <- lapply(tmp,future::value)

    ## ## We need to add in Files (after futures are resolved).
    ## for (nn in 1:nrow(ftags)) {
    ##     fn <- ftags[nn,Files]
    ##     ztmp[[nn]]$Files <- fn
    ## }
    m$extr$ms <- data.table::rbindlist(ztmp)
    message('Saving extracted data to ', m$extr$fn)
    saveRDS(object = m$extr, file = m$extr$fn)
    message('Done saving extracted data.')
    
    m$extr$tmp <- NULL
    m
    
}

##' @export
conf_trans <- function(conf) {
    conf$prescreen <- conf_trans_pres(conf$prescreen)
    conf
}

##' @export
prescreen <- function(m) {
    ## Top-level auto prescreening function.


    ## TODO need to fix max spec intensity
    gen_ms2_spec_tab <- function(ms) {data.table::rbindlist(lapply(1:nrow(ms), function (nr) {
        adduct <- ms$adduct[[nr]]
        ID <- ms$ID[[nr]]
        Files <- ms$Files[[nr]]
        spec <- ms$spec[[nr]]
        dt <- if (length(spec[[1]]) < 3)
                  dtable(CE=NA_real_,
                                       rt=NA_real_,
                                       spec=list(dtable(mz=NA_real_,intensity=NA_real_))) else {
                                                                                                            dtable(
                                                                                                                              CE=sapply(spec,
                                                                                                                                        function (x) x$CE),
                                                                                                                              rt=sapply(spec,
                                                                                                                                        function (x) x$rt),
                                                                                                                              spec=lapply(spec,
                                                                                                                                          function (x) x$spec))
                                                                                                        }
        dt$Files <- Files
        dt$ID <- ID
        dt$adduct <- adduct
        dt[,ms2_max_int := .(sapply(spec,function (sp) sp[,max(intensity)]))] 
        dt
    }))}

    gen_ms1_spec_tab <- function(ms) {
        cols <- MS1_SPEC_COLS
        ms[,..cols]
    }


    m$qa <- create_qa_table(m$extr$ms,m$conf$prescreen)
    mms1 <- assess_ms1(m)
    m <- assess_ms2(mms1)
    fields <- c("Files","adduct","ID",QA_COLS)
    m$out$tab$ms2_spec <- gen_ms2_spec_tab(m$qa$ms)
    data.table::setkeyv(m$out$tab$ms2_spec,c("adduct","Files","ID"))
    m$out$tab$ms1_spec <- gen_ms1_spec_tab(m$qa$ms)
    data.table::setkeyv(m$out$tab$ms1_spec,c("adduct","Files","ID"))
    m$out$tab$summ <- merge(m$out$tab$comp,m$qa$ms[,..fields],by=c("Files","adduct","ID"))
    m
}




##' @export
sort_spectra <- function(m) {
    ## Sorts the summary table (summ) in order specified either in
    ## `order spectra` sublist of m$conf, or if that is null, the
    ## DEF_ORDER_SPECTRA.
    order <- if (!is.null(m$conf[["order spectra"]])) m$conf[["order spectra"]] else DEF_ORDER_SPECTRA
    data.table::setkeyv(m$out$tab$summ,order)
    m
}
