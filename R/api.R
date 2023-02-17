## Copyright (C) 2020,2021,2023 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
#' @title Run Shinyscreen Pipeline
#'
#' @details TODO.
#' 
#' @param envopts `envopts`, an `envopts` object.
#' @param project `character(1)`, a directory containing input data.
#' @param m `state`, a Shinyscreen state.
#' @param phases `character(n)`, a character vector of Shinyscreen
#'     phases.
#' @param help `logical(1)`, print help?
#' @export
run <- function(envopts,
                project="",
                m=NULL,
                phases=NULL,
                help=F) {

                        
    
    all_phases=list(setup=setup_phase,
                    comptab=mk_comp_tab,
                    extract=extr_data,
                    prescreen=prescreen,
                    sort=sort_spectra,
                    subset=subset_summary,
                    plot=create_plots)

    if (help) {
        message("(run): You can run some of the following, or all the phases:")
        message(paste(paste0("(run): ",names(all_phases)),collapse = "\n"))
        return(invisible(NULL))
    }
    the_phases <- if (is.null(phases)) all_phases else {
                                                      x <- setdiff(phases,names(all_phases))
                                                      if (length(x)>0) {
                                                          message("(run): Error. Unknown phases:")
                                                          message(paste(paste0("(run): ",x),collapse = "\n"))
                                                          stop("Aborting.")
                                                      }
                                                      all_phases[phases]


                                                  }


    m = if (is.null(m)) {
            ## A project from scratch.
            new_project(project,envopts=envopts) 
        } else {
            ## Regenerate the runtime environment.

            project = if (nchar(project)==0L) m$run$project else project
            m$run = new_runtime_state(project=project,
                                      envopts = envopts,
                                      conf=m$conf)
            m
        }

    m = withr::with_dir(new=m$run$paths$project,code = Reduce(function (prev,f) f(prev),
                                                              x = the_phases,
                                                              init = m))
    return(invisible(m))
}


##' @export
setup_phase <- function(m) {
    message("Stage: setup")
    m <- mk_tol_funcs(m)
    m <- load_inputs(m)
    m <- concurrency(m)
    m
}

##' @export
run_in_dir <- function(m) {
    m <- setup_phase(m)
    m <- mk_comp_tab(m)
    m <- extr_data(m)
    m <- prescreen(m)
    m <- sort_spectra(m)
    m <- subset_summary(m)
    m <- create_plots(m)
    invisible(m)
    
}





##' @export
load_compound_input <- function(m) {
    coll <- list()
    fields <- colnames(EMPTY_CMPD_LIST)
    fns <- file.path(m$run$paths$project,m$conf$compounds$lists)
    message("fns:",paste0(fns,collapse=","))
    coltypes <- c(ID="character",
                  SMILES="character",
                  Formula="character",
                  Name="character",
                  RT="numeric",
                  mz="numeric")
    for (l in 1:length(fns)) {
        fn <- fns[[l]]

        ## Figure out column headers.
        nms <- colnames(file2tab(fn,nrows=0))

        ## Read the table. Knowing column headers prevents unnecessary
        ## warnings.
        dt <- file2tab(fn, colClasses=coltypes[nms])
        verify_cmpd_l(dt=dt,fn=fn)
                                        # nonexist <- setdiff(fnfields,fields)
        coll[[l]] <- dt #if (length(nonexist)==0) dt else dt[,(nonexist) := NULL]
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

    ## TODO: Should we just kick out the duplicates, instead of
    ## erroring?

    assert(all(!dups), msg = msg)
    
    cmpds[,("known"):=.(the_ifelse(!is.na(SMILES),"structure",the_ifelse(!is.na(Formula),"formula","mz")))]
    m$input$tab$cmpds <- cmpds
    fn_setid <- file.path(m$run$paths$project,m$conf$compounds$sets)
    m$input$tab$setid <- read_setid(fn_setid,
                                    m$input$tab$cmpds)
    m
}

##' @export
load_data_input <- function(m) {
    if (NROW(m$input$tab$mzml)==0L) {
        if (!file.exists(m$run$paths$datatab)) {
            stop("A CSV file with data file entries does not exist (`paths$datatab' in config).")
        }
        m$input$tab$mzml <- file2tab(m$run$paths$datatab)
    } else {
        message("Table `datatab' already loaded.")
    }
    m$input$tab$mzml <- as.data.table(m$input$tab$mzml)
    assert(all(unique(m$input$tab$mzml[,.N,by=c("adduct","tag")]$N)<=1),msg="Some rows in the data table contain multiple entries with same tag and adduct fields.")
    pref<-m$run$paths$data
    message("load_data_input: ", pref)
    for (fn in m$input$tab$mzml$file) {
        if (!file.exists(file.path(pref,fn))) stop("File ",fn," does not exist.")
    }
    ## m$input$tab$mzml[,file:=fifelse(file.exists(file),file,file.path(..pref,file))]
    ## m$input$tab$mzml[,file:=norm_path(file)]
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
    message("Stage: comptab")

    setid <- m$input$tab$setid
    setkey(setid,set)
    mzml<- m$input$tab$mzml
    setkey(mzml,set)
    cmpds<-m$input$tab$cmpds
    setkey(cmpds,ID)
    assert(nrow(cmpds)>0,msg="No compound lists have been provided.")
    assert(all(mzml[,unique(set)] %in% setid[,unique(set)]),msg="Not all set names in the `datatab' data file table match those in the provided set list.")
    assert(all(mzml[,!is.na(unique(adduct))]),msg="Some data file entries do not have selected adducts.")
    message("Begin generation of the comprehensive table.")

    comp <- cmpds[setid,on="ID"][mzml,.(tag,adduct,ID,RT,set,Name,file,SMILES,Formula,mz,known),on="set",allow.cartesian=T]
    tab2file(tab=comp,file=paste0("setidmerge",".csv"))
    setkey(comp,known,set,ID)
    message("Merged all sets.")
    message("Calculate formulas from SMILES (if any). Please wait.")
    ## Get just the info needed for mz calculation.
    comp_known <- comp[known=="structure" | known=="formula"]
    ## Remove mz==NA  col from knowns.
    comp_known[,mz:=NULL]
    comp_unknown <- comp[known=="mz"]
    smiforadd <- comp_known[known=="structure" | known=="formula",unique(.SD),.SDcols=c("adduct","ID","SMILES","Formula")]
    
    ## Turn SMILES into formulas.
    smiles <- smiforadd[,unique(.SD),.SDcols=c("SMILES")]
    smiles[,`:=`(Formula=smiles2form(SMILES))]
    badsmiles <- as.character(smiles[Formula=="",SMILES])
    if (length(badsmiles)>0) {
        stop("Unable to create formula from SMILES:",paste(badsmiles,collapse="\n"))
    }
    smiforadd <- smiles[smiforadd,.(ID,SMILES,Formula,adduct),on=c("SMILES")]
    data.table::setkey(smiforadd,"adduct","ID")

    ## FIXME: Why is Formula a list when there are no SMILES, instead
    ## of an empty string?
    smiforadd[,Formula:=as.character(Formula)]
    
    ## Update the intermediate table with masses.
    message("Formulas have been calculated. Start calculating masses from formulas.")
    smiforadd[,mz:=calc_mz_from_formula(Formula,adduct,ID)]
    message("Mass calculation has been completed.")

    ## Update the whole comprehensive table with masses from
    ## formulas. Doing it in a merge leaves a mess that has to be
    ## cleaned.
    comp2 <- merge(comp_known,smiforadd,all.x = T, by= c("adduct","ID"))
    ## Take Formulas from smiforadd (y) and SMILES from comp (x).
    comp2[,`:=`(Formula=Formula.y,SMILES=SMILES.x)]
    ## Now, populate mz from smiforadd (y) if SMILES/formula known,
    ## else take what was in the comp (x).
    ## comp2[,mz:=fifelse(known=="structure" | known=="formula",mz.y,mz.x)]
    nms <- names(comp)
    comp_known<-comp2[,..nms]
    ## In case you were wondering why is this all so complicated,
    ## well, for the moment I do not want to exclude mixed knowns and
    ## unknowns in the same run. The unknowns would have masses filled
    ## already at the stage of the compound list, so thay are taken
    ## from comp_unknown. Another twist is that mz can be calculated
    ## from either SMILES, or Formula.
    
    ## Combine knowns and unknowns finally.
    comp <- rbind(comp_known,comp_unknown)
    ## Rename stuff to be renamed and reorder columns.
    setnames(comp,names(COMP_NAME_MAP),
             function(o) COMP_NAME_MAP[[o]])
    setcolorder(comp,COMP_NAME_FIRST)

    ## Write it out.
    fn_out <- get_fn_comp(m)
    tab2file(tab=comp,file=fn_out)
    message("Generation of comprehensive table finished.")

    ## Index for fast search and access.
    setkeyv(comp,c("set","tag","mz"))
    m$out$tab$comp <- comp

    ## TODO: Not tested on cases when there are both knowns and
    ## unknowns present in the compound lists. It *should* work
    ## though.

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

verify_data_df <- function(data_path,mzml,all_sets) {
    no_file <- which(mzml[,!file.exists(file.path(data_path,file))])
    no_adducts <- which(mzml[,!(adduct %in% names(ADDUCTMAP))])
    no_sets <- which(mzml[,!(set %in% all_sets)])
    assert(length(no_file)==0,msg = paste("Non-existent data files at rows:",paste(no_file,collapse = ',')))
    assert(length(no_adducts)==0,msg = paste("Unrecognised adducts at rows:",paste(no_adducts,collapse = ',')))
    assert(length(no_sets)==0,msg = paste("Unknown sets at rows:",paste(no_sets,collapse = ',')))
}

verify_data <- function(conf,run,all_sets) {
    ## * Existence of input files
    fn_data <- run$paths$datatab
    assert(isThingFile(fn_data),msg=paste("Data table does not exist:",fn_data))
    mzml <- file2tab(fn_data)
    verify_data_df(run$paths$data,mzml=mzml,all_sets)
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

    ## TODO: Needs a rework to be useful. But, this is not a problem,
    ## because the user controls concurrency settings from the outside
    ## using future::plan.
    
    ## workers <- m$conf$concurrency$workers
    ## plan <- m$conf$concurrency$plan
    ## if (!is.null(plan) && plan!=user) {
    ##     n <- if (!is.null(workers)) workers else NO_WORKERS
    ##     if (!is.na(n)) future::plan(plan,workers=workers) else future::plan(plan)
    ##     m$conf$concurrency$workers <- n

    ## } else {
    ##     m$conf$concurrency$workers <- NA
    ##     m$conf$concurrency$plan <- "user"
    ## }
    ## message("plan: ",m$conf$concurrency$plan)
    ## message("workers: ",m$conf$concurrency$workers)

    ## So we can actually debug.
    m$future <- if (is.null(m$conf$debug) || !m$conf$debug)
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
extr_data <-function(m) {
    message("Stage: extract")
    if (is.null(m$conf$serial) || !m$conf$serial) {
        extr_data_future(m)
    } else {
        message("(extract): Serial extraction.")
        extr_data_serial(m)
    }
}

extr_data_future <- function(m) {
    ## Reduce the comp table to only unique masses (this is because
    ## different sets can have same masses).
    
    m$out$tab$data <- m$out$tab$comp[,head(.SD,1),by=BASE_KEY]
    m$out$tab$data[,set:=NULL] #This column is meaningless now.
    file <- m$out$tab$data[,unique(file)]
    fpaths <- file.path(m$run$paths$data,file)
    allCEs <- do.call(c,args=lapply(fpaths,function(fn) {
        z <- MSnbase::readMSData(files=fn,msLevel = c(1,2),mode="onDisk")

        
        unique(MSnbase::collisionEnergy(z),fromLast=T)
        
    }))
    allCEs <- unique(allCEs)
    allCEs <- allCEs[!is.na(allCEs)]
    cols <-paste('CE',allCEs,sep = '')
    vals <- rep(NA,length(cols))
    m$out$tab$data[,(cols) := .(rep(NA,.N))]
    file <- m$out$tab$data[,unique(file)]
    ftags <- m$out$tab$data[,.(tag=unique(tag)),by=file]
    fpaths <- file.path(m$run$paths$data,ftags[,file])
    futuref <- m$future
    tmp <- lapply(1:nrow(ftags),function(ii) {
        fn <- fpaths[[ii]]
        the_tag <- ftags[ii,tag]
        message("(extract): Commencing extraction for tag: ", the_tag, "; file: ",fn)
        tab <- as.data.frame(data.table::copy(m$out$tab$data[tag==the_tag,.(file,tag,adduct,mz,rt,ID)]))
        ## err_ms1_eic <- m$extr$tol$eic
        ## err_coarse_fun <- m$extr$tol$coarse
        ## err_fine_fun <- m$extr$tol$fine
        ## err_rt <- m$extr$tol$rt

        err_coarse <- m$conf$tolerance[["ms1 coarse"]]


        err_fine <- m$conf$tolerance[["ms1 fine"]]

        
        err_ms1_eic <- m$conf$tolerance$eic 
        
        
        err_rt <- m$conf$tolerance$rt

        missing_precursor_info <- m$conf$extract$missing_precursor_info
        x <- futuref(extract(fn=fn,
                             tag=the_tag,
                             tab=tab,
                             err_ms1_eic=err_ms1_eic,
                             err_coarse = err_coarse,
                             err_fine= err_fine,
                             err_rt= err_rt,
                             missing_precursors = missing_precursor_info),
                     lazy = F)

        x

    })

    msk <- sapply(tmp,future::resolved)
    curr_done <- which(msk)
    
    for (x in curr_done) {
        message("Done extraction for ", future::value(tmp[[x]])$ms1$tag[[1]])
    }
    while (!all(msk)) {
        msk <- sapply(tmp,future::resolved)
        newly_done <- which(msk)
        for (x in setdiff(newly_done,curr_done)) {
            message("Done extraction for ", future::value(tmp[[x]])$ms1$tag[[1]])
        }
        Sys.sleep(0.5)
        curr_done <- newly_done
    }
    
    ztmp <- lapply(tmp,future::value)
    m$extr$ms1 <- data.table::rbindlist(lapply(ztmp,function(x) x$ms1))
    m$extr$ms2 <- data.table::rbindlist(lapply(ztmp,function(x) x$ms2))
    data.table::setkeyv(m$extr$ms1,BASE_KEY)
    data.table::setkeyv(m$extr$ms2,c(BASE_KEY,"CE"))

    fn_ex <- get_fn_extr(m)
    timetag <- format(Sys.time(), "%Y%m%d_%H%M%S")
    saveRDS(object = m, file = file.path(m$run$paths$project,
                                         paste0(timetag,"_",FN_EXTR_STATE)))
    m
    
}


extr_data_serial <- function(m) {
    ## Reduce the comp table to only unique masses (this is because
    ## different sets can have same masses).
    
    m$out$tab$data <- m$out$tab$comp[,head(.SD,1),by=BASE_KEY]
    m$out$tab$data[,set:=NULL] #This column is meaningless now.
    file <- m$out$tab$data[,unique(file)]
    fpaths <- file.path(m$run$paths$data,file)
    allCEs <- do.call(c,args=lapply(fpaths,function(fn) {
        z <- MSnbase::readMSData(files=fn,msLevel = c(1,2),mode="onDisk")

        
        unique(MSnbase::collisionEnergy(z),fromLast=T)
        
    }))
    allCEs <- unique(allCEs)
    allCEs <- allCEs[!is.na(allCEs)]
    cols <-paste('CE',allCEs,sep = '')
    vals <- rep(NA,length(cols))
    m$out$tab$data[,(cols) := .(rep(NA,.N))]
    file <- file.path(m$run$paths$data,m$out$tab$data[,unique(file)])
    ftags <- m$out$tab$data[,.(tag=unique(tag)),by=file]
    ftags[,path:=file.path(..m$run$paths$data,file)]
    futuref <- m$future
    tmp <- lapply(1:nrow(ftags),function(ii) {
        fn <- ftags[ii,path]
        the_tag <- ftags[ii,tag]
        message("(extract): Commencing extraction for tag: ", the_tag, "; file: ",fn)
        tab <- as.data.frame(data.table::copy(m$out$tab$data[tag==the_tag,.(file,tag,adduct,mz,rt,ID)]))
        ## err_ms1_eic <- m$extr$tol$eic
        ## err_coarse_fun <- m$extr$tol$coarse
        ## err_fine_fun <- m$extr$tol$fine
        ## err_rt <- m$extr$tol$rt

        err_coarse <- m$conf$tolerance[["ms1 coarse"]]


        err_fine <- m$conf$tolerance[["ms1 fine"]]

        
        err_ms1_eic <- m$conf$tolerance$eic 
        
        
        err_rt <- m$conf$tolerance$rt

        missing_precursor_info <- m$conf$extract$missing_precursor_info
       
        x <- extract(fn=fn,
                     tag=the_tag,
                     tab=tab,
                     err_ms1_eic=err_ms1_eic,
                     err_coarse = err_coarse,
                     err_fine= err_fine,
                     err_rt= err_rt,
                     missing_precursors = missing_precursor_info)

        
        message("Done extraction for ", x$ms1$tag[[1]])
        x

    })

    ztmp <- tmp
    m$extr$ms1 <- data.table::rbindlist(lapply(ztmp,function(x) x$ms1))
    m$extr$ms2 <- data.table::rbindlist(lapply(ztmp,function(x) x$ms2))
    data.table::setkeyv(m$extr$ms1,BASE_KEY)
    data.table::setkeyv(m$extr$ms2,c(BASE_KEY,"CE"))

    fn_ex <- get_fn_extr(m)
    timetag <- format(Sys.time(), "%Y%m%d_%H%M%S")
    saveRDS(object = m, file = file.path(m$run$paths$project,
                                         paste0(timetag,"_",FN_EXTR_STATE)))
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
    message("(prescreen): Start.")
    ## confpres <- conf_trans_pres(m$conf$prescreen)
    m$qa <- NULL
    m$out$tab$summ <- NULL
    m$qa <- analyse_extracted_data(m$extr,m$conf$prescreen)
    m$out$tab$summ <- gen_summ(m$out$tab$comp,m$qa)
    message("(prescreen): End.")
    m
}




##' Sets the key specified by DEF_KEY_SUMM and adds second indices,
##' either from DEF_INDEX_SUMM, or user-specified in
##' conf[["summary table"]]$order. The order entry is a list of
##' strings with names of columns in summ, optionally prefixed with a
##' minus(-) sign. Columns prefixed with the minus are going to be in
##' ascending order.
##'
##' @title Sort the Summary Table 
##' @param m 
##' @return m
##' @author Todor Kondić
##' @export
sort_spectra <- function(m) {
    ## Sorts the summary table (summ) in order specified either in
    ## `order spectra` sublist of m$conf, or if that is null, the
    ## DEF_INDEX_SUMM.

    ## Now, add secondary indexing.
    cols <- if (!is.null(m$conf[["summary table"]]$order)) m$conf[["summary table"]]$order else DEF_INDEX_SUMM
    
    idx <- gsub("^\\s*-\\s*","",cols) #We need only column names for
                                        #now, so remove minuses where
                                        #needed.
    assertthat::assert_that(all(idx %in% colnames(m$out$tab$summ)),msg = "Some column(s) in order key in conf file does not exist in the summary table.")

    data.table::setindexv(m$out$tab$summ,idx)

    ## Now we order based on either summary table order subkey, or
    ## DEF_ORDER_SUMM
    
    tmp <- quote(data.table::setorder())
    tmp[[2]] <- quote(m$out$tab$summ)
    for (n in 1:length(cols)) tmp[[2+n]] <- parse(text=cols[[n]])[[1]]
    message("Ordering expression: \n",deparse(tmp))
    eval(tmp) #Execute the setorder call
    m
}

##' Subsets the summary table by applying conditions set out in the
##' filter subkey of summary table key of the config. Each member of
##' filter is an expression that and all of them are chained together
##' using AND logical operation and applied to the summary table.
##'
##' 
##' @title Subset the Summary Table
##' @param m 
##' @return m
##' @author Todor Kondić
##' @export
subset_summary <- function(m) {
    filt <- m$conf[["summary table"]]$filter
    m$out$tab$flt_summ <- if (!is.null(filt)) {
                              tmp <- lapply(filt, function (x) parse(text = x)[[1]])
                              expr <- Reduce(function (x,y) {z<-call("&");z[[2]]<-x;z[[3]]<-y;z},x=tmp)
                              message("Filtering with: ",deparse(bquote(m$out$tab$summ[.(expr)])))
                              eval(bquote(m$out$tab$summ[.(expr)]))
                              
                              
                          } else m$out$tab$summ

    m
}

#' @export
create_plots <- function(m) {
    ## Produce plots of EICs and spectra and group them acording to
    ## conf$figures$grouping.

    ## Select the data nedeed for plotting.
    flt_summ <- m$out$tab$flt_summ
    ms2 <- m$extr$ms2
    ms1 <- m$extr$ms1
    rt_min <- rt_in_min(m$conf$figures$rt_min)
    rt_max <- rt_in_min(m$conf$figures$rt_max)
    keytab <- flt_summ[,unique(.SD),.SDcol=c("adduct","ID")]
    for (n in 1:NROW(keytab)) {
        select <- dtable(adduct=keytab$adduct[[n]],
                         ID=keytab$ID[[n]])
        
        pdata_ms1 <- data4plot_ms1_cgram(ms1,select)
        pdata_ms2 <- data4plot_ms2_cgram(ms2,select)
        pdata_spec <- data4plot_ms2_spec(ms2,flt_summ,select)

        tabl_ms1 <- table_eic(pdata_ms1)
        ## tabl_ms2 <- table_eic(pdata_ms2) # For the moment, no real
                                            # info to be had here.
        tabl_spec <- table_spec(pdata_spec)

        palette = plot_palette(pdata_ms1)

        p_eic <- plot_eic_w_facet(pdata_ms1 = pdata_ms1,
                                  pdata_ms2 = pdata_ms2,
                                  rt_range = c(rt_min,rt_max),
                                  palette = palette)

        p_spec <- plot_spec_w_facet(pdata_ms2 = pdata_spec,
                                    mz_range = c(NA_real_,NA_real_),
                                    palette = palette)

        ## TODO
        plot_save_single(p_eic,
                         decotab = select,
                         figtag = "eic",
                         proj_path = m$run$paths$project,
                         tabl = tabl_ms1,
                         extension = m$conf$figures$ext)

        plot_save_single(p_spec,
                         decotab = select,
                         figtag = "spec",
                         proj_path = m$run$paths$project,
                         tabl = tabl_spec,
                         extension = m$conf$figures$ext)
        message("Plotting of figure ",n," out of ",NROW(keytab)," has been completed.")
        
    }


    m
}

prepare_app <- function(dir_before,
                        envopts) {
    ## Information that needs to be availabe to the shiny server.
    init <- list()
    init$dir_before <- dir_before
    init$envopts = envopts

    ## Create independent starting `home' for the server.
    dir_start <- tempfile("shinyscreen")
    dir.create(dir_start, recursive = T)

    ## Get list of app document contents.
    dir_rmd = system.file("rmd",package = "shinyscreen")
    fnms_rmd = list.files(dir_rmd,pattern = r"(^app.*\.Rmd$)",full.names=T)
    
    ## Copy startup files to that location.
    dir.create(file.path(dir_start,'www'), showWarnings=F)
    saveRDS(object = init,file=file.path(dir_start,"init.rds"))

    file.copy(system.file(file.path("www","custom.css"),package = "shinyscreen"),file.path(dir_start,"www","custom.css"))
    for (fn in fnms_rmd) file.copy(fn,file.path(dir_start,basename(fn)))
    
    dir_start
}


#' @export
#' @title app
#' @param envopts `envopts`. Shinyscreen environment options. 
#' @param shiny_args `list`, optional list of arguments conveyed to
#'     `rmarkdown::run` `shiny_args` argument.
#' @param render_args `list`, optional list of arguments conveyed to
#'     `rmarkdown::run` `render_args` argument.
#' @param metfrag_runtime `character(1)`, a location on the server side
#'     of the MetFrag jar file.
#' @return Nada.
#' @author Todor Kondić
app <- function(envopts,
                shiny_args=list(launch.browser=F),
                render_args=NULL) {
    dir_before = getwd()
    message("dir_before: ", dir_before)
    message("top_data_dir: ", envopts$top_data_dir)
    message("projects: ", evnopts$projects)
    dir_start = prepare_app(dir_before=dir_before,
                            envopts=envopts)

    on.exit(expr=setwd(dir_before))
    setwd(dir_start)
    rmarkdown::run(file = "app.Rmd", shiny_args = shiny_args, render_args = render_args)
}


#' @export
#' @title serve
#' @param envopts `envopts`, an `envopts` object.
#' @param user `character(1)`, subdir of usersdir.
#' @param host `character(1)`, optional, address where the page is
#'     served.
#' @param port `integer(1)`, optional, port at which the page is
#'     served.
#' @param top_data_dir `character(1)`, a location on the server side
#'     containing data directories.
#' @param metfrag_db_dir `character(1)`, a location on the server side
#'     containing MetFrag DBs.
#' @return Nada.
#' @author Todor Kondić
serve <- function(envopts,user,host='0.0.0.0',port=7777) {
    shiny_args <- c(list(launch.browser=F),list(host=host,port=port))
    projects <- file.path(envopts$users_dir,user)
    if (!dir.exists(projects)) {
        dir.create(projects)
        message('Created projects: ',projects)
    } else {
        message('Using existing projects: ', projects)
    }
    app(shiny_args=shiny_args,
        envopts=envopts)
}


#' @export
#' @title report
report <- function(m) {
    report_author <- if (!is.null(m$conf$report$author)) m$conf$report$author else REPORT_AUTHOR
    report_title <- if (!is.null(m$conf$report$title)) m$conf$report$title else REPORT_TITLE
    fn_header <- system.file(file.path('rmd','report_header.rmd'),package = "shinyscreen")
    fn_chunk <- system.file(file.path('rmd','report_chunk.rmd'), package = "shinyscreen")
    dir.create(REP_TOPDIR,recursive = T,showWarnings = F)
    header <- knitr::knit_expand(fn_header)
    flt_summ <- m$out$tab$reptab
    ms2 <- m$extr$ms2
    ms1 <- m$extr$ms1
    rt_min <- rt_in_min(m$conf$figures$rt_min)
    rt_max <- rt_in_min(m$conf$figures$rt_max)
    keytab <- flt_summ[,unique(.SD),.SDcol=c("adduct","ID")]

    repdoc <- header
    for (n in 1:NROW(keytab)) {
        select <- dtable(adduct=keytab$adduct[[n]],
                         ID=keytab$ID[[n]])
        
        pdata_ms1 <- data4plot_ms1_cgram(ms1,select)
        pdata_ms2 <- data4plot_ms2_cgram(ms2,select)
        pdata_spec <- data4plot_ms2_spec(ms2,flt_summ,select)

        tabl_ms1 <- table_eic(pdata_ms1)
        ## tabl_ms2 <- table_eic(pdata_ms2) # For the moment, no real
                                            # info to be had here.
        tabl_spec <- table_spec(pdata_spec)

        palette = plot_palette(pdata_ms1)


        p_eic <- plot_eic_w_facet(pdata_ms1 = pdata_ms1,
                                  pdata_ms2 = pdata_ms2,
                                  rt_range = c(rt_min,rt_max),
                                  palette = palette) + ggplot2::theme(legend.position = "bottom")
        p_spec <- plot_spec_w_facet(pdata_ms2 = pdata_spec,
                                    mz_range = c(NA_real_,NA_real_),
                                    palette = palette) + ggplot2::theme(legend.position = "bottom")
        eic_things <- plot_save_single(p_eic,
                                       decotab = select,
                                       figtag = "eic",
                                       proj = m$conf$project,
                                       tabl = tabl_ms1,
                                       subdir = REP_TOPDIR,
                                       extension = m$conf$figures$ext)
        

        spec_things <- plot_save_single(p_spec,
                                        decotab = select,
                                        figtag = "spec",
                                        proj = m$conf$project,
                                        tabl = tabl_spec,
                                        subdir = REP_TOPDIR,
                                        extension = m$conf$figures$ext)

        report_chunk_header <- paste0("Adduct: ",keytab$adduct[[n]],"; ",
                                      "ID: ",keytab$ID[[n]])


        report_tab_eic_cap <- sprintf("EIC for %s",keytab$ID[[n]])
        report_fn_eic <- eic_things$fn_plot
        report_tab_eic <- if (shiny::isTruthy(eic_things$tab)) knitr::kable(eic_things$tab, caption = report_tab_eic_cap) else ""

        report_tab_spec_cap <- sprintf("Spectrum for %s",keytab$ID[[n]])
        report_fn_spec <- spec_things$fn_plot
        report_tab_spec <- if (shiny::isTruthy(eic_things$tab)) knitr::kable(spec_things$tab, caption = report_tab_spec_cap) else ""
        
        repdoc <- c(repdoc,knitr::knit_expand(fn_chunk))
        message("(report) Knitting of chunk ",n," out of ",NROW(keytab)," has been completed.")
        
    }
    fn_rep <- file.path(m$run$paths$project,"report.Rmd")
    message("(report) Writing Rmd...")
    cat(repdoc,file=fn_rep,sep = "\n")
    message("(report) ...done.")
    message("(report) Render start ...")
    rmarkdown::render(fn_rep,output_dir = m$run$paths$project)
    message("(report) ...done.")
    m
}


#' @title Initialise Shinyscreen Configuration
#' @details This function is used to inform `shinyscreen` about the
#'     working environment. If argument `save` is T, the configuration
#'     will be memorised. Subsequent calls to `init` without arguments
#'     will just load the configuration. If `merge` argument is T, the
#'     resulting configuration object is going to be a merge between
#'     the new parameters and the memorised ones. Those arguments not
#'     mentioned in the argument list will be remembered from the save
#'     config.
#' @inheritParams envopts
#' @param merge `logical(1)`, optional. If T, merge with saved
#'     configuration.
#' @param save `logical(1)`, optional. If T, save configuration,
#'     otherwise just return the Shinyscreen environment options.
#' @return An `envopts` object.
#' @author Todor Kondić
#' @export
init <- function(projects=NULL,
                 top_data_dir=NULL,
                 metfrag_db_dir=NULL,
                 metfrag_jar=NULL,
                 java_bin=NULL,
                 metfrag_max_proc=NULL,
                 merge=T,
                 save=F) {

    
    env = environment()
    eargs = list()
    ## Merge into the untouched (NULLs), only.
    if (merge) {
        ## Merge with old values
        eold = load_envopts()
        cargs = formalArgs(envopts)
        if (length(eold)>0L) {
            for (a in cargs) {
                if (is.null(env[[a]])) eargs[[a]] = eold[[a]] else eargs[[a]]=env[[a]]
            }
        }
    }

    ## Default values = "" .
    chrargs = c("projects","top_data_dir","metfrag_db_dir")

    for (ca in chrargs) {
        if (is.null(eargs[[ca]])) eargs[[ca]]=""
    }

    if (is.null(java_bin)) {
        eargs[["java_bin"]]= Sys.which("java")
    }

    if (is.null(metfrag_max_proc)) {
        eargs$metfrag_max_proc = parallel::detectCores()
    }
    
    e = do.call(envopts,eargs)
    
    
    if (save) save_envopts(o=e)
    e
}

#' @title Run MetFrag
#' @details
#' @param m `state`, a state object.
#' @return m
#' @author Todor Kondić
metfrag <- function(m) {
    stagtab = metfrag_get_stag_tab(m$out$tab$summ[ms2_sel==T])
    ftab = metfrag_run(param = m$run$metfrag$param,
                       path = m$run$metfrag$path,
                       subpaths = m$run$metfrag$subpaths,
                       db_path = m$run$metfrag$db_path,
                       stag_tab = stagtab, ms2 = m$extr$ms2,
                       runtime=m$run$metfrag$runtime,
                       java_bin=m$run$metfrag$java_bin,
                       nproc = m$conf$metfrag$nproc)

    tab = summarise_metfrag_results(param = m$conf$metfrag$param,
                                    path = m$run$metfrag$path,
                                    subpaths = m$run$metfrag$subpaths,
                                    cand_parameters = m$conf$metfrag$cand_parameters,
                                    db_scores = m$conf$metfrag$database_scores,
                                    int_scores = m$conf$metfrag$intrinsic_scores,
                                    collect_candidates= m$conf$metfrag$collect_candidates,
                                    file_tab = ftab)

    fn = file.path(m$run$metfrag$path,"metfrag_summary.csv")

    data.table::fwrite(x=tab,file=fn,quote=T)

    m
}
