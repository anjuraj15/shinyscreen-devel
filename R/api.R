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
                    db=make_db,
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

    fns <- file.path(m$run$paths$project,m$conf$compounds$lists)
    cmpds = join_compound_lists(fns)
    ## Process sets.
    cmpds = process_cmpd_sets(cmpds)
    
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

    mzml<- m$input$tab$mzml
    setkey(mzml,set)
    cmpds<-m$input$tab$cmpds
    setkey(cmpds,set,ID)
    assert(nrow(cmpds)>0,msg="No compound lists have been provided.")
    assert(all(mzml[,unique(set)] %in% cmpds[,unique(set)]),msg="Not all set names in the `datatab' data file table match those in the provided set list.")
    assert(all(mzml[,!is.na(unique(adduct))]),msg="Some data file entries do not have selected adducts.")
    message("Begin generation of the comprehensive table.")

    comp <- cmpds[mzml,.(tag,adduct,ID,RT,set,Name,file,SMILES,Formula,mz,known),on="set",allow.cartesian=T]
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

    fine = create_fine_table(m)

    dpath = m$run$paths$data

    ## Open all files.
    fns = fine[,unique(file)]
    lms = lapply(fns,function(fn) read_data_file(file.path(dpath,fn)))
    names(lms) = fns

    ## Load all feature data.
    lfdata = lapply(lms,get_fdata)
    names(lfdata) = fns

    ## Extract MS1 chromatograms using "fine" tolerance.
    cgram_ms1 = fine[,extr_cgrams_ms1(lms[[file]],.SD,lfdata[[file]]),
                     by="file",
                     .SDcols=c("iso_fine_min",
                               "iso_fine_max",
                               "rt_min",
                               "rt_max",
                               "precid")]
    setkey(cgram_ms1,file,precid)

    ## Extract MS2 chromatograms.

    ## Create the "coarse" table. Parent masses are known with
    ## "coarse". We will prefilter our ms2 results based on that...
    coarse = create_coarse_table(m)

    
    cgram_ms2 = data.table(precid=integer(0),
                           ce=numeric(0),
                           scan=character(0),
                           idx=integer(0),
                           rt=numeric(0),
                           intensity=numeric(0))


    ## Extract MS2 spectra.
    spectra = empty_spectra_table()

    for (fn in names(lfdata)) {
        rtab = relate_ms2_to_precid(coarse=coarse[.(fn),on=.(file)],
                                    ms2=lfdata[[fn]]$ms2,
                                    cgram_ms1=cgram_ms1[.(fn),
                                                        on=.(file)])
        sptab = extract_spectra(lms[[fn]],rtab)
        cgram_ms2 = rbind(cgram_ms2,rtab)
        spectra = rbind(spectra,sptab)
    }
    setkey(cgram_ms1,precid,rt)
    setkey(cgram_ms2,precid,ce,rt)
    setkey(spectra,precid,scan)
    m$db$extr$cgm$ms1 = cgram_ms1
    m$db$extr$cgm$ms2 = cgram_ms2
    m$db$extr$spectra = spectra
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
    m$qa = NULL
    m$out$tab$summ = NULL
    m$qa = analyse_extracted_data(m$db,m$conf$prescreen)
    m$out$tab$summ = gen_summ(m$db,m$qa,m$out$tab$comp)
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
    ms2 <- m$db$extr$cgm$ms2
    ms1 <- m$db$extr$cgm$ms1
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
    message("projects: ", envopts$projects)
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
    ms2 <- m$db$extr$cgm$ms2
    ms1 <- m$db$extr$cgm$ms1
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
#' @inheritParams empty_envopts
#' @param merge `logical(1)`, optional. If T, merge with saved
#'     configuration.
#' @param save `logical(1)`, optional. If T, save configuration,
#'     otherwise just return the Shinyscreen environment options.
#' @param conf_dir `character(1)`, optional. Place where the
#'     configuration resides. Changing this usually only makes sense
#'     for testing.
#' @return An `envopts` object.
#' @author Todor Kondić
#' @export
init <- function(projects=NULL,
                 top_data_dir=NULL,
                 users_dir=NULL,
                 metfrag_db_dir=NULL,
                 metfrag_jar=NULL,
                 java_bin=NULL,
                 metfrag_max_proc=NULL,
                 no_structure_plots=NULL,
                 merge=T,
                 save=F,
                 conf_dir=tools::R_user_dir(package="shinyscreen",
                                            which="config")) {

    ## The function will usually return a merge between the saved
    ## configuration and arguments provided by the user. This is why
    ## we need to know which arguments have been actually changed by
    ## the user. In order to do this, all `envopts'-like arguments to
    ## `init' have been created with an illegal default value, NULL.

    ## Therefore, any argument with an user-supplied _valid_ value
    ## will be non-NULL. We can browse through the function
    ## environment, pick out these non-NULLs and then override the
    ## saved config with them.

    ## So, get the function environment.
    env = environment()

    ## Get the list of all possible arguments from the simpletst
    ## `envopts' constructor.
    eargs = formalArgs(empty_envopts)

    ## Check which are NULLs and retain only those which are
    ## not. 
    evals = lapply(eargs,function(ca) env[[ca]])
    ennull = sapply(evals,is.null,USE.NAMES=F)
    eargs = eargs[!ennull]
    evals = evals[!ennull]
    names(evals) = eargs
    ## Now call the empty envopts constructor only with non-NULL
    ## arguments.
    enew = do.call(empty_envopts,evals)
    ## If merging should occur.
    if (merge) {

        ## Get saved values.
        eold = load_envopts(dir=conf_dir)
        if (length(eold)>0L) {

            ## First, merge on non-metfrag keys. Overwrite only NULLs.
            simplekeys = setdiff(names(enew),"metfrag")
            for (a in simplekeys) {
                if (is.null(enew[[a]])) enew[[a]] = eold[[a]]
            }

            ## Now, metfrag. Do the same.
            mfkeys = names(enew$metfrag)
            for (a in mfkeys) {
                if (is.null(enew$metfrag[[a]])) enew$metfrag[[a]] = eold$metfrag[[a]]
            }
        }
        
    }

    ## Replace the remaining NULL values with actual defaults.
    e = seal_envopts(enew)
    
    if (save) save_envopts(o=e,dir=conf_dir)
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
                       db_file = m$run$metfrag$db_file,
                       stag_tab = stagtab, ms2 = m$db$extr$spectra,
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

make_db <- function(m) {
    m = make_db_catalogue(m)
    m = make_db_precursors(m)
    m
}
