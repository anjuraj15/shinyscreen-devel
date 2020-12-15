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
new_state <- function() {
    m <- new_conf()
    init_state(m)
}

##' @export
new_rv_state <- function() react_v(m=list2rev(new_state()))

##' @export
new_state_fn_conf <- function(fn_conf) {
    m <- new_state()
    m$conf <- read_conf(fn_conf)
    init_state(m)
}

##' @export
run <- function(fn_conf="",m=NULL,phases=NULL,help=F) {
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
    
    m <- if (nchar(fn_conf)!=0) new_state_fn_conf(fn_conf) else if (!is.null(m)) m else stop("(run): Either the YAML config file (fn_conf),\n or the starting state (m) must be provided\n as the argument to the run function.")
    
    dir.create(m$conf$project,
               showWarnings = F,
               recursive = T)
    m <- withr::with_dir(new=m$conf$project,code = Reduce(function (prev,f) f(prev),
                                                          x = the_phases,
                                                          init = m))
    return(invisible(m))
}


##' @export
setup_phase <- function(m) {
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
    fns <- m$conf$compounds$lists
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
    m$input$tab$setid <- read_setid(m$conf$compounds$sets,
                                    m$input$tab$cmpds)
    m
}

##' @export
load_data_input <- function(m) {
    m$input$tab$mzml <- file2tab(m$conf$data)
    assert(all(unique(m$input$tab$mzml[,.N,by=c("adduct","tag")]$N)<=1),msg="Some rows in the data table contain multiple entries with same tag and adduct fields.")
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
    ## mzml[,`:=`(wd=sapply(file,add_wd_to_mzml,m$conf$project))]
    assert(nrow(cmpds)>0,msg="No compound lists have been provided.")
    message("Begin generation of the comprehensive table.")
    
    comp <- cmpds[setid,on="ID"][mzml,.(tag,adduct,ID,RT,set,Name,file,SMILES,Formula,mz,known),on="set",allow.cartesian=T]
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
    fn_out <- get_fn_comp(m)
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
    no_file <- which(mzml[,!file.exists(file)])
    no_adducts <- which(mzml[,!(adduct %in% names(ADDUCTMAP))])
    no_sets <- which(mzml[,!(set %in% all_sets)])
    assert(length(no_file)==0,msg = paste("Non-existent data files at rows:",paste(no_file,collapse = ',')))
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
    
    m$out$tab$data <- m$out$tab$comp[,head(.SD,1),by=BASE_KEY]
    m$out$tab$data[,set:=NULL] #This column is meaningless now.
    file <- m$out$tab$data[,unique(file)]
    allCEs <- do.call(c,args=lapply(file,function(fn) {
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
    futuref <- m$future
    tmp <- lapply(1:nrow(ftags),function(ii) {
        fn <- ftags[ii,file]
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
        message("Done extraction for ", unique(future::value(tmp[[x]])$ms1$tag))
    }
    while (!all(msk)) {
        msk <- sapply(tmp,future::resolved)
        newly_done <- which(msk)
        for (x in setdiff(newly_done,curr_done)) {
            message("Done extraction for ", unique(future::value(tmp[[x]])$file))
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
    saveRDS(object = m, file = file.path(m$conf$project,
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
    confpres <- conf_trans_pres(m$conf$prescreen)

    m$qa <- create_qa_table(m$extr,confpres)
    m1 <- assess_ms1(m)
    m <- assess_ms2(m1)
    m$out$tab$summ <- gen_summ(m$out$tab$comp,m$qa$ms1,m$qa$ms2)
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


##' @export
gen_struct_plots <- function(m) {
    ## Generate structure plots.
    comp <- m$out$tab$comp

    res <- if (NROW(comp)>0) {
        structtab <- m$out$tab$comp[known=="structure",unique(.SD),.SDcols=c("ID","SMILES")]
        message("Start generating structures.")
        structtab[,img:=.({tmp <- lapply(SMILES,function (sm) smiles2img(sm,width = 500,height = 500, zoom = 4.5))
            tmp})]
        message("Done generating structures.")
        structtab
           } else {
               dtable(ID=character(0),SMILES=character(0),img=list())
           }
    
    m$out$tab$structfig <- res

    m
}


#' @export
create_plots <- function(m) {
    ## Produce plots of EICs and spectra and group them acording to
    ## conf$figures$grouping.

    ## Select the data nedeed for plotting.
    flt_summ <- m$out$tab$flt_summ

    group_data <- m$conf$figures$grouping
    plot_group <- if (!shiny::isTruthy(group_data$group)) FIG_DEF_CONF$grouping$group else group_data$group
    plot_plot <- if (!shiny::isTruthy(group_data$plot)) FIG_DEF_CONF$grouping$plot else group_data$plot
    plot_ms1_label <- if (!shiny::isTruthy(group_data$plot)) FIG_DEF_CONF$grouping$label else group_data$label
    plot_ms2_label <- "CE"

    plot_index <- c(plot_group,plot_plot)

    ## All the possible curve labels.
    all_ms1_labels <- flt_summ[,unique(.SD),.SDcols=plot_ms1_label][[plot_ms1_label]]
    all_ms1_labels <- sort(all_ms1_labels[!is.na(all_ms1_labels)])
    all_ms2_ce_labels <- flt_summ[,unique(CE)]
    all_ms2_ce_labels <- sort(all_ms2_ce_labels[!is.na(all_ms2_ce_labels)])

    ## Plot styling.
    style_eic_ms1 <- plot_decor(m,m$conf$logaxes$ms1_eic_int,
                                all_ms1_labels=all_ms1_labels,
                                legend_name_ms1=plot_ms1_label)
    style_eic_ms2 <- plot_decor(m,m$conf$logaxes$ms2_eic_int,
                                all_ms1_labels = all_ms1_labels,
                                all_ms2_labels = all_ms2_ce_labels,
                                legend_name_ms1 = plot_ms1_label,
                                legend_name_ms2 = "CE")
    style_spec_ms2 <- plot_decor(m,m$conf$logaxes$ms2_spec_int,
                                 all_ms1_labels = all_ms1_labels,
                                 all_ms2_labels = all_ms2_ce_labels,
                                 legend_name_ms1 = plot_ms1_label,
                                 legend_name_ms2 = "CE")

    style_ms2_leg <- plot_decor(m,m$conf$logaxes$ms2_spec_int,
                                all_ms1_labels = all_ms1_labels,
                                all_ms2_labels = all_ms2_ce_labels,
                                legend_name_ms1 = plot_ms1_label,
                                legend_name_ms2 = "CE",
                                ms1_legend_info = F)

    
    plot_key <- gen_key_plot_tab(m)

    topdir <- FIG_TOPDIR 
    dir.create(topdir,showWarnings = F)

    my_theme <- function(...) plot_theme(legend.position = "left",
                                         legend.direction = "vertical")

    theme_full <- my_theme()
    theme_noleg <- plot_theme(legend.position = "none")


    clean_range<-function(def,rng) {
        x1 <- rng[1]
        x2 <- rng[2]
        if (is.na(x1) || x1 == 0) x1 <- def[1]
        if (is.na(x2) || x2 == 0) x2 <- def[2]
        c(x1,x2)
    }
    
    ## If structures do not exist, generate them.
    if (is.null(m$out$tab$structfig)) m <- gen_struct_plots(m)
    
    plot_key[,mapply(function(gv,pv) {

        key <- c(gv,pv)
        names(key) <- plot_index


        
        p_chr_ms1 <- plot_ms1_chr(m, plot_index =  key)
        p_chr_ms2 <- plot_ms2_chr(m, plot_index =  key)
        p_spec_ms2 <- plot_ms2_spec(m, plot_index = key)
        p_struct <- plot_struct_nowrap(m, plot_index = key)


        ## Produce the filename.
        fn <- paste0(paste(..plot_group,gv,..plot_plot,pv,sep = "_"),".pdf")
        fn <- gsub("\\[","",fn)
        fn <- gsub("\\]","",fn)
        fn <- gsub("\\+","p",fn)
        fn <- gsub("-","m",fn)
        fn <- if (!is.null(topdir)) file.path(topdir,fn) else fn

        
        rt_int <- get_rt_interval(p_chr_ms1$data, p_chr_ms2$data, m$conf$figures)
        my_coord <- ggplot2::coord_cartesian(xlim = rt_int)

        p_chr_ms1 <- p_chr_ms1 + my_coord + theme_full
        p_chr_ms2 <- p_chr_ms2 + my_coord + theme_full
        leg1 <- cowplot::get_legend(p_chr_ms1)
        leg2 <- cowplot::get_legend(p_chr_ms2)
        p_spec_ms2 <- p_spec_ms2 + theme_full


        ## Plot labels.
        labels <- c(paste0("EIC (MS1) ",..plot_group,": ",gv,", ",..plot_plot,": ",pv),
                        NA,
                        paste0("EIC (MS2) ",..plot_group,": ",gv,", ",..plot_plot,": ",pv),
                        NA,
                        paste0("MS2 Spectra ",..plot_group,": ",gv,", ",..plot_plot,": ",pv),
                        NA)
        
        big_fig <- cowplot::plot_grid(p_chr_ms1+theme_noleg,
                                      p_struct,
                                      p_chr_ms2+theme_noleg,
                                      leg2,
                                      p_spec_ms2+theme_noleg,
                                      leg1,
                                      align = "hv",
                                      axis='l',
                                      ncol = 2,
                                      nrow = 3,
                                      labels = labels,
                                      rel_widths = c(2,1))
        
        message("Plotting: ",paste(key,collapse = ", ")," to: ",fn)
        ggplot2::ggsave(plot=big_fig,width = 21, height = 29.7, units = "cm", filename = fn)
        
    },
    get(..plot_group),
    get(..plot_plot))]
    m
}

#' @export
report <- function(m) {
    figtopdir <- FIG_TOPDIR #file.path(m$conf$project,FIG_TOPDIR)
    pander::evalsOptions("graph.output","pdf")
    author <- if (!is.null(m$conf$report$author)) m$conf$report$author else REPORT_AUTHOR
    title <- if (!is.null(m$conf$report$title)) m$conf$report$title else REPORT_TITLE
    doc <- pander::Pandoc$new(author,title)
    doc$add(pander::pandoc.header.return("Plots",level = 1))
    sets <- m$out$tab$flt_summ[,unique(set)]
    rep_theme <- ggplot2::labs(title = NULL)
    for (s in sets) {
        doc$add(pander::pandoc.header.return(paste('Set', s), level = 2))
        sdf <- m$out$tab$flt_summ[set==s,]
        group <- sdf[,unique(adduct)]
        for (g in group) {
            asdf <- sdf[adduct==g,] 
            ids <- asdf[,unique(ID)]
            for (id in ids) {
                message("Image ","set: ",s," group: ", g, " id: ",id)
                doc$add(pander::pandoc.header.return(paste('ID',id),level = 3))
                tab <- asdf[ID==id,.(tag,ms1_int,ms1_rt,adduct,mz,file)]
                ms2info <- m$out$tab$ms2_spec[adduct==g & ID==id,.(tag,ID,rt,ms2_max_int,file)]
                tab2 <- tab[ms2info,on="file"][,.(tag,mz,adduct,"$RT_{ms1}$[min]"=ms1_rt,"$RT_{ms2}$[min]"=rt,"$I{ms1}$"=formatC(ms1_int, format="e",digits = 2), "$I(ms2)$"= formatC(ms2_max_int, format="e",digits = 2))]
                data.table::setorderv(tab2,c("$I{ms1}$","$I(ms2)$"),c(-1,-1))
                doc$add.paragraph("")
                figpath <- fig_path(top=figtopdir,set=s,group=g,id=id,suff="all",ext="pdf")
                doc$add(pander::pandoc.image.return(img=paste0("file:",figpath)))
                doc$add.paragraph("")
                message("Adding table.")
                doc$add.paragraph(pander::pandoc.table.return(tab2))
                message("Done adding table.")
                ## doc$add(print(tab))
                doc$add.paragraph("")
                
            }
            
        }
    }
    doc$add(pander::pandoc.header.return("Appendix", level = 1))
    doc$add(pander::pandoc.header.return("Configuration",level = 2))
    doc$add(m$conf)
    doc$add(pander::pandoc.header.return("R Session Info",level = 2))
    doc$add(sessionInfo())
    m$out$report <- doc
    m$out$report$export('report.pdf')
    m
}


#' @export
app <- function() {
    unlink(list.files(pattern = "app_run.*html$"))
    unlink(list.files(pattern = "app_run.*Rmd$"))
    file.copy(system.file(file.path("rmd","app.Rmd"),package = "shinyscreen"),"app_run.Rmd")
    rmarkdown::run(file = "app_run.Rmd")
}
