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
run <- function(fn_conf) {
    m <- new_state_fn_conf(fn_conf)
    
    dir.create(m$conf$project,
               showWarnings = F,
               recursive = T)
    m <- withr::with_dir(new=m$conf$project,code = run_in_dir(m))
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
    m <- save_plots(m)
    invisible(m)
    
}





##' @export
load_compound_input <- function(m) {

    coll <- list()
    fields <- colnames(EMPTY_CMPD_LIST)
    fns <- m$conf$compounds$lists
    for (l in 1:length(fns)) {
        fn <- fns[[l]]
        # fnfields <- somehow read the file columns in
        dt <- file2tab(fn, colClasses=c(ID="character",
                                        SMILES="character",
                                        Formula="character",
                                        Name="character",
                                        RT="numeric",
                                        mz="numeric"))
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
    m$out$tab$data[,set:=NULL] #This column is meaningless now.
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
    fn_ex <- get_fn_extr(m)
    message('Saving extracted data to ', fn_ex)
    saveRDS(object = m$extr, file = fn_ex)
    message('Done saving extracted data.')
    
    m$extr$tmp <- NULL
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

    confpres <- conf_trans_pres(m$conf$prescreen)

    ## TODO need to fix max spec intensity
    gen_ms2_spec_tab <- function(ms) {data.table::rbindlist(lapply(1:nrow(ms), function (nr) {
        adduct <- ms$adduct[[nr]]
        ID <- ms$ID[[nr]]
        Files <- ms$Files[[nr]]
        spec <- ms$spec[[nr]]
        ms2_sel <- ms$ms2_sel[[nr]]
        dt <- if (length(spec[[1]]) < 3)
                  dtable(CE=NA_real_,
                         rt=NA_real_,
                         spec=list(dtable(mz=NA_real_,intensity=NA_real_)),
                         ms2_sel=NA) else {
                                                                                dtable(
                                                                                    CE=sapply(spec,
                                                                                              function (x) x$CE),
                                                                                    rt=sapply(spec,
                                                                                              function (x) x$rt),
                                                                                    spec=lapply(spec,
                                                                                                function (x) x$spec),
                                                                                    ms2_sel=F)
                                      }
        if (!is.na(ms2_sel)) dt$ms2_sel[[ms2_sel]] <- T
        
        
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


    m$qa <- create_qa_table(m$extr$ms,confpres)
    mms1 <- assess_ms1(m)
    m <- assess_ms2(mms1)
    fields <- c("Files","adduct","ID",QA_COLS)
    m$out$tab$ms2_spec <- gen_ms2_spec_tab(m$qa$ms)
    m$out$tab$ms1_spec <- gen_ms1_spec_tab(m$qa$ms)
    m$out$tab$summ <- merge(m$out$tab$comp,m$qa$ms[,..fields],by=c("Files","adduct","ID"))
    data.table::setkeyv(m$out$tab$ms2_spec,c("adduct","Files","ID"))
    data.table::setkeyv(m$out$tab$ms1_spec,c("adduct","Files","ID"))
    
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

    ## Here set default sorting keys.
    data.table::setkeyv(m$out$tab$summ,DEF_KEY_SUMM)

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
    message("Ordering expression: ",tmp)
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
    ## Empty ms1_plot table.
    

    ## Select the data nedeed for plotting.
    x <- m$out$tab$flt_summ
    message("Generate plot data.")
    ms1_plot_data <- gen_base_ms1_plot_tab(summ=x,
                                           ms1_spec=m$out$tab$ms1_spec)
    ms2_plot_data <- gen_base_ms2_plot_tab(summ=x,
                                           ms2_spec=m$out$tab$ms2_spec)
    message("Done generating plot data.")

    group_data <- m$conf$figures$grouping
    plot_group <- if (!shiny::isTruthy(group_data$group)) FIG_DEF_CONF$grouping$group else group_data$group
    plot_plot <- if (!shiny::isTruthy(group_data$plot)) FIG_DEF_CONF$grouping$plot else group_data$plot
    plot_ms1_label <- if (!shiny::isTruthy(group_data$plot)) FIG_DEF_CONF$grouping$ms1_label else group_data$ms1_label
    plot_ms2_label <- if (!shiny::isTruthy(group_data$plot)) FIG_DEF_CONF$grouping$ms2_label else group_data$ms2_label
    message("plot_group:",plot_group)
    message("plot_plot:",plot_plot)
    message("plot_ms1_label",plot_ms1_label)
    message("plot_ms2_label",plot_ms2_label)
    plot_index <- c(plot_group,plot_plot)

    ## All the possible curve labels.
    all_ms1_labels <- ms1_plot_data[,unique(.SD),.SDcols=plot_ms1_label][[plot_ms1_label]]
    all_ms2_labels <- ms2_plot_data[,unique(.SD),.SDcols=plot_ms2_label][[plot_ms2_label]]
    
    ## Plot styling.
    style_eic_ms1 <- plot_decor(m,m$conf$logaxes$ms1_eic_int,
                                all_labels=all_ms1_labels,
                                legend_name=plot_ms1_label)
    style_eic_ms2 <- plot_decor(m,m$conf$logaxes$ms2_eic_int,
                                all_labels=all_ms2_labels,
                                legend_name=plot_ms2_label)
    style_spec_ms2 <- plot_decor(m,m$conf$logaxes$ms2_spec_int,
                                 all_labels=all_ms2_labels,
                                 legend_name = plot_ms2_label)

    message("Create MS1 EIC plots.")
    ## Generate MS1 EIC plots.
    ms1_plot <- ms1_plot_data[,.(fig_eic={
        df <- .SD[,data.table::rbindlist(Map(function (a,b,c,d) {
            s <- a[[1]]
            s$plot_label <- b
            s$rt_peak <- c
            s$mz <- d
            s},
            eicMS1,
            .SD[[..plot_ms1_label]],
            rt_peak,
            mz))]
        list(plot_eic_ms1(df,style_fun = style_eic_ms1,
                          plot_label = ..plot_ms1_label))
        
    }),by = plot_index]
    m$out$tab$ms1_plot <- ms1_plot
    message("Done creating MS1 EIC plots.")
    ## Generate MS2 EIC plots.
    message("Create MS2 EIC plots.")
    ms2_plot_data[,plot_label:=factor(.SD[[1]]),.SDcols=plot_ms2_label]
    ms2_plot <- ms2_plot_data[,.(fig_eic=list(plot_eic_ms2(df=.SD,
                                                           style_fun = style_eic_ms2,
                                                           plot_label = plot_ms2_label)),
                                 fig_spec=list(plot_spec_ms2(df=.SD,
                                                             style_fun = style_spec_ms2,
                                                             plot_label = plot_ms2_label))),
                              .SDcols=c("rt_peak","int_peak",
                                        plot_ms2_label,
                                        "plot_label",
                                        "spec",
                                        "ms2_sel",
                                        "mz"),
                              by = plot_index]
    message("Done creating MS1 EIC plots.")

    ## Generate structure plots.
    structab <- ms1_plot_data[,.(ID=unique(ID))]
    structab <- m$out$tab$comp[known=="structure",][structab,.(ID=i.ID,SMILES=SMILES),on="ID",nomatch=NULL,mult="first"]
    message("Start generating structures.")
    structab[,structimg:=.({tmp <- lapply(SMILES,function (sm) smiles2img(sm,width = 500,height = 500, zoom = 4.5))
        tmp})]
    message("Done generating structures.")

    ## We need to check if we have multiplots grouped by ID in order
    ## for structure generation to make sense.
    if (plot_plot == "ID") {
        ms1_plot <- structab[ms1_plot,on="ID"][,c("fig_struct") := .(Map(function (st,eic) {
            df <- eic[[1]]$data
            ddf <- dtable(x=df$rt,
                          y=df$intensity)
            ggplot2::ggplot(ddf) +
                ggplot2::geom_blank() +
                ggplot2::annotation_custom(st) +
                ggplot2::theme_void()
        },
        structimg,
        fig_eic))]
        ms1_plot[,structimg:=NULL]
    }
    m$out$tab$ms2_plot <- ms2_plot
    m$out$tab$ms1_plot <- ms1_plot
    m
}

create_plots_old <- function(m) {
    ## Helpers
    textf <- ggplot2::element_text
    x <- m$out$tab$ms1_spec
    y <- m$out$tab$flt_summ
    

    ## Logarithmic, or linear y axis?
    scale_y_ms1_eic <- if (shiny::isTruthy(m$conf$logaxes$ms1_eic_int))
                           ggplot2::scale_y_log10 else ggplot2::scale_y_continuous

    scale_y_ms2_eic <- if (shiny::isTruthy(m$conf$logaxes$ms2_eic_int))
                           ggplot2::scale_y_log10 else ggplot2::scale_y_continuous

    scale_y_ms2_spec <- if (shiny::isTruthy(m$conf$logaxes$ms2_spec_int))
                            ggplot2::scale_y_log10 else ggplot2::scale_y_continuous

    scale_x <- function(...) ggplot2::scale_x_continuous(...,limits=DEFAULT_RT_RANGE)

    ## Colour palette.
    tags <- y[,unique(tag)]
    getpal <- colorRampPalette(RColorBrewer::brewer.pal(8,"Dark2"))
    col_all_vals <- getpal(length(tags))
    names(col_all_vals) <- tags
    scale_colour <- function(values=col_all_vals,...) ggplot2::scale_colour_manual(values = values,name=m$conf$figures[["legend title"]],...)

    rt_new_lim <- c(rt_in_min(m$conf$figures$rt_min),
                    rt_in_min(m$conf$figures$rt_max))
    rt_lim <- get_coord_lim(rt_new_lim,DEFAULT_RT_RANGE)
    
    
    my_coord <- ggplot2::coord_cartesian(xlim = rt_lim)
    conf_psub <- m$conf$figures[["plot subset"]]
    psub <- if (!is.null(conf_psub)) conf_psub else FIG_DEF_SUBSET
    assertthat::assert_that(all(psub %in% colnames(y)), msg = "Some plot subset columns are not recognised.")

    mk_title<-function(txt, mass) paste(txt," ",
                                   "m/z = ",
                                   formatC(mass,format='f',digits=M_DIGITS),sep='')


    mk_leg_lab<-function(tag,rt) {if (length(tag) > 0) paste(tag,"; rt= ",formatC(rt,format='f',digits=RT_DIGITS)," min",sep='') else character(0)}

    sci10<-function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))}
    
    my_theme <- function (...) ggplot2::theme()

    plot_eic_ms1 <- function(df) {

        mz <- df[,unique(mz)]
        ID <- df[,unique(ID)]
        tbl <- df[,.(labs=mk_leg_lab(tag,rt_peak),tag=tag),by=c("tag","rt_peak")]
        labs <- tbl[,labs]
        tags <- tbl[,tag]
        df[,tag:=factor(tag)]
        ggplot2::ggplot(df,ggplot2::aes(x=rt,y=intensity,colour=tag)) +
            ggplot2::geom_line(key_glyph=KEY_GLYPH) +
            ggplot2::labs(x=CHR_GRAM_X,
                          y=CHR_GRAM_Y
                          ## title=mk_title("EIC", mz),
                          ## tag=ID
                          ) +
            scale_y_ms1_eic(labels=sci10) +
            scale_colour(values=col_all_vals[as.character(tags)]) +
            scale_x() +
            my_coord +
            my_theme()
    }

    plot_eic_ms2 <- function(df) {
        mz <- df[,unique(mz)]
        ID <- df[,unique(ID)]
        ddf <- df[!is.na(rt)==T]
        ## df[,tag:=factor(tag)]
        ggplot2::ggplot(ddf,ggplot2::aes(x=rt,ymin=0,ymax=ms2_max_int,color=tag)) +
            ggplot2::geom_linerange(key_glyph=KEY_GLYPH) +
            ggplot2::labs(x=CHR_GRAM_X,
                          y=CHR_GRAM_Y
                          ## title=mk_title("MS2 EIC for precursor",mz),
                          ## tag=ID
                          ) +
            scale_y_ms2_eic(labels=sci10) +
            scale_colour(values=col_all_vals[as.character(ddf$tag)]) +
            scale_x() +
            my_coord +
            my_theme()
    }

    plot_spec_ms2 <- function(df) {
        ddf <- df[ms2_sel == T]
        mz <- ddf[,unique(mz)]
        ID <- ddf[,unique(ID)]

        tags <- ddf[,tag]
        specs <- ddf[,spec]
        rts <- ddf[,rt]
        lst <- Map(function(d,t) {d$tag<-t;d},specs,tags)
        data <- dtable(mz=numeric(0),intensity=numeric(0),tag=factor(0))
        data <- rbind(data,
                      data.table::rbindlist(lst),
                      fill=T)
        data <- data[!(is.na(mz)),]

        leglabs <- mk_leg_lab(tags,rts)
        ggplot2::ggplot(data,ggplot2::aes(x=mz,ymin=0,ymax=intensity,color=tag)) +
            ggplot2::geom_linerange(key_glyph=KEY_GLYPH) +
            ggplot2::labs(x="mz",
                          y="intensity"
                          ## tag=ID,
                          ## title=mk_title("MS2 spectrum for precursor",mz)
                          ) +
            scale_y_ms2_spec(labels=sci10) +
            scale_x() +
            scale_colour(values=col_all_vals[as.character(tags)]) +
            my_theme()
    }

    ## MS1
    tmp <- y[x,.(set,adduct,Files,ID,tag,mz,rt_peak=i.ms1_rt,eicMS1=lapply(i.eicMS1,list)), on=c("adduct","Files","ID"),nomatch=NULL]
    
    message("Start generating MS1 EICs.")
    z <- tmp[,.(fig= {
        df <- .SD[,data.table::rbindlist(Map(function (a,b,c) {
            s <- a[[1]]
            s$tag <- b
            s$rt_peak <- c
            s},eicMS1,tag,rt_peak))]

        df$mz <- .SD[,unique(mz)]
        df$ID <- .SD[,unique(ID)]
        list(plot_eic_ms1(df))
    }),by=psub,.SDcols=c("eicMS1","tag","mz","ID")]
    message("Done generating MS1 EICs.")
    structab <- z[,.(ID=unique(ID))]
    structab <- m$out$tab$comp[known=="structure",][structab,.(ID=i.ID,SMILES=SMILES),on="ID",nomatch=NULL,mult="first"]
    message("Start generating structures.")
    structab[,structimg:=.(lapply(SMILES,function (sm) smiles2img(sm,width = 500,height = 500, zoom = 4.5)))]
    message("Done generating structures.")
    
    q <- structab[z,on="ID"][,c("structfig") := .(Map(function (st,eic) {
        df <- eic[[1]]$data
        ddf <- dtable(x=df$rt,
                                    y=df$intensity)
        ggplot2::ggplot(ddf) +
            ggplot2::geom_blank() +
            ggplot2::annotation_custom(st) +
            ggplot2::theme_void()
    },
    structimg,
    fig))]
    m$out$tab$ms1_plot_eic <- q[,structimg:=NULL]
    data.table::setkeyv(m$out$tab$ms1_plot_eic,c("set","adduct","ID"))

    ## MS2
    x <- m$out$tab$ms2_spec 
    tmp <- y[x,.(set,adduct,Files,
                 ID=ID,tag=factor(tag),mz,CE=i.CE,
                 rt=i.rt,ms2_max_int,
                 spec=i.spec,
                 ms2_sel=i.ms2_sel), on = c("adduct","Files","ID")]

    message("Start generating MS2 EICs.")
    m$out$tab$ms2_plot <- tmp[,.(fig_eic  = list(plot_eic_ms2(.SD)),
                                 fig_spec = list(plot_spec_ms2(.SD))),
                              .SDcols=c("rt","ms2_max_int",
                                        "tag","spec","ms2_sel","mz","ID"),
                              by = psub]
    message("Done generating MS2 EICs.")
    data.table::setkeyv(m$out$tab$ms2_plot,c("set","adduct","ID"))

    
    m
    
}


#' @export
save_plots <- function(m) {
    topdir <- FIG_TOPDIR 
    dir.create(topdir,showWarnings = F)

    rt_lim <- DEFAULT_RT_RANGE
    if (isTruthy(m$conf$figures$rt_min)) rt_lim[[1]] <- rt_in_min(m$conf$figures$rt_min)
    if (isTruthy(m$conf$figures$rt_max)) rt_lim[[2]] <- rt_in_min(m$conf$figures$rt_max)
    
    my_theme <- function(...) ggplot2::theme(legend.position = "none",...)


    clean_range<-function(def,rng) {
        x1 <- rng[1]
        x2 <- rng[2]
        if (is.na(x1) || x1 == 0) x1 <- def[1]
        if (is.na(x2) || x2 == 0) x2 <- def[2]
        c(x1,x2)
    }

    
    sets <- m$out$tab$flt_summ[,unique(set)]
    for (s in sets) {
        sdf <- m$out$tab$flt_summ[set==s,]
        group <- sdf[,unique(adduct)]
        for (g in group) {
            asdf <- sdf[adduct==g,] 
            ids <- asdf[,unique(ID)]
            for (id in ids) {
                message("Image ","set: ",s," group: ", g, " id: ",id)
                
                tab <- asdf[ID==id,.(tag,ms1_int,ms1_rt,adduct,mz)]
                ms1_figs <- m$out$tab$ms1_plot_eic[set==s & adduct==g & ID==id,.(fig,structfig)]
                ms2_figs <- m$out$tab$ms2_plot[set==s & adduct==g & ID==id,.(fig_eic,fig_spec)]
                ms1_eic <- ms1_figs$fig[[1]]
                rt_rng <- range(ms1_eic$data[,rt])
                if (!is.na(rt_lim[[1]])) rt_rng[[1]] <- rt_lim[[1]]
                if (!is.na(rt_lim[[2]])) rt_rng[[2]] <- rt_lim[[2]]
                my_coord <- ggplot2::coord_cartesian(xlim = rt_rng)
                ms2_eic <- ms2_figs$fig_eic[[1]]+my_coord #ggplot2::coord_cartesian(xlim = rt_rng)
                ms2_spec <- ms2_figs$fig_spec[[1]]
                xxdf <- ms1_figs$fig[[1]]$data[,.(rt=rt,intensity=intensity)]
                empty_fig <- ggplot2::ggplot(xxdf,ggplot2::aes(x=rt,y=intensity)) +
                    ggplot2::geom_blank() +
                    ggplot2::theme_void()

                ## if (id == 1078) browser()
                if (NROW(ms2_eic$data) == 0) ms2_eic <- empty_fig
                if (NROW(ms2_spec$data) == 0) ms2_spec <- empty_fig
                leg <- cowplot::get_legend(ms1_eic)
                big_fig <- cowplot::plot_grid(ms1_eic+my_theme(),
                                              ms1_figs$structfig[[1]],
                                              ms2_eic+my_theme(),
                                              empty_fig,
                                              ms2_spec+my_theme(),leg,
                                              align = "hv",
                                              axis='l',
                                              ncol = 2,
                                              nrow = 3,
                                              rel_widths = c(2,1))
                ggplot2::ggsave(plot=big_fig,filename = fig_path(top=topdir,
                                                                 set=s,
                                                                 group=g,
                                                                 id=id,
                                                                 suff="all"))
                
                
            }
            
        }
    }

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
                tab <- asdf[ID==id,.(tag,ms1_int,ms1_rt,adduct,mz,Files)]
                ms2info <- m$out$tab$ms2_spec[adduct==g & ID==id,.(Files,ID,rt,ms2_max_int)]
                tab2 <- tab[ms2info,on="Files"][,.(tag,mz,adduct,"$RT_{ms1}$[min]"=ms1_rt,"$RT_{ms2}$[min]"=rt,"$I{ms1}$"=formatC(ms1_int, format="e",digits = 2), "$I(ms2)$"= formatC(ms2_max_int, format="e",digits = 2))]
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
