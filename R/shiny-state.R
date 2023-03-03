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

## Description
## 
## Helpers used in shiny server. Makes testing easier.


## Imports from shiny:

#' @importFrom shiny selectInput numericInput textInput HTML


GUI_SELECT_INPUTS <- c("proj_list",
                       "indir_list",
                       "ms1_coarse_unit",
                       "ms1_fine_unit",
                       "ms1_rt_win_unit",
                       "ret_time_shift_tol_unit",
                       "dfile_list")
                  
GUI_NUMERIC_INPUTS <- c("ms1_coarse",
                   "ms1_fine",
                   "ms1_eic",
                   "ms1_rt_win",
                   "ms1_int_thresh",
                   "ms2_int_thresh",
                   "s2n",
                   "ret_time_shift_tol")

GUI_TEXT_INPUTS <- c("rep_aut",
                     "rep_tit")

GUI_RADIO_INPUTS <- c("missingprec")



GUI_ALL_INPUTS <- c(GUI_SELECT_INPUTS,
                    GUI_NUMERIC_INPUTS,
                    GUI_TEXT_INPUTS,
                    GUI_RADIO_INPUTS)


add_new_def_tag <- function(old_tags,how_many) {
    ind = which(grepl(r"(^F\d+$)",old_tags))
    st_num = if (length(ind)>0L) {
                    old_def_tags <- old_tags[ind]
                    tag_nums <- gsub(r"(^F(\d+)$)",r"(\1)",old_def_tags)
                    max(as.integer(tag_nums))
                   
        
    } else 0L
    if (how_many>0L) c(old_tags,paste0('F',(st_num + 1L):(st_num + how_many))) else old_tags
}

filetag_add_file <- function(filetag,file) {
    curr_file = filetag$file
    curr_tag = filetag$tag
    nb = length(curr_file)
    nd = length(file)
    
    res_file = union(curr_file,file)
    really_new_files = setdiff(res_file,curr_file)
    res_tag = add_new_def_tag(as.character(curr_tag),how_many=length(really_new_files))
    filetag$file = res_file
    filetag$tag = res_tag
    filetag
}


datatab_update_tags <- function(tab,tags,files) {
    check_same_len(files,"files",tags,"tags")

    nft = data.table(tag=tags,
                     file=files,key="file")
    oldt = data.table(tag=tab$tag,
                      adduct=tab$adduct,
                      set=tab$set,
                      file=tab$file,key=c("file"))


    ## Adapt existing tags.
    res = merge(nft,oldt,by=c("file","tag"),all.y=T)
    tab$tag = res$tag
    tab$file= res$file
    tab$set = res$set
    tab$adduct = res$adduct
    tab
}

datatab_add_files <- function(tab,sets,adducts,tags,files) {
    check_len_zero(sets,"sets")
    check_len_zero(adducts,"adducts")
    check_len_zero(files,"files")
    check_same_len(files,"files",tags,"tags")

    nrows = prod(length(sets),length(adducts),length(tags))
    
    nft = as.data.table(expand.grid(tag=tags,
                                    set=sets,
                                    adduct=adducts),key="tag")
    ftt=data.table(tag=tags,file=files,key="tag")
    nft[ftt,file:=i.file]
    oldt = data.table(tag=tab$tag,
                      adduct=tab$adduct,
                      set=tab$set,
                      file=tab$file,key=c("tag","file"))

    res = merge(nft,oldt,by=c("tag","adduct","set","file"),all=T)

    tab$tag=res$tag
    tab$adduct=res$adduct
    tab$set=res$set
    tab$file=res$file
    tab
}

#' @export
create_stub_gui <- function() {
    gui = list()
    shiny::isolate({
        gui$compounds = shiny::reactiveValues(lists=character(),
                                               sets=character())
        gui$filetag = shiny::reactiveValues(file=character(),
                                            tag=character())
        gui$datatab = shiny::reactiveValues(file=character(),
                                             tag=character(),
                                             adduct=character(),
                                             set=character())

        gui$paths = shiny::reactiveValues(project=NA_character_,
                                           data=NA_character_)
        gui$project = shiny::reactiveVal(NA_character_)
    })
    gui
}


create_gui <- function(project_path=NA_character_) {
    shiny::isolate({
        gui = create_stub_gui()
        if (!is.na(project_path)) {
            gui$paths$project = project_path
            gui$project(basename(project_path))
        }
        
        gui
    })
}

#'@export
r2datatab <- function(rdatatab) {
    shiny::isolate({
        file <- rdatatab$file
        adduct <- rdatatab$adduct
        tag <- rdatatab$tag
        set <- rdatatab$set
        })
    if (length(file)==0L) file <- character(0)
    if (length(adduct)==0L) adduct <- rep(NA_character_,length(file))
    if (length(tag)==0L) tag <- rep(NA_character_,length(file))
    if (length(set)==0L) tag <- rep(NA_character_,length(file))
    data.table(tag=tag,adduct=adduct,set=set,file=file)
}


r2filetag <- function(rfiletag) {
    shiny::isolate({
        file <- rfiletag$file
        tag <- rfiletag$tag
        })
    if (length(file)==0L) file <- character(0)
    if (length(tag)==0L) tag <- rep(NA_character_,length(file))
    data.table(tag=tag,file=file)
}

gen_dtab <- function(tablist,sets) {
    data.table(tag=factor(tablist$tag,levels=unique(tablist$tag)),
               adduct=factor(tablist$adduct,levels=DISP_ADDUCTS),
               set=factor(tablist$set,levels=sets))
}

r2compounds <- function(rcompounds) {
    shiny::isolate({
        cmpd_lists <- rcompounds$lists
        })

    list(lists=cmpd_lists)
    }


#' @export
pack_app_state <- function(input, gui) {
    pack <- list()
    shiny::isolate({
        pack_inputs <- list()
        pack_input_names <- which_gui_inputs(inputs)
        pack_inputs <- shiny::reactiveValuesToList(input)[pack_input_names]
        pack$input <- pack_inputs
        pack$datatab <- r2datatab(gui$datatab)
        pack$filetag <- r2filetag(gui$filetag)
        pack$compounds <- r2compounds(gui$compounds)
        pack$paths <- list()
        pack$paths$data <- gui$paths$data

        })
    pack
}

which_metfrag_inputs <- function() {
    names(INPUTS_METFRAG)
}

which_gui_inputs <- function(inputs) {
    
    c(GUI_ALL_INPUTS,which_metfrag_inputs())
}


which_gui_select_inputs <- function() {
    GUI_SELECT_INPUTS
    }

which_gui_numeric_inputs <- function() {
    GUI_NUMERIC_INPUTS
    }

which_gui_text_inputs <- function() {
    GUI_TEXT_INPUTS
}

which_gui_radio_inputs <- function() {
    GUI_RADIO_INPUTS
}

unpack_app_state_metfrag <- function(session,envopts,input,packed_state) {
    shiny::isolate({
        for (inp in INPUTS_METFRAG_NUMERIC) {
            shiny::updateNumericInput(session=session,
                                      inputId = inp$name,
                                      value = packed_state$input[[inp$name]])
            
        }

        for (inp in INPUTS_METFRAG_SELECT_STANDARD) {
            shiny::updateSelectInput(session = session,
                                     inputId = inp$name,
                                     selected = packed_state$input[[inp$name]])
            
        }

        ## Check if the local db is, in fact, in the system db directory.
        locdb = packed_state$input[["mf_local_database"]]
        if (!length(locdb)==0L && !nchar(locdb)==0) {
            fn_absdb = file.path(envopts$metfrag$db_dir,locdb)
            if (file.exists(fn_absdb)) {
                dtnms = data.table::fread(file=fn_absdb,nrows=1L)
                nms = names(dtnms)

                for (inp in names(INPUTS_METFRAG_SELECT_LOCAL_DBCH)) {
                    shiny::updateSelectInput(session = session,
                                             inputId = inp,
                                             choices = c(character(0),nms),
                                             selected = packed_state$input[[inp]])
                }

                for (inp in names(INPUTS_METFRAG_SELECT_LOCAL_OTHER)) {
                    shiny::updateSelectInput(session = session,
                                             inputId = inp,
                                             selected = packed_state$input[[inp]])
                }
                
            }
        }
            
    })
}
unpack_app_state <- function(session,envopts,input,top_data_dir,project_path,packed_state) {
    shiny::isolate({
        for (inp in which_gui_select_inputs()) {
            shiny::updateSelectInput(session = session,
                                     inputId = inp,
                                     selected = packed_state$input[[inp]])
        }
        
        for (inp in which_gui_numeric_inputs()) {
            shiny::updateNumericInput(session = session,
                                      inputId = inp,
                                      value = packed_state$input[[inp]])
        }
        
        for (inp in which_gui_text_inputs()) {
            shiny::updateTextInput(session = session,
                                   inputId = inp,
                                   value = packed_state$input[[inp]])
        }
        
        for (inp in which_gui_radio_inputs()) {
            shiny::updateRadioButtons(session = session,
                                      inputId = inp,
                                      selected = packed_state$input[[inp]])
        }


        unpack_app_state_metfrag(session=session,
                                 envopts=envopts,
                                 input=input,
                                 packed_state=packed_state)
        
        gui <- create_gui(project_path=project_path)
        gui$compounds$lists <- packed_state$compounds$lists
        gui$datatab$file <- packed_state$datatab$file
        gui$datatab$adduct <- packed_state$datatab$adduct
        gui$datatab$tag <- packed_state$datatab$tag
        gui$datatab$set <- packed_state$datatab$set

        gui$filetag$file <- packed_state$filetag$file
        gui$filetag$tag <- packed_state$filetag$tag

        x <- packed_state$paths$data
        gui$paths$data = if (length(x)>0 && nchar(x)>0) basename(x) else ""
        if (!dir.exists(file.path(top_data_dir,gui$paths$data))) {warning("Data directory ", gui$paths$data, " does not exist. You must select one.")}
        gui
    })

 

}

input2conf_setup <- function(input,gui,conf=list()) {
    if (length(conf)==0L) {
        conf$compounds <- list()
        conf$summary_table <- list()
        conf$debug <- F
    }

    conf$compounds$lists <- gui$compounds$lists
    conf$paths$data = basename(gui$paths$data)
    conf
}

input2conf_extract <- function(input,conf) {
    conf$tolerance = list()
    conf$extract = list()
    conf$tolerance[["ms1 fine"]] <- paste(input$ms1_fine,input$ms1_fine_unit)
    conf$tolerance[["ms1 coarse"]] <- paste(input$ms1_coarse,input$ms1_coarse_unit)
    conf$tolerance[["eic"]] <- paste(input$ms1_eic,input$ms1_eic_unit)
    conf$tolerance[["rt"]] <- paste(input$ms1_rt_win,input$ms1_rt_win_unit)
    conf$extract$missing_precursor_info <- input$missingprec
    conf

}


input2conf_prescreen <- function(input,conf) {
    conf$prescreen = list()
    conf$prescreen[["ms1_int_thresh"]] <- input$ms1_int_thresh
    conf$prescreen[["ms2_int_thresh"]] <- input$ms2_int_thresh
    conf$prescreen[["s2n"]] <- input$s2n
    conf$prescreen[["ret_time_shift_tol"]] <- paste(input$ret_time_shift_tol,input$ret_time_shift_tol_unit)
    conf
}

input2conf_figures <- function(input,conf) {
    conf$figures = list()
    conf$figures$rt_min <- paste(input$plot_rt_min,input$plot_rt_min_unit)
    conf$figures$rt_max <- paste(input$plot_rt_max,input$plot_rt_max_unit)
    conf$figures$ext <- input$plot_ext
    conf
    
}

input2conf_report <- function(input,conf) {
    conf$report = list()
    conf$report$author <- input$rep_aut
    conf$report$title <- input$rep_tit
    conf

}

input2conf_metfrag <- function(input,conf) {

    mfpar = metfrag_param(MetFragDatabaseType=input$mf_database_type,
                          FragmentPeakMatchAbsoluteMassDeviation = input$mf_fragment_peak_match_absolute_mass_deviation,
                          FragmentPeakMatchRelativeMassDeviation = input$mf_fragment_peak_match_relative_mass_deviation,
                          DatabaseSearchRelativeMassDeviation = input$mf_database_search_relative_mass_deviation,
                          MetFragCandidateWriter = input$mf_metfrag_candidate_writer,
                          SampleName = input$proj_list,
                          MaximumTreeDepth = input$mf_maximum_tree_depth,
                          MetFragPreProcessingCandidateFilter = paste(input$mf_pre_processing_candidate_filter,collapse=","),
                          MetFragPostProcessingCandidateFilter = paste(input$mf_post_processing_candidate_filter,collapse=","))

    ## TODO: FIXME: We need to move away from unit weights from some
    ## point. This needs some extra widgets (Sigh!). 
    insc = sapply(input$mf_scores_intrinsic,function(x) 1.0)
    names(insc) = input$mf_scores_intrinsic

    ldbsc = sapply(input$mf_local_db_col_scores,function(x) 1.0)
    names(ldbsc) = input$mf_local_db_col_scores

    conf$metfrag = metfrag4conf(db_file = input$mf_local_database,
                                param = mfpar,
                                intrinsic_scores = insc,
                                database_scores = ldbsc,
                                cand_parameters = input$mf_local_db_col_ident,
                                collect_candidates = input$mf_local_db_col_coll,
                                nproc = input$mf_proc)
    conf
    
}

app_update_conf <- function(input,gui,envopts,fconf,m) {
    for (fstrp in fconf) {
        fstr = paste0("input2conf_",fstrp)
        m$conf = do.call(fstr,list(input,conf=m$conf))
    }
    m$run <- new_runtime_state(project=gui$paths$project,
                               envopts = envopts,
                               conf=m$conf)
    m
}

app_state2state <- function(input,gui,envopts,m=NULL) {
    if (is.null(m)) m = new_project(project = gui$paths$project,
                                    envopts = envopts)
    m$conf = input2conf_setup(input=input,
                              gui=gui)

    m = app_update_conf(input=input,
                        gui=gui,
                        envopts=envopts,
                        fconf = c("extract",
                                  "prescreen",
                                  "figures",
                                  "report",
                                  "metfrag"),
                        m=m)
                        
    m$input$tab$mzml = gui2datatab(gui)
    
    m
}
    
get_sets <- function(gui) {
    ## TODO FIXME
    ## Think about this
    fn_lists <- file.path(gui$paths$project,gui$compounds$lists)

    df <- fread(file=fn_lists)
    if (!
    res = df[,unique(set)]
    if (length(res)==0L) res = "ALL"
}


gen_dfiles_tab <- function(gui) {
    curr_file <- gui$filetag$file
    curr_tag <- gui$filetag$tag
    
    res <- data.table(file=curr_file,tag=curr_tag)
    res
    
}

gui2datatab <- function(gui) {
    df <- data.table(tag=as.character(gui$datatab$tag),
                     adduct=as.character(gui$datatab$adduct),
                     set=as.character(gui$datatab$set),
                     file=as.character(gui$datatab$file))
    df
                     
}

## SHINY HELPERS: COMPOUND INDEX


## Creating compound index table
##
## Take `summ', group first by set, adduct and id. Then, pick only the
## maximum quality rows in the sub-table. Calculate mean rt and use
## this as the group rt. This is, then, a row representing the group
## (of tags, CEs) in the index.
gen_cindex <- function(summ,sorder,cols = CINDEX_COLS,by. = CINDEX_BY) {
    if (NROW(summ) == 0L) return(NULL)
    allc <- c(by.,cols)
    xsumm <- summ[,..allc]
    setnames(xsumm,old="ms1_rt",new="rt",skip_absent=T)
    res <- xsumm[,.SD[max(qlt_ms1)==qlt_ms1][max(qlt_ms2)==qlt_ms2],by=by.]
    res <- res[,c("mz","rt","Name","qlt_ms1","qlt_ms2"):=.(first(mz),
                                                         first(mean(rt)),
                                                         first(Name),
                                                         first(qlt_ms1),
                                                         first(qlt_ms2)),
               by=by.]
    res <- res[,unique(.SD),by=by.]
   
    sorder <- unique(sorder)
    wna <- which(sorder=="nothing"); if (length(wna)>0L) sorder <- sorder[-wna]
    quality <- which("quality"==sorder)
    if (length(quality)>0L) {
        pre <- head(sorder,quality-1L)
        post <- tail(sorder,-quality)
        sorder <- c(pre,"qlt_ms1","qlt_ms2",post)
    }
    ord <- rep(1L,length(sorder))

    if ("qlt_ms1" %in% sorder) {
        ind <- which(sorder %in% c("qlt_ms1","qlt_ms2"))
        ord[ind] <- -1L
    }
    if (length(sorder)>0) setorderv(res,cols=sorder,order=ord)
    setnames(res,old="rt",new="rt(ms1)")
    res
}


cindex_from_input <- function(clabs,sort_catg=character(4),summ) {
    grp <- if (isTruthy(clabs)) setdiff(CINDEX_BY,clabs) else CINDEX_BY
    sorder <- setdiff(sort_catg,clabs)
    gen_cindex(summ,sorder=sorder,by=grp)
}

get_cindex_key <- function(cindex) {
    ## Select only valid category names.
    x <- which(CINDEX_BY %in% names(cindex))
    CINDEX_BY[x]
}

get_cindex_parents <- function(summ,ckey,kvals,labs) {
    ## Get kvals part of summ.
    tab <- summ[(kvals),on=names(kvals)][,unique(.SD),.SDcols=labs,by=ckey] #get_data_from_key(summ,kvals)
    tab[,item:=do.call(paste,c(.SD,list(sep=";"))),.SDcol=labs]
    keys <- names(tab)[names(tab)!="item"]
    data.table::setkeyv(tab,keys)
    tab
}

get_cindex_kval <- function(cindex,row,key) {
    ## Accounting for not fully initialised state.
    if (!is.numeric(row) || is.na(row) || length(key)==0L || is.na(key) || NROW(cindex)==0L) return(NULL)
    
    rowtab <- cindex[(row),..key]
    res <- lapply(rowtab,function (x) x[[1]])
    names(res) <- key
    res
}

get_summ_subset <- function(summ,ptab,paritem,kvals) {
    select <- ptab[item==(paritem)]
    tab <- get_data_from_key(summ,kvals)[select,nomatch=NULL,on=key(ptab)]
    if ("an.1" %in% names(tab)) tab[,an.1:=NULL] #TODO: This is
                                                 #probably a lousy
                                                 #hack.
    tab
}

get_ltab <- function(summ_subs,cols=c("an","ms2_rt")) {
    tab <- summ_subs
    if (NROW(tab)==1L && is.na(tab$an)) return(data.table::data.table(item=character()))
    tab[is.na(ms2_sel),ms2_sel:=F] #TODO FIXME: Check why NAs exist at all?
    tab[,passval:=fifelse(qa_pass==T,"OK","BAD")]
    tab[ms2_sel==T,passval:="SELECTED"]
    res <- tab[,item:=do.call(paste,c(.SD,list(sep=";"))),.SDcols=c(cols,"passval")]
    data.table::setkey(res,"ms2_rt")
    res
}
update_on_commit_chg <- function(summ,input,ptab,ltab) {
    n_ms1_rt = input$chg_ms1_rt
    n_ms1_int = input$chg_ms1_int
    
    n_qa = rep(F,length(QABOX_VALS))
    names(n_qa) = QABOX_VALS
    n_qa[input$qabox] = T

    n_ms2_sel = input$chg_ms2sel
    
    sel_par <- input$sel_parent_trace
    sel_spec <- input$sel_spec

    pkvals <- ptab[item==(sel_par),.SD,.SDcols=intersect(SUMM_KEY,names(ptab))]
    lkvals <- ltab[item==(sel_spec),.SD,.SDcols=intersect(SUMM_KEY,names(ltab))]
    kvals <- c(as.list(pkvals),as.list(lkvals))
    kvals <- kvals[unique(names(kvals))]
    if ('an' %in% names(kvals) && n_ms2_sel) {
        rkvals <- kvals[!(names(kvals) %in% 'an')]
        rktab <- tabkey(summ,kvals=rkvals)
        tabsel <- summ[rktab,.(an,ms2_sel)]
        ansel <- tabsel[ms2_sel == T,an]
        print('ansel')
        print(ansel)
        if (length(ansel)!=0) {
            rktab$an = ansel
            summ[rktab,ms2_sel:=F]
        }
        
        
    }

    tgts <- c("ms1_rt","ms1_int",names(n_qa),"ms2_sel")
    srcs <- c(list(n_ms1_rt,n_ms1_int),as.list(n_qa),as.list(n_ms2_sel))

    the_row <- tabkey(summ,kvals=kvals)
    summ[the_row,(tgts):=..srcs]
    summ[,an.1:=NULL] #FIXME: an.1 pops up somewhere.
    qflg <- QA_FLAGS[!(QA_FLAGS %in% "qa_pass")]
    summ[the_row,qa_pass:=apply(.SD,1,all),.SDcols=qflg]
    summ
}


get_mprop_ms2_metadata <- function(ltab_entry) {
 res <- list(rt=NA_real_,int=NA_real_,qa=character(0),ms2_sel=F)

 if (NROW(ltab_entry)==0L) return(res)
 
 res$rt = ltab_entry$ms1_rt
 res$int = ltab_entry$ms1_int
 z <- ltab_entry[.SD,.SDcols=patterns("qa_ms[12].*")]
 lqa_vals <- as.list(ltab_entry[,.SD,.SDcols=patterns("qa_ms[12].*")])
 qa_names <- names(lqa_vals)
 res$qa <- qa_names[as.logical(lqa_vals)]
 res$ms2_sel = ltab_entry$ms2_sel

 res
  
}
