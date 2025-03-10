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

init_state <- function(m) {
    m$out$tab = list()
    m$input$datafiles = NULL
    m$input$tab$mzml = EMPTY_MZML
    lab = gen_uniq_lab(list(),pref="L")
    m$input$tab$lists = list()
    m$input$tab[[lab[[1]]]] = EMPTY_CMPD_LIST
    m$out$tab$comp = EMPTY_COMP_TAB
    m
}

#' @title Create Initial State
#' @details Creates an initial, bare state.
#' @return A state object.
#' @export
#' @author Todor Kondić
new_state <- function() {
    m <- new_conf()
    init_state(m)
}



runtime_from_conf <- function(run,envopts,conf) {
    lst_cmpl <- conf$compounds$lists
    lst_fn_cmpl <- lapply(lst_cmpl,function (lst) {
        bfn_cmpl <- lst
        fn <- file.path(run$paths$project,bfn_cmpl)
        if (!file.exists(fn)) stop("File ", fn, " does not exist in ", run$paths$project," .")
        fn
    })
    run$paths$compounds$lists <- lst_fn_cmpl

    run$paths$data = norm_path(file.path(envopts$top_data_dir,conf$paths$data))
    check_dir_absent(run$paths$data,"data-dir", strict=T)
    run$paths$datatab <- file.path(run$paths$project,FN_DATA_TAB)
    run
}

#' @title Create Runtime Configuration
#' @details This is a part of the configuration that can only be
#'     diagnosed at runtime.
#' @param project `character(1)`, project title (and the directory name under `projects`)
#' @param conf `conf`, configuration object, optional 
#' @return `run` substate
#' @author Todor Kondić
#' @export
new_runtime_state <- function(project,envopts,conf=NULL) {

    ## A little helper for metfrag.
    .metfrag <- function(project_path) {


        ## Defensive default.
        metfrag = list(cando_local=F,
                       cando_remote=F,
                       cando_metfrag=F,
                       path="",
                       subpaths=list(results="",
                                     config="",
                                     spec="",
                                     log=""),
                       runtime="",
                       db_file="",
                       param=NULL)

        ## If jar defined.
        metfrag$runtime = envopts$metfrag$jar
        metfrag$java_bin = envopts$metfrag$java_bin
        if (nchar(metfrag$runtime)>0L) {
            metfrag$cando_metfrag = T
            metfrag$cando_remote = T
        }

        ## If `conf` not null, fully setup metfrag.
        if (!is.null(conf)) {

            

            ## If MetFrag possible, set up the file structure.
            if (metfrag$cando_metfrag) {
                mfdir = file.path(project_path,"metfrag")
                dir.create(mfdir, showWarnings = F)
                metfrag$path=mfdir
                subpaths = list(results = "results",
                                config = "config",
                                spec = "spec",
                                log = "log")
                for (x in subpaths) dir.create(file.path(mfdir,x),showWarnings=F)
                metfrag$subpaths = subpaths

                


            }


            ## Check for local DBs.
            root = envopts$metfrag$db_dir
            bname = conf$metfrag$db_file
            if (nchar(bname)>0L) {
                fpath = file.path(root,bname)
                check_file_absent(fpath,what="metfrag-db-file")
                metfrag$cando_local = T
                metfrag$db_file = fpath

                # Check if names exist in the database (if local).
                dbnms = colnames(fread(fpath,nrows=1L))
                check_key_absent(c(names(conf$metfrag$database_scores),
                                   conf$metfrag$cand_parameters,
                                   conf$metfrag$collect_candidates),
                                 dbnms,what="local-metfrag-database")



            }

            ## Create score and weight entries.
            param = conf$metfrag$param

            scores = c(conf$metfrag$intrinsic_scores,conf$metfrag$database_scores)
            param$MetFragScoreTypes = paste0(names(scores),collapse = ",")
            param$MetFragScoreWeights = paste0(scores,collapse = ",")
            ## Fully expanded params end up in run object.
            metfrag$param = param

            
        }

        metfrag
    }

    ## Check if all OK with project name.
    check_notastring(project,"project",strict=T)
    project_path = file.path(envopts$projects,basename(project))
    check_dir_absent(project_path,"project_path", strict=T)
    project = basename(project)
    run = list()
    run$project = project
    run$paths$project = project_path

    run$metfrag = .metfrag(project_path)

    
    run$paths$data = ""
    
    

    if (!is.null(conf)) runtime_from_conf(run=run,envopts=envopts,conf=conf) else run

}

#' @title Create a Bare Project
#' @details This function creates a `shinyscreen` project with no
#'     configuration. This is not enough to run a pipeline, but will
#'     start things for incremental configuration using the GUI.
#' @param project `character(1)`, path to a directory containing all the projects
#' @param envopts `envopts`, an `envopts` object. 
#' @return New project `state`
#' @rdname new_project
#' @seealso [shinyscreen::new_state()]
#' @author Todor Kondić
#' @export
new_empty_project <- function(project,envopts) {
    m <- new_state()
    m$run <- new_runtime_state(project,envopts)
    m
}

##' @export
##' @title Create a New Fully Defined Project
##' @param project `character(1)`, path to a directory containing all
##'     the projects
##' @param envopts `envopts`, an `envopts` object.
##' @param datatab `datatab`, a `data.table` describing data files,
##'     optional
##' @param conf `conf`, a configuration object, optional
##' @details After the call to `new_project`, the `shinyscreen`
##'     pipeline should be ready to start.
##' @rdname new_project
##' @seealso [shinyscreen::empty_envopts()],
##'     [shinyscreen::new_empty_project()]
##' @author Todor Kondić
new_project <- function(project,envopts,datatab=NULL,conf=NULL) {
    m = new_state()
    m$run = new_runtime_state(project,envopts=envopts)
    fn_conf = file.path(m$run$paths$project,FN_CONF)
    m$conf = if (is.null(conf)) {
                 check_file_absent(fn_conf,what="conf-file")
                 yaml::yaml.load_file(fn_conf)
             } else conf 
    ## m$conf$compounds$lists = label_cmpd_lists(m$conf$compounds$lists)
    m$run = new_runtime_state(project,envopts=envopts,conf=m$conf)
    if (!is.null(datatab)) {
        m$input$tab$mzml = datatab
    }
    
    m
}

##' @export
import_project <- function(project,envopts=envopts) {
    m <- new_project(project,envopts=envopts)
    fn_state <- file.path(m$run$paths$project,FN_STATE)
    if (!file.exists(fn_state)) stop(paste0("Cannot import project. State file ",fn_state," does not exist, or is unreadable."))
    lm <- readRDS(file=fn_state)
    lm$run <- m$run
    lm$conf <- m$conf
    lm
}

##' @export
new_rv_state <- function() react_v(m=list2rev(new_state()))

## write_conf <- function(m,fn) {
##     m$conf$paths$data <- get_fn_ftab(m)
##     if (NROW(m$input$tab$mzml)>0) tab2file(tab=m$input$tab$mzml,file=file.path(m$run$paths$project,FN_DATA_TAB))
##     yaml::write_yaml(x=m$conf,file=fn)
    
    
    
## }
## write_state <- function(m,fn_conf) {
##     write_conf(m,fn_conf)
##     tab2file(tab=m$input$tab$mzml,file=file.path(m$run$paths$project,FN_DATA_TAB))
## }

label_cmpd_lists <- function (lists) {
    if (length(lists)>0) {
        nms <- character(0)
        for (i in 1:length(lists)) {
            nms <- gen_uniq_lab(nms,pref = 'L')
        }
        names(lists) <- nms
        
    }

}

read_conf <- function(fn) {
    cf <- yaml::yaml.load_file(fn)
    cf$compounds$lists <- label_cmpd_lists(cf$compounds$lists)
    cf
}



##' @export
get_fn_comp <- function(m) {
    file.path(m$run$paths$project,FN_COMP_TAB)
}

##' @export
get_fn_summ <- function(m) {
    file.path(m$run$paths$project, FN_SUMM)
}

##' @export
get_fn_extr <- function(m) {
    file.path(m$run$paths$project, "extracted.rds")
}

##' @export
get_fn_conf <- function(m) {
    file.path(m$run$paths$project, FN_CONF)
}


##' @export
get_fn_ftab <- function(m) {
    file.path(m$run$paths$project, FN_DATA_TAB)
}

base_conf <- function () {
    ## Base state.
    m <- list()
    m$conf <- list(project=NA_character_,
                   compounds=list(lists=list(),
                                  sets=""),
                    debug = F)
    
    class(m$conf) = c("conf","list")
    class(m) = "state"
    m
}

extr_conf <- function(m) {
    ## Extraction defaults.
    m$conf$tolerance <- list("ms1 coarse"=MS1_ERR_COARSE,
                             "ms1 fine"=MS1_ERR_FINE,
                             "eic"=EIC_ERR,
                             "rt"=RT_EXTR_ERR)
    m$conf$extract <- list(missing_precursor_info=DEF_CONF_MISSING_PCS)

    m
}

presc_conf <- function(m) {
    ## Prescreening defaults.
    m$conf$prescreen <- list("ms1_int_thresh"=MS1_INT_THOLD,
                             "ms2_int_thresh"=MS2_INT_THOLD,
                             "s2n"=MS1_SN_FAC,
                             "ret_time_shift_tol"=RT_SHIFT_ERR)
    m
}

fig_conf <- function(m) {
    ## Plotting defaults.
    m$conf$figures$rt_min <- "NA_real_ min"
    m$conf$figures$rt_max <- "NA_real_ min"
    m$conf$figures$ext <- "pdf"
    m
}

metfrag_param <- function(MetFragDatabaseType,
                          FragmentPeakMatchAbsoluteMassDeviation,
                          FragmentPeakMatchRelativeMassDeviation,
                          DatabaseSearchRelativeMassDeviation,
                          MetFragCandidateWriter,
                          SampleName,
                          MaximumTreeDepth,
                          MetFragPreProcessingCandidateFilter,
                          MetFragPostProcessingCandidateFilter) {
    
    list(MetFragDatabaseType=MetFragDatabaseType,
         FragmentPeakMatchAbsoluteMassDeviation=FragmentPeakMatchAbsoluteMassDeviation,
         FragmentPeakMatchRelativeMassDeviation=FragmentPeakMatchRelativeMassDeviation,
         DatabaseSearchRelativeMassDeviation=DatabaseSearchRelativeMassDeviation,
         MetFragCandidateWriter=MetFragCandidateWriter,
         SampleName=SampleName,
         MaximumTreeDepth=MaximumTreeDepth,
         MetFragPreProcessingCandidateFilter=MetFragPreProcessingCandidateFilter,
         MetFragPostProcessingCandidateFilter=MetFragPostProcessingCandidateFilter)
}

metfrag4conf <- function(db_file,
                         param,
                         intrinsic_scores,
                         database_scores,
                         cand_parameters,
                         collect_candidates,
                         nproc) {
    list(db_file=db_file,
         param = param,
         intrinsic_scores = intrinsic_scores,
         database_scores = database_scores,
         cand_parameters = cand_parameters,
         collect_candidates = collect_candidates,
         nproc = nproc)
}
                           

metfrag_conf <- function(m) {
    ## MetFrag configuration defaults.
    param = metfrag_param(MetFragDatabaseType="",
                          FragmentPeakMatchAbsoluteMassDeviation=METFRAG_DEFAULT_ABSMASSDEV,
                          FragmentPeakMatchRelativeMassDeviation=METFRAG_DEFAULT_RELMASSDEV,
                          DatabaseSearchRelativeMassDeviation=METFRAG_DB_SEARCH_RELDEV,
                          MetFragCandidateWriter=METFRAG_DEFAULT_WRITER,
                          SampleName=METFRAG_SAMPLE_NAME,
                          MaximumTreeDepth=METFRAG_DEFAULT_MAX_TREE_DEPTH,
                          MetFragPreProcessingCandidateFilter=paste(METFRAG_PREPFLT_CHOICES,collapse=","),
                          MetFragPostProcessingCandidateFilter=paste(METFRAG_POSTPFLT_CHOICES,collapse=","))
    
    
    m$conf$metfrag = metfrag4conf(db_file = "",
                                  param = param,
                                  intrinsic_scores = METFRAG_DEFAULT_SCORES,
                                  database_scores = list(),
                                  cand_parameters = character(0),
                                  collect_candidates = character(0),
                                  nproc = 1L)
    m
}

new_conf <- function() {
    o = metfrag_conf(
        fig_conf(
            presc_conf(
                extr_conf(
                    base_conf()))))

 
    o
}

encode_ms2_to_line <- function(ms2) {
    ## ms2 is a data.frame whose first column is mz and the second intensity.
    paste0(paste(ms2[[1]],ms2[[2]],sep=':'),collapse=';')
}


pack_ms2_w_summ <- function(summ,ms2) {
    ## Takes summ, finds entries with high quality spectra and subsets ms2 based on that.

    ## Take the columns we need from summ.
    x = summ[ms2_sel==T,.SD,.SDcols=c(key(summ),"mz","SMILES","Name")]
    mrg_keys = intersect(key(ms2),key(summ))
    mrg_keys = c(mrg_keys,"scan")
    ## ms2[x,.(mz=i.mz,ms2_spectrum=encode_ms2_to_line(.SD[,c("mz","intensity")])),on=mrg_keys,by=.EACHI]
    ms2[x,.(ion_mz=i.mz,mz,intensity),on=mrg_keys,by=.EACHI]
}



pack_project <- function(m,fn_arch) {
    ppath = m$run$paths$project
    project = m$run$project
    tmpdir = tempfile()
    write.csv(data.frame(),file=file.path(ppath,"file1.csv"))
    write.csv(data.frame(),file=file.path(ppath,"file2.csv"))
    dir.create(file.path(ppath,"subdir"))
    write.csv(data.frame(),file=file.path(ppath,"subdir","filesubdir.csv"))
    dir.create(tmpdir,recursive=T)
    file.copy(from=ppath,to=tmpdir,recursive=T)
    withr::with_dir(tmpdir,{
        tar(tarfile=fn_arch,compression="gzip")
    })
    fn_arch
}

join_compound_lists <- function(fname) {
    coll <- list()
    fields <- colnames(EMPTY_CMPD_LIST)
    coltypes <- c(ID="character",
                  SMILES="character",
                  Formula="character",
                  Name="character",
                  RT="numeric",
                  mz="numeric")
    l=0L
    for (fn in fname) {
        l = l + 1L
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
    if (length(fname)>0) rbindlist(l=c(list(EMPTY_CMPD_LIST), coll), use.names = T, fill = T) else EMPTY_CMPD_LIST

}

process_cmpd_sets <- function(cmpdlist) {
    if (nrow(cmpdlist)==0L) return(EMPTY_CMPD_LIST)
    ## Process sets.
    if (! ("set" %in% colnames(cmpdlist))) cmpdlist$set=NA_character_ else cmpdlist[,set:=as.character(set)]
    slugs = cmpdlist[,.(slug=gen_fname_slug(ORIG)),by="ORIG"]
    slugs[,slug:=uniqy_slugs(slug)]
    cmpdlist[slugs,set:=fifelse(is.na(set),i.slug,set),on="ORIG"]
    cmpdlist
}
