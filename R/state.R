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



runtime_from_conf <- function(run,conf) {
    lst_cmpl <- conf$compounds$lists
    lst_fn_cmpl <- lapply(names(lst_cmpl),function (nm) {
        bfn_cmpl <- lst_cmpl[[nm]]
        fn <- file.path(run$paths$project,bfn_cmpl)
        if (!file.exists(fn)) stop("File ", fn, " does not exist in ", run$paths$project," .")
        fn
    })
    names(lst_fn_cmpl) <- names(lst_cmpl)
    run$paths$compounds$lists <- lst_fn_cmpl

    fn_sets <- file.path(run$paths$project,conf$compounds$sets[[1]]) #It's always only one.
    if (!file.exists(fn_sets)) stop("File ", basename(fn_sets), " does not exist in ", run$paths$project," .")
    run$paths$compounds$sets <- fn_sets

    run$paths$datatab <- file.path(run$paths$project,FN_DATA_TAB)
    run
}

reinit_run_data <- function(projects,top_data_dir,project,run=NULL) {
    if (!is.null(run)) {
        olddata <- run$paths$data
        oldproject <- basename(run$paths$project)
        if (project != oldproject) {
            message("Project has been renamed to: ",project)
            message("Old project name was: ", oldproject)
        }
        if (isTruthy(olddata)) run$paths$data <- file.path(top_user_dir,basename(olddata))
    }
    run$project <- project
    run$paths$project <- file.path(projects,project)
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
                       db_path="")

        ## If jar defined.
        metfrag$runtime = envopts$metfrag$jar
        if (nchar(metfrag$runtime)>0L) {
            metfrag$cando_metfrag = T
            metfrag$cando_remote = T
        }

        ## If `conf` not null, fully setup metfrag.
        if (!is.null(conf)) {

            ## Check for local DBs.
            root = envopts$metfrag$db_dir
            bname = conf$metfrag$db_file
            if (nchar(bname)>0L) {
                fpath = file.path(root,bname)
                check_file_absent(fpath,what="metfrag-db-file")
                metfrag$cando_local = T
                metfrag$db_path = fpath
            }

            ## If MetFrag possible, set up the file structure.
            if (metfrag$cando_metfrag) {
                mfdir = file.path(project_path,"metfrag")
                dir.create(mfdir)
                metfrag$path=mfdir
                subpaths = list(results = "results",
                                config = "config",
                                spec = "spec",
                                log = "log")
                for (x in subpaths) dir.create(file.path(mfdir,x),showWarnings=F)
                metfrag$subpaths = subpaths


            }
            
        }

        metfrag
    }

    ## Check if all OK with project name.
    check_notastring(project,"project")
    project_path = norm_path(project)
    check_dir_absent(project_path,"project")
    project = basename(project)
    run = list()
    run$project = project
    run$paths$project = project_path

    run$metfrag = .metfrag(project_path)

    
    run$paths$data = if (is.null(conf$paths$data)) {
        project_path
    } else {
        norm_path(conf$paths$data)
    }
    
    check_dir_absent(run$paths$data,"data-dir")

    if (!is.null(conf)) runtime_from_conf(run=run,conf=conf) else run

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
##' @seealso [shinyscreen::envopts()],
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
    m$conf$compounds$lists = label_cmpd_lists(m$conf$compounds$lists)
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

write_conf <- function(m,fn) {
    m$conf$paths$data <- get_fn_ftab(m)
    if (NROW(m$input$tab$mzml)>0) tab2file(tab=m$input$tab$mzml,file=file.path(m$run$paths$project,FN_DATA_TAB))
    yaml::write_yaml(x=m$conf,file=fn)
    
    
    
}
write_state <- function(m,fn_conf) {
    write_conf(m,fn_conf)
    tab2file(tab=m$input$tab$mzml,file=file.path(m$run$paths$project,FN_DATA_TAB))
}

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

metfrag_conf <- function(m) {
    ## MetFrag configuration defaults.
    metfrag = list(db_file = "")
    param = list(MetFragDatabaseType="",
                 FragmentPeakMatchAbsoluteMassDeviation=METFRAG_DEFAULT_ABSMASSDEV,
                 FragmentPeakMatchRelativeMassDeviation=METFRAG_DEFAULT_RELMASSDEV,
                 MetFragScoreTypes=METFRAG_DEFAULT_SCORES,
                 MetFragScoreWeights=METFRAG_DEFAULT_WEIGHTS,
                 MetFragCandidateWriter=METFRAG_DEFAULT_WRITER,
                 SampleName=METFRAG_SAMPLE_NAME,
                 MaximumTreeDepth=METFRAG_DEFAULT_MAX_TREE_DEPTH,
                 MetFragPreProcessingCandidateFilter=METFRAG_PREPFLT_CHOICES,
                 MetFragPostProcessingCandidateFilter=METFRAG_POSTPFLT_CHOICES)
    
    metfrag$param = param
    m$conf$metfrag = metfrag
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
    x = summ[ms2_sel==T,.SD,.SDcols=c(key(summ),"mz","SMILES","Formula","Name")]
    mrg_keys = c(intersect(key(ms2),key(summ)),"an")
    ms2[x,.(mz=i.mz,ms2_spectrum=encode_ms2_to_line(.SD[,c("mz","intensity")])),on=mrg_keys,by=.EACHI]
}


get_metfrag_targets <- function(summ,ms2) {
    ## Take the columns we need from summ.
    x = summ[ms2_sel==T,.SD,.SDcols=c(key(summ),"mz")]
    mrg_keys = c(intersect(key(ms2),key(summ)),"an")
    x=ms2[x,.(CE=CE,ion_mz=i.mz,mz,intensity),on=mrg_keys,by=.EACHI]
    ## Get column order so that `an' follows `CE'.
    resnms = setdiff(mrg_keys,"an")
    nms = union(union(resnms,"CE"),c("an","ion_mz","mz","intensity"))
    data.table::setcolorder(x,neworder = nms)
    setkeyv(x,unique(c(resnms,"CE","an")))
    x
    
}


metfrag_on_state <- function(m,fmany=metfrag_run_many) {
    tgts = get_metfrag_targets(m$out$tab$summ,m$extr$ms2)
    tgts[,write_metfrag_config(param=m$conf$metfrag$param,
                               path = m$run$metfrag$path,
                               subpaths = m$run$metfrag$subpaths,
                               stag = paste0(.BY,collapse="_"),
                               adduct = adduct,
                               ion_mz = ion_mz[1],
                               spec = .SD[,c("mz","intensity")]),
         by=key(tgts)]
}


write_metfrag_config <- function(param,path,subpaths,stag,adduct,ion_mz,spec) {
    check_not_one(ion_mz,"ion_mz")
    check_not_one(adduct,"adduct")
    dir_res = subpaths$results
    dir_spec = subpaths$spec
    dir_conf = subpaths$config
    dir_log = subpaths$log

    f_spec = file.path(dir_spec,paste0(param$SampleName,"_",stag,".csv"))
    f_conf = file.path(dir_conf,paste0(param$SampleName,"_",stag,".conf"))
    f_log = file.path(dir_log,paste0(param$SampleName,"_",stag,".log"))
    f_res = file.path(dir_res,paste0(param$SampleName,"_",stag,".",param$MetFragCandidateWriter))

    withr::with_dir(path,{
        param = c(param,list(IonizedPrecursorMass=ion_mz,
                             IsPositiveIonMode=ifelse(grepl(r"(\+$)",adduct),"True","False"),
                             PrecursorIonMode=METFRAG_ADDUCT_SWITCHES[[adduct]],
                             ResultsPath=f_res,
                             PeakListPath=f_spec))
        data.table::fwrite(spec,file=f_spec,col.names=F,sep=" ")
        write_keyval_file(namedl=param,fname=f_conf)
    })

    c(f_conf=f_conf,
      f_log=f_log,
      f_spec=f_spec)
    
}


metfrag_run <- function(fn_jar, fn_conf, fn_log, mem = NA_character_, java_bin = "java") {
    ## Check if file exists.

    ## Assemble arguments.
    args <- c('-jar',fn_jar,fn_conf)
    ## If total heap memory given (in quantities like '4m', or '2g')
    ## then make this argument.
    if (!is.na(mem)) args <- c(paste0('-Xmx', mem),args)
    ## Start new java process.
    p <- processx::process$new(java_bin,args=args,stdout=fn_log,stderr='2>&1')
    p$wait()
    p$get_exit_status()
    
}


metfrag_run_many_w_futures <- function(fn_jar,fn_conf,fn_log, mem = NA_character_, java_bin = "java") {
    ntasks = length(fn_conf)
    procs = list()
    compl_prev = 0
    message("MetFrag started: ", ntasks," tasks.")
    for (n in 1:ntasks) {
        procs = c(future::future({
            
            st =  metfrag_run(fn_jar = fn_jar,
                              fn_conf = fn_conf[n],
                              fn_log = fn_log[n],
                              mem = mem,
                              java_bin = java_bin)
            st
            
            
        },seed = T), procs)

        reslv = which(sapply(procs,future::resolved))
        compl = length(reslv)
        
        if (compl > compl_prev) {
            message("Completed MetFrag tasks: ", compl, "/",ntasks)
            compl_prev <- compl
        }

        
    }
}


metfrag_run_many <- function(fn_conf, fn_log, mem = NA_character_, java_bin = "java") {
    ntasks = length(fn_conf)
     message("MetFrag started: ", ntasks," tasks.")
    for (n in 1:ntasks) {
        metfrag_run(fn_jar = fn_jar,
                    fn_conf = fn_conf[[n]],
                    fn_log = fn_log[[n]],
                    mem = mem,
                    java_bin = java_bin)
        
        message("Completed MetFrag tasks: ", n, "/",ntasks)
    }

    
}
