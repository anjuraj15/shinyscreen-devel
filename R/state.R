## Copyright (C) 2020,2021 by University of Luxembourg

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




runtime_from_conf <- function(run,conf) {
    browser()
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

    tmp <- conf$paths$datatab
    datatab <- if (!is.null(tmp)) {
                   if (file.exists(tmp)) {
                       tmp
                   } else {
                       file.path(run$paths$project,tmp)
                   }
               } else {
                   file.path(run$paths$project,FN_DATA_TAB)
               }
    if (!file.exists(datatab)) stop("A CSV file with data file entries does not exist (`paths$datatab' in config).")
    datatab <- normalizePath(datatab)
    run$paths$datatab <- datatab
    run

}


## This helps decouple "cross-platform" configuration from the
## (file-)system dependent facts.
##' @export
new_runtime_state <- function(project,conf=NULL) {
    
    if (!is.character(project)) stop("Argument `project' must be a character string.")
    if (!dir.exists(project)) stop('Project directory either does not exist, or is unreadable.')
    project_path <- normalizePath(project)
    project <- basename(project)
    run <- list()
    run$project <- project
    run$paths$project <- project_path

    run$paths$data <- if (is.null(conf$paths$data)) {
                          project_path
                      } else {
                          run$paths$data <- normalizePath(conf$paths$data)
                      }
    
    if (!dir.exists(run$paths$data)) stop("Path to data directory either does not exist, or is inaccesible.")

    if (!is.null(conf)) runtime_from_conf(run=run,conf=conf) else run

}

##' @export
new_empty_project <- function(project) {
    m <- new_state()
    m$run <- new_runtime_state(project)
    m
}

##' @export
new_project <- function(project) {
    m <- new_state()
    m$run <- new_runtime_state(project)
    fn_conf <- file.path(m$run$paths$project,FN_CONF)
    m$conf <- read_conf(fn_conf)
    m$run <- new_runtime_state(project,conf=m$conf)
    m
}

##' @export
import_project <- function(project) {
    m <- new_project(project)
    fn_state <- file.path(m$run$paths$project,FN_STATE)
    if (!file.exists(fn_state)) stop(paste0("Cannot import project. State file ",fn_state," does not exist, or is unreadable."))
    lm <- readRDS(file=fn_state)
    lm$run <- m$run
    lm$conf <- m$conf
    lm
}

##' @export
refresh_state <- function(m) {
    m$run <- new_runtime_state(m$run$project,conf=m$conf)
    m
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

read_conf <- function(fn) {
    cf <- yaml::yaml.load_file(fn)
    fnl <- cf$compounds$lists
    if (length(fnl)>0) {
        nms <- character(0)
        for (i in 1:length(fnl)) {
            nms <- gen_uniq_lab(nms,pref = 'L')
        }
        names(fnl) <- nms
        
    }
    cf$compounds$lists <- fnl
    ## conf_trans(cf)
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

init_state <- function(m) {
    m$out$tab <- list()
    m$input$datafiles <- NULL
    m$input$tab$mzml <- EMPTY_MZML
    lab <- gen_uniq_lab(list(),pref="L")
    m$input$tab$lists <- list()
    m$input$tab[[lab[[1]]]] <- EMPTY_CMPD_LIST
    m$out$tab$comp <- EMPTY_COMP_TAB
    m
}

base_conf <- function () {
    m <- list()
    m$conf <- list(project=NA_character_,
                   compounds=list(lists=list(),
                                  sets=""),
                    debug = F)
    m
}

extr_conf <- function(m) {
    m$conf$tolerance <- list("ms1 coarse"=MS1_ERR_COARSE,
                             "ms1 fine"=MS1_ERR_FINE,
                             "eic"=EIC_ERR,
                             "rt"=RT_EXTR_ERR)
    m$conf$extract <- list(missing_precursor_info=DEF_CONF_MISSING_PCS)

    m
}

presc_conf <- function(m) {
    m$conf$prescreen <- list("ms1_int_thresh"=MS1_INT_THOLD,
                             "ms2_int_thresh"=MS2_INT_THOLD,
                             "s2n"=MS1_SN_FAC,
                             "ret_time_shift_tol"=RT_SHIFT_ERR)
    m
}

fig_conf <- function(m) {
    m$conf$figures$rt_min <- "NA_real_ min"
    m$conf$figures$rt_max <- "NA_real_ min"
    m$conf$figures$ext <- "pdf"
    m
}

new_conf <- function() fig_conf(
                           presc_conf(
                               extr_conf(
                                   base_conf())))
