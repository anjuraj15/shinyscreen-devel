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
## This file contains functions which manipulate per-system and
## per-run parameters. For example, paths that may change from
## computer to computer, such as the location of the MetFrag JAR file.




mk_envopts <- function() {
    res = list()
    class(res) = c("envopts","list") #Just to officially make it an
                                     #object.
    res
}


#' @title Create a `envopts` Object
#' @details An `envopts` object is Shinyscreen way to store settings
#'     related to a specific computing environment. Information such
#'     as the run time path to a MetFrag JAR will vary from one to
#'     another setup and we need to convey this to the `shinyscreen`
#'     pipeline.
#' @param projects `character(1)`, a directory which contains all
#'     shinyscreen projects directories. A single project directory
#'     contains input and output files.
#' @param top_data_dir `character(1)`, a directory which contains all
#'     `data` directories. A single `data` directory contains `mzML`
#'     spectrometry data files.
#' @param metfrag_db_dir `character(1)`, a path to the directory which
#'     contains MetFrag databases.
#' @param metfrag_jar `character(1)`, a path to MetFrag JAR file.
#' @param users_dir `character(1)`, a location on the server side
#'     containing individual user directories.
#' @param java_bin `character(1)`, a path to jave runtime
#'     (optional). We try to detect this.
#' @param metfrag_max_proc `integer(1)`, maximum number of CPU cores
#'     available for MetFrag.
#' @return An `envopts` object.
#' @author Todor Kondić
envopts <- function(projects="",
                    top_data_dir="",
                    metfrag_db_dir="",
                    metfrag_jar="",
                    users_dir="",
                    java_bin=Sys.which("java"),
                    metfrag_max_proc = parallel::detectCores()) {
    res = mk_envopts()
    res = list(metfrag=list())
    
 
    check_dir_absent(projects,what="projects-dir")
    res$projects = projects
    check_dir_absent(top_data_dir,what="top-data-dir")
    res$top_data_dir=top_data_dir
    
    check_dir_absent(users_dir,what="users-dir")
    res$users_dir=users_dir

    check_dir_absent(metfrag_db_dir,what="mf-db-dir")
    res$metfrag$db_dir = norm_path(metfrag_db_dir)

    check_file_absent(metfrag_jar,what="mf-jar")

    check_not_integer(value=metfrag_max_proc,
                      what="metfrag_max_proc")
    
    res$metfrag$jar = norm_path(metfrag_jar)
    res$metfrag$max_proc = metfrag_max_proc
    if (nchar(res$metfrag$jar)>0L) {
        check_file_absent(java_bin,"java-bin")
        res$metfrag$java_bin = java_bin
    }

    res
}


is_metfrag_available <- function(e) {
    nchar(e$metfrag$jar)>0L
}

is_metfrag_local_available <- function(e) {
    is_metfrag_available(e) && nchar(e$metfrag$db_dir)>0L
}


get_envopts_fn <- function() {
    file.path(tools::R_user_dir(package="shinyscreen",
                                which="config"),
              FN_ENVOPTS)
}

load_envopts <- function() {
    cfgfile = get_envopts_fn()
    if (file.exists(cfgfile)) readRDS(cfgfile) else mk_envopts()
    
}

save_envopts <- function(o) {
    cfgfile = get_envopts_fn()
    dr = dirname(cfgfile)
    dir.create(path = dr, showWarnings = F, recursive=T)
    saveRDS(o,cfgfile)
}
