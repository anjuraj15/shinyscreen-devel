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



#' @title Create an Empty `envopts` Object
#' @details An `envopts` object is Shinyscreen way to store settings
#'     related to a specific computing environment. Information such
#'     as the run time path to a MetFrag JAR will vary from one to
#'     another setup and we need to convey this to the `shinyscreen`
#'     pipeline.
#' 
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
#' @param java_bin `character(1)`, a path to java runtime. If no path
#'     has been supplied, we will try to detect it.
#' @param metfrag_max_proc `integer(1)`, maximum number of CPU cores
#'     available for MetFrag. If no number has been supplied, we will
#'     try to detect the number of logical CPUs and go with that.
#' @param no_structure_plots `logical(1)`, if T, structures will not
#'     be plotted, even when it is possibile.
#' @return An `envopts` object.
#' @author Todor Kondić
empty_envopts <- function(projects=NULL,
                          top_data_dir=NULL,
                          users_dir=NULL,
                          metfrag_db_dir=NULL,
                          metfrag_jar=NULL,
                          java_bin=NULL,
                          metfrag_max_proc=NULL,
                          no_structure_plots=NULL) {
    
    ## Creates an empty `envopts' object. Works in conjunction with
    ## shinyscreen::init().
    res = list(projects=projects,
               top_data_dir=top_data_dir,
               users_dir=users_dir,
               metfrag=list(db_dir=metfrag_db_dir,
                            jar=metfrag_jar,
                            java_bin=java_bin,
                            max_proc=metfrag_max_proc),
               no_structure_plots=no_structure_plots)
    class(res) = c("envopts","list") #Just to officially make it an
                                        #object.
    res
}



seal_envopts <- function(o) {


    ## Assign defaults to `envopts'. Works in conjunction with
    ## `shinyscreen::init()'.
    zero_str = c("projects","top_data_dir","users_dir")
    for (z in zero_str) {
        if (is.null(o[[z]])) o[[z]]=""
    }

    if (is.null(o$metfrag$db_dir)) o$metfrag$db_dir=""

    if (is.null(o$metfrag$java_bin)) o$metfrag$java_bin=Sys.which("java")
    if (is.null(o$metfrag$jar)) o$metfrag$jar = ""
    if (is.null(o$metfrag$max_proc)) o$metfrag$max_proc = parallel::detectCores()
    if (is.null(o$no_structure_plots)) o$no_structure_plots = F 
    check_dir_absent(o$projects,what="projects-dir")
    o$projects = norm_path(o$projects)

    check_dir_absent(o$top_data_dir,what="top-data-dir")
    o$top_data_dir=norm_path(o$top_data_dir)
    
    check_dir_absent(o$users_dir,what="users-dir")
    o$users_dir=norm_path(o$users_dir)

    check_dir_absent(o$metfrag$db_dir,what="mf-db-dir")
    o$metfrag$db_dir = norm_path(o$metfrag$db_dir)

    check_file_absent(o$metfrag$jar,what="mf-jar")
    if (nchar(o$metfrag$jar)>0) o$metfrag$jar = norm_path(o$metfrag$jar)

    check_not_integer(value=o$metfrag$max_proc,
                      what="metfrag-max-proc")
    


    if (nchar(o$metfrag$jar)>0L) {
        check_file_absent(o$metfrag$java_bin,"java-bin")
    }

    check_not_logical(value=o$no_structure_plots,
                      what="no-structure-plots")

    o
}


is_metfrag_available <- function(e) {
    nchar(e$metfrag$jar)>0L
}

is_metfrag_local_available <- function(e) {
    is_metfrag_available(e) && nchar(e$metfrag$db_dir)>0L
}


get_envopts_fn <- function(dir=tools::R_user_dir(package="shinyscreen",
                                                 which="config")) {
    file.path(dir,FN_ENVOPTS)
}

load_envopts <- function(dir=tools::R_user_dir(package="shinyscreen",
                                               which="config")) {
    cfgfile = get_envopts_fn(dir=dir)
    if (file.exists(cfgfile)) readRDS(cfgfile) else empty_envopts()
    
}

save_envopts <- function(o,dir=tools::R_user_dir(package="shinyscreen",
                                                 which="config")) {
    cfgfile = get_envopts_fn(dir=dir)
    dr = dirname(cfgfile)
    dir.create(path = dr, showWarnings = F, recursive=T)
    saveRDS(o,cfgfile)
}
