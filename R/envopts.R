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




#' @title Create a `envopts` Object
#' @details A `envopts` object is Shinyscreen way to store settings
#'     related to a specific computing environment. Information such
#'     as the run time path to a MetFrag JAR will vary from one to
#'     another setup and we need to convey this to the `shinyscreen`
#'     pipeline.
#' @param metfrag_db_dir `character(1)`, a path to the directory which contains MetFrag databases
#' @param metfrag_jar  `character(1)`, a path to MetFrag JAR file
#' @return An `envopts` object.
#' @author Todor Kondić
envopts <- function(metfrag_db_dir="",metfrag_jar="") {
    res = list(metfrag=list())
    class(res) = c("envopts","list") #Just to officially make it an
                                     #object.

    check_dir_absent(metfrag_db_dir,what="mf-db-dir")
    res$metfrag$db_dir = norm_path(metfrag_db_dir)

    check_file_absent(metfrag_jar,what="mf-jar")
    res$metfrag$jar = norm_path(metfrag_jar)

    res
}


is_metfrag_available <- function(e) {
    nchar(e$metfrag$jar)>0L
}

is_metfrag_local_available <- function(e) {
    is_metfrag_available(e) && nchar(e$metfrag$db_dir)>0L
}
