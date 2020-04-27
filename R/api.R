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



run <- function(fn_conf) {
    conf <- read_conf(fn_conf)
    dir.create(conf$project,
               showWarnings = F,
               recursive = T)
    
    withr::with_dir(new=conf$project,code = run_in_dir(conf))
    conf
}

read_conf <- function(fn_conf) {
    assertthat::assert_that(file.exists(fn_conf),msg=paste("Unable to read the configuration file:", fn_conf))
    conf <- yaml::yaml.load_file(fn_conf)
    conf <- vrfy_conf(conf)
    conf
}

vrfy_conf <- function(conf) {
    for (fn in unlist(conf$data,recursive=T)) assertthat::assert_that(file.exists(fn),msg=paste("Unable to read data file:",fn))
    fn_cmpd_known <- conf$compounds$known
    fn_cmpd_unk <- conf$compounds$unknown
    fn_cmpd_sets <- conf$compounds$sets
    assertthat::assert_that(file.exists(fn_cmpd_known),
                            msg=paste("Unable to read known compounds file:",fn_cmpd_known))
    assertthat::assert_that(file.exists(fn_cmpd_sets),
                            msg=paste("Unable to read compound sets file:",fn_cmpd_sets))
    if (!is.null(fn_cmpd_unk)) assertthat::assert_that(file.exists(fn_cmpd_unk),
                                                       msg=paste("Unable to read unknown compounds file:",fn_cmpd_unk))
    
    return(conf)
}

run_in_dir <- function(conf) {
    conf
}


