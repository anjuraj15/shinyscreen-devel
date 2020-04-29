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



run_in_dir <- function(conf) {
    conf
}


read_conf <- function(fn_conf) {
    assertthat::assert_that(file.exists(fn_conf),msg=paste("Unable to read the configuration file:", fn_conf))
    conf <- yaml::yaml.load_file(fn_conf)
    conf <- vrfy_conf(conf)
    conf
}

vrfy_conf <- function(conf) {
    ## * Existence of input files

    ## ** Data files
    for (fn in unlist(conf$data,recursive=T)) assertthat::assert_that(file.exists(fn),msg=paste("Unable to read data file:",fn))
    fn_cmpd_known <- conf$compounds$known
    fn_cmpd_unk <- conf$compounds$unknown
    fn_cmpd_sets <- conf$compounds$sets

    ## ** Compound lists and sets
    assertthat::assert_that(file.exists(fn_cmpd_known),
                            msg=paste("Unable to read known compounds file:",fn_cmpd_known))
    assertthat::assert_that(file.exists(fn_cmpd_sets),
                            msg=paste("Unable to read compound sets file:",fn_cmpd_sets))
    if (!is.null(fn_cmpd_unk)) assertthat::assert_that(file.exists(fn_cmpd_unk),
                                                       msg=paste("Unable to read unknown compounds file:",fn_cmpd_unk))


    ## * Data files
    df_sets <- file2tab(fn_cmpd_sets)
    all_sets<-unique(df_sets$set)

    fn_data <- conf$data
    assertthat::assert_that(file.exists(fn_data),msg=paste("Data table cannot be read:",fn_data))
    mzml <- file2tab(fn_data)
    
    no_files <- which(mzml[,!file.exists(Files)])
    no_modes <- which(mzml[,!(mode %in% names(MODEMAP))])
    no_sets <- which(mzml[,!(set %in% all_sets)])
    assertthat::assert_that(length(no_files)==0,msg = paste("Unreadable data files at rows:",paste(no_files,collapse = ','), "of",fn_data))
    assertthat::assert_that(length(no_modes)==0,msg = paste("Unrecognised modes at rows:",paste(no_modes,collapse = ','), "of", fn_data))
    assertthat::assert_that(length(no_sets)==0,msg = paste("Unknown sets at rows:",paste(no_sets,collapse = ','),"of", fn_data))

    df_k <- file2tab(fn_cmpd_known)
    
    are_knowns_OK <- shiny::isTruthy(vald_comp_tab(df_k,fn_cmpd_known, checkSMILES=T, checkNames=T))
    assertthat::assert_that(are_knowns_OK)

    ## ** Unknowns
    if (!is.null(fn_cmpd_unk)) {
        df_u <- file2tab(fn_cmpd_unk)
        are_unknowns_OK <- shiny::isTruthy(vald_comp_tab(df_u,fn_cmpd_unk, checkSMILES=F, checkMz=T))
        assertthat::assert_that(are_unknowns_OK)
    }
    
    
    
    return(conf)
}

mk_mzml_tab <- function(data) {
    files <- unlist(data,recursive = T)
    sets <- unique(names(data))
    tags <- c()
    for (s in sets) {
        tags<-c(tags,names(data[[s]]))
    }
    tags<-unique(tags)
    nr<-length(files)
    z<-suppressWarnings(data.table::data.table(Files=character(nr),
                                               mode=factor(levels = names(MODEMAP)),
                                               set=factor(levels = sets),
                                               tag=factor(levels= c(TAG_DEF,tags)),
                                               stringsAsFactors = F))
    z$Files <- files
    i <- 1
    for (s in names(data)) {
        for (t in names(data[[s]])) {
            z[i,"set"] <- s    
            z[i,"tag"]<-t
            i<-i+1
        }
    }
    z
}
