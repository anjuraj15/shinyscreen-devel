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

errc_conf_file_absent <- errorCondition("There is no config file in the project directory.",class="conf-file-absent")

check_notastring <- function(value, what, strict=F) {
    cond = !is.character(value)
    msg = paste0("The value (",value,") of, ",what," is not a character vector (or, it is, maybe, a missing value).")
    if (strict) {
        if (is.null(value)) {
            cond=T
            msg = paste0("The value of ",what," is NULL.")
        } else if (length(value)==0L) {
            cond=T
            msg = paste0("The variable ",what," is a zero-length object.")
        } else if (is.na(value)) {
            cond=T
            msg = paste0("The value of ",what," is NA.")
        } else if (nchar(value)==0L) {
            cond=T
            msg = paste0("The size of character variable ",what," is zero.")
        }
    }
    
    if (cond) stop(errorCondition(msg,class=paste0(what,'-notastring')))
}

check_dir_absent <- function(dir,what,strict=F) {
    check_notastring(dir,what,strict=strict)
    cond = !dir.exists(dir)
    if (!strict) cond = cond && nchar(dir)>0L
    if (cond) stop(errorCondition(paste0("The ", what, " directory --- ", dir, "--- does not exist, or cannot be found."), class=paste0(what,'-absent')))
}

check_dir_absent_nz <- function(dir,what) {
    check_notastring(dir,what)
    if (nchar(dir)>0L) {
        check_dir_absent(dir,what)
    }
}

check_file_absent <- function(file,what) {
    check_notastring(file,what)
    if (nchar(file)>0L && !file.exists(file)) stop(errorCondition(paste0("The ", what, " file --- ", file, "--- does not exist, or cannot be found."), class=paste0(what,'-absent')))
}

check_file_absent_nz <- function(file,what) {
    check_notastring(file,what)
    if (nchar(file)>0L) {
        check_file_absent(file,what)
    }
}

check_not_one <- function(value,what) {
    if (length(value)!=1L) stop(errorCondition(paste0("Size of", what, " is not one."), class=paste0(what,'-not-one')))
}

check_extension <- function(extfileval,what) {
    check_notastring(extfileval[[1]],what = what)
    if (extfileval[[1]]==extfileval[[2]]) stop(errorCondition(paste0("We could not find the extension for ",what, ". The returned value was: ", extfileval[[2]]),
                                                              class = paste0(what,'-no-ext-found')))
}

check_not_integer <- function(value,what) {
    if (!is.integer(value)) stop(errorCondition(paste0("The value (",value,") of `", what,"' must be an integer."), class = paste0(what,'-not-an-int')))
}

check_not_logical <- function(value,what) {
    if (!is.logical(value)) stop(errorCondition(paste0("The value (",value,") of `", what,"' must be logical."), class = paste0(what,'-not-a-logical')))
}

check_key_absent <- function(keys,l,what) {
    nms = names(l)
    keys_in = keys %in% l
    keys_absent = keys[!keys_in]
    hv = if (length(keys_absent)>1L) "have" else "has"
    if (length(keys_absent)>0L) stop(errorCondition(paste0("Keys [",paste0(keys_absent,collapse=', '), "] ",hv," not been found for ", what),
                                                    class = paste0(what,'-absent')))
}


check_conf_absent <- function(cfgfile) {
    check_notastring(cfgfile,"envopts")
    if (!file.exists(cfgfile)) stop(errorCondition("The system configuration file does not exist. Please initialise shinyscreen by calling `shinyscreen::init' function.", class="envopts-file-absent"))
}
