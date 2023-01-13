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

check_notastring <- function(value,what) {
    if (!is.character(value)) stop(errorCondition(paste0("The value (",str(value),") of, ",what," is not a character vector."),class=paste0(what,'-notastring')))
}

check_dir_absent <- function(dir,what) {
    check_notastring(dir,what)
    if (nchar(dir)>0L && !dir.exists(dir)) stop(errorCondition(paste0("The ", what, " directory --- ", dir, "--- does not exist, or cannot be found."), class=paste0(what,'-absent')))
}

check_file_absent <- function(file,what) {
    check_notastring(file,what)
    if (nchar(file)>0L && !file.exists(file)) stop(errorCondition(paste0("The ", what, " file --- ", file, "--- does not exist, or cannot be found."), class=paste0(what,'-absent')))
}

check_not_one <- function(value,what) {
    if (length(value)!=1L) stop(errorCondition(paste0("Size of", what, " is not one."), class=paste0(what,'-not-one')))
}
