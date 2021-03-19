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


##' @import data.table
## Redirections
the_ifelse <- data.table::fifelse
dtable <- data.table::data.table

tab2file<-function(tab,file,...) {
    data.table::fwrite(x=tab,file=file,...)
}

file2tab<-function(file,na.strings=c("","NA","\"\""),...) {
    data.table::fread(file=file,na.strings = na.strings, ...)
}

isThingFile<-function(fn) {
    if (length(fn)>0 && is.character(fn)) {
        file.exists(fn)
    } else F
}

## Stolen from Stack Overflow
split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}


print_table <- function (df) {
    paste(apply(df,1,function (row) paste(row,collapse=',')),collapse = "\n")
}

assert <- function(expr,msg) shiny::validate(shiny::need(expr,message=msg))


gen_uniq_lab <- function(prev,pref='',suff='') {
    l <- length(prev)
    gen <- function() paste0(pref,as.integer(runif(1,min=l,max=2L*l+1L)),suff)
    cand <- gen()
    while (cand %in% prev) cand <- gen()
    c(prev,cand)
}

yesno2log <- function(yesno) {
    yes <- which(yesno=="✔")
    no <- which(yesno=="❌")
    res <- logical(length(yesno))
    res[yes] <- T
    res[no] <- F
    res[!((1:length(res)) %in% c(yes,no))]<-NA
    res
}

log2yesno <- function (log) {
    wna <- log[is.na(log)]
    wyes <- which(log)
    wno <- !((1:length(log)) %in% c(wna,wyes))
    res <- factor(character(length(log)),levels = c("✔","❌","NA"))
    res[wyes] <- "✔"
    res[wno] <- "❌"
    res[wna] <- "NA"
    res
}

## TODO: Remove calls to this once the glitch with prefiltering in
## datatables is fixed.
fixlog2yesno <- function(log) {
    as.character(log2yesno(log))
}
