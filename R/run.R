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

is_gen_done<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    file.exists(fnFlag)
}

is_ms2_done<-function(dest) {
    fnFlag<-file.path(dest,EXTR_MS2_FLAG)
    file.exists(fnFlag)
}

set_ms2_done<-function(dest) {
    fnFlag<-file.path(dest,EXTR_MS2_FLAG)
    file.create(fnFlag)
}

set_gen_done<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    file.create(fnFlag)
}

unset_gen_done<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    if (is_gen_done(dest)) unlink(fnFlag,force=T)
}


##' Paste with no separator.
##'
##' 
##' @title Paste With No Separator
##' @param ... Strings to paste together.
##' @return Pasted string.
##' @author Todor Kondić
attch<-function(...) paste(...,sep='')

##' Do the prescreening.
##'
##' @title Prescreening on bunch of files.
##' @param fTab File table with Files,ID,wd,Name and mz
##'     columns. Column Files, as well as wd must have all rows
##'     identical.
##' @param extr_fun Extraction function from the backend. 
##' @param limEIC Absolute mz tolerance used to extract precursor EICs.
##' @param limFinePPM Tolerance given in PPM used to associate input
##'     masses with what the instrument assigned as precutsors to MS2.
##' @param proc Amount of processors, or FALSE. 
##' @param fnLog For parallel execution, dump messages there.
##' @return Nothing useful.
##' @author Todor Kondić
##' @export
gen<-function(fTab,limEIC,limFinePPM,rtDelta,proc=F,fnLog='prescreen.log',extr_fun=extr_msnb_ht) {
    message("*** Started to generate prescreen data ...")
    unlink(fnLog)
    fread<-function(fTab) {
        extract(fTab=fTab,
                extr_fun=extr_fun,
                limEIC=limEIC,
                limFinePPM=limFinePPM,
                rtDelta=rtDelta,
                limCoarse=0.5,
                fnSpec=FN_SPEC)
        
        return(T)
    }


    fns<-unique(fTab$Files)
    fTabs<-lapply(fns,function(fn) fTab[fTab$Files==fn,])
    if (proc>1) {
        cl<-parallel::makeCluster(spec=proc,type='PSOCK',outfile=fnLog)
        parallel::clusterEvalQ(cl,library(shinyscreen))
        ## parallel::clusterExport(cl,c("extract"),envir=environment())
        res<-parallel::parLapply(cl,fTabs,fread)
        parallel::stopCluster(cl)
        res
    } else {
        lapply(fTabs,fread)
    }
    message("*** ... done generating prescreen data.")
}

