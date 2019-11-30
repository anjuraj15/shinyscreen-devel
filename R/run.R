isGenDone<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    file.exists(fnFlag)
}

isPPDone<-function(dest) {
    fnFlag<-file.path(dest,".pp.DONE")
    file.exists(fnFlag)
    
}

setPPDone<-function(dest) {
    fnFlag<-file.path(dest,".pp.DONE")
    if (isPPDone(dest)) file.create(fnFlag)
}

unsetPPDone<-function(dest) {
    fnFlag<-file.path(dest,".pp.DONE")
    if (isPPDone(dest)) unlink(fnFlag,force=T)
}

setGenDone<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    file.create(fnFlag)
}

unsetGenDone<-function(dest) {
    fnFlag<-file.path(dest,".gen.DONE")
    if (isGenDone(dest)) unlink(fnFlag,force=T)
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
gen<-function(fTab,limEIC,limFinePPM,proc=F,fnLog='prescreen.log',extr_fun=extr_msnb) {
    message("*** Started to generate prescreen data ...")
    unlink(fnLog)
    fread<-function(fTab) {
        extract(fTab=fTab,
                extr_fun=extr_fun,
                limEIC=limEIC,
                limFinePPM=limFinePPM,
                limCoarse=0.5)
        
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

