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
    if (isGenDone(dest)) file.create(fnFlag)
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
##' @param fn_data The mzML files. Basis for the out directory name
##'     generation.
##' @param fn_cmpd_l The compound list.
##' @param mode RMB mode.
##' @param dest Destination directory.
##' @param proc Amount of processors, or FALSE. 
##' @param fn_cmpd_list The compound list CSV.
##' @return Nothing useful.
##' @author Todor Kondić
##' @export
presc.do<-function(fnTab,fnStgs,fnCmpdList,dest=".",proc=F,fnLog='prescreen.log',...) {
    
    unlink(fnLog)
    RMassBank::loadRmbSettings(fnStgs)
    RMassBank::loadList(fnCmpdList,check=F)

    fread <- function(fnData,fnStgs) {
        idc <- which(fnTab$Files %in% fnData)
        wd <- fnTab$wd[idc[[1]]]
        
        
        id <- fnTab$ID[idc]
        mode<-fnTab$mode[idc[[1]]]
        gen_presc_d(wd)
        RMassBank::loadRmbSettings(fnStgs)
        RMassBank::loadList(fnCmpdList,check=F)
        message("started: ",wd)
        gen_ftable(id=id,fnData=fnData,wd=wd)
        fn_ftable <- get_ftable_fn(wd)
        RMB_EIC_prescreen_df(wd=wd,RMB_mode=mode,FileList=fn_ftable,
                             cmpd_list=fnCmpdList,...)
        message("finished: ",wd)
        T
    }


    unsetGenDone(dest)
    unsetPPDone(dest)
    if (file.exists(fnFlag)) {
        unlink(fnFlag,force=T)
    }

    if (proc>1) {
        cl<-parallel::makeCluster(spec=proc,type='PSOCK',outfile=fnLog)
        parallel::clusterEvalQ(cl,library(shinyscreen))
        parallel::clusterExport(cl,c("fnTab","fnCmpdList"),envir=environment())
        parallel::clusterMap(cl,fread,levels(factor(fnTab$Files)),fnStgs)
        parallel::stopCluster(cl)
    } else {
        message("SERIAL")
        Map(fread,levels(factor(fnTab$Files)),fnStgs)
    }

    setGenDone(dest)
}

impCmpdList <- function(fnSrc,fnDest=file.path(".",basename(fnSrc))) {
    gen_cmpd_l(src_fn=fnSrc,dest_fn=fnDest)
}

gen<-function(fnFileTab,fnCmpdList,fnStgs,fnDestFileTable=attch(stripext(fnFiletable),"_candidate.csv"),dest=".",fnLog='prescreen.log',proc=1,intTresh=1e5,noiseFac=3,rtDelta=0.5,ppmLimFine=10,eicLim=1e-3) {
    message("*** Started to generate prescreen data ...")

    ## Read in the file table.
    fnTab <- read.csv(file = fnFileTab, header = T, sep=",", stringsAsFactors = F, comment.char='')


    ## Do the prescreen.
    presc.do(fnTab=fnTab,fnStgs = fnStgs,fnCmpdList=fnCmpdList,dest=dest,ppm_limit_fine=ppmLimFine,EIC_limit=eicLim,proc=proc,fnLog=fnLog)
    message("*** ... done generating prescreen data.")
}

