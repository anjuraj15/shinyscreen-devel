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
presc.do<-function(fnData,fnStgs=attch(stripext(fnData),".ini"),wd,fnCmpdList,mode,dest=".",proc=F,fnLog='prescreen.log',...) {
    
    RMassBank::loadRmbSettings(fnStgs[[1]])
    RMassBank::loadList(fnCmpdList)
    cmpd <- read.csv(file=fnCmpdList,stringsAsFactors = F)
    n_cmpd <- nrow(cmpd)
    
    fread <- function(fnData,fnStgs,wd) {
        gen_presc_d(wd)
        RMassBank::loadRmbSettings(fnStgs)
        RMassBank::loadList(fnCmpdList)
        message("Currently processing: ",wd)
        gen_ftable(fnData,wd,n_cmpd)
        fn_ftable <- get_ftable_fn(wd)
        RMB_EIC_prescreen_df(wd=wd,RMB_mode=mode,FileList=fn_ftable,
                             cmpd_list=fnCmpdList,...)
    }

    if (proc) {
        cl<-parallel::makeCluster(proc,type='PSOCK',outfile=fnLog)
        parallel::clusterEvalQ(cl,library(shinyscreen))
        parallel::clusterMap(cl,fread,fnData,fnStgs,wd)
    } else {
        Map(fread,fnData,fnStgs,wd)
    }
}

impCmpdList <- function(fnSrc,fnDest=file.path(".",basename(fnSrc))) {
    gen_cmpd_l(src_fn=fnSrc,dest_fn=fnDest)
}

gen<-function(fnFileTab,fnCmpdList,mode,fnDestFileTable=attch(stripext(fnFiletable),"_candidate.csv"),dest=".",stgsPath=dest,fnLog='prescreen.log',proc=F,intTresh=1e5,noiseFac=3,rtDelta=0.5,ppmLimFine=10,eicLim=1e-3) {
    message("*** Started to generate prescreen data ...")
    
    ## Read in the file table.
    fTab <- read.csv(file = fnFileTab, header = T, sep=",", stringsAsFactors = F)

    ## Get files and the associated work directories.
    fnData <- levels(factor(fTab$Files))
    wd <- fTab$wd[match(fnData,fTab$Files)]
    stgsName <- sapply(wd,function(w) paste(wd,".ini",sep = ''))
    fnStgs <- file.path(stgsPath,basename(stgsName))
    message(fnStgs[[1]])

    ## Do the prescreen.
    presc.do(fnData=fnData,wd=wd,fnStgs = fnStgs,fnCmpdList=fnCmpdList,mode=mode,dest=dest,ppm_limit_fine=ppmLimFine,EIC_limit=eicLim,proc=proc,fnLog=fnLog)
    message("*** ... done generating prescreen data.")
}

