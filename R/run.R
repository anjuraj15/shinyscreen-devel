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
presc.do<-function(fnTab,fnStgs,fnCmpdList,mode,dest=".",proc=F,fnLog='prescreen.log',...) {
    
    unlink(fnLog)    
    RMassBank::loadRmbSettings(fnStgs[[1]])
    RMassBank::loadList(fnCmpdList,check=F)


    fread <- function(fnData,fnStgs) {
        idc <- which(fnTab$Files %in% fnData)
        wd <- fnTab$wd[idc[[1]]]
        id <- fnTab$ID[idc]
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

    if (proc) {
        cl<-parallel::makeCluster(spec=proc,type='PSOCK',outfile=fnLog)
        parallel::clusterEvalQ(cl,library(shinyscreen))
        parallel::clusterExport(cl,c("fnTab","fnCmpdList"),envir=environment())
        parallel::clusterMap(cl,fread,levels(factor(fnTab$Files)),fnStgs)
        parallel::stopCluster(cl)
    } else {
        Map(fread,levels(factor(fnTab$Files)),fnStgs)
    }
}

impCmpdList <- function(fnSrc,fnDest=file.path(".",basename(fnSrc))) {
    gen_cmpd_l(src_fn=fnSrc,dest_fn=fnDest)
}

gen<-function(fnFileTab,fnCmpdList,mode,fnStgs,fnDestFileTable=attch(stripext(fnFiletable),"_candidate.csv"),dest=".",fnLog='prescreen.log',proc=F,intTresh=1e5,noiseFac=3,rtDelta=0.5,ppmLimFine=10,eicLim=1e-3) {
    message("*** Started to generate prescreen data ...")
    
    ## Read in the file table.
    fnTab <- read.csv(file = fnFileTab, header = T, sep=",", stringsAsFactors = F)
   

    ## Do the prescreen.
    presc.do(fnTab=fnTab,fnStgs = fnStgs,fnCmpdList=fnCmpdList,mode=mode,dest=dest,ppm_limit_fine=ppmLimFine,EIC_limit=eicLim,proc=proc,fnLog=fnLog)
    message("*** ... done generating prescreen data.")
}

