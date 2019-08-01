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
presc.do<-function(fn_data,fn_cmpd_l,mode,dest=".",proc=F,...) {
    conf(fn_data,fn_cmpd_l,dest)

    fread <- function(fn_data) {
        wd <- fn_data2wd(fn_data,dest)
        gen_presc_d(wd)
        reconf(wd)
        message("Currently processing: ",wd)
        fn_ftable <- get_ftable_fn(wd)
        fn_cmpd_l <- get_cmpd_l_fn(wd)
        RMB_EIC_prescreen_df(wd=wd,RMB_mode=mode,FileList=fn_ftable,
                             cmpd_list=fn_cmpd_l,...)
    }

    if (proc) {
        cl<-parallel::makeCluster(proc)
        parallel::clusterEvalQ(cl,library(shinyscreen))
        parallel::clusterMap(cl,fread,fn_data)
    } else {
        lapply(fn_data,fread)
    }
}

