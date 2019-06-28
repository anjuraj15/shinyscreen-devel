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
##' @param fn_cmpd_list The compound list CSV.
##' @param mode RMB mode.
##' @param proc Amount of processors, or FALSE. 
##' @return Nothing useful.
##' @author Todor Kondić
##' @export
presc.do<-function(fn_data,fn_cmpd_list,mode,proc=F) {


    if (proc) {
        cl<-parallel::makeCluster(proc,type='FORK')
        presc.p(cl=cl,fn_data,fn_cmpd_l=fn_cmpd_list,mode=mode)
    } else {
        presc.v(fn_data,fn_cmpd_l=fn_cmpd_list,mode)
    }
}




##' Performs massbank workflow on multiple mzML files:
##'
##'
##'  1. Create rdir if it does not exist.
##'
##'  2. Generate data subdirectories inside `rdir` that will be used to
##'  keep the processed data belonging to a particular mzML file in
##'  the fileset.
##' 
##'  3. Generate the full RMassBank settings files by merging the
##'  sections from the `fn_cmpd_list` with the defaults and place those
##'  new settings files in the appropriate data subdirs.
##'
##'  4. Finally perform the spectral workflow on each data file and
##'  place the results in the data subdirs.
##' 
##' @title Perform MassBank Workflow on Multiple Compound Mixtures
##' @param fn_data List of mzML data filenames to be processed.
##' @param fn_cmpd_l Compound list.
##' @param mode as in msmsRead.
##' @param dest The destination data directory.
##' @param combine If TRUE, use combineMultiplicies to merge
##'     workspaces corresponding to different collisional energies.
##' @param proc Split work between this amount of processes. If FALSE
##'     (or, 1), run sequential.
##' @param split This is the last step before combine
##' @return A named list of msmsWorkspace objects.
##' @author Todor Kondić
##' @export
sw.do <- function(fn_data, fn_cmpd_l, mode, dest=".", combine=F,
                  proc=F,split=3) {
    
    conf(fn_data,fn_cmpd_l,dest)
    fread <- function(fn_data) {
        wd <- fn_data2wd(fn_data,dest)
        reconf(wd)
        w <- RMassBank::newMsmsWorkspace()
        RMassBank::msmsRead(w=w,filetable = get_ftable_fn(wd),
                            mode=mode,readMethod = "mzR")
    }
    fwork <- Vectorize(function(w,wd,steps) {
        archdir <- file.path(wd,"archive")
        no_drama_mkdir(archdir)
        fn_arch <- file.path(archdir,"archive")
        reconf(wd)
        RMassBank::msmsWorkflow(w=w,mode=mode,steps=steps,archivename = fn_arch)
    }, vectorize.args = c("w","wd"),SIMPLIFY=F)
    
    w <- if (proc) {
             cl=parallel::makeCluster(proc)
             parallel::clusterEvalQ(cl,library(rmbmix))
             parallel::clusterMap(cl,fread,fn_data)
         } else {
             lapply(fn_data,fread)
         }
    wd <- fn_data2wd(fn_data,dest)
    w <- fwork(w,wd,steps=2:split)
    if (combine) {
        ## Combined workflow is not based on a single file, but the
        ## functions that generate config are. Therefore, lets create
        ## a fake filename.
        fakefile <- "combine.mzML"
        cwd <- fn_data2wd(fakefile,dest)
        xx <- get_stgs_fn(wd[[1]])
        file.copy(xx,"combine.ini",overwrite = T)
        # mk_combine_file(get_stgs_fn(wd),"combine.ini")
        conf(fakefile,fn_cmpd_l,dest)
        reconf(cwd)
        w <- list(RMassBank::combineMultiplicities(w))
        wd <- list(cwd)
    }
    w <- fwork(w,wd,steps=(split+1):8)
    names(w) <- wd
}

##' Creates and prepares mbWorkspace objects before the full workflow
##' is performed on them. In process, create directory `info` as a
##' subdir of any particular data dir and place the starting info list
##' there.
##'
##' 
##' @title Prepare mbWorkspace objects
##' @param w A list of spectral workspace inputs.
##' @return Named list of prepared mbWorkspace objects.
##' @author Todor Kondić
##' @export
mb.prep<-function(w) {
    wd <- names(w)
    fwork <- Vectorize(function(w,wd) {
        reconf(wd)
        idir <- gen_info_dir(wd)
        mb <- RMassBank::newMbWorkspace(w)
        RMassBank::resetInfolists(mb)
        RMassBank::mbWorkflow(mb,infolist_path = get_info_fn(wd))
    },vectorize.args = c("w","wd"))

    mb <- fwork(w,wd)
    names(mb) <- wd
    mb
}


##' Perform the Mass Bank workflow on the prepared mbWorkspace
##' objects. FOR THE RECORD: writing documentation is boring. Anyway,
##' record results in the info subfolder of each data subdir.
##'
##' 
##' @title Perform the Mass Bank workflow
##' @param mb The list of prepared mbWorkspace objects.
##' @param rdir Root data dir.
##' @param proc Split work between this amount of processes. If FALSE
##'     (or, 1), run sequential.
##' @return The named list of processed mbWorkspace objects.
##' @author Todor Kondić
##' @export
mb.do<-function(mb,rdir=".",proc=F) {
    idir<-function(n) file.path(rdir,stripext(n))
    infodir<-sapply(names(mb),function(n) file.path(idir(n),"info"))
    fn_stgs<-sapply(names(mb),function(n) file.path(idir(n),attch(n,'.ini')))

    if (proc) {
        cl<-parallel::makeCluster(proc)
        parallel::clusterEvalQ(cl,library("rmbmix"))
        mb.p(mb,infodir,fn <- stgs,cl=cl)
    } else {
        mb.v(mb,infodir,fn_stgs)
    }
}
