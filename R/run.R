##' Paste with no separator.
##'
##' 
##' @title Paste With No Separator
##' @param ... Strings to paste together.
##' @return Pasted string.
##' @author Todor Kondić
attch<-function(...) paste(...,sep='')


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
##' @param fn_cmpd_list Compound list.
##' @param mode as in msmsRead.
##' @param rdir The root data directory.
##' @param combine If TRUE, use combineMultiplicies to merge
##'     workspaces corresponding to different collisional energies.
##' @param proc Split work between this amount of processes. If FALSE
##'     (or, 1), run sequential.
##' @return A named list of msmsWorkspace objects.
##' @author Todor Kondić
##' @export
sw.do<-function(fn_data,fn_cmpd_list,mode,rdir=".",combine=F,proc=F) {
    no_drama_mkdir(rdir)
    wdirs<-sapply(basename(fn_data),function(nm) file.path(rdir,stripext(nm)))
    sapply(wdirs,no_drama_mkdir)
    stgs<-sapply(basename(wdirs),function (nm) paste(nm,"yml",sep='.'))
    cl<-parallel::makeCluster(proc)
    if (proc) {
        p.sw(fn_data,stgs,wdirs,fn_cmpd_list,mode,combine=combine,cl=cl)
    } else {
        v(fn_data,stgs,wdirs,fn_cmpd_list,mode,combine=combine)
    }
}

##' Creates and prepares mbWorkspace objects before the full workflow
##' is performed on them. In process, create directory `info` as a
##' subdir of any particular data dir and place the starting info list
##' there.
##'
##' 
##' @title Prepare mbWorkspace objects
##' @param w A list of spectral workspace inputs.
##' @param rdir Data root.
##' @return Named list of prepared mbWorkspace objects.
##' @author Todor Kondić
##' @export
mb.prep<-function(w,rdir=".") {
    idir<-function(n) file.path(rdir,stripext(n))
    sapply(names(w),function (n) no_drama_mkdir(file.path(idir(n),"info")))
    fn_info<-sapply(names(w),function (n) file.path(idir(n),"info",attch(n,'.info.csv')))
    fn_stgs<-sapply(names(w),function(n) file.path(idir(n),attch(n,'.ini')))
    mb.prep.v(w,fn_info,fn_stgs)
}


##' Perform the Mass Bank workflow on the prepared mbWorkspace
##' objects. FOR THE RECORD: writing documentation is boring. Anyway,
##' record results in the info subfolder of each data subdir.
##'
##' 
##' @title Perform the Mass Bank workflow
##' @param mb The list of prepared mbWorkspace objects.
##' @param rdir Root data dir.
##' @return The named list of processed mbWorkspace objects.
##' @author Todor Kondić
##' @export
mb.do<-function(mb,rdir=".") {
    idir<-function(n) file.path(rdir,stripext(n))
    infodir<-sapply(names(mb),function(n) file.path(idir(n),"info"))
    fn_stgs<-sapply(names(mb),function(n) file.path(idir(n),attch(n,'.ini')))
    mb.v(mb,infodir,fn_stgs)
}
