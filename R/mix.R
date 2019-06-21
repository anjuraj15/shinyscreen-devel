stripext<-function(fn) {
    bits<-strsplit(fn,split="\\.")[[1]]
    if (length(bits)> 1) paste(head(bits,-1),collapse=".") else fn}

##' Create directories without drama.
##'
##' Create directories without drama.
##' 
##' @title Create directories without drama
##' @param path Name of the directory.
##' @return The character string containing the input argument `path`.
##' @author Todor Kondić
no_drama_mkdir<-function(path) {

    if (! dir.exists(path)) dir.create(path)
    path
}

##' Produce the Rmb Settings file
##'
##' Produce the Rmb Settings file based on the customisation file in
##' YAML format.
##'
##' @title Generate RMassBank settings file.
##' @param sett_alist The named list of settings that are different
##'     from the RMassBank defaults.
##' @param file The name of the YAML specification that will be merged
##'     with the template Rmb settings file.
##' @return NULL
mk_sett_file<-function(sett_alist,file) {
    tmp<-tempfile()
    RMassBank::RmbSettingsTemplate(tmp)
    sett<-yaml::yaml.load_file(tmp)
    for (nm in names(sett_alist)) {
        sett[[nm]]<-sett_alist[[nm]]
    }
    yaml::write_yaml(x=sett,file=file)
    NULL
}

##' Combine the RMB settings files
##' 
##' Combine RMB settings with different collisional energies into one
##' settings file with multiple collisional energy entries.
##' 
##' @title Combine RMB Settings With Different Collisional Energies
##' @param sett_fns A list of settings files.
##' @param fname The name of the combined file.
##' @return fname
##' @author Todor Kondić
mk_combine_file<-function(sett_fns,fname) {
    all_settings <- lapply(sett_fns,yaml::yaml.load_file)
    comb_settings <- all_settings[[1]]
    
    for (n in 1:length(all_settings)) {
        comb_settings$spectraList[[n]] <- all_settings[[n]]$spectraList[[1]]
    }

    yaml::write_yaml(x=comb_settings,fname)
    fname
}

##' Generate the RMassBank compound list from the input compound list
##' in CSV file src_fn. The input compound list format is either a
##' Chemical Dashboard csv file with, at least, PREFERRED_ SMILES
##' columns _filled_ out, or just an ordinary CSV file with columns
##' SMILES and Names filled. Argument dest_fn is the destination
##' filename. Returns the number of compounds.
##'
##' 
##' @title Generate Compound List File
##' @param src_fn The input compound list CSV filename.
##' @param dest_fn The resulting compound list CSV filename.
##' @return Number of compounds.
##' @author Todor Kondić
gen_comp_list<-function(src_fn,dest_fn) {
    df<-read.csv(src_fn)
    ## Names
    nms<-if ("PREFERRED_NAME" %in% names(df)) df$PREFERRED_NAME else df$Name
    if (is.null(nms)) stop("Unable to read compound names from the input compound list.")

    ## SMILES
    haha<-df$SMILES
    sz<-length(haha)
    
    ## CAS
    casvals<-if ("CASRN" %in% names(df)) df$CASRN else rep(NA,sz)
    if (is.null(haha)) stop("Unable to read SMILES from the input compound list.")

    outdf<-data.frame(ID=1:sz,Name=nms,SMILES=haha,CAS=casvals,RT=rep(NA,sz))
    write.csv(outdf,file=dest_fn,row.names=F,na="")
    length(nms)
}

##' Generates settings file and loads it.
##'
##' 
##' @title Generate and Load the RMassBank Settings File
##' @param fn_data The mzML filename.
##' @param stgs Settings named list, or a settings filename.
##' @param wd Directory under which results are archived.
##' @return result of RMassBank::loadRmbSettings
##' @author Todor Kondić
gen_stgs_and_load <- function(fn_data,stgs,wd) {
    wd <- normalizePath(wd)
    fn_data <- normalizePath(fn_data)
    stgs<-if (is.character(stgs)) yaml::yaml.load_file(stgs) else stgs
    sfn<-file.path(wd,paste(basename(fn_data),".ini",sep=''))
    mk_sett_file(stgs,sfn)
    RMassBank::loadRmbSettings(sfn)
}

##' Generates the RMassBank compound list and loads it.
##'
##' 
##' @title Generate and Load the RMassBank Compound List
##' @param fn_data The mzML filename.
##' @param wd Directory under which results are archived.
##' @return Named list. The key `fn_cmpdl` is the path of the
##'     generated compound list and the key `n` the number of
##'     compounds.
##' @author Todor Kondić
gen_cmpdl_and_load <- function(fn_data,wd,fn_cmpdl) {
    wd <- normalizePath(wd)
    fn_data <- normalizePath(fn_data)
    fn_comp<-file.path(wd,paste(basename(fn_data),".comp.csv",sep=''))
    n_cmpd<-gen_comp_list(fn_cmpd_l,fn_comp)
    list(fn_cmpdl=fn_comp,n=n_cmpd)
}

##' Generates file table.
##'
##' 
##' @title Generate and Load the RMassBank Settings File
##' @param fn_data The mzML filename.
##' @param n_cmpd Number of compounds.
##' @param wd Directory under which results are archived.
##' @return NULL
##' @author Todor Kondić
gen_file_table <- function(fn_data,n_cmpd,wd) {
    wd <- normalizePath(wd)
    fn_data <- normalizePath(fn_data)
    df_table<-data.frame(Files=rep(fn_data,n_cmpd),ID=1:n_cmpd)
    fn_table<-file.path(wd,paste("fn-table.",basename(fn_data),".csv",sep=''))
    write.csv(x=df_table,file=fn_table,row.names=F)
    NULL
}



##' Runs a compound mixture workflow on a single mzML file.
##' 
##' @title RMassBank Spectral Workflow on a Single Compound Mixture
##' @param fn_data A mzML data file.
##' @param stgs_alist RMassBank settings. It can either be a named
##'     list of settings, or a filename of a YAML file.
##' @param wd The name of the work directory.
##' @param fn_cmpd_list The file name of he compound list
##'     corresponding to `fn_data`.
##' @param mode Modes as described in the standard workflow vignette
##'     of RMassBank.
##' @param readMethod Default read method is "mzR". Consult the
##'     documentation of `msmsRead` for details.
##' @param archdir The directory to store R objects created during
##'     workflow execution.
##' @param lastStep The last step in the workflow. Default is eight.
##' @return MsmsWorkspace object.
##' @author Todor Kondić
single.sw<-function(fn_data,stgs_alist,wd,fn_cmpd_list,mode,readMethod="mzR",archdir="archive",lastStep=8) {
    ## Generate settings file and load.
    stgs_alist<-if (is.character(stgs_alist)) yaml::yaml.load_file(stgs_alist) else stgs_alist
    sfn<-file.path(wd,paste(fn_data,".ini",sep=''))
    mk_sett_file(stgs_alist,sfn)
    RMassBank::loadRmbSettings(sfn)

    ## Generate and load the compound list.
    fn_comp<-file.path(wd,paste(fn_data,".comp.csv",sep=''))
    n_cmpd<-gen_comp_list(fn_cmpd_list,fn_comp)
    RMassBank::loadList(fn_comp)

    ## Generate file table.
    df_table<-data.frame(Files=rep(fn_data,n_cmpd),ID=1:n_cmpd)
    fn_table<-file.path(wd,paste("fn-table.",fn_data,".csv",sep=''))
    write.csv(x=df_table,file=fn_table,row.names=F)

    ## Make empty workspace.
    w <- RMassBank::newMsmsWorkspace()
    ## Run the workflow.
    message(paste("Reading in file:",fn_data))
    w <-RMassBank::msmsRead(w,filetable=fn_table,readMethod="mzR",mode=mode)
    archdir<-file.path(wd,archdir)
    if (!dir.exists(archdir)) dir.create(archdir)
    fn_arch<-file.path(archdir,paste(fn_data,".archive",sep=''))
    RMassBank::msmsWorkflow(w, mode=mode, steps=2:lastStep,archivename=fn_arch)
}


##' Prepare single mbWorkspace object based on the workspace, the
##' infolist name and RMassBank settings.
##'
##' 
##' @title Prepare Single mbWorkspace object
##' @param w MsmsWorkspace object.
##' @param fn_info Filename of the infolist to be generated.
##' @param fn_stgs Filename of the RMassBank settings.
##' @return A mbWorkspace object.
##' @author Todor Kondić
mb.prep.single<-function(w,fn_info,fn_stgs) {
    RMassBank::loadRmbSettings(fn_stgs)
    mb <- RMassBank::newMbWorkspace(w)
    RMassBank::resetInfolists(mb)
    RMassBank::mbWorkflow(mb,infolist_path=fn_info)
}
##' Vectorize mb.prep function.
##'
##' 
##' @title Vectorized mb.prep function.
##' @param w A sequence of msmsWorkspaces.
##' @param fn_info A sequence of infolist filenams to be generated.
##' @param fn_stgs A sequence of settings associated with each
##'     msmsWorkspace object.
##' @return A list of mbWorkspaces.
##' @author Todor Kondić
mb.prep.v<-function(w,fn_info,fn_stgs) {
    f<-Vectorize(mb.prep.single,vectorize.args=c("w","fn_info","fn_stgs"),SIMPLIFY=F)
    res<-f(w,fn_info,fn_stgs)
    names(res)<-names(w)
    res
}


##' Performs a single MassBank workflow after preparation.
##'
##' 
##' @title Single MassBank workflow.
##' @param mb A mbWorkspace object.
##' @param infodir Directory containing the infolist.
##' @param fn_stgs The settings associated with the mbWorkspace
##'     object.
##' @return A mbWorkflow object.
##' @author Todor Kondić
mb.single<-function(mb,infodir,fn_stgs) {
    RMassBank::loadRmbSettings(fn_stgs)
    
    mb <- RMassBank::resetInfolists(mb)
    mb <- RMassBank::loadInfolists(mb,infodir)
    ## loadInfolists
    ## addPeaks
    prevd<-setwd(infodir)
    res<-RMassBank::mbWorkflow(mb,step=1:8)
    setwd(prevd)
    res
}





##' Interface to vectorised spectral workflow.
##'
##' 
##' @title Vectorised Spectral Workflow.
##' @param fn_data A sequence of mzML input files.
##' @param stgs_alist A list of named list of settings, or a list of
##'     filenames of YAML files containing the settings.
##' @param wd The list of working directories.
##' @param fn_cmpd_list The compound list characterising the mixtures.
##' @param mode Same as in msmsRead.
##' @param readMethod Same as in msmsRead.
##' @param archdir Name of the archive.
##' @param lastStep The last step of the spectral workflow.
##' @param combine If TRUE, use combineMultiplicies to merge
##'     workspaces corresponding to different collisional energies.
##' @return A named list of spectral workspaces. The names are derived
##'     from data filenames.
##' @author Todor Kondić
v<-function(fn_data,stgs_alist,wd,fn_cmpd_list,mode,readMethod="mzR",archdir="archive",lastStep=8,combine=F) {
    idir<-function(n) file.path(".",stripext(n))
    f<-Vectorize(single.sw,vectorize.args=c("wd","fn_data","stgs_alist"),SIMPLIFY=F)
    rootdir <- getwd()
    if (combine) {
        z<-f(fn_data,stgs_alist,wd,fn_cmpd_list,mode,readMethod=readMethod,archdir=archdir,lastStep=7)
        names(z)<-basename(fn_data)
        zz<-RMassBank::combineMultiplicities(z)

        combdir<-"combined"
        archdir<-file.path(rootdir,combdir,archdir)
        no_drama_mkdir(combdir)
        no_drama_mkdir(archdir)
        fn_arch<-file.path(archdir,"archive")
        fn_comb_stgs <- file.path(rootdir,combdir,paste(combdir,".mzML.ini",sep=''))
        ddirs <- sapply(names(z),idir)
        stgs_fls <- sapply(ddirs,function(x) file.path(x,paste(x,".mzML.ini",sep='')))
        mk_combine_file(stgs_fls,fn_comb_stgs)

        res<-list(RMassBank::msmsWorkflow(zz, steps=8, mode=mode, archivename = fn_arch))
        names(res)<-paste(combdir,".mzML",sep='') #Clearly a hack.
        res
    } else {
        z<-f(fn_data,stgs_alist,wd,fn_cmpd_list,mode,readMethod=readMethod,archdir=archdir,lastStep=lastStep)
        names(z)<-basename(fn_data)
        z
    }
}

##' Wrapper for a single prescreening call. Produces output in the
##' usual mix method places.
##'
##' @title Wrapper for RMB_EIC_Prescreen
##' @param fn_data The mzML filename.
##' @param stgs_alist Settings named list, or a settings filename.
##' @param wd Directory under which results are archived.
##' @param mode RMB mode. 
##' @param fn_cmpd_l Filename of the compound list.
##' @param ppm_lim_fine The ppm_limit_fine argument to RMB_EIC_Prescreen
##' @param EIC_limit Passed down to RMB_EIC_Prescreen.
##' @param nm_arch Archive prefix.
##' @return result of RMB_EIC_Prescreen
##' @author Todor Kondić
##' @export
presc.single <- function(fn_data,stgs_alist,wd,mode,fn_cmpd_l,ppm_lim_fine=10,EIC_limit=0.001,nm_arch="archive") {
    gen_stgs_and_load(fn_data,stgs_alist,wd)
    
    ## Generate and load the compound list.
    x <- gen_cmpdl_and_load(fn_data,wd,fn_cmpd_l)
    fn_comp <- x$fn_cmpdl
    n_cmpd <- x$n

    ## Generate file table.
    gen_file_table(fn_data,n_cmpd,wd)
    
    curd <- setwd(wd)
    res <- ReSOLUTION::RMB_EIC_prescreen(archive_name=nm_arch,RMB_mode=mode,
                                  FileList=fn_table,
                                  cmpd_list=fn_comp,
                                  ppm_limit_fine=ppm_lim_fine,
                                  EIC_limit=EIC_limit)
    setwd(curd)
    res

}




##' Interface to parallel spectral workflow.
##'
##' 
##' @title Parallel Spectral Workflow.
##' @param fn_data A sequence of mzML input files.
##' @param stgs_alist A list of named list of settings, or a list of
##'     filenames of YAML files containing the settings.
##' @param wd The list of working directories.
##' @param fn_cmpd_list The compound list characterising the mixtures.
##' @param mode Same as in msmsRead.
##' @param readMethod Same as in msmsRead.
##' @param archdir Name of the archive.
##' @param lastStep The last step in spectral workflow.
##' @param combine If TRUE, use combineMultiplicies to merge
##'     workspaces corresponding to different collisional energies.
##' @param cl Cluster.
##' @return A named list of spectral workspaces. The names are derived
##'     from data filenames.
##' @author Todor Kondić
p.sw<-function(fn_data,stgs_alist,wd,fn_cmpd_list,mode,readMethod="mzR",archdir="archive",lastStep=8,combine=F,cl=NULL) {
    idir<-function(n) file.path(".",stripext(n))
    fnocomb<-function(fn,stgs,wd) {
        single.sw(fn,stgs,wd,fn_cmpd_list,mode,readMethod,archdir,lastStep=lastStep)
    }
    fcomb<-function(fn,stgs,wd) {
        single.sw(fn,stgs,wd,fn_cmpd_list,mode,readMethod,archdir,lastStep=7)
    }

    if (combine) {
        rootdir <- getwd()
        z<-parallel::clusterMap(cl,fcomb,fn_data,stgs_alist,wd)
        names(z)<-basename(fn_data)
        zz<-RMassBank::combineMultiplicities(z)

        combdir<-"combined"
        archdir<-file.path(rootdir,combdir,archdir)
        no_drama_mkdir(combdir)
        no_drama_mkdir(archdir)
        fn_arch<-file.path(archdir,"archive")
        fn_comb_stgs <- file.path(rootdir,combdir,paste(combdir,".mzML.ini",sep=''))
        ddirs <- sapply(names(z),idir)
        stgs_fls <- sapply(ddirs,function(x) file.path(x,paste(x,".mzML.ini",sep='')))
        mk_combine_file(stgs_fls,fn_comb_stgs)
        
        res<-list(RMassBank::msmsWorkflow(zz, steps=8, mode=mode, archivename = fn_arch))
        names(res)<-paste(combdir,".yml",sep='') #Clearly a hack.
        res
    } else {
        z<-parallel::clusterMap(cl,fnocomb,fn_data,stgs_alist,wd)
        names(z)<-basename(fn_data)
        z
    }
}

    
##' Interface to vectorised Mass Bank workflow.
##'
##' 
##' @title Vectorised Mass Bank Workflow
##' @param mb List of mass bank workflow objects
##' @param infodir List of subdirs containing info lists.
##' @param fn_stgs List of settings files.
##' @return A named list of mbWorkspace objects. The names are derived
##'     from the input mb sequence.
##' @author Todor Kondić
mb.v<-function(mb,infodir,fn_stgs) {
    f<-Vectorize(mb.single,vectorize.args=c("mb","infodir","fn_stgs"),SIMPLIFY=F)
    x<-f(mb,infodir,fn_stgs)
    names(x)<-names(mb)
    x}

##' Interface to parallelised Mass Bank workflow.
##'
##' 
##' @title Parallel Mass Bank Workflow
##' @param mb List of mass bank workflow objects
##' @param infodir List of subdirs containing info lists.
##' @param fn_stgs List of settings files.
##' @param cl Cluster.
##' @return A named list of mbWorkspace objects. The names are derived
##'     from the input mb sequence.
##' @author Todor Kondić
mb.p<-function(mb,infodir,fn_stgs,cl=F) {
    x<-parallel::clusterMap(cl=cl,mb.single,mb,infodir,fn_stgs)    
    names(x)<-names(mb)
    x}


