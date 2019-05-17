## Create a temporary directory to hold the files generated on the
## fly.
rmbmix.mkdir<-function() {
    nm<-tempfile(pattern="rmbmix")
    dir.create(nm)
    nm
}

##
rmbmix.mk_sett_file<-function(sett_alist,file) {
    require(yaml)
    tmp<-tempfile()
    RmbSettingsTemplate(tmp)
    sett<-yaml.load_file(tmp)
    for (nm in names(sett_alist)) {
        sett[[nm]]<-sett_alist[[nm]]
    }
    write_yaml(x=sett,file=file)
}


## Generate the RMassBank compound list from the input compound list
## in CSV file src_fn. The input compound list format is either
## Chemical Dashboard csv file with, at least, PREFERRED_NAMES and
## SMILES columns _filled_ out, or just an ordinary csv file with
## columns SMILES and Names filled. Argument dest_fn is the
## destination filename. Returns the number of compounds.
rmbmix.gen_comp_list<-function(src_fn,dest_fn) {
    df<-read.csv(src_fn)
    ## Names
    nms<-if ("PREFERRED_NAME" %in% names(df)) df$PREFERRED_NAME else df$Name
    if (is.null(nms)) stop("Unable to read compound names from the input compound list.")
    ## SMILES
    haha<-df$SMILES
    if (is.null(haha)) stop("Unable to read SMILES from the input compound list.")
    sz<-length(haha)
    outdf<-data.frame(Name=nms,SMILES=haha,ID=1:sz,RT=rep("",sz),CAS=rep("",sz))
    write.csv(outdf,file=dest_fn,row.names=F)
    length(nms)
}
    
    
    
## Perform the compound mixture workflow on the data file called
## fn_data with settings named list called sett_alist. Argument
## fn_cmpd_list is the compound list. Argument wd is the scratch dir
## to hold generated ini files and the like. Arguments mode and
## readMethod are the same as in msmsRead.
rmbmix.single<-function(fn_data,sett_alist,fn_cmpd_list,wd,mode,readMethod="mzR") {
    
    require(RMassBank)

    ## Generate settings file and load.
    sfn<-file.path(wd,paste(fn_data,".ini",sep=''))
    rmbmix.mk_sett_file(sett_alist,sfn)
    loadRmbSettings(sfn)

    ## Generate and load the compound list.
    fn_comp<-file.path(wd,paste(fn_data,".comp.csv",sep=''))
    n_cmpd<-rmbmix.gen_comp_list(fn_cmpd_list,fn_comp)
    loadList(fn_comp)

    ## Generate file table.
    df_table<-data.frame(Files=rep(fn_data,n_cmpd),ID=1:n_cmpd)
    fn_table<-file.path(wd,paste("fn-table.",fn_data,".csv",sep=''))
    write.csv(x=df_table,file=fn_table,row.names=F)

    ## Make empty workspace.
    w <- newMsmsWorkspace()
    #w <-msmsRead(w,filetable=fn_table,readMethod="mzR",mode=mode)
    ## Run the workflow.
    message(paste("Reading in file:",fn_data))
    w <-msmsRead(w,filetable=fn_table,readMethod="mzR",mode=mode)
}
