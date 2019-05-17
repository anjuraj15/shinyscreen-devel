
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


## Perform the compound mixture workflow on the data file called
## fn_data with settings named list called sett_alist.
rmbmix.single<-function(fn_data,sett_alist) {
    
    require(RMassBank)
    sfn<-paste(fn_data,".ini",sep='')
    rmbmix.mk_sett_file(sett_alist,sfn)
}
