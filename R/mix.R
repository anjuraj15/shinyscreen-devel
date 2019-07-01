stripext<-function(fn) {
    bits<-strsplit(fn,split="\\.")[[1]]
    if (length(bits)> 1) paste(head(bits,-1),collapse=".") else fn}

##' Create directories without drama.
##'
##' Create directories without drama.
##' 
##' @title Create directories without drama
##' @param path Names of the directories.
##' @return The character string containing the input argument `path`.
##' @author Todor Kondić
no_drama_mkdir<-function(path) {
    f <- Vectorize(function(path) {
        if (! dir.exists(path)) dir.create(path)
        path},vectorize.args="path")
    f(path)
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


fn_data2wd <- function(fn_data,dest) {
    
    f <- Vectorize(function(fn_data) {
        noext <- stripext(fn_data)
        file.path(dest,basename(noext))
    },vectorize.args="fn_data")
    f(fn_data)
}

get_presc_d <- function(wd) { file.path(wd,"prescreen")}
gen_presc_d <- function(wd) { no_drama_mkdir(get_presc_d(wd))}
    
    

get_cmpd_l_fn <- function(wd) {
    f <- function(wd) file.path(wd,"compounds.csv")
    fv <- Vectorize(f,vectorize.args=c("wd"))
    fv(wd)
}

get_stgs_fn <- function(wd) {
    f <- function(wd) file.path(wd,"settings.ini")
    fv <- Vectorize(f,vectorize.args=c("wd"))
    fv(wd)
}

get_ftable_fn <- function(wd) {
    f <- function(wd) file.path(wd,"ftable.csv")
    fv <- Vectorize(f,vectorize.args=c("wd"))
    fv(wd)
}

get_inp_stgs_fn<- function(fn_data) {
    f <- Vectorize(function(fn_data) {
        bnm <- stripext(fn_data)
        fn <- paste(bnm,".ini",sep='')},
        vectorize.args="fn_data")
    f(fn_data)}

get_info_dir <- function(wd) {
    file.path(wd,"info")
}

get_info_fn <- function(wd) {
    file.path(get_info_dir(wd),"info.csv")
}

gen_info_dir <- function(wd) {
    nm <- get_info_dir(wd)
    no_drama_mkdir(nm)
    nm
}



##' Generate the RMassBank compound list from the input compound list
##' in CSV file src_fn. The input compound list format is either a
##' Chemical Dashboard csv file with, at least, PREFERRED_ SMILES
##' columns _filled_ out, or just an ordinary CSV file with columns
##' SMILES and Names filled. Argument dest_fn is the destination
##' filename. Returns the number of compounds.
##'
##' @title Generate Compound List File
##' @param src_fn The input compound list CSV filename.
##' @param dest_fn The resulting compound list CSV filename.
##' @return Number of compounds.
##' @author Todor Kondić
gen_cmpd_l<-function(src_fn,dest_fn) {
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
    f <- Vectorize(function (dest_fn) {
        write.csv(outdf,file=dest_fn,row.names=F,na="")
    },vectorize.args="dest_fn",SIMPLIFY=F)
    
    f(dest_fn)
    length(nms)
}

##' Generates settings file and loads it.
##'
##' @title Generate and Load the RMassBank Settings File
##' @param stgs Settings named list, or a settings filename.
##' @param wd Directory under which results are archived.
##' @return result of RMassBank::loadRmbSettings
##' @author Todor Kondić
gen_stgs_and_load <- function(stgs,wd) {
    stgs<-if (is.character(stgs)) yaml::yaml.load_file(stgs) else stgs
    sfn<-get_stgs_fn(wd)
    mk_sett_file(stgs,sfn)
    RMassBank::loadRmbSettings(sfn)
}

##' Generates the RMassBank compound list and loads it.
##'
##' @title Generate and Load the RMassBank Compound List
##' @param wd Directory under which results are archived.
##' @param fn_cmpdl The input compound list filename. 
##' @return Named list. The key `fn_cmpdl` is the path of the
##'     generated compound list and the key `n` the number of
##'     compounds.
##' @author Todor Kondić
gen_cmpdl_and_load <- function(wd,fn_cmpdl) {
    fn_comp<-get_cmpd_l_fn(wd)
    n_cmpd<-gen_cmpd_l(fn_cmpdl,fn_comp)
    RMassBank::loadList(fn_comp)
    list(fn_cmpdl=fn_comp,n=n_cmpd)
}

##' Generates file table.
##'
##' 
##' @title Generate and Load the RMassBank Settings File
##' @param fn_data The mzML filename.
##' @param wd Directory under which results are archived.
##' @param n_cmpd Number of compounds.
##' @return File path of the file table.
##' @author Todor Kondić
gen_ftable <- function(fn_data,wd,n_cmpd) {
    f <- Vectorize(function(fn_data,wd) {
        df_table<-data.frame(Files=rep(fn_data,n_cmpd),ID=1:n_cmpd)
        fn_table<-get_ftable_fn(wd)
        write.csv(x=df_table,file=fn_table,row.names=F)
        fn_table
    }, vectorize.args=c("fn_data","wd"))

    f(fn_data,wd)
}

gen_fn_stgs <- function(fn_inp,fn) {
    f <- Vectorize(function(fn_inp,fn) {
        stgs <- yaml::yaml.load_file(fn_inp)
        mk_sett_file(stgs,fn)
        fn}, vectorize.args=c("fn_inp","fn"))

    f(fn_inp,fn)
}

conf <- function(fn_data,fn_cmpd_l,dest) {
    no_drama_mkdir(dest)
    wd <- fn_data2wd(fn_data,dest)
    no_drama_mkdir(wd)
    fn_inp_stgs <- get_inp_stgs_fn(fn_data)
    fn_stgs <- get_stgs_fn(wd)
    fn_out_cmpd_l <- get_cmpd_l_fn(wd)

    gen_fn_stgs(fn_inp_stgs,fn_stgs)
    n_cmpd <- gen_cmpd_l(fn_cmpd_l,fn_out_cmpd_l)
    gen_ftable(fn_data,wd,n_cmpd)
}

reconf <- function(wd) {## Load the settings.
    fn_stgs <- get_stgs_fn(wd)
    RMassBank::loadRmbSettings(fn_stgs)
    
    ## Load the compound list.
    fn_cmpd_l <- get_cmpd_l_fn(wd)
    RMassBank::loadList(fn_cmpd_l)
}

##' Prescreens. Writes data out. Adapted from ReSOLUTION
##'
##' 
##' @title Prescreen
##' @param wd Absolute path to the directory that will contain the
##'     resulting data frame.
##' @param RMB_mode ...
##' @param FileList ...
##' @param cmpd_list ...
##' @param ppm_limit_fine ...
##' @param EIC_limit ...
##' @author Emma Schymanski, Todor Kondić
RMB_EIC_prescreen_df <- function (wd, RMB_mode, FileList, cmpd_list,
                                  ppm_limit_fine = 10, EIC_limit = 0.001) {


    n_spec <- 0
    cmpd_RT_maxI <- ""
    msms_found <- ""
    rts <- 0
    max_I_prec <- ""
    cmpd_RT_maxI_min <- ""
    file_list <- read.csv(FileList, stringsAsFactors = FALSE)
    cmpd_info <- read.csv(cmpd_list, stringsAsFactors = FALSE)
    ncmpd <- nrow(cmpd_info)
    odir=file.path(wd,"prescreen")
    no_drama_mkdir(odir)
    get_width <- function(maxid) {log10(maxid)+1}
    id_field_width <- get_width(ncmpd)

    fn_out<- function(id,suff) {file.path(odir,paste(formatC(id,width=id_field_width,flag=0),suff,".csv",sep=''))}
    f <- mzR::openMSfile(file_list$Files[1])
    for (i in 1:length(file_list$ID)) {
        cpdID <- file_list$ID[i]
        n_spec <- n_spec + 1
        smiles <- tryCatch(RMassBank::findSmiles(cpdID), error = function(e) NA)
        if (!is.na(smiles)) {
            mz <- as.numeric(RMassBank::findMz(cpdID, RMB_mode)[3])
        }
        else {
            mz <- as.numeric(RMassBank::findMz(cpdID, RMB_mode, retrieval = "unknown")[3])
        }
        eic <- RMassBank::findEIC(f, mz, limit = EIC_limit)
        msms_found[n_spec] <- FALSE
        msms <- RMassBank::findMsMsHR.mass(f, mz, 0.5, RMassBank::ppm(mz, ppm_limit_fine, 
                                                           p = TRUE))
        max_I_prec_index <- which.max(eic$intensity)
        cmpd_RT_maxI[n_spec] <- eic[max_I_prec_index, 1]
        max_I_prec[n_spec] <- eic[max_I_prec_index, 2]
        cmpd_RT_maxI_min[n_spec] <- as.numeric(cmpd_RT_maxI[n_spec])/60
        ## plot.new()
        ## plot.window(range(eic$rt), range(eic$intensity))
        ## box()
        ## lines(eic$intensity ~ eic$rt)
        write.csv(x=eic[c("rt","intensity")],file=fn_out(cpdID,".eic"),row.names=F)
        cpd_df <- data.frame("rt"=c(),"intensity"=c())
        for (specs in msms) {
            if (specs@found == TRUE) {
                
                df <- do.call(rbind, lapply(specs@children, function(sp) c(sp@rt, 
                                                                           intensity = max(sp@intensity))))
                cpd_df <- rbind(cpd_df,df,make.row.names = F)
                ## lines(intensity ~ retentionTime, data = df, type = "h", 
                ##       col = "blue")
                msms_found[n_spec] <- TRUE
            }
        }
        if (nrow(cpd_df)>0) write.csv(x=cpd_df,file=fn_out(cpdID,".kids"),row.names=F)
        ## title(main = cpdID, xlab = "RT (sec)", ylab = "Intensity")
        ## text(as.numeric(cmpd_RT_maxI[n_spec]), as.numeric(max_I_prec[n_spec]), 
        ##      labels = as.numeric(cmpd_RT_maxI_min[n_spec]), pos = 4)
        ## axis(1)
        ## axis(2)
        ## gc()
        rts[i] <- (cmpd_RT_maxI[n_spec])
    }
    # dev.off()
    write.csv(cbind(file_list$ID, cmpd_info$mz, cmpd_info$Name, 
                    cmpd_RT_maxI, cmpd_RT_maxI_min, max_I_prec, msms_found), 
              file = file.path(odir,"RTs_wI.csv"), 
              row.names = F)
}

##' Plot the output of prescreen.
##'
##' @title Plot the Output of Prescreen
##' @param wd Sequence of data dirs containing the prescreen subdir.
##' @param out The name of the output file.
##' @param pal ColorBrewer palette name.
##' @param cex As in legend.
##' @param digits Number of significant digits for peak ret times. 
##' @return Nothing useful.
##' @author Todor Kondić
##' @export
presc.plot <- function(wd,out="prescreen.pdf",pal="Dark2",cex=0.75,digits=6) {
    dfdir <- file.path(wd,"prescreen")
    pdf(out)
    ## Get the basenames of eic files.
    eics <- list.files(path=dfdir[[1]],patt=".*eic.csv")
    maybekids <- sapply(strsplit(eics,split="\\."),function(x) {paste(x[[1]][1],'.kids.csv',sep='')})
    for (i in seq(length(eics))) {
        eic <- eics[[i]]
        maybekid <- maybekids[[i]]
        fn_ini <- lapply(wd,get_stgs_fn)
        
        lbls <- lapply(fn_ini,function(x) {s <- yaml::yaml.load_file(x);s$spectraList[[1]]$ce})
        plot.new()
        dfs <- lapply(file.path(dfdir,eic),function(fn) {
            tryCatch(read.csv(fn,stringsAsFactors = F),
                     error=function(e) {message(paste(e,"; offending file:",fn))})
        })
        
        dfs <- lapply(dfs,function(x) data.frame(rt=x$rt/60.,intensity=x$intensity))

        ## Find existing children.
        maybes <- file.path(dfdir,maybekid)
        indkids <- which(file.exists(maybes))
        kids <- maybes[indkids]
        dfs_kids <- lapply(kids,read.csv,stringsAsFactors=F)
        dfs_kids <- lapply(dfs_kids,function(x) data.frame(rt=x$retentionTime/60.,intensity= -x$intensity))


        ## Find max intensities.
        w_max <- sapply(dfs,function (x) which.max(x$intensity))
        rt_max <- Map(function(df,w) df$rt[[w]],dfs,w_max)
        i_max<- Map(function(df,w) df$intensity[[w]],dfs,w_max)
        symbs <- LETTERS[1:length(w_max)]

        ## Find max intensities in children
        w_max_kids <- sapply(dfs_kids,function (x) which.max(abs(x$intensity)))
        rt_max_kids <- Map(function(df,w) df$rt[[w]],dfs_kids,w_max_kids)
        i_max_kids <- Map(function(df,w) df$intensity[[w]],dfs_kids,w_max_kids)
        symbs_kids<- letters[indkids]

        
        
        rt_rng <- range(sapply(dfs,function(x) x$rt))
        int_rng <- range(sapply(append(dfs_kids,dfs),function(x) x$intensity))
        plot.window(rt_rng,c(1.2*int_rng[[1]],1.3*int_rng[[2]]))
        box()
        cols <- RColorBrewer::brewer.pal(n=length(dfs),name=pal)
        lgnd <- Map(function(k,v) paste(k,"= ",v,sep=''),symbs,format(rt_max,digits=digits))
        linfo <- legend("topleft",horiz=T,legend=lbls,col=cols,fill=cols,bty="n",cex=cex)
        legend(x=linfo$rect$left,y=linfo$rect$top-0.5*linfo$rect$h,horiz=T,legend=lgnd,fill=cols,bty="n",cex=cex)

        cols_kids <- cols[indkids]
        lgnd_kids <- Map(function(k,v) paste(k,"= ",v,sep=''),symbs_kids,format(rt_max_kids,digits=digits))
        if (length(lgnd_kids)>0) legend(x="bottomleft",horiz=T,legend=lgnd_kids,fill=cols[indkids],bty="n",cex=cex)

        ## Plot eic across the directory set.
        for (n in seq(length(dfs))) {
            df <- dfs[[n]]
            col <- cols[[n]]
            lines(df$intensity ~ df$rt,col=col)
        }

        if (length(dfs_kids) >0) {
            for (k in 1:length(indkids)) {
                lines(intensity ~ rt,data=dfs_kids[[k]],type="h",col=cols_kids[[k]])
            }
        }
        title(main=i,xlab="retention time [min]",ylab="intensity")
        for (k in seq(length(w_max))) text(rt_max[[k]],i_max[[k]],labels=symbs[[k]],pos=4,offset=0.5*k)
        if (length(dfs_kids)>0) for (k in seq(length(w_max_kids))) text(rt_max_kids[[k]],i_max_kids[[k]],labels=symbs_kids[[k]],pos=4,offset=0.5*k)
        axis(1)
        axis(2)
        gc()
    }
    dev.off()
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


