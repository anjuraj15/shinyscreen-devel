## Constants
FN_FTAB_BASE<-"ftable.base.csv"
FN_FTAB_PP<-"ftable.pp.csv"
FN_PP_OUT_PREF<-"PP.filetable"
FN_FTAB<-"ftable.csv"
FN_CMP_L<-"compounds.csv"
FN_LOC_SETID <-"setid.csv"
FN_COMP_TAB<-"comprehensive.csv"
MODEMAP<-list(pH="MpHp_mass",
              mH="MmHm_mass",
              pNH4="MpNH4_mass",
              pNa="MpNa_mass")

DEFAULT_RT_RANGE=c(NA,NA)

QANAMES <- c("MS1","MS2","Alignment","AboveNoise")
PLOT_DEF_TAGS<-NA
PLOT_DEF_SET<-NA

CEX<-0.75
RT_DIGITS=2
M_DIGITS=4
PAL="Dark2"

REST_TXT_INP<-c("fnStgsRMB",
                "fnTgtL",
                "fnUnkL",
                "fnSetId",
                "tagsInp",
                "confFileTabBase",
                "confFileTabProcInp",
                "confResFileTab")

## ***** Helper Functions *****
tab2file<-function(tab,file,...) {
    write.csv(x=tab,file=file,row.names=F,...)
}

file2tab<-function(file,stringsAsFactors=F,comment.char='',...) {
    read.csv(file=file,
             header=T,
             stringsAsFactors=stringsAsFactors,
             comment.char=comment.char,
             na.strings=c("","NA"),...)
}

isThingFile<-function(fn) {
    if (length(fn)>0 && is.character(fn)) {
        file.exists(fn)
    } else F
}

## ***** End helper functions *****
ppInpFt<-function() {
    tempfile(pattern=FN_PP_OUT_PREF,fileext=".csv")
}

stripext<-function(fn) {
    bits<-strsplit(fn,split="\\.")[[1]]
    if (length(bits)> 1) paste(head(bits,-1),collapse=".") else fn}

idsFromFiles<-function(setDir) {
    fls<-list.files(path=setDir,patt=".*eic.*csv",rec=T)
    bas<-basename(fls)
    res<-strsplit(bas,"\\.")
    sapply(res,function (r) as.integer(r[[1]]))
}


readCmpList<-function(fn) {
    read.csv(file=fn,
             header=T,
             stringsAsFactors = F,
             comment.char = '')
}


importCmpList<-function(fn) {
    df<-readCmpList(fn)
    dfNm<-colnames(df)
    nRow<-nrow(df)

    naCol<-rep(NA,nRow)
    blankCol<-rep("",nRow)
    if (! "CAS" %in% dfNm) df$CAS<-naCol
    if (! "Name" %in% dfNm) df$Name<-blankCol
    if (! "RT" %in% dfNm) df$RT<-naCol
    mzIn=as.logical(match("mz",dfNm,nomatch=F))
    SMILESIn=as.logical(match("SMILES",dfNm,nomatch=F))
    if (! (mzIn || SMILESIn)) stop("Either `mz', or `SMILES' columns must be present in the compound list.")

    if (! ("mz" %in% dfNm)) df$mz<-naCol
    if (! ("SMILES" %in% dfNm)) df$SMILES<-blankCol

    if (! ("Level" %in% dfNm)) df$Level<-1
    for (ri in 1:nRow) {
        if (emptyfield(df$SMILES[[ri]])) {
            if (! emptyfield(df$mz[[ri]])) {
                df$Level[[ri]]<-5
            } else
                stop ("At row ",ri," of the input compound list, there are neither SMILES, nor Mass to be found.")
        }
    }

    df
    
    
}

getMzFromCmpL<-function(id,mode,cmpL) {
    ind<-match(id,cmpL$ID)
    mz<-cmpL$mz[[ind]]
    smiles<-cmpL$SMILES[[ind]]
    res<-if (!is.null(mz) && !is.na(mz)) {
             mz
         } else if (nchar(smiles)>0)
         {
             mde<-as.character(mode)
             wh<-MODEMAP[[mde]]
             RChemMass::getSuspectFormulaMass(smiles)[[wh]]
         } else stop("Both SMILES and mz fields, for ID ",id,", found empty in the compound list. Aborting.")
    res
}

getSMILESFromCmpL<-function(id,cmpL) {
    ind<-match(id,cmpL$ID)
    smiles<-cmpL$SMILES[[ind]]
    smiles
}

getColFromCmpL<-function(id,cname,cmpL) {
    ind<-match(id,cmpL$ID)
    cmpL[[cname]][[ind]]
}

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

get_presc_d <- function(wd) {wd}
gen_presc_d <- function(wd) dir.create(wd,recursive = T,showWarnings = F)
    
    

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

emptyfield <- function (f) {length(f) == 0 | is.na(f) | f == ""}

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
    df<-read.csv(src_fn,sep=',',stringsAsFactors=F,comment.char='')

    ## Names
    nms<-df$Name
    ## CAS
    casvals<-df$CAS
    ## RT
    rt<- df$RT
    
    if (is.null(casvals)) casvals <- rep(NA,sz)
    if (is.null(nms)) nms <- rep(NA,nrow(df))
    if (is.null(rt)) rt <- rep(NA,nrow(df))
    
    odf <- data.frame(ID=df$ID,Name=nms,SMILES="",mz=NA,RT=rt,Level=3,CAS=casvals,stringsAsFactors=F)

    for (ri in 1:nrow(df)) {
        if (emptyfield(df$SMILES[ri])) {
            if (! emptyfield(df$Mass[ri])) {
                odf$mz[ri] <- df$Mass[ri]
                odf$Level[ri] <- 5
            } else 
                stop ("At row ",ri," of the input compound list, there are neither SMILES, nor Mass to be found.")
            
        } else odf$SMILES[ri] <- df$SMILES[ri]
    }
    
    f <- Vectorize(function (dest_fn) {
        write.csv(odf,file=dest_fn,row.names=F,na="")
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
    RMassBank::loadList(fn_comp,check=F) #reduce universality of this statement!!!
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
gen_ftable_old <- function(fn_data,wd,n_cmpd) {
    f <- Vectorize(function(fn_data,wd) {
        df_table<-data.frame(Files=rep(fn_data,n_cmpd),ID=1:n_cmpd)
        fn_table<-get_ftable_fn(wd)
        write.csv(x=df_table,file=fn_table,row.names=F)
        fn_table
    }, vectorize.args=c("fn_data","wd"))

    f(fn_data,wd)
}

gen_ftable <- function(fTab,file) {
    df<-fTab[fTab$Files %in% file,]
    wd<-unique(df$wd)
    tab2file(tab=df,file=get_ftable_fn(wd))
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
RMB_EIC_prescreen_df_old <- function (wd, RMB_mode, FileList, cmpd_list,
                                  ppm_limit_fine = 10, EIC_limit = 0.001) {


    n_spec <- 0
    cmpd_RT_maxI <- ""
    msms_found <- ""
    rts <- 0
    max_I_prec <- ""
    cmpd_RT_maxI_min <- ""
    file_list <- read.csv(FileList, stringsAsFactors = FALSE,comment.char='')
    cmpd_info <- read.csv(cmpd_list, stringsAsFactors = FALSE,comment.char='')
    ncmpd <- nrow(cmpd_info)
    odir=wd
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

        write.csv(x=eic[c("rt","intensity")],file=fn_out(cpdID,".eic"),row.names=F)
        cpd_df <- data.frame("rt"=c(),"intensity"=c())
        for (specs in msms) {
            if (specs@found == TRUE) {
                
                df <- do.call(rbind, lapply(specs@children, function(sp) c(sp@rt, 
                                                                           intensity = max(sp@intensity))))
                cpd_df <- rbind(cpd_df,df,make.row.names = F)

                msms_found[n_spec] <- TRUE
            }
        }
        if (nrow(cpd_df)>0) write.csv(x=cpd_df,file=fn_out(cpdID,".kids"),row.names=F)

        rts[i] <- (cmpd_RT_maxI[n_spec])
    }

    write.csv(cbind(file_list$ID, cmpd_info$mz, cmpd_info$Name, 
                    cmpd_RT_maxI, cmpd_RT_maxI_min, max_I_prec, msms_found), 
              file = file.path(odir,"RTs_wI.csv"), 
              row.names = F)
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
RMB_EIC_prescreen_df_old1 <- function (wd, RMB_mode, FileList, cmpd_list,
                                       ppm_limit_fine = 10, EIC_limit = 0.001) {

    n_spec <- 0
    cmpd_RT_maxI <- ""
    msms_found <- ""
    rts <- 0
    max_I_prec <- ""
    cmpd_RT_maxI_min <- ""
    file_list <- read.csv(FileList, stringsAsFactors = FALSE,comment.char='')
    cmpd_info <- read.csv(cmpd_list, stringsAsFactors = FALSE,comment.char='')
    ncmpd <- nrow(cmpd_info)
    odir=wd
    fid <- file_list$ID
    cmpind <- which(cmpd_info$ID %in% fid)
    mzCol <- cmpd_info$mz[cmpind]
    nmCol <- cmpd_info$Name[cmpind]
    get_width <- function(maxid) {log10(maxid)+1}
    id_field_width <- get_width(ncmpd)

    fn_out<- function(id,suff) {file.path(odir,paste(formatC(id,width=id_field_width,flag=0),suff,".csv",sep=''))}
    f <- mzR::openMSfile(file_list$Files[1])
    for (i in 1:length(file_list$ID)) {
        cpdID <- file_list$ID[i]
        n_spec <- n_spec + 1
        smiles <- tryCatch(RMassBank::findSmiles(cpdID), error = function(e) NA)
        mz<-if (!is.na(smiles)) {
                mz <- as.numeric(RMassBank::findMz(cpdID, RMB_mode)[3])
            } else {
                mzCol[[i]]  ## TODOR REMOVE mz <- as.numeric(RMassBank::findMz(cpdID, RMB_mode, retrieval = "unknown")[3])
            }
        ## TODOR REMOVED if (is.na(mzCol[[i]])) mzCol[[i]] <- mz ## infer from findMz.
        eic <- RMassBank::findEIC(f, mz, limit = EIC_limit)
        msms_found[n_spec] <- FALSE
        msms <- RMassBank::findMsMsHR.mass(f, mz, 0.5, RMassBank::ppm(mz, ppm_limit_fine, 
                                                                      p = TRUE))

        max_I_prec_index <- which.max(eic$intensity)
        cmpd_RT_maxI[n_spec] <- eic[max_I_prec_index, 1]
        max_I_prec[n_spec] <- eic[max_I_prec_index, 2]
        cmpd_RT_maxI_min[n_spec] <- as.numeric(cmpd_RT_maxI[n_spec])/60 ## conversion to minutes

        if (length(eic$rt)>0) eic$rt <- eic$rt/60 ## conversion to minutes

        write.csv(x=eic[c("rt","intensity")],file=fn_out(cpdID,".eic"),row.names=F)
        bindKids <- function(kids)
            do.call(rbind,lapply(kids,function (kid)
                c(rt=kid@rt,intensity=max(kid@intensity))))

        
        bindSpec <- function(specLst) {
            do.call(rbind,lapply(specLst,function (sp) bindKids(sp@children)))
        }
        
        found <- which(vapply(msms,function(sp) sp@found,FUN.VALUE=F))
        msmsExst <- msms[found]
        ## message("found:",found)
        ## message("Lall:",length(msms))
        ## message("Lsome:",length(msmsExst))
        if (length(found)>0) {
            msms_found[n_spec] <- T
            msmsTab <- as.data.frame(bindSpec(msmsExst),stringsAsFactors=F)
            names(msmsTab) <- c("rt","intensity")
            if (nrow(msmsTab)>0) {
                msmsTab$rt <- msmsTab$rt/60 ## conversion to minutes
                write.csv(x=msmsTab,file=fn_out(cpdID,".kids"),row.names=F)
            }
        }

        rts[i] <- (cmpd_RT_maxI[n_spec])
    }
    mzR::close(f)
    rtwiDf <- data.frame(ID=file_list$ID, mz=mzCol, Name=nmCol, 
                         cmpd_RT_maxI=cmpd_RT_maxI, cmpd_RT_maxI_min=cmpd_RT_maxI_min,
                         max_I_prec=max_I_prec, msms_found=msms_found,stringsAsFactors=F)
    
    write.csv(rtwiDf, file = file.path(odir,"RTs_wI.csv"), row.names = F)
}


RMB_EIC_prescreen_df <- function (wd, RMB_mode, FileList,
                                  ppm_limit_fine = 10, EIC_limit = 0.001) {


    file_list <- file2tab(file=FileList)
    file_list<-file_list[file_list$wd %in% wd,]
    ncmpd <- max(file_list$ID) # length(levels(factor(file_list$ID))) #nrow(cmpd_info)
    odir=wd
    fid <- file_list$ID
    tag<-unique(file_list$tag)
    modes<-unique(file_list$mode)
    message("how many modes? ",length(modes))
    fnProg<-file.path(wd,"progress.log")
    unlink(fnProg,force=T)
    cat("i","total\n",sep=",",file=fnProg)
    ## cmpind <- which(cmpd_info$ID %in% fid)
    get_width <- function(maxid) {log10(maxid)+1}
    id_field_width <- get_width(ncmpd)
    fn_out<- function(id,suff) {file.path(odir,paste(formatC(id,width=id_field_width,flag=0),suff,".csv",sep=''))}
    f <- mzR::openMSfile(file_list$Files[[1]])
    total<-nrow(file_list)
    n_spec <- 0
    cmpd_RT_maxI <- rep(0.0,total)
    msms_found <- rep(F,total)
    rts <- rep(0.0,total)
    max_I_prec <- rep(0.0,total)
    cmpd_RT_maxI_min <- rep(0.0,total)
    tellme<-function(i,point) NULL ##message(">>> tag: ", tag, "i: ",i,"point: ", point)
    for (i in 1:total) {
        tellme(i,"A1")
        cpdID <- file_list$ID[[i]]
        n_spec <- i
        mz<-file_list$mz[[i]]
        tellme(i,"A2")

        tellme(i,"B1")
        eic <- RMassBank::findEIC(f, mz, limit = EIC_limit)
        tellme(i,"B2")

        tellme(i,"C1")
        msms_found[i] <- FALSE
        theppm<-RMassBank::ppm(mz, ppm_limit_fine,p = TRUE)
        tellme(i,paste("C1 mz:",mz))
        tellme(i,paste("C1 ppm:",theppm))
        msms <- RMassBank::findMsMsHR.mass(f, mz, 0.5, theppm)
        max_I_prec_index <- which.max(eic$intensity)
        cmpd_RT_maxI[i] <- eic[max_I_prec_index, 1]
        max_I_prec[i] <- eic[max_I_prec_index, 2]
        cmpd_RT_maxI_min[i] <- as.numeric(cmpd_RT_maxI[i])/60 ## conversion to minutes
        tellme(i,"C2")

        tellme(i,"D1")
        if (length(eic$rt)>0) eic$rt <- eic$rt/60 ## conversion to minutes
        tab2file(tab=eic[c("rt","intensity")],file=fn_out(cpdID,".eic"))
        bindKids <- function(kids)
            do.call(rbind,lapply(kids,function (kid)
                c(rt=kid@rt,intensity=max(kid@intensity))))

        
        bindSpec <- function(specLst) {
            do.call(rbind,lapply(specLst,function (sp) bindKids(sp@children)))
        }
        tellme(i,"D2")
        tellme(i,"E1")
        found <- which(vapply(msms,function(sp) sp@found,FUN.VALUE=F))
        msmsExst <- msms[found]
        if (length(found)>0) {
            msms_found[i] <- T
            msmsTab <- as.data.frame(bindSpec(msmsExst),stringsAsFactors=F)
            names(msmsTab) <- c("rt","intensity")
            if (length(msmsTab)>0 && nrow(msmsTab)>0) {
                msmsTab$rt <- msmsTab$rt/60 ## conversion to minutes
                tab2file(tab=msmsTab,file=fn_out(cpdID,".kids"))
            }
        }
        tellme(i,"E2")

        tellme(i,"F1")
        rts[i] <- cmpd_RT_maxI[i]
        cat(i,total,"\n",file=fnProg,append=T,sep=",")
        tellme(i,"F2")
    }
    mzR::close(f)
    rtwiDf <- data.frame(ID=file_list$ID, mz=file_list$mz, Name=file_list$Name, 
                         cmpd_RT_maxI=cmpd_RT_maxI, cmpd_RT_maxI_min=cmpd_RT_maxI_min,
                         max_I_prec=max_I_prec, msms_found=msms_found,stringsAsFactors=F)
    write.csv(rtwiDf, file = file.path(odir,"RTs_wI.csv"), row.names = F)
}

preProc <- function (fnFileTab,lCmpdList,fnDest=paste(stripext(fnFileTab),"_candidate.csv",sep=''),noiseFac=3,rtDelta=0.5,intTresh=1e5) {
    ## read in .csv file as file
    ftable <- read.csv(file = fnFileTab, header = T, sep=",", stringsAsFactors = F,comment.char='')
    getWidth <- function(maxid) {log10(maxid)+1}
    ids <- as.numeric(levels(factor(ftable$ID)))
    id_field_width <- getWidth(lCmpdList)
    fn_out<- function(id,suff,wd) {
        patt<-paste("^0*",id,suff,".csv$",sep='')
        ## paste(formatC(id,width=id_field_width,flag=0),suff,".csv",sep='')
        res<-list.files(path=wd,pattern=patt,full.names=T)
        if (length(res)>0) res else character(0)

    }
    
    ## For loop through dataframe called file to set thresholds.
    ftable[c("MS1","MS2","Alignment","AboveNoise")] <- T
    ftable["MS2rt"] <- NA
    ## QA check plan:
    ##
    ## If MS1 does not exist, set MS1 to F, as well as everything else except MS2.
    ## If it exists, proceed to noise check.
    ## If noise check fails, set AboveNoise and Alignment to F.
    ##
    ##
    ## MS2 will be checked independently.
    ## If MS2 does not exist, set MS2 and Alignment to F.
    ## If it does, check the Alignment.
    ## If Alignment is wrong, set Alignment to F.
    ##
    ## Terminology: MS1 does not exist if the intensity is below the
    ## intensity threshold. MS2 does not exist if it was not picked up
    ## during the dataframe generation stage. In this case, the file
    ## with the corresponding ID will not be there.

    ftable$Comments <- ""
    for (ind in 1:nrow(ftable)) {
        wd <- ftable$wd[ind]
        id <- ftable$ID[ind]
        odir=file.path(wd)
        fn_eic <- fn_out(id,".eic",wd)
        eic <- NULL
        maxInt <- NULL
        if (length(fn_eic)==0) {
            message("wd: ",wd,"fn_eic: ",fn_eic)
            warning("No file for id ",id," . Skipping.")
            next
        }
        eic <- read.csv(fn_eic, sep = ",", stringsAsFactors = F,comment.char='')
        maxInt <- max(eic$intensity)
        ##If MS1 does not exist, set entry to F.
        if (maxInt < intTresh) {
            ftable[ind,"MS1"] <- F
            ## Other checks automatically fail, too.
            ftable[ind,"Alignment"] <- F
            ftable[ind,"AboveNoise"] <- F
        } else {
            ## Noisy?
            if (ftable[ind,"AboveNoise"]) {
                mInt <- mean(eic$intensity)
                if (maxInt < noiseFac*mInt) {
                    ftable[ind,"AboveNoise"] <- F
                    ftable[ind,"Alignment"] <- F ## If noisy, this is
                                                 ## probably meaningles, so
                                                 ## F.
                }
                
            }
        }
            
    

        ## MS2 checks.
        fn_kids <- fn_out(id,".kids",wd)
        if (length(fn_kids)==0) {
            ftable[ind,"MS2"] <- F
            ftable[ind,"Alignment"] <- F
        } else {
            ## Alignment still makes sense to be checked?
            if (ftable[ind,"Alignment"]) {
                rtInd <- match(maxInt,eic$intensity)
                rtMS1Peak <- eic$rt[[rtInd]]
                msms <- read.csv(fn_kids, sep = ",", stringsAsFactors = F,comment.char='')
                rtInd <- which(msms$rt > rtMS1Peak - rtDelta & msms$rt < rtMS1Peak + rtDelta)
                msmsRT <- msms$rt[rtInd]
                if (length(msmsRT) > 0) {
                    ftable[ind,"iMS2rt"] <- which.min(abs(msmsRT - rtMS1Peak))
                    ftable[ind,"MS2rt"] <- msmsRT[ftable[ind,"iMS2rt"]]
                }
            }
        }
    }
    write.csv(ftable, file = fnDest,row.names=F)
}





##' Helper function for rendersmiles2
##'
##' @title Render Compound from an Online Resource
##' @param depictURL The URL of the object to plot.
##' @param coords The positioning of the image (in data coords).
##' @param filename Temp filename.
##' @return Nothing useful.
##' @author Todor Kondić
renderurl <- function(depictURL,coords=c(0,0,100,100), filename=tempfile(fileext=".svg")) {
    h <- new_handle()
    curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer=0)
    curl::curl_download(url=depictURL,filename,handle=h)
    img <- rsvg(filename)
    if (length(img)>2) {
        rasterImage(img,xleft=coords[1],ybottom=coords[2],xright=coords[3],ytop=coords[4])
    }
}


## rendersmiles <- function(smiles, kekulise=TRUE, coords=c(0,0,100,100), width=200, height=200,
##                               zoom=1.3,style="cow", annotate="off", abbr="on",suppressh=TRUE,
##                               showTitle=FALSE, smaLimit=100, sma=NULL) {
##   dep <- get.depictor(width = width, height = height, zoom = zoom, style = style, annotate = annotate,
##                       abbr = abbr, suppressh = suppressh, showTitle = showTitle, smaLimit = smaLimit,
##                       sma = NULL)
##   library(rcdk)
##   library(RChemMass)
##   mol <- getMolecule(smiles)
##   img <- view.image.2d(mol, depictor=dep)
##   rasterImage(img, coords[1],coords[2], coords[3],coords[4])

## }

##' Render smiles from an online resource.
##'
##' @title Turn SMILES to an Image Using Online Resource
##' @param smiles The SMILES string.
##' @param style Structure style.
##' @param ... Hand over to renderurl.
##' @return Nothing useful.
##' @author Todor Kondić
rendersmiles2 <- function(smiles,style="cow",...) {
    dpurl <- buildCDKdepictURL(smiles,style=style)
    renderurl(dpurl,filename=tempfile(fileext=".svg"),...)
}

calcLogTics <- function(lims,powUnit=1,linDiv=1,howMany=NULL) {
    ## Find integer power limits.
    llim <- log10(lims)
    llim[is.infinite(llim)] <- 0
    nlim <- llim/powUnit
    ilim <- round(nlim)*powUnit

    all <- if (linDiv>1) {
               z <- sapply(seq(ilim[1],(ilim[2]-1),by=powUnit),
                           function(i) {
                               a <- 10.**i
                               b <- 10.**(i+1)
                               st <- b/linDiv
                               s <- seq(0,b,by=st)
                               s[2:length(s)]
                           })
               dim(z) <- prod(dim(z))
               z
           } else
               10**seq(ilim[1],ilim[2],by=powUnit)
    
    res <- if (!is.null(howMany)) {
               if (howMany<length(all)) {
                   step <- length(all) %/% howMany    
                   ind <- seq(1,howMany*step,by=step)
                   rev(rev(all)[ind])
               } else
                   return(NULL)
               
           } else
               all

    res

}

calcLabels <- function(ticVals) {
    pw <- as.integer(log10(abs(ticVals)))
    mags <- 10**pw
    mags[is.na(mags)] <- 0
    pw[is.na(mags)] <- 0
    mant <- signif(ticVals/mags,3)
    zz <- Map(function (m,p) c(m=m,p=p),mant,pw)
    sapply(zz,function (z) {as.expression(bquote(.(z['m']) %*% 10^.(z['p'])))},USE.NAMES = F)
}

arrPlot <- function(xlim,ylim,ytics,xaxis=F,log=NULL,cex=0.2) {
    ylim[is.na(ylim)] <- 1
    ylim[ylim == 0] <- 1
    if (is.null(ylim)) ylim <- c(1,10)

    if (xaxis) xaxt="s" else xaxt="n"
    if (! is.null(log) && ! any(is.na(ytics)) ) {
        plot(1,1,xlab="",ylab="",xlim = xlim,ylim = ylim,type="n",log=log,xaxt=xaxt,yaxt = "n")
        message("ytics:",ytics)
        ltics <- calcLabels(ytics)
        axis(side=2,at=ytics,labels=ltics,las=2,cex=cex,gap.axis = -1)
    } else {
        plot(1,1,xlab="",ylab="",xlim = xlim,ylim = ylim,type="n",xaxt = xaxt)
        axis(side=2,las=2,cex=cex)
    }

}

arrPlotStd <- function(xlim,ylim,xaxis=F,log=log,cex=1.5,mar,intTresh) {
    if (ylim[1]<intTresh) ylim[1] <- intTresh
    if  (is.na(ylim[2])) ylim[2] <- 10
    if (xaxis) xaxt="s" else xaxt="n"
    par(mar=mar)
    plot(1,1,xlab="",ylab="",xlim = xlim,ylim = ylim,type="n",log=log,xaxt=xaxt,yaxt = "n",cex.axis=cex)
    ytics <- if (log=="y") axTicks(side=2, nintLog = 3) else axTicks(side=2)
                                                                             
    ltics <- calcLabels(ytics)
    axis(side=2,at=ytics,labels=ltics,las=2,cex.axis=cex,gap.axis = -1)
    
}


cmpdID2nm_1 <- function(id) paste("id",id,sep='')
cmpdIDnm <- Vectorize(cmpdID2nm_1)
plot_id_aux <- function(i,wd,eics,maybekids,mass,smile,tags,fTab,logYAxis,pal="Dark2",cex=0.75,rt_digits=2,m_digits=4,rtrange=NULL) {
    clean_rtrange <- function(def) {
            x1 <- rtrange[1]
            x2 <- rtrange[2]
            if (is.na(x1) || x1 == 0) x1 <- def[1]
            if (is.na(x2) || x2 == 0) x2 <- def[2]

            c(x1,x2)
    }




    if (logYAxis == "linear") log = ""
    if (logYAxis == "log") log = "y"
    
    LEFT_MARGIN=9
    ##FIXME: fTab will break presc.plot.
    recs <- fTab[fTab$ID %in% as.integer(i),c("wd","MS2rt","iMS2rt")]
    ## osmesi <- fTab[fTab$ID %in% as.integer(i),"SMILES"]
    message("smile arg:",smile)
    MS2Peak <- sapply(wd,function(x) recs[recs$wd %in% x,"MS2rt"])
    iMS2Peak <- sapply(wd,function(x) recs[recs$wd %in% x,"iMS2rt"])
    eic <- eics[[i]]
    maybekid <- maybekids[[i]]
    dfs <- lapply(file.path(wd,eic),function(fn) {
        tryCatch(read.csv(fn,stringsAsFactors = F,comment.char=''),
                 error=function(e) {message(paste(e,"; offending file:",fn))})
    })
    dfs <- lapply(dfs,function(x) data.frame(rt=x$rt,intensity=x$intensity))

    ## Find existing children.
    maybes <- file.path(wd,maybekid)
    indkids <- which(file.exists(maybes))
    kids <- maybes[indkids]
    dfs_kids <- lapply(kids,read.csv,stringsAsFactors=F,comment.char='')
    MS2Peak <- MS2Peak[indkids]
    iMS2Peak <- iMS2Peak[indkids]
    #dfs_kids <- lapply(dfs_kids,function(x) data.frame(rt=x$retentionTime,intensity= x$intensity))


    ## Find max intensities.
    w_max <- sapply(dfs,function (x) which.max(x$intensity))
    rt_max <- Map(function(df,w) df$rt[[w]],dfs,w_max)
    i_max<- Map(function(df,w) df$intensity[[w]],dfs,w_max)
    symbs <- LETTERS[1:length(w_max)]

    ## Find max intensities in children
    w_max_kids <- sapply(dfs_kids,function (x) which.max(abs(x$intensity)))
    rt_near_kids <-  Map(function(df,w) {if (!is.na(w) && !is.null(df$rt)) df$rt[[w]] else NA},dfs_kids,iMS2Peak)
    i_near_kids <- Map(function(df,w) {if (!is.na(w) && !is.null(df$intensity)) df$intensity[[w]] else NA},dfs_kids,iMS2Peak)
    symbs_kids<- letters[indkids]
    
    def_rt_rng <- range(sapply(dfs,function(x) x$rt))
    rt_rng <- if (is.null(rtrange))  def_rt_rng else clean_rtrange(def_rt_rng)

    int_rng<- range(sapply(dfs,function(x) x$intensity))
    int_rng_kids<- if (! is.null(dfs_kids))
                       range(sapply(dfs_kids,function(x) x$intensity)) else
                                                                           c(0,1)
    cols <- RColorBrewer::brewer.pal(n=length(dfs),name=pal)
    lgnd <- Map(function(k,v) paste(k,"= ",formatC(v,format="f",digits=rt_digits),sep=''),symbs,rt_max)
   
    layout(matrix(c(3,3,4,4,1,2), 3, 2, byrow = TRUE))
    ## par(mar=c(1,2,1,4))
    struc_xr <- c(0,100)
    struc_yr <- c(0,100)

    par(mar=c(1,LEFT_MARGIN,3,4))
    
    plot(1,1,type="n",xlab="",ylab="",xlim=struc_xr,ylim=struc_yr,xaxt="n",yaxt="n",asp=1,axes = FALSE)
    if (!emptyfield(smile))
        rendersmiles2(smile,coords=c(struc_xr[1],struc_yr[1],struc_xr[2],struc_yr[2]))
    col_eng <- c(0,100)
    peak_int <- c(0,100)
    par(mar=c(1,6,3,1))
    plot(1,1,type="n",xlab="",ylab="",xlim=col_eng,ylim=peak_int,xaxt="n",yaxt="n",axes = FALSE)
    linfo <- legend("topleft",horiz=T,legend=tags,col=cols,fill=cols,bty="n",cex=1.5)
    legend(x=linfo$rect$left,y=linfo$rect$top-1*linfo$rect$h,horiz=F,legend=lgnd,fill=cols,bty='n',cex=1.5)
    
    cols_kids <- cols[indkids]
    lgnd_kids <- Map(function(k,v) paste(k,"= ",tryCatch(formatC(v,digits=rt_digits,format="f"),error=function(e) "NA"),sep=''),symbs_kids,rt_near_kids)
    if (length(lgnd_kids)>0) legend(x=linfo$rect$left-14*linfo$rect$left,y=linfo$rect$top-1*linfo$rect$h,horiz=F,legend=lgnd_kids,fill=cols[indkids],bty="n",cex=1.5)


    arrPlotStd(xlim=rt_rng,ylim=int_rng,mar=c(0,LEFT_MARGIN,3,0),log=log,intTresh=1e4)
    mass<- if (!is.na(mass)) mass else "NA" 
    title(main=paste("ID:",i,"Ion m:",formatC(mass,digits=m_digits,format="f")))
    for (k in seq(length(w_max))) text(rt_max[[k]],i_max[[k]],labels=symbs[[k]],pos=4,offset=0.5*k)
    mtext("intensity",side = 2,adj=0.2,cex=1.3,line=7)
    ## Plot eic across the directory set.
    for (n in 1:length(dfs)) {
        df <- dfs[[n]]
        col <- cols[[n]]
        lines(intensity ~ rt,data=df,col=col)
    }


    if (length(dfs_kids) >0) {
        arrPlotStd(xlim=rt_rng,ylim=int_rng_kids,xaxis=T,log=log,mar=c(4,LEFT_MARGIN,0,0),intTresh=1)
        for (k in 1:length(indkids)) {
            lines(intensity ~ rt,data=dfs_kids[[k]],type="h",col=cols_kids[[k]])
        }
    } else {
        arrPlotStd(xlim=rt_rng,ylim=c(1,10),xaxis=T,log=log,mar=c(4,9,0,0),intTresh=1)
    }
    mtext("retention time [min]",side = 1,adj=0.5,cex=1.3,line = 3)
    if (length(dfs_kids)>0) for (k in seq(length(w_max_kids))) text(rt_near_kids[[k]],i_near_kids[[k]],labels=symbs_kids[[k]],pos=4,offset=0.5*k)    
    gc()
    message("loc X")
}    



adornmzMLTab<-function(df,projDir=getwd()) {
    pref<-df$set
    mask<-is.na(pref)
    drop<-df$files[mask]
    for (d in drop) warning("Dropping",d,"because no set specified for it.")
    df<-df[!mask,]
    pref<-df$set
    wd<-basename(tools::file_path_sans_ext(df$Files))
    wd<-file.path(projDir,pref,wd)
    df$wd<-wd
    df
}

genSuprFileTbl <- function(fileTbl,compTab,destFn="ftable.csv") {
    genOneFileTbl <- function(id,fileTbl) {
        n <- nrow(fileTbl)
        K <- length(id)
        longid <- rep(id,n)
        cols <- lapply(names(fileTbl),function(cn) rep("",n*K))
        names(cols) <- names(fileTbl)
        bdf <- as.data.frame(cols,stringsAsFactors = F)
        rows <- lapply(1:n*K,function(x) NA)
        for (j in 1:n) {
            for (i in 1:K)
                rows[[(j-1)*K+i]] <- fileTbl[j,]
        }
        bdf <- as.data.frame(do.call(rbind,rows),stringsAsFactors = F)
        bdf <- cbind(bdf,data.frame(ID=longid))
        bdf
    }
    sets <- levels(factor(compTab$set))
    setTbl <- lapply(sets,function (s) {
        sl1<-compTab$set %in% s
        sl2<-fileTbl$set==s
        if (!any(sl2)) stop("Set",s,"does not select anything in the currently processed files.")
        genOneFileTbl(compTab[sl1,]$ID,fileTbl[sl2,])

    })
    allTbl <- do.call(rbind,setTbl)
    allTbl 
}

getEntryFromComp<-function(entry,id,set,mode,compTab) {
    ind <- which(compTab$ID %in% id &
                 compTab$set %in% set &
                 compTab$mode %in% mode)

    res<- if (length(ind)==1) compTab[ind,entry] else {
                                                     if (length(ind)>1) {
                                                         stop("Nonunique entry selection in comprehensive table.")
                                                     } else {
                                                         stop("Entries not found for id ", id,"set ",set, "and mode ", mode, " .")
                                                     } 
                                                 }
    res
        
}
addCompColsToFileTbl<-function(ft,compTab) {
    nR<-nrow(ft)
    mzCol<-rep(NA,nR)
    nmCol<-rep("",nR)
    
    for (ir in 1:nR) {
        id<-ft[ir,"ID"]
        set<-ft[ir,"set"]
        m<-ft[ir,"mode"]
        entries<-getEntryFromComp(c("mz","Name"),id,set,m,compTab)
        mzCol[[ir]]<-  entries[[1]]
        nm<-entries[[2]]
        nmCol[[ir]]<- if (!is.na(nm)) nm else ""
    }
    ft$mz<-mzCol
    ft$Name<-nmCol
    ft
}
