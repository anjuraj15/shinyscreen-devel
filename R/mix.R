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

get_ftable_fn <- function(wd) {
    f <- function(wd) file.path(wd,"ftable.csv")
    fv <- Vectorize(f,vectorize.args=c("wd"))
    fv(wd)
}

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

gen_ftable <- function(fTab,file) {
    df<-fTab[fTab$Files %in% file,]
    wd<-unique(df$wd)
    tab2file(tab=df,file=get_ftable_fn(wd))
}

conf <- function(fn_data,fn_cmpd_l,dest) {
    no_drama_mkdir(dest)
    wd <- fn_data2wd(fn_data,dest)
    no_drama_mkdir(wd)
    fn_out_cmpd_l <- get_cmpd_l_fn(wd)
    n_cmpd <- gen_cmpd_l(fn_cmpd_l,fn_out_cmpd_l)
    gen_ftable(fn_data,wd,n_cmpd)
}

reconf <- function(wd) {## Load the settings.
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



preProc <- function (fnFileTab,fnDest=paste(stripext(fnFileTab),"_candidate.csv",sep=''),noiseFac=3,rtDelta=0.5,intThresh=1e5,intThreshMS2=0.05) {
    ## read in .csv file as file
    ftable <- read.csv(file = fnFileTab, header = T, sep=",", stringsAsFactors = F,comment.char='')
    ids <- as.numeric(levels(factor(ftable$ID)))


    fn_spec<-function(wd) readRDS(file.path(wd,FN_SPEC))
    ftable$Comments <- ""
    wds<-unique(ftable$wd)
    message("Loading RDS-es ...")
    allData<-lapply(wds,fn_spec)
    names(allData)<-wds
    message("... done with RDSs")
    names(allData)<-wds
    ## For loop through dataframe called file to set thresholds.
    ftable[c("MS1","MS2","Alignment","AboveNoise")] <- T
    ftable["MS2rt"] <- NA
    ftable["rt"]<-NA
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
 
    for (ind in 1:nrow(ftable)) {
        wd <- ftable$wd[ind]
        id <- ftable$ID[ind]
        ## odir=file.path(wd)
        ## fn_eic <- fn_out(id,".eic",wd)
        eics<-allData[[wd]]$eic
        nid<-id2name(id)
        ii<-match(id,MSnbase::fData(eics)[["ID"]]) #id, because id-s, not nid-s are in fData for ms1 eics;
        eic1<-eics[[ii]]
        eic<-data.frame(rt=MSnbase::rtime(eic1)/60.,intensity=MSnbase::intensity(eic1))
        colnames(eic)<-c("rt","intensity")
        maxInt <- NULL
        if (nrow(eic)==0) {
            warning("No chromatogram for id ",id," found in", wd, " . Skipping.")
            next
        }
        ms1MaxInd<-which.max(eic$intensity)
        maxInt<-eic$intensity[[ms1MaxInd]]
        ftable[ind,"rt"]<-eic$rt[[ms1MaxInd]]
        ##If MS1 does not exist, set entry to F.
        if (maxInt < intThresh) {
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
        ms2<-allData[[wd]]$ms2
        ms2nids<-names(ms2)
        mInt<-mean(eic$intensity)
        if (! (nid %in% ms2nids)) {
            ftable[ind,"MS2"] <- F
            ftable[ind,"Alignment"] <- F
        } else {
            sp<-ms2[[nid]]
            ## Alignment still makes sense to be checked?
            if (ftable[ind,"Alignment"]) {
                rtInd <- match(maxInt,eic$intensity)
                rtMS1Peak <- eic$rt[[rtInd]]
                msms<-MSnbase::fData(sp)[,c("rtm","maxI")]
                colnames(msms)<-c("rt","intensity")
                rtInd <- which((msms$rt > rtMS1Peak - rtDelta) &
                               (msms$rt < rtMS1Peak + rtDelta)) #Close enough?
                rtInd <- rtInd[which(msms$intensity[rtInd]>intThreshMS2*mInt)] #Intense enough?
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


smiles2img <- function(smiles, kekulise=TRUE, width=300, height=300,
                              zoom=1.3,style="cow", annotate="off", abbr="on",suppressh=TRUE,
                              showTitle=FALSE, smaLimit=100, sma=NULL) {
  dep <- rcdk::get.depictor(width = width, height = height, zoom = zoom, style = style, annotate = annotate,
                      abbr = abbr, suppressh = suppressh, showTitle = showTitle, smaLimit = smaLimit,
                      sma = NULL)

  mol <- RMassBank::getMolecule(smiles)
  z<-rcdk::view.image.2d(mol, depictor=dep)
  grid::rasterGrob(z)
}

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

arrPlotStd <- function(xlim,ylim,xaxis=F,log=log,cex=1.5,mar,intThresh) {
    if (ylim[1]<intThresh) ylim[1] <- intThresh
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


    arrPlotStd(xlim=rt_rng,ylim=int_rng,mar=c(0,LEFT_MARGIN,3,0),log=log,intThresh=1e4)
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
        arrPlotStd(xlim=rt_rng,ylim=int_rng_kids,xaxis=T,log=log,mar=c(4,LEFT_MARGIN,0,0),intThresh=1)
        for (k in 1:length(indkids)) {
            lines(intensity ~ rt,data=dfs_kids[[k]],type="h",col=cols_kids[[k]])
        }
    } else {
        arrPlotStd(xlim=rt_rng,ylim=c(1,10),xaxis=T,log=log,mar=c(4,9,0,0),intThresh=1)
    }
    mtext("retention time [min]",side = 1,adj=0.5,cex=1.3,line = 3)
    if (length(dfs_kids)>0) for (k in seq(length(w_max_kids))) text(rt_near_kids[[k]],i_near_kids[[k]],labels=symbs_kids[[k]],pos=4,offset=0.5*k)    
    gc()
    message("loc X")
}

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {


  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
      for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                                layout.pos.col = matchidx$col))
    }
  }
}


plot_id_msn <- function(i,data,rtMS1,rtMS2,mass,smile,tags,fTab,logYAxis,theme,pal="Dark2",cex=0.75,rt_digits=2,m_digits=4,rtrange=NULL) {
    clean_rtrange <- function(def) {
            x1 <- rtrange[1]
            x2 <- rtrange[2]
            if (is.na(x1) || x1 == 0) x1 <- def[1]
            if (is.na(x2) || x2 == 0) x2 <- def[2]

            c(x1,x2)
    }
    mk_title<-function() paste("EIC (","mz= ",mass,")",sep='')
    mk_leg_lab<-function(tag,rt) {paste(tag,"; rt= ",formatC(rtMS1[[tag]],format='f',digits=rt_digits),"min")}

    sci10<-function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))}

    if (logYAxis == "linear") log = ""
    if (logYAxis == "log") log = "y"
    ii<-name2id(i)

    ## MS1 EIC and MS2 spectral time series.
    dfschrms1<-lapply(tags,function(tag) {d<-data[[tag]]$eic
        ind<-match(ii,MSnbase::fData(d)[["ID"]])
        cg<-d[[ind]]
        data.frame(rt=MSnbase::rtime(cg)/60.,intensity=MSnbase::intensity(cg),tag=as.character(tag),legend=mk_leg_lab(tag,rtMS1[[tag]]))
    })

    dfChrMS1<-do.call(rbind,c(dfschrms1,list(make.row.names=F)))
    rtDefRange<-range(dfChrMS1$rt)
    intDefRange<-range(dfChrMS1$intensity)
    rtRange <- if (is.null(rtrange))  rtDefRange else clean_rtrange(rtDefRange)
    titMS1<-mk_title()
    plMS1<-ggplot2::ggplot(data=dfChrMS1,ggplot2::aes(x=rt,y=intensity,group=legend))+ggplot2::geom_line(ggplot2::aes(colour=legend))+ggplot2::lims(x=rtRange)+ggplot2::labs(x=CHR_GRAM_X,y=CHR_GRAM_Y,title=titMS1,tag=ii,colour="Retention time at max. intensity (MS1)")+ggplot2::scale_y_continuous(labels = sci10)+theme()
    dfsChrMS2<-lapply(tags,function(tag) {
        d<-data[[tag]]$ms2[[i]]
        if (!is.null(d)) {
            df<-MSnbase::fData(d)[,c("rtm","maxI")]
            colnames(df)<-c("rt","intensity")
            df$tag<-as.character(tag)
            df$legend=mk_leg_lab(tag,rtMS2[[tag]])
            df
        } else data.frame(rt=numeric(0),intensity=numeric(0),tag=tag)
    })
    dfChrMS2<-do.call(rbind,c(dfsChrMS2,list(make.row.names=F)))
    plMS2<-ggplot2::ggplot(data=dfChrMS2,ggplot2::aes(x=rt,ymin=0,ymax=intensity,group=legend))+ggplot2::geom_linerange(ggplot2::aes(colour=legend))+ggplot2::labs(x=NULL,y="maximum intensity",title=NULL,subtitle = "MS2",tag = "   ")+ggplot2::lims(x=rtRange)+ggplot2::labs(colour="Retention time at max. intensity (MS2)")+ggplot2::scale_y_continuous(labels = sci10)+theme()


    ## Structure
    g<-smiles2img(smile,width=500,height=500,zoom=4.5)
    cowplot::plot_grid(plMS1,g,plMS2,NULL,align = "v",ncol = 2,nrow=2,rel_widths=c(2,1))
    
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

## genSuprFileTblOld <- function(fileTbl,compTab) {
##     genOneFileTbl <- function(id,fileTbl) {
##         n <- nrow(fileTbl)
##         K <- length(id)
##         longid <- rep(id,n)
##         cols <- lapply(names(fileTbl),function(cn) rep("",n*K))
##         names(cols) <- names(fileTbl)
##         bdf <- as.data.frame(cols,stringsAsFactors = F)
##         rows <- lapply(1:n*K,function(x) NA)
##         for (j in 1:n) {
##             for (i in 1:K)
##                 rows[[(j-1)*K+i]] <- fileTbl[j,]
##         }
##         bdf <- as.data.frame(do.call(rbind,rows),stringsAsFactors = F)
##         bdf <- cbind(bdf,data.frame(ID=longid))
##         bdf
##     }
##     sets <- levels(factor(compTab$set))
##     setTbl <- lapply(sets,function (s) {
##         sl1<-compTab$set %in% s
##         sl2<-fileTbl$set==s
##         if (!any(sl2)) stop("Set",s,"does not select anything in the currently processed files.")
##         genOneFileTbl(compTab[sl1,]$ID,fileTbl[sl2,])

##     })
##     allTbl <- do.call(rbind,setTbl)
##     allTbl 
## }

genSuprFileTab <- function(fileTab,compTab) {
    genOne<-function(ids,fn) {

        K<-length(ids)
        fTabRow<-fileTab[fileTab$Files == fn,]
        cols<-lapply(names(fileTab),function(n) rep(fTabRow[[n]],K))
        names(cols)<-NULL
        cols<-c(cols,list(ids))
        names(cols)<-c(names(fileTab),"ID")
        df<-as.data.frame(cols,stringsAsFactors = F)
        df
    }
    
    tabs<-lapply(fileTab$Files,function(fn)
    {
        wh<-which(fileTab$Files==fn)
        set<-fileTab$set[[wh]]
        md<-fileTab$mode[[wh]]
        sel<-(compTab$set %in% set) & (compTab$mode %in% md)
        ids<-compTab$ID[sel]
        genOne(ids,fn)
        
    })
    res<-do.call(rbind,tabs)
    res
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
    names(res)<-entry
    res
        
}
addCompColsToFileTbl<-function(ft,compTab) {
    nR<-nrow(ft)
    mzCol<-rep(NA,nR)
    nmCol<-rep("",nR)
    rtCol<-rep(NA,nR)
    
    for (ir in 1:nR) {
        id<-ft[ir,"ID"]
        set<-ft[ir,"set"]
        m<-ft[ir,"mode"]
        entries<-getEntryFromComp(c("mz","Name","rt"),id,set,m,compTab)
        mzCol[[ir]]<-  entries[["mz"]]
        nm<-entries[["Name"]]
        nmCol[[ir]]<- if (!is.na(nm)) nm else ""
        rtCol[[ir]]<- entries[["rt"]]
    }
    ft$mz<-mzCol
    ft$Name<-nmCol
    ft$rt<-rtCol
    ft
}


vald_comp_tab<-function(df,ndf,checkSMILES=F,checkMz=F,checkNames=F) {
    ## Fields.
    if (is.null(df$ID)) stop("Column ID missing in ",ndf," .")
    if (checkMz && is.null(df$mz)) stop("Column mz missing in ", ndf, " .")
    if (checkSMILES && is.null(df$SMILES)) stop("Column SMILES missing in", ndf, " .")
    
    if (checkNames && is.null(df$Name)) warning("Column Name missing in ", ndf," , continuing without.")
    if (is.null(df$RT) && is.null(df$rt)) {
        warning("Column RT (alternatively, rt) missing in ", ndf, ", continuing without.")
    } else {
        if (is.null(df$rt)) {
            df$rt<-df$RT
            df$RT<-NULL
        }
    }

    ## Missing IDs?
    ind<-which(is.na(df$ID))
    if (length(ind)>0) {
        for (i in ind) {
            warning("ID missing at row: ",i," .")
        }
        stop("Missing IDs found.")
    }
    
    ## Unique IDs?
    luids<-length(unique(df$ID))
    if (length(df$ID) > luids) stop("Duplicate IDs in ", ndf, " are not allowed.")

    ## Missing SMILES?
    if (checkSMILES) {
        ind<-which(is.na(df$SMILES))
        if (length(ind)>0) {
            for (i in ind) {
                warning("SMILES missing at row: ",i, "; ID: ",df$ID[[i]]," .")
            }
            stop("Missing SMILES found.")
        }
    }

    ## Missing mz?
    if (checkMz) {
        ind<-which(is.na(df$mz))
        if (length(ind)>0) {
            for (i in ind) {
                warning("mz missing at row: ",i, "; ID: ",df$ID[[i]]," .")
            }
            stop("Missing mz-s found.")
        }
    }

    df
}
