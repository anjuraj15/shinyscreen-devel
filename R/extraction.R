load_raw_data<-function(fn,mode="inMemory") {
    ms1<-MSnbase::readMSData(files=fn,mode=mode,msLevel=1)
    ms2<-MSnbase::readMSData(files=fn,mode=mode,msLevel=2)
    c(ms1=ms1,ms2=ms2)
}


acq_mz<-function(tabFn) {
    df<-read.csv(tabFn,
                 stringsAsFactors=F,
                 comment.char='')
    x<-as.numeric(df$mz)
    names(x)<-as.character(df$ID)
    x
}


ppm2dev<-function(m,ppm) 1e-6*ppm*m



gen_mz_range<-function(mz,limit) {
    mat<-matrix(data=numeric(1),nrow=length(mz),ncol=2,dimnames=list(as.character(names(mz))))
    mat[,1]<-mz-limit
    mat[,2]<-mz+limit
    mat
}

filt_ms2_by_prcs_old<-function(pre,preMZRng) {
    res<-apply(preMZRng,1,function(rng) {
        m1<-rng[[1]]
        m2<-rng[[2]]
        ind <- which((pre > rng[[1]]) & (pre < rng[[2]]))
        ind
    })

    names(res)<-dimnames(preMZRng)[[1]]
    res
}

filt_ms2_by_prcs <- function(ms2,mz,limCoarse) {
    ppmMzRange<-gen_mz_range(mz,limit=limCoarse)
    pre<-MSnbase::precursorMz(ms2)
    nR<-length(pre)
    df<-data.frame(sn=integer(nR),
                   prec_scan=integer(nR),
                   ID=character(nR),
                   OK=logical(nR),
                   stringsAsFactors=F)
    df$OK<-T #TODO Introduced for testing, be careful.

    offD<-0
    nms<-as.character(dimnames(ppmMzRange)[[1]])
    for (rM in 1:nrow(ppmMzRange)) {
        m1<-ppmMzRange[rM,1]
        m2<-ppmMzRange[rM,2]
        ind <- which((pre > m1) & (pre < m2))
        nm<-nms[[rM]]
        rng<-seq.int(offD+1,offD+length(ind),length.out=length(ind))
        df[rng,"sn"]<-ind
        df[rng,"ID"]<-nm
        offD<- offD + length(ind)
    }
    res <- df[1:offD,]
    for (i in 1:nrow(res)) {
        sn<-res$sn[[i]]
        res$prec_scan[[i]]<-MSnbase::precScanNum(ms2[[sn]])
    }
    res

}

add_ms2_prcs_scans<-function(ms2,idx) {

    df<-idx
    df$prec_scan<-integer(nrow(idx))
    for (i in 1:nrow(df)) {
        sn<-df$sn[[i]]
        df$prec_scan[[i]]<-MSnbase::precScanNum(ms2[[sn]])
    }
    ## sn<-as.integer(df$sn)
    ## df$prec_scan[]<-MSnbase::precScanNum(ms2[sn])
    ## 
    ## This errors with: msLevel for signature "NULL" for a specific
    ## compound. However, the above approach is cool.
    df
}

pick_unique_precScans<-function(idx) {
    ps<-unique(idx$prec_scan)
    mind<-match(ps,idx$prec_scan)
    ids<-idx$ID[mind]
    data.frame(prec_scan=idx$prec_scan[mind],ID=ids,stringsAsFactors=F)
    
}


verif_prec_fine<-function(preSc,ms1,mz,limFinePPM) {
    mzRng<-gen_mz_range(mz,limit=ppm2dev(mz,limFinePPM))
    for (n in 1:nrow(mzRng)) {
        message("dm: ",1e6*(mzRng[n,2]-mzRng[n,1])/mz[n]/2.,"; mz:",mz[[n]],";ID= ",names(mz)[[n]])
    }
    df<-preSc
    df$mz<-mz[as.character(df$ID)]
    mz1<-mzRng[as.character(df$ID),1]
    mz2<-mzRng[as.character(df$ID),2]

    df$OK<-logical(nrow(df))
    df$preMz<-numeric(nrow(df))
    acqN<-MSnbase::acquisitionNum(ms1)
    wh<-which(acqN %in% df$prec_scan)
    specMz<-MSnbase::mz(ms1[wh])


    for (i in 1:nrow(df)) {
        spec<-specMz[[i]]
        wh<-(spec > mz1[[i]] ) & ( spec < mz2[[i]])
        df$OK[[i]]<- any(wh)
        ind<-which(wh)
        df$preMz[[i]]<- if (length(ind)>0) spec[ind[[1]]] else 0
    }
    tab2file(tab=df,file="prec.fine.csv")
    df
}

refn_ms2_by_prec<-function(idxMS2,preFine) {
    pf<-preFine[preFine$OK,]
    pf$ID<-as.character(pf$ID)
    idxMS2$OK<-logical(nrow(idxMS2))
    idxMS2$ID<-as.character(idxMS2$ID)
    for (n in 1:nrow(idxMS2)) {
        scan<-idxMS2$prec_scan[[n]]
        id2<-idxMS2$ID[[n]]
        ppf<-pf[pf$ID==id2,]
        inPF<- ppf$prec_scan %in% scan
        idxMS2$OK[[n]]<-any(inPF)
    }

    idxMS2
}


grab_ms2_spec<-function(idx,raw) {
    idx<-idx[idx$OK,]
    IDs<-unique(idx$ID)
    res<-lapply(IDs,function (id) {
        sn<-idx$sn[idx$ID==id]
        spec<-raw[sn]
        rts<-MSnbase::rtime(spec)
        lmz<-MSnbase::mz(spec)
        lI<-MSnbase::intensity(spec)
        rts<-rts/60.
        names(lmz)<-NULL
        names(lI)<-NULL
        Map(function (mz,I,rt) {
            mat<-matrix(data=0.0,ncol=length(mz),nrow=2,dimnames=list(c("mz","intensity")))
            mat["mz",]<-as.numeric(mz)
            mat["intensity",]<-as.numeric(I)
            list(rt=rt,spec=mat)
        },lmz,lI,rts)
    })
    names(res)<-IDs
    res
}
               

gen_ms2_chrom<-function(ms2Spec) {
    lapply(ms2Spec, function(sp)
    {
        if (length(sp)>0) {
            nRow<-length(sp)
            mat<-matrix(0.0,nrow=nRow,ncol=2)
            rt<-sapply(sp,function(x) x$rt)
            ord<-order(rt)
            intn<-lapply(sp,function (x) max(x$spec))
            rt<-as.numeric(rt[ord])
            intn<-as.numeric(intn[ord])
            names(intn)<-NULL
            names(rt)<-NULL
            mat[,1]<-rt
            mat[,2]<-intn
            colnames(mat)<-c("rt","intensity")
            mat
        } else list()

        
    })
    
}


gen_ms1_chrom<-function(raw,mz,limEIC) {
    mzRng<-gen_mz_range(mz,limit=limEIC)
    ids<-dimnames(mzRng)[[1]]
    x<-MSnbase::chromatogram(raw,mz=mzRng,msLevel=1,missing=0.0)

    res<-lapply(x,function (xx) {
        rt<-MSnbase::rtime(xx)/60.
        ints<-MSnbase::intensity(xx)
        df<-data.frame(rt=rt,intensity=ints,stringsAsFactors=F)
        df
    })
    names(res)<-ids
    res
    
}

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

get_ext_width <- function(maxid) {as.integer(log10(maxid)+1)}
id_fn_ext<-function(width,id) {
    formatC(as.numeric(id),width=width,flag=0)
}

write_eic<-function(eic,suff="eic.csv",dir=".",width=get_ext_width(max(as.numeric(names(eic))))) {
    Map(function (e,n) {
        if (length(e)>0) {
            fn<-file.path(dir,paste(id_fn_ext(width,n),suff,sep="."))
            tab2file(tab=e,file=fn)
        }
    },eic,names(eic))
    
}


write_ms2_spec<-function(ms2Spec,dir=".") {
    ids<-as.numeric(names(ms2Spec))
    maxid<-max(ids)
    width<-get_ext_width(maxid)
    
    for (id in names(ms2Spec)) {
        sp<-ms2Spec[[id]]
        if (length(sp)>0) {
            dr<-file.path(dir,id_fn_ext(width,id))
            dir.create(path=dr,showWarnings=F)
            for (s in sp) {
                fn<-file.path(dr,paste("RT_",s$rt,"_spectrum.csv",sep=""))
                df<-t(s$spec)
                colnames(df)<-c("mz","intensity")
                tab2file(tab=df,file=fn)
            }
            
        }
    }
}

extr_msnb <-function(file,wd,mz,limEIC,limCoarse=0.5, limFinePPM,mode="inMemory") {
    ## Perform the entire data extraction procedure.
    ## 
    ## file - The input mzML file.
    ## wd - Top-level directory where the results should be deposited.
    ## mz - A named vector of precursor masses for which to scan the
    ## file. The names can be RMassBank IDs.
    ## limEIC - Absolute mz tolerance used to extract precursor EICs.
    ## limFinePPM - Tolerance given in PPM used to associate input
    ## masses with what the instrument assigned as precursors to MS2
    ## products.

    message("Loading ",file," in mode ",mode, ".")
    data<-load_raw_data(file,mode=mode)
    ms1<-data[["ms1"]]
    ms2<-data[["ms2"]]
    message("Done loading ",file,".")

    ## EICs for precursors.
    message("Extracting precursor EICs. Please wait.")
    eicMS1<-gen_ms1_chrom(raw=ms1,mz=mz,limEIC=limEIC)
    write_eic(eicMS1,dir=wd)
    message("Extracting precursor EICs finished.")

    ## Extract MS2 spectra.
    message("Extracting MS2 spectra.")
    idxMS2<-filt_ms2_by_prcs(ms2=ms2,mz=mz,limCoarse=limCoarse)
    message("Resampling MS2 spectra.")
    # idxMS2<-add_ms2_prcs_scans(ms2,idxMS2)
    prsc<-pick_unique_precScans(idxMS2)
    vprsc<-verif_prec_fine(preSc=prsc,ms1=ms1,mz=mz,limFinePPM = limFinePPM)
    idxMS2<-refn_ms2_by_prec(idxMS2=idxMS2,preFine=vprsc)
    message("Resampling MS2 spectra finished.")
    

    ms2Spec<-grab_ms2_spec(idxMS2,ms2)
    eicMS2<-gen_ms2_chrom(ms2Spec)
    message("Extracting MS2 spectra finished.")
    write_eic(eicMS2,dir=wd,suff="kids.csv",width=get_ext_width(max(as.numeric(names(eicMS1)))))
    specDir<-file.path(wd,"ms2_spectra")
    dir.create(specDir,showWarnings = F)
    write_ms2_spec(ms2Spec,dir=specDir)
    message("Done with ", file)

}

extr_rmb <- function (file,wd, mz, limEIC, limCoarse=0.5, limFinePPM) {
    ID<-as.numeric(names(mz))
    maxid <- max(ID)
    id_field_width <- as.integer(log10(maxid)+1)
    fn_out<- function(id,suff) {file.path(wd,paste(formatC(id,width=id_field_width,flag=0),suff,".csv",sep=''))}
    fnProg<-file.path(wd,"progress.log")
    unlink(fnProg,force=T)
    cat("i","total\n",sep=",",file=fnProg)
    f <- mzR::openMSfile(file)
    total<-length(ID)
    n_spec <- 0
    cmpd_RT_maxI <- rep(0.0,total)
    msms_found <- rep(F,total)
    rts <- rep(0.0,total)
    max_I_prec <- rep(0.0,total)
    cmpd_RT_maxI_min <- rep(0.0,total)
    for (i in 1:total) {
        cpdID <- ID[[i]]
        n_spec <- i
        mz<-mz[[i]]
        eic <- RMassBank::findEIC(f, mz, limit = limEIC)
        msms_found[i] <- FALSE
        theppm<-RMassBank::ppm(mz, limFinePPM,p = TRUE)
        msms <- RMassBank::findMsMsHR.mass(f, mz, limCoarse, theppm)
        max_I_prec_index <- which.max(eic$intensity)
        cmpd_RT_maxI[i] <- eic[max_I_prec_index, 1]
        max_I_prec[i] <- eic[max_I_prec_index, 2]
        cmpd_RT_maxI_min[i] <- as.numeric(cmpd_RT_maxI[i])/60 ## conversion to minutes
        if (length(eic$rt)>0) eic$rt <- eic$rt/60 ## conversion to minutes
        tab2file(tab=eic[c("rt","intensity")],file=fn_out(cpdID,".eic"))
        bindKids <- function(kids)
            do.call(rbind,lapply(kids,function (kid)
                c(rt=kid@rt,intensity=max(kid@intensity))))

        
        bindSpec <- function(specLst) {
            do.call(rbind,lapply(specLst,function (sp) bindKids(sp@children)))
        }
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
        rts[i] <- cmpd_RT_maxI[i]
        cat(i,total,"\n",file=fnProg,append=T,sep=",")
    }
    mzR::close(f)
    rtwiDf <- data.frame(ID=file_list$ID, mz=file_list$mz, Name=file_list$Name, 
                         cmpd_RT_maxI=cmpd_RT_maxI, cmpd_RT_maxI_min=cmpd_RT_maxI_min,
                         max_I_prec=max_I_prec, msms_found=msms_found,stringsAsFactors=F)
    write.csv(rtwiDf, file = file.path(wd,"RTs_wI.csv"), row.names = F)
}

##' Extracts data from mzML files.
##'
##' @title Data Extraction from mzML Files
##' @param fTab File table with Files,ID,wd,Name and mz
##'     columns. Column Files, as well as wd must have all rows
##'     identical.
##' @param extr_fun Extraction function from the backend.
##' @param limEIC Absolute mz tolerance used to extract precursor EICs.
##' @param limFinePPM Tolerance given in PPM used to associate input
##'     masses with what the instrument assigned as precutsors to MS2.
##' @return Nothing useful.
##' @author Todor Kondić
extract<-function(fTab,extr_fun,limEIC,limFinePPM) {
    fnData<-fTab$Files[[1]]
    wd<-fTab$wd[[1]]
    ID<-fTab$ID
    mz<-fTab$mz
    names(mz)<-ID
    dir.create(wd,showWarnings=F)
    extr_fun(file=fnData,
             wd=wd,
             mz=mz,
             limEIC=limEIC,
             limFinePPM=limFinePPM)
    
}
