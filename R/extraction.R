load_raw_data<-function(fn,mode="inMemory") {
    ms1<-MSnbase::readMSData(files=fn,mode=mode,msLevel=1)
    ms2<-MSnbase::readMSData(files=fn,mode=mode,msLevel=2)
    c(ms1=ms1,ms2=ms2)
}


acq_mz<-function(tabFn) {
    df<-read.csv(tabFn,
                 stringsAsFactors=F,
                 comment.char='')
    x<-unique(as.numeric(df$mz))
    inds<-match(x,as.numeric(df$mz))
    id<-df$ID[inds]
    names(x)<-id
    x
}


ppm2dev<-function(m,ppm) 1e-6*ppm*m



gen_mz_range<-function(mz,limit) {
    mat<-matrix(data=numeric(1),nrow=length(mz),ncol=2,dimnames=list(names(mz)))
    mat[,1]<-mz-limit
    mat[,2]<-mz+limit
    mat
}

filt_ms2_by_prcs<-function(pre,pre_mz_rng) {
    apply(pre_mz_rng,1,function(rng) {
        m1<-rng[[1]]
        m2<-rng[[2]]
        ind <- which((pre > rng[[1]]) & (pre < rng[[2]]))
        ind
    })
}

grab_ms2_spec<-function(idx,raw) {
    lapply(idx,function (id) {
        if (length(id)>0) {
            spec<-raw[id]
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
        } else list()
    })
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


gen_ms1_chrom<-function(raw,mzRng) {
    ids<-dimnames(mzRng)[[1]]
    x<-chromatogram(raw,mz=mzRng,msLevel=1,missing=0.0)

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

write_eic<-function(eic,suff="eic.csv",dir=".") {
    width=get_ext_width(max(as.numeric(names(eic))))
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

extr_msnb <-function(file,wd,mz,limEIC,limFinePPM,mode="inMemory") {
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
    mzRng<-gen_mz_range(mz,limit=LIM_EIC)
    eicMS1<-gen_ms1_chrom(ms1,mzRng)
    write_eic(eicMS1,dir=wd)
    message("Extracting precursor EICs finished.")

    ## Extract MS2 spectra.
    message("Extracting MS2 spectra.")
    ppmMzRange<-gen_mz_range(mz,limit=ppm2dev(mz,LIM_PPM_FINE))
    pre<-MSnbase::precursorMz(ms2)
    idxMS2<-filt_ms2_by_prcs(pre,ppmMzRange)
    ms2Spec<-grab_ms2_spec(idxMS2,ms2)
    eicMS2<-gen_ms2_chrom(ms2Spec)
    message("Extracting MS2 spectra finished.")
    write_eic(eicMS2,suff="kids.csv")
    specDir<-file.path(wd,"ms2_spectra")
    dir.create(specDir,showWarnings = F)
    write_ms2_spec(ms2Spec,dir=specDir)
    message("Done with ", file)

}

extr_rmb <- function (file,wd, mz, limEIC, limFinePPM) {
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
        msms <- RMassBank::findMsMsHR.mass(f, mz, 0.5, theppm)
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
