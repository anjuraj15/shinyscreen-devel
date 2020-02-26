## Copyright (C) 2020 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

load_raw_data<-function(fn,mode="inMemory") {
    ms1<-MSnbase::readMSData(files=fn,mode=mode,msLevel.=1)
    ms2<-MSnbase::readMSData(files=fn,mode=mode,msLevel.=2)
    c(ms1=ms1,ms2=ms2)
}


acq_mz<-function(tabFn) {
    df<-read.csv(tabFn,
                 stringsAsFactors=F,
                 comment.char='')
    x<-as.numeric(df$mz)
    names(x)<-paste("ID:",as.character(df$ID),sep='')
    x
}

name2id<-function(nm) {as.integer(substring(nm,4))}
id2name<-function(id) {paste("ID:",id,sep='')}

ppm2dev<-function(m,ppm) 1e-6*ppm*m



gen_mz_range<-function(mz,delta) {
    dhalf=delta/2
    mat<-matrix(data=numeric(1),nrow=length(mz),ncol=2,dimnames=list(as.character(names(mz))))
    mat[,1]<-mz - dhalf
    mat[,2]<-mz + dhalf
    mat
}

gen_rt_range<-function(rt,delta) {
    mat<-matrix(data=numeric(1),nrow=length(rt),ncol=2,dimnames=list(as.character(names(rt))))
    rV<-which(!is.na(rt))
    rNA<-which(is.na(rt))
    hdelta=delta/2
    mat[rV,1]<-(rt[rV] - hdelta)*60
    mat[rV,2]<-(rt[rV] + hdelta)*60
    mat[rNA,1]<--Inf
    mat[rNA,2]<-Inf
    mat
}

filt_ms2_by_prcs <- function(ms2,mz,deltaCoarse) {

    mzRng<-gen_mz_range(mz,delta=deltaCoarse)
    ids<-rownames(mzRng)
    pre<-MSnbase::precursorMz(ms2)
    psn<-MSnbase::precScanNum(ms2)
    acN<-MSnbase::acquisitionNum(ms2)
    nR<-length(pre)

    inRange<-function(i) {
        mp<-pre[[i]]
        x<-mzRng[,1]<mp & mp<mzRng[,2]
        mRows<-which(x)
        sids<-ids[mRows]
        sids
    }
    lst<-lapply(1:nR,function(i) list(n=i,prec_scan=psn[[i]],aN=acN[[i]],ids=inRange(i)))
    nemp<-sapply(lst,function(m) length(m$ids)>0)
    wrk<-lst[nemp]
    dfL<-sum(sapply(wrk,function(w) length(w$ids)))
    df<-data.frame(ID=character(dfL),
                   prec_scan=integer(dfL),
                   aN=integer(dfL),
                   OK=logical(dfL),
                   stringsAsFactors=F)
    df$OK<-T #TODO Introduced for testing, be careful.

    offD<-0
    for (m in wrk) {
        l<-length(m$ids)
        rng<-(offD+1):(offD+l)
        df[rng,"ID"]<-m$ids
        df[rng,"prec_scan"]=m$prec_scan
        df[rng,"aN"]<-m$aN
        offD<-offD+l
    }
    df[order(df$aN),]
}

filt_ms2_by_prcs_ht<-function(ms2,mz,deltaCoarse) {
    lgnd<-filt_ms2_by_prcs(ms2,mz,deltaCoarse)

    scans<-unique(lgnd$aN)
    ns<-which(MSnbase::acquisitionNum(ms2) %in% scans)
    sms2<-ms2[ns]
    list(ms2=sms2,leg=lgnd)
}


pick_unique_precScans<-function(idx) {
    ps<-unique(idx$prec_scan)
    mind<-match(ps,idx$prec_scan)
    ids<-idx$ID[mind]
    data.frame(prec_scan=idx$prec_scan[mind],ID=ids,stringsAsFactors=F)
    
}

pick_uniq_pscan<-function(leg) {
    ids<-unique(leg$ID)
    x<-lapply(ids,function(id) {ups<-unique(leg[id==leg$ID,"prec_scan"]);data.frame(ID=rep(id,length(ups)),prec_scan=ups,stringsAsFactors = F)})
    res<-do.call(rbind,c(x,list(stringsAsFactors=F)))
    res[order(res$prec_scan),]
}

verif_prec_fine_ht<-function(preLeg,ms1,mz,deltaFinePPM) {
    mzRng<-gen_mz_range(mz,delta=2*ppm2dev(mz,deltaFinePPM)) # Factor
                                                             # of two
                                                             # here is
                                                             # intentional
                                                             # and
                                                             # needed.
    df<-preLeg
    df$mz<-mz[df$ID]
    mz1<-mzRng[df$ID,1]
    mz2<-mzRng[df$ID,2]
    ipns<-match(df$prec_scan,MSnbase::acquisitionNum(ms1))
    rms1<-ms1[ipns]
    mzsp<-MSnbase::mz(rms1)
    df$OK<-mapply(function(m1,sp,m2) any((m1<sp) & (sp<m2)),mz1,mzsp,mz2)
    df[df$OK,]     
}

filt_ms2<-function(ms1,ms2,mz,deltaCoarse,deltaFinePPM) {
    tmp<-filt_ms2_by_prcs_ht(ms2,mz,deltaCoarse=deltaCoarse)
    legMS2<-tmp$leg
    legPcs<-pick_uniq_pscan(legMS2)
    legPcs<-verif_prec_fine_ht(legPcs,ms1=ms1,mz=mz,deltaFinePPM=deltaFinePPM)
    x<-Map(function (id,psn) {legMS2[id==legMS2$ID & psn==legMS2$prec_scan,]},legPcs[,"ID"],legPcs[,"prec_scan"])

    x<-do.call(rbind,c(x,list(make.row.names=F,stringsAsFactors=F)))[c("ID","aN")]
    rownames(x)<-NULL
    x<-x[order(x$aN),]
    uids<-unique(x$ID)
    acN<-MSnbase::acquisitionNum(ms2)
    res<-lapply(uids,function(id) {
        x<-ms2[match(x[id==x$ID,"aN"],acN)]
        fData(x)[,"rtm"]<-MSnbase::rtime(x)/60.
        fData(x)[,"maxI"]<-sapply(MSnbase::intensity(x),max)
        x})
    names(res)<-uids
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

trim_ms2_by_prec<-function(rawMS2,mz,deltaCoarse,deltaFinePPM) {
    idxMS2<-filt_ms2_by_prcs(ms2=ms2,mz=mz,deltaCoarse=deltaCoarse)
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


gen_ms1_chrom<-function(raw,mz,deltaEIC,rt=NULL,rtDelta=NULL) {
    mzRng<-gen_mz_range(mz,delta=deltaEIC)
    rtRng<-gen_rt_range(rt,delta=rtDelta)
    ids<-dimnames(mzRng)[[1]]
    x<-MSnbase::chromatogram(raw,mz=mzRng,msLevel=1,missing=0.0,rt=rtRng)

    res<-lapply(x,function (xx) {
        rt<-MSnbase::rtime(xx)/60.
        ints<-MSnbase::intensity(xx)
        df<-data.frame(rt=rt,intensity=ints,stringsAsFactors=F)
        df
    })
    names(res)<-ids
    res
    
}


gen_ms1_chrom_ht<-function(raw,mz,deltaEIC,rt=NULL,rtDelta=NULL) {
    mzRng<-gen_mz_range(mz,delta=deltaEIC)
    rtRng<-gen_rt_range(rt,delta=rtDelta)
    res<-MSnbase::chromatogram(raw,mz=mzRng,msLevel=1,missing=0.0,rt=rtRng)
    fData(res)[["ID"]]<-rownames(mzRng)
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

extr_msnb <-function(file,wd,mz,deltaEIC, deltaFinePPM,deltaCoarse=0.5,rt=NULL,rtDelta=NULL,mode="inMemory") {
    ## Perform the entire data extraction procedure.
    ## 
    ## file - The input mzML file.
    ## wd - Top-level directory where the results should be deposited.
    ## mz - A named vector of precursor masses for which to scan the
    ## file. The names can be RMassBank IDs.
    ## rt - A named vector of length 1, or same as mz, giving the retention
    ## times in minutes. The names should be the same as for mz.
    ## rtDelta - A vector of length 1, or same as mz, giving the
    ## half-width of the time window in which the peak for the
    ## corresponding mz is supposed to be.
    ## deltaEIC - Absolute mz tolerance used to extract precursor EICs.
    ## deltaFinePPM - Tolerance given in PPM used to associate input
    ## masses with what the instrument assigned as precursors to MS2
    ## products.

    message("Loading ",file," in mode ",mode, ".")
    data<-load_raw_data(file,mode=mode)
    ms1<-data[["ms1"]]
    ms2<-data[["ms2"]]
    message("Done loading ",file,".")

    ## EICs for precursors.
    message("Extracting precursor EICs. Please wait.")
    eicMS1<-gen_ms1_chrom(raw=ms1,mz=mz,deltaEIC=deltaEIC,rt=rt,rtDelta=rtDelta)
    write_eic(eicMS1,dir=wd)
    message("Extracting precursor EICs finished.")

    ## Extract MS2 spectra.
    message("Extracting MS2 spectra.")
    idxMS2<-filt_ms2_by_prcs(ms2=ms2,mz=mz,deltaCoarse=deltaCoarse)
    message("Resampling MS2 spectra.")
    # idxMS2<-add_ms2_prcs_scans(ms2,idxMS2)
    prsc<-pick_unique_precScans(idxMS2)
    vprsc<-verif_prec_fine(preSc=prsc,ms1=ms1,mz=mz,deltaFinePPM = deltaFinePPM)
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

extr_msnb_ht <-function(file,wd,mz,deltaEIC, deltaFinePPM,deltaCoarse,fnSpec,rt=NULL,rtDelta=NULL,mode="onDisk") {
    ## Perform the entire data extraction procedure.
    ## 
    ## file - The input mzML file.
    ## wd - Top-level directory where the results should be deposited.
    ## mz - A named vector of precursor masses for which to scan the
    ## file. The names can be RMassBank IDs.
    ## rt - A named vector of length 1, or same as mz, giving the retention
    ## times in minutes. The names should be the same as for mz.
    ## rtDelta - A vector of length 1, or same as mz, giving the
    ## half-width of the time window in which the peak for the
    ## corresponding mz is supposed to be.
    ## deltaEIC - Absolute mz tolerance used to extract precursor EICs.
    ## deltaFinePPM - Tolerance given in PPM used to associate input
    ## masses with what the instrument assigned as precursors to MS2
    ## products.

    message("Loading ",file," in mode ",mode, ".")
    data<-load_raw_data(file,mode=mode)
    ms1<-data[["ms1"]]
    ms2<-data[["ms2"]]
    message("Done loading ",file,".")


    ## Filtering
    mzCrs<-gen_mz_range(mz=mz,delta=deltaCoarse)
    mzMin<-min(mzCrs)
    mzMax<-max(mzCrs)
    ms1<-MSnbase::filterMz(ms1,c(mzMin,mzMax))
    fms2<-filt_ms2(ms1,ms2,mz,deltaCoarse=deltaCoarse,deltaFinePPM=deltaFinePPM)

    ## EICs for precursors.
    message("Extracting precursor EICs. Please wait.")
    eicMS1<-gen_ms1_chrom_ht(raw=ms1,mz=mz,deltaEIC=deltaEIC,rt=rt,rtDelta=rtDelta)
    message("Extracting precursor EICs finished.")
    

    x<-list(eic=eicMS1,ms2=fms2)
    saveRDS(object=x,file=file.path(wd,fnSpec))
    x
}

extr_rmb <- function (file,wd, mz, deltaEIC, deltaCoarse=0.5, deltaFinePPM,rt=NULL,rtDelta=NULL) {
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
        eic <- RMassBank::findEIC(f, mz, limit = deltaEIC)
        msms_found[i] <- FALSE
        theppm<-RMassBank::ppm(mz, deltaFinePPM,p = TRUE)
        msms <- RMassBank::findMsMsHR.mass(f, mz, deltaCoarse, theppm)
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
##' @param fTab File table with Files,ID,wd,Name,mz and RT
##'     columns. Column Files, as well as wd must have all rows
##'     identical.
##' @param extr_fun Extraction function from the backend.
##' @param deltaEIC Absolute mz tolerance used to extract precursor EICs.
##' @param deltaFinePPM Tolerance given in PPM used to associate input
##'     masses with what the instrument assigned as precursors to MS2.
##' @param deltaCoarse Absolute tolerance for preliminary association of
##'     precursors (from precursorMZ), to MS2 spectra.
##' @param rtDelta The half-width of the retention time window.
##' @param fnSpec Output file specification.
##' @return Nothing useful.
##' @author Todor Kondić
extract<-function(fTab,extr_fun,deltaEIC,deltaFinePPM,deltaCoarse,fnSpec,rtDelta) {
    fnData<-fTab$Files[[1]]
    wd<-fTab$wd[[1]]
    ID<-fTab$ID
    mz<-fTab$mz
    rt<-fTab$rt
    names(mz)<-id2name(ID)
    if (!is.null(rt)) names(rt)<-ID
    dir.create(wd,showWarnings=F)
    extr_fun(file=fnData,
             wd=wd,
             mz=mz,
             rt=rt,
             rtDelta=rtDelta,
             deltaEIC=deltaEIC,
             deltaFinePPM=deltaFinePPM,
             deltaCoarse=deltaCoarse,
             fnSpec=fnSpec)
    
}
