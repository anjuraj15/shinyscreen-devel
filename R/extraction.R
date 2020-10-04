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


centroided1 <- function(ms) {
    if (all(MSnbase::centroided(ms)) == T)
        return(T) else {
                      state <- MSnbase::isCentroided(ms)
                      N <- length(state)
                      fls <-length(which(state == F))
                      if (fls/(1.*N) < 0.01) T else F
                  }
                                                         
}

centroided <- function(msvec) {
    if (is.vector(msvec)) {
        f <- list()
        for (i in 1:length(msvec)) {
            f[[i]] <- future::future(centroided1(msvec[[i]]))
        }
        lapply(f, FUN = future::value)
    } else {
        centroided1(msvec)
    }
    
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

gen_mz_range<-function(mz,err) {
    mat<-matrix(data=numeric(1),nrow=length(mz),ncol=2)
    mat[,1]<-mz - err
    mat[,2]<-mz + err
    mat
}

gen_rt_range<-function(rt,err) {
    mat<-matrix(data=numeric(1),nrow=length(rt),ncol=2)
    rV<-which(!is.na(rt))
    rNA<-which(is.na(rt))
    mat[rV,1]<-(rt[rV] - err)*60
    mat[rV,2]<-(rt[rV] + err)*60
    mat[rNA,1]<--Inf
    mat[rNA,2]<-Inf
    mat
}

filt_ms2_by_prcs <- function(ms2,mzrng,ids,adduct) {
    pre<-MSnbase::precursorMz(ms2)
    psn<-MSnbase::precScanNum(ms2)
    acN<-MSnbase::acquisitionNum(ms2)
    nR<-length(pre)

    inRange<-function(i) {
        mp<-pre[[i]]
        x<-mzrng[,1]<mp & mp<mzrng[,2]
        ind<-which(x)
        sids <- ids[ind]
        add <- adduct[ind]
        dtable(ID=sids,adduct=add)
    }
    lst<-lapply(1:nR,function(i) {
        dt <- inRange(i)
        list(n=i,prec_scan=psn[[i]],aN=acN[[i]],ids=dt$ID,adduct=dt$adduct)
    })
    nemp<-sapply(lst,function(m) length(m$ids)>0)
    wrk<-lst[nemp]
    dfL<-sum(sapply(wrk,function(w) length(w$ids)))
    df<-dtable(ID=character(dfL),
               adduct=character(dfL),
               prec_scan=integer(dfL),
               aN=integer(dfL),
               OK=logical(dfL))
    df$OK<-T #TODO Introduced for testing, be careful.

    offD<-0
    for (m in wrk) {
        l<-length(m$ids)
        rng<-(offD+1):(offD+l)
        df[rng,"ID"] <- m$ids
        df[rng,"prec_scan"] <- m$prec_scan
        df[rng,"aN"] <- m$aN
        df[rng,"adduct"] <- m$adduct
        offD<-offD+l
    }
    df[order(df$aN),]
}

filt_ms2_by_prcs_ht<-function(ms2,mzrng,ids,adduct) {
    lgnd<-filt_ms2_by_prcs(ms2,mzrng=mzrng,ids=ids,adduct=adduct)
    scans<-unique(lgnd$aN)
    ns<-which(MSnbase::acquisitionNum(ms2) %in% scans)
    sms2<-ms2[ns]
    list(ms2=sms2,leg=lgnd)
}


pick_unique_precScans<-function(idx) {
    ps<-unique(idx$prec_scan)
    mind<-match(ps,idx$prec_scan)
    data.frame(prec_scan=idx$prec_scan[mind],
               ID=idx$ID[mind],
               adduct=idx$adduct[mind],
               stringsAsFactors=F)
    
}

pick_uniq_pscan<-function(leg) {
    res <- leg[,.(prec_scan=unique(prec_scan)),by=c("ID","adduct")]
    res[order(prec_scan),]
    ## ids<-unique(leg$ID)
    ## x<-lapply(ids,function(id) {ups<-unique(leg[id==leg$ID,"prec_scan"]);data.frame(ID=rep(id,length(ups)),prec_scan=ups,stringsAsFactors = F)})
    ## res<-do.call(rbind,c(x,list(stringsAsFactors=F)))
    ## res[order(res$prec_scan),]
}

verif_prec_fine_ht<-function(preLeg,ms1,mz,mzrng,ids,adduct) {
    ## TODO FIXME TESTPHASE Something goes wrong here, all mapply results are
    ## not OK. More testing needed.
    df<-preLeg
    xx <- dtable(adduct=adduct,ID=ids,mz=mz,mz1=mzrng[,1],mz2=mzrng[,2])
    df <- preLeg[xx,on=c("ID","adduct")]
    df$ipns<-match(df$prec_scan,MSnbase::acquisitionNum(ms1))
    df[, ("mzsp") := .(lapply(ipns,function (ip) if (!is.na(ip)) MSnbase::mz(ms1[[ip]]) else NA_real_))]
    df$OK<-mapply(function(m1,sp,m2) any((m1<sp) & (sp<m2)),df$mz1,df$mzsp,df$mz2)
    res<-df[df$OK,]
    res$ipns<-NULL
    res$mz1<-NULL
    res$mz2<-NULL
    res$mzsp<-NULL
    res
}

filt_ms2<-function(ms1,ms2,mz,errCoarse,errFinePPM) {
    tmp<-filt_ms2_by_prcs_ht(ms2,mz,errCoarse=errCoarse)
    legMS2<-tmp$leg
    legPcs<-pick_uniq_pscan(legMS2)
    legPcs<-verif_prec_fine_ht(legPcs,ms1=ms1,mz=mz,errFinePPM=errFinePPM)
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
filt_ms2_fine <- function(ms1,ms2,mz,ids,adduct,err_coarse_fun,err_fine_fun) {
    ## This function is supposed to extract only those MS2 spectra for
    ## which it is proven that the precursor exists within the fine
    ## error range.
    mzrng_c <- gen_mz_range(mz,err_coarse_fun(mz))
    mzrng_f <- gen_mz_range(mz,err_fine_fun(mz))
    
    tmp<-filt_ms2_by_prcs_ht(ms2,mzrng=mzrng_c,ids=ids,adduct=adduct)
    legMS2<-tmp$leg
    legPcs<-pick_uniq_pscan(legMS2)
    legPcs<-verif_prec_fine_ht(legPcs,ms1=ms1,mz=mz,mzrng=mzrng_f,ids=ids,adduct=adduct)
    ## x<-Map(function (id,psn,a) {legMS2[id==legMS2$ID & a==legMS2$adduct & psn==legMS2$prec_scan,]},legPcs[,"ID"],legPcs[,"prec_scan"],legPcs[,"adduct"])
    ## x <- data.table::rbindlist(x)[,.(ID,adduct,aN)]
    x <- legMS2[legPcs[,.(ID,adduct,prec_scan)],on=c("ID","adduct","prec_scan")]
    ## x<-do.call(rbind,c(x,list(make.row.names=F,stringsAsFactors=F)))[c("ID","aN")]
    ## rownames(x)<-NULL
    x<-x[order(x$aN),]
    x
}
extr_ms2<-function(ms1,ms2,ids,mz,adduct,err_coarse_fun, err_fine_fun) {
    ## Extraction of MS2 EICs and spectra.
    x <- filt_ms2_fine(ms1=ms1,
                       ms2=ms2,
                       mz=mz,
                       ids=ids,
                       adduct=adduct,
                       err_coarse_fun=err_coarse_fun,
                       err_fine_fun=err_fine_fun)
    
    uids <- unique(x$ID)
    uadds <- unique(x$adduct)
    acN<-MSnbase::acquisitionNum(ms2)
    chunks <- Map(function(id,ad) {
        ans <- x[id==x$ID & ad==x$adduct,]$aN
        sp<-ms2[which(acN %in% ans)]
        r<-list()
        n <- length(sp)
        dtable(ID=rep(id,n),
               adduct=rep(ad,n),
               CE=MSnbase::collisionEnergy(sp),
               rt=MSnbase::rtime(sp)/60.,
               maspI=spectrapply(sp,function (s) max(MSnbase::intensity(s))),
               spec=MSnbase::spectrapply(sp,function (s) list(spec=dtable(mz=MSnbase::mz(s),
                                                                          intensity=MSnbase::intensity(s)),
                                                              rt = MSnbase::rtime(s)/60.,
                                                              CE = MSnbase::collisionEnergy(s))))}, uids,uadds)
    data.table::rbindlist(chunks)
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

trim_ms2_by_prec<-function(rawMS2,mz,errCoarse,errFinePPM) {
    idxMS2<-filt_ms2_by_prcs(ms2=ms2,mz=mz,errCoarse=errCoarse)
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


gen_ms1_chrom <- function(raw,mz,errEIC,id,rt=NULL,errRT=NULL) {
    mzRng<-gen_mz_range(mz,err = errEIC)
    rtRng<-gen_rt_range(rt,err = errRT)
    x<-MSnbase::chromatogram(raw,mz=mzRng,msLevel=1,missing=0.0,rt=rtRng)

    res<-lapply(x,function (xx) {
        rt<-MSnbase::rtime(xx)/60.
        ints<-MSnbase::intensity(xx)
        df<-dtable(rt=rt,intensity=ints)
        df
    })
    names(res)<-id
    res
    
}


gen_ms1_chrom_ht<-function(raw,mz,errEIC,rt=NULL,errRT=NULL) {
    mzRng<-gen_mz_range(mz,err=errEIC)
    rtRng<-gen_rt_range(rt,err=errRT)
    res<-MSnbase::chromatogram(raw,mz=mzRng,msLevel=1,missing=0.0,rt=rtRng)
    fData(res)[["ID"]]<-rownames(mzRng)
    res
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

extr_msnb <-function(file,wd,mz,errEIC, errFinePPM,errCoarse=0.5,rt=NULL,errRT=NULL,mode="inMemory") {
    ## Perform the entire data extraction procedure.
    ## 
    ## file - The input mzML file.
    ## wd - Top-level directory where the results should be deposited.
    ## mz - A named vector of precursor masses for which to scan the
    ## file. The names can be RMassBank IDs.
    ## rt - A named vector of length 1, or same as mz, giving the retention
    ## times in minutes. The names should be the same as for mz.
    ## errRT - A vector of length 1, or same as mz, giving the
    ## half-width of the time window in which the peak for the
    ## corresponding mz is supposed to be.
    ## errEIC - Absolute mz tolerance used to extract precursor EICs.
    ## errFinePPM - Tolerance given in PPM used to associate input
    ## masses with what the instrument assigned as precursors to MS2
    ## products.

    message("Loading ",file," in mode ",mode, ".")
    data<-load_raw_data(file,mode=mode)
    ms1<-data[["ms1"]]
    ms2<-data[["ms2"]]
    message("Done loading ",file,".")

    ## EICs for precursors.
    message("Extracting precursor EICs. Please wait.")
    eicMS1<-gen_ms1_chrom(raw=ms1,mz=mz,errEIC=errEIC,rt=rt,errRT=errRT)
    write_eic(eicMS1,dir=wd)
    message("Extracting precursor EICs finished.")

    ## Extract MS2 spectra.
    message("Extracting MS2 spectra.")
    idxMS2<-filt_ms2_by_prcs(ms2=ms2,mz=mz,errCoarse=errCoarse)
    message("Resampling MS2 spectra.")
                                        # idxMS2<-add_ms2_prcs_scans(ms2,idxMS2)
    prsc<-pick_unique_precScans(idxMS2)
    vprsc<-verif_prec_fine(preSc=prsc,ms1=ms1,mz=mz,errFinePPM = errFinePPM)
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

extr_msnb_ht <-function(file,wd,mz,errEIC, errFinePPM,errCoarse,fnSpec,rt=NULL,errRT=NULL,mode="onDisk") {
    ## Perform the entire data extraction procedure.
    ## 
    ## file - The input mzML file.
    ## wd - Top-level directory where the results should be deposited.
    ## mz - A named vector of precursor masses for which to scan the
    ## file. The names can be RMassBank IDs.
    ## rt - A named vector of length 1, or same as mz, giving the retention
    ## times in minutes. The names should be the same as for mz.
    ## errRT - A vector of length 1, or same as mz, giving the
    ## half-width of the time window in which the peak for the
    ## corresponding mz is supposed to be.
    ## errEIC - Absolute mz tolerance used to extract precursor EICs.
    ## errFinePPM - Tolerance given in PPM used to associate input
    ## masses with what the instrument assigned as precursors to MS2
    ## products.

    message("Loading ",file," in mode ",mode, ".")
    data<-load_raw_data(file,mode=mode)
    ms1<-data[["ms1"]]
    ms2<-data[["ms2"]]
    message("Done loading ",file,".")


    ## Filtering
    mzCrs<-gen_mz_range(mz=mz,err=errCoarse)
    mzMin<-min(mzCrs)
    mzMax<-max(mzCrs)
    ms1<-MSnbase::filterMz(ms1,c(mzMin,mzMax))
    fms2<-filt_ms2(ms1,ms2,mz,errCoarse=errCoarse,errFinePPM=errFinePPM)

    ## EICs for precursors.
    message("Extracting precursor EICs. Please wait.")
    eicMS1<-gen_ms1_chrom_ht(raw=ms1,mz=mz,errEIC=errEIC,rt=rt,errRT=errRT)
    message("Extracting precursor EICs finished.")
    

    x<-list(eic=eicMS1,ms2=fms2)
    saveRDS(object=x,file=file.path(wd,fnSpec))
    x
}

##' Extracts data from mzML files.
##'
##' @title Data Extraction from mzML Files
##' @param fTab File table with Files,ID,wd,Name,mz and RT
##'     columns. Column Files, as well as wd must have all rows
##'     identical.
##' @param extr_fun Extraction function from the backend.
##' @param errEIC Absolute mz tolerance used to extract precursor EICs.
##' @param errFinePPM Tolerance given in PPM used to associate input
##'     masses with what the instrument assigned as precursors to MS2.
##' @param errCoarse Absolute tolerance for preliminary association of
##'     precursors (from precursorMZ), to MS2 spectra.
##' @param errRT The half-width of the retention time window.
##' @param fnSpec Output file specification.
##' @return Nothing useful.
##' @author Todor Kondić
extract.old<-function(fTab,extr_fun,errEIC,errFinePPM,errCoarse,fnSpec,errRT) {
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
             errRT=errRT,
             errEIC=errEIC,
             errFinePPM=errFinePPM,
             errCoarse=errCoarse,
             fnSpec=fnSpec)
    
}


extr_eic_ms1 <- function(tab,err) {
    ## Asynchronous extraction of ms1 spectra. The result is a list of
    ## running futures.
    files <- unique(tab$Files)

    res <-lapply(files,function (fn) future::futur(extr_fn(fn), lazy=T))
    names(res) <- files
    res
}

##' @export
extract <- function(fn,tab,err_ms1_eic.,err_coarse,err_fine,err_rt.) {
    ## Extracts MS1 and MS2 EICs, as well as MS2 spectra, subject to
    ## tolerance specifications.


    ## TODO: Still detecting external references ... but which?
    err_coarse_fun <- gen_mz_err_f(err_coarse,
                                   "ms1 coarse error: Only ppm, or Da units allowed.")

    err_fine_fun <- gen_mz_err_f(err_fine,
                                 "ms1 fine error: Only ppm, or Da units allowed.")

    err_ms1_eic <- gen_mz_err_f(err_ms1_eic.,
                                "eic error: Only ppm, or Da units allowed.")

    err_rt <- gen_rt_err(err_rt.,
                         "rt error: Only s(econds), or min(utes) allowed.")
    
    tab <- data.table::as.data.table(tab)
    chunk <- tab[Files==fn]
    mz <- chunk$mz
    rt <- chunk$rt
    id <- chunk$ID
    adduct <- chunk$adduct
    names(mz) <- id
    names(rt) <- id
    mzerr <- err_coarse_fun(mz)
    mzrng <- gen_mz_range(mz=mz,err=mzerr)
    rtrng <- gen_rt_range(rt=rt,err=err_rt)
    mzmin <- min(mzrng)
    mzmax <- max(mzrng)
    read_ms1 <- function() {
        message("Opening ", fn, " to read MS1")
        ms1 <- MSnbase::readMSData(file=fn,msLevel=1,mode="onDisk")
        ms1 <- MSnbase::filterMz(ms1,c(mzmin,mzmax))
        message("Done opening ", fn, " to read MS1.")
        ms1
    }
    read_ms2 <- function() {
        message("Opening ", fn, " to read MS2")
        ms2 <- MSnbase::readMSData(file=fn,msLevel=2,mode="onDisk")
        message("Done opening ", fn, " to read MS2.")
        ms2
    }
    extr_ms1_eic <- function(ms1) {
        message("Extracting EICs from ", fn, " .")
        eic <- MSnbase::chromatogram(ms1,mz=mzrng,msLevel=1,missing=0.0,rt=rtrng)
        eiccol <- lapply(eic,function (e) dtable(rt=MSnbase::rtime(e)/60.,intensity=MSnbase::intensity(e)))
        ## names(res) <- id
        res <- dtable(ID=id,adduct=adduct,eicMS1=eiccol)
        message("Done extracting EICs from ", fn, " .")
        res
    }
    ms1 <- read_ms1()
    ms2 <- read_ms2()
    res_ms1 <- extr_ms1_eic(ms1)
    rms2full <- extr_ms2(ms1=ms1,
                         ms2=ms2,
                         ids=id,
                         mz=mz,
                         adduct=adduct,
                         err_coarse_fun=err_coarse_fun,
                         err_fine_fun=err_fine_fun)
    res_ms2 <- rms2full[,.(eicMS2=list(dtable(CE=.SD$CE,rt=.SD$rt,intensity=.SD$maspI)),
                           spec=list(spec)),by=c("adduct","ID")]
    res <- res_ms2[res_ms1,on=c("adduct","ID"),allow.cartesian=T]
    res[sapply(eicMS2,is.null),c("eicMS2","spec"):=.(NA,NA)]
    res$Files <- fn
    res
}
