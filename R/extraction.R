## Copyright (C) 2020,2021 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.

create_fine_table <- function(m) {
    ## Select fine mz-ranges and split them into those with rt entries
    ## and those without.
    precs = m$db$precursors
    precs[,unique(.SD),.SDcols=c("iso_fine_min",
                                 "iso_fine_max",
                                 "rt_min",
                                 "rt_max",
                                 "file"),
          keyby=c("file","precid")]

}

create_coarse_table <- function(m) {
    ## Select coarse mz-ranges and split them into those with rt entries
    ## and those without.
    precs = m$db$precursors
    precs[,unique(.SD),.SDcols=c("iso_coarse_min",
                                 "iso_coarse_max",
                                 "rt_min",
                                 "rt_max",
                                 "file"),
          keyby=c("file","isocoarse","precid")]

}

read_data_file <- function(file) {
    MSnbase::readMSData(file=file,msLevel=c(1,2),mode="onDisk")
}


extr_cgrams_ms1 <- function(ms,tab,fdata) {
    ## Some helpers.
    new_restab <- function(intab,cgm) {
        base = intab[,.(precid=precid,cgmidx=.I)]
        cgm = base[,{
            rt = rtime(cgm[cgmidx,1])
            inte = intensity(cgm[cgmidx,1])
            .(precid=precid,
              rt = rt,
              intensity = inte,
              scan = names(rt))},
            by="cgmidx"]
        setkey(cgm,scan)
        cgm[fdata$ms1,idx:=i.idx,on="scan"]
        cgm
    }
    
    trt = tab[!is.na(rt_min)]
    tnort = tab[is.na(rt_min)]

    resrt = if (nrow(trt)>0L) {
                ## Call with rt argument (in seconds).
                mzrng = as.matrix(trt[,.(iso_fine_min,iso_fine_max)])
                rtrng = as.matrix(trt[,.(rt_min*60,rt_max*60)])
                new_restab(trt,MSnbase::chromatogram(ms,mz = mzrng, rt = rtrng))
            } else data.table()

    resnort = if (nrow(tnort)>0L) {
                  mzrng = as.matrix(tnort[,.(iso_fine_min,iso_fine_max)])
                  new_restab(tnort,MSnbase::chromatogram(ms,mz = mzrng))
              } else data.table()

    res = rbind(resnort,resrt,fill=T)
    res[,rt:=rt/60]

}

get_fdata <- function(ms) {
    fdata = as.data.table(fData(ms),keep.rownames="scan")
    setkey(fdata,scan)
    res = list()
    res$ms1 = fdata[msLevel==1L,.(scan,
                                  idx=spIdx)]
    res$ms2 = fdata[msLevel==2L,.(scan,
                                  idx=spIdx,
                                  an=acquisitionNum,
                                  rt=retentionTime/60.,
                                  intensity=basePeakIntensity,
                                  ce=collisionEnergy,
                                  prec_mz=precursorMZ,
                                  prec_idx=precursorScanNum)]
    res


}


relate_ms2_to_precid <- function(coarse,ms2,cgram_ms1) {
    ## Take `coarse' table (the one with coarse mass limits), ms2
    ## fData and ms1 chromatogram, then relate precids from cgram_ms1
    ## to ms2 data.

    ## Select those MS2 entries the parents of which coarsely match
    ## compound lists masses.
    res = ms2[coarse,on=.(prec_mz>iso_coarse_min,prec_mz<iso_coarse_max),.(prec_mz=x.prec_mz,precid,prec_idx,scan,idx,ce,rt,intensity),nomatch=NULL]
    setkey(res,precid,prec_idx)

    ## Now, make sure those coarsely matched MS2 actually have a
    ## parent that finely matches something in the chromatogram (and
    ## this is by ensuring that a `precid' with the correct scan (idx)
    ## shows up in the chromatogram.
    x = cgram_ms1[!is.na(intensity)]
    x[res,on=.(precid,idx==prec_idx),
      .(precid,ce,scan=i.scan,
        idx=i.idx,rt=i.rt,
        intensity=i.intensity),nomatch=NULL]
    
}

extract_spectra <- function(ms,cgram_ms2) {
    ## This will extract full MS2 spectra based on ms2 chromatogram entries.
    indices = cgram_ms2[,.SD,.SDcol=c("precid","scan","idx")]

    res = empty_spectra_table()
    selind = indices[,unique(.SD),.SDcol=c("scan","idx")]
    sel = ms[selind$idx]

    masses = mz(sel)
    intensities = intensity(sel)
    res = selind
    setkey(res,scan)
    res = res[,data.table(mz=masses[[scan]],
                          intensity=intensities[[scan]]),
              keyby=c("scan")]
    res[indices,on=.(scan),precid:=i.precid]
    
}


## PRESCREENING


## This function extracts intensity maxima on intervals given by
## RT vectors rt_1 and rt_2.
find_ms1_max <- function(rt,intensity,rt_1,rt_2)
{
    x = mapply(function (rt_1,rt_2) {
        rt_ival <- c(rt_1,rt_2)
        intv <- findInterval(rt,rt_ival)
        lintv = length(intv)
        if (intv[1]==0L && intv[lintv] == 2L) {
            pos = match(c(1L,2L),intv)
        } else if (intv[1]==1L && intv[lintv]!=1L) {
            pos = c(1L,match(2L,intv))
        } else if (intv[1]==0L && intv[lintv]!=0L) {
            pos = c(match(1L,intv),lintv)
        } else {
            pos = c(1L,lintv)
        }
        pmax = pos[[1]] + which.max(intensity[pos[[1]]:pos[[2]]]) - 1L
        if (length(pmax)==0L) pmax = pos[[1]]
        c(rt[pmax],intensity[pmax])
    }, rt_1, rt_2, USE.NAMES=F)
    x
    
}

analyse_extracted_data <- function(db,prescreen_param) {
    ## Note
    ##
    ## I am working on this two days before the group meeting. The
    ## point of a meeting is to have something to show, so I will just
    ## minimally adapt the old `analyse_extracted_data' to the new
    ## `db' entries in the state. I suspect, even this is not going to
    ## be very easy.
    ##
    ## If no meeting was happening, then I'd create a nice, sleek
    ## function that fully adheres to the new `data model'
    ## philosophy. Alas, ... 
    
    ms1 = db$extr$cgm$ms1
    ms2 = db$extr$cgm$ms2
    spectra = db$extr$spectra
    precursors = db$precursors

    ## Get file info.
    ms2_noise_table = precursors[spectra,.(file,intensity),on="precid",by=.EACHI,nomatch=NULL]

    ## Calculate threshold.
    ms2_noise_table[,threshold:=0.33333333*mean(intensity),by="file"]

    ## Reduce table.
    ms2_noise_table = ms2_noise_table[,.(threshold=first(threshold,1L)),keyby="precid"]


    
    ## Parameters.
    presconf = conf_trans_pres(prescreen_param)
    rt_shift = presconf$ret_time_shift_tol
    det_ms2_noise = presconf$det_ms2_noise
    ms2_int_thresh = presconf$ms2_int_thresh
    ms1_int_thresh = presconf$ms1_int_thresh

    ## We start populating the ms2 qa table.
    tab_ms2 = copy(ms2)

    ## Calculate noise.
    tab_ms2[ms2_noise_table,qa_ms2_good_int:=intensity>threshold,on=.(precid)]

    ## Rename as downstream wants it.
    setnames(tab_ms2,c("rt","intensity"),c("ms2_rt","ms2_int"))
    tab_ms2[,`:=`(rt_left = ms2_rt - rt_shift,rt_right = ms2_rt + rt_shift)]
    
    ## Get mean ms1 value.
    tab_ms1 = copy(ms1)

    ## To (artificially) differentiate beween ms1 and ms2 (because,
    ## they get stapled together later on, set scan to NA_character_.
    tab_ms1[,scan:=NA_character_]
    
    tab_ms1_mean = tab_ms1[,.(ms1_mean=mean(intensity,na.rm=T)),keyby="precid"]
    

    ## Perform MS1 maxima calculation in the neighbourhood of each
    ## MS2 result.

    tmp = tab_ms1[tab_ms2,
    {
        xx = find_ms1_max(rt,intensity,i.rt_left,i.rt_right)
        .(scan=i.scan,
          ms1_rt = xx[1,],
          ms1_int = xx[2,])
    },on=.(precid),
    by=.EACHI,
    nomatch=NULL]
    ## Calculate QA values.
    tab_ms2[tmp,on=.(precid,scan),c("ms1_rt","ms1_int"):=.(i.ms1_rt,i.ms1_int)]
    tab_ms2[,c("rt_left","rt_right"):=c(NULL,NULL)]
    tab_ms2[tab_ms1_mean,ms1_mean:=i.ms1_mean]
    tab_ms2[,`:=`(qa_ms1_good_int=fifelse(ms1_int>ms1_int_thresh,T,F),
               qa_ms1_above_noise=F,
               qa_ms2_near=F)]


    ## TODO: I wonder if so stupidly auto-calculated ms1 noise should
    ## be taken into account at all? My recollection from earlier
    ## times was that it was helpful, at least sometimes.
    tab_ms2[qa_ms1_good_int==T,qa_ms1_above_noise:=fifelse(ms1_int>ms1_mean/3.,T,F)]
    tab_ms2[qa_ms1_good_int==T & qa_ms1_above_noise==T & qa_ms2_good_int==T,qa_ms2_near:=T]
    tab_ms2$qa_ms2_exists=T

    ## Reduce tab_ms1 to only to MS1 with no children.
    precs_ms1 = tab_ms1[,unique(precid)]
    precs_ms2 = tab_ms2[,unique(precid)]
    precs_noms =  data.table(precid=precs_ms1[!(precs_ms1 %in% precs_ms2)])
    tab_noms2 = tab_ms1[precs_noms,
                        on=.(precid),
                        .SD,nomatch=NULL]


    tab_noms2 = tab_noms2[,.(ms1_mean=mean(intensity,na.rm=T),
                             ms1_rt=rt[which.max(intensity)],
                             ms1_int=max(intensity, na.rm=T)),
                          keyby="precid"]
    ## QA for the above (lazy qa ... take only the max peak into account).
    tab_noms2[,c("qa_ms1_good_int","qa_ms1_above_noise"):=.(ms1_int>ms1_int_thresh,ms1_int>ms1_mean/3.)]

    ## MS2 QA criteria all fail.
    tab_noms2[,c("qa_ms2_exists","qa_ms2_good_int","qa_ms2_near"):=.(F,F,F)]

    ## Bind MS1-only and MS1/MS2 entries together.
    res = rbind(tab_ms2,tab_noms2,fill=T,use.names=T)

    ## If ms1_int has been calculated as a Na(N) value, this means
    ## that no MS1 has been found for that precid.
    res[,qa_ms1_exists:=F]
    res[!is.na(ms1_int),qa_ms1_exists:=T]
    data.table::setkey(res,precid)

    
    qflg = QA_FLAGS[!(QA_FLAGS %in% "qa_pass")]
    res[,qa_pass:=apply(.SD,1,all),.SDcols=qflg]
    res[.(T),del_rt:=abs(ms2_rt - ms1_rt),on="qa_pass",by='scan']
    resby = BASE_KEY_MS2[! (BASE_KEY_MS2 %in% 'scan')]
    res[.(T),qa_tmp_ms1_max:= ms1_int==max(ms1_int),on="qa_pass",by=resby]
    res[,ms2_sel:=F]
    res[.(T,T),ms2_sel:= del_rt == del_rt[which.min(del_rt)],on=c("qa_pass","qa_tmp_ms1_max"),by=resby]
    res[,qlt_ms1:=apply(.SD,1,function(rw) sum(c(5L,3L,2L)*rw)),.SDcol=c("qa_ms1_exists",
                                                                 "qa_ms1_above_noise",
                                                                 "qa_ms1_good_int")]
    res[,qlt_ms2:=apply(.SD,1,function(rw) sum(c(5L,3L,2L)*rw)),.SDcol=c("qa_ms2_exists",
                                                                 "qa_ms2_near",
                                                                 "qa_ms2_good_int")]
    res[is.na(qlt_ms1),qlt_ms1:=0L]
    res[is.na(qlt_ms2),qlt_ms2:=0L]

    ## Set all other flags to false when qa_ms1_exists == F by decree.
    flgs = c(QA_FLAGS,"ms2_sel")
    res[qa_ms1_exists == F,(flgs):=F]

    res
}


