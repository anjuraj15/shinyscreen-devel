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

    rbind(resnort,resrt,fill=T)

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
                                  rt=retentionTime,
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
    cgram_ms1[res,on=.(precid,idx==prec_idx),
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
