#Copyright (C) 2023 by University of Luxembourg

## Shinyscreen works of an internal relational database implemented
## using `data.table' package. Implementation is here.


make_db_catalogue <- function(m) {
    ## Takes comprehensive database from state `m' and generates a
    ## catalogue with a unique key. This catalogue is based on
    ## inputs. Each entry in the catalogue corresponds to a single
    ## target mass from a single experimental run.
    res = m$out$tab$comp[,unique(.SD),.SDcols=c("set","tag","adduct","ID")]
    res[,catid:=.I]
    setkeyv(res,DB_CATALOGUE_KEY)
    setindex(res,catid)
    m$db$cat = res
    m
}


make_db_precursors <- function(m) {
    ## Generate masses and label isobars.

    cat = m$db$cat
    masses = m$out$tab$comp[cat,.(catid=catid,mz=mz,rt=rt),on=key(cat)]
    setkey(masses,mz)

    ## Retention time.
    tmp = get_val_unit(m$conf$tolerance[['rt']])
    rttol = as.numeric(tmp[['val']])
    rtunit = tmp[['unit']]
    if (rtunit == "s") {
        rttol = rttol/60.
    } else if (rtunit != "min") {
        stop('make_db_precursors: Unknown retention time unit.')
    }
    masses[!is.na(rt),`:=`(rt_min=rt-rttol,rt_max=rt+rttol)]
    
    ## Fine error.
    tmp = get_val_unit(m$conf$tolerance[['ms1 fine']])
    ms1tol = as.numeric(tmp[['val']])
    ms1unit = tmp[['unit']]
    if (ms1unit == "ppm") {
        masses[,`:=`(mz_fine_min=mz-ms1tol*mz*1e-6,mz_fine_max=mz+ms1tol*mz*1e-6)]
    } else if (ms1unit == "Da") {
        masses[,`:=`(mz_fine_min=mz-ms1tol,mz_fine_max=mz+ms1tol)]
    } else {
        stop('make_db_precursors: Unknown mass unit (fine).')
    }


    ## Coarse error.
    tmp = get_val_unit(m$conf$tolerance[['ms1 coarse']])
    ms1tol = as.numeric(tmp[['val']])
    ms1unit = tmp[['unit']]
    if (ms1unit == "ppm") {
        masses[,`:=`(mz_coarse_min=mz-ms1tol*mz*1e-6,mz_coarse_max=mz+ms1tol*mz*1e-6)]
    } else if (ms1unit == "Da") {
        masses[,`:=`(mz_coarse_min=mz-ms1tol,mz_coarse_max=mz+ms1tol)]
    } else {
        stop('make_db_precursors: Unknown mass unit (coarse).')
    }

    masses$isofine = -1L
    start = 1L
    while (start <= NROW(masses)) {
        sel = masses[start:.N]
        themz = sel[1L,mz]
        id = sel[1L,catid]
        upmz = sel[1L,mz_fine_max]
        x = sel[mz<(upmz)]
        stop = start + NROW(x) - 1L
        masses[(start):(stop),`:=`(isofine=..id,degfine=(1L+stop-start))]
        start = stop + 1L
    }

    masses$isocoarse = -1L
    start = 1L
    while (start <= NROW(masses)) {
        sel = masses[start:.N]
        themz = sel[1L,mz]
        id = sel[1L,catid]
        upmz = sel[1L,mz_coarse_max]
        x = sel[mz<(upmz)]
        stop = start + NROW(x) - 1L
        masses[(start):(stop),`:=`(isocoarse=..id,degcoarse=(1L+stop-start))]
        start = stop + 1L
    }

    

    masses[,`:=`(iso_coarse_min=min(mz_coarse_min),
                 iso_coarse_max=max(mz_coarse_max)),
                 by=isocoarse]

    masses[,`:=`(iso_fine_min=min(mz_fine_min),
                 iso_fine_max=max(mz_fine_max)),
                 by=isofine]
    setindex(masses,isocoarse,isofine)
    m$db$precursors = masses
    m
}




