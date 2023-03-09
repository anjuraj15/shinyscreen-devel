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

    ## Get tolerance.
    tmp = get_val_unit(m$conf$tolerance[['ms1 fine']])
    ms1tol = as.numeric(tmp[['val']])
    ms1unit = tmp[['unit']]
    cat = m$db$cat
    masses = m$out$tab$comp[cat,.(catid=catid,mz=mz),on=key(cat)]
    setkey(masses,mz)
    if (ms1unit == "ppm") {
        masses[,`:=`(mz_min=mz-ms1tol*mz*1e-6,mz_max=mz+ms1tol*mz*1e-6)]
    } else if (ms1unit == "Da") {
        masses[,`:=`(mz_min=mz-ms1tol,mz_max=mz+ms1tol)]
    } else {
        stop('make_db_precursors: Unknown mass unit.')
    }
    masses$isobar = -1L
    ## masses[,isobar := fifelse(isobar==-1L & mz_min < themz & themz < mz_max,..id,catid)]

    start = 1L
    while (start <= NROW(masses)) {
        sel = masses[start:.N]
        themz = sel[1L,mz]
        id = sel[1L,catid]
        upmz = sel[1L,mz_max]
        x = sel[mz<(upmz)]
        stop = start + NROW(x) - 1L
        message('a',start,'o',stop)
        masses[(start):(stop),`:=`(isobar=..id,deg=(1L+stop-start))]
        start = stop + 1L
    }
    m$db$precursors = masses
    m
}
