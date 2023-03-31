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



merge_precid_4_isobars <- function(orig_precids,masses,up_masses) {
    start = head(orig_precids,1L)
    n = length(orig_precids)
    precid = orig_precids
    i = 1L
    while (i < n) {
        theprecid = orig_precids[[i]]
        themz = masses[[i]]
        mzup = up_masses[[i]]
        w = which(masses[(i+1L):n]<mzup)
        precid[(i+1L):n][w] = theprecid
        i = i + length(w) + 1L
    }
    precid
}

make_db_precursors <- function(m) {
    ## Generate masses and label isobars.

    cat = m$db$cat
    masses = m$out$tab$comp[cat,.(tag=tag,catid=catid,mz=mz,rt=rt),on=key(cat)]
    setkey(masses,tag,mz)

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
    ## TODO: FIXME: Should precids be unique, or not?

    ## Assign "fine" isobars to same isocoarse number.
    masses[,precid:=merge_precid_4_isobars(catid,mz,mz_fine_max),by="tag"]

    ## Assign "coarse" isobars to same isocoarse number.
    masses[,isocoarse:=merge_precid_4_isobars(catid,mz,mz_coarse_max),by="tag"]
    

    masses[,`:=`(iso_coarse_min=min(mz_coarse_min),
                 iso_coarse_max=max(mz_coarse_max)),
                 by=isocoarse]

    masses[,`:=`(iso_fine_min=min(mz_fine_min),
                 iso_fine_max=max(mz_fine_max)),
                 by=precid]
    setindex(masses,isocoarse,precid)
    ## Add files.
    filetab = m$input$tab$mzml[m$db$cat,.(catid=i.catid,file=file),on=c("set","tag"),nomatch=NULL]
    masses[filetab,file:=i.file,on="catid"]
    m$db$precursors = masses
    m
}


empty_spectra_table <- function() {
    r = data.table(precid=integer(0),
                   scan=character(0),
                   mz=numeric(0),
                   intensity=numeric(0))
    setkey(r,precid,scan)
    r
}


summ_needs_from_cat <- function(cat) {
    ## Catalogue columns.
    cat
}

summ_needs_from_precursors <- function(res,precursors) {
    ## Mass columns.
    precursors[res,on=.(catid),.(precid,
                                 mz,
                                 set,
                                 adduct,
                                 tag,
                                 ID,
                                 mz_l=mz_fine_min,
                                 mz_r=mz_fine_max),by=.EACHI]
}

summ_needs_from_qa <- function(res,qa) {
    needs = qa[,.SD,.SDcols=c("precid",
                              "ce",
                              "scan",
                              "ms1_rt",
                              "ms1_int",
                              "ms2_rt",
                              "ms2_int",
                              "ms1_mean",
                              "ms2_sel",
                              "qa_pass",
                              "qa_ms1_exists",
                              "qa_ms2_exists",
                              "qa_ms1_good_int",
                              "qa_ms1_above_noise",
                              "qa_ms2_near",
                              "qa_ms2_good_int",
                              "qlt_ms1",
                              "qlt_ms2")]

    res = needs[res,on=.(precid),allow.cartesian=T]
    ## TODO: additional processing?
    res
}

summ_needs_from_comp <- function(res,comp) {
    needs = comp[,.(set,ID,Name,SMILES)]
    setkey(needs,set,ID)
    res[needs,on=.(set,ID),`:=`(Name=i.Name,
                                SMILES=i.SMILES)]
}

## This function creates `summ' table. 
gen_summ <- function(db,qa,comp) {

    ## Start with the basic things.
    res = summ_needs_from_cat(db$cat)

    ## Add masses and precids.
    res = summ_needs_from_precursors(res,db$precursors)

    ## Add qa columns.
    res = summ_needs_from_qa(res,qa)

    setkeyv(res,SUMM_KEY)

    ## Add comp columns.
    summ_needs_from_comp(res,comp)
    
}
