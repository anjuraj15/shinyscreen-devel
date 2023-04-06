


PLOTTING_STATE_DB = readRDS(system.file(package="shinyscreen","testdata","plotting-state-db.rds"))


synthetise_cgm_ms1 <- function(n,fac,shift) {
    dt = data.table(rt=numeric(n),intensity=numeric(n))
    dt[1L:n,rt:=seq(length.out=n)]
    rt0 = dt[.N/2L + shift,rt]
    dt[,intensity:=fac*exp(-((rt - rt0)/2.)**2)]
}

synthetise_cgm_ms2 <- function(cgm1,shift) {
    dt = copy(cgm1)
    m = dt[,max(intensity)]
    n0 = dt[,.N%/%2L] + shift
    dt[,intensity:=NA_real_]
    dt[n0-1L,intensity:=0.05*m]
    dt[n0,intensity:=0.1*m]
    dt[n0+1L,intensity:=0.07*m]
    dt
}


synthetise_spectra <- function(n_mz,precursors,cgm2) {

    do_int <- function(n_mz,i) {
        things = runif(n_mz - 1L,0.5*i,0.9*i)
        c(things,i)
    }
    do_mass <- function(n_mz,mz) {
        c(runif(n_mz-1L,0.1*mz,0.7*mz),mz)
    }
    cgm2[precursors,on=.(precid),
         .(mz=do_mass(n_mz,i.mz),
           intensity=do_int(n_mz,intensity[[1]]),
           scan=scan[[1]]),
         by=.EACHI]

}

synthetise_pseudo_state_db <- function(db) {


    ## Fix input parameters.
    set.seed(22) # Random generator seed for reproducible results.
    n_time = 11L
    n_mz = 3L
    n_precid = length(db$precursors[,unique(precid)])
    n_cgm = n_time * n_precid
    n_spectra = n_time * n_precid * n_mz
    
    shifts = sample(c(-1L,0L,1L),size=n_precid,replace=T)
    facs = runif(n_precid,min=0.5)*1.e8
    inp = data.table(precid=db$precursors[,unique(precid)],
                     fac=facs,
                     shift=shifts)
    setkey(inp,precid)


                     

    extr = list()
    extr$cgm$ms1 = empty_cgram_ms1(n_cgm)
    extr$cgm$ms2 = empty_cgram_ms2(n_cgm)
    extr$spectra = empty_spectra_table(n_spectra)
    uprec = db$precursors[,unique(.SD),.SDcols=c("file","tag","precid")]
    uprec[,{
        f=file
        for (i in seq(length.out=length(precid))) {
            b=(.GRP-1L)*length(precid)*n_time + (i-1L)*n_time + 1L
            e=b+n_time - 1L
            pp = precid[[i]]
            cx=(.GRP-1L)*length(precid) + i

            faci = inp[precid==pp,fac]
            shifti = inp[precid==pp,shift]
            ri = synthetise_cgm_ms1(n=n_time,
                                    fac=faci,
                                    shift=shifti)
            
            ri2 = synthetise_cgm_ms2(ri,
                                     shift=shifti)

            extr$cgm$ms1[b:e,
                        `:=`(precid=pp,
                             cgmidx=cx,
                             file=f,
                             rt=ri[,rt],
                             intensity=ri[,intensity])]

            scans = paste0("F1.S",formatC(b:e,width=6,flag="0"))
            extr$cgm$ms2[b:e,
                        `:=`(precid=pp,
                             ce=50,
                             scan=scans,
                             idx=b:e,
                             rt=ri2[,rt],
                             intensity=ri2[,intensity])]

            

            
        }
        
    },
    by=c("file","tag")]
    extr$cgm$ms2 = extr$cgm$ms2[!is.na(intensity)]
    db$extr = extr
    db$extr$spectra = synthetise_spectra(n_mz,db$precursors,db$extr$cgm$ms2)

    set.seed(NULL)

    browser()
    1+1
    db
    
}

## synthetise_eic_ms1 <- function(precursors) {
##     ## N = 
##     dt = empty_cgram_ms1(
## }

