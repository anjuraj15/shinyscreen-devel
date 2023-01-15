## Copyright (C) 2020,2021,2023 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.


## Description:
##
## Functions supporting MetFrag.


## Overall concept:
##
## For some input entries from `summ':
## 
## 1. Generate unique "stags" based on key of `summ'.
## 2. Deduce file names based on stags.
## 3. Run MetFrag on them.
## 4. Summarise Results.


metfrag_get_stag_tab <- function(summ) {
    ## Argument summ can be a subset of actual `summ' table.
    x = gen_1d_keytab(summ)
    data.table::setnames(x,old="key1d",new="stag")
    x[summ,`:=`(CE=i.CE,ion_mz=mz)]
}

metfrag_get_ms2_spec <- function(ms2,stag_entry) {
    x = ms2[kval_tab,.(mz,intensity),on=names(stag_entry)]

    x
}

metfrag_run <- function(param,path,subpaths,db_path,stag_tab,ms2,runtime,java_bin) {
    keys = intersect(colnames(stag_tab),colnames(ms2))
    message("Generating MetFrag configs.")
    file_tab = ms2[stag_tab,{
        r = write_metfrag_config(param = ..param,
                                 path = ..path,
                                 subpaths = ..subpaths,
                                 db_path = ..db_path,
                                 stag = stag,
                                 adduct = adduct,
                                 ion_mz = ion_mz,
                                 spec = data.table(mz=mz,intensity=intensity))
        c(r,stag = stag)
    },by=.EACHI,on=keys]
    message("Done generating MetFrag configs.")

    withr::with_dir(path,{
        metfrag_run_many(fn_jar = runtime,
                         file_tab = file_tab,
                         java_bin = java_bin)
    })
}

    

get_metfrag_targets <- function(stag_tab,ms2) {
    ## Take the columns we need from summ.
    x = summ[ms2_sel==T,.SD,.SDcols=c(key(summ),"mz")]
    mrg_keys = c(intersect(key(ms2),key(summ)),"an")
    x=ms2[x,.(CE=CE,ion_mz=i.mz,mz,intensity),on=mrg_keys,by=.EACHI]
    ## Get column order so that `an' follows `CE'.
    resnms = setdiff(mrg_keys,"an")
    nms = union(union(resnms,"CE"),c("an","ion_mz","mz","intensity"))
    data.table::setcolorder(x,neworder = nms)
    setkeyv(x,unique(c(resnms,"CE","an")))
    x
    
}

## get_metfrag_run_data(stag_tab,ms2,n=1) {
## }


metfrag_on_state <- function(mconf,mrun,summ) {
    
    
}

## metfrag_on_state <- function(m,fmany=metfrag_run_many) {
##     tgts = get_metfrag_targets(m$out$tab$summ,m$extr$ms2)

##     if (NROW(tgts)==0L) stop("No good spectra for MetFrag.")
    
##     files = tgts[,write_metfrag_config(param=m$conf$metfrag$param,
##                                        path = m$run$metfrag$path,
##                                        subpaths = m$run$metfrag$subpaths,
##                                        db_path = m$run$metfrag$db_path,
##                                        stag = paste0(.BY,collapse="_"),
##                                        adduct = adduct,
##                                        ion_mz = ion_mz[1],
##                                        spec = .SD[,c("mz","intensity")]),
##                  by=key(tgts)]

##     data.table::setnames(files,old="V1",new="files")

##     withr::with_dir(m$run$metfrag$path,{
##         x = files[,data.table(fn_conf= .SD[1,files],
##                      fn_log= .SD[2,files])
##                  ,by=key(files)]
##         fmany(fn_jar = m$run$metfrag$runtime,
##               fn_conf = x$fn_conf,
##               fn_log = x$fn_log,
##               java_bin = m$run$metfrag$java_bin)
        
##         list(targets=files)
##     })
## }


write_metfrag_config <- function(param,path,subpaths,db_path,stag,adduct,ion_mz,spec) {
    check_not_one(ion_mz,"ion_mz")
    check_not_one(adduct,"adduct")
    dir_res = subpaths$results
    dir_spec = subpaths$spec
    dir_conf = subpaths$config
    dir_log = subpaths$log

    f_spec = file.path(dir_spec,paste0(param$SampleName,"_",stag,".csv"))
    f_conf = file.path(dir_conf,paste0(param$SampleName,"_",stag,".conf"))
    f_log = file.path(dir_log,paste0(param$SampleName,"_",stag,".log"))
    f_res = paste0(param$SampleName,"_",stag)
    withr::with_dir(path,{
        param$SampleName = f_res
        param = c(param,list(IonizedPrecursorMass=ion_mz,
                             IsPositiveIonMode=ifelse(grepl(r"(\+$)",adduct),"True","False"),
                             PrecursorIonMode=METFRAG_ADDUCT_SWITCHES[[adduct]],
                             ResultsPath="results",
                             PeakListPath=f_spec))

        if (nchar(db_path)>0L) param = c(param,list(LocalDatabasePath = db_path))
        data.table::fwrite(spec,file=f_spec,col.names=F,sep=" ")
        write_keyval_file(namedl=param,fname=f_conf)
    })

    list(f_conf=f_conf,
         f_log=f_log,
         f_spec=f_spec)
    
}


metfrag_run_one <- function(fn_jar, fn_conf, fn_log, mem = NA_character_, java_bin = "java") {
    ## Check if file exists.

    ## Assemble arguments.
    args <- c('-jar',fn_jar,fn_conf)
    message(fn_conf)
    ## If total heap memory given (in quantities like '4m', or '2g')
    ## then make this argument.
    if (!is.na(mem)) args <- c(paste0('-Xmx', mem),args)
    ## Start new java process.
    p <- processx::process$new(java_bin,args=args,stdout=fn_log,stderr='2>&1')
    ## p$wait()
    ## p$get_exit_status()
    p
}


metfrag_run_many_w_futures <- function(fn_jar,fn_conf,fn_log, mem = NA_character_, java_bin = "java") {
    ntasks = length(fn_conf)
    procs = list()
    compl_prev = 0
    message("MetFrag started: ", ntasks," tasks.")
    for (n in 1:ntasks) {
        procs = c(future::future({
            
            st =  metfrag_run(fn_jar = fn_jar,
                              fn_conf = fn_conf[n],
                              fn_log = fn_log[n],
                              mem = mem,
                              java_bin = java_bin)
            st
            
            
        },seed = T), procs)

        reslv = which(sapply(procs,future::resolved))
        compl = length(reslv)
        
        if (compl > compl_prev) {
            message("Completed MetFrag tasks: ", compl, "/",ntasks)
            compl_prev <- compl
        }

        
    }
}


metfrag_run_many <- function(fn_jar,file_tab, mem = NA_character_, java_bin = "java",nproc=1L) {
    ntasks = NROW(file_tab)

    k = ntasks %/% nproc
    ndone = 0L
    lc = 1L
    while (ndone < ntasks) {
        ncurr_last = min(ndone + k*lc,ntasks)
        procs = list()
        for (i in (ndone + 1):ncurr_last) {
            fn_conf = file_tab[i,f_conf]
            fn_log = file_tab[i,f_log]
            message("fn_conf:", fn_conf)
            procs[[i-ndone]] = metfrag_run_one(fn_jar,
                                               fn_conf= fn_conf,
                                               fn_log = fn_log,
                                               mem = mem,
                                               java_bin = java_bin)
        }
        for (p in procs) {
            p$wait()
        }
        message("Completed MetFrag tasks: ", ncurr_last,"/",ntasks,".")
        ndone = ncurr_last
        lc = lc + 1L
    }

    
}


summarise_metfrag_results <- function(m) {
}
