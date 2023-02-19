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


metfrag_gen_entry_fname <- function(kv) {
    paste0("mf_summ_entry_",gen_1d_name(kv),".csv")
}

metfrag_get_stag_tab <- function(summ) {
    ## Argument summ can be a subset of actual `summ' table.
    x = gen_1d_keytab(summ)
    data.table::setnames(x,old="key1d",new="stag")
    res = x[summ,`:=`(CE=i.CE,ion_mz=mz)]
    res
}

metfrag_get_ms2_spec <- function(ms2,stag_entry) {
    x = ms2[kval_tab,.(mz,intensity),on=names(stag_entry)]

    x
}

get_mf_res_ext <- function(fn) {
    ext = sub(pattern = r"(^.*\.([[:alnum:]]+)$)",r"(\1)", fn)
    check_extension(c(ext=ext,file=fn),what="mf-res-file")
    ext
}

metfrag_run <- function(param,path,subpaths,db_dir,stag_tab,ms2,runtime,java_bin,nproc = 1L) {
    keys = intersect(colnames(stag_tab),colnames(ms2))
    message("Generating MetFrag configs.")
    file_tab = ms2[stag_tab,{
        r = write_metfrag_config(param = ..param,
                                 path = ..path,
                                 subpaths = ..subpaths,
                                 db_dir = ..db_dir,
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
                         java_bin = java_bin,
                         nproc = nproc)
    })

    ## We don't know (so well) in advance what are the endings of the
    ## results files. Lets find this out.
    pth = file.path(path,subpaths[["results"]])
    a_res_f = list.files(path = pth,
                         pattern = param$SampleName)[[1]]
    ext = get_mf_res_ext(a_res_f)
    file_tab[,f_res:=paste0(param$SampleName,"_",stag,".",(ext))]

    
    
}

mf_narrow_summ <- function(summ,kv,ms2_rt_i=NA_integer_,ms2_rt_f=NA_integer_) {
    skey = data.table::key(summ)
    cols = c("adduct","tag","ID","CE","an","mz","qa_pass","ms2_rt")
    nsumm = get_rows_from_summ(summ,kv,cols)
    nsumm = nsumm[qa_pass==T] # Those that make sense.
    nsumm_key = union(SUMM_KEY,"ms2_rt")
    data.table::setkeyv(nsumm,nsumm_key)
    
    if (!is.na(ms2_rt_i)) {
        nsumm = nsumm[ms2_rt>(ms2_rt_i)]
    }

    if (!is.na(ms2_rt_f)) {
        nsumm = nsumm[ms2_rt<(ms2_rt_f)]
    }

    nsumm
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

write_metfrag_config <- function(param,path,subpaths,db_dir,stag,adduct,ion_mz,spec) {
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

        if (nchar(db_dir)>0L) param = c(param,list(LocalDatabasePath = db_dir))
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

metfrag_run_many <- function(fn_jar,file_tab, mem = NA_character_, java_bin = "java",nproc=1L) {
    ntasks = NROW(file_tab)

    todo = min(nproc,ntasks)
    k = ntasks %/% todo
    ndone = 0L
    lc = 1L
    while (ndone < ntasks) {
        ncurr_last = min(ndone + k*lc,ntasks)
        procs = list()
        for (i in (ndone + 1):ncurr_last) {
            fn_conf = file_tab[i,f_conf]
            fn_log = file_tab[i,f_log]
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


summarise_metfrag_results <- function(param,path,subpaths,cand_parameters,db_scores,int_scores,collect_candidates,file_tab) {

    ## which(max(as.numeric(mf_res$Score))==as.numeric(mf_res$Score))
    index_maxScore = 1L
    
    ## First detect which reader we need.
    ext = get_mf_res_ext(file_tab[1,f_res])
    readf = METFRAG_RESULT_READF[[ext]]
    keyz = as.character(union(key(file_tab),"stag"))
    .read_results <- function() {
        file_tab[,{
            fn = file.path(..path,subpaths$results,f_res)
            dt = data.table::rbindlist(lapply(fn,function (ff) as.data.table(readf(ff))))
            dt
        },
        keyby = keyz]


    }

    .adapt_col_types <- function(x) {
        x[,(names(db_scores)):=lapply(.SD, as.numeric),.SDcol=names(db_scores)]
    }

    .calc_basic_scores <- function(x) {
        x[,.(num_poss_IDs=length(Score),
             max_Score=max(Score),
             n_Score_GE4=length(which(Score>=4)),
             n_Score_GE3=length(which(Score>=3)),
             n_Score_GE2=length(which(Score>=2))),
          by=keyz]

    }

    .get_candidate_param <- function(x) {
        res = x[,.SD[..index_maxScore],
                .SDcol=cand_parameters,
                keyby=keyz]
        ## data.table::setnames(res,old = names(res), new = paste0("top_",names(res)))
        res
    }


    .make_max_cols <- function(x) {
        res = x[,{
            cols = lapply(.SD,function(s) max(s,na.rm=T))
            names(cols) = lapply(names(.SD),function(nn) paste0("Max_",nn))
            cols}, .SDcol=c(names(int_scores),names(db_scores)),keyby=keyz]
        res
    }


    .collect_candidates <- function(x) {
        res = x[,{cols = lapply(.SD, function(col) paste(col,collapse=";"))
                  names(cols) = lapply(names(.SD),function(nn) paste0("All_",nn))
                  cols},
                .SDcol=collect_candidates,
                keyby=keyz]
        res
    }


    thetab = .read_results()
    .adapt_col_types(thetab)
    btab = .calc_basic_scores(thetab)
    cctab = .collect_candidates(thetab)
    candtab = .get_candidate_param(thetab)
    mxtab = .make_max_cols(thetab)


    res = file_tab[,`:=`(f_conf=NULL,f_log=NULL,f_res=NULL,f_spec=NULL)]
    data.table::setkeyv(res,c(key(res),"stag"))
    res = res[btab]
    res = res[candtab]
    res = res[mxtab]
    res = res[cctab]

    res

    
        
}

