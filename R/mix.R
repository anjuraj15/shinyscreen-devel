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

stripext<-function(fn) {
    bits<-strsplit(fn,split="\\.")[[1]]
    if (length(bits)> 1) paste(head(bits,-1),collapse=".") else fn}

get_mz_cmp_l<-function(id,adduct,cmpL) {
    ind<-match(id,cmpL$ID)
    mz<-cmpL$mz[[ind]]
    smiles<-cmpL$SMILES[[ind]]
    res<-if (!is.null(mz) && !is.na(mz)) {
             mz
         } else if (nchar(smiles)>0)
         {
             mde<-as.character(adduct)
             wh<-ADDUCTMAP[[mde]]
             RChemMass::getSuspectFormulaMass(smiles)[[wh]]
         } else stop("Both SMILES and mz fields, for ID ",id,", found empty in the compound list. Aborting.")
    res
}

calc_mz_from_formula_outer <- function(chform,adduct,id) {
    check_chform <- enviPat::check_chemform(ISOTOPES,chform)
    wind <- which(check_chform$warning)
    if (length(wind) > 0) stop("Cannot understand the following formulas: ",
                               paste(check_chform$new_formula[wind],collapse = ","))
    mol_form <- check_chform$new_formula
    l_mol <- length(mol_form)
    l_add <- length(adduct)
    
    adds <- ADDUCTS[Name %in% adduct,.(Name,
                                       add=as.character(Formula_add),
                                       ded=as.character(Formula_ded),
                                       charge=Charge)]

    dt <- dtable(ID = rep(id,each = l_add),
                 mol_form = rep(mol_form,each = l_add),
                 adduct = rep(adds$Name,l_mol),
                 add = rep(adds$add,l_mol),
                 ded = rep(adds$ded,l_mol),
                 charge= rep(adds$charge,l_mol))
    
    merger <- function (mol_form,add,ded) {
        full_form <- rep(NA_character_,length(mol_form))
        both_ind <- which(add != 'FALSE' & ded != 'FALSE')
        add_only_ind <- which(add != 'FALSE' & ded == 'FALSE')
        ded_only_ind <- which(ded != 'FALSE' & add == 'FALSE')
        ainds <- c(both_ind,add_only_ind)
        full_form[ainds] <- vapply(ainds,function (i) enviPat::mergeform(mol_form[[i]],add[[i]]),FUN.VALUE = character(1), USE.NAMES = F)
        dinds <- c(both_ind,ded_only_ind)
        full_form[dinds] <- vapply(dinds,function (i) {
            z <- check_ded2(mol_form[[i]],ded[[i]])
            if (z) enviPat::subform(mol_form[[i]],ded[[i]]) else NA_character_
        },
        FUN.VALUE = character(1))
        full_form
    }
    dt[,("full_form"):=.(merger(mol_form,add,ded))]
    dt[!is.na(full_form),("mz"):=.(mapply(function(ff,ch) enviPat::isopattern(ISOTOPES,chemforms = ff,
                                                                              charge = ch, verbose = F)[[1]][1],
                                          full_form,
                                          charge, USE.NAMES = F))]
    dt[is.na(full_form),("mz"):=NA_real_]
    dt
}

calc_mz_from_formula <- function(chform,adduct,id) {
    if (length(chform) == 0 ) return(numeric(0))
    check_chform <- enviPat::check_chemform(ISOTOPES,chform)
    wind <- which(check_chform$warning)
    if (length(wind) > 0) stop("Cannot understand the following formulas: ",
                               paste(check_chform$new_formula[wind],collapse = ","))
    mol_form <- check_chform$new_formula
    uad <- unique(adduct)
    uadds <- lapply(uad,function(a) ADDUCTS[Name==a,.(Name,
                                                      add=as.character(Formula_add),
                                                      ded=as.character(Formula_ded),
                                                      charge=Charge),on=""])
    names(uadds) <- uad
    adds <- rbindlist(l=lapply(adduct,function(a) uadds[[a]]))

    merger <- function (mol_form,add,ded) {
        res <- numeric(length(mol_form))
        both_ind <- which(add != 'FALSE' & ded != 'FALSE')
        add_only_ind <- which(add != 'FALSE' & ded == 'FALSE')
        ded_only_ind <- which(ded != 'FALSE' & add == 'FALSE')
        ainds <- c(both_ind,add_only_ind)
        res[ainds] <- vapply(ainds,function (i) enviPat::mergeform(mol_form[[i]],add[[i]]),FUN.VALUE = character(1), USE.NAMES = F)
        dinds <- c(both_ind,ded_only_ind)
        res[dinds] <- vapply(dinds,function (i) {
            z <- check_ded2(mol_form[[i]],ded[[i]])
            if (z) enviPat::subform(mol_form[[i]],ded[[i]]) else NA_character_
        },
        FUN.VALUE = character(1))
        res
    }
    forms  <- merger(mol_form,adds$add,adds$ded)
    
    ## Check if formulas actually calculated.
    bad_idx <- which(forms=="0")
    bad_adducts <- adduct[bad_idx]
    bad_ids <- id[bad_idx]
    non_dups <- !duplicated(bad_idx)
    bad_ids <- bad_ids[non_dups]
    bad_adducts <- bad_adducts[non_dups]
    if (length(bad_idx)>0) stop(paste0("Unable to process the adducts:\n",
                                        paste(bad_adducts,collapse = ","),
                                        "\nfor id-s:",
                                        paste(bad_ids,collapse = ",")))
    
    mz <- the_ifelse(!is.na(forms),
                     mapply(function(ff,ch) enviPat::isopattern(ISOTOPES,chemforms = ff,
                                                                charge = ch, verbose = F)[[1]][1],
                            forms,
                            adds$charge, USE.NAMES = F),
                     NA_real_)
    mz
}



smiles2form <- function(smiles) {
    res <- character(length(smiles))
    res <- NA_character_
    isomething <- which(!is.na(smiles) | nchar(smiles) > 0L) 
    one2form <- function (s) {
        mol <- try(RMassBank::getMolecule(s), silent = T)
        if (!is.atomic(mol)) {
            (rcdk::get.mol2formula(mol))@string            
        } else ""
    }

    res[isomething] <- sapply(smiles[isomething],one2form,USE.NAMES = F)
    res
}



calc_mz_from_smiles_outer <- function(smiles,adduct,id) {
    mol <- lapply(smiles,function(s) try(RMassBank::getMolecule(s), silent = T))
    check <- which(is.atomic(mol))
    if (length(check) > 0)
        stop("Errors in SMILES with IDs:",paste(id[which],collapse = ','))

    mol_form <- sapply(mol,function(x) (rcdk::get.mol2formula(x))@string,USE.NAMES = F)
    names(mol_form) <- id
    calc_mz_from_formula_outer(mol_form,adduct,id)
    
    
}

get_col_from_cmp_l<-function(id,cname,cmpL) {
    ind<-match(id,cmpL$ID)
    x<-cmpL[[cname]][[ind]]
    if (!is.null(x)) x else NA
    
}

gen_clean_state_summ<-function(summ) {
    summ$Comments <- ""
    summ[c("MS1","MS2","Alignment","AboveNoise")] <- T
    summ["MS2rt"] <- NA_real_
    summ["iMS2rt"] <- NA_integer_
    summ["rt"]<-NA_real_
    summ["checked"]<-'NONE'
    summ
}
gen_empty_summ <- function() {
    EMPTY_SUMM
}

## gen_summ <- function(comp,qa_ms1,qa_ms2) {
##     comp_cols <- intersect(SUMM_COLS,colnames(comp))
##     summ <- comp[,..comp_cols]
##     data.table::setkeyv(summ,BASE_KEY)
##     ms1_cols <- intersect(SUMM_COLS,colnames(qa_ms1))
##     ms1_cols <- setdiff(ms1_cols,colnames(summ))
##     summ <- qa_ms1[summ,c(..comp_cols,..ms1_cols),on=BASE_KEY]
##     ms2_cols <- intersect(colnames(qa_ms2),SUMM_COLS)
##     ms2_cols <- setdiff(ms2_cols,colnames(summ))
##     summ <- qa_ms2[summ,c(..comp_cols,..ms1_cols,..ms2_cols),on=BASE_KEY]
##     data.table::setkeyv(summ,c(BASE_KEY_MS2,"scan"))
##     summ[,qa_ms1_exists:=the_ifelse(!is.na(qa_ms1_good_int),T,F)]
##     summ[,qa_ms2_exists:=the_ifelse(!is.na(CE),T,F)]
##     summ[,qa_pass:=apply(.SD,1,all),.SDcols=QA_FLAGS[!(QA_FLAGS %in% "qa_pass")]]
##     summ$Comments<-""
##     data.table::setkeyv(summ,DEF_KEY_SUMM)
##     data.table::setcolorder(summ,SUMM_COLS)

##     ## Quality scores for ms1 and ms2.
##     summ[,qlt_ms1 := as.integer(Map(function(m1,m2,m3) { m1*5L + m2*3L + m3*2L},
##                                     as.integer(qa_ms1_exists),
##                                     as.integer(qa_ms1_above_noise),
##                                     as.integer(qa_ms1_good_int)))]
##     summ[,qlt_ms2 := as.integer(Map(function(m1,m2,m3) { m1*5L + m2*3L + m3*2L},
##                                     as.integer(qa_ms2_exists),
##                                     as.integer(qa_ms2_near),
##                                     as.integer(qa_ms2_good_int)))]

##     summ[is.na(qlt_ms1),qlt_ms1:=0L]
##     summ[is.na(qlt_ms2),qlt_ms2:=0L]

##     summ
    
## }
pp_touch_q<-function(summ) {
    ## Returns indices that are ok to be auto processed.
    which(summ$checked==SUMM_CHK_NONE | summ$checked==SUMM_CHK_AUTO)
}

preProc <- function (summ,noiseFac=3,errRT=0.5,intThreshMS1=1e5,intThreshMS2=5000.) {
    wds<-unique(summ$wd)
    fn_spec<-function(wd) readRDS(file.path(wd,FN_SPEC))
    message("Loading RDS-es ...")
    allData<-lapply(wds,fn_spec)
    names(allData)<-wds
    message("... done with RDSs")
    


    ## QA check plan:
    ##
    ## If MS1 does not exist, set MS1 to F, as well as everything else except MS2.
    ## If it exists, proceed to noise check.
    ## If noise check fails, set AboveNoise and Alignment to F.
    ##
    ##
    ## MS2 will be checked independently.
    ## If MS2 does not exist, set MS2 and Alignment to F.
    ## If it does, check the Alignment.
    ## If Alignment is wrong, set Alignment to F.
    ##
    ## Terminology: MS1 does not exist if the intensity is below the
    ## intensity threshold. MS2 does not exist if it was not picked up
    ## during the dataframe generation stage. In this case, the file
    ## with the corresponding ID will not be there.

    okinds<- pp_touch_q(summ)
    for (ind in okinds) {
        wd <- summ$wd[ind]
        id <- summ$ID[ind]
        eics<-allData[[wd]]$eic
        nid<-id2name(id)
        ii<-match(nid,MSnbase::fData(eics)[["ID"]]) #id, because id-s, not nid-s are in fData for ms1 eics;
        eic1<-eics[[ii]]
        eic<-data.frame(rt=MSnbase::rtime(eic1)/60.,intensity=MSnbase::intensity(eic1))
        colnames(eic)<-c("rt","intensity")
        maxInt <- NULL
        if (nrow(eic)==0) {
            warning("No chromatogram for id ",id," found in", wd, " . Skipping.")
            next
        }

        
        ms1MaxInd<-which.max(eic$intensity)
        maxInt<-eic$intensity[[ms1MaxInd]]
        summ[ind,"rt"]<-eic$rt[[ms1MaxInd]]
        ##If MS1 does not exist, set entry to F.
        if (maxInt < intThreshMS1) {
            summ[ind,"MS1"] <- F
            ## Other checks automatically fail, too.
            summ[ind,"Alignment"] <- F
            summ[ind,"AboveNoise"] <- F
        } else {
            ## Noisy?
            if (summ[ind,"AboveNoise"]) {
                mInt <- mean(eic$intensity)
                if (maxInt < noiseFac*mInt) {
                    summ[ind,"AboveNoise"] <- F
                    summ[ind,"Alignment"] <- F ## If noisy, this is
                    ## probably meaningles, so
                    ## F.
                }
                
            }
        }
        
        

        ## MS2 checks.
        ms2<-allData[[wd]]$ms2
        ms2nids<-names(ms2)
        if (! (nid %in% ms2nids)) {
            summ[ind,"MS2"] <- F
            summ[ind,"Alignment"] <- F
        } else {
            sp<-ms2[[nid]]
            ## Alignment still makes sense to be checked?
            if (summ[ind,"Alignment"]) {
                ## rtInd <- ms1MaxInd #match(maxInt,eic$intensity)
                rtMS1Peak <- eic$rt[[ms1MaxInd]]
                msms<-MSnbase::fData(sp)[,c("rtm","maxI")]
                colnames(msms)<-c("rt","intensity")
                rtInd <- which((msms$rt > rtMS1Peak - errRT) &
                               (msms$rt < rtMS1Peak + errRT)) #Close enough?
                
                rtIndMS1 <- which((eic$rt > rtMS1Peak - errRT) &
                                  (eic$rt < rtMS1Peak + errRT)) #Filter the relevant MS1 part.
                
                eicFilt<- eic[rtIndMS1,]
                eicFilt<- eicFilt[which(eicFilt$intensity>intThreshMS1),]
                mInt<- maxInt #mean(eicFilt$intensity)
                rtInd <- rtInd[which(msms$intensity[rtInd]>intThreshMS2)] #Intense enough?
                msmsRT <- msms$rt[rtInd]
                msmsInt<- msms$intensity[rtInd]
                if (length(msmsRT) > 0) {
                    msmsRTind <- which.min(abs(msmsRT - rtMS1Peak))
                    summ[ind,"iMS2rt"] <- rtInd[msmsRTind]
                    summ[ind,"MS2rt"] <- msmsRT[msmsRTind]
                } else {
                    summ[ind,"Alignment"] <- F
                }
            }
        }
        summ[ind,"checked"]<-SUMM_CHK_AUTO
    }
    
    summ
}

gen_ms2_spec_data <- function(id,tag,iMS2rt,data,luckyN=NA) {
    ## Given the id, tag and the index of the MS2 spectrum, return the
    ## dataframe of the spectrum, with luckyN number of lagerst
    ## intensity peaks.

    nid<-id2name(id)
    if (!is.na(iMS2rt)) {
        d <- data$ms2[[nid]][[iMS2rt]]
        if (!is.null(d)) {
            x<-data.frame(mz=MSnbase::mz(d),intensity=MSnbase::intensity(d))
            res<-if (!is.na(luckyN)) {
                     ord<-order(x$intensity,decreasing = T)
                     len<-length(ord)
                     sz<-min(len,luckyN)
                     nx<-x[ord,]
                     nx<-nx[1:sz,]
                     ord<-order(nx$mz)
                     nx[ord,]
                 } else x
            
            return(res)
            
        } else {
            return(NULL)}
        
    } else return(NULL)
}

gen_ms2_spec_fn <- function(id,tag,adduct,set,width=6) {
    suppressWarnings({
        iid<-as.numeric(id)
        iid<- if (!is.na(iid)) iid else id
        num <- formatC(iid,width = width,format='d',flag='0')
        ss<-trimws(paste(num,adduct,tag,set,sep="_"),which='both')
        paste(ss,".csv",sep='')
    })
}







add_wd_to_mzml <- function(fn,proj) {
    wd<-basename(tools::file_path_sans_ext(fn))
    file.path(proj,wd)
}


getEntryFromComp<-function(entry,id,set,adduct,compTab) {
    ind <- which(compTab$ID %in% id &
                 compTab$set %in% set &
                 compTab$adduct %in% adduct)

    res<- if (length(ind)==1) compTab[ind,entry] else {
                                                     if (length(ind)>1) {
                                                         warning("Nonunique selection in comprehensive table:")
                                                         for (i in ind) {
                                                             message('ID: ',compTab$ID[[i]],' set: ',compTab$set[[i]],' adduct: ',compTab$adduct[[i]])
                                                         }
                                                         warning("The compound set table likely containes duplicate IDs per set/adduct combination. Please correct this.")
                                                     } else {
                                                         warning("Entries not found for id ", id,"set ",set, "and adduct ", adduct, " .")
                                                     } 
                                                 }
    res
    names(res)<-entry
    res
    
}
## add_comp_summ <- function(ft,ctab) {
##     nR<-nrow(ft)
##     mzCol<-rep(NA,nR)
##     nmCol<-rep("",nR)
##     rtCol<-rep(NA,nR)

##     for (ir in 1:nR) {
##         id<-ft[ir,"ID"]
##         set<-ft[ir,"set"]
##         m<-ft[ir,"adduct"]
##         entries<-getEntryFromComp(c("mz","Name","rt"),id,set,m,ctab)
##         mzCol[[ir]]<-  entries[["mz"]]
##         nm<-entries[["Name"]]
##         nmCol[[ir]]<- if (!is.na(nm)) nm else ""
##         rtCol[[ir]]<- entries[["rt"]]
##     }
##     ft$mz<-mzCol
##     ft$Name<-nmCol
##     ft$rt<-rtCol
##     ft
## }

get_set_adduct <- function(s,mzml) {
    unique(mzml[set == s,adduct])
}

vald_comp_tab<-function(df,ndf,checkSMILES=F,checkMz=F,checkNames=F) {
    ## Fields.
    if (is.null(df$ID)) stop("Column ID missing in ",ndf," .")
    if (checkMz && is.null(df$mz)) stop("Column mz missing in ", ndf, " .")
    if (checkSMILES && is.null(df$SMILES)) stop("Column SMILES missing in", ndf, " .")
    
    if (checkNames && is.null(df$Name)) warning("Column Name missing in ", ndf," , continuing without.")
    if (is.null(df$RT) && is.null(df$rt)) {
        warning("Column RT (alternatively, rt) missing in ", ndf, ", continuing without.")
    } else {
        if (is.null(df$rt)) {
            df$rt<-df$RT
            df$RT<-NULL
        }
    }

    ## Missing IDs?
    ind<-which(is.na(df$ID))
    if (length(ind)>0) {
        for (i in ind) {
            warning("ID missing at row: ",i," . Big trouble ahead.")
        }
        return(NULL)
    }
    
    ## Unique IDs?
    luids<-length(unique(df$ID))
    if (length(df$ID) > luids) {
        warning("Duplicate IDs in ", ndf, " are not allowed.")
        return(NULL)
    }

    ## Missing SMILES?
    if (checkSMILES) {
        ind<-which(is.na(df$SMILES))
        if (length(ind)>0) {
            for (i in ind) {
                warning("SMILES missing at row: ",i, "; ID: ",df$ID[[i]]," .")
            }
            return(NULL)
        }
        lsmiles<-nrow(df)
        ll<-length(unique(df$SMILES))
        if (ll<lsmiles) {
            warning("There are duplicate SMILES in the compound list. Trouble ahead.")
        }
    }

    ## Missing mz?
    if (checkMz) {
        ind<-which(is.na(df$mz))
        if (length(ind)>0) {
            for (i in ind) {
                warning("mz missing at row: ",i, "; ID: ",df$ID[[i]]," .")
            }
            return(NULL)
        }
    }

    df
}

verify_cmpd_l <- function(dt,fn) {
    fields <- colnames(EMPTY_CMPD_LIST)
    dtflds <- colnames(dt)

    assert('ID' %in% dtflds, msg = paste('ID column must be present and filled in', fn))
    ess <- c('SMILES','Formula','mz')
    pres <- ess %in% dtflds
    assert(length(pres) > 0,
           msg = paste('Compound list from ',fn,
                       'does not contain any of "SMILES", "Formula", or "mz". \nThe compound list needs at least one of those to be valid.'))
    exst <- ess[pres]
    x <- lapply(exst,function (nm) do.call(all,as.list(is.na(dt[[nm]]))))
    assert(!do.call(all,x), msg = paste('At least one of', paste(exst,collapse = ','),
                                        '\nmust contain some values in compound list from',fn))
    
    invisible(T)
}


## INPUT TRANSLATORS

#' @export
grab_unit <- function(entry,unit) {
    what <- paste0("\\<",unit,"\\>$")
    entry <- trimws(entry,which="both")
    if (grepl(what,entry))
        suppressWarnings(as.numeric(sub(paste0("^(.*)",unit),"\\1",entry))) else NA_real_
}


rt_in_min <- function(entry) {
    if (length(entry)>0 && !is.na(entry) && nchar(entry)>0) {
        xs <- grab_unit(entry,"s")
        xm <- grab_unit(entry,"min")
        x <- if (is.na(xm)) xs/60. else xm
        x
    } else NA_real_
}

conf_trans_pres <- function(pres_list) {
    ## Translate and validate prescreening input.
    pres_list[CONF_PRES_NUM] <- sapply(pres_list[CONF_PRES_NUM],as.numeric)
    for (par in CONF_PRES_NUM) {
        assert(!suppressWarnings(is.na(pres_list[[par]])),msg=paste("Prescreen parameter",par,"is not a number."))
    }
    for (par in CONF_PRES_TU) {
        xs <- grab_unit(pres_list[[par]],"s")
        xm <- grab_unit(pres_list[[par]],"min")
        x <- if (is.na(xm)) xs else xm
        assert(!is.na(x),msg = paste("Time unit parameter error for",par,"Only s(econds) or min(utes) allowed."))
        pres_list[[par]] <- x
    }

    if (is.null(pres_list$det_ms2_noise)) pres_list$det_ms2_noise <- T
    if (is.null(pres_list$ms2_int_thresh) || is.na(pres_list$ms2_int_thresh)) pres_list$ms2_int_thresh <- 0.
    pres_list
}

gen_mz_err_f <- function(entry,msg) {
    eppm <- grab_unit(entry,"ppm")
    eda <- grab_unit(entry,"Da")
    shinyscreen:::assert(xor(is.na(eda), is.na(eppm)), msg = msg)
    if (is.na(eda)) function(mz) eppm*1e-6*mz else function (mz) eda
}


gen_rt_err <- function(entry,msg) {
    em <- grab_unit(entry,"min")
    es <- grab_unit(entry,"s")
    shinyscreen:::assert(xor(is.na(em), is.na(es)), msg = msg)
    if (is.na(em)) es/60. else em
}

##' @export
read_rt <- function(entry) {
    gen_rt_err(entry,"Badly formatted RT value. It should be: \"x min\", or \"x s\".")
}

fig_path <- function(top,set,group,id,suff,ext="pdf") {
    base <- paste("plot",set,group,id,suff,sep="_")
    fn <- paste0(base,".",ext)
    fn <- gsub("\\[","",fn)
    fn <- gsub("\\]","",fn)
    fn <- gsub("\\+","p",fn)
    fn <- gsub("-","m",fn)
    if (!is.null(top)) file.path(top,fn) else fn
}

get_coord_lim <- function(new,def) {
    if (is.null(new)) return(def)
    res <- new
    if (length(new[[1]])==0) res[[1]]<-def[[1]]
    if (length(new[[2]])==0) res[[2]]<-def[[2]]
    res
}


#' @export
tk_save_file <-  function (default = "", caption = "Select files", filters = NULL, index = 1) {
    args <- list("tk_getSaveFile", title = caption)
    if (nzchar(default)) 
        args <- c(args, initialdir = dirname(default), initialfile = basename(default))
    if (!is.null(filters)) {
        if (!is.character(filters) || length(dim(filters)) != 
            2 || ncol(filters) != 2) 
            stop("'filters' must be a 2-column character matrix")
        f <- filters
        f[] <- paste0("{", filters, "}")
        ff <- apply(f, 1, paste, collapse = " ")
        fff <- paste0("{", ff, "}")
        args <- c(args, filetypes = paste(fff, collapse = " "))
    }
    as.character(do.call(tcltk::tcl, args))
}

##' @export
get_rt_interval <- function(data_ms1,data_ms2,conf_figures) {
    rt_new_lim <- c(rt_in_min(conf_figures$rt_min),
                    rt_in_min(conf_figures$rt_max))
    rt_lim <- get_coord_lim(rt_new_lim,DEFAULT_RT_RANGE)

    ms1_lim <- range(data_ms1$rt)
    ms2_lim <- if (NROW(data_ms2)>0) range(data_ms2$rt_peak) else c(NA,NA)
    
    rlim <- min(rt_lim[[2]],ms1_lim[[2]],ms2_lim[[2]],na.rm = T)
    llim <- max(rt_lim[[1]],ms1_lim[[1]],ms2_lim[[1]],na.rm = T)
    c(llim-0.5,rlim+0.5)
}

##' @export
plot_struct <- function(m,plot_index) {

    id <- plot_index[["ID"]]
    if (is.null(id)) {
        p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                             ggplot2::aes(x=x,y=y))+
            ggplot2::geom_blank()+ggplot2::labs(x="",y="")

        p <- p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="STRUCTURE PLOT UNAVAILABLE", color="black")+
            ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                           axis.ticks.x=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.ticks.y=ggplot2::element_blank())
        return(p)
    } else {
        msfig <- m$out$tab$structfig
        if (id %in% msfig[,ID]) {
            grid::grid.draw(m$out$tab$structfig[ID==id,img][[1]],
                            recording = F)
        } else {
            p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                                 ggplot2::aes(x=x,y=y))+
                ggplot2::geom_blank()+ggplot2::labs(x="",y="")

            p <- p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="STRUCTURE PLOT UNAVAILABLE", color="black")+
                ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                               axis.ticks.x=ggplot2::element_blank(),
                               axis.text.y=ggplot2::element_blank(),
                               axis.ticks.y=ggplot2::element_blank())
            return(p)
        }

    }
    
}


##' @export
plot_struct_nowrap <- function(m,plot_index) {

    id <- plot_index[["ID"]]
    if (is.null(id)) {
        p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                             ggplot2::aes(x=x,y=y))+
            ggplot2::geom_blank()+ggplot2::labs(x="",y="")

        p <- p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="STRUCTURE PLOT UNAVAILABLE", color="black")+
            ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                           axis.ticks.x=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.ticks.y=ggplot2::element_blank())
        return(p)
    } else {
        msfig <- m$out$tab$structfig
        if (id %in% msfig[,ID]) {
            msfig[ID==id,img][[1]]
        } else {
            p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                                 ggplot2::aes(x=x,y=y))+
                ggplot2::geom_blank()+ggplot2::labs(x="",y="")

            p <- p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="STRUCTURE PLOT UNAVAILABLE", color="black")+
                ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                               axis.ticks.x=ggplot2::element_blank(),
                               axis.text.y=ggplot2::element_blank(),
                               axis.ticks.y=ggplot2::element_blank())
            return(p)
        }
    }
    
}

enum_na_scans <- function(vec) {
    ## Canibalised .locf function from RMassBank.
    tmp <- !is.na(vec)
    na_idx <- cumsum(tmp)
    
    ## The construct below deals with the fact that vec[0] ==
    ## integer(0) and not NA...  Therefore we retrieve indexes to
    ## c(NA,vec) from c(0, which(vec.isna)) + 1 (the +1 shifts all
    ## values so the 0s point to NA).

    c(NA,vec)[c(0,which(tmp))[na_idx+1]+1]
    
}

fix_na_precs <- function(ms) {
    fd_df <- MSnbase::fData(ms)
    fd <- data.table::as.data.table(fd_df, keep.rownames = "rn")
    ## Reset precursor numbers.
    fd[,precursorScanNum := NA_real_]
    ## Temporarily label MS1 precursors using their acquisition
    ## numbers.
    fd[msLevel == 1, precursorScanNum := acquisitionNum]
    ## Now populate the MS2 precursors based on MS1 scan numbers.
    fd[,precursorScanNum := enum_na_scans(precursorScanNum)]
    ## Reset MS1 precursors.
    fd[msLevel == 1, precursorScanNum := 0]
    ## Drop remaining MS2 NAs.
    notna_idx <- fd[!is.na(precursorScanNum),.I]
    fd <- fd[notna_idx]
    notna_rns <- fd[,rn]
    ms <- ms[notna_idx]
    fd_res <- as.data.frame(fd)
    ## Fix rownames.
    rownames(fd_res) <- notna_rns
    ## Modify header data for `ms'.
    MSnbase::fData(ms) <- fd_res
    ms
    
}

omit_na_precs <- function(ms) {
    fd_df <- MSnbase::fData(ms)
    fd <- data.table::as.data.table(fd_df, keep.rownames = "rn")
    fd[,idx:=.I]
    idx <- fd[msLevel == 1 | msLevel == 2 & !is.na(precursorScanNum),idx]
    ms[idx]
    
}

clean_na_precs <- function(ms,missing_precursors) {
    fd <- data.table::as.data.table(MSnbase::fData(ms))
    na_pcs <- fd[msLevel==2 & is.na(precursorScanNum),.I]
    pc_na_info <- missing_precursors

    if (length(na_pcs)>0) {
            warning("There are MS2 spectra with missing precursor entries in your data. Consider setting missing_precursor_info to `fill', or `omit'.")
            message("Current missing_precursor_info value is: ", pc_na_info)
        }
        
        
    ms <- if (pc_na_info == "fill") {
             fix_na_precs(ms)
         } else if (pc_na_info == "omit") {
             omit_na_precs(ms)
         } else if (pc_na_info == "do_nothing") {
             ms
         } else {
             stop("Fatal: m$conf$missing_precursor_info value ",pc_na_info, "is not one of `fill', `omit', or `do_nothing`.")
         }

    ms
    
}

## Given summary and MS2 spectra table, generate MetFrag-friendly
## output.
add_msms_peaks <- function(summ,ms2) {
    ## Flatten spectra.
    flatspec <- ms2[,.(msms_peaks=paste(mapply(function(x,y) paste0(x,":",y),mz,intensity,USE.NAMES=F),collapse=";")),keyby=c('adduct','tag','ID','an')]
    ## Add msms_peaks column to the summary table in all the correct
    ## places.
    res <- flatspec[summ,on=c('adduct','tag','ID','an'),nomatch=NULL]
    cols <- names(res)
    cols1 <- setdiff(cols,"msms_peaks")
    newcols <- c(cols1,"msms_peaks")
    data.table::setcolorder(res,newcols)
    res
    

}

## Return a subset of dt table with rows matching kvals and columns
## the names of kvals contained in key(dt).
tabkey <- function(dt,kvals=list()) {
    if (length(kvals)==0L) return(dt)
    dtkeys <- key(dt)
    x <- as.list(rep(NA,(length(dtkeys))))
    names(x) <- dtkeys
    common <- intersect(dtkeys,names(kvals))
    x[common] <- kvals[common]
    x <- x[!is.na(x)]
    dt[(x),on=names(x),nomatch=NULL][,.SD,.SDcols=key(dt)]

}
