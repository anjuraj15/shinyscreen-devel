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
##     data.table::setkeyv(summ,c(BASE_KEY_MS2,"an"))
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

read_setid <- function(fn,cmpds) {
    assert(file.exists(fn),msg=paste("Please provide valid compounds set table:", fn))
    assert(nrow(cmpds) > 0,msg="Please provide at least one compounds list.")
    setid <- file2tab(fn,colClasses=c(ID="character"))
    x<-cmpds[setid,on='ID'][,.SD,.SDcols=c(colnames(setid),'known')]

    sids <- unique(setid$ID)
    cids <- unique(cmpds$ID)
    diff <- setdiff(sids,cids)
    assert(length(diff)==0,msg=paste("The following IDs from set table have not been found in the compound table:","------",print_table(dtable(diff)),"------",sep = "\n"))
    x
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

## PRESCREENING

create_qa_table <- function(extr,conf_presc) {
    ## The first input argument is the extracted `ms`, table
    ## containing MS1 and MS2 spectra. The argument `conf_presc` is
    ## m$conf$prescreen, the prescreening parameters given in the conf
    ## file.

    ## The qa table is just a copy of ms with added quality control
    ## columns QA_COLS.

    ## The QA_FLAGS columns are flags specifying which properties of
    ## compounds are known well, or not.

    ## For each compound (mass) we ask the following questions:
    ## qa_ms1_exists -- does the MS1 spectrum exist at all?
    ## qa_ms2_exists -- do we have any MS2 spectra at all?
    ## qa_ms1_above_noise -- is MS1 above the noise treshold?
    ## qa_ms2_near -- is there any MS2 spectrum inside the tolerated
    ## retention time window around the MS1 peak? That is, are we
    ## non-RT-shifted?
    ## qa_ms2_good_int -- Is there any MS2 spectral intensity greater
    ## than the MS2 threshold and less than the MS1 peak?
    ## qa_pass -- did the spectrum pass all the checks?
    

    ## The columns in QA_NUM_REAL are:
    ## 
    ## ms1_int -- the maximum intensity of MS1 spectrum over the
    ## entire run;
    ##
    ## ms1_rt -- the retention time of the peak MS1.

    ## The columns in QA_NUM_INT are:
    ##
    ## ms2_sel -- index of the selected MS2 spectrum; if not NA, the
    ## associated spectrum passed all the checks (qa_pass == T); the
    ## spectrum itself is in one of the member sublists of the `spec'
    ## column. The integer `ms2_sel' is then the index of the spectrum
    ## in that sublist.
    ##
    ## ms1_rt_ind -- TODO (but not important to end users).
    
    
    qa <- list(prescreen=conf_presc)
    
    ## checks <- extr$ms2[,{
    ## },keyby=BASE_KEY_MS2]
    checks <- extr$ms2
    checks[,(QA_FLAGS):=F]
    ## message("checks:")
    ## print(checks)
    ## message("done checks")
    checks[,(QA_NUM_INT):=NA_integer_]
    checks[,(QA_NUM_REAL):=NA_real_]
    setkeyv(checks,BASE_KEY_MS2)
    qa$checks <- checks
    qa
}

assess_ms1 <- function(m) {
    qa <- m$qa
    ms1 <- m$extr$ms1
    ## Calculate auxiliary variables and indices.
    qa_ms1 <- ms1[,.(ms1_rt_ind=which.max(intensity)),keyby=BASE_KEY]
    qa_ms1 <- ms1[qa_ms1,.(ms1_rt_ind=ms1_rt_ind,
                           ms1_int=intensity[[ms1_rt_ind]],
                           ms1_rt=rt[[ms1_rt_ind]],
                           ms1_mean=mean(intensity)),on=BASE_KEY,by=.EACHI]

    
    qa_ms1[,qa_ms1_good_int := ms1_int > qa$prescreen$ms1_int_thresh]
    qa_ms1[,qa_ms1_above_noise := F]
    qa_ms1[qa_ms1_good_int==T,qa_ms1_above_noise := .(ms1_int > qa$prescreen$s2n*ms1_mean)]
    
    
    

    
    ## checks[(!qa_ms1_above_noise),c("qa_ms2_good_int","qa_ms2_near","qa_ms2_exists","qa_pass"):=F]
    ## qa_ms1 <- check_ms1_noise(check_ms1(qa_ms1))
    m$qa$ms1 <- qa_ms1
    m
}

assess_ms2 <- function(m) {

    presconf <- conf_trans_pres(m$conf$prescreen)

    ms1 <- m$extr$ms1
    ms2 <- m$extr$ms2
    qa_ms1 <- m$qa$ms1
    qa_ms2 <- ms2[qa_ms1[qa_ms1_above_noise==T],.(CE=unique(CE),
                                                  pc_rt=i.ms1_rt,
                                                  pc_int=i.ms1_int,
                                                  an=unique(an)),on=BASE_KEY,by=.EACHI,nomatch=NULL]

    rt_win2 <- presconf$ret_time_shift_tol
    qa_ms2 <- ms2[qa_ms2,.(pc_rt=pc_rt,
                           pc_int=pc_int,
                           ms2_int=max(intensity),
                           ms2_rt=unique(rt),
                           qa_ms2_near=head(rt,1) < pc_rt + rt_win2 & head(rt,1) > pc_rt - rt_win2),
                  by=.EACHI,on=c(BASE_KEY_MS2,"an")]

    qa_ms2$qa_ms2_good_int <-F
    qa_ms2[qa_ms2_near==T,
           qa_ms2_good_int := ms2_int > presconf$ms2_int_thresh & ms2_int < pc_int,
           by=c(BASE_KEY_MS2,"an")]


    ## qa_ms2$qa_pass <- F
    ## qa_ms2[qa_ms2_good_int==T,qa_pass:=T]
    qa_ms2$ms2_sel <- F
    qa_ms2[qa_ms2_good_int==T,ms2_sel:={
        ind<-which.min(abs(ms2_rt-pc_rt))
        z<-ms2_sel
        z[[ind]]<-T
        z
    },by=BASE_KEY_MS2]
    setkeyv(qa_ms2,BASE_KEY_MS2)
    m$qa$ms2 <- qa_ms2
    m    
}

## Analyze extracted data.
analyse_extracted_data <- function(extr,prescreen_param) {
    ms1 <- extr$ms1
    ms2 <- extr$ms2
    ## Parameters.
    presconf <- conf_trans_pres(prescreen_param)
    rt_shift <- presconf$ret_time_shift_tol
    det_ms2_noise <- presconf$det_ms2_noise
    ms2_int_thresh <- presconf$ms2_int_thresh
    ms1_int_thresh <- presconf$ms1_int_thresh

    ## Detect MS2 noise.
    ms2_clc_ns <- if (det_ms2_noise) {
                      if (ms2_int_thresh>0) {
                          warning("Ignore user specified ms2_int_thresh value and autodetect noise instead.")
                      }
                      ms2[,ms2_thr:=0.33333333*mean(intensity),by="tag"]

                          
                  } else {
                      ms2[,ms2_thr:=ms2_int_thresh]
                      
                  }

    ## message('ms2_clc_ns:',key(ms2_clc_ns))
    ## ms2_clc_ns <- ms2_clc_ns[intensity>ms2_thr]


    ## We drop mz info.
    tab_ms2 <- ms2_clc_ns[,.(ms2_rt=first(rt),ms2_int=max(intensity),ms2_thr=first(ms2_thr)),by=c(BASE_KEY_MS2,'an')]
    tab_ms2[,qa_ms2_good_int:=ms2_int>ms2_thr,by="an"]
    data.table::setkeyv(tab_ms2,BASE_KEY_MS2)
    tab_ms2[,`:=`(rt_left = ms2_rt - rt_shift,rt_right = ms2_rt + rt_shift)]

    ## Get mean ms1 value.
    tab_ms1 <- extr$ms1
    tab_ms1_mean <- tab_ms1[,.(ms1_mean=mean(intensity)),keyby=BASE_KEY]

    ## This function extracts intensity maxima on intervals given by
    ## RT vectors rt_1 and rt_2.
    find_ms1_max <- function(rt,intensity,rt_1,rt_2)
    {
        mapply(function (rt_1,rt_2) {
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
            c(rt[pmax],intensity[pmax])
        }, rt_1, rt_2, USE.NAMES=F)
        
    }

    ## Perform MS1 maxima calculation in the neighbourhood of each
    ## MS2 result.
    tmp = tab_ms1[tab_ms2,{
        xx = find_ms1_max(rt,intensity,i.rt_left,i.rt_right)
        .(an=i.an,
          ms1_rt = xx[1,],
          ms1_int = xx[2,])
    },by=.EACHI, nomatch=NULL]
    ## Calculate QA values.
    tab_ms2[tmp,c("ms1_rt","ms1_int"):=.(i.ms1_rt,i.ms1_int),on=c(BASE_KEY,'an')]
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


    ## Find MS1 with no corresponding MS2.
    ms1key <- tab_ms1[,unique(.SD),.SDcol=BASE_KEY]
    ms2key <- tab_ms2[,unique(.SD),.SDcol=BASE_KEY]
    ms2key$mch=T
    tabmatches <- ms2key[ms1key]
    ms1woms2 <- tabmatches[is.na(mch)][,mch:=NULL]

    ## calculate the most intense peak, its location and the mean for
    ## childless MS1.
    tab_noms2 <- tab_ms1[ms1woms2,.(ms1_mean=mean(intensity),ms1_rt=rt[which.max(intensity)],ms1_int=max(intensity)),by=.EACHI,nomatch=NULL]

    ## QA for the above (lazy qa ... take only the max peak into account).
    tab_noms2[,c("qa_ms1_good_int","qa_ms1_above_noise"):=.(ms1_int>ms1_int_thresh,ms1_int>ms1_mean/3.)]

    ## MS2 QA criteria all fail.
    tab_noms2[,c("qa_ms2_exists","qa_ms2_good_int","qa_ms2_near"):=.(F,F,F)]

    ## Bind MS1-only and MS1/MS2 entries together.
    res <- rbind(tab_ms2,tab_noms2,fill=T,use.names=T)

    ## Every single entry which was extracted has at least MS1.
    res[,qa_ms1_exists:=T]
    data.table::setkeyv(res,BASE_KEY)
    
    qflg <- QA_FLAGS[!(QA_FLAGS %in% "qa_pass")]
    res[,qa_pass:=apply(.SD,1,all),.SDcols=qflg]
    res[.(T),del_rt:=abs(ms2_rt - ms1_rt),on="qa_pass",by='an']
    resby <- BASE_KEY_MS2[! (BASE_KEY_MS2 %in% 'an')]
    res[.(T),qa_tmp_ms1_max:= ms1_int==max(ms1_int),on="qa_pass",by=resby]
    res[,ms2_sel:=F]
    res[.(T,T),ms2_sel:= del_rt == del_rt[which.min(del_rt)],on=c("qa_pass","qa_tmp_ms1_max"),by=resby]
    res[,qlt_ms1:=apply(.SD,1,function(rw) sum(c(5L,3L,2L)*rw)),.SDcol=c("qa_ms1_exists",
                                                                 "qa_ms1_above_noise",
                                                                 "qa_ms1_good_int")]
    res[,qlt_ms2:=apply(.SD,1,function(rw) sum(c(5L,3L,2L)*rw)),.SDcol=c("qa_ms2_exists",
                                                                 "qa_ms2_near",
                                                                 "qa_ms2_good_int")]
    res
}

## Based on the `comprehensive' and `qa' tabs, greate `summ'.
gen_summ <- function(comp,qa) {
    comp_cols <- intersect(SUMM_COLS,colnames(comp))
    rdcomp <- comp[,..comp_cols]
    data.table::setkeyv(rdcomp,BASE_KEY)
    summ <- qa[rdcomp,nomatch=F] #We changed `nomatch' cases from NA
                                 #to F, because NA does not work well
                                 #with X == F condition.
    ## flgs <- c(QA_FLAGS,"ms2_sel")
    ## summ[is.na(qa_ms1_exists),(flgs):=F]
    data.table::setkeyv(summ,SUMM_KEY)
    summ[.(F),c("qlt_ms1","qlt_ms2"):=0.,on="qa_ms1_exists"]
    summ
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
