## Copyright (C) 2020 by University of Luxembourg

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

calc_mz_from_smiles <- function(smiles,adduct,id) {
    mol <- lapply(smiles,function(s) try(RMassBank::getMolecule(s), silent = T))
    check <- which(is.atomic(mol))
    if (length(check) > 0)
        stop("Errors in SMILES with IDs:",paste(id[which],collapse = ','))

    mol_form <- sapply(mol,function(x) (rcdk::get.mol2formula(x))@string,USE.NAMES = F)
    names(mol_form) <- id
    calc_mz_from_formula(mol_form,adduct,id)
    
    
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

gen_summ <- function(comp,qa_ms1,qa_ms2) {
    comp_cols <- intersect(SUMM_COLS,colnames(comp))
    summ <- comp[,..comp_cols]
    data.table::setkeyv(summ,BASE_KEY)
    ms1_cols <- intersect(SUMM_COLS,colnames(qa_ms1))
    ms1_cols <- setdiff(ms1_cols,colnames(summ))
    summ <- qa_ms1[summ,c(..comp_cols,..ms1_cols),on=BASE_KEY]
    ms2_cols <- intersect(colnames(qa_ms2),SUMM_COLS)
    ms2_cols <- setdiff(ms2_cols,colnames(summ))
    summ <- qa_ms2[summ,c(..comp_cols,..ms1_cols,..ms2_cols),on=BASE_KEY]
    data.table::setkeyv(summ,c(BASE_KEY_MS2,"an"))
    summ[,qa_ms1_exists:=the_ifelse(!is.na(qa_ms1_good_int),T,F)]
    summ[,qa_ms2_exists:=the_ifelse(!is.na(CE),T,F)]
    summ[,qa_pass:=apply(.SD,1,all),.SDcols=QA_FLAGS[!(QA_FLAGS %in% "qa_pass")]]
    summ$Comments<-""
    data.table::setkeyv(summ,DEF_KEY_SUMM)
    data.table::setcolorder(summ,SUMM_COLS)
    summ
}
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

smiles2img <- function(smiles, kekulise=TRUE, width=300, height=300,
                       zoom=1.3,style="cow", annotate="off", abbr="on",suppressh=TRUE,
                       showTitle=FALSE, smaLimit=100, sma=NULL) {
    dep <- rcdk::get.depictor(width = width, height = height, zoom = zoom, style = style, annotate = annotate,
                              abbr = abbr, suppressh = suppressh, showTitle = showTitle, smaLimit = smaLimit,
                              sma = NULL)

    mol <- RMassBank::getMolecule(smiles)
    z<-rcdk::view.image.2d(mol, depictor=dep)
    grid::rasterGrob(z)
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





plot_id_msn <- function(ni,
                        data,
                        rtMS1,
                        rtMS2,
                        iMS2rt,
                        mass,
                        smile,
                        tags,
                        summ,
                        prop,
                        theme,
                        pal="Dark2",
                        cex=0.75,
                        rt_digits=2,
                        m_digits=4) {

    clean_range<-function(def,rng) {
        x1 <- rng[1]
        x2 <- rng[2]
        if (is.na(x1) || x1 == 0) x1 <- def[1]
        if (is.na(x2) || x2 == 0) x2 <- def[2]
        c(x1,x2)
    }



    
    mk_title<-function() paste("EIC (",
                               "m/z = ",
                               formatC(mass,format='f',digits=m_digits),
                               ")",sep='')
    mk_leg_lab<-function(tag,rt) {paste(tag,"; rt= ",formatC(rt[[tag]],format='f',digits=rt_digits),"min")}


    i<-name2id(ni)


    dfChrMS1<-NULL
    dfChrMS2<-NULL
    dfSpecMS2<-NULL

    ## MS1 time series.
    dfschrms1<-lapply(tags,function(tag) {d<-data[[tag]]$eic
        ind<-match(ni,MSnbase::fData(d)[["ID"]])
        cg<-d[[ind]]
        data.frame(rt=MSnbase::rtime(cg)/60.,
                   intensity=MSnbase::intensity(cg),tag=as.character(tag),legend=mk_leg_lab(tag,rtMS1))
    })
    
    dfChrMS1<-do.call(rbind,c(dfschrms1,list(make.row.names=F)))


    ## MS2 spectral time series.
    dfsChrMS2<-lapply(tags,function(tag) {
        d<-data[[tag]]$ms2[[ni]]
        if (!is.null(d)) {
            df<-MSnbase::fData(d)[,c("rtm","maxI")]
            colnames(df)<-c("rt","intensity")
            df$tag<-as.character(tag)
            df$legend=mk_leg_lab(tag,rtMS2)
            df
        } else NULL
    })
    
    dfsChrMS2<-dfsChrMS2[!is.null(dfsChrMS2)]
    if (!all(sapply(dfsChrMS2,is.null))) dfChrMS2<-do.call(rbind,c(dfsChrMS2,list(make.row.names=F)))

    ## MS2 Spectrum.
    if (!all(sapply(dfsChrMS2,is.null))) {
        dfsSpecMS2<-lapply(tags,function(tag) {
            d<-data[[tag]]$ms2[[ni]]
            if (!is.null(d)) {
                ind<-iMS2rt[[tag]]
                if (!is.na(ind)) {
                    x<-data.frame(mz=MSnbase::mz(d[[ind]]),intensity=MSnbase::intensity(d[[ind]]))
                    x$tag<-tag
                    x
                } else NULL
                
            }
        })
        dfsSpecMS2<-dfsSpecMS2[!is.null(dfsSpecMS2)]
        dfSpecMS2<-do.call(rbind,c(dfsSpecMS2,list(make.row.names=F)))
    }


    ## Ranges
    if (!is.null(dfChrMS1)) {
        rrtMS1<-range(dfChrMS1$rt)
        rrtMS1 <- if (is.null(prop$ms1$rt))  rrtMS1 else clean_range(rrtMS1,prop$ms1$rt)
        rrtMS2<-rrtMS1
        
        rintMS1<-range(dfChrMS1$intensity)
        rintMS1 <- if (is.null(prop$ms1$irng))  rintMS2 else clean_range(rintMS1,prop$ms1$irng)
    }

    if (!is.null(dfChrMS2)) {
        rrtMS2 <- if (is.null(prop$ms2$rt))  rrtMS2 else clean_range(rrtMS2,prop$ms2$rt)
        rintMS2<-range(dfChrMS2$intensity)
        rintMS2 <- if (is.null(prop$ms2$irng))  rintMS2 else clean_range(rintMS2,prop$ms2$irng)
    }

    if (is.data.frame(dfSpecMS2)) {
        rmzSpMS2<-range(dfSpecMS2$mz)
        rintSpMS2<-range(dfSpecMS2$intensity)
        rmzSpMS2<- if (is.null(prop$spec$mzrng))  rmzSpMS2 else clean_range(rmzSpMS2,prop$spec$mzrng)
        rintSpMS2<- if (is.null(prop$spec$irng)) rintSpMS2 else clean_range(rintSpMS2,prop$spec$irng)
    }

    ch_ms1_deco<-function(ggobj) {
        titMS1<-mk_title()
        scale_y<-if (!prop$ms1$axis=="log") {
                     ggplot2::scale_y_continuous
                 } else {
                     ggplot2::scale_y_log10
                 }
        
        ggobj+
            ggplot2::geom_line(ggplot2::aes(colour=legend),key_glyph=KEY_GLYPH)+
            ggplot2::coord_cartesian(xlim = rrtMS1,
                                     ylim = rintMS1)+
            ggplot2::labs(x=CHR_GRAM_X,y=CHR_GRAM_Y,
                          title=titMS1,tag=i,
                          colour=PLOT_MS1_LEG_TIT)+
            scale_y(labels=sci10)+theme()
    }

    ch_ms2_deco<-function(ggobj) {
        scale_y<-if (!prop$ms2$axis=="log") {
                     ggplot2::scale_y_continuous
                 } else {
                     ggplot2::scale_y_log10
                 }
        ggobj+
            ggplot2::geom_linerange(ggplot2::aes(colour=legend),key_glyph=KEY_GLYPH)+
            ggplot2::coord_cartesian(xlim = rrtMS2,
                                     ylim = rintMS2)+
            ggplot2::labs(x=CHR_GRAM_X,y=CHR_GRAM_Y,title=NULL,subtitle = "MS2",tag = "   ")+
            scale_y(labels=sci10)+

    ggplot2::labs(colour=PLOT_MS2_LEG_TIT)+theme()

    }

    ch_spec_deco<-function(ggobj) {
        scale_y<-if (!prop$spec$axis=="log") {
                     ggplot2::scale_y_continuous
                 } else {
                     ggplot2::scale_y_log10
                 }
        
        ggobj+
            ggplot2::geom_linerange(ggplot2::aes(colour=tag),key_glyph=KEY_GLYPH)+
            ggplot2::coord_cartesian(xlim = rmzSpMS2,
                                     ylim = rintSpMS2)+
            ggplot2::labs(subtitle="MS2",y="intensity")+
            scale_y(labels=sci10)+theme()
    }
    

    ## MS1 time series.
    plMS1<- if(is.data.frame(dfChrMS1) && nrow(dfChrMS1)>0) {
                ch_ms1_deco(ggplot2::ggplot(data=dfChrMS1,ggplot2::aes(x=rt,y=intensity,group=legend)))
            } else NULL

    ## Empty
    plEmpty<-ggplot2::ggplot(data=dfChrMS1,ggplot2::aes(x=rt,y=intensity))+ggplot2::theme_void()


    ## MS2 time series.
    plMS2 <- if (!all(sapply(dfsChrMS2,is.null))) {
                 ch_ms2_deco(ggplot2::ggplot(data=dfChrMS2,ggplot2::aes(x=rt,ymin=0,ymax=intensity,group=legend)))
             } else plEmpty


    

    

    ## Structure
    if (!is.null(smile) && !is.na(smile) && !nchar(smile)<1) {
        g<-smiles2img(smile,width=500,height=500,zoom=4.5)
        plStruc<-ggplot2::ggplot(data=dfChrMS1,ggplot2::aes(x=rt,y=intensity))+
            ggplot2::geom_blank()+ggplot2::annotation_custom(g)+ggplot2::theme_void()
    } else plStruc<-plEmpty


    ## MS2 Spectrum
    if (!all(sapply(dfsChrMS2,is.null))) {
        plSpecMS2<-if (is.data.frame(dfSpecMS2)) { #sometimes
                                        #dfSpecMS2 ends up
                                        #as a list of
                                        #logicals; this
                                        #probably happens
                                        #when either MS2 is
                                        #bad in some way,
                                        #or the RT
                                        #intervals are
                                        #mismatched.
                       ch_spec_deco(ggplot2::ggplot(data=dfSpecMS2,
                                                    ggplot2::aes(x=mz,
                                                                 ymin=0,
                                                                 ymax=intensity,group=tag)))

                       
                   } else plEmpty
    } else plSpecMS2<-plEmpty

    ## Lucky N the most intense N TODO
    ## lckN<-if (is.data.frame(dfSpecMS2)) {
    ##           ord<-order(dfSpecMS2$intensity,decreasing=T)
    ##           ll<-length(ord)
    ##           theL<-min(ll,MS2_1ST_N)
    ##           mzN<-dfSpecMS2$mz[ord][1:theL]
    ##           inN<-dfSpecMS2$intensity[ord][1:theL]
    ##           df<-data.frame("m/z"=mzN,"intensity"=inN)
    ##           message("DF:")
    ##           str(df)
    ##           message("---DF")
    ##           gridExtra::tableGrob(df) #+ggplot2::labs(subtitle="Top m/z")
    
    ##       } else NULL

    res<- if (!is.null(plMS1)) cowplot::plot_grid(plMS1,plStruc,plMS2,plEmpty,plSpecMS2,align = "hv",axis='l',ncol = 2,nrow=3,rel_widths=c(3,1)) else NULL
    
    res
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


write_conf <- function(m,fn) {
    m$conf$data <- get_fn_ftab(m)
    if (NROW(m$input$tab$mzml)>0) tab2file(tab=m$input$tab$mzml,file=file.path(m$conf$project,FN_DATA_TAB))
    yaml::write_yaml(x=m$conf,file=fn)
    
    
    
}
write_state <- function(m,fn_conf) {
    write_conf(m,fn_conf)
    tab2file(tab=m$input$tab$mzml,file=file.path(m$conf$project,FN_DATA_TAB))
}

read_conf <- function(fn) {
    cf <- yaml::yaml.load_file(fn)
    fnl <- cf$compound$lists
    if (length(fnl)>0) {
        nms <- character(0)
        for (i in 1:length(fnl)) {
            nms <- gen_uniq_lab(nms,pref = 'L')
        }
        names(fnl) <- nms
        
    }
    cf$compound$lists <- fnl
    ## conf_trans(cf)
    cf
}



##' @export
get_fn_comp <- function(m) {
    file.path(m$conf$project,FN_COMP_TAB)
}

##' @export
get_fn_summ <- function(m) {
    file.path(m$conf$project, FN_SUMM)
}

##' @export
get_fn_extr <- function(m) {
    file.path(m$conf$project, "extracted.rds")
}

##' @export
get_fn_conf <- function(m) {
    file.path(m$conf$project, FN_CONF)
}


##' @export
get_fn_ftab <- function(m) {
    file.path(m$conf$project, FN_DATA_TAB)
}

init_state <- function(m) {
    m$out$tab <- list()
    m$input$datafiles <- NULL
    m$input$tab$mzml <- EMPTY_MZML
    lab <- gen_uniq_lab(list(),pref="L")
    m$input$tab$lists <- list()
    m$input$tab[[lab[[1]]]] <- EMPTY_CMPD_LIST

    m$out$tab$ms1_plot <- EMPTY_MS1_PLOT_TAB
    m$out$tab$ms2_plot <- EMPTY_MS2_PLOT_TAB
    m$out$tab$comp <- EMPTY_COMP_TAB
    m
}

base_conf <- function () {
    m <- list()
    m$conf <- list(project=getwd(),
                   compounds=list(lists=list(),
                                  sets="",
                                  data=""),
                   extr=list(fn=""),
                   debug = F)
    m
}

extr_conf <- function(m) {
    m$conf$tolerance <- list("ms1 coarse"=MS1_ERR_COARSE,
                             "ms1 fine"=MS1_ERR_FINE,
                             "eic"=EIC_ERR,
                             "rt"=RT_EXTR_ERR)
    m
}

presc_conf <- function(m) {
    m$conf$prescreen <- list("ms1_int_thresh"=1e5,
                             "ms2_int_thresh"=2.5e3,
                             "s2n"=3,
                             "ret_time_shift_tol"=0.5)
    m
}


new_conf <- function() presc_conf(
                           extr_conf(
                               base_conf()))



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
    xs <- grab_unit(entry,"s")
    xm <- grab_unit(entry,"min")
    x <- if (is.na(xm)) xs/60. else xm
    x
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
    
    checks <- extr$ms2[,{
        z <-..QA_FLAGS
        z[1:length(z)]<-F
        names(z)<-..QA_FLAGS
        z
    },keyby=BASE_KEY_MS2]
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



gen_base_ms1_plot_tab <- function(summ,ms1_spec) {
    
    ident <- c("set",
               "adduct",
               "tag",
               "ID",
               "mz",
               "file")
    
    res <- summ[ms1_spec,c(.SD,
                           list(rt_peak=i.ms1_rt,
                                eicMS1=lapply(i.eicMS1,list))),
                .SDcols=ident,
                on=BASE_KEY,
                nomatch=NULL]
    setkeyv(res,cols=BASE_KEY)
    res
}

gen_base_ms2_plot_tab <- function(summ,ms2_spec) {
    ident <- c("set",
               "adduct",
               "tag",
               "ID",
               "mz",
               "file")
    res <- summ[ms2_spec,c(.SD,
                           list(CE=i.CE,
                                rt_peak = i.rt,
                                int_peak = ms2_max_int,
                                spec = i.spec,
                                ms2_sel = i.ms2_sel)),
                .SDcols=ident,
                on=BASE_KEY,
                nomatch=NULL]
    setkeyv(res,cols=BASE_KEY)
    res
}

## Format the y labels.
sci10 <- function(x) {
    prefmt <- formatC(x,format="e",digits=2)
    bits <- strsplit(prefmt,split="e")
    bits1 <-sapply(bits,function(x) {
        if (length(x) > 1) {
            res <- x[[1]]
            sub(" ","~",res)
        } else {
            x
        }
    })
    bits2 <-sapply(bits,function(x) if (length(x)>1) paste0(" %*% 10^","'",sub("[+]"," ",x[[2]]),"'") else "")
    txt <- mapply(function(b1,b2) if (nchar(b2)!=0) {paste0("'",b1,"'",b2)} else NA,
                  bits1,
                  bits2,
                  SIMPLIFY = F)
    names(txt) <- NULL
    txt <- gsub(pattern = "^'0\\.00'.*$","  0",x=txt)
    parse(text=txt)
    
    

}

sci10_old <- function(x) {
    if (length(x)!=0) {
        x<-sapply(x,function(x) if(x!=0) x else "0")
        ifelse(x==0,"0",{
            prefmt <- formatC(x,format="e",digits=2)
            bits <- strsplit(prefmt,split="e")
            bits1 <-sapply(bits,function(x) {
                if (length(x) > 1) {
                    res <- x[[1]]
                    sub(" ","~",res)
                } else {
                    x
                }
            })
            print(bits1)
            bits2 <-sapply(bits,function(x) if (length(x)>1) paste0(" %*% 10^",sub("[+]","~",x[[2]])) else "")
            txt <- mapply(function(b1,b2) if (nchar(b2)!=0) {paste0(b1,b2)} else NA,
                                 bits1,
                                 bits2,
                          SIMPLIFY = F)
            names(txt) <- NULL
            message("---------")
            print(txt)
            message("________")
            ## parse(text=txt)})
            txt})
        
     } else ""
}

plot_theme <- function (legend.position="none",...)
    ggplot2::theme(
                 plot.margin = unit(c(0,0,0,0),"cm"),
                 legend.position = legend.position,
                 axis.text = ggplot2::element_text(size=ggplot2::rel(1.2)),
                 axis.title = ggplot2::element_text(size=ggplot2::rel(1.2)),
                 ...)

plot_decor <- function(m,islog,all_ms1_labels,legend_name_ms1,legend_name_ms2="CE",all_ms2_labels=NULL,
                       ms1_legend_info=T) {
    textf <- ggplot2::element_text

    my_theme <- plot_theme(legend.position="bottom",legend.box="horizontal")
    ## Logarithmic, or linear y axis?
    scale_y <- if (shiny::isTruthy(islog))
                   ggplot2::scale_y_log10 else ggplot2::scale_y_continuous


    getpal <- colorRampPalette(RColorBrewer::brewer.pal(8,"Dark2"))
    
    col_all_vals <- getpal(length(all_ms1_labels))
    names(col_all_vals) <- all_ms1_labels
    
    scale_colour <- if (ms1_legend_info) {
                        function(breaks, labels, ...) ggplot2::scale_colour_manual(values = col_all_vals,
                                                                                   breaks = breaks,
                                                                                   labels = labels,
                                                                                   name = legend_name_ms1,...)
                    } else {
                        function(breaks=NULL, labels=NULL, ...) NULL
                    }

    
    shape_all_vals <- 1:length(all_ms2_labels)
    scale_ms2 <- if (length(shape_all_vals)>0) {
                     names(shape_all_vals) <- all_ms2_labels
                     function(breaks, labels, ...)  ggplot2::scale_shape_manual(values = shape_all_vals,
                                                                                breaks = breaks,
                                                                                labels = labels,
                                                                                name = legend_name_ms2, ...)
                 } else {
                     function(breaks=NULL, labels=NULL, ...) NULL
                     
                     
                 }
    
    

    function(plot, breaks, labels, ms2_breaks=NULL, ms2_labels=NULL) {
        plot +
            scale_colour(breaks=breaks,
                         labels=labels) +
            scale_ms2(breaks=ms2_breaks,
                      labels=ms2_labels) +
            scale_y(labels=sci10) + my_theme
    } 
}


gen_get_ms2_legend <- function(m,legend_name_ms2="CE",all_ms2_labels) {
    shape_all_vals <- 1:length(all_ms2_labels)
    scale_ms2 <- if (length(shape_all_vals)>0) {
                     names(shape_all_vals) <- all_ms2_labels
                     function(breaks, labels, ...)  ggplot2::scale_shape_manual(values = shape_all_vals,
                                                                                breaks = breaks,
                                                                                labels = labels,
                                                                                name = legend_name_ms2, ...)
                 } else {
                     function(breaks=NULL, labels=NULL, ...) NULL
                     
                     
                 }
    function(plot,breaks,labels) {
        thing <- plot + scale_ms2(breaks=breaks,
                                  labels=labels)

        cowplot::get_legend(thing)
        
    }
}


plot_eic_ms1_df <- function(df,style_fun,plot_label) {
    mk_leg_lab<-function(tag,rt) {if (length(tag) > 0) paste(tag,"; rt= ",formatC(rt,format='f',digits=RT_DIGITS)," min",sep='') else character(0)}

    ## mz <- df[,unique(prec_mz)]
    tbl <- df[,.(verb_labs=mk_leg_lab(get(..plot_label),rt_peak),plot_label=get(..plot_label)),
              by=c(plot_label,"rt_peak")]
    
    verb_labs <- tbl[,verb_labs]
    labs <- tbl[,plot_label]
    df[,plot_label:=factor(get(..plot_label))]
    style_fun(ggplot2::ggplot(df,ggplot2::aes(x=rt,y=intensity,colour=plot_label)),
              breaks=labs,
              labels=verb_labs) +
        ggplot2::geom_line(key_glyph=KEY_GLYPH) +
        ggplot2::labs(x=CHR_GRAM_X,
                      y=CHR_GRAM_Y)
}

plot_eic_ms2_df <- function(df,style_fun) {
    mz <- df[,unique(prec_mz)]
    ddf <- df[!is.na(rt_peak)==T]
    

    ms2_labs <- ddf[,plot_label]
    ms1_labs <- ddf[,levels(parent_label)]
    
    res <- if (NROW(ddf)>0) {
               plot <- style_fun(ggplot2::ggplot(ddf,ggplot2::aes(x = rt_peak,ymin = 0,ymax = int_peak,
                                                                  y = int_peak,
                                                                  color = parent_label, shape = plot_label)),
                                 breaks=ms1_labs,
                                 labels=ms1_labs,
                                 ms2_breaks=ms2_labs,
                                 ms2_labels=ms2_labs)
               plot + ggplot2::geom_linerange(key_glyph=KEY_GLYPH) +
                   ggplot2::geom_point() +
                   ggplot2::labs(x=CHR_GRAM_X,
                                 y=CHR_GRAM_Y)
           } else {
               p <- ggplot2::ggplot(ddf,ggplot2::aes(x=1:10,y=1:10))+ggplot2::geom_blank()+ggplot2::labs(x="",y="")
               p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="NO MS2 SPECTRA", color="black")+ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                                                                                                                          axis.ticks.x=ggplot2::element_blank(),
                                                                                                                          axis.text.y=ggplot2::element_blank(),
                                                                                                                          axis.ticks.y=ggplot2::element_blank())
           }

    res
}

plot_spec_ms2_df <- function(df,style_fun) {
    
    ms2_labs <- df[,levels(plot_label)]
    ms1_labs <- df[,levels(parent_label)]

    plot <- if (NROW(df)>0) {
                ddf <- df[,.(mz,intensity,parent_label,plot_label)]
                plot <-style_fun(ggplot2::ggplot(ddf,
                                                 ggplot2::aes(x=mz,ymin=0,ymax=intensity,
                                                              y = intensity,
                                                              color=parent_label,
                                                              shape=plot_label)),
                                 labels=ms1_labs,
                                 breaks=ms1_labs,
                                 ms2_breaks=ms2_labs,
                                 ms2_labels=ms2_labs)

                plot + ggplot2::geom_linerange(key_glyph=KEY_GLYPH) +
                    ggplot2::geom_point() +
                    ggplot2::labs(x="mz", y="intensity")
        
            } else {
                ddf <- data.table(x=1:10,y=1:10)
                p <- ggplot2::ggplot(ddf,ggplot2::aes(x=x,y=y))+ggplot2::geom_blank()+ggplot2::labs(x="",y="")
                p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="NO MS2 SPECTRA", color="black")+ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                                                                                                                           axis.ticks.x=ggplot2::element_blank(),
                                                                                                                           axis.text.y=ggplot2::element_blank(),
                                                                                                                           axis.ticks.y=ggplot2::element_blank())

                
            }

    plot
    

}

plot_leg_ms2 <- function(df,style_fun) {
    mz <- df[,unique(mz)]
    ddf <- df[!is.na(rt_peak)==T]
    
    mk_leg_lab<-function(tag,rt,have_sel) {if (length(tag) > 0 && have_sel) paste(tag,"; rt= ",formatC(rt,format='f',digits=RT_DIGITS)," min",sep='') else if (!have_sel) tag  else character(0)}
    tbl <- ddf[,.(verb_labs=mk_leg_lab(plot_label,.SD[ms2_sel==T,rt_peak],any(ms2_sel)),plot_label),
               by="plot_label"]
    ms2_verb_labs <- tbl[,verb_labs]
    ms2_labs <- tbl[,plot_label]
    ms1_labs <- ddf[,levels(parent_label)]
    blah <- ggplot2::ggplot(ddf,ggplot2::aes(shape = plot_label,y=int_peak,x=rt_peak)) + ggplot2::geom_point()
    
    
    plot <- style_fun(blah,
                      breaks=ms1_labs,
                      labels=ms1_labs,
                      ms2_breaks=ms2_labs,
                      ms2_labels=ms2_verb_labs)
    cowplot::get_legend(plot)

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



get_plot_data <- function(plot_index,plot_label,
                          summ_tab,summ_cols,extr_tab=NULL,
                          extr_cols=NULL) {
    thenames<-ifelse(nchar(names(summ_cols))!=0,names(summ_cols),summ_cols)
    names(summ_cols)<-thenames

    ind_nms <- names(plot_index)
    plot_group <- ind_nms[[1]]
    plot_plot <- ind_nms[[2]]
    
    
    meta <- summ_tab[get(ind_nms[[1]]) == plot_index[[1]] &
                     get(ind_nms[[2]]) == plot_index[[2]],
                     unique(.SD),.SDcols=c(plot_label,summ_cols)]

    data.table::setkeyv(meta,plot_label)
    data.table::setnames(meta,summ_cols,names(summ_cols))

    if (!is.null(extr_tab)) {
        data_cols <- c(plot_label,extr_cols)
        data <- extr_tab[get(ind_nms[[1]]) == plot_index[[1]] &
                         get(ind_nms[[2]]) == plot_index[[2]],..data_cols]
        return(meta[data,on=plot_label])
    } else meta

}


get_ms1_chr_pdata <- function(m,plot_index) get_plot_data(m$out$tab$summ,
                                                      c("mz",
                                                        rt_peak="ms1_rt"),
                                                      m$extr$ms1,
                                                      extr_cols = c("rt","intensity"),
                                                      plot_index = plot_index,
                                                      plot_label = m$conf$figures$grouping$label)

get_ms2_chr_pdata <- function(m,plot_index) {
    z<- get_plot_data(plot_index = plot_index,
                      plot_label = c(m$conf$figures$grouping$label,"an"),
                      summ_tab = m$out$tab$summ,
                      summ_cols = c(prec_mz="mz",
                                    rt_peak="ms2_rt",
                                    int_peak="ms2_int",
                                    "CE",
                                    "ms2_sel"))
    z$plot_label =  factor(z$CE)
    z$parent_label = factor(z[[m$conf$figures$grouping$label]])
    z
}

get_ms2_spec_pdata <- function(m,plot_index) {
    z <- get_plot_data(plot_index = plot_index,
                       plot_label = c(m$conf$figures$grouping$label,"an"),
                       summ_tab = m$out$tab$summ[ms2_sel == T,],
                       summ_cols = c(prec_mz="mz",
                                     rt_peak="ms2_rt",
                                     int_peak="ms2_int",
                                     "CE",
                                     "ms2_sel"),
                       extr_tab = m$extr$ms2,
                       extr_cols = c("mz","intensity"))
    z$plot_label =  factor(z$CE)
    z$parent_label = factor(z[[m$conf$figures$grouping$label]])
    z
}

##' @export
plot_ms1_chr <- function(m,plot_index) {
    pdata <- get_ms1_chr_pdata(m,plot_index)

    if (NROW(data) < 0 ) {
        p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                             ggplot2::aes(x=x,y=y))+
            ggplot2::geom_blank()+ggplot2::labs(x="",y="")

        p <- p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="NO MS1 SPECTRA", color="black")+
            ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                           axis.ticks.x=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.ticks.y=ggplot2::element_blank())
        return(p)
    } else
        
        group_data <- m$conf$figures$grouping
    plot_group <- names(plot_index)[[1]]
    plot_plot <- names(plot_index)[[2]]
    plot_label <- group_data$label

    all_labels <- m$out$tab$flt_summ[,sort(unique(get(..plot_label)))]

    style <- plot_decor(m,m$conf$logaxes$ms1_eic_int,
                        all_ms1_labels=all_labels,
                        legend_name_ms1=plot_label)

    plot_eic_ms1_df(pdata,
                    style_fun = style,
                    plot_label = plot_label)

    
    
}


##' @export
plot_ms2_chr <- function(m,plot_index) {
    pdata <- get_ms2_chr_pdata(m,plot_index)

    if (NROW(data) < 0 ) {
        p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                             ggplot2::aes(x=x,y=y))+
            ggplot2::geom_blank()+ggplot2::labs(x="",y="")

        p <- p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="NO MS2 SPECTRA", color="black")+
            ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                           axis.ticks.x=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.ticks.y=ggplot2::element_blank())
        return(p)
    } else
        
    group_data <- m$conf$figures$grouping
    plot_group <- names(plot_index)[[1]]
    plot_plot <- names(plot_index)[[2]]
    plot_ms1_label <- group_data$label

    all_ms1_labels <- m$out$tab$summ[,sort(unique(get(plot_ms1_label)))]
    all_ms2_ce_labels <- m$out$tab$summ[,sort(na.omit(unique(CE)))]

    style <- plot_decor(m,m$conf$logaxes$ms2_eic_int,
                        all_ms1_labels = all_ms1_labels,
                        all_ms2_labels = all_ms2_ce_labels,
                        legend_name_ms1 = plot_ms1_label,
                        legend_name_ms2 = "CE")

    plot_eic_ms2_df(pdata, style_fun = style)

    
    
}


##' @export
plot_ms2_spec <- function(m,plot_index) {
    pdata <- get_ms2_spec_pdata(m,plot_index)
    if (NROW(data) < 0 ) {
        p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                             ggplot2::aes(x=x,y=y))+
            ggplot2::geom_blank()+ggplot2::labs(x="",y="")

        p <- p + ggplot2::annotate(geom="text", x=5, y=5, size=6, label="NO MS2 SPECTRA", color="black")+
            ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                           axis.ticks.x=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.ticks.y=ggplot2::element_blank())
        return(p)
    } else
        
    group_data <- m$conf$figures$grouping
    plot_group <- names(plot_index)[[1]]
    plot_plot <- names(plot_index)[[2]]
    plot_ms1_label <- group_data$label

    all_ms1_labels <- m$out$tab$summ[,sort(unique(get(plot_ms1_label)))]
    all_ms2_ce_labels <- m$out$tab$summ[,sort(na.omit(unique(CE)))]

    style <- plot_decor(m,m$conf$logaxes$ms2_spec_int,
                        all_ms1_labels = all_ms1_labels,
                        all_ms2_labels = all_ms2_ce_labels,
                        legend_name_ms1 = plot_ms1_label,
                        legend_name_ms2 = "CE")


    plot_spec_ms2_df(pdata, style_fun = style)

    
    
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
        ## grid::grid.draw(gridExtra::arrangeGrob(m$out$tab$structfig[ID==id,img][[1]]))
        grid::grid.draw(m$out$tab$structfig[ID==id,img][[1]],
                        recording = F)

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
        m$out$tab$structfig[ID==id,img][[1]]
    }
    
}


##' @export
gen_key_plot_tab <- function(m) {


    fltsumm <- m$out$tab$flt_summ
    validate(need(NROW(fltsumm) > 0,
                  message = "Generate summary table first."))
    
    
    plot_group <- m$conf$figures$grouping$group
    plot_plot <- m$conf$figures$grouping$plot
    plot_label <- m$conf$figures$grouping$label
    plot_key <- c(plot_group,plot_plot)

    
    idx <- fltsumm[,{
        lapply(.SD,
               function (col) {
                   val <- unique(col)
                   if (length(val)<=1) T else F
               })
        
    },
    by=c(plot_group,plot_plot)]
    idxlst <- as.logical(idx[,lapply(.SD,function (col) all(col))])
    nmidx <- colnames(idx)
    cols <- nmidx[idxlst]
    cols <- na.omit(setdiff(cols,plot_key))
    fltsumm[,unique(.SD[,..cols]),
            by=plot_key]

}
