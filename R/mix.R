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

get_mz_cmp_l<-function(id,mode,cmpL) {
    ind<-match(id,cmpL$ID)
    mz<-cmpL$mz[[ind]]
    smiles<-cmpL$SMILES[[ind]]
    res<-if (!is.null(mz) && !is.na(mz)) {
             mz
         } else if (nchar(smiles)>0)
         {
             mde<-as.character(mode)
             wh<-MODEMAP[[mde]]
             RChemMass::getSuspectFormulaMass(smiles)[[wh]]
         } else stop("Both SMILES and mz fields, for ID ",id,", found empty in the compound list. Aborting.")
    res
}

get_col_from_cmp_l<-function(id,cname,cmpL) {
    ind<-match(id,cmpL$ID)
    x<-cmpL[[cname]][[ind]]
    if (!is.null(x)) x else NA
    
}

gen_clean_state_ftab<-function(ftable) {
    ftable$Comments <- ""
    ftable[c("MS1","MS2","Alignment","AboveNoise")] <- T
    ftable["MS2rt"] <- NA_real_
    ftable["iMS2rt"] <- NA_integer_
    ftable["rt"]<-NA_real_
    ftable["checked"]<-'NONE'
    ftable
}

pp_touch_q<-function(ftab) {
    ## Returns indices that are ok to be auto processed.
    which(ftab$checked==FTAB_CHK_NONE | ftab$checked==FTAB_CHK_AUTO)
}

preProc <- function (ftable,noiseFac=3,errRT=0.5,intThreshMS1=1e5,intThreshMS2=5000.) {
    wds<-unique(ftable$wd)
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

    okinds<- pp_touch_q(ftable)
    for (ind in okinds) {
        wd <- ftable$wd[ind]
        id <- ftable$ID[ind]
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
        ftable[ind,"rt"]<-eic$rt[[ms1MaxInd]]
        ##If MS1 does not exist, set entry to F.
        if (maxInt < intThreshMS1) {
            ftable[ind,"MS1"] <- F
            ## Other checks automatically fail, too.
            ftable[ind,"Alignment"] <- F
            ftable[ind,"AboveNoise"] <- F
        } else {
            ## Noisy?
            if (ftable[ind,"AboveNoise"]) {
                mInt <- mean(eic$intensity)
                if (maxInt < noiseFac*mInt) {
                    ftable[ind,"AboveNoise"] <- F
                    ftable[ind,"Alignment"] <- F ## If noisy, this is
                                                 ## probably meaningles, so
                                                 ## F.
                }
                
            }
        }
        
    

        ## MS2 checks.
        ms2<-allData[[wd]]$ms2
        ms2nids<-names(ms2)
        if (! (nid %in% ms2nids)) {
            ftable[ind,"MS2"] <- F
            ftable[ind,"Alignment"] <- F
        } else {
            sp<-ms2[[nid]]
            ## Alignment still makes sense to be checked?
            if (ftable[ind,"Alignment"]) {
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
                    ftable[ind,"iMS2rt"] <- rtInd[msmsRTind]
                    ftable[ind,"MS2rt"] <- msmsRT[msmsRTind]
                } else {
                    ftable[ind,"Alignment"] <- F
                }
            }
        }
        ftable[ind,"checked"]<-FTAB_CHK_AUTO
    }
          
    ftable
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

gen_ms2_spec_fn <- function(id,tag,mode,set,width=6) {
    suppressWarnings({
        iid<-as.numeric(id)
        iid<- if (!is.na(iid)) iid else id
        num <- formatC(iid,width = width,format='d',flag='0')
        ss<-trimws(paste(num,mode,tag,set,sep="_"),which='both')
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
                        fTab,
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

    sci10<-function(x) {ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scales::scientific_format()(x)))))}


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



adornmzMLTab<-function(df,projDir=getwd()) {
    pref<-df$set
    mask<-is.na(pref)
    drop<-df$files[mask]
    for (d in drop) warning("Dropping",d,"because no set specified for it.")
    df<-df[!mask,]
    pref<-df$set
    wd<-basename(tools::file_path_sans_ext(df$Files))
    wd<-file.path(projDir,pref,wd)
    df$wd<-wd
    df
}

genSuprFileTab <- function(fileTab,compTab) {
    genOne<-function(ids,fn) {

        K<-length(ids)
        fTabRow<-fileTab[fileTab$Files == fn,]
        cols<-lapply(names(fileTab),function(n) rep(fTabRow[[n]],K))
        names(cols)<-NULL
        cols<-c(cols,list(ids))
        names(cols)<-c(names(fileTab),"ID")
        df<-as.data.frame(cols,stringsAsFactors = F)
        df
    }
    
    tabs<-lapply(fileTab$Files,function(fn)
    {
        wh<-which(fileTab$Files==fn)
        set<-fileTab$set[[wh]]
        md<-fileTab$mode[[wh]]
        sel<-(compTab$set %in% set) & (compTab$mode %in% md)
        ids<-compTab$ID[sel]
        genOne(ids,fn)
        
    })
    res<-do.call(rbind,tabs)
    res
}

getEntryFromComp<-function(entry,id,set,mode,compTab) {
    ind <- which(compTab$ID %in% id &
                 compTab$set %in% set &
                 compTab$mode %in% mode)

    res<- if (length(ind)==1) compTab[ind,entry] else {
                                                     if (length(ind)>1) {
                                                         stop("Nonunique entry selection in comprehensive table.")
                                                     } else {
                                                         stop("Entries not found for id ", id,"set ",set, "and mode ", mode, " .")
                                                     } 
                                                 }
    res
    names(res)<-entry
    res
        
}
addCompColsToFileTbl<-function(ft,compTab) {
    nR<-nrow(ft)
    mzCol<-rep(NA,nR)
    nmCol<-rep("",nR)
    rtCol<-rep(NA,nR)
    
    for (ir in 1:nR) {
        id<-ft[ir,"ID"]
        set<-ft[ir,"set"]
        m<-ft[ir,"mode"]
        entries<-getEntryFromComp(c("mz","Name","rt"),id,set,m,compTab)
        mzCol[[ir]]<-  entries[["mz"]]
        nm<-entries[["Name"]]
        nmCol[[ir]]<- if (!is.na(nm)) nm else ""
        rtCol[[ir]]<- entries[["rt"]]
    }
    ft$mz<-mzCol
    ft$Name<-nmCol
    ft$rt<-rtCol
    ft
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
