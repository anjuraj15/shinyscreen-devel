stripext<-function(fn) {
    bits<-strsplit(fn,split="\\.")[[1]]
    if (length(bits)> 1) paste(head(bits,-1),collapse=".") else fn}

##' Create directories without drama.
##'
##' Create directories without drama.
##' 
##' @title Create directories without drama
##' @param path Names of the directories.
##' @return The character string containing the input argument `path`.
##' @author Todor Kondić
no_drama_mkdir<-function(path) {
    f <- Vectorize(function(path) {
        if (! dir.exists(path)) dir.create(path)
        path},vectorize.args="path")
    f(path)
}

##' Produce the Rmb Settings file
##'
##' Produce the Rmb Settings file based on the customisation file in
##' YAML format.
##'
##' @title Generate RMassBank settings file.
##' @param sett_alist The named list of settings that are different
##'     from the RMassBank defaults.
##' @param file The name of the YAML specification that will be merged
##'     with the template Rmb settings file.
##' @return NULL
mk_sett_file<-function(sett_alist,file) {
    tmp<-tempfile()
    RMassBank::RmbSettingsTemplate(tmp)
    sett<-yaml::yaml.load_file(tmp)
    for (nm in names(sett_alist)) {
        sett[[nm]]<-sett_alist[[nm]]
    }
    yaml::write_yaml(x=sett,file=file)
    NULL
}

##' Combine the RMB settings files
##' 
##' Combine RMB settings with different collisional energies into one
##' settings file with multiple collisional energy entries.
##' 
##' @title Combine RMB Settings With Different Collisional Energies
##' @param sett_fns A list of settings files.
##' @param fname The name of the combined file.
##' @return fname
##' @author Todor Kondić
mk_combine_file<-function(sett_fns,fname) {
    all_settings <- lapply(sett_fns,yaml::yaml.load_file)
    comb_settings <- all_settings[[1]]
    
    for (n in 1:length(all_settings)) {
        comb_settings$spectraList[[n]] <- all_settings[[n]]$spectraList[[1]]
    }

    yaml::write_yaml(x=comb_settings,fname)
    fname
}


fn_data2wd <- function(fn_data,dest) {
    
    f <- Vectorize(function(fn_data) {
        noext <- stripext(fn_data)
        file.path(dest,basename(noext))
    },vectorize.args="fn_data")
    f(fn_data)
}

get_presc_d <- function(wd) {wd}
gen_presc_d <- function(wd) { no_drama_mkdir(wd)}
    
    

get_cmpd_l_fn <- function(wd) {
    f <- function(wd) file.path(wd,"compounds.csv")
    fv <- Vectorize(f,vectorize.args=c("wd"))
    fv(wd)
}

get_stgs_fn <- function(wd) {
    f <- function(wd) file.path(wd,"settings.ini")
    fv <- Vectorize(f,vectorize.args=c("wd"))
    fv(wd)
}

get_ftable_fn <- function(wd) {
    f <- function(wd) file.path(wd,"ftable.csv")
    fv <- Vectorize(f,vectorize.args=c("wd"))
    fv(wd)
}

get_inp_stgs_fn<- function(fn_data) {
    f <- Vectorize(function(fn_data) {
        bnm <- stripext(fn_data)
        fn <- paste(bnm,".ini",sep='')},
        vectorize.args="fn_data")
    f(fn_data)}

get_info_dir <- function(wd) {
    file.path(wd,"info")
}

get_info_fn <- function(wd) {
    file.path(get_info_dir(wd),"info.csv")
}

gen_info_dir <- function(wd) {
    nm <- get_info_dir(wd)
    no_drama_mkdir(nm)
    nm
}



##' Generate the RMassBank compound list from the input compound list
##' in CSV file src_fn. The input compound list format is either a
##' Chemical Dashboard csv file with, at least, PREFERRED_ SMILES
##' columns _filled_ out, or just an ordinary CSV file with columns
##' SMILES and Names filled. Argument dest_fn is the destination
##' filename. Returns the number of compounds.
##'
##' @title Generate Compound List File
##' @param src_fn The input compound list CSV filename.
##' @param dest_fn The resulting compound list CSV filename.
##' @return Number of compounds.
##' @author Todor Kondić
gen_cmpd_l<-function(src_fn,dest_fn) {
    df<-read.csv(src_fn)
    ## Names
    nms<-if ("PREFERRED_NAME" %in% names(df)) df$PREFERRED_NAME else df$Name
    if (is.null(nms)) stop("Unable to read compound names from the input compound list.")
    print(df)	
    print(is.null(df$SMILES))
    ## take Mass, else SMILES 	
    if (is.null(df$SMILES)){ # SMILES column in cpdList must be non-existent
	    mass<- df$Mass
	    haha<-rep("",length(mass))
	    level<-rep(5,length(mass))
    }else{
	    haha <- df$SMILES
	    level<- rep(5,length(haha))
    }
    
    sz<-length(haha)
    
    ## CAS
    casvals<-if ("CASRN" %in% names(df)) df$CASRN else rep(NA,sz)
    if (is.null(haha)) stop("Unable to read SMILES from the input compound list.")
    outdf<-data.frame(ID=1:sz,Name=nms,SMILES=haha,CAS=casvals,RT=rep(NA,sz),mz=mass,Level=level)
    f <- Vectorize(function (dest_fn) {
        write.csv(outdf,file=dest_fn,row.names=F,na="")
    },vectorize.args="dest_fn",SIMPLIFY=F)
    f(dest_fn)
    length(nms)
}

##' Generates settings file and loads it.
##'
##' @title Generate and Load the RMassBank Settings File
##' @param stgs Settings named list, or a settings filename.
##' @param wd Directory under which results are archived.
##' @return result of RMassBank::loadRmbSettings
##' @author Todor Kondić
gen_stgs_and_load <- function(stgs,wd) {
    stgs<-if (is.character(stgs)) yaml::yaml.load_file(stgs) else stgs
    sfn<-get_stgs_fn(wd)
    mk_sett_file(stgs,sfn)
    RMassBank::loadRmbSettings(sfn)
}

##' Generates the RMassBank compound list and loads it.
##'
##' @title Generate and Load the RMassBank Compound List
##' @param wd Directory under which results are archived.
##' @param fn_cmpdl The input compound list filename. 
##' @return Named list. The key `fn_cmpdl` is the path of the
##'     generated compound list and the key `n` the number of
##'     compounds.
##' @author Todor Kondić
gen_cmpdl_and_load <- function(wd,fn_cmpdl) {
    fn_comp<-get_cmpd_l_fn(wd)
    n_cmpd<-gen_cmpd_l(fn_cmpdl,fn_comp)
    RMassBank::loadList(fn_comp,check=F) #reduce universality of this statement!!!
    list(fn_cmpdl=fn_comp,n=n_cmpd)
}

##' Generates file table.
##'
##' 
##' @title Generate and Load the RMassBank Settings File
##' @param fn_data The mzML filename.
##' @param wd Directory under which results are archived.
##' @param n_cmpd Number of compounds.
##' @return File path of the file table.
##' @author Todor Kondić
gen_ftable <- function(fn_data,wd,n_cmpd) {
    f <- Vectorize(function(fn_data,wd) {
        df_table<-data.frame(Files=rep(fn_data,n_cmpd),ID=1:n_cmpd)
        fn_table<-get_ftable_fn(wd)
        write.csv(x=df_table,file=fn_table,row.names=F)
        fn_table
    }, vectorize.args=c("fn_data","wd"))

    f(fn_data,wd)
}

gen_fn_stgs <- function(fn_inp,fn) {
    f <- Vectorize(function(fn_inp,fn) {
        stgs <- yaml::yaml.load_file(fn_inp)
        mk_sett_file(stgs,fn)
        fn}, vectorize.args=c("fn_inp","fn"))

    f(fn_inp,fn)
}

conf <- function(fn_data,fn_cmpd_l,dest) {
    no_drama_mkdir(dest)
    wd <- fn_data2wd(fn_data,dest)
    no_drama_mkdir(wd)
    fn_inp_stgs <- get_inp_stgs_fn(fn_data)
    fn_stgs <- get_stgs_fn(wd)
    fn_out_cmpd_l <- get_cmpd_l_fn(wd)

    gen_fn_stgs(fn_inp_stgs,fn_stgs)
    n_cmpd <- gen_cmpd_l(fn_cmpd_l,fn_out_cmpd_l)
    gen_ftable(fn_data,wd,n_cmpd)
}

reconf <- function(wd) {## Load the settings.
    fn_stgs <- get_stgs_fn(wd)
    RMassBank::loadRmbSettings(fn_stgs)
    
    ## Load the compound list.
    fn_cmpd_l <- get_cmpd_l_fn(wd)
    RMassBank::loadList(fn_cmpd_l)
}

##' Prescreens. Writes data out. Adapted from ReSOLUTION
##'
##' 
##' @title Prescreen
##' @param wd Absolute path to the directory that will contain the
##'     resulting data frame.
##' @param RMB_mode ...
##' @param FileList ...
##' @param cmpd_list ...
##' @param ppm_limit_fine ...
##' @param EIC_limit ...
##' @author Emma Schymanski, Todor Kondić
RMB_EIC_prescreen_df <- function (wd, RMB_mode, FileList, cmpd_list,
                                  ppm_limit_fine = 10, EIC_limit = 0.001) {


    n_spec <- 0
    cmpd_RT_maxI <- ""
    msms_found <- ""
    rts <- 0
    max_I_prec <- ""
    cmpd_RT_maxI_min <- ""
    file_list <- read.csv(FileList, stringsAsFactors = FALSE)
    cmpd_info <- read.csv(cmpd_list, stringsAsFactors = FALSE)
    ncmpd <- nrow(cmpd_info)
    odir=wd
    get_width <- function(maxid) {log10(maxid)+1}
    id_field_width <- get_width(ncmpd)

    fn_out<- function(id,suff) {file.path(odir,paste(formatC(id,width=id_field_width,flag=0),suff,".csv",sep=''))}
    f <- mzR::openMSfile(file_list$Files[1])
    for (i in 1:length(file_list$ID)) {
        cpdID <- file_list$ID[i]
        n_spec <- n_spec + 1
        smiles <- tryCatch(RMassBank::findSmiles(cpdID), error = function(e) NA)
        if (!is.na(smiles)) {
            mz <- as.numeric(RMassBank::findMz(cpdID, RMB_mode)[3])
        }
        else {
            mz <- as.numeric(RMassBank::findMz(cpdID, RMB_mode, retrieval = "unknown")[3])
        }
        eic <- RMassBank::findEIC(f, mz, limit = EIC_limit)
        msms_found[n_spec] <- FALSE
        msms <- RMassBank::findMsMsHR.mass(f, mz, 0.5, RMassBank::ppm(mz, ppm_limit_fine, 
                                                           p = TRUE))
        max_I_prec_index <- which.max(eic$intensity)
        cmpd_RT_maxI[n_spec] <- eic[max_I_prec_index, 1]
        max_I_prec[n_spec] <- eic[max_I_prec_index, 2]
        cmpd_RT_maxI_min[n_spec] <- as.numeric(cmpd_RT_maxI[n_spec])/60
        ## plot.new()
        ## plot.window(range(eic$rt), range(eic$intensity))
        ## box()
        ## lines(eic$intensity ~ eic$rt)
        write.csv(x=eic[c("rt","intensity")],file=fn_out(cpdID,".eic"),row.names=F)
        cpd_df <- data.frame("rt"=c(),"intensity"=c())
        for (specs in msms) {
            if (specs@found == TRUE) {
                
                df <- do.call(rbind, lapply(specs@children, function(sp) c(sp@rt, 
                                                                           intensity = max(sp@intensity))))
                cpd_df <- rbind(cpd_df,df,make.row.names = F)
                ## lines(intensity ~ retentionTime, data = df, type = "h", 
                ##       col = "blue")
                msms_found[n_spec] <- TRUE
            }
        }
        if (nrow(cpd_df)>0) write.csv(x=cpd_df,file=fn_out(cpdID,".kids"),row.names=F)
        ## title(main = cpdID, xlab = "RT (sec)", ylab = "Intensity")
        ## text(as.numeric(cmpd_RT_maxI[n_spec]), as.numeric(max_I_prec[n_spec]), 
        ##      labels = as.numeric(cmpd_RT_maxI_min[n_spec]), pos = 4)
        ## axis(1)
        ## axis(2)
        ## gc()
        rts[i] <- (cmpd_RT_maxI[n_spec])
    }
    # dev.off()
    write.csv(cbind(file_list$ID, cmpd_info$mz, cmpd_info$Name, 
                    cmpd_RT_maxI, cmpd_RT_maxI_min, max_I_prec, msms_found), 
              file = file.path(odir,"RTs_wI.csv"), 
              row.names = F)
}

preProc <- function (fnFileTab,fnDest=paste(stripext(fnFileTab),"_candidate.csv",sep=''),noiseFac=3,rtDelta=0.5,ms1_intTresh=1e5,ms2_intTresh=1e4,MS1peakWi=0.3) {

    ## read in .csv file as file
    ftable <- read.csv(file = fnFileTab, header = T, sep=",", stringsAsFactors = F)
    
    getWidth <- function(maxid) {log10(maxid)+1}
    ids <- as.numeric(levels(factor(ftable$ID))
		      
		      ##is MS1 intensity high enough?)
    id_field_width <- getWidth(max(ids))
    fn_out<- function(id,suff) {paste(formatC(id,width=id_field_width,flag=0),suff,".csv",sep='')}

    ## for loop through dataframe called file to set tresholds
    ftable[c("MS1","MS2","Alignment","MS1Intensity","AboveNoise","MS2Intensity","MS1peakWidth")] <- T
    ftable$Comments <- ""
    for (ind in 1:nrow(ftable)) {
        wd <- ftable$wd[ind]
        id <- ftable$ID[ind]
        odir=file.path(wd)
        fn_eic <- file.path(wd,fn_out(id,".eic"))
        eic <- NULL
        maxInt <- NULL
        eicExists <- T

	##does MS1 exist?
        if(!file.exists(fn_eic)) { 
            ftable[ind,"MS1"] = FALSE
            eicExists <- F
        }
        else {
            eic <- read.csv(fn_eic, sep = ",", stringsAsFactors = F)
            maxInt <- max(eic$intensity)

	    ##is MS1 intensity high enough?
            if (maxInt < ms1_intTresh) {
                ftable[ind,"MS1Intensity"] = FALSE
            }
            ##Detect noisy signal. This is a naive implementation, so careful.
            mInt <- mean(eic$intensity)
            if (maxInt < noiseFac*mInt) ftable[ind,"AboveNoise"] <- F

	    ##Is MS1 peak a proper peak, or just a spike? Check peakwidth.
	    MS1peakWi

        }

	#####MS2 checks
        fn_kids <- file.path(wd,fn_out(id,".kids"))

	##does MS2 exist? Regardless of quality/alignment.
        if(!file.exists(fn_kids)) {
            ftable[ind,"MS2"] = FALSE
	    ftable[ind,"MS2Intensity"] = NA #moot
	    ftable[ind,"Alignment"] = NA #moot
        } else {
        ## Detect RT shifts. Naive implementation, so careful.
            if (eicExists) {  ################WHY THIS IF CONDITIONAL?? If MS2 exists, then eic MUST exist, no? Seems redundant.
		    ##Is MS2 intensity high enough?
		    ms2maxInt <- max(msms@intensity)
		    if (ms2maxInt > ms2_intTresh){
			    rtInd <- match(maxInt,eic$intensity) #returns position of first match in eic$intensity
                	    rtMax <- eic$rt[rtInd] #fetch the rtmax value (RT with highest int;seconds)  using above index
                	    msms <- read.csv(fn_kids, sep = ",", stringsAsFactors = F)
                	    whc <- msms$rt > rtMax - rtDelta #T/F vector: are RT vals of ms2 above rtMax within Delta? Good peaks=TRUE;rt must be retentionTime!!!
                	    whc <- whc < rtMax + rtDelta #T/F vector: are RT vals of ms2 above rtMax within Delta?
		#Overwrites whc! In any case, cannot use this as both must be T to give final whc of T (i.e. fall within the window).
                	    ints <- msms$intensity[whc] #builds ints vector with intensities which fall in window
                	    if (! any(ints>0)) ftable[ind,"Alignment"] = FALSE #if none of ints larger than 0, Alignment <-F 
		    } else {
			ftable[ind,"MS2Intensity"] = FALSE
		    	ftable[ind,"Alignment"] = NA #neither T or F, the MS2 was not of decent intensity in first place, alignment moot. 
		}   
	    }
        }
    }

    ## get a csv outfile
    write.csv(ftable, file = fnDest,row.names=F)

}


##' Helper function for rendersmiles2
##'
##' @title Render Compound from an Online Resource
##' @param depictURL The URL of the object to plot.
##' @param coords The positioning of the image (in data coords).
##' @param filename Temp filename.
##' @return Nothing useful.
##' @author Todor Kondić
renderurl <- function(depictURL,coords=c(0,0,100,100), filename=tempfile(fileext=".svg")) {
    h <- new_handle()
    curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer=0)
    curl::curl_download(url=depictURL,filename,handle=h)
    img <- rsvg(filename)
    if (length(img)>2) {
        rasterImage(img,xleft=coords[1],ybottom=coords[2],xright=coords[3],ytop=coords[4])
    }
}


## rendersmiles <- function(smiles, kekulise=TRUE, coords=c(0,0,100,100), width=200, height=200,
##                               zoom=1.3,style="cow", annotate="off", abbr="on",suppressh=TRUE,
##                               showTitle=FALSE, smaLimit=100, sma=NULL) {
##   dep <- get.depictor(width = width, height = height, zoom = zoom, style = style, annotate = annotate,
##                       abbr = abbr, suppressh = suppressh, showTitle = showTitle, smaLimit = smaLimit,
##                       sma = NULL)
##   library(rcdk)
##   library(RChemMass)
##   mol <- getMolecule(smiles)
##   img <- view.image.2d(mol, depictor=dep)
##   rasterImage(img, coords[1],coords[2], coords[3],coords[4])

## }

##' Render smiles from an online resource.
##'
##' @title Turn SMILES to an Image Using Online Resource
##' @param smiles The SMILES string.
##' @param style Structure style.
##' @param ... Hand over to renderurl.
##' @return Nothing useful.
##' @author Todor Kondić
rendersmiles2 <- function(smiles,style="cow",...) {
    dpurl <- buildCDKdepictURL(smiles,style=style)
    renderurl(dpurl,filename=tempfile(fileext=".svg"),...)
}

calcLogTics <- function(lims,powUnit=1,linDiv=1,howMany=NULL) {
    ## Find integer power limits.
    llim <- log10(lims)
    llim[is.infinite(llim)] <- 0
    nlim <- llim/powUnit
    ilim <- round(nlim)*powUnit

    all <- if (linDiv>1) {
               z <- sapply(seq(ilim[1],(ilim[2]-1),by=powUnit),
                           function(i) {
                               a <- 10.**i
                               b <- 10.**(i+1)
                               st <- b/linDiv
                               s <- seq(0,b,by=st)
                               s[2:length(s)]
                           })
               dim(z) <- prod(dim(z))
               z
           } else
               10**seq(ilim[1],ilim[2],by=powUnit)
    
    res <- if (!is.null(howMany)) {
               if (howMany<length(all)) {
                   step <- length(all) %/% howMany    
                   ind <- seq(1,howMany*step,by=step)
                   rev(rev(all)[ind])
               } else
                   return(NULL)
               
           } else
               all

    res

}

calcLabels <- function(ticVals) {
    pw <- as.integer(log10(abs(ticVals)))
    mags <- 10**pw
    mags[is.na(mags)] <- 0
    pw[is.na(mags)] <- 0
    mant <- signif(ticVals/mags,3)
    zz <- Map(function (m,p) c(m=m,p=p),mant,pw)
    sapply(zz,function (z) {as.expression(bquote(.(z['m']) %*% 10^.(z['p'])))},USE.NAMES = F)
}

arrPlot <- function(xlim,ylim,ytics,xaxis=F,log=NULL,cex=0.2) {
    ylim[is.na(ylim)] <- 1
    ylim[ylim == 0] <- 1
    if (is.null(ylim)) ylim <- c(1,10)

    if (xaxis) xaxt="s" else xaxt="n"
    if (! is.null(log) && ! any(is.na(ytics)) ) {
        plot(1,1,xlab="",ylab="",xlim = xlim,ylim = ylim,type="n",log=log,xaxt=xaxt,yaxt = "n")
        message("ytics:",ytics)
        ltics <- calcLabels(ytics)
        axis(side=2,at=ytics,labels=ltics,las=2,cex=cex,gap.axis = -1)
    } else {
        plot(1,1,xlab="",ylab="",xlim = xlim,ylim = ylim,type="n",xaxt = xaxt)
        axis(side=2,las=2,cex=cex)
    }

}

arrPlotStd <- function(xlim,ylim,xaxis=F,log=log,cex=1.5,mar,intTresh) {
    if (ylim[1]<intTresh) ylim[1] <- intTresh
    if  (is.na(ylim[2])) ylim[2] <- 10
    if (xaxis) xaxt="s" else xaxt="n"
    par(mar=mar)
    plot(1,1,xlab="",ylab="",xlim = xlim,ylim = ylim,type="n",log=log,xaxt=xaxt,yaxt = "n",cex.axis=cex)
    ytics <- if (log=="y") axTicks(side=2, nintLog = 3) else axTicks(side=2)
    message("YTICS:",do.call(paste,as.list(ytics)))
                                                                             
    ltics <- calcLabels(ytics)
    axis(side=2,at=ytics,labels=ltics,las=2,cex.axis=cex,gap.axis = -1)
    
}



plot_id_aux <- function(i,wd,eics,maybekids,masses,osmesi,tags,logYAxis,pal="Dark2",cex=0.75,rt_digits=2,m_digits=4,rtrange=NULL) {
    clean_rtrange <- function(def) {
            x1 <- rtrange[1]
            x2 <- rtrange[2]
            if (is.na(x1) || x1 == 0) x1 <- def[1]
            if (is.na(x2) || x2 == 0) x2 <- def[2]

            c(x1,x2)
    }



    if (logYAxis == "linear") log = ""
    if (logYAxis == "log") log = "y"
    
    LEFT_MARGIN=9

    
    eic <- eics[[i]]
    maybekid <- maybekids[[i]]
    dfs <- lapply(file.path(wd,eic),function(fn) {
        tryCatch(read.csv(fn,stringsAsFactors = F),
                 error=function(e) {message(paste(e,"; offending file:",fn))})
    })
    
    dfs <- lapply(dfs,function(x) data.frame(rt=x$rt/60.,intensity=x$intensity))

    ## Find existing children.
    maybes <- file.path(wd,maybekid)
    indkids <- which(file.exists(maybes))
    kids <- maybes[indkids]
    dfs_kids <- lapply(kids,read.csv,stringsAsFactors=F)
    dfs_kids <- lapply(dfs_kids,function(x) data.frame(rt=x$retentionTime/60.,intensity= x$intensity))


    ## Find max intensities.
    w_max <- sapply(dfs,function (x) which.max(x$intensity))
    rt_max <- Map(function(df,w) df$rt[[w]],dfs,w_max)
    i_max<- Map(function(df,w) df$intensity[[w]],dfs,w_max)
    symbs <- LETTERS[1:length(w_max)]

    ## Find max intensities in children
    w_max_kids <- sapply(dfs_kids,function (x) which.max(abs(x$intensity)))
    rt_max_kids <- Map(function(df,w) df$rt[[w]],dfs_kids,w_max_kids)
    i_max_kids <- Map(function(df,w) df$intensity[[w]],dfs_kids,w_max_kids)
    symbs_kids<- letters[indkids]

    
    def_rt_rng <- range(sapply(dfs,function(x) x$rt))
    rt_rng <- if (is.null(rtrange))  def_rt_rng else clean_rtrange(def_rt_rng)

    int_rng<- range(sapply(dfs,function(x) x$intensity))
    int_rng_kids<- if (! is.null(dfs_kids))
                       range(sapply(dfs_kids,function(x) x$intensity)) else
                                                                           c(0,1)
    cols <- RColorBrewer::brewer.pal(n=length(dfs),name=pal)
    lgnd <- Map(function(k,v) paste(k,"= ",formatC(v,format="f",digits=rt_digits),sep=''),symbs,rt_max)


   
    layout(matrix(c(3,3,4,4,1,2), 3, 2, byrow = TRUE))
    ## par(mar=c(1,2,1,4))
    struc_xr <- c(0,100)
    struc_yr <- c(0,100)

    par(mar=c(1,LEFT_MARGIN,3,4))
    plot(1,1,type="n",xlab="",ylab="",xlim=struc_xr,ylim=struc_yr,xaxt="n",yaxt="n",asp=1,axes = FALSE)
    #rendersmiles2(osmesi[i],coords=c(struc_xr[1],struc_yr[1],struc_xr[2],struc_yr[2]))
    
    col_eng <- c(0,100)
    peak_int <- c(0,100)
    par(mar=c(1,6,3,1))
    plot(1,1,type="n",xlab="",ylab="",xlim=col_eng,ylim=peak_int,xaxt="n",yaxt="n",axes = FALSE)
    linfo <- legend("topleft",horiz=T,legend=tags,col=cols,fill=cols,bty="n",cex=1.5)
    legend(x=linfo$rect$left,y=linfo$rect$top-1*linfo$rect$h,horiz=F,legend=lgnd,fill=cols,bty='n',cex=1.5)
    
    cols_kids <- cols[indkids]
    lgnd_kids <- Map(function(k,v) paste(k,"= ",formatC(v,digits=rt_digits,format="f"),sep=''),symbs_kids,rt_max_kids)

    if (length(lgnd_kids)>0) legend(x=linfo$rect$left-14*linfo$rect$left,y=linfo$rect$top-1*linfo$rect$h,horiz=F,legend=lgnd_kids,fill=cols[indkids],bty="n",cex=1.5)


    arrPlotStd(xlim=rt_rng,ylim=int_rng,mar=c(0,LEFT_MARGIN,3,0),log=log,intTresh=1e4)
    title(main=paste("ID:",i,"Ion m:",formatC(masses[[i]],digits=m_digits,format="f")))
    for (k in seq(length(w_max))) text(rt_max[[k]],i_max[[k]],labels=symbs[[k]],pos=4,offset=0.5*k)

    mtext("intensity",side = 2,adj=0.2,cex=1.3,line=7)
    ## Plot eic across the directory set.
    for (n in 1:length(dfs)) {
        df <- dfs[[n]]
        col <- cols[[n]]
        lines(intensity ~ rt,data=df,col=col)
    }


    if (length(dfs_kids) >0) {
        arrPlotStd(xlim=rt_rng,ylim=int_rng_kids,xaxis=T,log=log,mar=c(4,LEFT_MARGIN,0,0),intTresh=1)
        for (k in 1:length(indkids)) {
            lines(intensity ~ rt,data=dfs_kids[[k]],type="h",col=cols_kids[[k]])
        }
    } else {
        arrPlotStd(xlim=rt_rng,ylim=c(1,10),xaxis=T,log=log,mar=c(4,9,0,0),intTresh=1)
    }
    mtext("retention time [min]",side = 1,adj=0.5,cex=1.3,line = 3)
    if (length(dfs_kids)>0) for (k in seq(length(w_max_kids))) text(rt_max_kids[[k]],i_max_kids[[k]],labels=symbs_kids[[k]],pos=4,offset=0.5*k)    
    ## RChemMass::renderSMILES.rcdk(smiles[[i]],coords=c(x1,y1,x2,y2))
    gc()
    
}
##' Plot the output of prescreen.
##'
##' @title Plot the Output of Prescreen
##' @param prescdf File table data-frame. See presc.shiny for details.
##' @param mode RMB mode.
##' @param out The name of the output file.
##' @param fn_cmpd_l The compound list name.
##' @param pal ColorBrewer palette name.
##' @param cex As in legend.
##' @param rt_digits Number of digits after the point for the retention time.
##' @param m_digits Number of digits after the point for the mass.
##' @param wd Sequence of data dirs containing the prescreen subdir.
##' @param digits Number of significant digits for peak ret times. 
##' @return Nothing useful.
##' @author Todor Kondić
##' @author Mira Narayanan
##' @author Anjana Elapavalore
##' @export
presc.plot <- function(prescdf,mode,out="prescreen.pdf",fn_cmpd_l,pal="Dark2",cex=0.75,rt_digits=2,m_digits=4) {
    modemap=list(pH="MpHp_mass",
                 mH="MmHm_mass",
                 blahnh4="MpNH4_mass",
                 blahna="MpNa_mass")

    tags <- levels(factor(prescdf$tag))
    wd <- prescdf$wd[match(tags,prescdf$tag)]
    
    wd1 <- wd[[1]]
    df <- read.csv(file=fn_cmpd_l,stringsAsFactors = F)
    osmesi <- df$SMILES
    no_cmpds <- length(osmesi)
    # reconf(wd1)
    masses <- lapply(osmesi,function (smile) {
        #osmesi <- tryCatch(RMassBank::findSmiles(i), error = function(e) NA)
        zz <- RChemMass::getSuspectFormulaMass(smile)
        zz[[modemap[[mode]]]]
    })
    #message("Masses:",masses)
    # return(osmesi)

    ## Get the basenames of eic files.
    eics <- list.files(path=wd[[1]],patt=".*eic.csv")
    maybekids <- sapply(strsplit(eics,split="\\."),function(x) {paste(x[[1]][1],'.kids.csv',sep='')})

    pdf(out)
    for (i in 1:length(osmesi)) plot_id_aux(i=i,wd=wd,eics=eics,maybekids=maybekids,masses=masses,osmesi=osmesi,log="y",tags=tags,rtrange=rtrange,cex=cex,pal=pal,rt_digits=rt_digits,m_digits=m_digits)
    dev.off()
}
    
mkUI <- function(idSliderRange,setName,rtRange,tags) {

    tabPanelList <- lapply(tags, function(tag) {
        shiny::tabPanel(tag, shiny::checkboxGroupInput(paste("spectProps",tag,sep=""), "Quality Control",
                                                      c(MS1 = "MS1",
                                                        MS2 = "MS2",
                                                        Alignment = "Alignment",
                                                        Intensity = "Intensity",
                                                        AboveNoise = "AboveNoise")),
                        shiny::textAreaInput(paste("caption",tag,sep=""), "Comments:", "Insert your comment here..."),
                        shiny::verbatimTextOutput(paste("value",tag,sep=""))
                        )})
    
    nvPanel <- do.call(shiny::navlistPanel, tabPanelList)
    
    ui <- shinydashboard::dashboardPage(skin="black",
          shinydashboard::dashboardHeader(title = "Prescreening"),
          shinydashboard::dashboardSidebar(
                              width = 350,
                              shinydashboard::sidebarMenu(
                                                  shinydashboard::menuItem(text = "The Prescreening", tabName = "Prescreen", icon = shiny::icon("dashboard")),
                                               shinydashboard::menuItem(text = "Adduct Calculator", tabName = "Adducts", icon = shiny::icon("th")))),
          shinydashboard::dashboardBody(
                              shinydashboard::tabItems(
                                  shinydashboard::tabItem(tabName = "Adducts",
                                                          shiny::h2("Calculate the adducts"),
                                                          shiny::fluidRow(
                                                      shinydashboard::box(
                                                                          title = "INPUT", width = 4, solidHeader = TRUE, status = "primary",
                                                                          shiny::textAreaInput("IdSmiles","Enter Smiles",value="",width="100%"),
                                                                          shiny::fileInput("smileslist", "Upload SMILES List", multiple = FALSE,accept=(".csv"))
                                                                      ),
                                                      shinydashboard::box(
                                                                          title = "Adduct Masses",width = 8,status="primary",
                                                                          shiny::tableOutput("masses"),
                                                                          shiny::textInput("adduct-table", "Adduct Masses",value="adductable.csv"),
                                                                          shiny::actionButton("downloadtable", "Download", icon = shiny::icon("download"))
                                                                      )
                                                      )),
                        shinydashboard::tabItem(tabName = "Prescreen",
                                          shiny::h2("The Prescreen plot"),
                              shiny::fluidRow(
                                         shinydashboard::box(
                                                             title = "MS Prescreening", width = 7, height = "80px", background = "blue", ""
                                                         ),
                                         shinydashboard::box(
                                                             title = "Compound ID N°",width = 5, height = "80px", background = "olive",
                                                             shiny::textOutput("compoundID")
                                                         )
                                     ),
                              shiny::fluidRow(
                                         shinydashboard::box(
                                                             title = "Plot", width = 7, color = "olive", solidHeader = FALSE, collapsible = TRUE,
                                                             shiny::plotOutput("plot1", width = "100%", height = "750px", click = NULL,
                                                                               dblclick = NULL, hover = NULL, hoverDelay = NULL,
                                                                               hoverDelayType = NULL, brush = NULL, clickId = NULL,
                                                                               hoverId = NULL),
                                                             shiny::textInput("plotname", "Insert plot name: (e.g. plotname_%i.pdf)",value="plotCpdID_%i.pdf"),
                                                             shiny::actionButton("saveplot", "Save", icon = shiny::icon("save")),
                                                             shiny::actionButton("saveallplots", "Save All Plots", icon = shiny::icon("save"))
                                                            
                                                         ),
                                         shinydashboard::box(
                                                             title = "Compounds", width=5, solidHeader = FALSE, color = "olive", collapsible = TRUE, "", shiny::br(),
                                                             shiny::sliderInput("idslider", "Compound number:", idSliderRange[1], idSliderRange[2], value=1,step=1)
                                                         ),
                                         shinydashboard::box(
                                                             title = "Prescreening analysis", width = 5, solidHeader = FALSE, collapsible = TRUE,
                                                             shiny::titlePanel(setName),
                                                             nvPanel,
                                                             shiny::actionButton("submitQA", "Submit", icon = shiny::icon("save")),
                                                             shiny::textInput("fn_ftable", "File table Name",value="ftable.csv"),
                                                             shiny::actionButton("savefiletable", "Save File Table", icon = shiny::icon("save"))
                                                         ),
                                         shinydashboard::box(
                                                             title = "Plot Parameters", width=7, solidHeader = FALSE, collapsible = TRUE, "", shiny::br(),
                                                             shiny::numericInput("min_val", "Minimum x Axis Value", rtRange[1]),
                                                             shiny::numericInput("max_val", "Maximum x Axis Value", rtRange[2]),
                                                             shiny::radioButtons("yaxis", "Parameters for y Axis",
                                                                                 c(linear = "linear",
                                                                                   log = "log")),
                                                             shiny::numericInput("nice", "Nice", rtRange[1]),
                                                             shiny::numericInput("steps", "Steps", rtRange[2])
                                                         )                                      
                                         )
                          )
         )))}

##' Prescreening using shiny interface.
##'
##' @title Prescreening with Shiny 
##' @return Nothing useful. 
##' @author Jessy Krier
##' @author Mira Narayanan
##' @author Hiba Mohammed Taha
##' @author Anjana Elapavalore
##' @author Todor Kondić
##' @param prescdf File table data-frame. Columns: Files,ID,wd,tag,set_name ...
##' @param mode RMassBank mode.
##' @param fn_cmpd_l Compound list file name.
##' @param pal ColorBrewer palette.
##' @param cex Size of fonts.
##' @param rt_digits Number of decimal places for the retention time.
##' @param m_digits Number of decimal places for the mass.
##' @export
presc.shiny <-function(prescdf,mode,fn_cmpd_l,pal="Dark2",cex=0.75,rt_digits=2,m_digits=4){
    ## Helper functions
    
    queryFileTable <- function(df,id) {
    df[df$ID %in% id,]
    }


    updateFileTable <- function(df,id,linput) {
        for (tag in names(linput)) {
            entries <- names(linput[[tag]])
            cond <- (df$ID %in% id) & (df$tag == tag)
            df[cond,entries] <- linput[[tag]]
        }
        df
    }

    ## Constants
    MODEMAP=list(pH="MpHp_mass",
                 mH="MmHm_mass",
                 blahnh4="MpNH4_mass",
                 blahna="MpNa_mass")
    DEFAULT_RT_RANGE=c(NA,NA)

    QANAMES <- c("MS1","MS2","Alignment","Intensity","AboveNoise")

    prescdf$tag <- as.character(prescdf$tag)
    tags <- levels(factor(prescdf$tag))
    wd <- prescdf$wd[match(tags,prescdf$tag)]
    
    wd1 <- wd[[1]]
    cmpd_l_df <- read.csv(file=fn_cmpd_l,stringsAsFactors = F)
    osmesi <- cmpd_l_df$SMILES
    no_cmpds <- length(osmesi)
    # reconf(wd1)
    masses <- lapply(osmesi,function (smile) {
        #osmesi <- tryCatch(RMassBank::findSmiles(i), error = function(e) NA)
        zz <- RChemMass::getSuspectFormulaMass(smile)
        zz[[MODEMAP[[mode]]]]
    })

    for (col in c("MS1","MS2","Alignment","Intensity","AboveNoise","Comments")) {
        if (is.null(prescdf[[col]])) prescdf[[col]] <- T
    }


    ## Get the basenames of eic files.
    eics <- list.files(path=wd[[1]],patt=".*eic.csv")
    maybekids <- sapply(strsplit(eics,split="\\."),function(x) {paste(x[[1]][1],'.kids.csv',sep='')})

    plot_id <- function (i,rtrange=NULL,log=rv$yaxis) plot_id_aux(i=i,wd=wd,eics=eics,maybekids=maybekids,masses=masses,osmesi=osmesi,tags=tags,log=log,rtrange=rtrange,cex=cex,pal=pal,rt_digits=rt_digits,m_digits=m_digits)
    


    spectProps <- sapply(tags,function (tag) paste("spectProps",tag,sep=""))
    idSliderRange <- range(cmpd_l_df$ID)
    

    
    ui <- mkUI(idSliderRange=idSliderRange,setName=prescdf$set_name,rtRange=DEFAULT_RT_RANGE,tags=tags)


    

    getCheckboxValues <- function(tag,input) {
        chkbox <- input[[spectProps[[tag]]]]
        q <- sapply(QANAMES,function (qn) if (qn %in% chkbox) T else F)
        names(q) <- QANAMES
        q
    }
    
    server <- function(input, output, session) {
        rv_adduc<- shiny::reactiveValues(smiles=1,
                                         resTable=1)
        rv <- shiny::reactiveValues(prescList=list(),
                                    prescdf=prescdf,
                                    spectProps=spectProps,
                                    tags=tags,
                                    default_range=DEFAULT_RT_RANGE,
                                    no_cmpds=no_cmpds)

        output$plot1 <- renderPlot(
        { i=input$idslider
            
          rtrange <- c(input$min_val,input$max_val)
          plot_id(i,rtrange=rtrange, log=input$yaxis)

                                        #           intParameter <- c(input$nice, input$steps)
                
        })

        output$value <- renderText(
        {
            input$caption
        })

        output$compoundID <- renderText(
        {
            i=input$idslider
        })
        shiny::observeEvent(input$smileslist,
        {
            rv_adduc$smiles <- read.csv(input$smileslist$datapath, header=FALSE,sep=',',stringsAsFactors=FALSE)[[1]]
        })

        shiny::observeEvent(input$IdSmiles,
        {
            rv_adduc$smiles <- trimws(strsplit(input$IdSmiles,split="\n")[[1]],which="both")
        })
        
        output$masses <- renderTable(
        {

            message("LEN:",length(rv_adduc$smiles))

            expr =lapply(rv_adduc$smiles,RChemMass::getSuspectFormulaMass) 
            if (! length(expr) == 0) {
                df <- do.call(rbind,expr)
                df <- as.data.frame(df)
                names(df) <- c("SMILES", "Monoisotopicmass", "[M+H]+", "[M+NH4]+", "[M+Na]+", "[M-H]-")
                rv$resTable <- df
                df
            } else NULL
                      
        },digits=4)

        shiny::observeEvent(input$downloadtable,
        {
          
            write.csv(file=input[["adduct-table"]],x=rv$resTable,row.names = F)
          
        })
 

        shiny::observeEvent(input$saveplot,
        {
            i=input$idslider
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotCpdID_%i.pdf"
            fn <- sprintf(pfn,i)
            rtrange <- c(input$min_val,input$max_val)
            pdf(file=fn, width=12, height=8)
            plot_id(i,rtrange=rtrange, log=input$yaxis)
            dev.off()
        })

        shiny::observeEvent(input$saveallplots,
        {
            i=input$idslider
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotall.pdf"
            fn <- sprintf(pfn,i)
            pdf(file=fn, width=12, height=8)
            for (i in 1:rv$no_cmpds) {
                plot_id(i,log=input$yaxis)
                message("Compound ID ",i," done.")
            }
            dev.off()
        })

        shiny::observeEvent(input$submitQA,{
            res <- lapply(rv$tags,getCheckboxValues,input)
            names(res) <- rv$tags
            rv$prescdf <- updateFileTable(df=rv$prescdf,id=input$idslider,linput=res)
        })

        ## shiny::observeEvent(input$yaxis,{
        ##     rv$yaxis <- input$yaxis
            
        ## })

        shiny::observe({
            i <- input$idslider
            sdf <- queryFileTable(df=rv$prescdf,id=i)
            for (t in sdf$tag) {
                sprop <- rv$spectProps[[t]]
                sel <- as.logical(sdf[sdf$tag %in% t,QANAMES])
                choices <- QANAMES[sel]
                names(choices) <- QANAMES[sel]
                shiny::updateCheckboxGroupInput(session = session,inputId = sprop,selected=choices)
            }
        })

        shiny::observeEvent(input$savefiletable,
        {
            write.csv(file=input$fn_ftable,x=rv$prescdf,row.names = F)
            
        })
        

        session$onSessionEnded(function() {
            stopApp()
        })
    }
    
    shiny::shinyApp(ui = ui, server = server)
}

