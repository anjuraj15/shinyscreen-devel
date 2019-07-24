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

get_presc_d <- function(wd) { file.path(wd,"prescreen")}
gen_presc_d <- function(wd) { no_drama_mkdir(get_presc_d(wd))}
    
    

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

    ## SMILES
    haha<-df$SMILES
    sz<-length(haha)
    
    ## CAS
    casvals<-if ("CASRN" %in% names(df)) df$CASRN else rep(NA,sz)
    if (is.null(haha)) stop("Unable to read SMILES from the input compound list.")
    outdf<-data.frame(ID=1:sz,Name=nms,SMILES=haha,CAS=casvals,RT=rep(NA,sz))
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
    RMassBank::loadList(fn_comp)
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
    odir=file.path(wd,"prescreen")
    no_drama_mkdir(odir)
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
##' @param ... Hand over to renderurl.
##' @return Nothing useful.
##' @author Todor Kondić
rendersmiles2 <- function(smiles,...) {
    dpurl <- buildCDKdepictURL(smiles)
    renderurl(dpurl,filename=tempfile(fileext=".svg"),...)
}

##' Plot the output of prescreen.
##'
##' @title Plot the Output of Prescreen
##' @param wd Sequence of data dirs containing the prescreen subdir.
##' @param mode RMB mode.
##' @param out The name of the output file.
##' @param pal ColorBrewer palette name.
##' @param cex As in legend.
##' @param rt_digits Number of digits after the point for the retention time.
##' @param m_digits Number of digits after the point for the mass.
##' @param digits Number of significant digits for peak ret times. 
##' @return Nothing useful.
##' @author Todor Kondić
##' @export
presc.plot <- function(wd,mode,out="prescreen.pdf",pal="Dark2",cex=0.75,rt_digits=2,m_digits=4) {
    modemap=list(pH="MpHp_mass",
                 mH="MmHm_mass",
                 blahnh4="MpNH4_mass",
                 blahna="MpNa_mass")
    dfdir <- file.path(wd,"prescreen")

    wd1 <- wd[[1]]
    df <- read.csv(file=get_cmpd_l_fn(wd1),stringsAsFactors = F)
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
    eics <- list.files(path=dfdir[[1]],patt=".*eic.csv")
    maybekids <- sapply(strsplit(eics,split="\\."),function(x) {paste(x[[1]][1],'.kids.csv',sep='')})

    pdf(out)
    for (i in seq(length(eics))) {
        eic <- eics[[i]]
        maybekid <- maybekids[[i]]
        fn_ini <- lapply(wd,get_stgs_fn)
        
        lbls <- lapply(fn_ini,function(x) {s <- yaml::yaml.load_file(x);s$prescreen$tag})
        dfs <- lapply(file.path(dfdir,eic),function(fn) {
            tryCatch(read.csv(fn,stringsAsFactors = F),
                     error=function(e) {message(paste(e,"; offending file:",fn))})
        })
        
        dfs <- lapply(dfs,function(x) data.frame(rt=x$rt/60.,intensity=x$intensity))

        ## Find existing children.
        maybes <- file.path(dfdir,maybekid)
        indkids <- which(file.exists(maybes))
        kids <- maybes[indkids]
        dfs_kids <- lapply(kids,read.csv,stringsAsFactors=F)
        dfs_kids <- lapply(dfs_kids,function(x) data.frame(rt=x$retentionTime/60.,intensity= -x$intensity))


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

        
        
        rt_rng <- range(sapply(dfs,function(x) x$rt))
        int_rng <- range(sapply(append(dfs_kids,dfs),function(x) x$intensity))
        cols <- RColorBrewer::brewer.pal(n=length(dfs),name=pal)
        lgnd <- Map(function(k,v) paste(k,"= ",formatC(v,format="f",digits=rt_digits),sep=''),symbs,rt_max)
        
        layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE), 
               widths=c(7,8), heights=c(6,6))
        struc_xr <- c(0,100)
        struc_yr <- c(0,100)
        plot(1,1,type="n",xlab="",ylab="",xlim=struc_xr,ylim=struc_yr,xaxt="n",yaxt="n")
        rendersmiles2(osmesi[i],coords=c(struc_xr[1],struc_yr[1],struc_xr[2],struc_yr[2]))
       
        col_eng <- c(0,100)
        peak_int <- c(0,100)
        plot(1,1,type="n",xlab="",ylab="",xlim=col_eng,ylim=peak_int,xaxt="n",yaxt="n",axes = FALSE)
        linfo <- legend("topleft",horiz=T,legend=lbls,col=cols,fill=cols,bty="n",cex=cex)
        legend(x=linfo$rect$left,y=linfo$rect$top-0.5*linfo$rect$h,horiz=T,legend=lgnd,fill=cols,bty='n',cex=cex)
         
        cols_kids <- cols[indkids]
        lgnd_kids <- Map(function(k,v) paste(k,"= ",formatC(v,digits=rt_digits,format="f"),sep=''),symbs_kids,rt_max_kids)
        if (length(lgnd_kids)>0) legend(x=linfo$rect$left,y=linfo$rect$top-1*linfo$rect$h,horiz=T,legend=lgnd_kids,fill=cols[indkids],bty="n",cex=cex)
        plot(1,1,xlab="",ylab="",xlim = rt_rng,ylim = int_rng,type="n")

          ## Plot eic across the directory set.
        for (n in seq(length(dfs))) {
            df <- dfs[[n]]
            col <- cols[[n]]
            lines(df$intensity ~ df$rt,col=col)
        }

        if (length(dfs_kids) >0) {
            for (k in 1:length(indkids)) {
                lines(intensity ~ rt,data=dfs_kids[[k]],type="h",col=cols_kids[[k]])
            }
        }
        title(main=paste("ID:",i,"Ion m:",formatC(masses[[i]],digits=m_digits,format="f")),xlab="retention time [min]",ylab="intensity")
        for (k in seq(length(w_max))) text(rt_max[[k]],i_max[[k]],labels=symbs[[k]],pos=4,offset=0.5*k)
        if (length(dfs_kids)>0) for (k in seq(length(w_max_kids))) text(rt_max_kids[[k]],i_max_kids[[k]],labels=symbs_kids[[k]],pos=4,offset=0.5*k)
        axis(1)
        axis(2)
               
             
        ## RChemMass::renderSMILES.rcdk(smiles[[i]],coords=c(x1,y1,x2,y2))
        gc()
    }
    dev.off()
}
    
##' Interface to vectorised Mass Bank workflow.
##'
##' 
##' @title Vectorised Mass Bank Workflow
##' @param mb List of mass bank workflow objects
##' @param infodir List of subdirs containing info lists.
##' @param fn_stgs List of settings files.
##' @return A named list of mbWorkspace objects. The names are derived
##'     from the input mb sequence.
##' @author Todor Kondić
mb.v<-function(mb,infodir,fn_stgs) {
    f<-Vectorize(mb.single,vectorize.args=c("mb","infodir","fn_stgs"),SIMPLIFY=F)
    x<-f(mb,infodir,fn_stgs)
    names(x)<-names(mb)
    x}

##' Interface to parallelised Mass Bank workflow.
##'
##' 
##' @title Parallel Mass Bank Workflow
##' @param mb List of mass bank workflow objects
##' @param infodir List of subdirs containing info lists.
##' @param fn_stgs List of settings files.
##' @param cl Cluster.
##' @return A named list of mbWorkspace objects. The names are derived
##'     from the input mb sequence.
##' @author Todor Kondić
mb.p<-function(mb,infodir,fn_stgs,cl=F) {
    x<-parallel::clusterMap(cl=cl,mb.single,mb,infodir,fn_stgs)    
    names(x)<-names(mb)
    x}

##' Prescreening using shiny interface.
##'
##' @title Prescreening with Shiny 
##' @return Nothing useful. 
##' @author Jessy Krier
##' @author Mira Narayanan
presc.shiny <-function(wd,mode,pal="Dark2",cex=0.75,rt_digits=2,m_digits=4){
    modemap=list(pH="MpHp_mass",
                 mH="MmHm_mass",
                 blahnh4="MpNH4_mass",
                 blahna="MpNa_mass")

    default_min_rt=0
    default_max_rt=40
    dfdir <- file.path(wd,"prescreen")

    wd1 <- wd[[1]]
    df <- read.csv(file=get_cmpd_l_fn(wd1),stringsAsFactors = F)
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
    eics <- list.files(path=dfdir[[1]],patt=".*eic.csv")
    maybekids <- sapply(strsplit(eics,split="\\."),function(x) {paste(x[[1]][1],'.kids.csv',sep='')})
    idsliderrange <- range(df$ID)
    tabPanelList <- lapply(1:6, function(tag) {
        shiny::tabPanel(paste("tag",tag),shiny::checkboxGroupInput("variable", "Checkboxes:",
                                                                   c("MS1" = "MS1 present",
                                                                     "MS2" = "MS2 present",
                                                                     "Alignment" = "Alignment MS1/MS2",
                                                                     "Intensity" = "Intensity is good",
                                                                     "Noise" = "MS is noisy")),
                        shiny::textAreaInput("caption", "Comments:", "Insert your comment here..."),
                        shiny::verbatimTextOutput("value")
                        )
    })
    nvp <- do.call(shiny::navlistPanel, tabPanelList)
    ui <- shinydashboard::dashboardPage(
          shinydashboard::dashboardHeader(title = "Prescreening"),
          shinydashboard::dashboardSidebar(
                              width = 350,
                              shinydashboard::sidebarMenu(
                                                  shinydashboard::sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
                                                  shinydashboard::menuItem(text = "Dashboard", tabName = "Dashboard", icon = shiny::icon("dashboard")))),
          shinydashboard::dashboardBody(
                              shiny::fluidRow(
                                         shinydashboard::box(
                                                             title = "MS Prescreening", width = 8, height = "80px", background = "blue", ""
                                                         ),
                                         shinydashboard::box(
                                                             title = "Compound ID N°",width = 4, height = "80px", background = "olive",
                                                             shiny::textOutput("compoundID")
                                                         )
                                     ),
                              shiny::fluidRow(
                                         shinydashboard::box(
                                                             title = "Plot", width = 8, solidHeader = TRUE, collapsible = TRUE,
                                                             shiny::plotOutput("plot1", width = "100%", height = "750px", click = NULL,
                                                                               dblclick = NULL, hover = NULL, hoverDelay = NULL,
                                                                               hoverDelayType = NULL, brush = NULL, clickId = NULL,
                                                                               hoverId = NULL),
                                                             shiny::textInput("plotname", "Insert plot name: (e.g. plotname_%i.pdf)",value="plotCpdID_%i.pdf"),
                                                             shiny::actionButton("saveplot", "Save", icon = shiny::icon("save"))
                                                         ),
                                         shinydashboard::box(
                                                             title = "Compounds", width=4,solidHeader = TRUE, collapsible = TRUE, "", shiny::br(),
                                                             shiny::sliderInput("idslider", "Compound number:", idsliderrange[1], idsliderrange[2], value=1,step=1)
                                                         ),
                                         shinydashboard::box(
                                                             title = "Plot x axis range", width = 4, solidHeader = TRUE, collapsible = TRUE,
                                                             shiny::numericInput("min_val", "Minimum x Axis Value", default_min_rt),
                                                             shiny::numericInput("max_val", "Maximum x Axis Value", default_max_rt)
                                                         ),                                                     
                                         shinydashboard::box(
                                                             title = "Prescreening Analysis", width = 4, solidHeader = TRUE, collapsible = TRUE,
                                                             shiny::uiOutput("nvp")

                                                         )
                                     )
                          )
          )

    plotall <- function(i,rtrange) {
        eic <- eics[[i]]
        maybekid <- maybekids[[i]]
        fn_ini <- lapply(wd,get_stgs_fn)
        
        lbls <- lapply(fn_ini,function(x) {s <- yaml::yaml.load_file(x);s$prescreen$tag})
        dfs <- lapply(file.path(dfdir,eic),function(fn) {
            tryCatch(read.csv(fn,stringsAsFactors = F),
                     error=function(e) {message(paste(e,"; offending file:",fn))})
        })
        
        dfs <- lapply(dfs,function(x) data.frame(rt=x$rt/60.,intensity=x$intensity))

        ## Find existing children.
        maybes <- file.path(dfdir,maybekid)
        indkids <- which(file.exists(maybes))
        kids <- maybes[indkids]
        dfs_kids <- lapply(kids,read.csv,stringsAsFactors=F)
        dfs_kids <- lapply(dfs_kids,function(x) data.frame(rt=x$retentionTime/60.,intensity= -x$intensity))


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

        
        
        rt_rng <- rtrange #range(sapply(dfs,function(x) x$rt))
        int_rng <- range(sapply(append(dfs_kids,dfs),function(x) x$intensity))
        cols <- RColorBrewer::brewer.pal(n=length(dfs),name=pal)
        lgnd <- Map(function(k,v) paste(k,"= ",formatC(v,format="f",digits=rt_digits),sep=''),symbs,rt_max)
        
        layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE), 
               widths=c(7,8), heights=c(6,6))
        struc_xr <- c(0,100)
        struc_yr <- c(0,100)
        plot(1,1,type="n",xlab="",ylab="",xlim=struc_xr,ylim=struc_yr,xaxt="n",yaxt="n")
        rendersmiles2(osmesi[i],coords=c(struc_xr[1],struc_yr[1],struc_xr[2],struc_yr[2]))
       
        col_eng <- c(0,100)
        peak_int <- c(0,100)
        plot(1,1,type="n",xlab="",ylab="",xlim=col_eng,ylim=peak_int,xaxt="n",yaxt="n",axes = FALSE)
        linfo <- legend("topleft",horiz=T,legend=lbls,col=cols,fill=cols,bty="n",cex=cex)
        legend(x=linfo$rect$left,y=linfo$rect$top-0.5*linfo$rect$h,horiz=T,legend=lgnd,fill=cols,bty='n',cex=cex)
         
        cols_kids <- cols[indkids]
        lgnd_kids <- Map(function(k,v) paste(k,"= ",formatC(v,digits=rt_digits,format="f"),sep=''),symbs_kids,rt_max_kids)
        if (length(lgnd_kids)>0) legend(x=linfo$rect$left,y=linfo$rect$top-1*linfo$rect$h,horiz=T,legend=lgnd_kids,fill=cols[indkids],bty="n",cex=cex)
        plot(1,1,xlab="",ylab="",xlim = rt_rng,ylim = int_rng,type="n")

          ## Plot eic across the directory set.
        for (n in seq(length(dfs))) {
            df <- dfs[[n]]
            col <- cols[[n]]
            lines(df$intensity ~ df$rt,col=col)
        }

        if (length(dfs_kids) >0) {
            for (k in 1:length(indkids)) {
                lines(intensity ~ rt,data=dfs_kids[[k]],type="h",col=cols_kids[[k]])
            }
        }
        title(main=paste("ID:",i,"Ion m:",formatC(masses[[i]],digits=m_digits,format="f")),xlab="retention time [min]",ylab="intensity")
        for (k in seq(length(w_max))) text(rt_max[[k]],i_max[[k]],labels=symbs[[k]],pos=4,offset=0.5*k)
        if (length(dfs_kids)>0) for (k in seq(length(w_max_kids))) text(rt_max_kids[[k]],i_max_kids[[k]],labels=symbs_kids[[k]],pos=4,offset=0.5*k)
        axis(1)
        axis(2)
               
             
        ## RChemMass::renderSMILES.rcdk(smiles[[i]],coords=c(x1,y1,x2,y2))
        gc()
       
    }
    clean_rtrange <- function(rtrange) {
            x1 <- rtrange[1]
            x2 <- rtrange[2]
            if (is.na(x1)) x1 <- default_min_rt
            if (is.na(x2)) x2 <- default_max_rt

            c(x1,x2)
        }
    server <- function(input, output, session) {
        
        output$plot1 <- renderPlot(
        {
            i=input$idslider

            rtrange <- c(input$min_val,input$max_val)
            plotall(i,rtrange=clean_rtrange(rtrange))


        }
        )

        output$value <- renderText(
        {
            input$caption
        })

        output$compoundID <- renderText(
        {
            i=input$idslider
        })
        
        shiny::observeEvent(input$saveplot,{
            i=input$idslider
            
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotCpdID_%i.pdf"
            fn <- sprintf(pfn,i)
            rtrange <- c(input$min_val,input$max_val)
            pdf(file=fn, width=12, height=8)
            plotall(i,rtrange=clean_rtrange(rtrange))
            dev.off()
        })

        output$nvp <- shiny::renderUI({nvp})

        session$onSessionEnded(function() {
            stopApp()
        })
    }
    
    shiny::shinyApp(ui = ui, server = server)
}

