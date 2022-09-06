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

plot_save_single <- function(plot,decotab,extension,proj_path,subdir=FIG_TOPDIR,tabl=NULL,figtag="") {
    if (is.null(plot)) return()
    
    fname <- plot_fname_prefix(decotab,proj_path,subdir=subdir)
    
    fnplot <- paste0(fname,"__",figtag,".",extension)
    
    if (extension == "rds" || extension == "RDS") {
        saveRDS(plot,file=fnplot)
    } else ggplot2::ggsave(filename = fnplot,
                           plot = plot)
    fntab <- paste0(fname,"__",figtag,".csv")
    if (! is.null(tabl)) data.table::fwrite(tabl,file=fntab,sep = ",")
    list(plot=plot,tab=tabl,fn_plot=fnplot)
    
}




## PLOTTING

### PLOTTING: HELPERS

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

scale_legend <- function(colrdata,pdata) {
    if (is.null(colrdata) || is.null(pdata)) NULL
    labs <- data.table::key(colrdata)
    message("labslabs")
    print(labs)
    sdcols <- c(labs,"label")
    print("pdata")
    print(names(pdata))
    print("namesnames")
    print(names(colrdata))
    tab_lab <- pdata[,unique(.SD),.SDcols=sdcols][colrdata,.(colour=i.colour),on=labs,nomatch=NULL]
    print("tablab")
    print(tab_lab)
    print("-------")
    x <- tab_lab$colour
    names(x) <- tab_lab$label
    scale_colour_manual(values=x)
}

pal_maker <- function(n,palname = NULL) {
    ## The silliest implementation possible. There may be cases when
    ## user requires more than the number of colours accessible in any
    ## ColorBrewer palette. In we need to automatically generate
    ## colours. There are no guarantees the new colours look nice
    ## together with the rest of the palette, so the idea is to fit
    ## the first colours to the original palette, then go over to the
    ## generated ones. Returns a vector of colours.

    krzywinski <- c("#68023F","#008169","#EF0096","#00DCB5",
                    "#FFCFE2","#003C86","#9400E6","#009FFA",
                    "#FF71FD","#7CFFFA","#6A0213","#008607")
    info <- as.data.table(RColorBrewer::brewer.pal.info,keep.rownames = T)
    maxcol <- if (!is.null(palname)) info[rn == palname]$maxcolors else length(krzywinski)
    startpal <- if (!is.null(palname)) RColorBrewer::brewer.pal(maxcol,palname) else krzywinski
    pal <- if (n>length(startpal)) {
               intrppal <-(colorRampPalette(startpal))(n)
               newcol <- setdiff(intrppal,startpal)
               unique(c(startpal,intrppal))
           } else startpal

    pal[1:n]
    
    
}

### PLOTTING: AESTHETIC FUNCTIONS
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

guide_fun <- function() {
    ## ggplot2::guides(colour=ggplot2::guide_legend(nrow=2,
    ##                                              byrow=T,
    ##                                              override.aes=list(shape=15)))
    NULL
}

theme_eic <- function(...) theme_light()+ggplot2::theme(axis.title=ggplot2::element_text(size=15L),
                                                        axis.text=ggplot2::element_text(size=12L,colour="black"),
                                                        legend.title=ggplot2::element_text(size=15L),
                                                        plot.title=ggplot2::element_text(size=15L),
                                                        plot.subtitle=ggplot2::element_text(size=12L),
                                                        legend.text=ggplot2::element_text(size=12L),
                                                        plot.caption=ggplot2::element_text(size=12L),
                                                        legend.position='bottom',...)+guide_fun()


theme_print <- function(...) theme_light()+ggplot2::theme(axis.title=ggplot2::element_text(size=5L),
                                                        axis.text=ggplot2::element_text(size=4L,colour="black"),
                                                        plot.title=ggplot2::element_text(size=5L),
                                                        plot.subtitle=ggplot2::element_text(size=4L),
                                                        legend.text=ggplot2::element_text(size=4L),
                                                        plot.caption=ggplot2::element_text(size=7L),
                                                        legend.position='bottom',
                                                        legend.spacing=ggplot2::unit(0.5,'lines'),
                                                        legend.key.size=unit(0.5,'lines'),
                                                        legend.title=ggplot2::element_blank(),
                                                        ...)+guide_fun()
                               

theme_empty <- ggplot2::theme_bw()
theme_empty$line <- ggplot2::element_blank()
theme_empty$rect <- ggplot2::element_blank()
theme_empty$strip.text <- ggplot2::element_blank()
theme_empty$axis.text <- ggplot2::element_blank()
theme_empty$plot.title <- ggplot2::element_blank()
theme_empty$axis.title <- ggplot2::element_blank()


cust_geom_line <- function(key_glyph="rect",...) geom_line(...,key_glyph=key_glyph)
cust_geom_linerange <- function(key_glyph="rect",...) geom_linerange(...,key_glyph=key_glyph)



scale_y<- function (axis="linear", ...) if (axis!="log") {
                                            ggplot2::scale_y_continuous(...)
                                        } else {
                                            ggplot2::scale_y_log10(...)
                                        }


### PLOTTING: DATA WRANGLING

## Concatenates items of a named list in a chain of &-s: x==xval &
## y=yval & z=zval ...)
mk_logic_exp <- function(rest,sofar=NULL) {
    if (length(rest)==0L) {
        return(sofar)
    } else {
        nm = names(rest)[[1]]
        val = rest[[1]]
        ex <- bquote(.(as.symbol(nm)) %in% .(val))
        zz <- if (is.null(sofar)) ex else bquote(.(ex) & .(sofar))
        mk_logic_exp(tail(rest,-1L), zz)
    }
}


get_data_from_key <- function(tab,key) {
    skey <- mk_logic_exp(key)
    tab <- eval(bquote(tab[.(skey)]))
    setkeyv(tab,names(key))
    tab
}


get_label_group <- function(key) {
    message('CINDEX:',paste0(CINDEX_BY,coll=','))
    message('key:', paste0(key,coll=','))
    setdiff(CINDEX_BY,key)
}

make_line_label <- function(...) {
    paste(...,sep="; ")
}



## Define a table which matches labelling columns to colours for
## plotting.
define_colrdata <- function(comptab,labs) {
    ## Determine colours based on `labs'. 
    one_keyset <- function(dt) {
        labtab = dt[,unique(.SD),.SDcol=labs]
        n <- NROW(labtab)
        cols <- if (n<13L) {
                    pal <- RColorBrewer::brewer.pal(n=n,name="Paired") 
                    if (n>3L) pal else if (n>0L) pal[1:n] else character()
                } else {
                    scales::viridis_pal()(n)
                }
        labtab[,colour:=(cols)]
        labtab
    }

    ## Calculate lengths of all the COLRDATA_KEY subgroups.
    dt <- comptab[,unique(.SD),.SDcols=labs,by=COLRDATA_KEY]

    ## Arrange colours to map to specific labels by sorting.
    allcols <- union(COLRDATA_KEY,labs)
    data.table::setkeyv(dt,allcols)
    print(dt)

    ## Assign colours to labels subgroups.
    res <- dt[,one_keyset(.SD),by=COLRDATA_KEY]

    ## Sort everything again, 
    data.table::setkeyv(res,allcols)
    res
}


## Narrow to a specific subset that will be plotted together (eg, one
## compound set.).
narrow_colrdata <- function(colrdata,kvals) {
    if (is.null(colrdata)) return(NULL)
    theset <- kvals[[COLRDATA_KEY]]
    labs <- data.table::key(colrdata)
    labs <- c(labs[labs!=COLRDATA_KEY],"colour")
    res <- eval(bquote(colrdata[(.(as.symbol(COLRDATA_KEY))==theset),.SD,.SDcol=labs]))
    data.table::setkeyv(res,labs)
    res
}



## Prepare MS1 eic data: rt and intensity of a subset of extracted
## data defined by the key named list. Argument `summ_rows' is a
## subset of the `summ' table based on `kvals'. We need it for rt-s in
## the labels. Argument `labs' is a vector of names that will be used
## to construct the legend labels.
get_data_4_eic_ms1 <- function(extr_ms1,summ_rows,kvals,labs) {

    ## Which of the selected keys are in the extr_ms1? This can be
    ## made more obvious to the user, but note necessary atm.
    keys <- names(kvals)
    actual_key <- intersect(keys,names(extr_ms1))
    actual_kvals <- kvals[actual_key]

    ## Subset extr_ms1 by the actual key.
    tab <-get_data_from_key(tab=extr_ms1,key=actual_kvals)

    ## Group the plot data per label group (ie tags, or adducts, or
    ## both).
    xlxx <- intersect(labs,names(extr_ms1))
    xlxx <- as.character(xlxx)
    pdata <- tab[,.(rt,intensity),by=xlxx]


    ## TODO: FIXME: This fails because summ_rows sux wrt calcing of ms1_rt for labels. #Now, add the RTs in.
    ## pdata[summ_rows,ms1_rt:=signif(i.ms1_rt,5),on=xlxx]

    ## Create labels.
    ## xlxx <- unique(c(xlxx,"ms1_rt"))
    pdata <- eval(bquote(pdata[,label:=make_line_label(..(lapply(xlxx,as.symbol))),by=xlxx],splice=T))
    setkeyv(pdata,cols=unique(as.character(xlxx),"rt"))
    pdata
}

## Prepare MS2 eic data: rt and intensity + key made of splitby.
get_data_4_eic_ms2 <- function(summ,kvals,labs) {
    tab <-get_data_from_key(tab=summ,key=kvals)
    nms <- names(kvals)
    byby <- unique(c(nms,labs,"an"))
    pdata <- tab[,.(intensity=ms2_int,rt=ms2_rt),by=byby]
    if (NROW(pdata)==0L) return(NULL)
    xlxx <- as.character(labs)
    pdata <- eval(bquote(pdata[,label:=make_line_label(..(lapply(xlxx,as.symbol))),by=.(xlxx)],splice=T))
    setkeyv(pdata,cols=c(labs,"rt"))
    pdata
}


get_rows_from_summ <- function(summ,kvals,...) {
    summ_rows_cols <- c(names(kvals),...)
    get_data_from_key(summ,key=kvals)[,unique(.SD),.SDcol=summ_rows_cols]
}

narrow_summ <- function(summ,kvals,labs,...) {
        keys <- names(kvals)
        ## keys <- keys[!is.na(keys)]
        needed <- setdiff(labs,keys)
        x <- as.list(c(needed,...))
        x <- c(list(summ,kvals),x)
        do.call(get_rows_from_summ,x)
}


### PLOTTING: TOP-LEVEL PLOT CREATION

make_eic_ms1_plot <- function(extr_ms1,summ,kvals,labs,axis="linear",rt_range=NULL,i_range=NULL, asp=1,colrdata=NULL) {
    key <- names(kvals)
    ## Get metadata.

    ## TODO: FIXME: Somehow calculating representationve ms1_rt for
    ## plots is wrong. Horrible and wrong. Will remove those labels
    ## until we fix.
    summ_rows <- narrow_summ(summ,kvals,labs,"mz","ms1_rt","ms1_int","Name","SMILES","Formula","qa_ms1_exists","an","ms2_sel")
    rows_key <- union(data.table::key(summ_rows),labs)
    summ_rows$sel_ms1_rt=NA_real_
    summ_rows[ms2_sel==T,sel_ms1_rt:=ms1_rt[which.max(ms1_int)],by=rows_key]
    summ_rows[is.na(sel_ms1_rt) & ms2_sel==F & qa_ms1_exists==T,sel_ms1_rt:=ms1_rt[which.max(ms1_int)],by=rows_key]
    summ_rows[,ms1_rt:=sel_ms1_rt]
    summ_rows[,sel_ms1_rt:=NULL]
    summ_rows[,c("an","qa_ms1_exists","ms2_sel"):=NULL]
    summ_rows <- summ_rows[,unique(.SD)]
    
    ## Get the table with ms1 data.
    pdata <- get_data_4_eic_ms1(extr_ms1, summ_rows, kvals, labs)


    ## Deal with retention time range.
    coord <- if (is.null(rt_range) && is.null(i_range)) {
                 NULL
             } else {
                 ggplot2::coord_cartesian(xlim=rt_range,
                                          ylim=i_range)
             }
    xrng <- range(pdata$rt) #if (!is.null(rt_range)) rt_range else range(pdata$rt)
    dx <- abs(xrng[[2]]-xrng[[1]])
    yrng <- range(pdata$intensity)
    dy <- abs(yrng[[2]]-yrng[[1]])

    ## Calculate aspect ratio.
    aspr <- if (dx < .Machine$double.eps) 1 else asp*as.numeric(dx)/as.numeric(dy)

    tag_txt = paste0(sapply(names(kvals),function (nx) paste0(nx,": ", kvals[[nx]])),
                     collapse='; ') ## paste0("Set: ", set, " ID: ",id)
    title_txt = paste0("MS1 EIC for ion m/z = ",paste0(signif(unique(summ_rows$mz),digits=7L),collapse=", "))
    nm <- paste(unique(summ_rows$Name),collapse="; ")
    subt_txt = if (!length(nm)==0L && !is.na(nm) && nchar(nm)>0L) nm else NULL
    p <- ggplot2::ggplot(pdata,aes(x=rt,y=intensity,colour=label))+
        ggplot2::labs(caption=tag_txt,title=title_txt,subtitle=subt_txt)+
        ggplot2::xlab("retention time")+
        cust_geom_line()+
        scale_y(axis=axis,labels=sci10)+
        coord
    ## annt_dx <- 5*dx/100.
    ## annt <- summ_rows[,.(x=..annt_dx+ms1_rt,y=ms1_int,txt=signif(ms1_rt,5))]
    ## ## Annotate.
    ## p <- p + annotate("text",x=annt$x,y=annt$y,label=annt$txt,size=4,check_overlap=T)+guide_fun()

    ## Add theme.
    colrdata <- narrow_colrdata(colrdata,kvals)
    p + scale_legend(colrdata,pdata) + theme_eic()
}


make_eic_ms2_plot <- function(summ,kvals,labs,axis="linear",rt_range=NULL,asp=1, colrdata=NULL) {
    ## Get metadata.
    summ_rows <- narrow_summ(summ,kvals,labs,"mz","ms2_rt","ms2_int","Name","SMILES","Formula")

    ## Get plotting data for the compound.
    pdata <- get_data_4_eic_ms2(summ,
                                kvals=kvals,
                                labs=labs)

    if (NROW(pdata)==0L) return(NULL)

    ## Deal with retention time range.
    rt_lim <- if (is.null(rt_range)) NULL else ggplot2::coord_cartesian(xlim=rt_range)#ggplot2::xlim(rt_range)
    xrng <- range(pdata$rt) #if (!is.null(rt_range)) rt_range else range(pdata$rt)
    dx <- abs(xrng[[2]]-xrng[[1]])
    yrng <- range(pdata$intensity)
    dy <- abs(yrng[[2]]-yrng[[1]])

    ## Fix aspect ratio.
    aspr <- if (is.null(dx) || is.na(dx) || dx < .Machine$double.eps) 1 else asp*as.numeric(dx)/as.numeric(dy)
    ## Derive various labels.
    tag_txt = paste0(sapply(names(kvals),function (nx) paste0(nx,": ", kvals[[nx]])),
                     collapse='; ')
    title_txt = paste0("MS2 EIC for ion m/z = ",paste0(signif(unique(summ_rows$mz),digits=7L),collapse=", "))
    subt_txt = if (!length(summ_rows$Name)==0L && !is.na(summ_rows$Name) && nchar(summ_rows$Name)>0L) summ_rows$Name else NULL
    ## Base plot.
    p <- ggplot2::ggplot(pdata,aes(x=rt,ymin=0,ymax=intensity,colour=label)) +
         ggplot2::labs(caption=tag_txt,title=title_txt,subtitle=subt_txt) +
         ggplot2::xlab("retention time")+ggplot2::ylab("intensity")+cust_geom_linerange()+
         scale_y(axis=axis,labels=sci10)+rt_lim+guide_fun()
    ans <- pdata[,unique(an)]
    ## annt_dx <- 5*dx/100.
    ## annt <- summ[an %in% (ans),.(an=an,x=ms2_rt+..annt_dx,y=1.1*ms2_int,txt=signif(ms2_rt,5))]
    ## ## Annotate.
    ## p <- p + annotate("text",x=annt$x,y=annt$y,label=annt$txt,size=3,check_overlap=T)

    ## Add theme.
    colrdata <- narrow_colrdata(colrdata,kvals)
    p + scale_legend(colrdata,pdata) + theme_eic()
}


make_spec_ms2_plot <- function(extr_ms2,summ,kvals,labs,axis="linear",asp=1, colrdata=NULL) {

    
    ## Only the chosen ones.
    mdata  <- get_data_from_key(summ,key=kvals)[ms2_sel==T]
    common_key <- intersect(names(extr_ms2),names(kvals))
    common_vals <- kvals[common_key]
    if (length(common_key) == 0L) return(NULL)
    subxdata <- get_data_from_key(extr_ms2,key=common_vals)
    if (NROW(mdata)==0L) return(NULL)
    if (NROW(subxdata) == 0L) return(NULL)
    ans <- data.table(an=mdata[,unique(an)],key="an")
    ms2ctg <- c(intersect(c(names(kvals),labs),names(extr_ms2)),"CE")
    xlxx <- intersect(as.character(labs),names(extr_ms2))
    common_labels <- unique(c("an",common_key,intersect(names(extr_ms2),labs)))
    pdata <- subxdata[ans,on="an"][,.(mz=mz,intensity=intensity,rt=signif(unique(rt),5)),by=common_labels]
    pdata <- eval(bquote(pdata[,label:=make_line_label(..(lapply(c(xlxx,"rt"),as.symbol))),by=.(xlxx)],splice=T))
    pdata <- pdata[,.(mz=mz,intensity=intensity,label=label)]
    if (NROW(pdata)==0L) return(NULL)
    # Aspect ratio.
    xrng <- range(pdata$mz)
    dx <- abs(xrng[[2]]-xrng[[1]])
    yrng <- range(pdata$intensity)
    dy <- abs(yrng[[2]]-yrng[[1]])
    aspr <- if (dx < .Machine$double.eps) 1 else asp*as.numeric(dx)/as.numeric(dy)

    ## Get labels.
    tag_txt = paste0(sapply(names(kvals),function (nx) paste0(nx,": ", kvals[[nx]])),
                     collapse='; ')
    title_txt = paste0("MS2 spectra for ion m/z = ",paste0(signif(unique(mdata$mz),digits=7L),collapse=", "))
    nm <- paste(unique(mdata$Name),collapse="; ")
    subt_txt = if (!length(nm)==0L && !is.na(nm) && nchar(nm)>0L) nm else NULL

    p <- ggplot2::ggplot(pdata,aes(x=mz,ymin=0,ymax=intensity,colour=label))+ggplot2::labs(caption=tag_txt,title=title_txt,subtitle=subt_txt)+ggplot2::xlab("m/z")+cust_geom_linerange()+scale_y(axis=axis,labels=sci10)+guide_fun()

    ## Add theme.
    colrdata <- narrow_colrdata(colrdata,kvals)
    p + scale_legend(colrdata,pdata) + theme_eic()
 
}

combine_plots <- function(p_eic_ms1,p_eic_ms2,p_spec_ms2,p_struct) {
    cowplot::plot_grid(p_eic_ms1,p_struct,p_eic_ms2,
                       p_spec_ms2,ncol=2,rel_widths=c(2,1),align='v',axis='l')
}



    
    
