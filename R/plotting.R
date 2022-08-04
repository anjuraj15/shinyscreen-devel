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




data4plot_ms1_cgram <- function(tab,select=dtable(adduct=character(0),ID=character(0))) {
    res <- tab[select,.(adduct,tag,ID,rt,intensity),on=c('ID','adduct'),nomatch=NULL]
    if (NROW(res) == 0) return(res)
    data.table::setkeyv(res,c('ID','adduct','tag','rt'))
    res[,max_int:=max(intensity),by=c('ID','adduct','tag')]
    res[,rt_at_max:=rt[which(max_int == intensity)],by=c('ID','adduct','tag')]
    res[,`:=`(lab_id=factor(ID),
              lab_adduct=factor(adduct),
              lab_tag=factor(tag),
              lab_adduct_break = paste0(adduct,":",tag),
              lab_adduct_tag = paste0(adduct,', ',tag))]
    
    
    res
}

data4plot_ms2_cgram <- function(tab,select=dtable(adduct=character(0),ID=character(0))) {
    res <- tab[select,.(adduct,tag,ID,CE,an,rt,intensity),on=c('ID','adduct'),nomatch=NULL]
    if (NROW(res) == 0) return(res)
    data.table::setkeyv(res,c('ID','adduct','tag','CE','an'))
    ## NOTE: I used to not have 'an' in keyby here, but that obviously
    ## generates bad MS2 chromatogram. So, why?
    res <- res[,.(rt,intensity=max(intensity)),keyby=c('ID','adduct','tag','CE','an')]
    res[,`:=`(lab_id=factor(ID),
              lab_adduct=factor(adduct),
              lab_tag=factor(tag),
              lab_adduct_break = paste0(adduct,":",tag))]
    ## Create a table where NCE counts the number of CEs per tab.
    tmp <- res[,.(CE=unique(CE)),keyby=c("ID","adduct","tag")]
    tmp <- tmp[,.(CE=CE,NCE=.N),by=c("ID","adduct","tag")]
    
    tmp[,lab_ce:=fifelse(NCE>0,as.character(CE),NA_character_),by=c("ID","adduct","tag")]
    res <- res[tmp,on=c("ID","adduct","tag","CE")]
    res
}

data4plot_ms2_spec <- function(tab,qatab,select=dtable(adduct=character(0),ID=character(0))) {
    fullkeys <- c('ID','adduct','tag','CE','an')
    select <- qatab[select,on=c('adduct','ID'),nomatch=NULL]
    select <- select[ms2_sel==T,..fullkeys]
    setkeyv(select,fullkeys)
    res <- tab[select,.(adduct,tag,ID,CE,an,rt,mz,intensity),on=fullkeys,nomatch=NULL]
    if (NROW(res) == 0) return(res)
    ## Create a table where NCE counts the number of CEs per tab.
    tmp <- res[,.(CE=unique(CE)),keyby=c("ID","adduct","tag")]
    tmp <- tmp[,.(CE=CE,NCE=.N),by=c("ID","adduct","tag")]
    tmp[,lab_ce:=fifelse(NCE>0,as.character(CE),NA_character_),by=c("ID","adduct","tag")]
    res <- res[tmp,on=c("ID","adduct","tag","CE")]
    res <- res[,.(NCE,lab_ce,rt,mz,intensity),keyby=fullkeys]
    
    res[,`:=`(lab_id=factor(ID),
              lab_adduct=factor(adduct),
              lab_tag=factor(tag),
              lab_adduct_break = paste0(adduct,":",tag),
              lab_adduct_tag = paste0(adduct,', ',tag))]
    res
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

plot_text <- function(text) {
    theme <- ggplot2::theme_bw() + ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                                                  axis.ticks.x=ggplot2::element_blank(),
                                                  axis.text.y=ggplot2::element_blank(),
                                                  axis.ticks.y=ggplot2::element_blank())
    p <- ggplot2::ggplot(data.frame(x=1:10,y=1:10),
                         ggplot2::aes(x=x,y=y))+
        ggplot2::geom_blank()+ggplot2::labs(x="",y="")

    p <- p + ggplot2::annotate(geom="text",
                               x=5,
                               y=5,
                               size=6,
                               label=text,
                               color="black") + theme

    p
    
}


plot_theme <- function(base_size = 14,
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       ...) ggplot2::theme_bw(base_size = base_size) +
                                ggplot2::theme(panel.grid.major=panel.grid.major,
                                               panel.grid.minor=panel.grid.minor)


plot_base <- function(pdata,aes,labs,geom) {
    ggplot2::ggplot(pdata,mapping = aes()) +
        geom() +
        labs +
        plot_theme()
}




plot_palette <- function(pdata) {
    breakslabs  <- pdata[,unique(.SD),.SDcol=c("lab_adduct_break","lab_adduct_tag")]
    breaks <- breakslabs$lab_adduct_break
    labels <- breakslabs$lab_adduct_tag
    values <- pal_maker(length(breaks),palname="Paired")
    names(values) <- breaks
    list(breaks=breaks,
         labels=labels,
         values=values)
}
plot_eic_w_facet <- function(pdata_ms1,pdata_ms2,rt_range,palette) {
    if (NROW(pdata_ms1)==0) return(NULL)
    aes <- function () ggplot2::aes(x=rt,
                                    y = intensity)
    
    aes_ms1 <- function() ggplot2::aes(## y = intensity,
                                       colour = lab_adduct_break)
    aes_ms2 <- if (NROW(pdata_ms2)>0) {
                   if (pdata_ms2[,all(NCE==1)]) {
                       function() ggplot2::aes(colour = lab_adduct_break,
                                               ymin = 0,
                                               ymax = intensity)
                   } else {
                       function() ggplot2::aes(colour = lab_adduct_break,
                                               linetype = lab_ce,
                                               ymin = 0,
                                               ymax = intensity)
                   }
               } else function() ggplot2::aes()

    
    
    scale_colour <- function(name,...) ggplot2::scale_colour_manual(values = palette$values,
                                                                    breaks = palette$breaks,
                                                                    labels = palette$labels,
                                                                    name = name,...)



    pdata_ms1$plottype <- "MS1 EIC"
    pdata_ms2$plottype <- "MS2 EIC"

    labs <- ggplot2::labs(x="retention time [min]",
                          y="intensity")

    
    obj <- ggplot2::ggplot(pdata_ms1, aes()) +
        ggplot2::geom_line(data = pdata_ms1,
                           key_glyph = KEY_GLYPH,
                           aes_ms1())

    if (NROW(pdata_ms2)>0) {
     obj <- obj + ggplot2::geom_linerange(data = pdata_ms2,
                                          aes_ms2()) 
    }

    obj + labs  + scale_y(labels=sci10) + scale_colour(name = "MS1") +
        ggplot2::facet_grid(plottype ~ ID) +
        ggplot2::coord_cartesian(xlim = rt_range) +
        plot_theme()

}


plot_spec_w_facet <- function(pdata_ms2,mz_range,palette) {

    if (NROW(pdata_ms2)==0) return(NULL)
    
    aes_ms2 <- if (NROW(pdata_ms2)>0) {
                   if (pdata_ms2[,all(NCE==1)]) {
                       function() ggplot2::aes(colour = lab_adduct_break,
                                               x = mz,
                                               ymin = 0,
                                               ymax = intensity)
                   } else {
                       function() ggplot2::aes(colour = lab_adduct_break,
                                               linetype = lab_ce,
                                               x = mz,
                                               ymin = 0,
                                               ymax = intensity)
                   }
               } else function() ggplot2::aes()

    breakslabs <- pdata_ms2[,unique(.SD),.SDcol=c("lab_adduct_break","lab_adduct_tag")]
    breaks <-  breakslabs$lab_adduct_break
    labels <- breakslabs$lab_adduct_tag
    
    scale_colour <- function(name,...) ggplot2::scale_colour_manual(values = palette$values,
                                                                    breaks = palette$breaks,
                                                                    labels = palette$labels,
                                                                    name = name,...)
    

    pdata_ms2$plottype <- "MS2 SPECTRA"

    labs <- ggplot2::labs(x="mz",
                          y="intensity")

    

    obj <- if (NROW(pdata_ms2)>0) {
        ggplot2::ggplot(pdata_ms2, aes_ms2()) +
            ggplot2::geom_linerange(key_glyph=KEY_GLYPH) + labs  + scale_y(labels=sci10) +
            scale_colour(name = "MS2") + ggplot2::facet_grid(plottype ~ ID) +
            ggplot2::coord_cartesian(xlim = mz_range) +
            plot_theme()
           } else NULL

    obj

}


## Table legends.

table_eic <- function(pdata) {
    tbl <- pdata[,.(rt=first(rt_at_max)),by=c("ID","adduct","tag")]
    tbl$rt <- format(tbl$rt,digits = 5)
    data.table::setnames(tbl,old = c("adduct","tag","rt"),new = c("Adduct","Tag","RT (MS1) [min]"))
    tbl
}

table_spec <- function(pdata) {
    tbl <- pdata[,.(rt=first(rt)),by=c("ID","adduct","tag","CE")]
    tbl$rt <- format(tbl$rt,digits = 5)
    data.table::setnames(tbl,old = c("adduct","tag","rt"),new = c("Adduct","Tag","RT (MS2) [min]"))
    tbl
}

plot_fname_prefix <- function(decotab,proj_path,subdir=FIG_TOPDIR) {
    if (NROW(decotab)==0) return()
    adducts <- decotab[,adduct]
    ids <- decotab[,ID]
    rpls <- list("\\["="","\\]"="","\\+"="p","\\-"="m")
    fname<-"plot_adduct_"
    for (adduct in adducts) {
        chunk <- adduct
        for (rp in names(rpls)) chunk <- gsub(rp,rpls[[rp]],chunk)
        fname <- paste(fname,chunk,sep = "_")
            
    }
    ddir <- file.path(proj_path,subdir)
    if (!dir.exists(ddir)) dir.create(ddir,recursive = T)
    
    fname <- paste0(fname,"__id_")
    fname <- paste0(fname,paste(ids,collapse = "_"))

    fname <- file.path(ddir,fname)
    fname
    
}

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




## NEW BEGINNINGS


theme_eic <- function(...) theme_light()+ggplot2::theme(axis.title=ggplot2::element_text(size=15L),
                                                        axis.text=ggplot2::element_text(size=12L,colour="black"),
                                                        legend.title=ggplot2::element_text(size=15L),
                                                        plot.title=ggplot2::element_text(size=15L),
                                                        plot.subtitle=ggplot2::element_text(size=12L),
                                                        legend.text=ggplot2::element_text(size=12L),
                                                        plot.caption=ggplot2::element_text(size=12L),...)

theme_empty <- ggplot2::theme_bw()
theme_empty$line <- ggplot2::element_blank()
theme_empty$rect <- ggplot2::element_blank()
theme_empty$strip.text <- ggplot2::element_blank()
theme_empty$axis.text <- ggplot2::element_blank()
theme_empty$plot.title <- ggplot2::element_blank()
theme_empty$axis.title <- ggplot2::element_blank()

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

scale_y<- function (axis="linear", ...) if (axis!="log") {
                                            ggplot2::scale_y_continuous(...)
                                        } else {
                                            ggplot2::scale_y_log10(...)
                                        }


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
    eval(bquote(tab[.(skey)]))
}


get_label_group <- function(key) {
    message('CINDEX:',paste0(CINDEX_BY,coll=','))
    message('key:', paste0(key,coll=','))
    setdiff(CINDEX_BY,key)
}

make_line_label <- function(...) {
    paste(...,sep="; ")
}

## Prepare MS1 eic data: rt and intensity of a subset of extracted
## data defined by the key named list. Argument `labs' is a vector of
## names that will be used to construct the legend labels.
get_data_4_eic_ms1 <- function(extr_ms1,kvals,labs) {

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
    ## Create labels.
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



make_eic_ms1_plot <- function(extr_ms1,summ,kvals,labs,axis="linear",rt_range=NULL, asp=0.5) {

   
    ## Get the table with ms1 data.
    pdata <- get_data_4_eic_ms1(extr_ms1, kvals, labs)
    ## Get metadata.
    summ_row  <- get_data_from_key(summ,key=kvals)
    key <- names(kvals)
    ## Deal with retention time range.
    rt_lim <- if (is.null(rt_range)) NULL else ggplot2::xlim(rt_range)
    xrng <- if (!is.null(rt_range)) rt_range else range(pdata$rt)
    dx <- abs(xrng[[2]]-xrng[[1]])
    yrng <- range(pdata$intensity)
    dy <- abs(yrng[[2]]-yrng[[1]])

    ## Calculate aspect ratio.
    aspr <- if (dx < .Machine$double.eps) 1 else asp*as.numeric(dx)/as.numeric(dy)


    tag_txt = paste0(sapply(names(kvals),function (nx) paste0(nx,": ", kvals[[nx]])),
                     collapse='; ') ## paste0("Set: ", set, " ID: ",id)
    title_txt = paste0("MS1 EIC for ion m/z = ",paste0(signif(unique(summ_row$mz),digits=7L),collapse=", "))
    nm <- paste(unique(summ_row$Name),collapse="; ")
    subt_txt = if (!length(nm)==0L && !is.na(nm) && nchar(nm)>0L) nm else NULL
    p <- ggplot2::ggplot(pdata,aes(x=rt,y=intensity,colour=label))+ggplot2::labs(caption=tag_txt,title=title_txt,subtitle=subt_txt)+ggplot2::xlab("retention time")+ggplot2::geom_line()+scale_y(axis=axis,labels=sci10)+rt_lim
    ## +ggplot2::coord_fixed(ratio=aspr)
    annt_dx <- 5*dx/100.
    annt <- summ[summ_row,on=key,nomatch=NULL][,.(x=..annt_dx+ms1_rt,y=ms1_int,txt=signif(ms1_rt,5))]

    ## Annotate.
    p <- p + annotate("text",x=annt$x,y=annt$y,label=annt$txt,size=4,check_overlap=T)

    ## Add theme.
    p + theme_eic()
}


make_eic_ms2_plot <- function(summ,kvals,labs,axis="linear",rt_range=NULL,asp=0.5) {
    ## TODO
    ## Get plotting data for the compound.
    pdata <- get_data_4_eic_ms2(summ,
                                kvals=kvals,
                                labs=labs)

    if (NROW(pdata)==0L) return(NULL)

    ## Get metadata.
    summ_row  <- get_data_from_key(summ,key=kvals)

    ## Deal with retention time range.
    rt_lim <- if (is.null(rt_range)) NULL else ggplot2::xlim(rt_range)
    xrng <- if (!is.null(rt_range)) rt_range else range(pdata$rt)
    dx <- abs(xrng[[2]]-xrng[[1]])
    yrng <- range(pdata$intensity)
    dy <- abs(yrng[[2]]-yrng[[1]])

    ## Fix aspect ratio.
    aspr <- if (dx < .Machine$double.eps) 1 else asp*as.numeric(dx)/as.numeric(dy)

    ## Derive various labels.
    tag_txt = paste0(sapply(names(kvals),function (nx) paste0(nx,": ", kvals[[nx]])),
                     collapse='; ')
    title_txt = paste0("MS2 EIC for ion m/z = ",paste0(signif(unique(summ_row$mz),digits=7L),collapse=", "))
    subt_txt = if (!length(summ_row$Name)==0L && !is.na(summ_row$Name) && nchar(summ_row$Name)>0L) summ_row$Name else NULL
    ## Base plot.
    p <- ggplot2::ggplot(pdata,aes(x=rt,ymin=0,ymax=intensity,colour=label)) +
        ggplot2::labs(caption=tag_txt,title=title_txt,subtitle=subt_txt) +
        ggplot2::xlab("retention time")+ggplot2::ylab("intensity")+ggplot2::geom_linerange()+
        scale_y(axis=axis,labels=sci10)+rt_lim
    ## ggplot2::coord_fixed(ratio=aspr)+
    ans <- pdata[,unique(an)]
    annt_dx <- 5*dx/100.
    annt <- summ[an %in% (ans),.(an=an,x=ms2_rt+..annt_dx,y=1.1*ms2_int,txt=signif(ms2_rt,5))]
    ## Annotate.
    p <- p + annotate("text",x=annt$x,y=annt$y,label=annt$txt,size=3,check_overlap=T)

    ## Add theme.
    p + theme_eic()
}


make_spec_ms2_plot <- function(extr_ms2,summ,set,adduct,id,splitby,axis="linear") {

    ## Get metadata.
    key <- list(set=set,
                adduct=adduct,
                ID=id)
    
    ## Only the chosen ones.
    mdata  <- get_data_from_key(summ,key=key)[ms2_sel==T]
    ans <- mdata[,unique(an)]
    pdata <- extr_ms2[an %in% ans,.(mz=mz,intensity=intensity,rt=signif(unique(rt),5)),by=c("adduct","tag","CE")]
    pdata <- eval(bquote(pdata[,label:=make_line_label(..(lapply(c(splitby,"rt"),as.symbol))),by=.(splitby)],splice=T))
    pdata <- pdata[,.(mz=mz,intensity=intensity,label=label)]

    # Aspect ratio.
    xrng <- range(pdata$mz)
    dx <- abs(xrng[[2]]-xrng[[1]])
    yrng <- range(pdata$intensity)
    dy <- abs(yrng[[2]]-yrng[[1]])
    aspr <- if (dx < .Machine$double.eps) 1 else 0.5*as.numeric(dx)/as.numeric(dy)

    ## Get labels.
    tag_txt = paste0("Set: ", set, " ID: ",id)
    title_txt = paste0("MS2 spectra for ion m/z = ",paste0(signif(unique(mdata$mz),digits=7L),collapse=", "))
    nm <- paste(unique(mdata$Name),collapse="; ")
    subt_txt = if (!length(nm)==0L && !is.na(nm) && nchar(nm)>0L) nm else NULL

    p <- ggplot2::ggplot(pdata,aes(x=mz,ymin=0,ymax=intensity,colour=label))+ggplot2::labs(caption=tag_txt,title=title_txt,subtitle=subt_txt)+ggplot2::xlab("m/z")+ggplot2::geom_linerange()+ggplot2::coord_fixed(ratio=aspr)+scale_y(axis=axis,labels=sci10)

    ## Add theme.
    p + theme_eic()
 
}

combine_plots <- function(p_eic_ms1,p_eic_ms2) {
    cowplot::plot_grid(p_eic_ms1,p_eic_ms2,ncol=1,align='v',axis='b')
}

    
    
