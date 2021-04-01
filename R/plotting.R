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


scale_y<- function (axis="linear", ...) if (axis!="log") {
                                            ggplot2::scale_y_continuous(...)
                                        } else {
                                            ggplot2::scale_y_log10(...)
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
    res <- res[,.(rt,intensity=max(intensity)),keyby=c('ID','adduct','tag','CE')]
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

plot_save_single <- function(plot,decotab,extension,proj,tabl,figtag="") {
    if (is.null(plot)) return()
    print(decotab)
    adducts <- decotab[,adduct]
    ids <- decotab[,ID]
    rpls <- list("\\["="","\\]"="","\\+"="p","\\-"="m")
    fname<-"plot_adduct_"
    for (adduct in adducts) {
        chunk <- adduct
        for (rp in names(rpls)) chunk <- gsub(rp,rpls[[rp]],chunk)
        fname <- paste(fname,chunk,sep = "_")
            
    }
    ddir <- file.path(proj,FIG_TOPDIR)
    if (!dir.exists(ddir)) dir.create(ddir,recursive = T)
    
    fname <- paste0(fname,"__id_")
    fname <- paste0(fname,paste(ids,collapse = "_"))

    fname <- file.path(ddir,fname)
    
    fnplot <- paste0(fname,"__",figtag,".",extension)

    if (extension == "rds" || extension == "RDS") {
        saveRDS(plot,file=fn)
    } else ggplot2::ggsave(filename = fnplot,
                           plot = plot)

    fntab <- paste0(fname,"__",figtag,".csv")
    data.table::fwrite(tabl,file=fntab,sep = ",")


}
