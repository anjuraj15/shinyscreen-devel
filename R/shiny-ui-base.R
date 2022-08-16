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

##' @importFrom shiny validate
##' @importFrom promises future_promise
##' @importFrom promises %...>%
react_v <- shiny::reactiveValues
react_f <- shiny::reactive
react_e <- shiny::eventReactive
obsrv <- shiny::observe
obsrv_e <- shiny::observeEvent
isol <- shiny::isolate


celledit_values <- function(col,values,labels=NULL,addna=T) {
    if (is.null(labels)) labels <- values
    if (length(values)==0 || nchar(values)==0) return(character(0))

    
    part1 <- mapply(function (v,l) {
        sprintf("{value: '%s', display: '%s'},",v,l)
    },
    head(values,-1),
    head(labels,-1),
    USE.NAMES = F)
    
    part2 <- sprintf("{value: '%s', display: '%s'}",tail(values,1),tail(labels,1))

    res <- if (length(part1)>0 || length(part2)>0) {
               a1 <- c("{",sprintf("column: %s, ",col),
                       "type: 'list', ",
                       "options: [")
               a2 <- c(part1,part2,"]","}")
               if (addna) c(a1,"{value: 'NA', display: 'NA'},",a2) else c(a1,a2)
                       
           } else character(0)

    as.character(res)
    
}

shinymsg <- function(ui,duration=NULL,type="message",...) showNotification(ui=paste(ui,
                                                                                    Sys.time(),
                                                                                    sep="\n"),
                                                                           duration=duration,
                                                                           type=type,...)
# volumes <- function() c(wd=getwd(), shinyFiles::getVolumes()())
validate1 <- function(expr,msg) shiny::validate(shiny::need(expr,msg))


path2vol <- function(path) {
    ## This function returns shinyFiles compatible volumes.
    splits <- split_path(path)
    file.path(tail(splits,1),'')
}


prim_box<-function(...) {shinydashboard::box(...,
                                             status="primary",
                                             solidHeader=T)}
good_box<-function(...) {shinydashboard::box(...,
                                             status="success",
                                             solidHeader=T)}
err_box<-function(...) {shinydashboard::box(...,
                                            status="danger",
                                            solidHeader=T)}

inact_box<-function(...) {shinydashboard::box(...,
                                            status="danger",
                                            solidHeader=T)}


html<-function(...) {shiny::tags$div(shiny::HTML(...))}

## num_input<-function(...,width=NUM_INP_WIDTH) {shiny::tags$div(id="inline",shiny::textInput(...,width=width))}

num_input <- function(inputId,label,...,width=NUM_INP_WIDTH) {
    shiny::tags$div(style="display:inline-block",
                    shiny::tags$label(label, `for` = inputId),
                    shiny::tags$input(id = inputId, type = "text",style=paste("width:",width,sep = ""),...))
}
num_input_unit <- function(inputId,l1,l2,width=NUM_INP_WIDTH,...) {
    shiny::tags$div(style="display:inline-block",
                    shiny::tags$label(l1, `for` = inputId), 
                    shiny::tags$input(id = inputId, type = "text",style=paste("width:",width,sep = ""),...),
                    shiny::tags$label(paste(" ",l2,sep=""), `for` = inputId))
}

txt_file_input <- function(inputId,input,fileB,label,volumes,default = "") {

    fnobj<-shinyFiles::parseFilePaths(roots = volumes,
                                      selection = input[[fileB]])
    fn <- fnobj[['datapath']]
    
    if (isThingFile(fn)) {
        shiny::textInput(inputId = inputId,
                         label = label,
                         value = fn)
    } else {
        shiny::isolate(currFn <- input[[inputId]])
        if (!isThingFile(currFn)) {
            shiny::textInput(inputId = inputId,
                             label = label,
                             value = default)
        } else {
            shiny::textInput(inputId = inputId,
                             label = label,
                             value = currFn)
        }
    }
    
}

##' @export
mz_input <- function(input_mz,input_unit,width=NUM_INP_WIDTH,height=NUM_INP_HEIGHT,def_mz=0,def_unit="Da",pref="+/-") {
    style <- "display: inline-block; vertical-align:top; width: "
    stylel <- "display: inline-block; vertical-align:top;"
    style=paste0(style,width,"; ")
    shiny::div(shiny::div(style=stylel,
                          shiny::tags$label(pref,`for`=input_mz)),
               shiny::div(style=style,
                          shiny::numericInput(input_mz,
                                              label=NULL,
                                              value = def_mz)),
               shiny::div(style=style,
                          shiny::selectInput(input_unit,
                                             label=NULL,
                                             c("ppm","Da"),
                                             selected=def_unit)))
}

##' @export
rt_input <- function(input_rt,input_unit,width=NUM_INP_WIDTH,width_u=1-NUM_INP_WIDTH,height=NUM_INP_HEIGHT,def_rt=0,def_unit="min",pref="+/-") {
    width=paste0(as.character(width), "%")
    width_u=paste0(as.character(width_u), "%")

    style="display: inline-block; vertical-align:top; width: "
    style=paste0(style,width,"; ")
    stylel <- "display: inline-block; vertical-align:top;"
    styleu <- paste0("display: inline-block; vertical-align:top; color: black; width: ",width_u,";")
    shiny::div(shiny::div(style=stylel,
                          shiny::tags$label(pref,`for`=input_rt)),
               shiny::div(style=style,
                          shiny::numericInput(input_rt,
                                              label=NULL,
                                              value = def_rt)),
               shiny::div(style=styleu,
                          shiny::selectInput(input_unit,
                                             label=NULL,
                                             c("min","s"),
                                             selected=def_unit)))

}

##'@export
rev2list <- function(rv) {
    ## Take reactive values structure and convert them to nested
    ## lists.
    if (class(rv)[[1]] != "reactivevalues")
        rv else lapply(shiny::reactiveValuesToList(rv),rev2list)
}

##' @export
list2rev <- function(lst) {
    ## Take nested named list and create reactive values from it.
    if (class(lst)[[1]] != "list")
        lst else do.call(react_v,lapply(lst,list2rev))
}

mk_roots <- function(wd) local({
    addons <- c("project"=normalizePath(wd,winslash = '/'))
    def_vol <- function() {
             path <- addons[['project']]
             svols <- shinyFiles::getVolumes()()
             vol <- path2vol(path)
             sel <- match(vol,svols)
             res <- names(svols)[[sel]]
             res
         }
    list(set=function (rts) {addons <<- rts},
         get=function () c(addons,shinyFiles::getVolumes()()),
         def_vol=def_vol,
         def_path=function() {
             vol <- def_vol()
             svols <- shinyFiles::getVolumes()()
             pref <- svols[[vol]]
             res <- sub(paste0(pref,'(.*)'),'\\1',addons[["project"]])
             message('Relative path: ',res)
             res
         })
})

#' @export
merge2rev <- function(rev,lst) {
    crawllist <- function(lst,currname=""){
    cls <- class(lst)

    if (cls[[1]]=="list" && length(names(lst)) > 0)
        invisible(lapply(names(lst),
                         function (nm)
                             crawllist(lst[[nm]],
                                       currname=paste0(currname,'[["',nm,'"]]'))))
        
    else {
            currname
        }
    }

    vars <- unlist(crawllist(lst),recursive = T)
    vars
    pref_r <- deparse(substitute(rev))
    pref_l <- deparse(substitute(lst))
    lhs <- paste0(pref_r,vars)
    rhs <- paste0(pref_l,vars)
    exprs <- Map(function (a,b) call("<-",
                                     parse(text=a)[[1]],
                                     parse(text=b)[[1]]),
                 lhs,
                 rhs)
    code <- quote({})
    for (n in 1:length(exprs)) {
        code[[n+1]] <- exprs[[n]]
        
    }
    code
    
}

## Given a data.frame/table, swap internal names for nicer-to-view
## names used in DTs.
style_tab_cols <- function (dt) {
    
    old <- colnames(dt)
    new <- list()
    for (nm in old) {
        new <- c(switch(EXPR = nm,
                        "adduct" = "Adduct",
                        "tag" = "File Tag",
                        "ms1_int" = "I(ms1)",
                        "ms1_rt" = "RT(ms1) [min]",
                        "ms2_int" = "I(ms2)",
                        "ms2_rt" = "RT(ms2) [min]",
                        "ms2_sel" = "Selected?",
                        "qa_ms2_near" ="MS1/MS2 RT match?",
                        "qa_ms2_exists" = "MS2 Exists?",
                        "qa_ms2_good_int" = "Above threshold?",
                        "qa_ms1_exists" = "MS1 Exists?",
                        "qa_ms1_good_int" = "Above threshold?",
                        "qa_ms1_above_noise" = "Not noisy?",
                        nm),new)
    }
    
    rev(new)
}

## Format typical cols with typical digits for a DT `dt`.
style_tab_signif <- function(dt) {
    numcols <- c(mz=SIGNF_MZ,
                 "ms1_int"=SIGNF_I,
                 "ms2_int"=SIGNF_I,
                 "ms1_rt"=SIGNF_RT,
                 "ms2_rt"=SIGNF_RT)
    for (col in names(numcols)) {
        dt <- tryCatch(DT::formatSignif(dt,col,digits = numcols[[col]]),
                       error = function(e) dt)

        }
    dt
}

## A customised DT intended for spec data.
styled_dt <- function(tab,
                      extensions = 'Scroller',
                      scrollY=200L,
                      dom = "t",
                      scroller = T,
                      ordering = F,
                      colnames = style_tab_cols(tab),
                      rownames = F,
                      filter = 'top',
                      ...) {

    options = list(scrollY=scrollY,
                   scrollX=T,
                   dom = "t",
                   deferRender = T,
                   scroller = scroller,
                   ordering = ordering)
    
    dttab <- DT::datatable(tab,
                           options = options,
                           colnames = colnames,
                           rownames = rownames,
                           filter = filter,
                           ...)
    dttab
    style_tab_signif(dttab)
    
}

dt_drop_callback = function (col_adduct,col_set,sets) DT::JS(c(
                                                              "var tbl = $(table.table().node());",
                                                              "var id = tbl.closest('.datatables').attr('id');",
                                                              "function onUpdate(updatedCell, updatedRow, oldValue) {",
                                                              "  var cellinfo = [{",
                                                              "    row: updatedCell.index().row + 1,",
                                                              "    col: updatedCell.index().column,",
                                                              "    value: updatedCell.data()",
                                                              "  }];",
                                                              "  Shiny.setInputValue(id + '_cell_edit:DT.cellInfo', cellinfo);",
                                                              "}",
                                                              "table.MakeCellsEditable({",
                                                              "  onUpdate: onUpdate,",
                                                              "  inputCss: 'my-input-class',",
                                                              sprintf("  columns: [%s, %s],",col_adduct,col_set),
                                                              "  confirmationButton: false,",
                                                              "  inputTypes: [",
                                                              celledit_values(col_adduct,DISP_ADDUCTS),
                                                              ",",
                                                              celledit_values(col_set,sets),
                                                              "  ]",
                                                              "});"))

dt_summ_subset_callback = function () DT::JS(c(
                                              "var tbl = $(table.table().node());",
                                              "var id = tbl.closest('.datatables').attr('id');",
                                              "function onUpdate(updatedCell, updatedRow, oldValue) {",
                                              "  var cellinfo = [{",
                                              "    row: updatedCell.index().row + 1,",
                                              "    col: updatedCell.index().column,",
                                              "    value: updatedCell.data()",
                                              "  }];",
                                              "  Shiny.setInputValue(id + '_cell_edit:DT.cellInfo', cellinfo);",
                                              "}",
                                              "table.MakeCellsEditable({",
                                              "  onUpdate: onUpdate,",
                                              "  inputCss: 'my-input-class',",
                                              "  columns: [1],",
                                              "  confirmationButton: false,",
                                              "  inputTypes: [",
                                              celledit_values('1',SUBSET_VALS,addna=F),
                                              "  ]",
                                              "});"))

## FIXME: order_summ reordering/editing unstable. Therefore temporarily removed.
## dt_order_summ_callback = function () DT::JS(c(
##                                               "var tbl = $(table.table().node());",
##                                               "var id = tbl.closest('.datatables').attr('id');",
##                                               "function onUpdate(updatedCell, updatedRow, oldValue) {",
##                                               "  var cellinfo = [{",
##                                               "    row: updatedCell.index().row + 1,",
##                                               "    col: updatedCell.index().column,",
##                                               "    value: updatedCell.data()",
##                                               "  }];",
##                                               "  Shiny.setInputValue(id + '_cell_edit:DT.cellInfo', cellinfo);",
##                                               "}",
##                                               "table.MakeCellsEditable({",
##                                               "  onUpdate: onUpdate,",
##                                               "  inputCss: 'my-input-class',",
##                                               "  columns: [1],",
##                                               "  confirmationButton: false,",
##                                               "  inputTypes: [",
##                                               celledit_values('1',c('Descending','Ascending')),
##                                               "  ]",
##                                              "});",
##                                              "//pass on data to R",
##                                              "table.on('row-reorder', function(e, details, changes) {",
##                                                  "Shiny.onInputChange(id +'_row_reorder', JSON.stringify(details));",
##                                              "});"))


render_dt <- function(data, server = T) {
    DT::renderDT(data, server = server)
}

dropdown_dt <- function(tab,callback,rownames=F,editable="cell",selection = "none",...) {
    ce_path <- system.file("www", package = "shinyscreen")
    dep <- htmltools::htmlDependency(
      "CellEdit", "1.0.19", ce_path, 
      script = "dataTables.cellEdit.js",
      stylesheet = "dataTables.cellEdit.css", 
      all_files = FALSE)
    tab <- DT::datatable(tab,
                         callback = callback,
                         rownames = rownames,
                         selection = selection,
                         fillContainer = T,
                         ...)
    tab$dependencies <- c(tab$dependencies, list(dep))
    tab
    
}


simple_style_dt <- function(tab,
                            rownames = F,
                            ...) {

    tab <- DT::datatable(tab,
                         rownames = rownames,
                         fillContainer=T,
                         ...)

    tab
}

scroll_style_dt <- function(tab,
                            rownames = F,
                            deferRender = T,
                            options = list(),
                            ...) {
    DT::datatable(tab,
                  rownames=rownames,
                  options= c(list(deferRender = deferRender,
                                scrollY = 400L,
                                scroller = TRUE),
                             options),
                  extensions = 'Scroller',
                  ...)
}

scroll_dropdown_dt <- function(tab,callback,rownames=F,editable="cell",selection = "none",...) {
    ce_path <- system.file("www", package = "shinyscreen")
    dep <- htmltools::htmlDependency(
      "CellEdit", "1.0.19", ce_path, 
      script = "dataTables.cellEdit.js",
      stylesheet = "dataTables.cellEdit.css", 
      all_files = FALSE)
    tab <- scroll_style_dt(tab,
                           callback = callback,
                           rownames = rownames,
                           selection = selection,
                           ...)
    tab$dependencies <- c(tab$dependencies, list(dep))
    tab
    
}


mk_shinyscreen_server <- function(projects,init) {
    ## This used to be context='setup'.
    ## library(shinydashboard)
    def_state <- new_state()
    def_datafiles <- shinyscreen:::dtable(file=character(0),
                                          tag=character(0))
    def_datatab <- shinyscreen:::dtable("tag"=factor(),
                                        "adduct"=factor(levels=shinyscreen:::DISP_ADDUCTS),
                                        "set"=factor())

    def_summ_subset <- shinyscreen:::dtable("QA Column"=shinyscreen:::QA_FLAGS,
                                            "Select"=factor("ignore",levels=shinyscreen:::SUBSET_VALS))
    ## RMassBank masks shiny::validate. Unmask it.
    validate <- shiny::validate
    ## def_state$input$tab$tags <- def_datatab


    ## The reactive world.
    rvs <- reactiveValues(m=def_state,
                          gui=create_gui(),
                          status=reactiveValues(is_extracted_stat=NA_character_,
                                                is_qa_stat=NA_character_,
                                                ms1_coarse_stat=NA_character_,
                                                ms1_fine_stat=NA_character_,
                                                ms1_eic_stat=NA_character_,
                                                rt_stat=NA_character_,
                                                ms1_int_thresh_stat=NA_character_,
                                                ms2_int_thresh_stat=NA_character_,
                                                s2n_stat=NA_character_,
                                                ret_time_shift_tol_stat=NA_character_))
    
    compl_sets <- eventReactive(rvs$m$input$tab$setid,
                                rvs$m$input$tab$setid[,unique(set)])


    ## Reactive values to support some of the UI elements.

    ## Modifiable version.
    the_summ_subset <- data.table::copy(def_summ_subset)
    
    
    ## Re-definitions.
    PLOT_FEATURES <- shinyscreen:::PLOT_FEATURES

    ## Plotting parameters.

    ## Transient rt range.
    rv_rtrange <- reactiveValues(min=Inf,
                                 max=-Inf)

    ## Transient mz range.
    rv_mzrange <- reactiveValues(min=NA,
                                 max=NA)



    
    ## Other transient values.
    rv_tran <- reactiveValues(qa_compsel_tab=dtable(), # QA clickable table for MS1.
                              qa_ms2sel_tab=dtable())  # QA clickable table for MS2.


    rv_projects <- reactiveVal(projects)
    ## Some more setup.
    ord_nms <- gsub("^-(.+)","\\1",DEF_INDEX_SUMM)
    ord_asc <- grepl("^-.+",DEF_INDEX_SUMM)
    ord_asc <- factor(ifelse(ord_asc, "descending", "ascending"),levels = c("ascending","descending"))
    def_ord_summ <- shinyscreen:::dtable("Column Name"=ord_nms,"Direction"=ord_asc)
    ## Modifiable version.
    the_ord_summ <- data.table::copy(def_ord_summ)

    gen_compsel_tab <- function(summ,criteria=character(0)) {
        ## Given summary table, create a table with only adduct/tag/ID
        ## entries and associated MS1 quantities.
        seln <- logical(length(criteria))
        res <-if (length(seln)>0) {
                  names(seln) <-criteria
                  seln[] <- T
                  critab <- do.call(data.table::data.table,as.list(seln))
                  summ <- summ[critab,on=names(critab)]
                  summ[,unique(.SD),.SDcol=c("adduct","tag","ID",
                                             "mz","ms1_rt","ms1_int",
                                             "Name")]
              } else {
                  summ[,unique(.SD),.SDcol=c("adduct","tag","ID",
                                             "mz","ms1_rt","ms1_int",
                                             "Name")]
              }
        data.table::setkeyv(res,c("adduct", "tag", "mz","ms1_rt"))
        res
        
        
    }

    gen_qa_compsel_tab <- function(clicked,compsel,summ) {
        ## Given the info about what was clicked in compsel table,
        ## retrieve QA information and return it as a table.
        info <- get_key_sel_cmpd(clicked,compsel)
        if (NROW(info$key)==0) return (dtable(qa_ms1_exists=character(0),
                                                  qa_ms1_good_int=character(0),
                                                  qa_ms1_above_noise=character(0)))
        summ[info$key,.(qa_ms1_exists=fixlog2yesno(qa_ms1_exists),
                            qa_ms1_good_int=fixlog2yesno(qa_ms1_good_int),
                            qa_ms1_above_noise=fixlog2yesno(qa_ms1_above_noise)),
             on=c("adduct","tag","ID")][,unique(.SD)]
    }

    gen_ms2_sel <- function(tab,sel_dt) {
        triv <- dtable(an=character(0),
                       ms2_rt=character(0),
                       ms2_int=character(0),
                       CE=character(0),
                       ms2_sel=character(0),
                       qa_ms2_good_int=character(0),
                       qa_ms2_near=character(0),
                       qa_ms2_exists=character(0))
        coln <- colnames(triv)
        
        res <- if (NROW(sel_dt)>0) {
                   
                   tab[sel_dt,..coln,on=c("adduct","tag","ID")]
               } else triv

        
        data.table::setkeyv(res,c("ms2_rt","ms2_int"))
        if (NROW(sel_dt)>0) {
            res[,`:=`(ms2_sel=fixlog2yesno(ms2_sel),
                      qa_ms2_good_int=fixlog2yesno(qa_ms2_good_int),
                      qa_ms2_near=fixlog2yesno(qa_ms2_near),
                      qa_ms2_exists=fixlog2yesno(qa_ms2_exists))]
        }
        res
    }


    gen_ms2_sel_spec <- function(tab,sel_dt) {
        triv <- dtable(mz=numeric(0),
                       intensity=numeric(0))
        coln <- colnames(triv)
        
        res <- if (NROW(sel_dt)>0) {
                   
                   tab[sel_dt,..coln,on=c("adduct","tag","ID","an")]
               } else triv
        data.table::setnames(res,"intensity","ms2_int")
        res
    }


    gen_plot_comp_sel <- function(summ) {
        res <- summ[,unique(.SD),.SDcol=c("adduct","ID",
                                          "mz",
                                          "Name")]
        data.table::setkeyv(res,c("adduct", "mz"))
        res
    }

    uni_ass <- function(input,val,unit) {
        paste(input[[val]],
              input[[unit]])
    }

    adapt_range <- function(fig,x_range=NULL) {
        if (is.null(x_range)) fig else fig+coord_cartesian(xlim=x_range)
    }


    plot_boiler <- function(m,tab,row,plot_fun,rv_x_range,adapt_x_range=T) {
        plot_group <- m$conf$figures$grouping$group
        plot_plot <- m$conf$figures$grouping$plot
        req(row)
        idx <- get_plot_idx(tab = tab,
                            plot_group = plot_group,
                            plot_plot = plot_plot,
                            row =row)
        fig <- plot_fun(m=m,plot_index = idx)
        x_range <- if (adapt_x_range) c(rv_x_range$min,rv_x_range$max) else NULL
        adapt_range(fig,x_range=x_range)
    }


    get_plot_idx <- function(tab,plot_group,plot_plot,row) {
        pg <- tab[row,..plot_group]
        pp <- tab[row,..plot_plot]
        res <- c(pg,pp)
        names(res) <- c(plot_group,plot_plot)
        res
        
    }

    

    ## This is a JavaScript callback which is meant to capture double
    ## clicks on DT datatables and return data in an input field of
    ## the form input$tblId_dbl_click_pos.
    dblclick_callback <- paste0(c(
            "table.on('dblclick.dt', 'td', function(){",
            "var tbl = table.table().node();",
            "var tblId = $(tbl).closest('.datatables').attr('id');",
            "row_ = table.cell(this).index().row;",
            "col_ = table.cell(this).index().column;",
            "var schmuck = new Date().getTime();",
            "var res = {row: row_ + 1,col: col_ + 1, chg: schmuck};",
            "Shiny.setInputValue(tblId + '_dbl_click_pos', res);",
            "})"
        ),collapse = "\n")


    get_pos_from_dblclick <- function(pos,currrows=NULL) {
        if (length(pos) == 0) return(NULL)
        nr <- pos$row
        nc <- pos$col
        rows <- if (!is.null(currrows)) currrows[nr] else nr
        data.frame(row=rows,col=nc)
    }


    
    get_tab_sel <- function(clicked,selector,keys) {
        nr <- clicked$row
        nc <- clicked$col
        colnms <- names(selector)
        the_name <- colnms[[nc]]
        sel_row <- selector[nr,..keys]
        list(col=the_name,
             key=sel_row)
    }
    ## Get info about compound selected in the browser.
    get_key_sel_cmpd <- function(clicked,selector) {
        if (length(clicked) == 0) return(dtable())
        get_tab_sel(clicked,selector,c('adduct','tag','ID'))

    }

    ## Info about MS2 spectrum selected.
    get_ms2_sel <- function(clicked,selector) {
        get_tab_sel(clicked,selector,c('CE','an'))
    }


    pdf(file="dummy.pdf",width = 1.9685,height = 1.9685)
    dev.off()

    server <- function(input,output,session) {
        ## REACTIVE VALUES
        rv_extr_flag <- reactiveVal(F)
        rv_presc_flag <- reactiveVal(F)
        rtimer1000 <- reactiveTimer(1000)
        rtimer_presc <- reactiveTimer(500)
        
        ## REACTIVE FUNCTIONS
        rf_compound_set <- reactive({
            req(rvs$gui$compounds$sets,
                rvs$gui$paths$project)

            get_sets(rvs$gui)
        })



        rf_get_sets <- reactive({
            req(rvs$gui$paths$project,
                rvs$gui$compounds$sets)

            get_sets(rvs$gui)
            
        })

        rf_setup_state <- reactive({
            rvs$gui$project
            rvs$gui$datatab$file
            rvs$gui$datatab$tag
            rvs$gui$datatab$adduct
            rvs$gui$datatab$set
            isolate({
                req(pre_setup_val_block(rvs$gui))
                q = app_state2state(input,rvs$gui)
                
            })
            run(m=q,phases=c("setup","comptab"))
        })

        rf_get_cmpd_tab <- reactive({
            m <- rf_setup_state()
            m$input$tab$cmpds
            })

        rf_get_sets_tab <- reactive({
            m <- rf_setup_state()
            m$input$tab$setid
        })

        rf_cindex_key <- reactive({
            if (isTruthy(input$cindex_group)) setdiff(CINDEX_BY,input$cindex_group) else CINDEX_BY
        })

        rf_get_cindex <- reactive({
            rvs$status$is_qa_stat
            grp <- rf_cindex_key()
            s1 <- input$sort1
            s2 <- input$sort2
            s3 <- input$sort3
            s4 <- input$sort4
            sorder <- setdiff(c(s1,s2,s3,s4),input$cindex_group)
            summ <- req(rvs$m$out$tab$summ)
            isolate({
            if (NROW(summ)>0L) {
                gen_cindex(summ,
                           sorder=sorder,
                           by.=grp)
            } else {
                NULL
            }
            })
        })
        
        ## Get current grouping categories (`cindex key').
        rf_get_cindex_key <- reactive({

            cind <- rf_get_cindex()
            req(NROW(cind)>0L)
            
            ## Select only valid category names.
            x <- which(CINDEX_BY %in% names(cind))
            CINDEX_BY[x]
        })

        ## Get currently selected cindex values as a list.
        rf_get_cindex_kval <- reactive({
            cind <- rf_get_cindex()
            key <- rf_get_cindex_key()
            req(NROW(cind)>0L)
            row <- req(input$cindex_row_last_clicked)
            rowtab <- cind[row][,..key]
            res <- lapply(rowtab,function (x) x[[1]])
            names(res) <- key
            res
        })

        ## Get the labels which will define plot curves in EIC MS1.
        rf_get_cindex_labs <- reactive({
            key <- rf_get_cindex_key()
            res <- setdiff(CINDEX_BY,key)
            if (length(res)!=0L) res else CINDEX_BY
        })

        ## REACTIVE FUNCTIONS: PLOTS
        rf_get_rtrange <- reactive({
            x1 <- input$plot_rt_min
            x2 <- input$plot_rt_max

            if (is.na(x1)) x1 <- NA_real_
            if (is.na(x2)) x2 <- NA_real_
            c(x1,x2)
        })
        rf_plot_eic_ms1 <- reactive({
            isolate({
                ms1 <- rvs$m$extr$ms1
                summ <- rvs$m$out$tab$summ

            })
            req(NROW(summ)>0L)
            req(NROW(ms1)>0L)
            make_eic_ms1_plot(ms1,summ,kvals=req(rf_get_cindex_kval()),
                              labs=req(rf_get_cindex_labs()),
                              asp=PLOT_EIC_ASPECT,
                              rt_range=rf_get_rtrange())
        })

        rf_plot_eic_ms2 <- reactive({
            isolate({
                summ <- rvs$m$out$tab$summ
            })
            req(NROW(summ)>0L)


            gg <- rf_plot_eic_ms1()
            rt_rng <- range(gg$data$rt)
            make_eic_ms2_plot(summ,
                              kvals=rf_get_cindex_kval(),
                              labs=rf_get_cindex_labs(),
                              rt_range = rf_get_rtrange(),
                              asp=PLOT_EIC_ASPECT)
            
            
            
        })

        rf_plot_struct <- reactive({
            cind <- rf_get_cindex()
            key <- rf_get_cindex_key()
            req(NROW(cind)>0L)
            row <- req(input$cindex_row_last_clicked)
            id <- rowtab <- cind[row][,..key][["ID"]][[1]]
            smi <- rvs$m$out$tab$comp[ID==(id),SMILES][[1]]
            print("smiles:")
            print(smi)
            grb <- smiles2img(smi)
            xx <- qplot(1:5, 2*(1:5), geom="blank") +
                annotation_custom(grb, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
                theme_empty
            xx
        })

        rf_plot_spec_ms2 <- reactive({
            isolate({
                summ <- rvs$m$out$tab$summ
                ms2 <- rvs$m$extr$ms2
            })
            req(NROW(summ)>0L)
            req(NROW(ms2)>0L)
            make_spec_ms2_plot(ms2,
                               summ,
                               kvals=req(rf_get_cindex_kval()),
                               labs=req(rf_get_cindex_labs()))
        })

        
        ## OBSERVERS

        observe({
            indir <- rvs$gui$paths$project
            req(isTruthy(indir) && dir.exists(indir))
            updateSelectInput(session = session,
                              inputId = "comp_list",
                              choices = list.files(path=indir,
                                                   pattern = CMPD_LIST_PATT))

            updateSelectInput(session = session,
                              inputId = "set_list",
                              choices = list.files(path=indir,
                                                   pattern = SET_LIST_PATT))

            updateSelectInput(session = session,
                              inputId = "dfile_list",
                              choices = list.files(path=indir,
                                                   pattern = DFILES_LIST_PATT))
            
            updateSelectInput(session = session,
                              inputId = "indir_list",
                              selected = basename(indir),
                              choices = list.dirs(path = init$indir,
                                                  full.names = F,
                                                  recursive = F))
        })

        observe({
            indir <- rvs$gui$paths$data
            req(isTruthy(indir) && dir.exists(indir))
            
            updateSelectInput(session = session,
                              inputId = "dfile_list",
                              choices = list.files(path=indir,
                                                   pattern = DFILES_LIST_PATT))
        })


        
        observeEvent(input$load_proj_b,{
            ## A single place where a new project is initialised, or
            ## loaded. Everything else works off rvs$m and rvs$gui.
            wd <- input$proj_list
            req(!is.null(wd) && !is.na(wd) && nchar(wd)>0)
            fullwd <- file.path(init$userdir,wd)

            ## Load saved state if existing, create if it does not.
            fn_packed_state <- file.path(fullwd,FN_GUI_STATE)
            fn_state <- file.path(fullwd,FN_STATE)
            if (file.exists(fn_packed_state)) {
                message("Loading project: ",wd)
                pack <- readRDS(file=fn_packed_state)
                rvs$gui <- unpack_app_state(session=session,
                                            input=input,
                                            project_path=fullwd,
                                            packed_state=pack)
                ## Load computational state.
                rvs$m <- readRDS(file=fn_state)

                ## Update status variables.
                m <- rvs$m
                rvs$status$ms1_coarse_stat = m$conf$tolerance[["ms1 coarse"]]
                rvs$status$ms1_fine_stat = m$conf$tolerance[["ms1 fine"]]
                rvs$status$ms1_eic_stat = m$conf$tolerance[["eic"]]
                rvs$status$rt_stat = m$conf$tolerance[["rt"]]
                rvs$status$ms1_int_thresh_stat = rvs$m$conf$prescreen[["ms1_int_thresh"]]
                rvs$status$ms2_int_thresh_stat = rvs$m$conf$prescreen[["ms2_int_thresh"]]
                rvs$status$s2n_stat = rvs$m$conf$prescreen[["s2n"]]
                rvs$status$ret_time_shift_tol_stat = rvs$m$conf$prescreen[["ret_time_shift_tol"]]
                if (NROW(m$extr$ms1)>0L) rvs$status$is_extracted_stat <- "Yes."
                if (NROW(m$out$tab$summ)>0L) rvs$status$is_qa_stat <- "Yes."
            } else {
                message("Initialising project: ",wd)
                rvs$gui <- create_gui(project_path=fullwd)
                
            }
            message("project: ",rvs$gui$project())
        }, label = "project-b")

        observeEvent(input$extract_b,{
            rvs$m <-req(rf_setup_state())
            m <- rvs$m
            shinymsg("Extraction has started. This may take a while.")
            rvs$status$ms1_coarse_stat = m$conf$tolerance[["ms1 coarse"]]
            rvs$status$ms1_fine_stat = m$conf$tolerance[["ms1 fine"]]
            rvs$status$ms1_eic_stat = m$conf$tolerance[["eic"]]
            rvs$status$rt_stat = m$conf$tolerance[["rt"]]
            rvs$status$is_extracted_stat = "In progress."
            rv_extr_flag(T)
    
        })

        observe({
            rtimer1000()
            isolate({
                if (rv_extr_flag()) {
                    rv_extr_flag(F)
                    m <-rvs$m
                    
                    promises::future_promise(run(m=m,phases="extract")) %...>% {
                        rvs$m = .
                        rvs$status$is_extracted_stat = "Yes."
                        fn_c_state <- file.path(rvs$m$run$paths$project,
                                                paste0("extract.",shinyscreen:::FN_CONF))
                        yaml::write_yaml(x=rvs$m$conf,file=fn_c_state)
                        message("(extract) Done extracting.")
                        message("(extract) Config written to ", fn_c_state)
                        shinymsg("Extraction has been completed.")

                    }


                }
            })
        })

        ## Update projects and data directories every second.
        observeEvent(rtimer1000(),{
            projects <- rv_projects()
            curr_projects <- list.dirs(path=init$userdir, full.names = F, recursive = F)
            if (length(union(curr_projects,projects)) != length(intersect(curr_projects,projects))) {
                updateSelectInput(session=session,
                                  inputId="proj_list",
                                  choices=curr_projects)
                updateSelectInput(session=session,
                                  inputId="indir_list",
                                  choices=curr_projects)
                rv_projects(curr_projects)
            }

            
        }, label = "update-proj-list")

        observeEvent(input$presc_b,{
            if (NROW(rvs$m$extr$ms1)>0L) {
                rvs$m$conf <- input2conf_prescreen(input=input,conf=rvs$m$conf)
                rvs$status$ms1_int_thresh_stat = rvs$m$conf$prescreen[["ms1_int_thresh"]]
                rvs$status$ms2_int_thresh_stat = rvs$m$conf$prescreen[["ms2_int_thresh"]]
                rvs$status$s2n_stat = rvs$m$conf$prescreen[["s2n"]]
                rvs$status$ret_time_shift_tol_stat = rvs$m$conf$prescreen[["ret_time_shift_tol"]]
                rvs$status$is_qa_stat = "In progress."
                rv_presc_flag(T)
            } else {
                shinymsg("You must extract the data first.",type="warning")
            }
        })
        
        

        observe({
            rtimer_presc()
            isolate({
                if (rv_presc_flag()) {
                    shinymsg("Prescreening started. Please wait.")
                    rv_presc_flag(F)
                    m <-rvs$m
                    promises::future_promise(run(m=m,phases="prescreen")) %...>% {
                        rvs$m = .
                        rvs$status$is_qa_stat = "Yes."
                        shinymsg("Prescreening has been completed.")


                    }
                }
            })
        })

        observeEvent(input$save_proj_b,{
            fn <- file.path(rvs$gui$paths$project,FN_STATE)
            fn_packed_state <- file.path(rvs$gui$paths$project,FN_GUI_STATE)
            fn_tab <- file.path(rvs$gui$paths$project,FN_DATA_TAB)
            fn_conf <-file.path(rvs$gui$paths$project,FN_CONF)
            shinymsg(paste("Saving state to: ",fn,"Please wait.",sep="\n"))
            message("(config) Saving state to: ", paste(fn,collapse = ","))
            message("(config) Saving app state to: ", fn_packed_state)
            fn <- if (length(fn)>0 && nchar(fn[[1]])>0) fn else ""

            if (nchar(fn) > 0) {
                m <- rvs$m
                yaml::write_yaml(m$conf,
                                 file = fn_conf)
                shinyscreen:::tab2file(tab=gui2datatab(rvs$gui),file=fn_tab)
                
                pack <- pack_app_state(input=input,gui=rvs$gui)
                saveRDS(pack,file=fn_packed_state)
                saveRDS(rvs$m,file=fn)
                
            }
            shinymsg("Saving state completed.")
        })

        observeEvent(input$sel_indir_b,{
            indir <- input$indir_list
            req(isTruthy(indir))
            rvs$gui$paths$data <- file.path(init$indir, indir)
            
            message("Selected data dir:",rvs$gui$paths$data)

        })

        

        observeEvent(input$comp_list_b, {
            sels <- input$comp_list
            req(isTruthy(sels))
            rvs$gui$compounds$lists <- sels
            message("(config) Selected compound lists: ", paste(sels,collapse = ","))
        })

        observeEvent(input$set_list_b, {
            sels <- input$set_list
            req(isTruthy(sels))
            message("(config) Selected set lists: ", paste(sels,collapse = ","))
            rvs$gui$compounds$sets <- sels
        })

        observeEvent(input$datafiles_b,{
            new_file <- input$dfile_list
            if (isTruthy(new_file)) {
                curr_file <- rvs$gui$datatab$file
                curr_tag <- rvs$gui$datatab$tag
                curr_adduct <- rvs$gui$datatab$adduct
                curr_set <- rvs$gui$datatab$set

                nb <- length(curr_file)
                nd <- length(new_file)
                res_file <- c(curr_file,new_file)
                res_adduct <- c(curr_adduct,rep(NA_character_,nd))
                res_set <- c(curr_set,rep(NA_character_,nd))

                rvs$gui$datatab$file <- res_file
                rvs$gui$datatab$tag <- add_new_def_tag(as.character(rvs$gui$datatab$tag),nd)
                rvs$gui$datatab$adduct <- res_adduct
                rvs$gui$datatab$set <- res_set
            }

            updateSelectInput(session=session,
                              inputId="dfile_list",
                              selected=NULL)

            
        })

        observeEvent(input$rem_dfiles_b,{
            if (isTruthy(input$datafiles_rows_selected)) {
                rmv <- input$datafiles_rows_selected
                rvs$gui$datatab$file <- rvs$gui$datatab$file[-rmv]
                rvs$gui$datatab$set <- rvs$gui$datatab$set[-rmv]
                rvs$gui$datatab$adduct <- rvs$gui$datatab$adduct[-rmv]
                rvs$gui$datatab$tag <- rvs$gui$datatab$tag[-rmv]
            }
        })
        
        observeEvent(input$datafiles_cell_edit,{
            df <- gen_dfiles_tab(rvs$gui)
            df <- DT::editData(df,
                               input$datafiles_cell_edit,
                               rownames = F)
            rvs$gui$datatab$file <- as.character(df$file)
            rvs$gui$datatab$tag <- as.character(df$tag)
            
        }, label = "datafiles-edit")

        observeEvent(input$datatab_cell_edit,{
            df <- gen_dtab(rvs$gui$datatab,sets=rf_get_sets())
            z <- DT::editData(df,
                              input$datatab_cell_edit,
                              rownames = F)

            rvs$gui$datatab$set <- z$set
            rvs$gui$datatab$adduct <- z$adduct
        }, label = "datatab-edit")

        
        ## RENDER
        output$curr_proj <- renderText({
            xx <- rvs$gui$project()
            txt <- if (is.null(xx) || length(xx) == 0L || is.na(xx) || nchar(xx)=="") "Nothing selected." else basename(xx)
            paste0("Current project: ", txt)})
        
        output$curr_data_dir <- renderText({
            xx <- rvs$gui$paths$data
            txt <- if (is.null(xx)) "Nothing selected" else basename(xx)
            paste0("Current data directory: ", txt)
        })

        output$comp_list_report <- renderUI({
            lsts <- rvs$gui$compounds$lists
            HTML(if (length(lsts) > 0 &&
                     isTruthy(lsts) &&
                     lsts != "Nothing selected.") {
                     paste(c("<ul>",
                             sapply(lsts,
                                    function (x) paste("<li><em>",x,"</em></li>")),
                             "</ul>"))
                 } else "No compound list selected yet.")
        })

        output$sets_report <- renderUI({
            sets <- rvs$gui$compounds$sets
            HTML(if (isTruthy(sets) && sets != "Nothing selected.")
                     paste("selected <em>setid</em> table:",
                           sets) else "No <em>setid</em> table selected.")
        })

        output$datafiles <- DT::renderDT(
        {
            rvs$gui$datatab$file
            rvs$gui$datatab$tag
            res <- gen_dfiles_tab(rvs$gui)
            ## simple_style_dt(res,editable=list(target="cell",disable=list(columns=0)))
            scroll_style_dt(res,editable=list(target="cell",disable=list(columns=0)))
        })

        output$datatab <- DT::renderDT({
            rvs$gui$datatab$tag
            rvs$gui$datatab$set
            rvs$gui$datatab$adduct
            sets <- rf_get_sets()
            dtab <- gen_dtab(rvs$gui$datatab,
                             sets=sets)
            tab <- scroll_dropdown_dt(dtab, callback = dt_drop_callback('1','2',sets))
            ## tab <- dropdown_dt(dtab, callback = dt_drop_callback('1','2',sets))
            tab
            
        })

        output$comp_table <- DT::renderDataTable({
            cmpds <- rf_get_cmpd_tab()
            validate(need(NROW(cmpds)>0,"No compound list loaded yet."))
            DT::datatable(cmpds,
                          ## style = 'bootstrap',
                          ## class = 'table-condensed',
                          extensions = 'Scroller',
                          options = list(scrollX = T,
                                         scrollY = 300,
                                         deferRender = T,
                                         scroller = T))
        })

        output$setid_table <- DT::renderDataTable({
            setid <- rf_get_sets_tab()
            validate(need(NROW(setid)>0,"No set id list loaded yet."))
            DT::datatable(setid,
                          ## style = 'bootstrap',
                          ## class = 'table-condensed',
                          extensions = 'Scroller',
                          options = list(scrollX = T,
                                         scrollY = 300,
                                         deferRender = T,
                                         scroller = T))
        })

        ## RENDER: STATUS

        output$is_extracted_stat <- renderText({
            x <- rvs$status$is_extracted_stat
            if (isTruthy(x)) x else "No."
        })

        output$is_qa_stat <- renderText({
            x <- rvs$status$is_qa_stat
            if (isTruthy(x)) x else "No."
        })

        output$ms1_coarse_stat <- renderText({
            req(rvs$status$ms1_coarse_stat)
        })

        output$ms1_fine_stat <- renderText({
            req(rvs$status$ms1_fine_stat)
        })

        output$ms1_eic_stat <- renderText({
            req(rvs$status$ms1_eic_stat)
        })

        output$rt_stat <- renderText({
            req(rvs$status$rt_stat)
        })

        output$ms1_int_thresh_stat <- renderText({
            req(rvs$status$ms1_int_thresh_stat)
        })

        output$ms2_int_thresh_stat <- renderText({
            req(rvs$status$ms2_int_thresh_stat)
        })

        output$s2n_stat <- renderText({
            req(rvs$status$s2n_stat)
        })

        output$ret_time_shift_tol <- renderText({
            req(rvs$status$ret_time_shift_tol_stat)
        })

        ## RENDER: COMPOUND INDEX

        output$cindex <- DT::renderDT({
            tab <- rf_get_cindex()
            validate(need(NROW(tab)>0L,message="Need to prescreen, first."))
            scroll_style_dt(tab,options=list(filter=T,ordering=F),
                            selection="single")
            ## DT::datatable(tab,
            ##               rownames=NULL,
            ##               options=list(filter=T,ordering=F),
            ##               selection="single")
        })

        ## RENDER: PLOTS

        ## output$plot_eic_combined <- renderPlot({
        ##     p1 <- rf_plot_eic_ms1()
        ##     p2 <- rf_plot_eic_ms2()
        ##     p3 <- rf_plot_spec_ms2()
        ##     combine_plots(p1,p2,p3)
            
            
        ## },height=1000)

        output$plot_eic_ms1 <- renderPlot({
            rf_plot_eic_ms1()
        })

        output$plot_eic_ms2 <- renderPlot({
            rf_plot_eic_ms2()
        })

        output$plot_spec_ms2 <- renderPlot({
            rf_plot_spec_ms2()
        })


        output$plot_hover_out <- renderText({
            inp1 <- input$plot_hover[[1]]
            inp2 <- input$plot_hover[[2]]
            res <- if (all(!(c(is.null(inp1),is.null(inp2))))) {
                       paste0('(',
                              format(inp1,digits=5),
                              ',',
                              format(inp2,digits=2,scientific=T),
                              ')')
                   } else "Currently not in the plot."
            
        })

        output$plot_struct <- renderPlot({
            rf_plot_struct()
        })
        ## output$plot_eic_ms1 <- renderPlot({
        ##     rf_plot_eic_ms1()
        ## })

        ## output$plot_eic_ms2 <- renderPlot({
        ##     rf_plot_eic_ms2()
        ## })

        ## output$plot_spec_ms2 <- renderPlot({
        ##     NULL
        ## })

        

        
            
            
    }

    server_old  <- function(input,output,session) {
        
        ## REACTIVE FUNCTIONS
        


        rf_sort_state <- reactive({
            m <- rf_prescreen_state()
            run(m=m,phases=c("sort","subset"))
        })


        rf_figures_state <- reactive({
            m <- rf_sort_state()

            input$plot_rt_min
            input$plot_rt_min_unit

            input$plot_rt_max
            input$plot_rt_max_unit

            input$plot_ext

            m$conf <- input2conf_figures(input,m$conf)
            run(m=m,phases="plot")
        })

        rf_report_state <- reactive({

            m <- rf_sort_state()
            
            input$plot_rt_min
            input$plot_rt_min_unit

            input$plot_rt_max
            input$plot_rt_max_unit

            input$plot_ext

            input$rep_aut
            input$rep_tit


            m$conf <- input2conf_figures(input,m$conf)
            m$conf <- input2conf_report(input,m$conf)
            run(m=m,phases="report")
        })

            
            

        

        rf_conf_proj <- reactive({
            
            state <- rev2list(rvs$m)
            if (!is.null(state$run$paths$project)) dir.create(state$run$paths$project,showWarnings = F)
            state
            
        })

        rf_conf_state <- reactive({
            state <- rf_conf_proj()
            ftab <- get_fn_ftab(state)
            state$run$paths$datatab <- ftab
            state$conf[["summary table"]]$filter <- rf_get_subset()
            state$conf[["summary table"]]$order <- rf_get_order()
            state
        })

        rf_get_subset <- reactive({
            input$summ_subset_cell_edit
            dt <- data.table::copy(the_summ_subset)
            dt[Select == SUBSET_VALS[["GOOD"]], extra := T]
            dt[Select == SUBSET_VALS[["BAD"]], extra := F]
            sdt <- dt[!is.na(extra)]
            if (NROW(sdt) > 0) {
                sdt[,paste0(`QA Column`," == ",extra)]
            } else NULL
        })

        rf_get_order <- reactive({
            ## FIXME: order_summ reordering/editing unstable. Therefore temporarily removed.
            ## input$order_summ_cell_edit
            dt <- data.table::copy(the_ord_summ)
            tmp <- dt[Direction == "descending",.(`Column Name`=paste0("-",`Column Name`))]
            tmp[,`Column Name`]
        })

        rf_dtab <- reactive({
            rvs$gui$datatab$tag
            rvs$gui$datatab$adduct
            rvs$gui$datatab$set
            sets <- rf_get_sets()
            validate(need(isTruthy(sets), message = "Please select a set list first."))
            gen_dtab(rvs$gui$datatab,sets=sets)
            })

        rf_summ_table_rows <- eventReactive(input$summ_table_rows_all,{
            input$summ_table_rows_all
            
        })

        rf_gen_sel_plot_tab <- reactive({


            rvs$m$out$tab$flt_summ
            m <- rev2list(rvs$m)
            fltsumm <- m$out$tab$flt_summ
            validate(need(NROW(fltsumm) > 0,
                          message = "Generate summary table first."))
            
            rows <- rf_summ_table_rows()
            
            ## Reduce to currently selected rows.
            m$out$tab$flt_summ <- m$out$tab$flt_summ[rows,]
            gen_key_plot_tab(m)
            
        })

        rf_rtrange_from_data <- eventReactive(input$plot_sel_cell_clicked,{
            ## Determine the plotting RT range.
            
            ptab <- rf_gen_sel_plot_tab()
            plot_group <- rvs$m$conf$figures$grouping$group
            plot_plot <- rvs$m$conf$figures$grouping$plot
            row <- input$plot_sel_cell_clicked[["row"]]
            req(row)
            idx <- get_plot_idx(tab = ptab,
                                plot_group = plot_group,
                                plot_plot = plot_plot,
                                row =row)

            m <- rev2list(rvs$m)
            pdata_ms1 <- get_ms1_chr_pdata(m,plot_index=idx)
            pdata_ms2 <- get_ms2_chr_pdata(m,plot_index=idx)
            ## rt_ms1_rng <- as.numeric(pdata_ms1[,range(rt)])
            ## rt_ms2_rng <- as.numeric(pdata_ms2[,range(rt_peak)])
            
            get_rt_interval(pdata_ms1,pdata_ms2,rvs$m$conf$figures)
        })


        rf_gen_struct_figs <- eventReactive(rvs$m$out$tab$comp,gen_struct_plots(rvs$m))

        rf_compsel_tab <- reactive({
            input$cmpd_sel_filter
            message("cmpd_sel_filter:",paste0(input$cmpd_sel_filter,collapse=";"))
            tab <- rvs$m$out$tab$flt_summ
            req(NROW(tab)>0)
            gen_compsel_tab(tab,criteria=input$cmpd_sel_filter)
        })

        rf_qa_compsel_tab <- reactive({
            ## Generate the QA line for compound selector.
            compsel <- rf_compsel_tab()
            tab <- rvs$m$out$tab$flt_summ
            clicked <- input$compound_selector_cell_clicked
            gen_qa_compsel_tab(clicked,compsel,tab)
        })

        rf_ms2_sel <- reactive({
            tab <- rvs$m$out$tab$flt_summ
            req(NROW(tab)>0)
            rows <- input$compound_selector_rows_selected
            compsel <- rf_compsel_tab()
            
            sel_dt <- if(length(rows>0)) compsel[rows,unique(.SD),.SDcol=c("adduct","tag","ID")] else data.table("adduct"=character(0),
                                                                                                                 "tag"=character(0),
                                                                                                                 "ID"=character(0))
            gen_ms2_sel(tab,sel_dt=sel_dt)
        })

        rf_ms2_sel_spec <- reactive({
            ## tab <- rvs$m$out$tab$flt_summ
            ## req(NROW(tab)>0)
            req(NROW(rv_tran$qa_ms2sel_tab)>0)
            ms1rows <- input$compound_selector_rows_selected
            ms2rows <- input$compound_ms2_table_rows_selected
            req(length(ms1rows)>0,
                length(ms2rows)>0)
            compsel <- rf_compsel_tab()
            ms2tab <- rf_ms2_sel()
            
            sel_dt <- if (length(ms1rows>0)) compsel[ms1rows,unique(.SD),.SDcol=c("adduct","tag","ID")] else data.table("adduct"=character(0),
                                                                                                                        "tag"=character(0),
                                                                                                                        "ID"=character(0))

            
            ms2_an <- if (length(ms2rows>0)) as.integer(ms2tab[ms2rows,an]) else NA

            
            if (!is.na(ms2_an)) sel_dt$an <- ms2_an else sel_dt <- data.table()
            
            
            gen_ms2_sel_spec(rvs$m$extr$ms2,sel_dt=sel_dt)})

        rf_plot_comp_select <- eventReactive(rvs$m$out$tab$flt_summ,{
            tab <- rvs$m$out$tab$flt_summ
            req(NROW(tab)>0)
            gen_plot_comp_sel(tab)
        })


        rf_dt_sel <- reactive({
            cmp_sel <- rf_plot_comp_select()
            rows <- input$plot_comp_select_rows_selected
            if (NROW(cmp_sel) == 0 ||
                length(rows) == 0) return(NULL)
            cmp_sel[rows,.(adduct,ID)]
            
        })

        rf_d4p_ms1_cgram <- reactive({
            ms1 <- rvs$m$extr$ms1
            dt_sel <- rf_dt_sel()
            req(NROW(dt_sel)>0)
            data4plot_ms1_cgram(rvs$m$extr$ms1,dt_sel)
        })

        rf_d4p_ms2_cgram <- reactive({
            ms2 <- rvs$m$extr$ms2
            dt_sel <- rf_dt_sel()
            req(NROW(dt_sel)>0)
            data4plot_ms2_cgram(rvs$m$extr$ms2,dt_sel)
        })

        rf_d4p_ms2_spec <- reactive({
            summ <- rvs$m$out$tab$flt_summ
            ms2 <- rvs$m$extr$ms2
            dt_sel <- rf_dt_sel()
            req(NROW(dt_sel)>0)
            data4plot_ms2_spec(rvs$m$extr$ms2,summ,dt_sel)
        })

        rf_rtrange_from_data <- reactive({
            dt1 <- rf_d4p_ms1_cgram()
            dt2 <- rf_d4p_ms2_cgram()
            rdt1 <- c(NA_real_,NA_real_)
            rdt2 <- c(NA_real_,NA_real_)
            if (NROW(dt1)>0) rdt1 <- dt1[,range(rt)]
            if (NROW(dt2)>0) rdt2 <- dt2[,range(rt)]

            rdt <- c(NA_real_,NA_real_)
            rdt[[1]]<-min(rdt1[[1]],rdt2[[1]],na.rm = T)
            if (is.infinite(rdt[[1]])) rdt[[1]] <- NA_real_
            rdt[[2]]<-max(rdt1[[2]],rdt2[[2]],na.rm = T)
            if (is.infinite(rdt[[2]])) rdt[[2]] <- NA_real_

            rdt
            
            
        })


        rf_plot_palette <- reactive({
            pdata <- rf_d4p_ms1_cgram()
            req(NROW(pdata)>0)
            plot_palette(pdata)
        })
        
        rf_plot_eic_facet <- reactive({
            ms2 <- rvs$m$extr$ms2
            
            pdata_ms1 <- rf_d4p_ms1_cgram()
            pdata_ms2 <- rf_d4p_ms2_cgram()
            
            plot_eic_w_facet(pdata_ms1 = pdata_ms1,
                             pdata_ms2 = pdata_ms2,
                             rt_range = c(rv_rtrange$min,
                                          rv_rtrange$max),
                             palette = rf_plot_palette())
            
            
        })

        rf_plot_spec_facet <- reactive({
        
            pdata <- rf_d4p_ms2_spec()
            plot_spec_w_facet(pdata_ms2 = pdata,
                              mz_range = c(rv_mzrange$min,
                                           rv_mzrange$max),
                              palette = rf_plot_palette())
            
            
        })


        rf_tab4plot_eic <- reactive({
            pdata <- rf_d4p_ms1_cgram()
            table_eic(pdata)
        })

        rf_tab4plot_spec <- reactive({
            pdata <- rf_d4p_ms2_cgram()
            table_spec(pdata)
        })


        rf_get_cmpd_sel <- reactive({
            clicked <- input$compound_selector_cell_clicked
            isolate({
                selector <- rf_compsel_tab()
                
            })
            req(NROW(clicked)>0,
                NROW(selector)>0)
            info <- get_key_sel_cmpd(clicked,selector)
            info
        })

        rf_get_cmpd_dblclick <- reactive({
            input$compound_selector_dbl_click_pos
            isolate({
                selector <- rf_compsel_tab()
                curr_rows <- input$compound_selector_rows_current
            })
            clicked <- get_pos_from_dblclick(curr_rows,
                                             input$compound_selector_dbl_click_pos)
                        
            req(NROW(clicked)>0,
                NROW(selector)>0)
            info <- get_key_sel_cmpd(clicked,selector)
            info
        })



        ## Observers

        

        

        

        

        

        


        observeEvent(input$summ_subset_cell_edit,{
            the_summ_subset <<- DT::editData(the_summ_subset,
                                             input$summ_subset_cell_edit,
                                             rownames = F)
            
        }, label = "summ_subset-edit")
        

        

        

        observeEvent(input$plot_ext, {
            rvs$m$conf$figures$ext <- input$plot_ext
        })

        observeEvent(input$plot_save_single,{
            ext <- rvs$m$conf$figures$ext
            eic <- "eic"
            spec <- "spec"
            dt_sel <- rf_dt_sel()
            if (!is.null(dt_sel)) {
                plot_save_single(rf_plot_eic_facet(),
                                 decotab = dt_sel,
                                 figtag = eic,
                                 proj = rvs$m$conf$project,
                                 tabl = rf_tab4plot_eic(),
                                 extension = ext)
                plot_save_single(rf_plot_spec_facet(),
                                 decotab = dt_sel,
                                 proj = rvs$m$conf$project,
                                 tabl = rf_tab4plot_spec(),
                                 figtag = spec,
                                 extension = ext)
                message("Plots saved to ",file.path(rvs$gui$paths$project,
                                                    FIG_TOPDIR))

            } else message("Nothing to save.")
        })

        observeEvent(input$plot_save_all, {
            req(NROW(rvs$m$out$tab$flt_summ)>0)
            create_plots(rvs$m)
        })


        observeEvent(input$make_report_b, {
            shinymsg("Started creating report. Please wait.")
            req(NROW(rvs$m$out$tab$flt_summ)>0)
            mm <- rev2list(rvs$m)
            subs <- rf_get_subset()
            message("we're here")
            if (shiny::isTruthy(subs)) {
                message("now here")
                tmp <- lapply(subs, function (x) parse(text = x)[[1]])
                expr <- Reduce(function (x,y) {z<-call("&");z[[2]]<-x;z[[3]]<-y;z},x=tmp)
                message("Filtering with: ",deparse(bquote(mm$out$tab$flt_summ[.(expr)])))
                mm$out$tab$reptab <- eval(bquote(mm$out$tab$flt_summ[.(expr)]))
                
            } else mm$out$tab$reptab <- mm$out$tab$flt_summ
            report(mm)
            shinymsg("Finished creating report.")
            
        }, label = "make-report")

        observeEvent(input$updatesumm_b,{
            compsel <- rf_compsel_tab()
            the_row <- input$compound_selector_rows_selected
            req(NROW(compsel)>0,
                NROW(rvs$m$out$tab$summ)>0,
                NROW(rvs$m$out$tab$flt_summ)>0)
            summ <- rvs$m$out$tab$summ
            flt_summ <- rvs$m$out$tab$flt_summ
            if (NROW(rv_tran$qa_compsel_tab)>0) {
                
                
                
                qa_names <- c("qa_ms1_exists",
                              "qa_ms1_good_int",
                              "qa_ms1_above_noise")
                
                qatab <- copy(rv_tran$qa_compsel_tab)
                qatab[,(qa_names):=lapply(.SD,yesno2log),.SDcol=qa_names]
                entries <- cbind(compsel[the_row][,.(adduct,tag,ID)],
                                 qatab)
                summ[entries,c("qa_ms1_exists",
                              "qa_ms1_good_int",
                              "qa_ms1_above_noise"):=.(i.qa_ms1_exists,
                                                       i.qa_ms1_good_int,
                                                       i.qa_ms1_above_noise),
                     on=c('adduct','tag','ID')]
                flt_summ[entries,c("qa_ms1_exists",
                                   "qa_ms1_good_int",
                                   "qa_ms1_above_noise"):=.(i.qa_ms1_exists,
                                                            i.qa_ms1_good_int,
                                                            i.qa_ms1_above_noise),
                         on=c('adduct','tag','ID')]

                
            }

            if (NROW(rv_tran$qa_ms2sel_tab)>0) {
                qa_names <- c("ms2_sel",colnames(flt_summ)[grepl("qa_ms2",colnames(flt_summ))])
                qatab <- copy(rv_tran$qa_ms2sel_tab)
                qatab[,(qa_names):=lapply(.SD,yesno2log),.SDcol=qa_names]
                entries <- cbind(compsel[the_row][,.(adduct,tag,ID)],
                                 qatab)
                qt_qa_names <- sapply(qa_names,function(x) paste0("'",x,"'"),USE.NAMES=F)
                i_qa_names <- sapply(qa_names,function(x) paste0("i.",x),USE.NAMES=F)
                strexpr <- paste0("c(",paste(qt_qa_names,collapse=","),
                                  ")",":=",".(",paste(i_qa_names,collapse=","),")")
                expr <- parse(text=strexpr)
                eval(bquote(summ[entries,.(expr[[1]]),on=c("adduct",
                                                           "tag",
                                                           "ID",
                                                           "an")]))
                eval(bquote(flt_summ[entries,.(expr[[1]]),on=c("adduct",
                                                               "tag",
                                                               "ID",
                                                               "an")]))
               
            }

            rvs$m$out$tab$summ <- summ
            rvs$m$out$tab$flt_summ <- flt_summ
            shinymsg("Changes to QA information have been commited.")
            
            
        })

        observeEvent(input$exportsumm_b,{
            ## Exports sorted/subsetted summary.
            req(NROW(rvs$m$out$tab$flt_summ)>0)
            shinymsg("Starting to export the summary file.")
            tab <- add_msms_peaks(rvs$m$out$tab$flt_summ,
                                  rvs$m$extr$ms2)
            tab2file(tab=tab,
                     file=file.path(rvs$gui$paths$project,
                                    "summary.csv"))
            shinymsg("Summary file export has been completed.")
        },label = "exportsumm_b")


        ## TODO: Uncomment this once done refacing.
        ## observeEvent(rvs$m$out$tab$comp,{
        ##     m <- gen_struct_plots(rev2list(rvs$m))
        ##     rvs$m$out$tab$structfig <- m$out$tab$structfig
        ## }, label = "gen_struct_plots")

        

        

        
        observe({
            vals <- input$plot_log
            checked <- c("MS1 EIC"=F,
                         "MS2 EIC"=F,
                         "MS2 Spectrum"=F)
            if (length(vals)!=0) checked[vals] <- T
            l <- list()
            l <- c(if (checked[["MS1 EIC"]]) "ms1_eic_int" else NULL,l)
            l <- c(if (checked[["MS2 EIC"]]) "ms2_eic_int" else NULL,l)
            l <- c(if (checked[["MS2 Spectrum"]]) "ms2_spec_int" else NULL,l)
            rvs$m$conf$figures[["logaxes"]] <- l[!sapply(l,is.null)]
            

        }, label = "plot-logaxes")

        observe({
            rv_tran$qa_compsel_tab <- rf_qa_compsel_tab()
        })

        observeEvent(input$compound_selector_qa_dbl_click_pos,{
            dbl_clicked <- input$compound_selector_qa_dbl_click_pos
            isolate({
                dbl_info <- get_pos_from_dblclick(dbl_clicked)
            })
            res <- rv_tran$qa_compsel_tab
            if (NROW(dbl_info)>0) {
                ncol <- dbl_info$col
                nrow <- dbl_info$row
                lval <- !yesno2log(as.character(res[[ncol]]))
                res[1,ncol] <- fixlog2yesno(lval)
            }
            rv_tran$qa_compsel_tab <- res
            
        }, label = "qa_compsel_tab: dblclick")

        observe({
            rv_tran$qa_ms2sel_tab <- rf_ms2_sel()
        }, label = "qa_ms2sel_tab: init")

        observeEvent(input$compound_ms2_table_dbl_click_pos,{
            dbl_clicked <- input$compound_ms2_table_dbl_click_pos
            isolate({
                dbl_info <- get_pos_from_dblclick(dbl_clicked,
                                                  currrows = input$compound_ms2_table_rows_current)
            })
            res <- rv_tran$qa_ms2sel_tab
            req(NROW(res)>0,NROW(dbl_info)>0)

            ncol <- dbl_info$col
            nrow <- dbl_info$row
            colnm <- colnames(res)[[ncol]]
            if (colnm == "ms2_sel" || grepl("^qa_",colnm)) {
                val <- res[nrow,..ncol]
                message("val:",val)
                if (!is.na(val)) {
                    if (grepl("^qa_",colnm)) {
                        lval <- !yesno2log(as.character(val))
                        message("qa lval:",lval)
                        res[nrow,ncol] <- fixlog2yesno(lval)
                        rv_tran$qa_ms2sel_tab <- res

                    } else  {
                        lval <- !yesno2log(as.character(val))
                        message("ms2sel lval:",lval)
                        if (lval) {
                            col <- res[[colnm]]
                            wind <- which(yesno2log(col))
                            if (length(wind) > 0) res[wind,colnm] <- fixlog2yesno(F)
                        }
                        res[nrow,ncol] <- fixlog2yesno(lval)
                        rv_tran$qa_ms2sel_tab <- res
                    }
                    
                }
            }
            
        }, label = "qa_ms2sel_tab: dblclick")

        observeEvent(input$plot_rt_click,
        {
            ## TODO: update to sensible range.
            res <- rf_rtrange_from_data()
            rv_rtrange$min <- if (length(rv_rtrange$min) == 0 ||
                                  is.na(rv_rtrange$min)) res[[1]] else NA_real_
            rv_rtrange$max <- if (length(rv_rtrange$max) == 0 ||
                                  is.na(rv_rtrange$max)) res[[2]] else NA_real_
        }, label = "reset_rt_range")

        observeEvent(input$plot_mz_click,
        {
            rv_mzrange$min <- NA
            rv_mzrange$max <- NA
        }, label = "reset_mz_range")


        observeEvent(input$plot_brush,{
            xmin <- input$plot_brush[["xmin"]]
            xmax <- input$plot_brush[["xmax"]]
            if (!is.null(xmin)) rv_rtrange$min <- xmin
            if (!is.null(xmin)) rv_rtrange$max <- xmax
            session$resetBrush("plot_brush")
            
        },label = "get_rt_from_selection")

        observeEvent(input$plot_mz_brush,{
            xmin <- input$plot_mz_brush[["xmin"]]
            xmax <- input$plot_mz_brush[["xmax"]]
            if (!is.null(xmin)) rv_mzrange$min <- xmin
            if (!is.null(xmin)) rv_mzrange$max <- xmax
            session$resetBrush("plot_mz_brush")
            
        })

        ## Render Outputs
        

        ## output$order_summ <- DT::renderDT({
        ##     input$order_summ_row_reorder
        ##     input$order_summ_cell_edit
        ##     dropdown_dt(the_ord_summ,
        ##                 callback = dt_order_summ_callback(),
        ##                 extensions='RowReorder',
        ##                 options = list(rowReorder=T))
        ## })
        
        

        
        

        output$summ_subset <- DT::renderDT({
            input$summ_subset_cell_edit
            dropdown_dt(the_summ_subset, callback = dt_summ_subset_callback())
        })

        output$summ_table <- DT::renderDataTable({

            
            tab <- rvs$m$out$tab$flt_summ
            nms <- colnames(tab)
            dpl_nms <- nms[nms!="file"]
            validate(need(NROW(tab)>0, message = "Please prescreen the data first."))
            DT::datatable(tab[,..dpl_nms],
                          style = 'bootstrap',
                          class = 'table-condensed',
                          extensions = 'Scroller',
                          options = list(scrollX = T,
                                         scrollY = 200,
                                         deferRender = T,
                                         scroller = T))
        })


        output$compound_selector <- DT::renderDT({
            
            styled_dt(rf_compsel_tab(),
                      selection = "single")
        })

        output$compound_selector_qa <- DT::renderDT({
            styled_dt(rv_tran$qa_compsel_tab,
                      extensions = NULL,
                      selection = "none",
                      filter = 'none',
                      scroller = F,
                      callback = DT::JS(dblclick_callback))

        })

        output$compound_ms2_table <- DT::renderDT({
            styled_dt(rv_tran$qa_ms2sel_tab,
                          selection = "single",
                          callback = DT::JS(dblclick_callback))
        })

        output$update_summ_ui <- renderUI({
            ## req(NROW(rf_compsel_tab())>0) # this somehow ruins
            ## buffered scrolling in compsel tab.
            actionButton(inputId = "updatesumm_b",
                         label="Commit manual QA")
        })

        output$export_summ_ui <- renderUI({
            ## req(NROW(rf_compsel_tab())>0) # this somehow ruins
            ## buffered scrolling in compsel tab.
            actionButton(inputId = "exportsumm_b",
                         label="Export MS1/MS2 summary")
        })

        output$compound_ms2_spectrum <- DT::renderDT({
            styled_dt(rf_ms2_sel_spec(),
                      filter = 'none',
                      selection = "none")
        })

        output$plot_comp_select <- DT::renderDT({
            styled_dt(rf_plot_comp_select())
        })

        ## Plots

        output$plot_eic <- renderPlot({
            rf_plot_eic_facet()
        })


        output$plot_ms2_spec <- renderPlot({
            rf_plot_spec_facet()
        })

        

        output$dwn_proj_b <- shiny::downloadHandler(
                                        filename=function() {
                                            format(Sys.time(), "project_%Y%m%d_%H_%M_%S.tar.gz")
                                        },
                                        content=function(file) {
                                            pdir <- rvs$gui$paths$project
                                            shiny::req(!is.null(pdir) &&
                                                       !is.na(pdir) &&
                                                       (nchar(pdir) > 0))
                                            ddir <- tempfile("projectdata",tmpdir=".")
                                            if (dir.exists(ddir)) unlink(ddir,recursive=T)
                                            dir.create(ddir)
                                            srcfig <- file.path(pdir,'figures')
                                            file.copy(from=srcfig,to=ddir,recursive=T,copy.date=T)
                                            csvfns <- list.files(path=pdir,pattern=r"(.*\.csv$)",full.names=T)
                                            ymlfns <- list.files(path=pdir,pattern=r"(.*\.y.ml$)",full.names=T)
                                            pdffns <- list.files(path=pdir,pattern=r"(.*\.pdf$)",full.names=T)
                                            fns <- c(csvfns,ymlfns,pdffns)
                                            for (fn in fns) {file.copy(from=fn,to=ddir,copy.date=T)} 
                                            tar(file,files=ddir,compression="gzip")
                                            unlink(ddir,recursive=T)
                                        })


        


        session$onSessionEnded(function () stopApp())
    }

    server
}
