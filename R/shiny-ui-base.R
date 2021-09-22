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

react_v <- shiny::reactiveValues
react_f <- shiny::reactive
react_e <- shiny::eventReactive
obsrv <- shiny::observe
obsrv_e <- shiny::observeEvent
isol <- shiny::isolate


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
styled_dt <- function(tab,style = 'bootstrap',
                      class = 'cell-border',
                      extensions = 'Scroller',
                      options = list(scrollY=200,
                                     scrollX=T,
                                     dom = "t",
                                     deferRender = T,
                                     scroller = T,
                                     ordering = F),
                      colnames = style_tab_cols(tab),
                      rownames = F,
                      filter = 'top',
                      ...) {
    dttab <- DT::datatable(tab,
                           style = style,
                           class = class,
                           options = options,
                           colnames = colnames,
                           rownames = rownames,
                           filter = filter,
                           ...)
    dttab
    style_tab_signif(dttab)
    
}

render_dt <- function(data, server = T) {
    DT::renderDT(data, server = server)
}

simple_style_dt <- function(tab,
                            style = 'bootstrap',
                            class = 'table-bordered table-condensed',
                            rownames = F, ...) {
    DT::datatable(tab,
                  style = style,
                  class = class,
                  rownames = rownames,
                  ...)
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
    rvs <- shiny::reactiveValues(m=list2rev(def_state))

    compl_sets <- eventReactive(rvs$m$input$tab$setid,
                                rvs$m$input$tab$setid[,unique(set)])


    ## Reactive values to support some of the UI elements.
    ## rv_ui <- reactiveValues(datatab=def_tags)

    ## Update with data-files.
    rv_dfile <- reactiveVal(def_datafiles)

    ## Data-file table when loading.
    rv_datatab <- reactiveVal(def_datatab)

    ## Re-definitions.
    PLOT_FEATURES <- shinyscreen:::PLOT_FEATURES

    ## Plotting parameters.

    ## Transient rt range.
    rv_rtrange <- reactiveValues(min=rt_in_min(def_state$conf$figures$rt_min),
                                 max=rt_in_min(def_state$conf$figures$rt_max))

    ## Transient mz range.
    rv_mzrange <- reactiveValues(min=NA,
                                 max=NA)



    
    ## Other transient values.
    rv_tran <- reactiveValues(qa_compsel_tab=dtable(), # QA clickable table for MS1.
                              qa_ms2sel_tab=dtable())  # QA clickable table for MS2.


    rv_projects <- reactiveVal(projects)
    ## Some more setup.
    ord_nms <- gsub("^-(.+)","\\1",shinyscreen:::DEF_INDEX_SUMM)
    ord_asc <- grepl("^-.+",shinyscreen:::DEF_INDEX_SUMM)
    ord_asc <- factor(ifelse(ord_asc, "descending", "ascending"),levels = c("ascending","descending"))
    def_ord_summ <- shinyscreen:::dtable("Column Name"=ord_nms,"Direction"=ord_asc)


    gen_compsel_tab <- function(summ) {
        ## Given summary table, create a table with only adduct/tag/ID
        ## entries and associated MS1 quantities.
        res <- summ[,unique(.SD),.SDcol=c("adduct","tag","ID",
                                          "mz","ms1_rt","ms1_int",
                                          "Name")]
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

    update_gui <- function(in_conf, session) {
        upd_unit <- function(entry,inp_val,inp_unit,choices) {
            if (isTruthy(entry)) {
                cntnt <- strsplit(entry,split = "[[:space:]]+")[[1]]
                cntnt <- cntnt[nchar(cntnt) > 0]
                if (length(cntnt)!=2) stop("(upd_unit) ","Unable to interpret ", entry)
                val <- cntnt[[1]]
                unit <- cntnt[[2]]
                updateNumericInput(session = session,
                                   inputId = inp_val,
                                   value = as.numeric(val))
                updateSelectInput(session = session,
                                  inputId = inp_unit,
                                  selected = unit,
                                  choices = choices)
            }
        }

        upd_num <- function(entry,inp_val) {
            if (isTruthy(entry)) {
                updateNumericInput(session = session,
                                   inputId = inp_val,
                                   value = as.numeric(entry))
            }
        }

        upd_sel <- function(inputId,selected,choices) {
            if (isTruthy(selected)) {
                updateSelectInput(session = session,
                                  inputId = inputId,
                                  selected = selected,
                                  choices = choices)
            }
        }

        isolate({
            rvs$m$conf$project <- in_conf$project
            rvs$m$conf$data <- in_conf$data
            ## Lists
            rvs$m$conf$compounds$lists <- in_conf$compounds$lists
            rvs$m$conf$compounds$sets <- in_conf$compounds$sets
            
            ## Tolerance
            
            upd_unit(in_conf$tolerance[["ms1 fine"]],
                     "ms1_fine",
                     "ms1_fine_unit",
                     choices=c("ppm","Da"))
            upd_unit(in_conf$tolerance[["ms1 coarse"]],
                     "ms1_coarse",
                     "ms1_coarse_unit",
                     choices=c("ppm","Da"))

            upd_unit(in_conf$tolerance[["eic"]],
                     "ms1_eic",
                     "ms1_eic_unit",
                     choices=c("ppm","Da"))
            upd_unit(in_conf$tolerance[["rt"]],
                     "ms1_rt_win",
                     "ms1_rt_win_unit",
                     choices=c("min","s"))

            ## Prescreen
            upd_num(in_conf$prescreen[["ms1_int_thresh"]],
                    "ms1_int_thresh")
            upd_num(in_conf$prescreen[["ms2_int_thresh"]],
                    "ms2_int_thresh")
            upd_num(in_conf$prescreen[["s2n"]],
                    "s2n")
            upd_unit(in_conf$prescreen[["ret_time_shift_tol"]],
                     "ret_time_shift_tol",
                     "ret_time_shift_tol_unit",
                     choices=c("min","s"))

            ## Files
            if (isTruthy(in_conf$data)) {
                df <- shinyscreen:::file2tab(in_conf$data)
                dfile <- data.table::copy(df[,tag:=as.character(tag),with=T])
                dfile <- dfile[,unique(.SD),.SDcol=c("file","tag")]
                ## rv_dfile(df[,.(file,tag),by=c("file","tag"),mult="first"][,file:=NULL])
                rv_dfile(dfile)
                nms <- colnames(df)
                nms <- nms[nms!="file"]
                fdt <- df[,..nms]
                rv_datatab(fdt)
            }

            ## figures
            upd_unit(in_conf$figures$rt_min,
                     "plot_rt_min",
                     "plot_rt_min_unit",
                     choices=c("min","s"))
            
            upd_unit(in_conf$figures$rt_max,
                     "plot_rt_max",
                     "plot_rt_max_unit",
                     choices=c("min","s"))

            if (isTruthy(in_conf$figures$logaxes)) {
                logentry <- in_conf$figures$logaxes
                logchoice <- logical(0)
                logchoice <- mapply(function(cn,uin) if (cn %in% logentry) uin else NA,
                                    c("ms1_eic_int","ms2_eic_int","ms2_spec_int"),
                                    c("MS1 EIC","MS2 EIC","MS2 Spectrum"),USE.NAMES = F)
                logchoice <- logchoice[!is.na(logchoice)]
                
                updateCheckboxGroupInput(session = session,
                                         inputId = "plot_log",
                                         choices = c("MS1 EIC",
                                                     "MS2 EIC",
                                                     "MS2 Spectrum"),
                                         selected = logchoice)
            }
            ## Report
            if (isTruthy(in_conf$report$author)) updateTextInput(session,"rep_aut",value = in_conf$report$author)
            if (isTruthy(in_conf$report$title)) updateTextInput(session,"rep_tit",value = in_conf$report$title)

            
        })
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

    server  <- function(input,output,session) {
        ## REACTIVE FUNCTIONS

        rf_compound_input_state <- reactive({
            sets <- rvs$m$conf$compounds$sets
            lst <- as.list(rvs$m$conf$compounds$lists)
            validate(need(length(lst)>0,
                          message = "Load the compound lists(s) first."))
            validate(need(length(sets)>0 && nchar(sets)>0,
                          message = "Load the setid table first."))
            isolate({
                state <- rev2list(rvs$m)
                m <- load_compound_input(state)
                ## Side effect! This is because my pipeline logic does not
                ## work nicely with reactive stuff.
                rvs$m$input$tab$cmpds <- list2rev(m$input$tab$cmpds)
                rvs$m$input$tab$setid <- m$input$tab$setid
                m
            })
        })

        rf_conf_proj <- reactive({
            
            state <- rev2list(rvs$m)
            dir.create(state$conf$project,showWarnings = F)
            state
            
        })

        rf_conf_state <- reactive({
            state <- rf_conf_proj()
            ## mzml1 <- rf_get_inp_datatab()
            ## mzml1[,`:=`(tag=as.character(tag),
            ##             set=as.character(set),
            ##             adduct=as.character(adduct))]
            ## mzml2 <- rf_get_inp_datafiles()
            
            ## mzml <- mzml1[mzml2,on="tag"]
            
            ftab <- get_fn_ftab(state)
            state$conf$data <- ftab
            state$conf[["summary table"]]$filter <- rf_get_subset()
            state$conf[["summary table"]]$order <- rf_get_order()
            state
        })

        rf_get_subset <- reactive({
            input$summ_subset
            dt <- if (NROW(input$summ_subset)==0) def_summ_subset else rhandsontable::hot_to_r(input$summ_subset)
            
            dt[Select == shinyscreen:::SUBSET_VALS[["GOOD"]], extra := T]
            dt[Select == shinyscreen:::SUBSET_VALS[["BAD"]], extra := F]
            sdt <- dt[!is.na(extra)]
            if (NROW(sdt) > 0) {
                sdt[,paste0(`QA Column`," == ",extra)]
            } else NULL
        })

        rf_get_order <- reactive({
            dt <- if (NROW(input$order_summ)==0) def_ord_summ else rhandsontable::hot_to_r(input$order_summ)
            
            tmp <- dt[Direction == "descending",.(`Column Name`=paste0("-",`Column Name`))]
            tmp[,`Column Name`]
        })

        rf_get_inp_datatab <- eventReactive(input$datatab,{
            ## z <- data.table::as.data.table(tryCatch(rhandsontable::hot_to_r(input$datatab)),
            ##                                error = function(e) def_datatab)

            
            ## z[,.(tag=as.character(tag),
            ##      adduct=as.character(adduct),
            ##      set=as.character(set)), with = T]
            z <- as.data.table(rv_datatab())
            z[,.(tag=as.character(tag),
                 adduct=as.character(adduct),
                 set=as.character(set)), with = T]
            
        })

        ## rf_get_inp_datafiles <- reactive({
        ##     input$datafiles
        ##     input$datafiles_cell_edit

        ##     isolate({
        ##         z <- DT::editData(rv_dfiles(),input$datafiles_cell_edit,'datafiles')
                
        ##         if (isTruthy(z)) z else def_datafiles})
        ## })

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
            tab <- rvs$m$out$tab$flt_summ
            req(NROW(tab)>0)
            gen_compsel_tab(tab)
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

        observeEvent(input$create_proj_b,{
            wd <- input$new_proj_name
            req(!is.null(wd) && !is.na(wd) && nchar(wd)>0)
            fullwd <- file.path(init$userdir,wd)
            dir.create(fullwd,recursive = F,showWarnings = F)

            ## Add to the project list if new.
            if (! (wd %in% rv_projects())) {
                message("Updating proj list.")
                
                new_projects <- list.dirs(path=init$userdir,
                                          full.names = F,
                                          recursive = F)
                rv_projects(new_projects)
                updateSelectInput(session = session,
                                  inputId = "proj_list",
                                  choices = new_projects,
                                  selected = wd)

                rvs$m <- list2rev(new_state())
                rvs$m$conf$project <- fullwd
                saveRDS(rev2list(rvs$m),file.path(fullwd,FN_STATE))

                
            } else {
                msg <- "Project already exists. Refusing to overwrite."
                message(msg)
                shinymsg(msg)
            }

        })

        observeEvent(input$load_proj_b,{
            
            wd <- input$proj_list
            req(!is.null(wd) && !is.na(wd) && nchar(wd)>0)
            fullwd <- file.path(init$userdir,wd)
            ## If a saved state exists, load it.
            fn_state <- file.path(fullwd,FN_STATE)
            if (file.exists(fn_state)) {
                rvs$m <- list2rev(readRDS(fn_state))
                rvs$m$conf$project <- fullwd
                update_gui(rvs$m$conf, session = session)
            } else {
                message("No saved state found. This directory is not a project.")
            }
        }, label = "project-b")

        observeEvent(rv_projects,
        {
            message("This is triggered as it should be.")
            
        },label = "upd-proj-list")


        observeEvent(input$save_proj_b,{
            fn <- file.path(rvs$m$conf$project,FN_STATE)
            shinymsg(paste("Saving state to: ",fn,"Please wait.",sep="\n"))
            message("(config) Saving state to: ", paste(fn,collapse = ","))
            fn <- if (length(fn)>0 && nchar(fn[[1]])>0) fn else ""

            if (nchar(fn) > 0) {
                m <- rev2list(rvs$m)
                ftab <- get_fn_ftab(m)
                fconf <- get_fn_conf(m)

                yaml::write_yaml(m$conf,
                                 file = fconf)
                shinyscreen:::tab2file(tab=m$input$tab$mzml,file=ftab)
                m$conf$data <- ftab
                saveRDS(object=m,file=fn)
            }
            shinymsg("Saving state completed.")
        })

        observeEvent(input$sel_indir_b,{
            indir <- input$indir_list
            req(isTruthy(indir))
            rvs$m$conf$indir <- file.path(init$indir, indir)
            message("Selected input dir:",rvs$m$conf$indir)
        })

        observeEvent(rvs$m$conf$indir,{
            indir <- rvs$m$conf$indir
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

        ## Hold your horses.
        ## observeEvent(rvs$m$conf$project,{
        ##     wd <- rvs$m$conf$project
        ##     req(isTruthy(wd))
        ##     updateSelectInput(session = session,
        ##                       inputId = 'comp_list',
        ##                       choices = list.files(rvs$m$conf$project,
        ##                                            pattern = "\\.csv$"))
            
        ## })
        
        observeEvent(input$comp_list_b, {
            sels <- input$comp_list
            req(isTruthy(sels))
            compfiles <- file.path(rvs$m$conf$indir,sels)
            message("(config) Selected compound lists: ", paste(sels,collapse = ","))
            rvs$m$conf$compounds$lists <- if (length(compfiles)>0 && nchar(compfiles[[1]])>0) compfiles else "Nothing selected."
            
        })

        observeEvent(input$set_list_b, {
            sels <- input$set_list
            req(isTruthy(sels))
            setfiles <- file.path(rvs$m$conf$indir,sels)
            message("(config) Selected set lists: ", paste(sels,collapse = ","))
            rvs$m$conf$compounds$sets <- if (length(setfiles)>0 && nchar(setfiles[[1]])>0) setfiles else "Nothing selected."
            
        })

        observeEvent(input$datafiles_cell_edit,{
            z <- DT::editData(rv_dfile(),
                              input$datafiles_cell_edit,
                              rownames = F)
            rv_dfile(z)
            
        }, label = "datafiles-edit")
        
        observeEvent(input$datafiles_b,{
            sels <- input$dfile_list
            req(isTruthy(sels))
            dfiles <- file.path(rvs$m$conf$indir,sels)
            message("(config) Selected mzMl files: ", paste(sels,collapse = ","))
            if (length(dfiles) > 0) {
                oldtab <- rv_dfile()
                
                newf <- setdiff(dfiles,oldtab$file)
                nr <- NROW(oldtab)
                tmp <- if (length(newf)>0) shinyscreen:::dtable(file=newf,tag=paste0('F',(nr+1):(nr + length(newf)))) else shinyscreen:::dtable(file=character(),tag=character())

                z <- rbind(oldtab, tmp)
                z[,tag:=as.character(tag)]
                rv_dfile(z)
            }
        })

        observe({
            df_tab <- rv_dfile()#rf_get_inp_datafiles()
            state <- rf_compound_input_state()
            isolate(oldtab <- rf_get_inp_datatab())
            
            oldt <- oldtab$tag
            tagl <- df_tab$tag
            diff <- setdiff(tagl,
                            oldt)
            
            res <- if (length(diff)!=0) {
                       ## Only change the tag names in the old ones.
                       pos_tag <- 1:length(tagl)
                       pos_old <- 1:NROW(oldtab)
                       pos_mod <- intersect(pos_tag,pos_old)
                       new_tag <- tagl[pos_mod]
                       if (NROW(oldtab)>0) oldtab[pos_mod,tag := ..new_tag]

                       ## Now add tags for completely new files, if any.
                       rest_new <- if (NROW(oldtab) > 0) setdiff(diff,new_tag) else diff
                       tmp <- shinyscreen:::dtable(tag=rest_new,
                                                   adduct=character(0),
                                                   set=character(0))

                       dt <-data.table::as.data.table(rbind(as.data.frame(oldtab),
                                                            as.data.frame(tmp)))
                       dt[tag %in% df_tab$tag,]
                   } else oldtab
            
            rv_datatab(res)
        })

        observe({
            mc <- rf_conf_state()
            rvs$m$conf <- mc$conf
        }, label = "conf_state")

        observe({
            dtab <- rv_datatab()
            dfiles <- rv_dfile()
            message("(config) Generating mzml from rv.")
            isolate(rvs$m$input$tab$mzml <- dtab[dfiles,on="tag"])
            message("(config) Done generating mzml from rv.")

            
        }, label = "mzml_from_rv")

        observe({
            dtab <- rf_get_inp_datatab()
            dfiles <- rv_dfile() #rf_get_inp_datafiles()
            req(isTruthy(dfiles) && NROW(dfiles)>0)
            message("(config) Generating mzml from inputs.")
            res <- dtab[dfiles,on="tag"]
            isolate(rvs$m$input$tab$mzml <- res)
            message("(config) Generating mzml from inputs.")

            
        }, label = "mzml_from_inp")

        observeEvent(input$extract_b,{
            shinymsg("Extraction has started. This may take a while.")
            m <- rf_conf_state()
            fn_c_state <- file.path(m$conf$project,
                                    paste0("extract.",shinyscreen:::FN_CONF))
            yaml::write_yaml(x=m$conf,file=fn_c_state)
            message("(extract) Config written to ", fn_c_state)
            state <- shinyscreen::run(m=m,
                                      phases=c("setup",
                                               "comptab",
                                               "extract"))
            message("(extract) Done extracting.")
            z <- shinyscreen::merge2rev(rvs$m,lst = state)
            eval(z)
            shinymsg("Extraction has been completed.")
        })

        observeEvent(input$presc_b,{
            validate(need(NROW(rvs$m$extr$ms1) > 0,
                          message = "Perform extraction first."))
            m <- rev2list(rvs$m)

            fn_c_state <- file.path(m$conf$project,
                                    paste0("presc.",shinyscreen:::FN_CONF))
            yaml::write_yaml(x=m$conf,file=fn_c_state)
            message("(prescreen) Config written to ", fn_c_state)
            shinymsg("Prescreening started. Please wait.")
            state <- shinyscreen::run(m=m,
                                      phases=c("prescreen"))
            message("(prescreen) Done prescreening.")
            shinymsg("Prescreening completed.")
            z <- shinyscreen::merge2rev(rvs$m,lst = state)
            eval(z)
            
            
        })

        observeEvent(input$sortsubset_b,{
            m <- rev2list(rvs$m)

            fn_c_state <- file.path(m$conf$project,
                                    paste0("sortsubset.",shinyscreen:::FN_CONF))
            yaml::write_yaml(x=m$conf,file=fn_c_state)
            message("(sortsubset) Config written to ", fn_c_state)
            state <- shinyscreen::run(m=m,
                                      phases=c("sort",
                                               "subset"))
            message("(sortsubset) Done with sorting and subsetting.")
            
            z <- shinyscreen::merge2rev(rvs$m,lst = state)
            eval(z)
            
            
        })

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
                message("Plots saved to ",file.path(rvs$m$conf$project,
                                                    FIG_TOPDIR))

            } else message("Nothing to save.")
        })

        observeEvent(input$plot_save_all, {
            req(NROW(rvs$m$out$tab$flt_summ)>0)
            create_plots(rvs$m)
        })

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
                     file=file.path(rvs$m$conf$project,
                                    "summary.csv"))
            shinymsg("Summary file export has been completed.")
        },label = "exportsumm_b")


        ## TODO: Uncomment this once done refacing.
        ## observeEvent(rvs$m$out$tab$comp,{
        ##     m <- gen_struct_plots(rev2list(rvs$m))
        ##     rvs$m$out$tab$structfig <- m$out$tab$structfig
        ## }, label = "gen_struct_plots")

        observe({
            
            rvs$m$conf$tolerance[["ms1 fine"]] <- uni_ass(input,
                                                             "ms1_fine",
                                                             "ms1_fine_unit")
            
            rvs$m$conf$tolerance[["ms1 coarse"]] <- uni_ass(input,
                                                               "ms1_coarse",
                                                               "ms1_coarse_unit")

            rvs$m$conf$tolerance[["eic"]] <- uni_ass(input,
                                                        "ms1_eic",
                                                        "ms1_eic_unit")

            rvs$m$conf$tolerance[["rt"]] <- uni_ass(input,
                                                       "ms1_rt_win",
                                                       "ms1_rt_win_unit")


        })

        observe({
            
            rvs$m$conf$prescreen[["ms1_int_thresh"]] <- input[["ms1_int_thresh"]]
            rvs$m$conf$prescreen[["ms2_int_thresh"]] <- input[["ms2_int_thresh"]]
            rvs$m$conf$prescreen[["s2n"]] <- input$s2n
            rvs$m$conf$prescreen[["ret_time_shift_tol"]] <- uni_ass(input,
                                                                       "ret_time_shift_tol",
                                                                       "ret_time_shift_tol_unit")

        })

        ## observe({
        ##     plot_group <- PLOT_FEATURES[[as.integer(input$plot_grp)]]
        ##     plot_plot <- PLOT_FEATURES[[as.integer(input$plot_grp_plot)]]
        ##     plot_label <-PLOT_FEATURES[[as.integer(input$plot_label)]]
        ##     isolate({
        ##         rvs$m$conf$figures$grouping$group <- plot_group
        ##         rvs$m$conf$figures$grouping$plot <- plot_plot
        ##         rvs$m$conf$figures$grouping$label <- plot_label
        ##     })

        ## }, label = "plot-grouping")
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
            rvs$m$conf$report$author <- input$rep_aut
            rvs$m$conf$report$title <- input$rep_tit
        })

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

        observeEvent(input$missingprec,{
            rvs$m$conf$extract$missing_precursor_info <- input$missingprec
        })
        

        ## Render Outputs
        output$project <- renderText(rvs$m$conf$project)

        output$comp_lists <- renderText({
            lsts <- rev2list(rvs$m$conf$compounds$lists)
            if (length(lsts) > 0 &&
                isTruthy(lsts) &&
                lsts != "Nothing selected.") {
                paste(c("<ul>",
                        sapply(lsts,
                               function (x) paste("<li>",x,"</li>")),
                        "</ul>"))
            } else "No compound list selected yet."
        })

        output$setids <- renderText({
            sets <- rvs$m$conf$compounds$sets
            if (isTruthy(sets) && sets != "Nothing selected.")
                paste("selected <em>setid</em> table:",
                      sets) else "No <em>setid</em> table selected."
        })

        output$order_summ <- rhandsontable::renderRHandsontable(rhandsontable::rhandsontable(def_ord_summ,
                                                                                             manualRowMove = T))

        output$datafiles <- DT::renderDT(
        {
            res <- rv_dfile()
            req(isTruthy(res))
            res$tag <- as.factor(res$tag)
            simple_style_dt(res, editable = list(target = "column",
                                                 disable= list(columns=0)))
        },
        server = T)

        output$datatab <- rhandsontable::renderRHandsontable(
        {
            setid <- rvs$m$input$tab$setid
            res <- rv_datatab()
            
            if (NROW(res)>0) {
                res$tag <- factor(res$tag,
                                  levels = c(unique(res$tag),
                                             "invalid"))
                res$set <- factor(res$set,
                                  levels = c(unique(setid$set),
                                             "invalid"))
                res$adduct <- factor(res$adduct,
                                     levels = shinyscreen:::DISP_ADDUCTS)
            }


            rhandsontable::rhandsontable(res,stretchH="all",
                                         allowInvalid=F)
        })

        
        output$comp_table <- DT::renderDataTable({
            state <- rf_compound_input_state()


            DT::datatable(state$input$tab$cmpds,
                          style = 'bootstrap',
                          class = 'table-condensed',
                          extensions = 'Scroller',
                          options = list(scrollX = T,
                                         scrollY = 200,
                                         deferRender = T,
                                         scroller = T))
        })

        
        output$setid_table <- DT::renderDataTable({
            state <- rf_compound_input_state()

            DT::datatable(state$input$tab$setid,
                          style = 'bootstrap',
                          class = 'table-condensed',
                          extensions = 'Scroller',
                          options = list(scrollX = T,
                                         scrollY = 200,
                                         deferRender = T,
                                         scroller = T))
        })

        output$summ_subset <- rhandsontable::renderRHandsontable({
            

            rhandsontable::rhandsontable(def_summ_subset)
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
            options <- list(scrollX=T,
                            dom = "t",
                            scroller = F,
                            ordering = F)
            styled_dt(rv_tran$qa_compsel_tab,
                      extensions = NULL,
                      selection = "none",
                      filter = 'none',
                      options = options,
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

        


        session$onSessionEnded(function () stopApp())
    }

    server
}
