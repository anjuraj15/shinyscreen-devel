## Managing the state of shiny application.


GUI_SELECT_INPUTS <- c("proj_list",
                       "indir_list",
                       "ms1_coarse_unit",
                       "ms1_fine_unit",
                       "ms1_rt_win_unit",
                       "ret_time_shift_tol_unit",
                       "dfile_list")
                  
GUI_NUMERIC_INPUTS <- c("ms1_coarse",
                   "ms1_fine",
                   "ms1_eic",
                   "ms1_rt_win",
                   "ms1_int_thresh",
                   "ms2_int_thresh",
                   "s2n",
                   "ret_time_shift_tol")

GUI_TEXT_INPUTS <- c("rep_aut",
                     "rep_tit")

GUI_RADIO_INPUTS <- c("missingprec")

GUI_ALL_INPUTS <- c(GUI_SELECT_INPUTS,
                      GUI_NUMERIC_INPUTS,
                      GUI_TEXT_INPUTS,
                      GUI_RADIO_INPUTS)

#' @export
create_stub_gui <- function() {
    gui <- list()
    shiny::isolate({
        gui$compounds <- shiny::reactiveValues(lists=character(),
                                               sets=character())
        gui$datatab <- shiny::reactiveValues(file=character(),
                                             tag=character(),
                                             adduct=character(),
                                             set=character())

        gui$paths <- shiny::reactiveValues(project=NA_character_,
                                           data=NA_character_)
        gui$project <- shiny::reactiveVal(NA_character_)
    })
    gui
}


create_gui <- function(project_path=NA_character_) {
    shiny::isolate({
        gui <- create_stub_gui()
        if (!is.na(project_path)) {
            gui$paths$project = project_path
            gui$project(basename(project_path))
        }
        
        gui
    })
}

#'@export
r2datatab <- function(rdatatab) {
    shiny::isolate({
        file <- rdatatab$file
        adduct <- rdatatab$adduct
        tag <- rdatatab$tag
        set <- rdatatab$set
        })
    if (length(file)==0L) file <- character(0)
    if (length(adduct)==0L) adduct <- rep(NA_character_,length(file))
    if (length(tag)==0L) tag <- rep(NA_character_,length(file))
    if (length(set)==0L) tag <- rep(NA_character_,length(file))
    data.table(tag=tag,adduct=adduct,set=set,file=file)
}

gen_dtab <- function(tablist,sets) {
    data.table(tag=factor(tablist$tag,levels=unique(tablist$tag)),
               adduct=factor(tablist$adduct,levels=DISP_ADDUCTS),
               set=factor(tablist$set,levels=sets))
}

r2compounds <- function(rcompounds) {
    shiny::isolate({
        cmpd_lists <- rcompounds$lists
        cmpd_set <- rcompounds$sets
        })

    list(lists=cmpd_lists,sets=cmpd_set)
    }


#' @export
pack_app_state <- function(input, gui) {
    pack <- list()
    shiny::isolate({
        pack_inputs <- list()
        pack_input_names <- which_gui_inputs()
        pack_inputs <- shiny::reactiveValuesToList(input)[pack_input_names]
        pack$input <- pack_inputs
        pack$datatab <- r2datatab(gui$datatab)
        pack$compounds <- r2compounds(gui$compounds)
        pack$paths <- list()
        pack$paths$data <- gui$paths$data

        })
    pack
}

which_gui_inputs <- function() {
    GUI_ALL_INPUTS
}


which_gui_select_inputs <- function() {
    GUI_SELECT_INPUTS
    }

which_gui_numeric_inputs <- function() {
    GUI_NUMERIC_INPUTS
    }

which_gui_text_inputs <- function() {
    GUI_TEXT_INPUTS
}

which_gui_radio_inputs <- function() {
    GUI_RADIO_INPUTS
}

unpack_app_state <- function(session,input,project_path,packed_state) {
    shiny::isolate({
        for (inp in which_gui_select_inputs()) {
            shiny::updateSelectInput(session = session,
                                     inputId = inp,
                                     selected = packed_state$input[[inp]])
        }
        
        for (inp in which_gui_numeric_inputs()) {
            shiny::updateNumericInput(session = session,
                                      inputId = inp,
                                      value = packed_state$input[[inp]])
        }
        
        for (inp in which_gui_text_inputs()) {
            shiny::updateTextInput(session = session,
                                   inputId = inp,
                                   value = packed_state$input[[inp]])
        }
        
        for (inp in which_gui_radio_inputs()) {
            shiny::updateRadioButtons(session = session,
                                      inputId = inp,
                                      selected = packed_state$input[[inp]])
        }
        
        gui <- create_gui(project_path)
        gui$compounds$lists <- packed_state$compounds$lists
        gui$compounds$sets <- packed_state$compounds$sets
        gui$datatab$file <- packed_state$datatab$file
        gui$datatab$adduct <- packed_state$datatab$adduct
        gui$datatab$tag <- packed_state$datatab$tag
        gui$datatab$set <- packed_state$datatab$set
        gui$paths$data <- packed_state$paths$data
        gui
    })

 

}


input2conf_setup <- function(input,gui,conf=list()) {
    conf$compounds <- list()
    conf$figures <- list()
    conf$prescreen <- list()
    conf$tolerance <- list()
    conf$extract <- list()
    conf$summary_table <- list()
    conf$report <- list()
    
    conf$debug <- F

    conf$compounds$lists <- gui$compounds$lists
    conf$compounds$sets <- gui$compounds$sets
    
    conf$tolerance[["ms1 fine"]] <- paste(input$ms1_fine,input$ms1_fine_unit)
    conf$tolerance[["ms1 coarse"]] <- paste(input$ms1_coarse,input$ms1_coarse_unit)
    conf$tolerance[["eic"]] <- paste(input$ms1_eic,input$ms1_eic_unit)
    conf$tolerance[["rt"]] <- paste(input$ms1_rt_win,input$ms1_rt_win_unit)
    conf$extract$missing_precursor_info <- input$missingprec
    conf
}


input2conf_prescreen <- function(input,conf) {
    conf$prescreen[["ms1_int_thresh"]] <- input$ms1_int_thresh
    conf$prescreen[["ms2_int_thresh"]] <- input$ms2_int_thresh
    conf$prescreen[["s2n"]] <- input$s2n
    conf$prescreen[["ret_time_shift_tol"]] <- paste(input$ret_time_shift_tol,input$ret_time_shift_tol_unit)
    conf
}

input2conf_figures <- function(input,conf) {
    conf$figures$rt_min <- paste(input$plot_rt_min,input$plot_rt_min_unit)
    conf$figures$rt_max <- paste(input$plot_rt_max,input$plot_rt_max_unit)
    conf$figures$ext <- input$plot_ext
    conf
    
}

input2conf_report <- function(input,conf) {
    conf$report$author <- input$rep_aut
    conf$report$title <- input$rep_tit
    conf

}
input2conf <- function(input,gui,conf=list()) {
    conf <- input2conf_setup(input,gui=gui,conf)
    conf <- input2conf_prescreen(input,conf)
    conf <- input2conf_figures(input,conf)
    conf <- input2conf_report(input,conf)
    conf
}

app_state2state <- function(input,gui) {
    shiny::req(gui$paths$project)
    m <- new_project(gui$paths$project)
    m$run$paths <- shiny::reactiveValuesToList(gui$paths)
    m$conf <- input2conf_setup(input,gui=gui)
    m$input$tab$mzml <- gui2datatab(gui)
    m
}


gen_comp_state <- function(input,gui) {
    m <- app_state2state(input,gui)
    run(m=m,phases=c("setup","mk_comp_tab"))
}

    
get_sets <- function(gui) {
    fn_sets <- file.path(gui$paths$project,gui$compounds$sets)
    df <- fread(file=fn_sets)
    df[,unique(set)]
}


gen_dfiles_tab <- function(gui) {
    curr_file <- gui$datatab$file
    curr_tag <- gui$datatab$tag
    
    res <- data.table(file=curr_file,tag=curr_tag)
    res[,tag:=as.factor(tag)]
    
}

gui2datatab <- function(gui) {
    df <- data.table(tag=as.character(gui$datatab$tag),
                     adduct=as.character(gui$datatab$adduct),
                     set=as.character(gui$datatab$set),
                     file=as.character(gui$datatab$file))
    df
                     
}
