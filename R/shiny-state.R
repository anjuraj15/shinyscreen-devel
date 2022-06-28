## Managing the state of shiny application.


GUI_SELECT_INPUTS <- c("proj_list",
                  "indir_list",
                  "ms1_coarse_unit",
                  "ms1_fine_unit",
                  "ms1_rt_win_unit",
                  "ret_time_shift_tol")
                  
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
create_rvs <- function(project_path) {
    rvs <- list()
    rvs$m <- new_empty_project(project_path)
    
    rvs$compounds <- shiny::reactiveValues(lists=character(),
                                           set=character())
    
    rvs$datatab <- shiny::reactiveValues(file=character(),
                                         tag=character(),
                                         adduct=character(),
                                         set=character())

    rvs
                         
}

#'@export
r2datatab <- function(rdatatab) {
    shiny::isolate({
        file <- rdatatab$file
        adduct <- rdatatab$adduct
        tag <- rdatatab$tag
        set <- rdatatab$set
        if (is.null(adduct)) adduct <- rep(NA_character_,length(file))
        if (is.null(tag)) tag <- rep(NA_character_,length(file))
        if (is.null(set)) tag <- rep(NA_character_,length(file))
        })
    data.table(tag=tag,adduct=adduct,set=set,file=file)
}

r2compounds <- function(rcompounds) {
    shiny::isolate({
        cmpd_lists <- rcompounds$lists()
        cmpd_set <- rcompounds$set()
        })

    list(lists=cmpd_lists,set=cmpd_set)
    }


#' @export
pack_app_state <- function(input, rvs) {
    gui <- list()
    shiny::isolate({
        gui_inputs <- list()
        gui_input_names <- which_gui_inputs()
        gui_inputs <- shiny::reactiveValuesToList(input)[gui_input_names]
        gui$input <- gui_inputs
        gui$datatab <- r2datatab(rvs$datatab)
        gui$compounds <- r2compounds(rvs$compounds)

        })
    gui
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
            message("Updating: ",inp)
            shiny::updateSelectInput(session = session,
                                     inputId = inp,
                                     selected = packed_state$input[inp])
        }
        
        for (inp in which_gui_numeric_inputs()) {
            message("Updating: ",inp)
            shiny::updateNumericInput(session = session,
                                      inputId = inp,
                                      value = packed_state$input[inp])
        }
        
        for (inp in which_gui_text_inputs()) {
            message("Updating: ",inp)
            shiny::updateTextInput(session = session,
                                   inputId = inp,
                                   value = packed_state$input[inp])
        }
        
        for (inp in which_gui_radio_inputs()) {
            message("Updating: ",inp)
            shiny::updateRadioButtons(session = session,
                                      inputId = inp,
                                      selected = packed_state$input[inp])
        }
        
        ## df <- file2tab(rvs$m$run$paths$datatab)
        ## dfile <- data.table::copy(df[,tag:=as.character(tag),with=T])
        ## rv_dfile(dfile)
        ## nms <- colnames(df)
        ## nms <- nms[nms!="file"]
        ## fdt <- df[,..nms]
        ## rv_datatab(fdt)
        ## rv_flag_datatab(rv_flag_datatab() + 1L)
        ## NULL

        rvs <- create_rvs(project_path)
        rvs$compounds$lists <- packed_state$compounds$lists
        rvs$compounds$sets <- packed_state$compounds$sets
        rvs$datatab$file <- packed_state$file
        rvs$datatab$adduct <- packed_state$adduct
        rvs$datatab$tag <- packed_state$tag
        rvs$datatab$set <- packed_state$set
            
    })

    rvs

}

