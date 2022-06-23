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
which_gui_inputs <- function() {
    GUI_ALL_INPUTS
}


#' @export
which_gui_select_inputs <- function() {
    GUI_SELECT_INPUTS
    }

#' @export
which_gui_numeric_inputs <- function() {
    GUI_NUMERIC_INPUTS
    }

#' @export
which_gui_text_inputs <- function() {
    GUI_TEXT_INPUTS
}

#' @export
which_gui_radio_inputs <- function() {
    GUI_RADIO_INPUTS
}

