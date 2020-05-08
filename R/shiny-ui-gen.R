## Copyright (C) 2020 by University of Luxembourg

## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at

##     http://www.apache.org/licenses/LICENSE-2.0

## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License. 

## Commentary:
##
## Defines gen tab.


mk_ui_gen <- function() {
    genBoxExtract<-prim_box(title="Extract Spectra",
                            num_input(inputId="genNoProc",
                                      label="Number of processes:  ",
                                      value=1),
                            num_input_unit(inputId="errCoarse",
                                           l1="Precursor m/z error (coarse) (+/-):  ",
                                           l2="[Da]",
                                           value=MS1_ERR_COARSE),
                            num_input_unit("errFinePPM",
                                           l1="Precursor m/z error (fine) (+/-):  ",
                                           l2="[ppm]",
                                           value=MS1_ERR_FINE),
                            num_input_unit("errEIC",
                                           l1="EIC m/z error (+/-): ",
                                           l2="[Da]",
                                           value=EIC_ERR),
                            num_input_unit("errRTWin",
                                           l1="Retention time tolerance (+/-): ",
                                           l2="[min]",
                                           value=RT_EXTR_ERR),
                            shiny::uiOutput("genSetSelInpCtrl"),
                            shiny::actionButton(inputId="genRunB",
                                                label="Run!",
                                                icon=shiny::icon("bomb")),
                            width=NULL)
    
    genBoxAutoQA<-prim_box(title="Automatic Quality Control",
                           num_input("intThreshMS1",
                                     label="Intensity threshold (MS1): ",
                                     value=MS1_INT_THOLD),
                           num_input("intThreshMS2",
                                     label="Intensity threshold (MS2): ",
                                     value=MS2_INT_THOLD),
                           num_input("noiseFac",
                                     label="Signal-to-noise ratio: ",
                                     value=MS1_SN_FAC),
                           num_input_unit("errRT",
                                          l1="Retention time shift tolerance (+/-): ",
                                          value=RT_SHIFT_ERR,
                                          l2="[min]"),
                           shiny::actionButton(inputId="qaAutoB",
                                               label="Preprocess!",
                                               icon=shiny::icon("bomb")),
                           width=NULL)
    
    genBoxProcessed<-prim_box(title="Processed Sets",
                              rhandsontable::rHandsontableOutput("genTabProcCtrl"),
                              width=NULL)
    
    genTab<-shinydashboard::tabItem(tabName = "gen",
                                    shiny::h2(GUI_TAB_TITLE[["gen"]]),
                                    shiny::fluidRow(shiny::column(genBoxExtract,
                                                                  width=4),
                                                    shiny::column(genBoxProcessed,
                                                                  genBoxAutoQA,width=4)))
    genSideItem <- shinydashboard::menuItem(text=GUI_SIDE_TITLE[["gen"]],
                                            tabName="gen",
                                            icon=shiny::icon("cogs"))

    return(list(tab=genTab,
                side=genSideItem))
}


server_gen <- function(input,output,session,rv,rf,roots) {
    rv
}
