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
## Defines compound lists tab.


mk_ui_cmpd <- function() {
    ## ***** Compound List Tab *****

    knownListBox<-prim_box(title="Known Compounds List",
                         rhandsontable::rHandsontableOutput("knownCtrl"),
                         width=NULL)

    unkListBox<-prim_box(title="Unknown Compounds List",
                         rhandsontable::rHandsontableOutput("unkCtrl"),
                         width=NULL)
    
    cmpListLayout <- shiny::fluidRow(shiny::column(knownListBox,
                                                   unkListBox,
                                                   width = 12))

    cmpListTab <- shinydashboard::tabItem(tabName="compList",
                                          cmpListLayout)

    compListSideItem <- shinydashboard::menuItem(text="Compound list",
                                                 tabName="compList",
                                                 icon=shiny::icon("table"))

    return(list(tab=cmpListTab,
                side=compListSideItem))
}

server_cmpd <- function(input,output,session,rv,rf,roots) {
    output$knownCtrl <- rhandsontable::renderRHandsontable({
        df<-rv$m$input$tab$known
        out<-if (!is.null(df)) {
                 df
             } else {
                 data.frame(ID=numeric(),Name=character(),SMILES=character(),RT=numeric())
             }
        rhandsontable::rhandsontable(out,stretchH="all")
    })

    output$unkCtrl <- rhandsontable::renderRHandsontable({
        df<-rv$m$input$tab$unknown
        out<-if (!is.null(df)) {
                 df
             } else {
                 data.frame(ID=numeric(),mz=numeric())
             }
        rhandsontable::rhandsontable(out,stretchH="all")
    })

    rv
}
