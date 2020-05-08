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
## Defines compounds sets tab.


mk_ui_sets <- function() {
    ## ***** Sets of compounds *****
    setIdBox<-prim_box(title="Compound Sets",
                       rhandsontable::rHandsontableOutput("setIdCtrl"),
                       width = NULL)

    setIdLayout<-shiny::fluidRow(shiny::column(setIdBox,
                                               width = 12))

    setIdTab<-shinydashboard::tabItem(tabName="setId",
                                      setIdLayout)

    setIdSideItem <- shinydashboard::menuItem(text="Compound sets",
                                              tabName="setId",
                                              icon=shiny::icon("table"))
    return(list(tab=setIdTab,
                side=setIdSideItem))

}


server_sets <- function(input,output,session,rv,rf,roots) {
    output$setIdCtrl<- rhandsontable::renderRHandsontable({
        df<-rv$m$input$tab$setid
        rhandsontable::rhandsontable(df,stretchH="all")
    })

    rv

}
