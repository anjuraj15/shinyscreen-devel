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

    cmpdsListBox<-prim_box(title="Compounds List",
                         rhandsontable::rHandsontableOutput("cmpdsCtrl"),
                         width=NULL)

    compFileList <- shiny::verbatimTextOutput("filelist")
    ## shiny::tags$style(type="text/css", "#filelist {white-space: pre-wrap;}")
    cmpListLayout <- shiny::fluidRow(shiny::column(
                                                compFileList,
                                                cmpdsListBox,
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

    output$filelist <- shiny::renderText({
        header <- "Compounds list generated from files:"
        cmpds <- rv$m$input$tab$cmpds

        files <- unique(cmpds$ORIG)
        entries <- sapply(files,function(fn) paste0('- ',fn))
        paste(c(header,entries),collapse = '\n')
    })
    
    output$cmpdsCtrl <- rhandsontable::renderRHandsontable({
        df<-rv$m$input$tab$cmpds
        out<-if (!is.null(df)) {
                 df
             } else {
                 EMPTY_CMPD_LIST
             }
        rhandsontable::rhandsontable(out,stretchH="all")
    })
    rv
}
