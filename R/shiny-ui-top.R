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
## This is a top-level Shiny file to bind them all.

mk_ui <- function (fn_style) {
    ## Top-level UI function.

    headerText <- "Shinyscreen"
    header <- shinydashboard::dashboardHeader(title=headerText,
                                              shinydashboard::dropdownMenuOutput("notify"))
    sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id='tabs',
                                                                            shiny::hr(),
                                                                            shiny::h5("Inputs"),
                                                                            shiny::hr()))
    body <- shinydashboard::dashboardBody(
                                shiny::tags$head(shiny::tags$style(shiny::includeHTML(fn_style))),
                                shinydashboard::tabItems())

    shinydashboard::dashboardPage(
                        header,
                        sidebar,
                        body)

}

mk_shinyscreen <- function(fn_style=system.file('www/custom.css',package = 'shinyscreen')) {
    server <- function(input,output,session) {
        ## Top-level server function.
        session$onSessionEnded(function () {
            stopApp()
        })
    }
    shiny::shinyApp(ui=mk_ui(fn_style=fn_style),server=server)

}


##' @export
launch <- function(GUI=T,fnConf="",...) {
    if (GUI) {
        app<-mk_shinyscreen()
        shiny::runApp(appDir = app,...)
    } else {
        if (nchar(fnConf)==0) {
            fnConf <- commandArgs(trailingOnly=T)[[1]]
        }
        run(fnConf)
    }
}
