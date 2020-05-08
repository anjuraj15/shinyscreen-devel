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


    ## Plugins
    conf <- mk_ui_config()
    cmpd <- mk_ui_cmpd()
    sets <- mk_ui_sets()
    gen <- mk_ui_gen()
    
    sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id='tabs',
                                                                            conf$side,
                                                                            gen$side,
                                                                            shiny::hr(),
                                                                            shiny::h5("Inputs"),
                                                                            cmpd$side,
                                                                            sets$side,
                                                                            shiny::hr()))
    body <- shinydashboard::dashboardBody(
                                shiny::tags$head(shiny::tags$style(shiny::includeHTML(fn_style))),
                                shinydashboard::tabItems(conf$tab,
                                                         cmpd$tab,
                                                         gen$tab,
                                                         sets$tab))

    shinydashboard::dashboardPage(
                        header,
                        sidebar,
                        body)

}

mk_shinyscreen <- function(wd=getwd(),fn_style=system.file('www/custom.css',package = 'shinyscreen')) {
    roots <- mk_roots(wd)
    


    server <- function(input,output,session) {
        ## Top-level server function.
        
        rf <- list()           # Container for all
                               # reactive functions.
    
        rv <- new_rv_state(project=wd)   # Container for all
                                         # reactive values.        
        



        
        rf <- react_conf_f(input,output,session,rv=rv,rf=rf) # Config related r. functions.

        ## Plugin servers.
        rv <- server_conf(input,output,session,rv=rv,rf=rf,roots=roots)
        rv <- server_cmpd(input,output,session,rv=rv,rf=rf,roots=roots)
        rv <- server_sets(input,output,session,rv=rv,rf=rf,roots=roots)


        
        obsrv_e(rv,{
            message(paste("rv changed at ",Sys.time()))
        })
        
        session$onSessionEnded(function () {
            stopApp()
        })
    }
    shiny::shinyApp(ui=mk_ui(fn_style=fn_style),server=server)

}


##' @export
launch <- function(GUI=T,fn_conf="",wd=getwd(),...) {
    if (GUI) {
        app<-mk_shinyscreen(wd=wd)
        shiny::runApp(appDir = app,...)
    } else {
        if (nchar(fn_conf)==0) {
            fn_conf <- commandArgs(trailingOnly=T)[[1]]
        }
        run(fn_conf)
    }
}
