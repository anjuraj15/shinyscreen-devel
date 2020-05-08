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

##' @importFrom shiny validate

react_v <- shiny::reactiveValues
react_f <- shiny::reactive
react_e <- shiny::eventReactive
obsrv <- shiny::observe
obsrv_e <- shiny::observeEvent
isol <- shiny::isolate

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

rev2list <- function(rv) {
    ## Take reactive values structure and convert them to nested
    ## lists.
    if (class(rv)[[1]] != "reactivevalues")
        rv else lapply(shiny::reactiveValuesToList(rv),rev2list)
}

list2rev <- function(lst) {
    ## Take nested named list and create reactive values from it.
    if (class(lst)[[1]] != "list")
        lst else do.call(react_v,lapply(lst,list2rev))
}

new_rv_state <- function(project) {
    p <- normalizePath(path=project,winslash = '/')
    x <- react_v(m=list2rev(new_state(list(project=p,data=""),GUI=T)))
    x
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
