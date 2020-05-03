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
vols <- shinyFiles::getVolumes
vol_f <- vols()
isol <- shiny::isolate
volumes <- function() c(wd=getwd(), shinyFiles::getVolumes()())
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

txt_file_input <- function(inputId,input,fileB,label,volumes) {

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
                             value = "")
        } else {
            message('Why is this happening so much?')
            shiny::textInput(inputId = inputId,
                             label = label,
                             value = currFn)
        }
    }
    
}

rev2list <- function(rv) {
    ## Take reactive values structure and convert them to nested
    ## lists.
    if (class(rv) != "reactivevalues")
        rv else lapply(shiny::reactiveValuesToList(rv),rev2list)
}

list2rev <- function(lst) {
    ## Take nested named list and create reactive values from it.
    if (class(lst) != "list")
        lst else do.call(react_v,lapply(lst,list2rev))
}

txt2tags <- function(txt) {
    ## Turns a string into tags
    x <- if (shiny::isTruthy(txt)) {
             trimws(unlist(strsplit(txt, ",")))
         } else list()
    
    
    as.list(c("unspecified",x))
}

combine_tags <- function(df_tags,txt_tags) {
    diff <- setdiff(df_tags,txt_tags)
    for (x in diff) df_tags[df_tags %in% x] <- "unspecified"
    df_tags <- factor(as.character(df_tags))
    df_tags <- factor(as.character(df_tags),levels = unique(c('unspecified',levels(df_tags),txt_tags)))
    df_tags
}

add_mzML_files<-function(df,paths) {
    lSet<-levels(df$set)
    if (length(lSet>0) && !is.na(lSet)) {
        nR<-length(paths)
        if (nR>0) {
            st<-nrow(df)+1
            fi<-nrow(df)+nR
            df[st:fi,'tag']<-levels(df$tag)[[1]]
            df[st:fi,'set']<-levels(df$set)[[1]]
            df[st:fi,'mode']<-levels(df$mode)[[1]]
            df[st:fi,'Files']<-paths
        }
        df
    } else {
        warning("Define sets using the compound set table before trying to add files!")
        df
    }
}
