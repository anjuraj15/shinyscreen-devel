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
## Defines config tab.

mk_ui_config <- function() {
    browseFile <- function(title,
                           buttonName,
                           txtName,
                           buttonTxt="Import compound list.",
                           txtTxt="",
                           icon="file",
                           description=NULL,
                           ...) {
        prim_box(title=title,
                 shiny::h5(description),
                 collapsible=F,...)}
    
    confImport <- prim_box(title="Import",
                           shiny::uiOutput("fnKnownLCtrl"),
                           shiny::uiOutput("fnUnkLCtrl"),
                           shiny::uiOutput("fnSetIdCtrl"),
                           shinyFiles::shinyFilesButton("impKnownListB",
                                                        label="Import knowns.",
                                                        title="",
                                                        icon=shiny::icon("file"),
                                                        multiple=F),
                           shinyFiles::shinyFilesButton("impUnkListB",
                                                        label="Import unknowns.",
                                                        title="",
                                                        icon=shiny::icon("file"),
                                                        multiple=F),
                           shinyFiles::shinyFilesButton("impSetIdB",
                                                        label="Import set ID table.",
                                                        title="",
                                                        icon=shiny::icon("file"),
                                                        multiple=T),
                           width=NULL)

    confmzMLTags <- prim_box(title="Tags",
                             shiny::textInput("tagPropInp",
                                              "What is a tag? (optional)",
                                              value=TAG_DEF_DESC),
                             shiny::textInput("tagsInp",
                                              "Comma-delimited list of tag types.",
                                              value=""),
                             shiny::actionButton("updTagsB",
                                                 label = "Update tags.",
                                                 icon=shiny::icon("bomb")),

                             width=NULL)

    confState <- prim_box(title="Configuration State",
                          shinyFiles::shinySaveButton("saveConfB",
                                                      "Save configuration.",
                                                      title="Save",
                                                      filename = "conf-state.yaml",
                                                      "yaml"),
                          shinyFiles::shinyFilesButton("restoreConfB",
                                                       label="Restore configuration.",
                                                       multiple=F,
                                                       title="Restore"),
                          shiny::actionButton(inputId="resetConfB",
                                              label="Reset config (CAUTION!)",
                                              icon=shiny::icon("trash")),
                          width=NULL)

    confProj <- prim_box(title="Project",
                         shinyFiles::shinyDirButton(id="switchProjB",
                                                    label="Switch project.",
                                                    title="Switch project.",
                                                    icon=shiny::icon("recycle")),
                         width=NULL)


    confmzMLtab <-prim_box(title="Raw Files in mzML Format",
                           shiny::h5("Use this file table to assign adduct modes and tags to the data files."),
                           shinyFiles::shinyFilesButton("mzMLB",
                                                        label="Select mzML files",
                                                        title="Select mzML files",
                                                        icon=shiny::icon("files-o"),
                                                        multiple=T),
                           rhandsontable::rHandsontableOutput("mzMLtabCtrl"),
                           width=NULL)

    
    confLayout <- shiny::fluidRow(shiny::column(confImport,
                                                confmzMLTags,
                                                confState,
                                                confProj,
                                                width=4),
                                  shiny::column(width=8,
                                                confmzMLtab))


    confTab <- shinydashboard::tabItem(tabName="config",
                                       shiny::h2(GUI_TAB_TITLE[["conf"]]),
                                       confLayout)

    confSideItem <- shinydashboard::menuItem(text=GUI_SIDE_TITLE[["conf"]],
                                             tabName="config",
                                             icon=shiny::icon("user-cog"))

    return(list(tab=confTab,
                side=confSideItem))
}

react_conf_v <- function(input,output,session,rv,rf) {
    rv$conf <- react_v(data=CONF$data,
                       project=CONF$project,
                       compounds=react_v(known=CONF$compounds$known,
                                         unknown=CONF$compounds$unknown,
                                         sets=CONF$compounds$sets))

    rv
}

react_conf_f <- function(input,output,session,rv,rf) {
    rf$get_proj_vol <- react_e(rv$conf$project,{
        ## For shinyfiles dialogs.
        path <- normalizePath(rv$conf$project, winslash = '/')
        vls <- vols()() #Ugly! :)
        vol <- path2vol(path)
        sel<-match(vol,vls)
        validate1(sel,msg="Yikes! Unable to detect current project's volume.")
        res<-names(vls)[[sel]]
        res
    })

    rf$get_proj_path <- react_e(rv$conf$project,{
        ## For shinyfiles dialogs.
        wd <- rv$conf$project
        vol <- rf$get_proj_vol()
        v <- vols()()
        pref<-v[[vol]]
        res<-wd 
        message('Relative project path is: ',res)
        res
    })
    
    rf
}

server_conf <- function(input,output,session,rv,rf) {
    ## ***** shinyFiles observers *****
    droot <- rf$get_proj_vol
    dpath <- rf$get_proj_path
    shinyFiles::shinyFileChoose(input, 'impKnownListB',defaultRoot=droot(),
                                defaultPath=dpath(),roots=volumes)
    shinyFiles::shinyFileChoose(input, 'impUnkListB',defaultRoot=droot(),
                                defaultPath=dpath(),roots=volumes)
    shinyFiles::shinyFileChoose(input, 'impSetIdB',defaultRoot=droot(),
                                defaultPath=dpath(),roots=volumes)
    
    shinyFiles::shinyFileSave(input, 'saveConfB',defaultRoot=droot(),
                              defaultPath=dpath(),roots=volumes)
    shinyFiles::shinyFileChoose(input, 'restoreConfB',defaultRoot=droot(),
                                defaultPath=dpath(),roots=volumes)
    shinyFiles::shinyFileChoose(input, 'mzMLB',defaultRoot=droot(),
                                defaultPath=dpath(),roots=volumes)
    shinyFiles::shinyDirChoose(input, 'switchProjB',roots=volumes)

    obsrv_e(input$saveConfB, {
        conf<-rv_conf2conf(rv)
        vol <- vol_f()
        fn <- shinyFiles::parseSavePath(roots=vol_f,input$saveConfB)[["datapath"]]
        validate1(fn,msg="Invalid file to save config to.")
        write_conf(conf,fn)
    })

    obsrv_e(input$restoreConfB,{
        fn <- shinyFiles::parseSavePath(roots=vol_f,input$saveConfB)[["datapath"]]
        validate(need())
        validate1(fn,msg="Something went wrong with the config file name.")
        validate1(file.exists(fn),msg="The file is unreadable.")
        conf <- read_conf(fn)
        rv <- conf2rv_conf(conf,rv)
        
    })
    

    obsrv_e(rv$conf,message("updated rv"))


    ## ***** Render *****
    output$fnKnownLCtrl <- shiny::renderUI({
        txt_file_input(inputId = 'fnKnownL',
                       input = input,
                       label = html("The list of knowns. Required columns: <i>ID</i>, <i>SMILES</i>, <i>Name</i> and <i>RT</i> (the last two can be empty). Remember to quote <i>SMILES</i> and <i>Name</i> entries!"),
                       fileB = 'impKnownListB',
                       volumes=volumes)
    })
    output$fnUnkLCtrl <- shiny::renderUI({
        txt_file_input(inputId = 'fnUnkL',
                       input = input,
                       label = html("The list of unknowns. Required columns: <i>ID</i>, <i>mz</i> and <i>RT</i> (<i>RT</i> can be empty)."),
                       fileB = 'impUnkListB',
                       volumes=volumes)
    })
    output$fnSetIdCtrl <- shiny::renderUI({
        txt_file_input(inputId = 'fnSetId',
                       input = input,
                       label = html("Set table. Required columns <i>ID</i> and <i>set</i>."),
                       fileB = 'impSetIdB',
                       volumes=volumes)
    })
    
    rv
}
