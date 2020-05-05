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

react_conf_f <- function(input,output,session,rv,rf) {

    rf$get_tags_from_txt <- react({
        ## Tags in the text box.
        input$updTagsB
        if (isTruthy(input$tagsInp)) txt2tags(input$tagsInp) else TAG_DEF
    })
    
    rf
}

server_conf <- function(input,output,session,rv,rf,roots) {
    
    ## ***** shinyFiles observers *****



    shinyFiles::shinyFileChoose(input, 'impKnownListB',defaultRoot=roots$def_vol(),
                                defaultPath=roots$def_path(),roots=roots$get)
    shinyFiles::shinyFileChoose(input, 'impUnkListB',defaultRoot=roots$def_vol(),
                                defaultPath=roots$def_path(),roots=roots$get)
    shinyFiles::shinyFileChoose(input, 'impSetIdB',defaultRoot=roots$def_vol(),
                                defaultPath=roots$def_path(),roots=roots$get)
    
    shinyFiles::shinyFileSave(input, 'saveConfB',defaultRoot=roots$def_vol(),
                              defaultPath=roots$def_path(),roots=roots$get)
    shinyFiles::shinyFileChoose(input, 'restoreConfB',defaultRoot=roots$def_vol(),
                                defaultPath=roots$def_path(),roots=roots$get)
    shinyFiles::shinyFileChoose(input, 'mzMLB',defaultRoot=roots$def_vol(),
                                defaultPath=roots$def_path(),roots=roots$get)
    shinyFiles::shinyDirChoose(input, 'switchProjB',
                               roots=roots[["get"]],
                               defaultRoot = "Computer",
                               defaultPath = "")


    obsrv_e(input$switchProjB,{
        ## Update volumes function as needed.
        spath<-shinyFiles::parseDirPath(roots=roots$get,
                                        selection=input$switchProjB)
        path<- if(length(spath)>0) spath[[1]] else NA
        if (shiny::isTruthy(path)) {
            rv$m$conf$project <- path
        }
    })

    obsrv_e(input$saveConfB, {
        conf<-rev2list(rv$m$conf)
        fn <- shinyFiles::parseSavePath(roots=roots$get,input$saveConfB)[["datapath"]]
        validate1(fn,msg="Invalid file to save config to.")
        write_state(rev2list(rv$m),fn)
    })

    obsrv_e(input$restoreConfB,{
        fn <- shinyFiles::parseFilePaths(roots=roots$get,input$restoreConfB)[["datapath"]]
        assert(file.exists(fn), msg="The file is unreadable.")
        rv$m$conf <- list2rev(read_conf(fn))
        for (nm in names(rv$m$conf$compounds)) {
            shiny::updateTextInput(session=session,
                                   inputId=nm,
                                   value=rv$m$conf$compounds[[nm]])
        }
        ## Tags
    })

    obsrv_e(input$mzMLB,
    {
        shiny::req(input$mzMLB)
        fchoice<-shinyFiles::parseFilePaths(roots = roots$get,input$mzMLB)
        paths<-fchoice[["datapath"]]
        df <- rhandsontable::hot_to_r(input$mzMLtabCtrl)
        df <- add_mzML_files(df,paths)
        rv$m$input$tab$mzml <- disp2mzml(df)
    })

    obsrv_e(rv$m$conf,message("updated rv$m$conf"))

    obsrv({
        ## Building rv objects here. Probably should change to
        ## something like reactive get_m.
        
        rv$m$conf$compounds$known <- input$known
        rv$m$conf$compounds$unknown <- input$unknown
        rv$m$conf$compounds$sets <- input$sets
        
        assert(isTruthy(rv$m$conf$compounds$known) || isTruthy(rv$m$conf$compounds$unknown),
               msg = "Please provide at least one (known, or unknown) compounds table.")
        assert(isTruthy(rv$m$conf$compounds$sets), msg = "Please provide the compounds set table.")
        rv$m <- load_compound_input(rv$m)
        if (nrow(rv$m$input$tab$mzml)==0 && file.exists(rv$m$conf$data)) rv$m <- load_data_input(rv$m)

        
        ## Rebuild tags.
        isol({
            df_tags <- unique(rv$m$input$tab$mzml$tag)
            txt_tags <- input$tagsInp
            new_tags <- combine_tags(df_tags,txt_tags)
            shiny::updateTextInput(session=session,
                                   inputId=input$tagsInp,
                                   value=new_tags)})
        message("Here at:",Sys.time())
    })

    obsrv_e(rv$m$conf$project,{
        ## Update shinyFiles roots when project path changes.
        shiny::req(rv$m$conf$project)
        dir <- normalizePath(rv$m$conf$project,winslash = '/')
        if (roots$get()[["project"]] != dir) {
            roots$set(c("start"= roots$get()[['project']] ,
                        "project" = dir))
            
        } else {
            roots$set(c("project" = dir))
        }
    })



    ## ***** Render *****
    output$fnKnownLCtrl <- shiny::renderUI({
        txt_file_input(inputId = 'known',
                       input = input,
                       label = html("The list of knowns. Required columns: <i>ID</i>, <i>SMILES</i>, <i>Name</i> and <i>RT</i> (the last two can be empty). Remember to quote <i>SMILES</i> and <i>Name</i> entries!"),
                       fileB = 'impKnownListB',
                       volumes=roots$get)
    })
    output$fnUnkLCtrl <- shiny::renderUI({
        txt_file_input(inputId = 'unknown',
                       input = input,
                       label = html("The list of unknowns. Required columns: <i>ID</i>, <i>mz</i> and <i>RT</i> (<i>RT</i> can be empty)."),
                       fileB = 'impUnkListB',
                       volumes=roots$get)
    })
    output$fnSetIdCtrl <- shiny::renderUI({
        txt_file_input(inputId = 'sets',
                       input = input,
                       label = html("Compounds set table. Required columns <i>ID</i> and <i>set</i>."),
                       fileB = 'impSetIdB',
                       volumes=roots$get)
    })

    output$mzMLtabCtrl <- rhandsontable::renderRHandsontable({
        assert(rv$m$input$tab$setid, msg = "Compounds set table not built yet.")
        tags <- unique(rf$get_tags_from_txt())
        mzml <- rv$m$input$tab$mzml
        message("mzml: ----")
        print(mzml)
        message("---- mzml")
        all_sets <- unique(rv$m$input$tab$setid$set)
        rhandsontable::rhandsontable(mzml2disp(mzml, sets = all_sets, tags = tags),stretchH="all")
    })
    
    rv
}


mzml2disp <- function(mzml,sets, tags) {
    ## Add factors for nicer rhandsontable output.
    df <- as.data.frame(mzml,stringsAsFactors=F)
    df$set <- factor(df$set,levels=sets)
    df$tag <- factor(df$tag,levels=tags)
    df$mode <- factor(df$mode,levels=names(MODEMAP))
    df
}

disp2mzml <- function(df) {
    df$set <- as.character(df$set)
    df$mode <- as.character(df$mode)
    df$tag <- as.character(df$tag)
    dtable(df)
}


txt2tags <- function(txt) {
    ## Turns a string into tags
    x <- if (shiny::isTruthy(txt)) {
             trimws(unlist(strsplit(txt, ",")))
         } else list()
    
    
    as.list(c(TAG_DEF,x))
}

combine_tags <- function(df_tags,txt_tags) {
    diff <- setdiff(df_tags,txt_tags)
    for (x in diff) df_tags[df_tags %in% x] <- TAG_DEF
    df_tags <- factor(as.character(df_tags))
    df_tags <- factor(as.character(df_tags),levels = unique(c(TAG_DEF,levels(df_tags),txt_tags)))
    df_tags
}
