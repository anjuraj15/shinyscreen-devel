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
                          shinyFiles::shinySaveButton(id="saveConfB",
                                                      label="Save project configuration.",
                                                      title="Save",
                                                      filename = FN_CONF,
                                                      "yaml"),
                          shinyFiles::shinyFilesButton(id="restoreConfB",
                                                       label="Restore project configuration.",
                                                       multiple=F,
                                                       title="Restore"),
                          shinyFiles::shinyDirButton(id="switchProjB",
                                                     label="Switch project.",
                                                     title="Switch project.",
                                                     icon=shiny::icon("recycle")),
                          shiny::actionButton(inputId="resetConfB",
                                              label="Reset config (CAUTION!)",
                                              icon=shiny::icon("trash")),
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
    ## Reactive functions.

    rf$gen_cmpd_inputs <- react_f({
        rv$m$conf$compounds$known
        rv$m$conf$compounds$unknown
        rv$m$conf$compounds$sets
        verify_compounds(rv$m$conf)
        load_compound_input(rv$m)
    })

    rf$get_tags_from_txt <- react_f({
        ## Tags in the text box.
        input$updTagsB
        isol(if (isTruthy(input$tagsInp)) unique(txt2tags(input$tagsInp)) else TAG_DEF)
    })

    rf$ctrl2mzml_df <- react_f({
        x <- tryCatch(rhandsontable::hot_to_r(input$mzMLtabCtrl),error=function(m) NULL)
        shiny::req(x)
        x
    })

    rf$ctrl2mzml <- react_f({
        dtable(rf$ctrl2mzml_df())
    })

    rf$get_all_sets <- react_e(rv$m$input$tab$setid,unique(rv$m$input$tab$setid$set))

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
                               roots=roots$get,
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
        rv$m$conf <- read_conf(fn)
        for (nm in names(rv$m$conf$compounds)) {
            shiny::updateTextInput(session=session,
                                   inputId=nm,
                                   value=rv$m$conf$compounds[[nm]])
        }

        fn <- rv$m$conf$data
        shiny::req(fn)
        rv$work_mzml_pre <- file2tab(fn)
        
    })

    obsrv_e(rv$work_mzml_pre,{
        ## update-files-on-restore
        assert(rv$m$input$tab$setid, msg = "Compounds set table not built yet.")
        all_sets <- rf$get_all_sets()
        dt <- rv$work_mzml_pre
        txt_tags <- rf$get_tags_from_txt()
        dt_tags <- unique(dt$tag)
        tags <- combine_tags(dt_tags,txt_tags)
        inp_tags <- setdiff(tags,TAG_DEF)
        
        shiny::updateTextInput(session = session,
                               inputId = "tagsInp",
                               value = inp_tags)
        rv$work_mzml <- mzml2disp(dt, sets = all_sets, tags = tags)
    })

    obsrv_e(input$mzMLB,{
        ## update-files-on-mzmlb
        df <- tryCatch(rhandsontable::hot_to_r(input$mzMLtabCtrl),error=function (e) NULL)
        shiny::req(df)
        assert(rv$m$input$tab$setid, msg = "Compounds set table not built yet.")
        fchoice<-shinyFiles::parseFilePaths(roots = roots$get,input$mzMLB)
        paths<-fchoice[["datapath"]]
        tags <- rf$get_tags_from_txt()
        all_sets <- unique(rv$m$input$tab$setid$set)
        
        df <- add_mzML_files(df,paths)
        rv$work_mzml <- df
        
    })

    obsrv_e(input$updTagsB,{
        message("update-tags:",Sys.time())
        df <- rf$ctrl2mzml_df()
        tags <- rf$get_tags_from_txt()
        z <- factor(as.character(df$tag), levels = tags)
        df$tag <- factor(sapply(as.character(z),function(x) if (!is.na(x)) x else TAG_DEF),levels = tags)

        rv$work_mzml <- df
    })

    obsrv({
        ## build-config
        message("build-config:",Sys.time())
        rv$m$conf$compounds$known <- input$known
        rv$m$conf$compounds$unknown <- input$unknown
        rv$m$conf$compounds$sets <- input$sets
        rv$m$conf$data <- file.path(rv$m$conf$project,FN_DATA_TAB)
        
        
    })

    obsrv({
        ## build-compounds
        message("build-compounds:",Sys.time())
        rv$m <- rf$gen_cmpd_inputs()
    })

    obsrv({
        ## update-data-table
        message("update-data-table:",Sys.time())
        mzml <- rf$ctrl2mzml()
        verify_data_df(mzml=mzml,all_sets=rf$get_all_sets())
        rv$m$input$tab$mzml <- mzml
    })

    obsrv_e(rv$m$conf$project,{
        ## update-roots
        message("update-roots:",Sys.time())
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
        df <- rv$work_mzml
        if (!shiny::isTruthy(df)) {
            assert(rv$m$input$tab$setid, msg = "Compounds set table not built yet.")
            all_sets <- unique(rv$m$input$tab$setid$set)
            txt_tags <- rf$get_tags_from_txt()
            df <- mzml2disp(EMPTY_MZML, sets = all_sets, tags = txt_tags)
        }
        rhandsontable::rhandsontable(df,stretchH="all")
    })
    
    rv
}


mzml2disp <- function(mzml,sets, tags) {
    ## Add factors for nicer rhandsontable output.
    df <- as.data.frame(mzml,stringsAsFactors=F)
    df$set <- factor(df$set,levels=sets)
    df$tag <- factor(df$tag,levels=tags)
    df$mode <- factor(df$mode,levels=names(DISP_MODEMAP))
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
    unique(c(TAG_DEF,df_tags,txt_tags))
}

add_mzML_files<-function(df,paths) {
    lSet<-levels(df$set)
    if (length(lSet > 0) && !is.na(lSet)) {
        
        nR<-length(paths)
        if (nR > 0) {
            st <- nrow(df)+1
            fi <- nrow(df)+nR
            df[st:fi,'tag'] <- levels(df$tag)[[1]]
            df[st:fi,'set'] <- levels(df$set)[[1]]
            df[st:fi,'mode'] <- levels(df$mode)[[1]]
            df[st:fi,'Files'] <- paths
        }
    } else {
        warning("Define sets using the compound set table before trying to add files!")
        
    }
    df
}
