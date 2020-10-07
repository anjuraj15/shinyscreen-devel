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
                           shiny::includeMarkdown(system.file("ui/compounds-list-button.md",package="shinyscreen")),
                           shinyFiles::shinyFilesButton("impCmpdsListB",
                                                        label="Import compound lists.",
                                                        title="",
                                                        icon=shiny::icon("file"),
                                                        multiple=T),
                           shiny::includeMarkdown(system.file("ui/compound-sets-button.md",package="shinyscreen")),
                           shinyFiles::shinyFilesButton("impSetIdB",
                                                        label="Import set ID table.",
                                                        title="",
                                                        icon=shiny::icon("file"),
                                                        multiple=T),
                           shiny::uiOutput("fnDataFilesCtrl"),
                           shinyFiles::shinyFilesButton("impDataFilesB",
                                                        label="Import data files table.",
                                                        title="",
                                                        icon=shiny::icon("file"),
                                                        multiple=F),
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


    consumm <- shinydashboard::tabItem(tabName="config",
                                       shiny::h2(GUI_TAB_TITLE[["conf"]]),
                                       confLayout)

    confSideItem <- shinydashboard::menuItem(text=GUI_SIDE_TITLE[["conf"]],
                                             tabName="config",
                                             icon=shiny::icon("user-cog"))

    return(list(tab=consumm,
                side=confSideItem))
}

react_conf_f <- function(input,output,session,rv,rf) {
    ## Reactive functions.



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

    rf$get_all_sets <- react_f({
        m <- rf$m_input_cmpds()
        unique(m$input$tab$setid$set)})

    rf$m_conf <- react_f({
        m <- list()
        m$conf$project <- rv$project_path
        m$conf$compounds$cmpds <- input$cmpds #TODO multi-lists.
        m$conf$compounds$sets <- input$sets
        m$conf$data <- input$datafiles
        verify_compounds(m$conf)
        m
    })

    rf$m_input_cmpds <- react_f({
        m <- rf$m_conf()
        load_compound_input(m)
    })

    rf$m_input <- react_f({
        m <- rf$m_input_cmpds()
        mzml <- rf$ctrl2mzml()
        verify_data_df(mzml=mzml,all_sets=rf$get_all_sets())
        m$input$tab$mzml <- mzml
        m
    })

    

    rf$m <- react_f(rf$m_input())
    
    rf
}

server_conf <- function(input,output,session,rv,rf,roots) {
    
    ## ***** shinyFiles observers *****



    shinyFiles::shinyFileChoose(input, 'impCmpdsListB',defaultRoot=roots$def_vol(),
                                defaultPath=roots$def_path(),roots=roots$get)
    shinyFiles::shinyFileChoose(input, 'impSetIdB',defaultRoot=roots$def_vol(),
                                defaultPath=roots$def_path(),roots=roots$get)
    shinyFiles::shinyFileChoose(input, 'impDataFilesB',defaultRoot=roots$def_vol(),
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
            rv$project_path <- path
        }
    })

    obsrv_e(input$saveConfB, {
        m <- rf$m()
        fn <- shinyFiles::parseSavePath(roots=roots$get,input$saveConfB)[["datapath"]]
        validate1(fn,msg="Invalid file to save config to.")
        write_state(m,fn)
    })

    obsrv_e(input$restoreConfB,{
        fn <- shinyFiles::parseFilePaths(roots=roots$get,input$restoreConfB)[["datapath"]]
        assert(file.exists(fn), msg="The file is unreadable.")
        conf <- read_conf(fn)
        rv$project_path <- conf$project
        for (nm in names(conf$compounds)) {
            shiny::updateTextInput(session=session,
                                   inputId=nm,
                                   value=conf$compounds[[nm]])
        }

        shiny::updateTextInput(session = session,
                               inputId = "datafiles",
                               value = conf$data)

        fn <- conf$data
        assert(fn,msg = "Bad data file table path.")
        rv$work_mzml_pre <- file2tab(fn)
        
    })

    obsrv({
        ## update-files-on-restore
        message("update-files-on-restore")
        rv$work_mzml_pre
        m <- rf$m_input_cmpds()
        if (shiny::isTruthy(m$input$tab$setid)) {
            isol({
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
        } else assert(m$input$tab$setid, msg = "Compounds set table not built yet.")
    })

    obsrv_e(input$mzMLB,{
        ## update-files-on-mzmlb
        m <- rf$m_input()
        df <- tryCatch(rhandsontable::hot_to_r(input$mzMLtabCtrl),error=function (e) NULL)
        shiny::req(df)
        fchoice<-shinyFiles::parseFilePaths(roots = roots$get,input$mzMLB)
        paths<-fchoice[["datapath"]]
        tags <- rf$get_tags_from_txt()
        
        
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

    obsrv_e(rv$project_path,{
        ## update-roots
        message("update-roots:",Sys.time())
        shiny::req(rv$project_path)
        dir <- normalizePath(rv$project_path,winslash = '/')
        if (roots$get()[["project"]] != dir) {
            roots$set(c("start"= roots$get()[['project']] ,
                        "project" = dir))
            
        } else {
            roots$set(c("project" = dir))
        }
        
    })

    ## ***** Render *****

    ##     txt_file_input(inputId = 'cmpds',
    ##                    input = input,
    ##                    label = html("The list of cmpds. Required columns: <i>ID</i>, <i>SMILES</i>, <i>Name</i> and <i>RT</i> (the last two can be empty). Remember to quote <i>SMILES</i> and <i>Name</i> entries!"),
    ##                    fileB = 'impCmpdsListB',
    ##                    volumes=roots$get) #TODO multi-lists
    ## })
    output$fnSetIdCtrl <- shiny::renderUI({
        txt_file_input(inputId = 'sets',
                       input = input,
                       label = html("Compounds set table. Required columns <i>ID</i> and <i>set</i>."),
                       fileB = 'impSetIdB',
                       volumes=roots$get)
    })
    output$fnDataFilesCtrl <- shiny::renderUI({
        m <- rf$m_conf()
        txt_file_input(inputId = 'datafiles',
                       input = input,
                       label = html("Data files table. Required columns <i>Files</i>, <i>tag</i>, <i>set</i> and <i>adduct</i>."),
                       fileB = 'impDataFilesB',
                       volumes=roots$get,
                       default = file.path(m$conf$project, FN_DATA_TAB))
    })

    output$mzMLtabCtrl <- rhandsontable::renderRHandsontable({
        df <- rv$work_mzml
        m <- rf$m_input_cmpds()
        if (!shiny::isTruthy(df)) {
            all_sets <- rf$get_all_sets()
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
    df$set <- factor(as.character(df$set),levels=c(SET_NA,sets))
    df$set <- factor(sapply(as.character(df$set),function (x) if (!is.na(x)) x else SET_NA),levels = c(SET_NA,sets))
    df$tag <- factor(df$tag,levels=tags)
    df$adduct <- factor(df$adduct,levels=names(DISP_ADDUCTMAP))
    df
}

disp2mzml <- function(df) {
    df$set <- as.character(df$set)
    df$adduct <- as.character(df$adduct)
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
            df[st:fi,'adduct'] <- levels(df$adduct)[[1]]
            df[st:fi,'Files'] <- paths
        }
    } else {
        warning("Define sets using the compound set table before trying to add files!")
        
    }
    df
}
