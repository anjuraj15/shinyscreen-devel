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

mkUI <- function() {
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
                           shiny::textInput("fnTgtL",
                                            "Knowns list. Required columns: ID and SMILES. Optional column: Name. Remember to quote SMILES and Name entries!",
                                            value=""),
                           shiny::textInput("fnUnkL",
                                            "Unknows list. Required columns: ID and mz.",
                                            value=""),
                           shiny::textInput("fnSetId",
                                            "Set table.",
                                            value=""),
                           shinyFiles::shinyFilesButton("impTgtListB",
                                                        label="Import targets.",
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

    confmzMLTags <- prim_box(title="Sets and Tags",
                             shiny::textInput("tagPropInp",
                                              "What is a tag? (example: collision energy; can be left empty.)",
                                              value=TAG_DEF_DESC),
                             shiny::textInput("tagsInp",
                                              "Comma-delimited list of tag types.",
                                              value=""),
                             width=NULL)

    confState <- prim_box(title="Configuration State",
                          shinyFiles::shinySaveButton("saveConfB",
                                                      "Save configuration",
                                                      title="Save",
                                                      filename = "conf-state.rds",
                                                      "rds"),
                          shinyFiles::shinyFilesButton("restoreConfB",
                                                       label="Restore configuration",
                                                       multiple=F,
                                                       title="Restore"),
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

    ## ***** Compound List Tab *****

    cmpListBox<-prim_box(title="Compound List",
                         rhandsontable::rHandsontableOutput("tgtCtrl"),
                         width=NULL)
    
    cmpListLayout <- shiny::fluidRow(shiny::column(cmpListBox,
                                                   width = 12))

    cmpListTab <- shinydashboard::tabItem(tabName="compList",
                                          cmpListLayout)

    ## ***** Sets of compounds *****

    setIdBox<-prim_box(title="Compound Sets",
                       rhandsontable::rHandsontableOutput("setIdCtrl"),
                       width = NULL)

    setIdLayout<-shiny::fluidRow(shiny::column(setIdBox,
                                               width = 12))

    setIdTab<-shinydashboard::tabItem(tabName="setId",
                                      setIdLayout)

    genBoxExtract<-prim_box(title="Extract Spectra",
                            shiny::textInput("genNoProc",
                                             label="Number of processes.",
                                             value=1),
                            shiny::textInput("ppmLimFine",
                                             label="Precursor mz tolerance (relative [ppm]).",
                                             value=10),
                            shiny::textInput("eicLim",
                                             label="EIC mz tolerance (absolute).",
                                             value=1e-3),
                            shiny::textInput("rtDeltaWin",
                                             label="Retention time tolerance (minutes).",
                                             value=0.5),
                            shiny::uiOutput("genSetSelInpCtrl"),
                            shiny::actionButton(inputId="genRunB",
                                                label="Run!",
                                                icon=shiny::icon("bomb")),
                            width=NULL)
    
    genBoxAutoQA<-prim_box(title="Automatic Quality Control",
                           shiny::textInput("intThresh",
                                            label="Intensity threshold.",
                                            value=1e5),
                           shiny::textInput("noiseFac",
                                            label="Signal-to-noise ratio.",
                                            value=3),
                           shiny::textInput("rtDelta",
                                            label="Retention time shift tolerance (minutes).",
                                            value=0.5),
                           shiny::actionButton(inputId="genRunPPB",
                                               label="Preprocess!",
                                               icon=shiny::icon("bomb")),
                           width=NULL)
    
    genBoxProcessed<-prim_box(title="Processed Sets",
                              rhandsontable::rHandsontableOutput("genTabProcCtrl"),
                              width=NULL)
    
    genTab<-shinydashboard::tabItem(tabName = "gen",
                                    shiny::h2(GUI_TAB_TITLE[["gen"]]),
                                    shiny::fluidRow(shiny::column(genBoxExtract,
                                                                  width=4),
                                                    shiny::column(genBoxProcessed,
                                                                  genBoxAutoQA,width=4)))

    ## ***** Prescreening *****



    ## Prescreening elements
    presTitle <- shinydashboard::box(title = "MS Prescreening",
                                     width = NULL,
                                     height = "80px",
                                     background = "blue",
                                     "")
    presCompDsc <- shinydashboard::box(title = "Compound ID N°",
                                       width = NULL,
                                       height = "80px",
                                       background = "olive",
                                       shiny::textOutput("compoundID"))



    presPlotBox <- shinydashboard::box(title = "Plot",
                                       width = NULL,color = "olive",
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       shiny::plotOutput("chromGram",
                                                         width = "100%",
                                                         height = "750px",
                                                         click = NULL,
                                                         dblclick = NULL,
                                                         hover = NULL,
                                                         hoverDelay = NULL,
                                                         hoverDelayType = NULL,
                                                         brush = NULL,
                                                         clickId = NULL,
                                                         hoverId = NULL))

    presChromPropMS1<-shinydashboard::box(title="Chromatogram (MS1)",
                                       width=NULL,
                                       solidHeader = F,
                                       collapsible = F,
                                       shiny::numericInput("min_ms1_rt",
                                                           "Minimal RT",
                                                           DEFAULT_RT_RANGE[[1]]),
                                       shiny::numericInput("max_ms1_rt",
                                                           "Maximal RT",
                                                           DEFAULT_RT_RANGE[[2]]),
                                       shiny::numericInput("min_ms1_int",
                                                           "Minimal Intensity",
                                                           DEFAULT_INT_RANGE[[1]]),
                                       shiny::numericInput("max_ms1_int",
                                                           "Maximal Intensity",
                                                           DEFAULT_INT_RANGE[[2]]),
                                       shiny::radioButtons("int_ms1_axis",
                                                      "Intensity Scale",
                                                      c(linear = "linear",
                                                        log = "log")))
    
    presChromPropMS2<-shinydashboard::box(title="Chromatogram (MS2)",
                                       width=NULL,
                                       solidHeader = F,
                                       collapsible = F,
                                       shiny::numericInput("min_ms2_rt",
                                                           "Minimal RT",
                                                           DEFAULT_RT_RANGE[[1]]),
                                       shiny::numericInput("max_ms2_rt",
                                                           "Maximal RT",
                                                           DEFAULT_RT_RANGE[[2]]),
                                       shiny::numericInput("min_ms2_int",
                                                           "Minimal Intensity",
                                                           DEFAULT_INT_RANGE[[1]]),
                                       shiny::numericInput("max_ms2_int",
                                                           "Maximal Intensity",
                                                           DEFAULT_INT_RANGE[[2]]),
                                       shiny::radioButtons("int_ms2_axis",
                                                      "Intensity Scale",
                                                      c(linear = "linear",
                                                        log = "log")))

    presSpecPropMS2<-shinydashboard::box(title="MS2 Spectrum",
                                         width=NULL,
                                         solidHeader = F,
                                         collapsible = F,
                                         shiny::numericInput("min_ms2_mz",
                                                             "Minimal m/z",
                                                             DEFAULT_MZ_RANGE[[1]]),
                                         shiny::numericInput("max_ms2_mz",
                                                             "Maximal m/z",
                                                             DEFAULT_MZ_RANGE[[2]]),
                                         shiny::numericInput("min_ms2_sp_int",
                                                             "Minimal Intensity",
                                                             DEFAULT_INT_RANGE[[1]]),
                                         shiny::numericInput("max_ms2_sp_int",
                                                             "Maximal Intensity",
                                                             DEFAULT_INT_RANGE[[2]]),
                                         shiny::radioButtons("int_ms2_sp_axis",
                                                             "Intensity Scale",
                                                             c(linear = "linear",
                                                               log = "log")))
    

    presSaveBox<-shinydashboard::box(title = "Saving Plots",
                                     width = NULL,
                                     solidHeader = F,
                                     collapsible = F,
                                     shiny::textInput("plotname",
                                                      "Insert plot name: (e.g. plotname_%i.pdf)",
                                                      value="plotCpdID_%i.pdf"),
                                     shiny::actionButton("saveplot",
                                                         "Save",
                                                         icon = shiny::icon("save")),
                                     shiny::actionButton("saveallplots",
                                                         "Save All Plots",
                                                         icon = shiny::icon("save")))



    presCompSelBox <- shinydashboard::box(title = "Compounds",
                                          width=NULL,
                                          solidHeader = FALSE,
                                          color = "olive",
                                          collapsible = TRUE,
                                          "",
                                          shiny::br(),
                                          shiny::uiOutput("presSelSetCtrl"),
                                          shiny::uiOutput("presSelModeCtrl"),
                                          shiny::actionButton("presPrev",
                                                              "Previous compound.",
                                                              icon = shiny::icon("backward")),
                                          shiny::actionButton("presNext",
                                                              "Next compound.",
                                                              icon = shiny::icon("forward")),
                                          shiny::uiOutput("presSelCmpdCtrl"))

    presQABox <- shinydashboard::box(title = "Prescreening analysis",
                                     width = NULL,
                                     solidHeader = FALSE,
                                     collapsible = TRUE,
                                     shiny::uiOutput("nvPanel"),
                                     shiny::actionButton("submitQA",
                                                         "Submit",
                                                         icon = shiny::icon("save")),
                                     shiny::textInput("fn_ftable",
                                                      "File table Name",
                                                      value=FN_FTAB_DEF_OUT),
                                     shiny::actionButton("savefiletable",
                                                         "Save File Table",
                                                         icon = shiny::icon("save")))

    presTab <- shinydashboard::tabItem(tabName = "prescreen",
                                       shiny::h2(GUI_TAB_TITLE[["pres"]]),
                                       shiny::fluidRow(shiny::column(width=9,
                                                                     presTitle),
                                                       shiny::column(width=3,
                                                                     presCompDsc)),
                                       shiny::fluidRow(shiny::column(width=9,
                                                                     presPlotBox),
                                                       shiny::column(width=3,
                                                                     presCompSelBox,
                                                                     presQABox)),
                                       shiny::fluidRow(shiny::column(width=2,
                                                                     presChromPropMS1),
                                                       shiny::column(width=2,
                                                                     presChromPropMS2),
                                                       shiny::column(width=2,
                                                                     presSpecPropMS2),
                                                       shiny::column(width = 3,
                                                                     presSaveBox)))





    ## ***** Top-level Elements *****
    
    headerText <- "Shinyscreen"

    

    confSideItem <- shinydashboard::menuItem(text=GUI_SIDE_TITLE[["conf"]],
                                             tabName="config",
                                             icon=shiny::icon("user-cog"))
    
    compListSideItem <- shinydashboard::menuItem(text="Compound list",
                                                 tabName="compList",
                                                 icon=shiny::icon("table"))

    setIdSideItem <- shinydashboard::menuItem(text="Compound sets",
                                              tabName="setId",
                                              icon=shiny::icon("table"))

    genSideItem <- shinydashboard::menuItem(text=GUI_SIDE_TITLE[["gen"]],
                                            tabName="gen",
                                            icon=shiny::icon("cogs"))

    presSideItem <- shinydashboard::menuItem(text=GUI_SIDE_TITLE[["pres"]],
                                             tabName="prescreen",
                                             icon=shiny::icon("chart-bar"))
    
    header <- shinydashboard::dashboardHeader(title=headerText)
    
    sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(id='tabs',
                                                                            confSideItem,
                                                                            genSideItem,
                                                                            presSideItem,
                                                                            shiny::hr(),
                                                                            shiny::h5("Inputs"),
                                                                            compListSideItem,
                                                                            setIdSideItem))


    body <- shinydashboard::dashboardBody(shinydashboard::tabItems(confTab,
                                                                   cmpListTab,
                                                                   setIdTab,
                                                                   genTab,
                                                                   presTab))


    
    shinydashboard::dashboardPage(header,
                                  sidebar,
                                  body)}

##' @export
shinyScreenApp <- function(projDir=getwd()) {
    message("projDir=",projDir)
    modeLvl<- c("pH","pNa","pM",
                "mH","mFA")
    vols<-shinyFiles::getVolumes()
    volumes <- c(project=projDir,
                 home="~",
                 vols())

    mk_mzMLtab<-function() {
        modeLvl<- c("pH","pNa","pM",
                    "mH","mFA")
        res<-data.frame(Files=character(),
                        mode=factor(levels=modeLvl),
                        set=factor(),
                        tag=factor(),
                        stringsAsFactors=F)
        res
        
    }

    mk_cmpList<-function() {
        data.frame(ID=integer(),
                   Name=character(),
                   SMILES=character(),
                   RT=double(),
                   CAS=character(),
                   stringsAsFactors = F)
    }

    mk_setId<-function() {
        data.frame(ID=integer(),
                   set=character())}
    
    extd_mzMLtab<-function(ft,fn,sets,tags) {
        modeLvl<- c("select",names(MODEMAP))
        res<- if (is.null(ft)) {
                  data.frame(Files=fn,
                             mode=factor(modeLvl[[1]],levels=modeLvl),
                             set=factor(sets[[1]],levels=sets),
                             tag=factor(tags[[1]],levels=tags),
                             stringsAsFactors=F)
                  
              } else {
                  nR<-nrow(ft)
                  ft[nR+1,]<-c(Files=fn,
                               mode=modeLvl[[1]],
                               set=sets[[1]],
                               tag=tags[[1]])
                  ft
              }
        res
    }

    readSetId<-function(fn) {
        read.csv(file=fn,
                 header=T,
                 stringsAsFactors = T,
                 comment.char = '',
                 na.strings = c("","NA"))
    }

    getMz<-function(ids,cmpdL) {
        mz<-sapply(ids,function(i) {mzs<-cmpdL$mz[cmpdL$ID==i]
            if (length(mzs)>1) mzs[[1]] else mzs
        })
        names(mz)<-NULL
        mz
    }
    mk_cmpd_drop<-function(set,setTab) {
        wh<-which(setTab$set %in% set)
        ids<-setTab$ID[wh]
        mz<-setTab$mz[wh]
        entries<-base::Map(function(i,m,x) paste(i,'; ','mz: ',m,sep=''),ids,mz)
        entries
    }

    queryFileTable <- function(df,set,mode,id) {
        sdf<-df[df$set %in% set,]
        msdf<-sdf[sdf$mode %in% mode,]
        msdf[msdf$ID %in% id,]
    }


    updateFileTable <- function(df,set,mode,id,linput) {
        for (tag in names(linput)) {
            entries <- names(linput[[tag]])
            cond <- (df$ID %in% id) & (df$tag == tag) & (df$mode %in% mode) & (df$set %in% set)
            wh<-which(cond)

            df[wh,entries] <- linput[[tag]]
        }
        df
    }
    
    getCheckboxValues <- function(tag,input,rv) {
        chkbox <- input[[rv$spectProps[[tag]]]]
        q <- sapply(QANAMES,function (qn) {
            if (qn %in% chkbox) T else F
        })
        names(q) <- QANAMES
        q
    }

    getSetMode <- function(set,mzMLtab) {
        sdf<-mzMLtab[which(mzMLtab$set %in% set),]
        levels(factor(sdf$mode))
    }


    mk_mzML_work<-function() {
        df<-data.frame(Files=character(),
                       mode=factor(),
                       set=factor(),
                       tag=factor(),
                       stringsAsFactors=F)
        levels(df$mode)<-names(MODEMAP)
        df
    }

    prep_mzML_work <- function(df,sets,tags) {
        ## Keeps the dataframe behind the mzML control in shape.
        if (is.null(df)) df<-mk_mzML_work()

        if (length(tags)>0 && !is.na(tags)) {
            x<-as.character(df$tag)
            df$tag<-factor(x,levels=unlist(tags))
            ina<-which(is.na(df$tag))
            df$tag[ina]<-TAG_DEF
        }
        if (length(sets)>0 && !is.na(sets)) {
            y<-as.character(df$set)
            df$set<-factor(y,levels=sets)
        }
        df
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
    server <- function(input,output,session) {

        ## ***** reactive values *****
        rvConf <- shiny::reactiveValues(
                             allTags=list(),
                             tags=list(),
                             sets=list(),
                             spectProps=list(),
                             QANAMES=QANAMES,
                             MODEMAP=MODEMAP,
                             REST_TXT_INP=REST_TXT_INP,
                             fnLocSetId=FN_LOC_SETID,
                             fnFTBase=FN_FTAB_BASE,
                             fnFTPP=FN_FTAB_PP,
                             tagProp="",
                             setProp="",
                             fnComp=FN_COMP_TAB,
                             mode=modeLvl,
                             projDir=projDir,
                             currIDpos=1,
                             currIDSet=list(),
                             currID=NA,
                             fnFT=FN_FTAB_STATE)
        rvTab<-shiny::reactiveValues(
                          mzML=NULL, # files (File), sets (set) and mode (mode)
                          mzMLWork=NULL,
                          tgt=NULL, # ids(ID),SMILES(SMILES) and names (Name)
                          unk=NULL, # ids(ID),mz
                          setId=NULL, # ids(ID), sets (set)
                          comp=NULL,  # everything, except file info
                          mtr=NULL)     #master table (everything combined)
        rvPres<-shiny::reactiveValues(cex=CEX,
                                      rt_digits=RT_DIGITS,
                                      m_digits=M_DIGITS,
                                      pal=PAL,
                                      data=NULL,
                                      plot_id=NULL)

        ## ***** shinyFiles observers *****
        wdroot<-c(wd=projDir)
        shinyFiles::shinyFileChoose(input, 'impTgtListB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impUnkListB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impSetIdB',roots=volumes)
        
        shinyFiles::shinyFileSave(input, 'saveConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'restoreConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'mzMLB',roots=volumes)

        ## ***** reactive function definitions *****
        
        get_all_tags<-shiny::reactive({
            ## Returns all tags from the input box.
            tagsInp<-input$tagsInp
            x<-if (length(tagsInp)>0 && !is.na(tagsInp) && nchar(tagsInp)>0) unlist(strsplit(tagsInp, ",")) else list()
            as.list(c(x,"unspecified"))
        })

        get_all_sets<-shiny::reactive({
            ## Returns all sets defined in a setid table.
            df<-rvTab$setId
            sets<-levels(df$set)
            sets
        })

        get_sets<-shiny::reactive({
            ## Returns only the sets set for the mzML files. This
            ## takes precedense over the sets in setid table.
            mzml<-get_mzml()
            sets<-as.character(mzml$set)
            shiny::validate(need(sets,"Please assign sets to the mzML files."))
            ## res<-if (!is.null(sets)) {
            ##          if (!anyNA(sets)) levels(factor(sets)) else NULL
            ##      } else NULL
            ##      res
            levels(factor(sets))
        })

        getCmpL<-shiny::reactive({
            rvTab$tgt #rhandsontable::hot_to_r(input$tgtCtrl)
        })

        getSetId<-shiny::reactive({
            shiny::validate(need(rvTab$setId,"Compound set table not read!"))
            rvTab$setId #rhandsontable::hot_to_r(input$setIdCtrl)
        })

        getTgt<-shiny::reactive({
            rvTab$tgt
        })

        getUnk<-shiny::reactive({
            rvTab$unk
        })



        get_curr_set<-shiny::reactive({
            set<-input$presSelSet
            # sets<-get_sets()
            shiny::validate(need(set,"Initialising set selection control ..."))
            set
        })

        get_mset_comp_tab<-shiny::reactive({
            comp<-get_comp_tab()
            md<-get_curr_mode()
            set<-get_curr_set()
            scomp<-comp[comp$set %in% set,]
            mscomp<-scomp[scomp$mode %in% md,]
            mscomp
        })

        get_set_modes<-shiny::reactive({
            set<-get_curr_set()
            mzML<-get_mzml()
            levels(factor(mzML$mode[mzML$set %in% set]))
            
        })
        get_curr_mode<-shiny::reactive({
            md<-input$presSelMode
            shiny::validate(need(md,"Initialising mode selection control ..."))
            md
        })

        get_curr_set_ids<-shiny::reactive({
            comp<-get_comp_tab()
            set<-get_curr_set()
            md<-get_curr_mode()
            comp<-comp[comp$set %in% set,]
            comp<-comp[comp$mode %in% md,]
            comp$ID
        })

        get_curr_id_pos<-shiny::reactive({
            pos<-as.numeric(input$presSelCmpd)
            shiny::validate(need(pos,"Initialising compound selection control ..."))
            pos
        })

        get_curr_id<-shiny::reactive({
            pos<-get_curr_id_pos()
            ids<-get_curr_set_ids()
            ids[[pos]]
        })

        saveConf<-shiny::reactive({
            fn<-shinyFiles::parseSavePath(root=c(wd=rvConf$projDir),input$saveConfB)[["datapath"]]
            if ((! is.na(fn)) && length(fn)>0) {
                message("Saving config to",fn)
                sav<-list()
                sav<-list(rvConf=list(),
                          input=list())
                shiny::isolate(for (nm in names(rvConf)) {
                                   sav$rvConf[[nm]]<-rvConf[[nm]]
                               })

                for (nm in rvConf$REST_TXT_INP) {
                    sav$input[[nm]]<-input[[nm]]
                    
                }
                sav$tab<-list()
                for (nm in REST_TAB) {
                    df<-rvTab[[nm]]
                    sav$tab[[nm]]<-df
                }
                
                saveRDS(object=sav,file=fn)
                message("Saving config finished.")
            }
        })

        restoreConf<-shiny::reactive({
            input$restoreConfB
            fnobj<-shinyFiles::parseFilePaths(root=c(wd=rvConf$projDir),input$restoreConfB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn) && nchar(fn)>0) {
                message("Restoring config from",fn)
                sav<-readRDS(fn)

                for (nm in rvConf$REST_TXT_INP) {
                    shiny::updateTextInput(session=session,
                                           inputId=nm,
                                           value=sav$input[[nm]])
                }
                for (nm in REST_TAB) {
                    rvTab[[nm]]<-sav$tab[[nm]]
                }
            }
        })

        get_mzml_work<-shiny::reactive({
            tags<-get_all_tags()
            sets<-get_all_sets()
            prep_mzML_work(rvTab$mzMLWork,sets,tags)
        })

        get_mzml <- shiny::reactive({
            mzml<-get_mzml_work()
            chset<-as.character(mzml$set)
            shiny::validate(need(chset,"Sets not properly specified for the mzML files."))
            mzml$set<-factor(chset)
            mzml
        })
        
        gen_pres_set_menu<-shiny::reactive({
            ids<-get_curr_set_ids()
            set<-get_curr_set()
            smcomp<-get_mset_comp_tab()
            shiny::validate(need(ids,"Bad return from get_curr_set_ids."),
                            need(set,"Bad return from get_curr_set."),
                            need(smcomp,"Bad return from get_mset_comp_tab"))

            entries<-mk_cmpd_drop(set,smcomp)
            ch<-as.list(1:length(ids))
            names(ch)<-entries
            ch

        })
        
        gen_mset_plot_f<-shiny::reactive({
            set<-get_curr_set()
            md<-get_curr_mode()
            fTab<-get_mtr()
            
            comp<-get_mset_comp_tab()
            compIds<-comp[,"ID"]
            compSMILES<-comp[,"SMILES"]
            compMz<-comp[,"mz"]
            tags<-as.character(rvConf$tags)
            iSet<-which(set==fTab$set & md==fTab$mode)
            sfTab<-fTab[iSet,]
            sfTab$tag<-as.character(sfTab$tag)
            tags<-unique(sfTab$tag)

            ## Associate wd-s with tags.
            wdTag<- match(tags,sfTab$tag)
            wd<-sfTab$wd[wdTag]

            ## Preload the required data for a give set/mode combination.
            pData<-lapply(wd,function (w) readRDS(file.path(w,FN_SPEC)))
            names(pData)<-sfTab$tag[wdTag]


            preID<-compIds
            smiles<-compSMILES
            mz<-compMz
            names(smiles)<-id2name(preID)
            names(mz)<-id2name(preID)

            theme<-cowplot::theme_half_open
            plot_id <- function (id,
                                 prop,
                                 log=input$int_axis) {
                ni=id2name(id)
                mz=mz[[ni]]
                smile<-smiles[[ni]]

                ## Extract metadata for the required ID.
                idTab<-sfTab[sfTab$ID==id,]
                tags<-idTab$tag
                rtMS1<-idTab$rt
                rtMS2<-idTab$MS2rt
                rtMS2Ind<-idTab$iMS2rt

                names(rtMS1)<-tags
                names(rtMS2)<-tags
                names(rtMS2Ind)<-tags


                
                plot_id_msn(ni,data=pData,
                            rtMS1=rtMS1,
                            rtMS2=rtMS2,
                            rtMS2Ind=rtMS2Ind,
                            mass=mz,
                            smile=smile,
                            tags=tags,
                            prop=prop,
                            theme=theme,
                            cex=rvPres$cex,
                            pal=rvPres$pal,
                            rt_digits=rvPres$rt_digits,
                            m_digits=rvPres$m_digits,
                            fTab=sfTab)
            }

            plot_id
        })

        gen_base_ftab<-shiny::reactive({
            message("Generating basic file table in file ",rvConf$fnFTBase)
            mzML<-get_mzml_work()
            files<-adornmzMLTab(mzML,projDir=rvConf$projDir)
            comp<- get_comp_tab() 
            df<-genSuprFileTab(files,comp)
            df<-addCompColsToFileTbl(df,comp)
            df$mode<-as.character(df$mode)
            tab2file(tab=df,file=rvConf$fnFTBase)
            message("Done generating basic file table in file ",rvConf$fnFTBase)
            df
        })


        get_mtr<-shiny::reactive({
            fnFT<-rvConf$fnFT
            mtr<-rvTab$mtr
            if (!is.null(mtr)) {
                message("Grabbing existing mtr")
                mtr
            } else if (!file.exists(fnFT)) {
                message("Generating the first instance of the state file table")
                bdf <- gen_base_ftab()
                df<-gen_clean_state_ftab(bdf)
                tab2file(tab=df,file=fnFT)
                message("Done generating the first instance of the state file table.")
                rvTab$mtr<-df
                df
            } else {
                message("Reading in the state file table.")
                df<-file2tab(fnFT,colClasses=c("rt"="numeric",
                                               "MS2rt"="numeric",
                                               "iMS2rt"="numeric"))
                df
            }
        })

        get_comp_tab<-shiny::reactive({

            setId<-getSetId()
            mzML<-get_mzml()
            unk<-getUnk()
            tgt<-getTgt()
            message("Begin generation of comp table.")
            availSets<-get_sets()
            idTgt<-tgt$ID
            idUnk<-unk$ID
            if (is.null(availSets)) stop("Sets have not been (properly) set on the mzML files. Please check.")
            
            if (length(intersect(idTgt,idUnk))>0) stop("There must not be unknowns and targets with the same IDs.")
            setId$orig<-rep("",nrow(setId))
            lTgt<-setId$ID %in% idTgt
            lUnk<-setId$ID %in% idUnk
            iTgt<-which(lTgt)
            iUnk<-which(lUnk)
            setId[iTgt,"orig"]<-"known"
            setId[iUnk,"orig"]<-"unknown"
            rvTab$setId<-setId ## !!!


            
            ## knowns
            setIdTgt<-setId[setId$orig=="known",]
            sets<-levels(factor(setIdTgt$set))
            sets<-intersect(availSets,sets) #Make sure we only
                                        #consider sets defined
                                        #on the data files.
            nRow<-0
            for (s in sets) {
                sMode<-getSetMode(s,mzML)
                n<-length(sMode)
                nRow<-nRow+n*length(which(setIdTgt$set %in% s))
                
            }

            compTgt<-data.frame(
                ID=rep(0,nRow),
                mz=rep(0.0,nRow),
                rt=rep(NA,nRow),
                mode=rep("",nRow),
                set=rep("",nRow),
                orig=rep("known",nRow),
                Name=rep("",nRow),
                SMILES=rep("",nRow),
                stringsAsFactors=F)

            i<-1
            for (s in sets) {
                sMode<-getSetMode(s,mzML)
                for (m in sMode) {
                    for (id in setIdTgt[setIdTgt$set %in% s,"ID"]) {
                        compTgt[i,"ID"]<-id
                        compTgt[i,"mode"]<-m
                        compTgt[i,"set"]<-s
                        compTgt[i,"mz"]<-getMzFromCmpL(id,m,tgt)
                        sm<-getColFromCmpL(id,"SMILES",tgt)
                        nm<-getColFromCmpL(id,"Name",tgt)
                        rt<-getColFromCmpL(id,"rt",tgt)
                        compTgt[i,"SMILES"]<-sm
                        compTgt[i,"Name"]<-nm
                        compTgt[i,"rt"]<-rt
                        i<-i+1
                    }
                    
                }
            }

            message("Generation of comp table: knowns done.")
            ## unknows
            setIdUnk<-setId[setId$orig=="unknown",]
            sets<-levels(factor(setIdUnk$set))
            sets<-intersect(availSets,sets)
            
            nRow<-0
            for (s in sets) {
                sMode<-getSetMode(s,mzML)
                n<-length(sMode)
                if (n>1) stop("Set of unknowns ",s,"has more than one mode. Sets of unknowns cannot have more than one mode.")

                nRow<-nRow+length(which(setIdUnk$set %in% s))
                
            }

            compUnk<-data.frame(
                ID=rep(0,nRow),
                mz=rep(0.0,nRow),
                rt=rep(NA,nRow),
                mode=rep("",nRow),
                set=rep("",nRow),
                orig=rep("unknown",nRow),
                Name=rep("",nRow),
                SMILES=rep("",nRow),
                stringsAsFactors=F)

            i<-1
            for (s in sets) {
                m<-getSetMode(s,mzML)
                for (id in setIdUnk[setIdUnk$set %in% s,"ID"]) {
                    compUnk[i,"ID"]<-id
                    compUnk[i,"mode"]<-m
                    compUnk[i,"set"]<-s
                    compUnk[i,"mz"]<-getColFromCmpL(id,"mz",unk)
                    nm<-getColFromCmpL(id,"Name",unk)
                    rt<-getColFromCmpL(id,"rt",unk)
                    compUnk[i,"Name"]<-nm
                    compUnk[i,"rt"]<-rt
                    i<-i+1
                }
                
            }
            message("Generation of comp table: unknowns done.")
            df<-rbind(compTgt,compUnk,stringsAsFactors=F)
            tab2file(df,rvConf$fnComp)
            message("Generation of comp table finished.")
            df   

            

        })


        plotProps<-shiny::reactive({
            prop<-list(ms1=list(rtrng=c(input$min_ms1_rt,
                                                         input$max_ms1_rt),
                                            irng= c(input$min_ms1_int,
                                                         input$max_ms1_int),
                                            axis=input$int_ms1_axis),
                             ms2=list(rtrng=c(input$min_ms2_rt,
                                                         input$max_ms2_rt),
                                             irng= c(input$min_ms2_int,
                                                         input$max_ms2_int),
                                             axis=input$int_ms2_axis),
                             spec=list(mzrng=c(input$min_ms2_mz,
                                                            input$max_ms2_mz),
                                             irng=c(input$min_ms2_sp_int,
                                                         input$max_ms2_sp_int),
                                             axis=input$int_ms2_sp_axis))

            prop
        })
        ## ***** Observe Event *****

        shiny::observeEvent(input$saveConfB,{
            saveConf()
        })

        shiny::observeEvent(input$restoreConfB,{
            message("Restore event observed.")
            restoreConf()
            message("Restore event finished.")
        })
        
        shiny::observeEvent(input$impSetIdB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impSetIdB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="fnSetId",
                                       value=fn)
            }})

        shiny::observeEvent(input$impTgtListB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impTgtListB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="fnTgtL",
                                       value=fn)
            }})

        shiny::observeEvent(input$impUnkListB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impUnkListB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="fnUnkL",
                                       value=fn)
            }})

        shiny::observeEvent(input$mzMLtabCtrl,
        {
            df<-rhandsontable::hot_to_r(input$mzMLtabCtrl)
            rvTab$mzMLWork<-df
        })

        shiny::observeEvent(input$fnTgtL,
        {
            fn<-input$fnTgtL
            if (isThingFile(fn)) {
                message("Importing knowns/suspects from:",fn)
                df<-file2tab(file=fn)
                rvTab$tgt<-vald_comp_tab(df,fn,checkSMILES=T,checkNames=T)
                message("Done knowns/suspects from: ",fn)

            }
        })


        shiny::observeEvent(input$fnUnkL,
        {
            fn<-input$fnUnkL
            if (isThingFile(fn)) {
                message("Importing unknowns list from:",fn)
                df<-file2tab(file=fn)
                rvTab$unk<-vald_comp_tab(df,fn,checkSMILES=F,checkMz=T)
                message("Done importing unknowns list from: ",fn)

            }
        })

        shiny::observeEvent(input$fnSetId,
        {
            fn<-input$fnSetId
            if (isThingFile(fn)) {
                message("Importing compound sets from:",fn)
                rvTab$setId<-file2tab(file=fn,
                                      colClasses=c(set="factor"))
                rvTab$setId$set<-factor(rvTab$setId$set)
                message("Done importing compound sets from: ",fn)
            }
        })

        shiny::observeEvent(input$mzMLB,
        {
            fchoice<-shinyFiles::parseFilePaths(root=volumes,input$mzMLB)
            paths<-fchoice[["datapath"]]
            rvTab$mzMLWork<-add_mzML_files(rvTab$mzMLWork,paths)
        })
        
        shiny::observeEvent(input$genRunB,{
            fTab<-gen_base_ftab()
            nProc<-as.integer(input$genNoProc)
            sets<-input$genSetSelInp
            message("Selected sets:")
            message("Number of processes:",nProc)
            intThresh<-as.numeric(input$intThresh)
            noiseFac<-as.numeric(input$noiseFac)
            rtDelta<-as.numeric(input$rtDelta)
            limFinePPM<-as.numeric(input$ppmLimFine)
            limEIC<-as.numeric(input$eicLim)
            rtDelta<-as.numeric(input$rtDeltaWin)
            for (s in sets) {
                message("***** BEGIN set ",s, " *****")
                ## fnCmpdList<-input$fnTgtL
                
                dest<-rvConf$projDir
                gc()
                dir.create(s,showWarnings=F)
                unsetGenDone(s)
                gen(fTab=fTab[fTab$set==s,],
                    proc=nProc,
                    limFinePPM=limFinePPM,
                    limEIC=limEIC,
                    rtDelta=rtDelta)
                setGenDone(s)
                message("***** END set ",s, " *****")
            }
            gc()
        })

        shiny::observeEvent(input$genRunPPB,{
            message("Starting preprocessing.")

            sets<-get_sets()
            comp<-get_comp_tab()
            
            nr<-nrow(comp)
            if (!is.null(nr)) {
                if (is.na(nr)) nr<-0
            } else nr<-0
            if (length(sets)>0 && nr>0) {
                doneSets<-sets[sapply(sets,is_gen_done)]
                message("done sets: ",doneSets)
                if (length(doneSets)>0) {
                                        #fnFullTab<-rvConf$fnFTPP
                    fullFTab<- get_mtr() #gen_base_ftab()
                    doneFTab<-fullFTab[fullFTab$set %in% doneSets,]
                    if (nrow(doneFTab) > 0) {

                        intThresh<-as.numeric(input$intThresh)
                        noiseFac<-as.numeric(input$noiseFac)
                        rtDelta<-as.numeric(input$rtDelta)
                        ppFTab<-preProc(ftable=doneFTab,
                                        intThresh=intThresh,
                                        noiseFac=noiseFac,
                                        rtDelta=rtDelta)

                        extNms<-names(ppFTab)
                        basNms<-names(fullFTab)
                        diffNms<-setdiff(extNms,basNms)
                        nrf<-nrow(fullFTab)
                        for (nm in diffNms) {
                            z<-logical(length=nrf)
                            z<-T
                            fullFTab[[nm]]<-z
                        }
                        fullFTab[fullFTab$set %in% doneSets,]<-ppFTab
                        write.csv(file=rvConf$fnFT,
                                  x=fullFTab,
                                  row.names=F)
                        
                        rvTab$mtr<-fullFTab
                        message("Finished preprocessing.")
                        
                        
                    }
                }
            }
        })

        shiny::observeEvent(input$presSelCmpd,{
            pos<-input$presSelCmpd
            rvConf$currIDpos<-as.numeric(pos)
        })

        shiny::observeEvent(input$presPrev,{
            x<-rvConf$currIDpos-1
            if (x>0) rvConf$currIDpos<-x
        })

        shiny::observeEvent(input$presNext,{
            len<-length(get_curr_set_ids())
            x<-rvConf$currIDpos+1
            if (x<=len) rvConf$currIDpos<-x
        })

        shiny::observeEvent(input$submitQA,{

            res <- lapply(rvConf$tags,getCheckboxValues,input,rvConf)
            names(res) <- rvConf$tags
            df<-get_mtr()
            rvTab$mtr <- updateFileTable(df=df,
                                         set=get_curr_set(),
                                         mode=get_curr_mode(),
                                         id=get_curr_id(),
                                         linput=res)
            tab2file(tab=rvTab$mtr,file=rvConf$fnFT)

        })
        
        shiny::observeEvent(input$savefiletable,
        {
            fn<-input$fn_ftable
            message("Writing current file table to ",fn)
            write.csv(file=fn,x=rvTab$mtr,row.names = F)
        })

        shiny::observeEvent(input$saveplot,
        {
            i=get_curr_id()
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotCpdID_%i.pdf"
            fn <- sprintf(pfn,i)
            rtrange <- c(input$min_rt,input$max_rt)
            pdf(file=fn, width=12, height=8)
            #rvPres$plot_id(i,rtrange=rtrange, log=input$int_axis)
            dev.off()
            message("Plotting compound ", i," to ",fn," done.")
        })

        shiny::observeEvent(input$saveallplots,
        {
            i=get_curr_id()
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotall.pdf"
            fn <- sprintf(pfn,i)
            pdf(file=fn, width=12, height=8)
            ids<-get_curr_set_ids()
            plot_id<-gen_mset_plot_f()
            for (id in ids) {
                #print(plot_id(id,log=input$int_axis))
                message("Plotting compound ", id," done.")
            }
            dev.off()
        })
        
        ## ***** Observe *****


        shiny::observe({
            if (input$tabs=="prescreen") {
                id<-get_curr_id()
                spectProps<-rvConf$spectProps
                set<-get_curr_set()
                md<-get_curr_mode()
                mtr<-get_mtr()
                if (!is.na(id) && length(spectProps)>0 && !is.na(md)) {
                    QANAMES<-rvConf$QANAMES
                    sdf <- queryFileTable(df=mtr,set=set,mode=md,id=id)
                    sdf$tag<-as.character(sdf$tag)
                    for (t in sdf$tag) {
                        sprop <- rvConf$spectProps[[t]]
                        sdfSel<-sdf[sdf$tag %in% t,QANAMES]
                        sel <- as.logical(sdfSel)
                        
                        choices <- QANAMES[sel]
                        names(choices) <- QANAMES[sel]
                        shiny::updateCheckboxGroupInput(session = session,inputId = sprop,selected=choices)
                    }
                    
                }
                
                
                
            }
        })




        ## ***** Render *****
        output$tgtCtrl <- rhandsontable::renderRHandsontable({
            df<-rvTab$tgt
            rhandsontable::rhandsontable(df,stretchH="all")
        })

        output$setIdCtrl<- rhandsontable::renderRHandsontable({
            df<-rvTab$setId
            rhandsontable::rhandsontable(df,stretchH="all")
        })

        output$mzMLtabCtrl <- rhandsontable::renderRHandsontable({
            df<-get_mzml_work()
            rhandsontable::rhandsontable(df,stretchH="all")
        })


        output$genTabProcCtrl<-rhandsontable::renderRHandsontable({
            shiny::invalidateLater(100,
                                   session=session)
            sets<-get_sets()
            genState<-sapply(sets,is_gen_done)
            df<-if (!is.null(sets)) {data.frame(set=sets,
                                                generated=genState,
                                                stringsAsFactors=F)
                } else {data.frame(sets=character(),
                                   generated=logical(),
                                   stringsAsFactors=F)} 
            rhandsontable::rhandsontable(df,
                                         rowHeaders=NULL,
                                         readOnly=F,
                                         stretchH="all")
            
        })
        
        output$genSetSelInpCtrl<-shiny::renderUI({
            sets<-get_sets()
            shiny::selectInput("genSetSelInp",
                               label="Select set(s).",
                               choices=sets,
                               multiple=T)
        })

        output$presSelSetCtrl<-shiny::renderUI({
            sets<-get_sets()
            shiny::selectInput("presSelSet",
                               "Set",
                               choices=sets,
                               selected=sets[[1]],
                               multiple=F)
        })
        
        output$presSelModeCtrl<-shiny::renderUI({
            mds<-get_set_modes()
            shiny::selectInput("presSelMode",
                               "Mode",
                               choices=mds,
                               selected = mds[[1]],
                               multiple=F)
        })


        output$presSelCmpdCtrl <- shiny::renderUI({
            choices<-gen_pres_set_menu()
            x<-rvConf$currIDpos
            shiny::selectInput("presSelCmpd",
                               "Compound",
                               choices = choices,
                               selected = x,
                               multiple = F)
        })
        
        output$nvPanel<-shiny::renderUI({
            message("Rendering panel started")
            ft<- get_mtr() 
            set<-input$presSelSet
            if (!is.null(set) && !is.na(set) && nchar(set)>0 && !is.null(ft)) {
                QANms<-rvConf$QANAMES
                names(QANms)<-QANms
                ftSec<-ft[ft$set %in% set,]
                tags<-levels(factor(as.character(ftSec$tag)))
                rvConf$tags<-tags
                spectProps<-sapply(tags,function (tag) paste("spectProps",tag,sep=""))
                rvConf$spectProps<-spectProps
                tabPanelList <- lapply(tags, function(tag) {
                    shiny::tabPanel(tag, shiny::checkboxGroupInput(spectProps[[tag]], "Quality Control",
                                                                   QANms),
                                    shiny::textAreaInput(paste("caption",tag,sep=""), "Comments:", "Insert your comment here..."),
                                    shiny::verbatimTextOutput(paste("value",tag,sep=""))
                                    )})
                
                
                message("done rendering panel")
                do.call(shiny::navlistPanel, tabPanelList)
            } else NULL
        })

        output$chromGram <- renderPlot(
        {

            plot_id<-gen_mset_plot_f()
            shiny::validate(need(plot_id,"Initialising the plotting function ..."))
            id=get_curr_id()
            prop<-plotProps()
            plot_id(id=id,prop=prop)
        })

        
        session$onSessionEnded(function () stopApp())
    }

    shiny::shinyApp(ui=mkUI(),server=server)
}
