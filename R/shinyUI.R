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
                           shiny::textInput("fnKnownL",
                                            "Knowns list. Required columns: ID and SMILES. Optional column: Name. Remember to quote SMILES and Name entries!",
                                            value=""),
                           shiny::textInput("fnUnkL",
                                            "Unknows list. Required columns: ID and mz.",
                                            value=""),
                           shiny::textInput("fnSetId",
                                            "Set table.",
                                            value=""),
                           shinyFiles::shinyFilesButton("impKnownListB",
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

    knownListBox<-prim_box(title="Known Compounds List",
                         rhandsontable::rHandsontableOutput("knownCtrl"),
                         width=NULL)

    unkListBox<-prim_box(title="Unknown Compounds List",
                         rhandsontable::rHandsontableOutput("unkCtrl"),
                         width=NULL)
    
    cmpListLayout <- shiny::fluidRow(shiny::column(knownListBox,
                                                   unkListBox,
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
                           shiny::actionButton(inputId="qaAutoB",
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
                                                                            setIdSideItem
                                                                            ))


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
        entries<-base::Map(function(i,m) paste(i,'; ','mz: ',m,sep=''),ids,mz)
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
        levels(factor(as.character(sdf$mode)))
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
            oldlvl<-levels(df$tag)
            exttag<-unique(c(unlist(tags),oldlvl))
            x<-as.character(df$tag)
            df$tag<-factor(x,levels=exttag)
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

    extr_data_set<-function(df,set) {
        i<-match(set,df$set)
        df[i,'extracted']<-T
        df
    }

    extr_data_ms2_todo <- function(df){
        todo<-which(df$extracted & !df$ms2)
        message("todo:")
        str(todo)
        message("todo.")
        todo
    }

    extr_data_done<-function(df) {
        ind<-which(df$extracted)
        df$set[ind]
    }
    server <- function(input,output,session) {

        ## ***** reactive values *****
        rvConf <- shiny::reactiveValues(
                             QANAMES=QANAMES,
                             MODEMAP=MODEMAP,
                             REST_TXT_INP=REST_TXT_INP,
                             fnLocSetId=FN_LOC_SETID,
                             fnFTBase=FN_FTAB_BASE,
                             fnFTPP=FN_FTAB_PP,
                             fnComp=FN_COMP_TAB,
                             mode=modeLvl,
                             projDir=projDir,
                             currIDpos=1,
                             fnFT=FN_FTAB_STATE)
        rvTab<-shiny::reactiveValues(
                          mzml=NULL,    # mzML file table
                          mtr=NULL)     # master table (everything combined)
        rvPres<-shiny::reactiveValues(cex=CEX,
                                      rt_digits=RT_DIGITS,
                                      m_digits=M_DIGITS,
                                      pal=PAL,
                                      data=NULL,
                                      plot_id=NULL)

        ## ***** shinyFiles observers *****
        wdroot<-c(wd=projDir)
        shinyFiles::shinyFileChoose(input, 'impKnownListB',roots=volumes)
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
            df<-get_setid_file()
            sets<-levels(factor(df$set))
            sets
        })

        get_sets<-shiny::reactive({
            ## Returns only the sets set for the mzML files. This
            ## takes precedense over the sets in setid table.
            mzml<-get_mzml()
            shiny::validate(need(mzml,"Please add the mzML files."))
            sets<-as.character(mzml$set)
            
            levels(factor(sets))
        })

        get_curr_tags<-shiny::reactive({
            ## Tags in the currently selected set.
            ft<- get_mtr() 
            set<-input$presSelSet
            ftSec<-ft[ft$set %in% set,]
            tags<-levels(factor(as.character(ftSec$tag)))
            tags
        })

        get_known_setid<-shiny::reactive({
            setid<-get_setid_tab()
            df<-setid[setid$orig=="known",]
            df$set<-factor(as.character(df$set))
            df
        })

        get_unknown_setid<-shiny::reactive({
            setid<-get_setid_tab()
            df<-setid[setid$orig=="unknown",]
            df$set<-factor(as.character(df$set))
            df
        })
        
        get_known_sets<-shiny::reactive({
            fsets<-get_sets()
            ktab<-get_known_setid()
            intersect(levels(ktab$set),fsets)
        })


        get_unknown_sets<-shiny::reactive({
            fsets<-get_sets()
            utab<-get_unknown_setid()
            usets<-levels(utab$set)
            intersect(usets,fsets)
        })

        get_setid_file<-shiny::reactive({
            fn<-input$fnSetId
            shiny::validate(need(fn,"Please set the compounds set CSV filename."),
                            need(isThingFile(fn),"Cannot find the set CSV file."))
            
            message("Importing compound sets from:",fn)
            df<-file2tab(file=fn,
                         colClasses=c(set="factor"))
            df$set<-factor(df$set)
            message("Done importing compound sets from: ",fn)
            df
        })

        get_setid_tab<-shiny::reactive({
            setId<-get_setid_file()
            unk<-get_unk()
            known<-get_known()
            idKnown<-known$ID
            idUnk<-unk$ID

            no_id_clash<-length(intersect(idKnown,idUnk))==0
            shiny::validate(need(no_id_clash,"IDs of known and unknown compounds must be different. Change this in your compound lists and compounds set input CSV tables."))
            
            setId$orig<-rep("",nrow(setId))
            lKnown<-setId$ID %in% idKnown
            lUnk<-setId$ID %in% idUnk
            iKnown<-which(lKnown)
            iUnk<-which(lUnk)
            setId[iKnown,"orig"]<-"known"
            setId[iUnk,"orig"]<-"unknown"
            setId
        })

        get_known<-shiny::reactive({
            fn<-input$fnKnownL
            res<- if (isThingFile(fn)) {
                      message("Importing knowns/suspects from:",fn)
                      df<-file2tab(file=fn)
                      x <-vald_comp_tab(df,fn,checkSMILES=T,checkNames=T)
                      message("Done knowns/suspects from: ",fn)
                      x

                  } else NULL
            res
        })

        get_unk<-shiny::reactive({
            fn<-input$fnUnkL
            if (isThingFile(fn)) {
                message("Importing unknowns list from:",fn)
                df<-file2tab(file=fn)
                res<-vald_comp_tab(df,fn,checkSMILES=F,checkMz=T)
                message("Done importing unknowns list from: ",fn)
                res

            } else NULL
        })



        get_curr_set<-shiny::reactive({
            set<-input$presSelSet
            shiny::validate(need(set,"Initialising set selection control ..."))
            set
        })

        get_mset_comp_tab<-shiny::reactive({
            set<-get_curr_set()
            md<-get_curr_mode()
            comp<-get_comp_tab()
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
            modes<-get_set_modes()
            shiny::req(md)
            shiny::req(md %in% modes)
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
            lids<-length(get_curr_set_ids())
            shiny::req(pos <= lids)
            ## shiny::validate(need(pos,"Initialising compound selection control ..."))
            ## pos
            ## rvConf$currIDpos
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
            prep_mzML_work(rvTab$mzml,sets,tags)
        })

        get_mzml <- shiny::reactive({
            mzml<-get_mzml_work()
            chset<-as.character(mzml$set)
            shiny::validate(need(chset,"Sets not properly specified for the mzML files."))
            mzml$set<-factor(chset)
            sets<-levels(mzml$set)
            for (s in sets) {
                smzml<-mzml[mzml$set %in% s,]
                modes<-smzml$mode
                for (m in modes) {
                    msmzml<-smzml[smzml$mode %in% m,]
                    tags<-as.character(msmzml$tag)
                    shiny::validate(need(length(tags)==length(unique(tags)),paste("Tags for a single mode in a set `",s,"' must be unique. Please change!",sep='')))
                }
            }
            tag<-as.character(mzml$tag)
            mzml$tag<-factor(tag)
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
            tags<-get_curr_tags()
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




        extr_data_qa <- shiny::reactive({
            dfProc<-extr_data_mzml()
            shiny::validate(need(input$qaAutoB,"Please submit QA parameters by clicking `Preprocess'."))
            
            shiny::isolate({
                message("Starting preprocessing.")
                intThresh<-as.numeric(input$intThresh)
                noiseFac<-as.numeric(input$noiseFac)
                rtDelta<-as.numeric(input$rtDelta)

                sets<-get_sets()
                comp<-get_comp_tab()
                dsets<-extr_data_done(dfProc)
                
                message("done sets: ",dsets)
                if (length(dsets)>0) {
                    mtr<- get_mtr() 
                    mtrDone<-mtr[mtr$set %in% dsets,]

                    mtrPP<-preProc(ftable=mtrDone,
                                   intThresh=intThresh,
                                   noiseFac=noiseFac,
                                   rtDelta=rtDelta)

                    ## In case preProc added more names to the
                    ## table.
                    extNms<-names(mtrPP)
                    basNms<-names(mtr)
                    diffNms<-setdiff(extNms,basNms)
                    nrf<-nrow(mtr)
                    for (nm in diffNms) {
                        z<-logical(length=nrf)
                        z<-T
                        mtr[[nm]]<-z
                    }
                    
                    mtr[mtr$set %in% dsets,]<-mtrPP
                    tab2file(tab=mtr,file=rvConf$fnFT)

                    rvTab$mtr<-mtr
                    message("Finished preprocessing.")
                    idx<-which(dfProc$set %in% dsets)
                    dfProc$qa[idx]<-T
                    
                } else message("Nothing to preprocess")
            })
            dfProc
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

            setId<-get_setid_tab()
            mzML<-get_mzml()
            unk<-get_unk()
            known<-get_known()
            shiny::validate(need(!(is.null(unk) && is.null(known)),"No compound lists have been provided. At least one of the known, or unknown compound lists is required."))
            message("Begin generation of comp table.")
            idKnown<-known$ID
            idUnk<-unk$ID
            ## knowns
            setIdKnown<-get_known_setid()
            sets<-get_known_sets()
            nRow<-0
            for (s in sets) {
                sMode<-getSetMode(s,mzML)
                n<-length(sMode)
                nRow<-nRow+n*length(which(setIdKnown$set %in% s))
                
            }
            compKnown<-data.frame(
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
                    for (id in setIdKnown[setIdKnown$set %in% s,"ID"]) {
                        compKnown[i,"ID"]<-id
                        compKnown[i,"mode"]<-m
                        compKnown[i,"set"]<-s
                        compKnown[i,"mz"]<-get_mz_cmp_l(id,m,known)
                        sm<-get_col_from_cmp_l(id,"SMILES",known)
                        nm<-get_col_from_cmp_l(id,"Name",known)
                        rt<-get_col_from_cmp_l(id,"rt",known)
                        compKnown[i,"SMILES"]<-sm
                        compKnown[i,"Name"]<-nm
                        compKnown[i,"rt"]<-rt
                        i<-i+1
                    }
                    
                }
            }

            message("Generation of comp table: knowns done.")
            ## unknows
            setIdUnk<-get_unknown_setid()
            sets<-get_unknown_sets()
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
                    compUnk[i,"mz"]<-get_col_from_cmp_l(id,"mz",unk)
                    nm<-get_col_from_cmp_l(id,"Name",unk)
                    rt<-get_col_from_cmp_l(id,"rt",unk)
                    compUnk[i,"Name"]<-nm
                    compUnk[i,"rt"]<-rt
                    i<-i+1
                }
            }
            message("Generation of comp table: unknowns done.")
            df<-rbind(compKnown,compUnk,stringsAsFactors=F)
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

        extr_data_scan <- shiny::reactive({
            ## Which sets are done.
            sets<-get_sets()
            sapply(sets,is_gen_done)
        })

        extr_ms2_scan <- shiny::reactive({
            sets<-get_sets()
            sapply(sets,is_ms2_done)
        })

        extr_data_prep <- shiny::reactive({
            sets<-get_sets()
            L<-length(sets)
            data.frame(set=sets,extracted=rep(F,L),
                       qa=rep(F,L),
                       ms2=rep(F,L),stringsAsFactors = F)
        })


        extr_data_curr<- shiny::reactive({
            ## Set up the status data frame.
            message("Detecting processed data.")
            df<-extr_data_prep()
            dsets<-extr_data_scan()
            dms2<-extr_ms2_scan()
            df$extracted<-dsets
            df$ms2<-dms2
            message("Detecting processed data finished.")
            df
        })


        extr_data_mzml<-shiny::reactive({
            ## Extract data for selected sets and return the status
            ## dataframe.
            dfProc<-extr_data_curr()
            input$genRunB
            shiny::isolate({
                fTab<-gen_base_ftab()
                nProc<-as.integer(input$genNoProc)
                sets<-input$genSetSelInp
                intThresh<-as.numeric(input$intThresh)
                noiseFac<-as.numeric(input$noiseFac)
                rtDelta<-as.numeric(input$rtDelta)
                limFinePPM<-as.numeric(input$ppmLimFine)
                limEIC<-as.numeric(input$eicLim)
                rtDelta<-as.numeric(input$rtDeltaWin)
                for (s in sets) {
                    message("***** BEGIN set ",s, " *****")
                    dest<-rvConf$projDir
                    gc()
                    dir.create(s,showWarnings=F)
                    unset_gen_done(s)
                    gen(fTab=fTab[fTab$set==s,],
                        proc=nProc,
                        limFinePPM=limFinePPM,
                        limEIC=limEIC,
                        rtDelta=rtDelta)
                    set_gen_done(s)
                    dfProc<-extr_data_set(dfProc,s)
                    message("***** END set ",s, " *****")
                }
                gc()
            })
            dfProc
        })

        ## TODO TODO TODO
        gen_ms2_data<-shiny::reactive({
            ## After qa is done, process the ms2 spectra.
            dfProc<-extr_data_qa()
            dfProc
        })

        extr_data<-shiny::reactive({
            ## Top-level call to start the chain of reactions needed
            ## to extract data from sets.
            gen_ms2_data()
        })


        gen_spec_props<-shiny::reactive({
            tags<-get_curr_tags()
            spectProps<-sapply(tags,function (tag) paste("spectProps",tag,sep=""))
            names(spectProps) <- tags
            spectProps
        })

        gen_spec_chk_box<-shiny::reactive({
            id<-get_curr_id()
            set<-get_curr_set()
            md<-get_curr_mode()
            mtr<-get_mtr()
            QANAMES<-rvConf$QANAMES
            sdf <- queryFileTable(df=mtr,set=set,mode=md,id=id)
            sdf$tag<-as.character(sdf$tag)
            sprops<-gen_spec_props()
            res<-lapply(sdf$tag,function(t) {
                sprop <- sprops[[t]]
                sdfSel<-sdf[sdf$tag %in% t,QANAMES]
                sel <- as.logical(sdfSel)
                selected <- QANAMES[sel]
                names(selected) <- QANAMES[sel]
                selected
            })
            names(res)<-sdf$tag
            res
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

        shiny::observeEvent(input$impKnownListB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impKnownListB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="fnKnownL",
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
            rvTab$mzml<-df
        })

        shiny::observeEvent(input$mzMLB,
        {
            fchoice<-shinyFiles::parseFilePaths(root=volumes,input$mzMLB)
            paths<-fchoice[["datapath"]]
            rvTab$mzml<-add_mzML_files(rvTab$mzml,paths)
        })
        

        shiny::observeEvent(input$presSelSet,{
            rvConf$currIDpos<-1
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
            tags<-get_curr_tags()
            res <- lapply(tags,getCheckboxValues,input,rvConf)
            names(res) <- tags
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
            mtr<-get_mtr()
            tab2file(tab=mtr,file=fn)
        })

        shiny::observeEvent(input$saveplot,
        {
            id=get_curr_id()
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotCpdID_%i.pdf"
            fn <- sprintf(pfn,id)
            plot_id<-gen_mset_plot_f()
            prop<-plotProps()
            pdf(file=fn, width=12, height=8)
            print(plot_id(id,prop=prop))
            dev.off()
            message("Plotting compound ", i," to ",fn," done.")
        })

        shiny::observeEvent(input$saveallplots,
        {
            id=get_curr_id()
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotall.pdf"
            fn <- sprintf(pfn,id)
            pdf(file=fn, width=12, height=8)
            ids<-get_curr_set_ids()
            plot_id<-gen_mset_plot_f()
            prop<-plotProps()
            for (id in ids) {
                print(plot_id(id,prop=prop))
                message("Plotting compound ", id," done.")
            }
            dev.off()
        })

        ## ***** Observe *****




        ## ***** Render *****
        output$knownCtrl <- rhandsontable::renderRHandsontable({
            df<-get_known()
            out<-if (!is.null(df)) {
                     df
                 } else {
                     data.frame(ID=numeric(),Name=character(),SMILES=character(),RT=numeric())
                 }
            rhandsontable::rhandsontable(out,stretchH="all")
        })

        output$unkCtrl <- rhandsontable::renderRHandsontable({
            df<-get_unk()
            out<-if (!is.null(df)) {
                     df
                 } else {
                     data.frame(ID=numeric(),mz=numeric())
                 }
            rhandsontable::rhandsontable(out,stretchH="all")
        })

        output$setIdCtrl<- rhandsontable::renderRHandsontable({
            df<-get_setid_tab()
            rhandsontable::rhandsontable(df,stretchH="all")
        })

        output$mzMLtabCtrl <- rhandsontable::renderRHandsontable({
            df<-get_mzml_work()
            rhandsontable::rhandsontable(df,stretchH="all")
        })


        output$genTabProcCtrl<-rhandsontable::renderRHandsontable({
            df<-extr_data()
            ## todo<-extr_data_ms2_todo(rvTab$extrData)
            rhandsontable::rhandsontable(df,stretchH="all",rowHeaders = F)
            
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
            lids<-length(get_curr_set_ids())
            x<-rvConf$currIDpos
            req(x<=lids)
            shiny::selectInput("presSelCmpd",
                               "Compound",
                               choices = choices,
                               selected = x,
                               multiple = F)
        })
        
        output$nvPanel<-shiny::renderUI({
            message("Rendering panel started")
            QANms<-rvConf$QANAMES
            names(QANms)<-QANms
            tags<-get_curr_tags()
            sprops<-gen_spec_props()
            schoices<-gen_spec_chk_box()
            tabPanelList <- lapply(tags, function(tag) {
                shiny::tabPanel(tag, shiny::checkboxGroupInput(inputId = sprops[[tag]],
                                                               label = "Quality Control",
                                                               choices = QANms,
                                                               selected = schoices[[tag]]),
                                shiny::textAreaInput(paste("caption",tag,sep=""), "Comments:", "Insert your comment here..."),
                                shiny::verbatimTextOutput(paste("value",tag,sep=""))
                                )})
                
                
            message("done rendering panel")
            do.call(shiny::navlistPanel, tabPanelList)
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
