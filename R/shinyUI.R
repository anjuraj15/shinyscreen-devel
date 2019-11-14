mkUI <- function() {
    browseFile <- function(title,
                           buttonName,
                           txtName,
                           buttonTxt="Import compound list.",
                           txtTxt="",
                           icon="file",
                           description=NULL,
                           ...) {
        shinydashboard::box(title=title,
                            shiny::h5(description),

,
                            solidHeader=T,
                            collapsible=F,...)}
    
    confImport <- shinydashboard::box(title="Import",
                                      shiny::h5("There are two tables that need to be supplied before prescreening starts. One is the compound list, its format being the same like the one for the RMassBank (fields: ID,Name,SMILES,RT,CAS,mz,Level). Another is the compound set table (fields: ID,set). Once those tables are imported, they can further be modified as copies inside the project dir. Shinyscreen will never modify any initial (meta)data. If set field of the compound set table is NA, then that file is in a set of its own. The table format should be CSV with `,' as delimiter and any strings that possibly contain commas should be protected. LibreOffice Calc is helpful when it is needed to convert CSVs from one format to another, as well as protecting strings with quotes. "),
                                      shiny::textInput("impCmpListInp",
                                                       "Imported compound list.",
                                                       value=""),
                                      shiny::textInput("impSetIdInp",
                                                       "Compound set table.",
                                                       value=""),
                                      shiny::textInput("impGenRMBInp",
                                                       "RMassBank settings.",
                                                       value=""),
                                      shinyFiles::shinyFilesButton("impCmpListB",
                                                                   label="Import compound list.",
                                                                   title="",
                                                                   icon=shiny::icon("file"),
                                                                   multiple=F),
                                      shinyFiles::shinyFilesButton("impSetIdB",
                                                                   label="Import compound set table.",
                                                                   title="",
                                                                   icon=shiny::icon("file"),
                                                                   multiple=T),
                                      shinyFiles::shinyFilesButton("impGenRMBB",
                                                                   label="Import RMassBank settings.",
                                                                   title="",
                                                                   icon=shiny::icon("file"),
                                                                   multiple=F),
                                      width=NULL)

    confmzMLTags <- shinydashboard::box(title="Sets and Tags",
                                        shiny::h5("Shinyscreen uses two properties, tags and sets, to categorise mzML data. Tags are properties of individual files. For example, if a single file represents a collection of spectra acquired at a specific collision energy, that energy could be used as a tag. Tags are used to differentiate the spectra in a chromatogram. Sets are collections of tagged files and are read from the compound set table. Each set is going to be screened for a designated collection of masses."),
                                        shiny::textInput("tagPropInp",
                                                         "What is a tag? (example: collision energy; can be left empty.)",
                                                         value=""),
                                        shiny::textInput("tagsInp",
                                                         "Comma-delimited list of tag types",
                                                         value=""),
                                        width=NULL)

    confState <- shinydashboard::box(title="Configuration state",
                                     shiny::h5("Saves and restores current configuration."),
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

    confPP<-shinydashboard::box(title="Preprocessing settings",
                                shiny::textInput("confFileTabBase",
                                                 "Basic file table.",
                                                 value=FN_FTAB_BASE),
                                shiny::textInput("confFileTabProcInp",
                                                 "Preprocessed file table.",
                                                 value=FN_FTAB_PP),
                                shiny::textInput("confResFileTab",
                                                 "Resulting file table.",
                                                 value=FN_FTAB),
                                width=NULL)
    
    confmzMLtab <-shinydashboard::box(title="mzML file table",
                                      shiny::h5("Use this file table to assign adduct modes and tags to the data files."),
                                      shinyFiles::shinyFilesButton("mzMLB",
                                                                   label="Select mzML files",
                                                                   title="Select mzML files",
                                                                   icon=shiny::icon("files-o"),
                                                                   multiple=T),
                                      shiny::actionButton("mzMLtabSubmB",
                                                          label="Submit mzML list.",
                                                          icon=shiny::icon("check")),
                                      rhandsontable::rHandsontableOutput("mzMLtabCtrl"),
                                      width=NULL)

    
    confLayout <- shiny::fluidRow(shiny::column(confImport,
                                                confmzMLTags,
                                                confState,
                                                width=4),
                                  shiny::column(width=8,
                                                confPP,
                                                confmzMLtab))


    confTab <- shinydashboard::tabItem(tabName="config",
                                       confLayout)

    ## ***** Compound List Tab *****

    cmpListBox<-shinydashboard::box(title="Compound list",
                                    rhandsontable::rHandsontableOutput("cmpListCtrl"),
                                    width=NULL)
    
    cmpListLayout <- shiny::fluidRow(shiny::column(cmpListBox,
                                                   width = 12))

    cmpListTab <- shinydashboard::tabItem(tabName="compList",
                                          shiny::h5("This is an editable view of the compound list."),
                                          cmpListLayout)

    ## ***** Sets of compounds *****

    setIdBox<-shinydashboard::box(title="Compound sets",
                                  rhandsontable::rHandsontableOutput("setIdTabCtrl"),
                                  width = NULL)

    setIdLayout<-shiny::fluidRow(shiny::column(setIdBox,
                                               width = 12))

    setIdTab<-shinydashboard::tabItem(tabName="setId",
                                      shiny::h5("This is an editable view of the id/set list."),
                                      setIdLayout)

    genBoxParam1<-shinydashboard::box(title="Parameters of the run",
                                      shiny::textInput("genNoProc",
                                                      label="Number of processes.",
                                                      value=1),
                                      shiny::textInput("ppmLimFine",
                                                       label="ppm limit fine",
                                                       value=10),
                                      shiny::textInput("eicLim",
                                                       label="EIC limit.",
                                                       value=1e-3),
                                      shiny::textInput("intTresh",
                                                       label="Intensity threshold.",
                                                       value=1e5),
                                      shiny::textInput("noiseFac",
                                                       label="Signal-to-noise ratio.",
                                                       value=3),
                                      shiny::textInput("rtDelta",
                                                       label="Retention time Δ",
                                                       value=0.5),
                                      width=NULL)
    
    genBoxParam2<-shinydashboard::box(title=NULL,
                                      shiny::selectInput("genSetSelInp",
                                                         label="Select set(s).",
                                                         choices="",
                                                         multiple=T),
                                      shiny::actionButton(inputId="genRunB",
                                                          label="Run!",
                                                          icon=shiny::icon("bomb")),
                                      width=NULL)
    genBoxProcessed<-shinydashboard::box(title="Processed sets",
                                         shiny::actionButton(inputId="genFileTabB",
                                                             label="Generate file table.",
                                                             icon=shiny::icon("save")),
                                         shiny::actionButton(inputId="genRunPPB",
                                                             label="Preprocess!",
                                                             icon=shiny::icon("bomb")),
                                         rhandsontable::rHandsontableOutput("genTabProcCtrl"),
                                         width=NULL)
    
    genTab<-shinydashboard::tabItem(tabName = "gen",
                                    shiny::h5("Prepare for prescreening."),
                                    shiny::fluidRow(shiny::column(genBoxProcessed,
                                                                  width=4)),
                                    shiny::fluidRow(shiny::column(genBoxParam1,width=4)),
                                    shiny::fluidRow(shiny::column(genBoxParam2,width=4)))

    ## ***** Prescreening *****



    ## Prescreening elements
    preshead <- shinydashboard::dashboardHeader(title = "Prescreening")
    
    presCompInfo <- shiny::fluidRow(shinydashboard::box(title = "MS Prescreening",
                                                        width = 7,
                                                        height = "80px",
                                                        background = "blue",
                                                        ""),
                                    shinydashboard::box(title = "Compound ID N°",
                                                        width = 5,
                                                        height = "80px",
                                                        background = "olive",
                                                        shiny::textOutput("compoundID")))

    presPlotBox <- shinydashboard::box(title = "Plot",
                                       width = 7,color = "olive",
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
                                                         hoverId = NULL),
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
                                          width=5,
                                          solidHeader = FALSE,
                                          color = "olive",
                                          collapsible = TRUE,
                                          "",
                                          shiny::br(),
                                          shiny::selectInput("presSelSet",
                                                             "Set",
                                                             choices="",
                                                             multiple=F),
                                          shiny::actionButton("presPrev",
                                                              "Previous compound.",
                                                              icon = shiny::icon("backward")),
                                          shiny::actionButton("presNext",
                                                              "Next compound.",
                                                              icon = shiny::icon("forward")),
                                          shiny::selectInput("presSelCmpd",
                                                             "Compound",
                                                             choices="",
                                                             multiple=F))

    nvPanel<-shiny::uiOutput("nvPanel")
    presQABox <- shinydashboard::box(title = "Prescreening analysis",
                                     width = 5,
                                     solidHeader = FALSE,
                                     collapsible = TRUE,
                                     nvPanel,
                                     shiny::actionButton("submitQA",
                                                         "Submit",
                                                         icon = shiny::icon("save")),
                                     shiny::textInput("fn_ftable",
                                                      "File table Name",
                                                      value=FN_FTAB),
                                     shiny::actionButton("savefiletable",
                                                         "Save File Table",
                                                         icon = shiny::icon("save")))
    presPlotParBox <- shinydashboard::box(title = "Plot Parameters",
                                          width=7,
                                          solidHeader = FALSE,
                                          collapsible = TRUE,
                                          "",
                                          shiny::br(),
                                          shiny::numericInput("min_val",
                                                              "Minimum x Axis Value",
                                                              DEFAULT_RT_RANGE[[1]]),
                                          shiny::numericInput("max_val",
                                                              "Maximum x Axis Value",
                                                              DEFAULT_RT_RANGE[[2]]),
                                          shiny::radioButtons("yaxis",
                                                              "Parameters for y Axis",
                                                              c(linear = "linear",
                                                                log = "log")),
                                          shiny::numericInput("nice",
                                                              "Nice",
                                                              DEFAULT_RT_RANGE[[1]]),
                                          shiny::numericInput("steps",
                                                              "Steps",
                                                              DEFAULT_RT_RANGE[[2]]))
    presPlotWidget <- shiny::fluidRow(presPlotBox,
                                      presCompSelBox,
                                      presQABox,
                                      presPlotParBox)
    
    presTab <- shinydashboard::tabItem(tabName = "prescreen",
                                       shiny::h2("Prescreening"),
                                       presCompInfo,
                                       presPlotWidget)





    ## ***** Top-level Elements *****
    
    headerText <- "Shinyscreen"

    

    confSideItem <- shinydashboard::menuItem(text="Config",
                                             tabName="config",
                                             icon=shiny::icon("user-cog"))
    
    compListSideItem <- shinydashboard::menuItem(text="Compound list",
                                                 tabName="compList",
                                                 icon=shiny::icon("table"))

    setIdSideItem <- shinydashboard::menuItem(text="Compound sets",
                                                 tabName="setId",
                                              icon=shiny::icon("table"))

    genSideItem <- shinydashboard::menuItem(text="Generate prescreen data",
                                                 tabName="gen",
                                                 icon=shiny::icon("cogs"))

    presSideItem <- shinydashboard::menuItem(text="Prescreening",
                                             tabName="prescreen",
                                             icon=shiny::icon("chart-bar"))
    
    header <- shinydashboard::dashboardHeader(title=headerText)
    
    sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(confSideItem,
                                                                            compListSideItem,
                                                                            setIdSideItem,
                                                                            genSideItem,
                                                                            presSideItem))


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
    volumes <- shinyFiles::getVolumes()

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
    
    extd_mzMLtab<-function(ft,fn) {
        modeLvl<- c("select","pH","pNa","pM",
                    "mH","mFA")

        lSet<-levels(ft$set)
        lTag<-levels(ft$tag)
        newRow<-data.frame(Files=fn,
                           mode=factor(modeLvl[[1]],levels=modeLvl),
                           set=if (length(lSet) > 0) factor(lSet[[1]],levels=lSet) else factor("unspecified"),
                           tag=if (length(lTag) > 0) factor(lTag[[1]],levels=lTag) else factor("unspecified"),
                           stringsAsFactors = F)

        levels(newRow$mode)<-modeLvl


        res<-rbind(ft,newRow,
                   stringsAsFactors = F,
                   make.row.names = F)
        levels(res$mode)<-modeLvl

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
    mkCmpdDrop<-function(set,setTab) {
        wh<-which(setTab$set %in% set)
        ids<-setTab$ID[wh]
        mz<-setTab$mz[wh]
        entries<-base::Map(function(i,m) paste(i,'; ','mz: ',m,sep=''),ids,mz)
        entries
    }

    queryFileTable <- function(df,set,id) {
        df[(df$ID %in% id) & (df$set %in% set),]
    }


    updateFileTable <- function(df,set,id,linput) {
        for (tag in names(linput)) {
            entries <- names(linput[[tag]])
            cond <- (df$ID %in% id) & (df$tag == tag) & (df$set %in% set)

            df[cond,entries] <- linput[[tag]]
        }
        df
    }
    
    getCheckboxValues <- function(tag,input,rv) {
        chkbox <- input[[rv$spectProps[[tag]]]]
        q <- sapply(QANAMES,function (qn) if (qn %in% chkbox) T else F)
        names(q) <- QANAMES
        q
    }

    getSetMode <- function(set,mzMLtab) {
        sdf<-mzMLtab[which(mzMLtab$set %in% set),]
        levels(factor(sdf$mode))
    }

    server <- function(input,output,session) {

        ## ***** reactive values *****
        rvConf <- shiny::reactiveValues(mzMLtab=mk_mzMLtab(),
                                        tags=list(),
                                        sets=list(),
                                        spectProps=list(),
                                        QANAMES=QANAMES,
                                        MODEMAP=MODEMAP,
                                        impCmpListFn="",
                                        impSetIdFn="",
                                        impGenRMBFn="",
                                        fnLocSetId=FN_LOC_SETID,
                                        fnFTBase="",
                                        tagProp="",
                                        setProp="",
                                        mode=modeLvl,
                                        projDir=projDir,
                                        currSet=NA,
                                        currIDSel=1,
                                        currIDSet=list(),
                                        currID=NA,
                                        fnFT=NULL,
                                        fTab=NULL)

        rvCmpList<- shiny::reactiveValues(df=mk_cmpList())
        rvSetId<- shiny::reactiveValues(df=mk_setId())
        rvPres<-shiny::reactiveValues(cex=CEX,
                                      rt_digits=RT_DIGITS,
                                      m_digits=M_DIGITS,
                                      pal=PAL,
                                      plot_id=NULL)

        ## ***** shinyFiles observers *****
        wdroot<-c(wd=projDir)
        shinyFiles::shinyFileChoose(input, 'impCmpListB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impSetIdB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impGenRMBB',roots=volumes)
        
        shinyFiles::shinyFileSave(input, 'saveConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'restoreConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'mzMLB',roots=volumes)

        ## ***** reactive function definitions *****
        
        getTags<-shiny::reactive({
            if (length(input$tagsInp)>0 && !is.na(input$tagsInp)) unlist(strsplit(input$tagsInp, ",")) else list()
        })

        getSets<-shiny::reactive({
            levels(rvSetId$df$set)
        })

        getCmpL<-shiny::reactive({
            rvCmpList$df #rhandsontable::hot_to_r(input$cmpListCtrl)
        })

        getSetId<-shiny::reactive({
            rvSetId$df #rhandsontable::hot_to_r(input$setIdTabCtrl)
        })

        update_tags_mzMLtab<-shiny::reactive({
            input$tagsInp
            tags<-getTags()
            tagCol<-rvConf$mzMLtab$tag
            if (length(levels(tagCol))==0) rvConf$mzMLtab$tag<-factor(tagCol)
            rvConf$mzMLtab$tag<-factor(tagCol,levels=tags)
        })

        update_sets_mzMLtab<-shiny::reactive({
            sets<-getSets()
            setCol<-rvConf$mzMLtab$set
            if (length(levels(setCol))==0) rvConf$mzMLtab$set<-factor(setCol)
            rvConf$mzMLtab$set<-factor(setCol,levels=sets)
        })

        saveConf<-reactive({
            fn<-shinyFiles::parseSavePath(root=c(wd=rvConf$projDir),input$saveConfB)[["datapath"]]
            if ((! is.na(fn)) && length(fn)>0) {
                message("Saving config to",fn)
                sav<-list()
                sav<-list(rvConf=list(),
                          input=list())
                shiny::isolate(for (nm in names(rvConf)) {
                                   sav$rvConf[[nm]]<-rvConf[[nm]]
                               })
                sav$input$tagsInp<-input$tagsInp
                sav$input$setsInp<-input$setsInp
                sav$input$impCmpListInp<-input$impCmpListInp
                sav$input$impSetIdInp<-input$impSetIdInp
                sav$input$impGenRMBInp<-input$impGenRMBInp
                saveRDS(object=sav,file=fn)
                }
        })

        restoreConf<-reactive({
            input$restoreConfB
            fnobj<-shinyFiles::parseFilePaths(root=c(wd=rvConf$projDir),input$restoreConfB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn) && nchar(fn)>0) {
                message("Restoring config from",fn)
                sav<-readRDS(fn)
                ## shiny::isolate({
                ##     sav<-readRDS(fn)
                ##     for (nm in names(sav$rvConf)) {
                ##         rvConf[[nm]]<-sav$rvConf[[nm]]
                ##     }

                shiny::updateTextInput(session=session,
                                       inputId="impGenRMBInp",
                                       value=sav$input$impGenRMBInp)
                shiny::updateTextInput(session=session,
                                       inputId="impCmpListInp",
                                       value=sav$input$impCmpListInp)
                shiny::updateTextInput(session=session,
                                       inputId="impSetIdInp",
                                       value=sav$input$impSetIdInp)
                shiny::updateTextInput(session=session,
                                       inputId="tagsInp",
                                       value=sav$input$tagsInp)
                shiny::updateTextInput(session=session,
                                       inputId="confFileTabBase",
                                       value=sav$input$confFileTabBase)
                shiny::updateTextInput(session=session,
                                       inputId="confFileTabProcInp",
                                       value=sav$input$confFileTabProcInp)
                shiny::updateTextInput(session=session,
                                       inputId="confResFileTab",
                                       value=sav$input$confResFileTab)

                rvConf$mzMLtab<-sav$rvConf$mzMLtab
                rvConf$fnFTBase<-sav$rvConf$fnFTBase
                rvConf$fnFT<-sav$rvConf$fnFT


                    
                
            }
        })


        currSetMkCmpMenu<-shiny::reactive({
            set<-rvConf$currSet
            setID<-getSetId()
            hasMz<-"mz" %in% colnames(setID)
            if (!is.na(set) && hasMz) {
                setID<-setID[setID$set %in% set,]
                ids<-setID$ID[set %in% setID$set]
                entries<-mkCmpdDrop(set,setID)
                ch<-as.list(1:length(ids))
                names(ch)<-entries
                shiny::updateSelectInput(session=session,
                                         "presSelCmpd",
                                         choices=ch,
                                         selected = 1)
                rvConf$currIDSet<-ids

            }
        })
        currSetPreCalc<-shiny::reactive({
            set<-rvConf$currSet
            fTab<-rvConf$fTab

            if (!is.na(set) && length(fTab)>0) {
                setId<-getSetId()
                hasMz<-"mz" %in% colnames(setId)
                if (hasMz) {
                    setIdIds<-setId[,"ID"]
                    setIdSMILES<-setId[,"SMILES"]
                    setIdMz<-setId[,"mz"]
                    tags<-rvConf$tags
                    iSet<-which(set==fTab$set)
                    sfTab<-fTab[iSet,]
                    tags<-levels(factor(sfTab$tag))
                    iTag<- match(tags,sfTab$tag)
                    wd<-sfTab$wd[iTag]
                    cmpL<-rvCmpList$df
                    
                    preID<-setIdIds
                    smiles<-setIdSMILES
                    mz<-setIdMz
                    names(smiles)<-as.character(preID)
                    names(mz)<-as.character(preID)
                    ## Get the basenames of eic files.
                    eics <- list.files(path=wd[[1]],patt=".*eic.csv")
                    eicsPref <- sapply(strsplit(eics,split="\\."),function(x) x[[1]])
                    eicsID <- as.integer(eicsPref)
                    maybekids <- sapply(eicsPref,function(x) {paste(x,'.kids.csv',sep='')})
                    names(eics) <- eicsID
                    names(maybekids) <- eicsID
                    plot_id <- function (i,rtrange=NULL,log=input$yaxis) {
                        i=as.character(i)
                        mz=mz[[i]]
                        smile<-smiles[[i]]
                        message("i:",i)
                        message("mz:",mz)
                        message("smile:",smile)
                        
                        plot_id_aux(i,wd=wd,eics=eics,maybekids=maybekids,mass=mz,smile=smile,tags=tags,log=log,rtrange=rtrange,cex=rvPres$cex,pal=rvPres$pal,rt_digits=rvPres$rt_digits,m_digits=rvPres$m_digits,fTab=sfTab)
                    }
                    rvPres$plot_id<-plot_id
                }
            }
        })




        ## ***** Observe Event *****

        shiny::observeEvent(input$saveConfB,{
            saveConf()
        })


        shiny::observeEvent(input$impSetIdB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impSetIdB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="impSetIdInp",
                                       value=fn)
            }})


        shiny::observeEvent(input$impCmpListB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impCmpListB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="impCmpListInp",
                                       value=fn)
            }})


        shiny::observeEvent(input$mzMLtabSubmB,{
            mzML<-rhandsontable::hot_to_r(input$mzMLtabCtrl)
            rvConf$mzMLtab<-mzML

            ## Fill out mz in sets
            sets<-getSets()
            setMode<-mzML$mode[match(sets,mzML$set)]
            names(setMode)<-sets
            nR<-nrow(rvSetId$df)
            dfSet<-rvSetId$df
            dfSet$mz<-rep(NA,nR)
            dfSet$SMILES<-rep(NA,nR)
            dfSet$Name<-rep(NA,nR)
            cmpL<-getCmpL()
            for (s in sets) {
                md<-setMode[[s]]
                ind<-which(dfSet$set==s)
                for (i in ind) {
                    id<-dfSet[i,"ID"]
                    dfSet[i,"mz"]<-getMzFromCmpL(id,md,cmpL)
                    dfSet[i,"SMILES"]<-getSMILESFromCmpL(id,cmpL)
                    dfSet[i,"Name"]<-getColFromCmpL(id,"Name",cmpL)
                }
            }
            ## rvSetId$df<-dfSet
            fn<-rvConf$fnLocSetId
            write.csv(file=fn,
                      row.names=F,
                      x=dfSet)
            message("New set id table written to: ",fn)
            rvConf$impSetIdFn<-fn
        })

        shiny::observeEvent(input$impGenRMBB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impGenRMBB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="impGenRMBInp",
                                       value=fn)
            }})

        shiny::observeEvent(input$impCmpListInp,
        {
            impCmpFn<-input$impCmpListInp
            if (length(impCmpFn)>0 && !is.na(impCmpFn) && nchar(impCmpFn)>0) {
                rvConf$impCmpListFn<-impCmpFn
                message("rvConf$impCmpListFn is changed to:",impCmpFn)
                
            }
        })

        shiny::observeEvent(rvConf$impCmpListFn,
        {
            fn<-rvConf$impCmpListFn
            if (file.exists(fn)) {
                message("Importing compound list from:",fn)
                df<-importCmpList(fn)
                rvCmpList$df<-df
                message("Done importing compound list from: ",fn)

            }
        })

        
        shiny::observeEvent(input$impSetIdInp,
        {
            fn<-input$impSetIdInp
            if (length(fn)>0 && !is.na(fn) && nchar(fn)>0) {
                rvConf$impSetIdFn<-fn
                message("rvConf$impSetIdFn is changed to:",fn)
            }
        })

        shiny::observeEvent(input$genFileTabB,{
            fn<-input$confFileTabBase
            if (length(fn)>0 && !is.na(fn) && nchar(fn)>0) {
                message("Generating basic file table in file ",fn)
                files<-adornmzMLTab(rvConf$mzMLtab,projDir=rvConf$projDir)
                setId<-rvSetId$df
                cmpL<-rvCmpList$df
                df<-genSuprFileTbl(files,setId,destFn=fn)
                df<-addCmpLColsToFileTbl(df,cmpL)
                write.csv(x=df,file=fn,row.names=F)
                rvConf$fnFTBase<-fn
                message("Done generating basic file table in file ",fn)
            }

        })

        shiny::observeEvent(input$genRunB,{
            FnRMB<-input$impGenRMBInp
            nProc<-as.integer(input$genNoProc)
            fnTab<-rvConf$fnFTBase
            sets<-input$genSetSelInp
            message("Selected sets:")
            message(str(sets))
            message("Number of processes:",nProc)
            message("RMassBank settings file:",FnRMB)
            message("File table:",fnTab)
            if (length(fnTab)>0) {

                for (s in sets) {
                    message("***** BEGIN set ",s, " *****")
                    fnCmpdList<-input$impCmpListInp
                    fnStgs<-input$impGenRMBInp
                    intTresh<-as.numeric(input$intTresh)
                    noiseFac<-as.numeric(input$noiseFac)
                    rtDelta<-as.numeric(input$rtDelta)
                    ppmLimFine<-as.numeric(input$ppmLimFine)
                    dest<-rvConf$projDir
                    eicLim<-as.numeric(input$eicLim)
                    gc()
                    gen(fnFileTab=fnTab,
                        fnCmpdList=fnCmpdList,
                        fnStgs=fnStgs,
                        dest=dest,
                        proc=nProc,
                        intTresh=intTresh,
                        noiseFac=noiseFac,
                        rtDelta=rtDelta,
                        ppmLimFine=ppmLimFine,
                        eicLim=eicLim)
                    message("***** END set ",s, " *****")
                }
                gc()
            }})

        shiny::observeEvent(input$genRunPPB,{
            message("Starting preprocessing.")
            
            shiny::isolate({
                sets<-getSets()
                cdf<-if (!is.null(rvCmpList$df)) rvCmpList$df else NULL})
            
            nr<-nrow(cdf)
            if (!is.null(nr)) {
                if (is.na(nr)) nr<-0
            } else nr<-0
            if (length(sets)>0 && nr>0) {
                doneSets<-sets[sapply(sets,isGenDone)]
                if (length(doneSets)>0) {
                    fnFullTab<-input$confFileTabProcInp
                    fnBaseTab<-input$confFileTabBase
                    fnOpen<-""
                    fnOpen<-if (file.exists(fnFullTab)) fnFullTab else fnBaseTab
                    fullFTab<-read.csv(file=fnOpen,
                                       comment.char = '',
                                       stringsAsFactors = F)

                                    doneFTab<-fullFTab[fullFTab$set %in% doneSets,]
                    if (nrow(doneFTab)>0) {
                        fnTmp<-ppInpFt()
                        write.csv(file=fnTmp,
                                  x=doneFTab,
                                  row.names=F)
                        message("fnTmp: ",fnTmp)
                        cmpdL<-rvCmpList$df
                        maxId<-do.call(max,as.list(sapply(doneSets,idsFromFiles)))
                        intTresh<-as.numeric(input$intTresh)
                        noiseFac<-as.numeric(input$noiseFac)
                        rtDelta<-as.numeric(input$rtDelta)
                        ## dr<-file.path(dirname(fnCand),sets)
                        preProc(fnFileTab=fnTmp,
                                lCmpdList=maxId,
                                fnDest=fnTmp,
                                intTresh=intTresh,
                                noiseFac=noiseFac,
                                rtDelta=rtDelta)
                        ppFnTab<-read.csv(file=fnTmp,
                                          comment.char = '',
                                          stringsAsFactors = F)
                        extNms<-names(ppFnTab)
                        basNms<-names(fullFTab)
                        diffNms<-setdiff(extNms,basNms)
                        nrf<-nrow(fullFTab)
                        for (nm in diffNms) {
                            z<-logical(length=nrf)
                            z<-T
                            fullFTab[[nm]]<-z
                        }
                        fullFTab[fullFTab$set %in% doneSets,]<-ppFnTab
                        write.csv(file=fnFullTab,
                                  x=fullFTab,
                                  row.names=F)
                        file.copy(fnFullTab,input$confResFileTab,overwrite=T)
                        message("Finished preprocessing.")
                                        
                                        
                    }
                }
            }
        })

        shiny::observeEvent(input$presSelCmpd,{
            pos<-input$presSelCmpd
            rvConf$currIDSel<-as.numeric(pos)
        })

        shiny::observeEvent(input$presPrev,{
            len<-length(rvConf$currIDSet)
            x<-rvConf$currIDSel-1
            if (x>0) rvConf$currIDSel<-x
        })

        shiny::observeEvent(input$presNext,{
            len<-length(rvConf$currIDSet)
            x<-rvConf$currIDSel+1
            if (x<=len) rvConf$currIDSel<-x
        })

        shiny::observeEvent(rvConf$fnFT,{
            fn<-rvConf$fnFT
            if (!is.null(fn) && file.exists(fn)) {
                rvConf$fTab<-read.csv(file=fn,
                                      comment.char = '',
                                      stringsAsFactors = F)
            }
        })

        shiny::observeEvent(rvConf$currIDSel,{
            ids<-rvConf$currIDSet
            if (length(ids)>0) rvConf$currID<-ids[[rvConf$currIDSel]]
        })


        shiny::observeEvent(input$submitQA,{
            res <- lapply(rvConf$tags,getCheckboxValues,input,rvConf)
            names(res) <- rvConf$tags
            rvConf$fTab <- updateFileTable(df=rvConf$fTab,
                                           set=input$presSelSet,
                                           id=rvConf$currID,
                                           linput=res)
        })
        
        shiny::observeEvent(input$savefiletable,
        {
            fn<-input$fn_ftable
            message("Writing current file table to ",fn)
            write.csv(file=fn,x=rvConf$fTab,row.names = F)
        })


        shiny::observeEvent(input$saveplot,
        {
            i=rvConf$currID
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotCpdID_%i.pdf"
            fn <- sprintf(pfn,i)
            rtrange <- c(input$min_val,input$max_val)
            pdf(file=fn, width=12, height=8)
            rvPres$plot_id(i,rtrange=rtrange, log=input$yaxis)
            dev.off()
            message("Plotting compound ", i," to ",fn," done.")
        })

        shiny::observeEvent(input$saveallplots,
        {
            i=rvConf$currID
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotall.pdf"
            fn <- sprintf(pfn,i)
            pdf(file=fn, width=12, height=8)
            for (i in rvConf$currIDSet) {
                rvPres$plot_id(i,log=input$yaxis)
                message("Plotting compound ", i," done.")
            }
            dev.off()
        })
        ## ***** Observe *****
        shiny::observe({
            fchoice<-shinyFiles::parseFilePaths(root=volumes,input$mzMLB)
            paths<-fchoice[["datapath"]]
            isolate({
                for (pt in paths) {
                    rvConf$mzMLtab<-extd_mzMLtab(rvConf$mzMLtab,pt)
                }
        
                })
        })

        shiny::observe({
            rvConf$currIDSel
            shiny::updateSelectInput(session=session,
                                     inputId="presSelCmpd",
                                     selected=rvConf$currIDSel)
        })
        shiny::observe({
            shiny::invalidateLater(100,
                                   session=session)
            output$genTabProcCtrl<-rhandsontable::renderRHandsontable({
                sets<-getSets()
                genState<-sapply(sets,isGenDone)
                df<-if (!is.null(sets)) {data.frame(set=sets,
                                                    generated=genState,
                                                    stringsAsFactors=F)
                    } else {data.frame(sets=character(),
                                       generated=logical(),
                                       stringsAsFactors=F)} 
                rhandsontable::rhandsontable(df,
                                             rowHeaders=NULL,
                                             readOnly=T,
                                             stretchH="all")
                
            })
        })
    
        shiny::observe({
            shiny::invalidateLater(100,
                                   session=session)
            fnFT<-if (file.exists(input$confResFileTab)) input$confResFileTab else NULL
            rvConf$fnFT<-fnFT
              
        })
    

        shiny::observe({
            sets<-getSets()
            if (length(sets)>0 && !is.na(sets)) {
                message("Bef upd inputs")
                shiny::updateSelectInput(session=session,
                                         "presSelSet",
                                         choices=sets,
                                         selected=sets[[1]])
                message("upd sel worked")
            }
        })


        shiny::observe({
            i<-rvConf$currID
            spectProps<-rvConf$spectProps
            set<-input$presSelSet
            if (!is.na(i) && length(spectProps)>0) {
                message("Updating QA checkboxes.")
                QANAMES<-rvConf$QANAMES
                sdf <- queryFileTable(df=rvConf$fTab,set=set,id=i)
                sdf$tag<-as.character(sdf$tag)
                for (t in sdf$tag) {
                    sprop <- rvConf$spectProps[[t]]
                    sdfSel<-sdf[sdf$tag %in% t,QANAMES]
                    sel <- as.logical(sdfSel)
                    choices <- QANAMES[sel]
                    names(choices) <- QANAMES[sel]
                    shiny::updateCheckboxGroupInput(session = session,inputId = sprop,selected=choices)
                }
                message("Updating QA checkboxes done.")

            }
        })

        shiny::observe({
            sets<-getSets()
            set<-input$presSelSet
            if (!nchar(set)==0) {
                cmpdL<-getCmpL()
                setID<-getSetId()
                rvConf$currSet<-set
            }
        })

        shiny::observe({
            message("currSet START")
            currSetMkCmpMenu()
            currSetPreCalc()
            message("currSet END")

        })

        shiny::observe({
            input$restoreConfB
            message("Restore event observed.")
            restoreConf()
            message("Restore event finished.")
        })

        ## shiny::observe({
        ##     rvConf$tags<-getTags()
        ## })

        shiny::observe({
            message("Update tags and sets.")
            update_sets_mzMLtab()
            update_tags_mzMLtab()
            message("Done updating tags and sets.")
        })

        shiny::observe(
        {
            fn<-rvConf$impSetIdFn
            if (file.exists(fn)) {
                message("Importing compound sets from:",fn)
                rvSetId$df<-readSetId(fn)
                message("Done importing compound sets from: ",fn)
            }
            df<-rvSetId$df
            if (length(df)>0 && !is.na(df) && nrow(df)>0) {
                shiny::updateSelectInput(session=session,
                                         inputId="genSetSelInp",
                                         choices=levels(df$set))
            }
            shiny::isolate({
                message("Changing the inpSetIdFn to: ",fn, " in isolation.")
                shiny::updateTextInput(session=session,
                                       inputId="impSetIdInp",
                                       value=fn)
            })
        })





        ## ***** Render *****
        output$cmpListCtrl <- rhandsontable::renderRHandsontable({
            df<-rvCmpList$df
            rhandsontable::rhandsontable(df,stretchH="all")
        })

        output$setIdTabCtrl<- rhandsontable::renderRHandsontable({
            df<-rvSetId$df
            rhandsontable::rhandsontable(df,stretchH="all")
        })

        output$mzMLtabCtrl <- rhandsontable::renderRHandsontable({
            if (nrow(rvConf$mzMLtab) !=0) rhandsontable::rhandsontable(rvConf$mzMLtab,stretchH="all") else NULL
        })

        output$nvPanel<-shiny::renderUI({
            message("Rendering panel started")
            ft<-rvConf$fTab
            set<-input$presSelSet
            if (nchar(set)>0 && !is.null(ft)) {
                QANms<-rvConf$QANAMES
                names(QANms)<-QANms
                tags<-levels(factor(ft[ft$set==set,]$tag))
                rvConf$tags<-tags
                spectProps<-sapply(tags,function (tag) paste("spectProps",tag,sep=""))
                rvConf$spectProps<-spectProps
                tabPanelList <- lapply(tags, function(tag) {
                    shiny::tabPanel(tag, shiny::checkboxGroupInput(spectProps[[tag]], "Quality Control",
                                                                   QANms),
                                    shiny::textAreaInput(paste("caption",tag,sep=""), "Comments:", "Insert your comment here..."),
                                    shiny::verbatimTextOutput(paste("value",tag,sep=""))
                                    )})
        
                do.call(shiny::navlistPanel, tabPanelList)
                message("done rendering panel")
            } else NULL
        })

        output$chromGram <- renderPlot(
        {
            plot_id<-rvPres$plot_id
            i=rvConf$currID
            rtrange <- c(input$min_val,input$max_val)
            if (!is.null(plot_id)) {
                plot_id(i,rtrange=rtrange, log=input$yaxis)
            }
        })

        
        session$onSessionEnded(function () stopApp())
    }

    shiny::shinyApp(ui=mkUI(),server=server)
}
