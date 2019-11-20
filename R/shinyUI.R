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
                                      shiny::h5("There are up to three tables that need to be supplied before prescreening starts. There are two compound lists, one for targets, containing at least ID and SMILES columns. If column Name is present, it will also be used. The second is the suspect list with columns ID, mz and mode. Consider ID to be a tag of a particular SMILES (for targets), or a mz/mode-combination (for unknowns). The final list is the set ID list which classifies IDs into different sets. Every set consists of unique IDs. The columns of this list are ID and set. Shinyscreen will never modify any initial (meta)data. The table format should be CSV with `,' as delimiter and any strings that possibly contain commas should be protected. LibreOffice Calc is helpful when it is needed to convert CSVs from one format to another, as well as protecting strings with quotes. "),
                                      shiny::textInput("fnTgtL",
                                                       "Target list (ID, SMILES).",
                                                       value=""),
                                      shiny::textInput("fnUnkL",
                                                       "Unknows list (mz and mode)",
                                                       value=""),
                                      shiny::textInput("fnSetId",
                                                       "Set table.",
                                                       value=""),
                                      shiny::textInput("fnStgsRMB",
                                                       "RMassBank settings.",
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
                                      shinyFiles::shinyFilesButton("impGenRMBB",
                                                                   label="Import RMassBank settings.",
                                                                   title="",
                                                                   icon=shiny::icon("file"),
                                                                   multiple=F),
                                      width=NULL)

    confmzMLTags <- shinydashboard::box(title="Sets and Tags",
                                        shiny::h5("Sets and tags."),
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
                                    rhandsontable::rHandsontableOutput("tgtCtrl"),
                                    width=NULL)
    
    cmpListLayout <- shiny::fluidRow(shiny::column(cmpListBox,
                                                   width = 12))

    cmpListTab <- shinydashboard::tabItem(tabName="compList",
                                          shiny::h5("This is an editable view of the compound list."),
                                          cmpListLayout)

    ## ***** Sets of compounds *****

    setIdBox<-shinydashboard::box(title="Compound sets",
                                  rhandsontable::rHandsontableOutput("setIdCtrl"),
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
                                          shiny::selectInput("presSelMode",
                                                             "Mode",
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
    mkCmpdDrop<-function(set,setTab) {
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
        rvConf <- shiny::reactiveValues(
                             allTags=list(),
                             tags=list(),
                             sets=list(),
                             spectProps=list(),
                             QANAMES=QANAMES,
                             MODEMAP=MODEMAP,
                             REST_TXT_INP=REST_TXT_INP,
                             impGenRMBFn="",
                             fnLocSetId=FN_LOC_SETID,
                             fnFTBase="",
                             tagProp="",
                             setProp="",
                             fnComp=FN_COMP_TAB,
                             mode=modeLvl,
                             projDir=projDir,
                             currSet=NA,
                             currMode=NA,
                             currIDSel=1,
                             currIDSet=list(),
                             currID=NA,
                             fnFT=NULL,
                             flMzMLSub=F)
        rvTab<-shiny::reactiveValues(
                          mzML=NULL, # files (File), sets (set) and mode (mode)
                          tgt=NULL, # ids(ID),SMILES(SMILES) and names (Name)
                          unk=NULL, # ids(ID),mz
                          setId=NULL, # ids(ID), sets (set)
                          comp=NULL,  # everything, except file info
                          mtr=NULL)     #master table (everything combined)
        rvPres<-shiny::reactiveValues(cex=CEX,
                                      rt_digits=RT_DIGITS,
                                      m_digits=M_DIGITS,
                                      pal=PAL,
                                      plot_id=NULL)

        ## ***** shinyFiles observers *****
        wdroot<-c(wd=projDir)
        shinyFiles::shinyFileChoose(input, 'impTgtListB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impUnkListB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impSetIdB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impGenRMBB',roots=volumes)
        
        shinyFiles::shinyFileSave(input, 'saveConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'restoreConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'mzMLB',roots=volumes)

        ## ***** reactive function definitions *****
        
        getTags<-shiny::reactive({
            x<-if (length(input$tagsInp)>0 && !is.na(input$tagsInp)) unlist(strsplit(input$tagsInp, ",")) else list()
            as.list(c(x,"unspecified"))
        })

        getSets<-shiny::reactive({
            levels(rvTab$setId$set)
        })

        getMzMLFiles<-shiny::eventReactive(input$mzMLB,
        {
            sets<-getSets()
            tags<-getTags()
            
            fchoice<-shinyFiles::parseFilePaths(root=volumes,input$mzMLB)
            paths<-fchoice[["datapath"]]
            df<-rvTab$mzML
            res<- if (length(sets)>0) {
                      for (pt in paths) df<-extd_mzMLtab(df,pt,sets,tags)
                      df
                  } else {
                      warning("No sets specified. Load the set id table first.")
                      NULL
                  }
            res
                
        })

        getCmpL<-shiny::reactive({
            rvTab$tgt #rhandsontable::hot_to_r(input$tgtCtrl)
        })

        getSetId<-shiny::reactive({
            rvTab$setId #rhandsontable::hot_to_r(input$setIdCtrl)
        })

        getMzML<-shiny::reactive({
            rvTab$mzML
        })

        getComp<-shiny::reactive({
            rvTab$comp
        })

        getTgt<-shiny::reactive({
            rvTab$tgt
        })

        getUnk<-shiny::reactive({
            rvTab$unk
        })

        

        update_tags_mzMLtab<-shiny::reactive({
            input$tagsInp
            tags<-getTags()
            tagCol<-rvTab$mzML$tag
            if (length(levels(tagCol))==0) rvTab$mzML$tag<-factor(tagCol)
            rvTab$mzML$tag<-factor(tagCol,levels=tags)
        })

        update_sets_mzMLtab<-shiny::reactive({
            sets<-getSets()
            setCol<-rvTab$mzML$set
            if (length(levels(setCol))==0) rvTab$mzML$set<-factor(setCol)
            rvTab$mzML$set<-factor(setCol,levels=sets)
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
                sav$input$fnTgtL<-input$fnTgtL
                sav$input$fnUnkL<-input$fnUnkL
                sav$input$fnSetId<-input$fnSetId
                sav$input$fnStgsRMB<-input$fnStgsRMB
                sav$tab<-list()
                for (nm in names(rvTab)) {
                    df<-rvTab[[nm]]
                    sav$tab[[nm]]<-df
                }
                
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

                for (nm in rvConf$REST_TXT_INP) {
                    shiny::updateTextInput(session=session,
                                           inputId=nm,
                                           value=sav$input[[nm]])
                }
                rvConf$fnFTBase<-sav$rvConf$fnFTBase
                rvConf$fnFT<-sav$rvConf$fnFT
                for (nm in names(rvTab)) {
                    rvTab[[nm]]<-sav$tab[[nm]]
                }
            }
        })


        currSetMkCmpMenu<-shiny::reactive({
            set<-rvConf$currSet
            md<-rvConf$currMode
            setID<-getSetId()
            comp<-getComp()
            if (!is.na(set) && !is.null(comp) && !is.na(md)) {
                comp<-comp[comp$set %in% set,]
                comp<-comp[comp$mode %in% md,]
                ids<-comp$ID #[set %in% comp$set]
                entries<-mkCmpdDrop(set,comp)
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
            fTab<-rvTab$mtr

            if (!is.na(set) && length(fTab)>0) {
                comp<-getComp()
                if (!is.null(comp)) {
                    compIds<-comp[,"ID"]
                    compSMILES<-comp[,"SMILES"]
                    compMz<-comp[,"mz"]
                    tags<-rvConf$tags
                    iSet<-which(set==fTab$set)
                    sfTab<-fTab[iSet,]
                    tags<-levels(factor(sfTab$tag))
                    iTag<- match(tags,sfTab$tag)
                    wd<-sfTab$wd[iTag]
                    
                    preID<-compIds
                    smiles<-compSMILES
                    mz<-compMz
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

        shiny::observeEvent(input$impGenRMBB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impGenRMBB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="fnStgsRMB",
                                       value=fn)
            }})

        shiny::observeEvent(input$mzMLtabSubmB,{
                mzML<-rhandsontable::hot_to_r(input$mzMLtabCtrl)
                rvTab$mzML<-mzML
                rvConf$flMzMLSub<-T
        })

        shiny::observeEvent(input$fnTgtL,
        {
            fn<-input$fnTgtL
            if (isThingFile(fn)) {
                message("Importing targets list from:",fn)
                df<-file2tab(file=fn)
                rvTab$tgt<-df
                message("Done importing targets list from: ",fn)

            }
        })


        shiny::observeEvent(input$fnUnkL,
        {
            fn<-input$fnUnkL
            if (isThingFile(fn)) {
                message("Importing unknowns list from:",fn)
                df<-file2tab(file=fn)
                rvTab$unk<-df
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
            df<-rvTab$setId
            if (length(df)>0 && !is.na(df) && nrow(df)>0) {
                shiny::updateSelectInput(session=session,
                                         inputId="genSetSelInp",
                                         choices=levels(df$set))
            }
            ## shiny::isolate({
            ##     message("Changing the inpSetIdFn to: ",fn, " in isolation.")
            ##     shiny::updateTextInput(session=session,
            ##                            inputId="fnSetId",
            ##                            value=fn)
            ## })
        })

        shiny::observeEvent(input$mzMLB,
        {
            rvConf$flMzMLSub<-F
            rvTab$mzML<-getMzMLFiles()

        })

        ## shiny::observeEvent(rvTab$tmpMzML,
        ## {
            
        ## })

        

        shiny::observeEvent(input$genFileTabB,{
            fn<-input$confFileTabBase
            if (length(fn)>0 && !is.na(fn) && nchar(fn)>0) {
                message("Generating basic file table in file ",fn)
                files<-adornmzMLTab(rvTab$mzML,projDir=rvConf$projDir)
                comp<-rvTab$comp
                df<-genSuprFileTab(files,comp)
                df<-addCompColsToFileTbl(df,comp)
                df$mode<-as.character(df$mode)
                tab2file(tab=df,file=fn)
                rvConf$fnFTBase<-fn
                message("Done generating basic file table in file ",fn)
            }

        })

        shiny::observeEvent(input$genRunB,{
            FnRMB<-input$fnStgsRMB
            nProc<-as.integer(input$genNoProc)
            fnTab<-rvConf$fnFTBase
            sets<-input$genSetSelInp
            message("Selected sets:")
            message("Number of processes:",nProc)
            message("RMassBank settings file:",FnRMB)
            message("File table:",fnTab)
            if (length(fnTab)>0) {

                fTab<-file2tab(file=fnTab)
                for (s in sets) {
                    message("***** BEGIN set ",s, " *****")
                    fnCmpdList<-input$fnTgtL
                    fnStgs<-FnRMB
                    intTresh<-as.numeric(input$intTresh)
                    noiseFac<-as.numeric(input$noiseFac)
                    rtDelta<-as.numeric(input$rtDelta)
                    ppmLimFine<-as.numeric(input$ppmLimFine)
                    dest<-rvConf$projDir
                    eicLim<-as.numeric(input$eicLim)
                    gc()
                    gen(fTab=fTab[fTab$set==s,],
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

            sets<-getSets()
            comp<-rvTab$comp
            
            nr<-nrow(comp)
            if (!is.null(nr)) {
                if (is.na(nr)) nr<-0
            } else nr<-0
            if (length(sets)>0 && nr>0) {
                doneSets<-sets[sapply(sets,isGenDone)]
                message("done sets: ",doneSets)
                if (length(doneSets)>0) {
                    fnFullTab<-input$confFileTabProcInp
                    fnBaseTab<-input$confFileTabBase
                    fnOpen<-""
                    fnOpen<-if (isThingFile(fnFullTab)) fnFullTab else fnBaseTab
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
                        comp<-rvTab$comp
                        intTresh<-as.numeric(input$intTresh)
                        noiseFac<-as.numeric(input$noiseFac)
                        rtDelta<-as.numeric(input$rtDelta)
                        preProc(fnFileTab=fnTmp,
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
            if (!is.null(fn) && isThingFile(fn)) {
                rvTab$mtr<-read.csv(file=fn,
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
            rvTab$mtr <- updateFileTable(df=rvTab$mtr,
                                           set=input$presSelSet,
                                           id=rvConf$currID,
                                           linput=res)
        })
        
        shiny::observeEvent(input$savefiletable,
        {
            fn<-input$fn_ftable
            message("Writing current file table to ",fn)
            write.csv(file=fn,x=rvTab$mtr,row.names = F)
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


        shiny::observeEvent(input$presSelMode,
        {
            
            if (!is.na(rvConf$currSet)) rvConf$currMode<-input$presSelMode})

        
        ## ***** Observe *****

        shiny::observe({
            setId<-getSetId()
            mzML<-getMzML()
            unk<-getUnk()
            tgt<-getTgt()
            isSubm<-rvConf$flMzMLSub
            cond<-! (is.null(tgt) && is.null(unk)) &&
                ! is.null(setId) &&
                ! is.null(mzML) &&
                isSubm
            
            if (cond) {

                message("Begin generation of comp table.")

                idTgt<-tgt$ID
                idUnk<-unk$ID

                if (length(intersect(idTgt,idUnk))>0) stop("There must not be unknowns and targets with the same IDs.")
                setId$orig<-rep("",nrow(setId))
                lTgt<-setId$ID %in% idTgt
                iTgt<-which(lTgt)
                iUnk<-which(!lTgt)
                setId[iTgt,"orig"]<-"known"
                setId[iUnk,"orig"]<-"unknown"
                rvTab$setId<-setId ## !!!

                
                ## knowns
                setIdTgt<-setId[setId$orig=="known",]
                sets<-levels(factor(setIdTgt$set))
                nRow<-0
                for (s in sets) {
                    sMode<-getSetMode(s,mzML)
                    n<-length(sMode)
                    nRow<-nRow+n*length(which(setIdTgt$set %in% s))
                    
                }

                compTgt<-data.frame(
                    ID=rep(0,nRow),
                    mz=rep(0.0,nRow),
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
                            compTgt[i,"SMILES"]<-sm
                            compTgt[i,"Name"]<-nm
                            i<-i+1
                        }
                        
                    }
                }

                message("Generation of comp table: knowns done.")
                ## unknows
                setIdUnk<-setId[setId$orig=="unknown",]
                sets<-levels(factor(setIdUnk$set))
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
                        compUnk[i,"Name"]<-nm
                        i<-i+1
                    }
                    
                }
                message("Generation of comp table: unknowns done.")
                df<-rbind(compTgt,compUnk,stringsAsFactors=F)
                rvTab$comp<-df
                tab2file(df,rvConf$fnComp)
                message("Generation of comp table finished.")
                
            }
            
                
        })
       
        
        #rvTab$mzML<-getMzMLFiles()


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
            fnFT<-if (isThingFile(input$confResFileTab)) input$confResFileTab else NULL
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

        shiny::observe(
        {
            comp<-getComp()
            currSet<-rvConf$currSet
            mzML<-getMzML()
            if (!(is.na(currSet) || is.null(comp))) {
                mds<-levels(factor(mzML$mode[mzML$set %in% currSet]))
                rvConf$currMode<-mds[[1]]
                shiny::updateSelectInput(session=session,
                                         "presSelMode",
                                         choices=mds,
                                         selected=mds[[1]])
            }
        })


        shiny::observe({
            i<-rvConf$currID
            spectProps<-rvConf$spectProps
            set<-input$presSelSet
            md<-input$presSelMode
            if (!is.na(i) && length(spectProps)>0 && !is.na(md)) {
                message("Updating QA checkboxes.")
                QANAMES<-rvConf$QANAMES
                sdf <- queryFileTable(df=rvTab$mtr,set=set,mode=md,id=i)
                sdf$tag<-as.character(sdf$tag)
                for (t in sdf$tag) {
                    sprop <- rvConf$spectProps[[t]]
                    sdfSel<-sdf[sdf$tag %in% t,QANAMES]
                    ## browser()
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
                ## cmpdL<-getCmpL()
                ## setID<-getSetId()
                rvConf$currSet<-set
            }
        })

        shiny::observe({
            message("currSet START")
            currSetMkCmpMenu()
            message("And here?")
            currSetPreCalc()
            message("currSet END")

        })

        ## ## shiny::observe({
        ## ##     rvConf$tags<-getTags()
        ## ## })

        ## shiny::observe({
        ##     message("Update tags and sets.")
        ##     update_sets_mzMLtab()
        ##     update_tags_mzMLtab()
        ##     message("Done updating tags and sets.")
        ## })


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
            rhandsontable::rhandsontable(rvTab$mzML,stretchH="all")
        })

        output$nvPanel<-shiny::renderUI({
            message("Rendering panel started")
            ft<-rvTab$mtr
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
                message("done rendering panel")
                do.call(shiny::navlistPanel, tabPanelList)
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
