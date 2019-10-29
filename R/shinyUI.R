mkUI <- function(idSliderRange,setName,rtRange,tags,QANms) {

    
    names(QANms) <- QANms
    ## Elements
    tabPanelList <- lapply(tags, function(tag) {
        shiny::tabPanel(tag, shiny::checkboxGroupInput(paste("spectProps",tag,sep=""), "Quality Control",
                                                       QANms),
                        shiny::textAreaInput(paste("caption",tag,sep=""), "Comments:", "Insert your comment here..."),
                        shiny::verbatimTextOutput(paste("value",tag,sep=""))
                        )})
    
    nvPanel <- do.call(shiny::navlistPanel, tabPanelList)

    ## Prescreening elements
    preshead <- shinydashboard::dashboardHeader(title = "Prescreening")
    presMenuItem <- shinydashboard::menuItem(text = "The Prescreening",
                                             tabName = "Prescreen",
                                             icon = shiny::icon("dashboard"))
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
                                       shiny::plotOutput("plot1",
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
                                          shiny::sliderInput("idslider",
                                                             "Compound number:",
                                                             idSliderRange[1],
                                                             idSliderRange[2],
                                                             value=1,
                                                             step=1))
    
    presQABox <- shinydashboard::box(title = "Prescreening analysis",
                                     width = 5,
                                     solidHeader = FALSE,
                                     collapsible = TRUE,
                                     shiny::titlePanel(setName),
                                     nvPanel,
                                     shiny::actionButton("submitQA",
                                                         "Submit",
                                                         icon = shiny::icon("save")),
                                     shiny::textInput("fn_ftable",
                                                      "File table Name",
                                                      value="ftable.csv"),
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
                                                              rtRange[1]),
                                          shiny::numericInput("max_val",
                                                              "Maximum x Axis Value",
                                                              rtRange[2]),
                                          shiny::radioButtons("yaxis",
                                                              "Parameters for y Axis",
                                                              c(linear = "linear",
                                                                log = "log")),
                                          shiny::numericInput("nice",
                                                              "Nice",
                                                              rtRange[1]),
                                          shiny::numericInput("steps",
                                                              "Steps",
                                                              rtRange[2]))
    presPlotWidget <- shiny::fluidRow(presPlotBox,
                                      presCompSelBox,
                                      presQABox,
                                      presPlotParBox)
    presTabItem <- shinydashboard::tabItem(tabName = "Prescreen",
                                           shiny::h2("The Prescreen plot"),
                                           presCompInfo,
                                           presPlotWidget)

    ## Assemble the UI.
    ui <- shinydashboard::dashboardPage(skin="black",
                                        presHead,
                                        shinydashboard::dashboardSidebar(width = 350,
                                                                         shinydashboard::sidebarMenu(presMenuItem)),
                                        shinydashboard::dashboardBody(shinydashboard::tabItems(presTabItem)))}

##' Prescreening using shiny interface.
##'
##' @title Prescreening with Shiny 
##' @return Nothing useful. 
##' @author Jessy Krier
##' @author Mira Narayanan
##' @author Hiba Mohammed Taha
##' @author Anjana Elapavalore
##' @author Todor Kondić
##' @param prescdf File table data-frame. Columns: Files,ID,wd,tag,set_name ...
##' @param mode RMassBank mode.
##' @param fn_cmpd_l Compound list file name.
##' @param pal ColorBrewer palette.
##' @param cex Size of fonts.
##' @param rt_digits Number of decimal places for the retention time.
##' @param m_digits Number of decimal places for the mass.
##' @export
presc.shiny <-function(prescdf=NULL,mode=NULL,fn_cmpd_l=NULL,pal="Dark2",cex=0.75,rt_digits=2,m_digits=4){
    ## Helper functions
    
    queryFileTable <- function(df,id) {
    df[df$ID %in% id,]
    }


    updateFileTable <- function(df,id,linput) {
        for (tag in names(linput)) {
            entries <- names(linput[[tag]])
            cond <- (df$ID %in% id) & (df$tag == tag)
            df[cond,entries] <- linput[[tag]]
        }
        df
    }

    ## Constants
    MODEMAP=list(pH="MpHp_mass",
                 mH="MmHm_mass",
                 blahnh4="MpNH4_mass",
                 blahna="MpNa_mass")
    DEFAULT_RT_RANGE=c(NA,NA)

    QANAMES <- c("MS1","MS2","Alignment","AboveNoise")

    prescdf$tag <- as.character(prescdf$tag)
    tags <- levels(factor(prescdf$tag))
    wd <- prescdf$wd[match(tags,prescdf$tag)]

    wd1 <- wd[[1]]
    cmpd_l_df <- read.csv(file=fn_cmpd_l,stringsAsFactors = F,comment.char='')
    preID <- as.integer(levels(factor(prescdf$ID)))
    selID <- which(cmpd_l_df$ID %in% preID)
    osmesi <- cmpd_l_df$SMILES[selID]
    no_cmpds <- length(preID)
    # reconf(wd1)
    masses <- lapply(osmesi,function (smile) {
        #osmesi <- tryCatch(RMassBank::findSmiles(i), error = function(e) NA)
        zz <- RChemMass::getSuspectFormulaMass(smile)
        zz[[MODEMAP[[mode]]]]
    })

    
    names(osmesi) <- as.character(preID)
    names(masses) <- as.character(preID)

    ## Get the basenames of eic files.
    eics <- list.files(path=wd[[1]],patt=".*eic.csv")
    eicsPref <- sapply(strsplit(eics,split="\\."),function(x) x[[1]])
    eicsID <- as.integer(eicsPref)
    maybekids <- sapply(eicsPref,function(x) {paste(x,'.kids.csv',sep='')})
    names(eics) <- eicsID
    names(maybekids) <- eicsID

    plot_id <- function (i,rtrange=NULL,log=rv$yaxis) plot_id_aux(i=as.character(i),wd=wd,eics=eics,maybekids=maybekids,mass=masses[[as.character(i)]],smile=osmesi[[as.character(i)]],tags=tags,log=log,rtrange=rtrange,cex=cex,pal=pal,rt_digits=rt_digits,m_digits=m_digits,fTab=prescdf)

    spectProps <- sapply(tags,function (tag) paste("spectProps",tag,sep=""))
    idSliderRange <- c(1,length(preID))
    

    
    ui <- mkUI(idSliderRange=idSliderRange,setName=prescdf$set_name,rtRange=DEFAULT_RT_RANGE,tags=tags,QANms=QANAMES)


    

    getCheckboxValues <- function(tag,input) {
        chkbox <- input[[spectProps[[tag]]]]
        q <- sapply(QANAMES,function (qn) if (qn %in% chkbox) T else F)
        names(q) <- QANAMES
        q
    }
    
    server <- function(input, output, session) {
        rv <- shiny::reactiveValues(prescList=list(),
                                    prescdf=prescdf,
                                    spectProps=spectProps,
                                    tags=tags,
                                    default_range=DEFAULT_RT_RANGE,
                                    no_cmpds=no_cmpds)

        output$plot1 <- renderPlot(
        {
            i=preID[[input$idslider]]
            
          rtrange <- c(input$min_val,input$max_val)
          plot_id(i,rtrange=rtrange, log=input$yaxis)

                                        #           intParameter <- c(input$nice, input$steps)
                
        })

        output$value <- renderText(
        {
            input$caption
        })

        output$compoundID <- renderText(
        {
            preID[[input$idslider]]
        })
        shiny::observeEvent(input$saveplot,
        {
            i=preID[[input$idslider]]
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotCpdID_%i.pdf"
            fn <- sprintf(pfn,i)
            rtrange <- c(input$min_val,input$max_val)
            pdf(file=fn, width=12, height=8)
            plot_id(i,rtrange=rtrange, log=input$yaxis)
            dev.off()
        })

        shiny::observeEvent(input$saveallplots,
        {
            i=preID[[input$idslider]]
            pfn <-input$plotname
            if (is.na(pfn)) pfn <- "plotall.pdf"
            fn <- sprintf(pfn,i)
            pdf(file=fn, width=12, height=8)
            for (i in preID) {
                plot_id(i,log=input$yaxis)
                message("Compound ID ",i," done.")
            }
            dev.off()
        })

        shiny::observeEvent(input$submitQA,{
            res <- lapply(rv$tags,getCheckboxValues,input)
            names(res) <- rv$tags
            rv$prescdf <- updateFileTable(df=rv$prescdf,id=preID[[input$idslider]],linput=res)
        })

        shiny::observe({
            i <- preID[[input$idslider]]
            sdf <- queryFileTable(df=rv$prescdf,id=i)
            for (t in sdf$tag) {
                sprop <- rv$spectProps[[t]]
                sel <- as.logical(sdf[sdf$tag %in% t,QANAMES])
                choices <- QANAMES[sel]
                names(choices) <- QANAMES[sel]
                shiny::updateCheckboxGroupInput(session = session,inputId = sprop,selected=choices)
            }
        })

        shiny::observeEvent(input$savefiletable,
        {
            write.csv(file=input$fn_ftable,x=rv$prescdf,row.names = F)
            
        })

        session$onSessionEnded(function() {
            stopApp()
        })
    }
    
    shiny::shinyApp(ui = ui, server = server)
}

mkUI2 <- function() {
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
                                      shiny::h5("There are two tables that need to be supplied before prescreening starts. One is the compound list, its format being the same like the one for the RMassBank (fields: ID,Name,SMILES,RT,CAS,mz,Level). Another is the compound set table (fields: ID,set). Once those tables are imported, they can further be modified as copies inside the project dir. Shinyscreen will never modify any initial (meta)data. If set field of the compound set table is NA, then that file is in a set of its own."),
                                      shiny::textInput("impCmpListInp",
                                                       "Compound list.",
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

    confmzMLtab <-shinydashboard::box(title="mzML file table",
                                      shiny::h5("Use this file table to assign adduct modes and tags to the data files."),
                                      shinyFiles::shinyFilesButton("mzMLB",
                                                                   label="Select mzML files",
                                                                   title="Select mzML files",
                                                                   icon=shiny::icon("files"),
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
                                       confLayout)

    ## ***** Compound List Tab *****

    cmpListBox<-shinydashboard::box(title="Compound list",
                                    rhandsontable::rHandsontableOutput("cmpListCtrl"),
                                    width=NULL)

    cmpListState <- shinydashboard::box(title="Compound list state",
                                     shinyFiles::shinySaveButton("saveCmpListB",
                                                                 "Save",
                                                                 title="Save",
                                                                 filename = "compounds.csv",
                                                                 "csv"),
                                     shinyFiles::shinyFilesButton("restoreCmpListB",
                                                                  label="Restore",
                                                                  multiple=F,
                                                                  title="Restore"),
                                     width=NULL)
    
    cmpListLayout <- shiny::fluidRow(shiny::column(cmpListState,
                                                   cmpListBox,
                                                   width = 12))

    cmpListTab <- shinydashboard::tabItem(tabName="compList",
                                          shiny::h5("This is an editable view of the compound list."),
                                          cmpListLayout)

    ## ***** Sets of compounds *****

    setIdBox<-shinydashboard::box(title="Compound sets",
                                  rhandsontable::rHandsontableOutput("setIdTabCtrl"),
                                  width = NULL)

    setIdBoxState<-shinydashboard::box(title="Compound list state",
                                     shinyFiles::shinySaveButton("saveSetIdB",
                                                                 "Save",
                                                                 title="Save",
                                                                 filename = "compound_sets.csv",
                                                                 "csv"),
                                     shinyFiles::shinyFilesButton("restoreSetIdB",
                                                                  label="Restore",
                                                                  multiple=F,
                                                                  title="Restore"),
                                     width=NULL)

    setIdLayout<-shiny::fluidRow(shiny::column(setIdBoxState,
                                               setIdBox,
                                               width = 12))

    setIdTab<-shinydashboard::tabItem(tabName="setId",
                                      shiny::h5("This is an editable view of the id/set list."),
                                      setIdLayout)

    ## ***** Generate Prescreen Data *****

    genBox<-shinydashboard::box(title="Produce file table",
                                shinyFiles::shinySaveButton("genFileTabB",
                                                            "Generate File table.",
                                                            "Generate File table.",
                                                            filename = "ftable.csv",
                                                            "csv"),
                                width=NULL)

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
                                     width=NULL) #TODO more boxes


    genTab<-shinydashboard::tabItem(tabName = "gen",
                                    shiny::h5("Prepare for prescreening."),
                                    shiny::fluidRow(shiny::column(genBox,width=4)),
                                    shiny::fluidRow(shiny::column(genBoxParam1,width=4)),
                                    shiny::fluidRow(shiny::column(genBoxParam2,width=4)))

    ## ***** Prescreening *****

    presTab <- shinydashboard::tabItem(tabName="prescreen")

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

    readCmpList<-function(fn) {
        read.csv(file=fn,
                 header=T,
                 stringsAsFactors = F,
                 comment.char = '')
    }

    readSetId<-function(fn) {
        read.csv(file=fn,
                 header=T,
                 stringsAsFactors = T,
                 comment.char = '',
                 na.strings = c("","NA"))
    }


    server <- function(input,output,session) {

        ## ***** reactive values *****
        rvConf <- shiny::reactiveValues(mzMLtab=mk_mzMLtab(),
                                        tags=list(),
                                        sets=list(),
                                        impCmpListFn="",
                                        impGenRMBFn="",
                                        FileTabFn="",
                                        tagProp="",
                                        setProp="",
                                        mode=modeLvl,
                                        freshCmpListInp=F,
                                        freshSetIdInp=F,
                                        projDir=projDir)

        rvCmpList<- shiny::reactiveValues(df=mk_cmpList())
        rvSetId<- shiny::reactiveValues(df=mk_setId())

        ## ***** shinyFiles observers *****
        wdroot<-c(wd=projDir)
        shinyFiles::shinyFileChoose(input, 'impCmpListB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impSetIdB',roots=volumes)
        shinyFiles::shinyFileChoose(input, 'impGenRMBB',roots=volumes)
        
        shinyFiles::shinyFileSave(input, 'saveConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'restoreConfB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'mzMLB',roots=volumes)

        shinyFiles::shinyFileSave(input, 'saveCmpListB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'restoreCmpListB',roots=projDir)

        shinyFiles::shinyFileSave(input, 'saveSetIdB',roots=wdroot)
        shinyFiles::shinyFileChoose(input, 'restoreSetIdB',roots=wdroot)
        shinyFiles::shinyFileSave(input, 'genFileTabB',roots=wdroot)


        ## ***** reactive function definitions *****
        
        getTags<-shiny::reactive({
            if (length(input$tagsInp)>0 && !is.na(input$tagsInp)) unlist(strsplit(input$tagsInp, ",")) else list()
        })

        getSets<-shiny::reactive({
            levels(rvSetId$df$set)
        })

        update_tags_mzMLtab<-shiny::reactive({
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
            if (length(fn)>0 && !is.na(fn)) {
                message("Restoring config from",fn)

                shiny::isolate({
                    sav<-readRDS(fn)
                    for (nm in names(sav$rvConf)) {
                        rvConf[[nm]]<-sav$rvConf[[nm]]
                    }

                    #FIXME: remove this, once local saves/restores supported
                    rvConf$freshCmpListInp<-T
                    rvConf$freshSetIdInp<-T
                    
                    shiny::updateTextInput(session=session,
                                           inputId="tagsInp",
                                           value=sav$input$tagsInp)
                    shiny::updateTextInput(session=session,
                                           inputId="impCmpListInp",
                                           value=sav$input$impCmpListInp)
                    shiny::updateTextInput(session=session,
                                           inputId="impGenRMBInp",
                                           value=sav$input$impGenRMBInp)
                    shiny::updateTextInput(session=session,
                                           inputId="impSetIdInp",
                                           value=sav$input$impSetIdInp)
                })
            }
        })

        getCmpListdf<-shiny::reactive({rvCmpList$df})

        importCmpListdf<-shiny::reactive({
            impCmpFn<-input$impCmpListInp
            if (length(impCmpFn)>0 && !is.na(impCmpFn)) {
                message("Importing compound list from ",impCmpFn)
                rvConf$impCmpListFn<-impCmpFn
                rvConf$freshCmpListImp<-T
                rvCmpList$df<-readCmpList(rvConf$impCmpListFn)
            }
        })
        
        importSetIddf<-shiny::reactive({
            impSetIdFn<-input$impSetIdInp
            if (length(impSetIdFn)>0 && !is.na(impSetIdFn)) {
                message("Importing setid table from ",impSetIdFn)
                rvConf$impSetIdFn<-impSetIdFn
                rvConf$freshSetIdImp<-T
                rvSetId$df<-readSetId(rvConf$impSetIdFn)
            }
        })


        shiny::observe({
            input$mzMLB

            fchoice<-shinyFiles::parseFilePaths(root=volumes,input$mzMLB)
            paths<-fchoice[["datapath"]]
            isolate({
                for (pt in paths) {
                    rvConf$mzMLtab<-extd_mzMLtab(rvConf$mzMLtab,pt)
                }

            })
        })

        shiny::observeEvent(input$mzMLtabCtrl,{
            input$cmpListCtrl
            input$setIdTabCtrl
            shiny::isolate({rvConf$mzMLtab<-rhandsontable::hot_to_r(input$mzMLtabCtrl)})
            
        })


        shiny::observeEvent(input$cmpListCtrl,{
            shiny::isolate({rvCmpList$df<-rhandsontable::hot_to_r(input$cmpListCtrl)})
        })

        shiny::observeEvent(input$setIdTabCtrl,{
            df<-rhandsontable::hot_to_r(input$setIdTabCtrl)
            rvSetId$df<-df
            shiny::updateSelectInput(session=session,
                                     inputId="genSetSelInp",
                                     choices=levels(df$set))})

        shiny::observeEvent(input$restoreConfB,{
            message("Restore event observed.")
            restoreConf()
        })

        shiny::observeEvent(input$saveConfB,{
            saveConf()
        })


        shiny::observeEvent(input$genFileTabB,{
            fn<-shinyFiles::parseSavePath(root=c(wd=rvConf$projDir),input$genFileTabB)[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                message("Saving file table to",fn)
                files<-adornmzMLTab(rhandsontable::hot_to_r(input$mzMLtabCtrl),projDir=rvConf$projDir)
                setId<-rhandsontable::hot_to_r(input$setIdTabCtrl)
                genSuprFileTbl(files,setId,destFn=fn)
                rvConf$FileTabFn<-fn
            }

        })

        shiny::observeEvent(input$genRunB,{
            FnRMB<-input$impGenRMBInp
            nProc<-as.integer(input$genNoProc)
            fnTab<-rvConf$FileTabFn
            sets<-input$genSetSelInp
            message("Selected sets:")
            message(str(sets))
            message("Number of processes:",nProc)
            message("RMassBank settings file:",FnRMB)
            message("File table:",fnTab)
            if (length(fnTab)>0) {
                ## fTab<-read.csv(file=fnTab,
                ##                row.names=F,
                ##                heander=T,
                ##                comment.char='',
                ##                na.strings=c("","NA"),
                ##                stringsAsFactors=F)
                ## mask<-fTab$set %in% sets
                ## fTab<-fTab[mask,]

                for (s in sets) {
                    message("***** BEGIN set",s, " *****")
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
                    message("***** END set",s, " *****")
                }}
        })
        
        output$mzMLtabCtrl <- rhandsontable::renderRHandsontable({
            rvConf$mzMLtab
            update_tags_mzMLtab()
            update_sets_mzMLtab()
            if (nrow(rvConf$mzMLtab) !=0) rhandsontable::rhandsontable(rvConf$mzMLtab,stretchH="all") else NULL
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

        shiny::observeEvent(input$impGenRMBB,{
            fnobj<-shinyFiles::parseFilePaths(roots=volumes,input$impGenRMBB)
            fn<-fnobj[["datapath"]]
            if (length(fn)>0 && !is.na(fn)) {
                shiny::updateTextInput(session=session,
                                       inputId="impGenRMBInp",
                                       value=fn)
            }})
        shiny::observe({
            input$impGenRMBInp
            input$impSetIdInp
            input$impCmpListInp
        })

        output$cmpListCtrl <- rhandsontable::renderRHandsontable({
            importCmpListdf()
            df<-rvCmpList$df #getCmpListdf()
            if (rvConf$freshCmpListImp) {
                shiny::isolate({
                    rvConf$freshCmpListImp<-F
                })
            }
            rhandsontable::rhandsontable(df,stretchH="all")
        })

        output$setIdTabCtrl<- rhandsontable::renderRHandsontable({
            importSetIddf()
            df<-rvSetId$df
            if (rvConf$freshSetIdInp) {
                shiny::isolate({
                    rvConf$freshSetIdInp<-F
                })
            }
            rhandsontable::rhandsontable(df,stretchH="all")
        })
        

        
        session$onSessionEnded(function () stopApp())
    }

    shiny::shinyApp(ui=mkUI2(),server=server)
}
