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
                           buttonTxt="Set",
                           txtTxt="",
                           icon="file",
                           ...) {
        shinydashboard::box(title=title,
                            shiny::textInput(txtName,NULL,value=txtTxt),
                            shinyFiles::shinyFilesButton(buttonName,
                                                         label=buttonTxt,
                                                         title=buttonTxt,
                                                         icon=shiny::icon(icon),
                                                         multiple=T),
                            solidHeader=T,
                            collapsible=F,...)}
    
    confCompFnBrowse <- browseFile(title="Compound table file",
                                   txtName="compListInp",
                                   buttonName="compListB",
                                   width=NULL)

    confmzMLSets <- shinydashboard::box(title="Sets and tags",
                                        shiny::textInput("setPropInp",
                                                         "What is a set?",
                                                         value=""),
                                        shiny::textInput("setsInp",
                                                         "Comma-delimited list of set types",
                                                         value=""),
                                        shiny::textInput("tagPropInp",
                                                         "What is a tag?",
                                                         value=""),
                                        shiny::textInput("tagsInp",
                                                         "Comma-delimited list of tag types",
                                                         value=""),
                                        width=NULL)

    confState <- shinydashboard::box(title="Configuration state",
                                     shinyFiles::shinySaveButton("saveConfB",
                                                                 "Save configuration",
                                                                 title="Save",
                                                                 filename = "conf-state.rds",
                                                                 "rds"),
                                     shinyFiles::shinyFilesButton("restoreConfB",
                                                                  label="Restore configuration",
                                                                  title="Restore",
                                                                  multiple=F,
                                                                  icon=shiny::icon("file")),
                                     width=NULL)

    confLayout <- shiny::fluidRow(shiny::column(confCompFnBrowse,
                                                confmzMLSets,
                                                confState,
                                                width=4),
                                  shiny::column(width=8,
                                                shinydashboard::box(title="mzML file table",
                                                                    shinyFiles::shinyFilesButton("mzMLB",
                                                                                                 label="Select mzML files",
                                                                                                 title="Select mzML files",
                                                                                                 icon=shiny::icon("files"),
                                                                                                 multiple=T),
                                                                    rhandsontable::rHandsontableOutput("mzMLtabCtrl"),
                                                                    width=NULL)))


    headerText <- "Shinyscreen"

    

    confSideItem <- shinydashboard::menuItem(text="Config",
                                             tabName="config",
                                             icon=shiny::icon("dashboard"))
    
    compListSideItem <- shinydashboard::menuItem(text="Compound List",
                                                 tabName="compList",
                                                 icon=shiny::icon("dashboard"))

    presSideItem <- shinydashboard::menuItem(text="Prescreening",
                                             tabName="prescreen",
                                             icon=shiny::icon("dashboard"))
    
    header <- shinydashboard::dashboardHeader(title=headerText)
    sidebar <- shinydashboard::dashboardSidebar(confSideItem,
                                                compListSideItem,
                                                presSideItem)

    confTab <- shinydashboard::tabItem(tabName="config",
                                       shiny::h2("Config"),
                                       confLayout)
    compListTab <- shinydashboard::tabItem(tabName="compList",shiny::h2("Compound Table"))
    presTab <- shinydashboard::tabItem(tabName="prescreen",shiny::h2("Prescreening"))
    body <- shinydashboard::dashboardBody(shinydashboard::tabItems(confTab,
                                                                   compListTab,
                                                                   presTab))
    
    shinydashboard::dashboardPage(header,
                                  sidebar,
                                  body)}

##' @export
shinyScreenApp <- function() {

    volumes <- shinyFiles::getVolumes()
    
    mk_mzMLtab<-function() {data.frame(Files=character(),
                                        set=character(),
                                        tag=character(),
                                        stringsAsFactors=F)}
    extd_mzMLtab<-function(ft,fn) {
        as.data.frame(rbind(ft,
                            data.frame(Files=fn,
                                       set="",
                                       tag="",
                                       stringsAsFactors=F),
                            make.row.names=F,
                            stringsAsFactors = F),
                      stringsAsFactors=F)
    }

    server <- function(input,output,session) {
        rvConf <- shiny::reactiveValues(mzMLtab=mk_mzMLtab(),
                                        tags=list(),
                                        sets=list(),
                                        compListFn="")
        shinyFiles::shinyFileChoose(input, 'compListB',root=volumes)
        shinyFiles::shinyFileChoose(input, 'mzMLB',root=volumes)


        shiny::observe({
            input$compListB
            res<-shinyFiles::parseFilePaths(root=volumes,input$compListB)
            rvConf$compListFn<-res[["datapath"]]
            message("huh:",str(rvConf$compListFn))
        })
        
        output$compListInp<-shiny::renderText({
            "aaa"             ## rvConf$compListFn
        })

        shiny::observe({
            input$mzMLB

            fchoice<-shinyFiles::parseFilePaths(root=volumes,input$mzMLB)
            paths<-fchoice[["datapath"]]
            for (pt in paths) {
                isolate(rvConf$mzMLtab<-extd_mzMLtab(rvConf$mzMLtab,pt))
            }
        })

        shiny::observe({
            input$setsInp
            rvConf$sets <- if (length(input$setsInp)!=0) unlist(strsplit(input$setsInp, ",")) else list()
            if (length(rvConf$sets) != 0) {
                if (!is.factor(rvConf$mzMLtab$set)) {
                    rvConf$mzMLtab$set <- factor(rvConf$mzMLtab$set)
                }
                
                lvl<-levels(rvConf$mzMLtab$set)
                sets<-rvConf$sets
                if (length(sets) < length(lvl)) rvConf$mzMLtab$set<-as.character(rvConf$mzMLtab$set)
                levels(rvConf$mzMLtab$set)<-rvConf$sets
                 
            }
        })


        shiny::observe({
            input$tagsInp
            rvConf$tags <- if (length(input$tagsInp)!=0) unlist(strsplit(input$tagsInp, ",")) else list()
            if (length(rvConf$tags) != 0) {
                if (!is.factor(rvConf$mzMLtab$tag)) {
                    rvConf$mzMLtab$tag <- factor(rvConf$mzMLtab$tag)
                }
                
                lvl<-levels(rvConf$mzMLtab$tag)
                tags<-rvConf$tags
                if (length(tags) < length(lvl)) rvConf$mzMLtab$tag<-as.character(rvConf$mzMLtab$tag)
                levels(rvConf$mzMLtab$tag)<-rvConf$tags
                 
            }
        })




        output$mzMLtabCtrl <- rhandsontable::renderRHandsontable({
            if (nrow(rvConf$mzMLtab) !=0) rhandsontable::rhandsontable(rvConf$mzMLtab,stretchH="all")
        })
        
        session$onSessionEnded(function () stopApp())
    }

    shiny::shinyApp(ui=mkUI2(),server=server)
}
