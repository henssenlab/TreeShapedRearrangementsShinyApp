#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(circlize)
source("helpers.R")
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(div("Tree-shaped Rearrangement Patterns in Pediatric Cancer Genomes", 
                 style="margin-bottom:15px")),
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("dataset", 
                   label = "Dataset",
                   choices = list("Peifer et al. (2015)" = "peifer", "DKFZ Pediatric Pan Cancer Dataset" = "pedpancan", "Upload your own data" = "upload"), 
                   selected = "peifer"),
      
      sliderInput("slider_threshold", label = "Cluster Threshold (default: 3)", min = 2, 
                  max = 9, value = 3),
      
      conditionalPanel(
        condition = "input.dataset == 'upload'",
        p("Please upload a tab-separated text file that specifies the interchromosomal rearrangements in your dataset. Columns should be named Sample, ChrA, PosA, ChrB and PosB. Chromsome names should contain the chr-prefix."),
        fileInput("file1", "",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv,.txt,.tsv"))
      ),
      
      conditionalPanel(
        condition = "input.dataset == 'pedpancan'",
        selectizeInput(
          "entity", "Tumor Entity",
          choices = NULL
        )
      ),
      
      conditionalPanel(
        condition = "input.dataset == 'peifer'",
        selectizeInput(
          "svcaller", "Structural Variant Call Set",
          choices = c("Delly", "Smufin", "Novobreak", "Svaba", "Brass", "Union", "AtLeastTwo"),
          selected = "Union"
        )
      ),
      
      selectInput(
        "SampleSelectize", 'Sample',
        choices = NULL,
        multiple=TRUE, selectize=FALSE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Info",
                           div(imageOutput("circoswelcome"), align="center"),
                           fluidRow(
                             column(12, align="center",
                             p("This web application visualizes the data from Koche R., Fos E. and Helmsauer K., et al. (under review). It was created to enable easy access to the data presented in the manuscript. Furthermore, it allows users to identify similar rearrangement patterns in structural variants detected in their own datasets.") 
                           ))),
                  tabPanel("Circos Plots",
                           fluidRow(
                             style = "padding-top:30px",
                             plotOutput("circos", width = "100%", height="500px")
                           )),
                  tabPanel("Rearrangements", 
                           fluidRow(
                             style = "padding-top:30px",
                             column(12, align="center",
                                    dataTableOutput("tx_table")))),
                  tabPanel("Cluster Regions",
                           fluidRow(
                             style = "padding-top:30px",
                             column(12,align="center",
                                    uiOutput("palmtrees_table_with_msg")
                             ))),
                    tabPanel("References",
                                      fluidRow(
                                        style = "padding-top:30px",
                                        p(tags$b("Datasets"), br(),
                                          "Peifer, M. ", tags$em("et al."), " Telomerase activation by genomic rearrangements in high-risk neuroblastoma. ", tags$em("Nature "), tags$b("526"),", 700-704, doi:10.1038/nature14980 (2015).", br(),
                                          "Gröbner, S. N. ", tags$em("et al."), " The landscape of genomic alterations across childhood cancers. ", tags$em("Nature "), tags$b("555"), ", 321-327, doi:10.1038/nature25480 (2018).", br(), br(),
                                          tags$b("Structural Variant Callers"), br(),
                                          "Rausch, T. ", tags$em("et al."), " DELLY: structural variant discovery by integrated paired-end and split-read analysis. ", tags$em("Bioinformatics "), tags$b("28"), ", i333-i339, doi:10.1093/bioinformatics/bts378 (2012).", br(),
                                          "Moncunill, V. ", tags$em("et al."), " Comprehensive characterization of complex structural variations in cancer by directly comparing genome sequence reads. ", tags$em("Nat Biotechnol "), tags$b("32"), ", 1106-1112, doi:10.1038/nbt.3027 (2014).", br(), 
                                          "Chong, Z. ", tags$em("et al."), " novoBreak: local assembly for breakpoint detection in cancer genomes.", tags$em("Nat Methods "), tags$b("14"), ", 65-67, doi:10.1038/nmeth.4084 (2017).", br(),
                                          "Wala, J. A. ", tags$em("et al."), " SvABA: genome-wide detection of structural variants and indels by local
                                          assembly. ", tags$em("Genome Res "), tags$b("28"), ", 581-591, doi:10.1101/gr.221028.117 (2018).", br(),
                                          "https://github.com/cancerit/BRASS.", br()
                                          )
                                        
                                      ))
      )
    )
  ),
  hr(),
  p(
  "DOI ", a("10.5281/zenodo.2634261", href="https://zenodo.org/badge/latestdoi/180385829"),
  br(),
  "Code is available at ", a("github.com/henssenlab/TreeShapedRearrangementsShinyApp", href="https://www.github.com/henssenlab/TreeShapedRearrangementsShinyApp"),
  br(),
  "Contact: ", a("henssenlab@gmail.com", href="mailto:henssenlab@gmail.com")
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  pedpancan_meta = read.delim("PedPanCanMeta.csv", sep=";", header=T) %>% 
    filter(seq_type == "wgs") %>% mutate(entity = entity_long)
  entity_names = pedpancan_meta$entity %>% as.character() %>% unique()
  
  observe({
    if (input$dataset == "pedpancan"){
      updateSelectizeInput(
        session,
        "entity",
        label = "Tumor Entity",
        choices = entity_names
      )
    }
  })
  
  get_data = reactive({
    if (input$dataset == "upload"){
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      data = read.table(inFile$datapath, header = TRUE, sep="\t")
    }
    if (input$dataset == "pedpancan"){
      if (is.null(input$entity)) return(NULL)
      data = read.table("pedpancan_wgs_txcalls.tsv", header = TRUE, sep="\t") %>%
        filter(Sample %in% (pedpancan_meta %>% filter(entity == input$entity) %>% .$sample %>% unique()))
    }
    if (input$dataset == "peifer"){
      if (is.null(input$svcaller)) return(NULL)
      data = read.table("peifer_tx.tsv", header = TRUE, sep="\t") %>%
        filter(SVCaller == input$svcaller) %>%
        dplyr::select(Sample, ChrA, PosA, ChrB, PosB)
    }
    return(data)
  })
  
  samples = reactive({
    t = get_data()
    if (is.null(t)) return(NULL)
    return(t %>% .$Sample %>% as.character() %>% unique())
  })
  
  tx = reactive({
    tx = get_data()
    if (is.null(tx)) return(NULL)
    sample_choice = input$SampleSelectize
    if (is.null(sample_choice)) return(NULL)
    return(tx %>% mutate(Sample = as.character(Sample)) %>% filter(Sample == sample_choice))
  })
  
  observe({
    updateSelectInput(
      session,
      "SampleSelectize",
      label = "Sample",
      choices = samples()
    )
  })
  
  palmtrees = reactive({
    t = tx()
    k = input$slider_threshold
    if (is.null(t)) return(NULL)
    pt = helpers_callPalmTrees(t, k)
    return(pt)
  })
  
  output$tx_table <- renderDataTable({
    t = tx()
    if (is.null(t)) return(NULL)
    return(t %>% data.frame())
  }, 
  options = list(pages=-1))
  
  output$palmtrees_table <- renderDataTable({
    pt = palmtrees()
    if (is.null(pt)) return(NULL)
    if (nrow(pt$palmtrees)==0) return(NULL)
    pt_table = pt$palmtrees %>% data.frame() %>% 
      mutate(Start=FirstElement%>%sprintf("%.0f",.), End=LastElement%>%sprintf("%.0f",.)) %>%
      select(Sample, Chr, Start, End) %>%
      mutate(Link = createUCSCLinkFromRegion(Chr, Start, End))
    return(pt_table)
    },
    escape=FALSE)
  
  output$palmtrees_table_with_msg = renderUI({
    pt = palmtrees()
    if (is.null(pt)) return(NULL)
    if(nrow(pt$palmtrees) == 0)
      return("No palm trees can be identified for this sample.")
    dataTableOutput("palmtrees_table")
  })
  
  output$circos <- renderPlot({
    pt = palmtrees()
    if (is.null(pt)) return(NULL)
    if (is.null(input$SampleSelectize)) return(NULL)
    helpers_circos(pt$palmtrees, pt$txptinfo, pt$txdouble, pt$tx_original, input$SampleSelectize)
  })
  
  output$circoswelcome <- renderImage({
    filename <- normalizePath("./NB2013UnionPalmTreeCircosRed.png")

    # Return a list containing the filename and alt text
    list(src = filename,
         contentType = 'image/png',
         width=400,
         height=400)

  }, deleteFile = FALSE)

}

# Run the application 
shinyApp(ui = ui, server = server)

