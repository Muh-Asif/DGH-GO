library(shiny)
library(shinythemes)
library(sparkline)
library(timevis)
library(DT)
library(shinycssloaders)
library(fontawesome)
library(shinydashboard)
library(ggvenn)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(org.Hs.eg.db)
library(GOSemSim)
library(tidyr)
library(MASS)
library(Rtsne)
library(cluster)
library(MASS)
library(vegan)
library(mclust)
library(tidyr) 
library(plyr)
library(MASS)
library(Rtsne)
library(clValid)
library("cluster")
library("factoextra")
library("magrittr")
library(cluster)
library(MASS)
library(vegan)
library(factoextra)
library(fpc)
library(tidyr)
library(dplyr)
library(plotly)
library(shinyalert)



ui <-dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody( 
    
    fluidPage(
      
      
      # define theme ####
      theme = shinytheme("lumen"),
      # use custom css #### 
      
      tags$head(
        tags$link(href = "style.css", rel = "stylesheet")
      ),
      
      titlePanel("DGH-GO: Disecting the genetic heterogeneity of complex diseases",
                 windowTitle = "Clustering based on functional similarties"),
      
      tabsetPanel(
        tabPanel(div(fa("notes-medical", fill = "#158cba"), "Import"),
                 
                 
          #to add the space
          fluidRow(tags$hr(), tags$hr()),

          fluidRow( 
            
            column(width=6,
                        box(title = "Import the input genes", height = NULL, width = NULL, solidHeader = TRUE, status =
                         "primary",
                            collapsible = T,

                            fileInput("inFile1", "Choose CSV File",
                              multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain",
                                                           ".csv")
                             ),
                           #textInput(inputId="geneCol", label="Genes column name", value = "", 
                            #         width = NULL, placeholder = NULL),
                           #textInput(inputId="disCol", label="Genes column name", value = "", 
                             #        width = NULL, placeholder = NULL),
                           # Horizontal line ----
                           tags$hr(),
                           # Input: Checkbox if file has header ----
                           checkboxInput("header", "Header", TRUE),
                           # Input: Select separator ----
                           radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", 
                                                                        Tab = "\t"), selected = ",", inline=TRUE),
                           
                           # Input: Select quotes ----
                           radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"
                              Single Quote" = "'"), selected = '"', inline=TRUE),
                           
                           # Horizontal line ----
                           tags$hr(),
                           # Input: Select number of rows to display ----
                           radioButtons("disp", "Display", choices = c(Head = "head", All = "all"),
                                        selected = "head", inline=TRUE)
                  
                        )#box
                        
                 ),
            column(width=6,
              box(title = "Input data", height = NULL, width = NULL, solidHeader = TRUE, status =
               "primary", 
                           collapsible = T,
                           
                           DT::dataTableOutput("outFile1"),
                           br(),
                           br()
                       )#box
          )

          
          ),# endo fo fluid row,#endo fo row

          fluidRow(
                     fluidRow(
                       
                       column(width = 6,
                              box(solidHeader = TRUE,title = "Histogram", collapsible = TRUE, collapsed = FALSE,
                                  height = NULL, width = NULL, status = "primary",
                                  plotlyOutput("histPlot") %>% withSpinner(color="#0dc5c1")
                              )
                       ),
                       column(width = 6,
                              box(solidHeader = TRUE,title = "Shared genes", collapsible = TRUE, collapsed = FALSE,
                                  height = NULL, width = NULL, status = "primary",
                                  plotOutput("venPlot") %>% withSpinner(color="#0dc5c1")
                              )
                              
                       )
                     )# end of row

          )# end of row
                 
                
                 
        ), #import tabpanle
        tabPanel(div(fa("syringe", fill = "#158cba"), "Semantic similarty matrix"),
                 
                 fluidRow(

                  column(width=12,
                        box(title = "Semnatic similarty measures", height = NULL, solidHeader = TRUE, status = "primary",
                            collapsible = T,

                            prettyRadioButtons(

                              inputId = "ssMethod",
                              label = "Select the semantic similarty method",
                              outline = TRUE, shape = "round", 
                              choices = c(Resnik="Resnik", Wang="Wang", Lin="Lin", Jiang="Jiang", Schlicker="Schlicker"),
                              inline = TRUE
                            ),
                            br(),
                       
                        #aggregating mtheod for genes
                       
                            prettyRadioButtons(
                              inputId = "ssMeasure",
                              label = "Select the gene semantic similarty measure",
                              outline = TRUE, shape = "round", 
                              choices = c(max="max", avg="avg", rcmax="rcmax", BMA="BMA"),
                              inline = TRUE
                            ),
                            br(),
                            actionButton("ssRun", "Run", class="btn Primary")
                  
                        )#box
                        
                 )
                ), #row for radio button
               fluidRow(
                 column(width=12,
                        conditionalPanel(condition = "input.ssRun",
                                         box(title = "Genes semnatic similarty matrix", 
                                             height = NULL, width = NULL, solidHeader = TRUE, status = "primary",
                                             collapsible = T,
                                             
                                             DT::dataTableOutput("ssm"),
                                             br(),
                                             br()
                                         )#box
                                         )#cp
                 )

               ) #fluidRow

        ), # SSM tab
        tabPanel(div(fa("syringe", fill = "#158cba"), 
                     "Visualization of similarty matrix with reduced dimensions"),
                 

                 fluidRow(
                   br(),
                   column(width=6,
                          box(solidHeader = TRUE,title = "Principal Coordinates Analysis Parameters", collapsible = TRUE, 
                              collapsed = TRUE,
                              height = NULL, width = NULL, status = "success",
                              numericInput("pcoa_num_dim", "Number of dimensions", 2, min = 1, max = 10),
                              uiOutput("pcoa_dim1"),
                              uiOutput("pcoa_dim2"),
                              
                              
                              br(),
                              actionButton("pcoa_para", "plot"),
                              br(),
                              br(),
                              
                              conditionalPanel(condition = "input.pcoa_para",
                                               #set parameters
                                               box(solidHeader = TRUE, title = "Principal Coordinates Analysis Plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "success",
                                                   plotlyOutput("plot_pcoa") %>% withSpinner(color="#0dc5c1")
                                                   
                                               ),
                                               #Shepard plot
                                               box(solidHeader = TRUE, title = "Shepard plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "success",
                                                   plotlyOutput("plot_pcoa_sh") %>% withSpinner(color="#0dc5c1")
                                               )
                                               
                                               
                                               
                                               )
                             
                          )
                   )
                   ,
                   column(width=6,
                          
                          box(solidHeader = TRUE,title = "T-SNE", collapsible = TRUE, 
                              collapsed = TRUE,
                              height = NULL, width = NULL, status = "info",
                              numericInput("tsne_num_dim", "Number of dimensions", 2, min = 1, max = 10),
                              uiOutput("tsne_dim1"),
                              uiOutput("tsne_dim2"),
                              
                              
                              br(),
                              actionButton("tsne_para", "Visualiz plot"),
                              br(),
                              br(),
                              
                              #set parameters
                              conditionalPanel(condition = "input.tsne_para",
                                               
                                               box(solidHeader = TRUE, title = "T-sne Plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "info",
                                                   plotlyOutput("plot_tsne") %>% withSpinner(color="#0dc5c1")
                                                   
                                               ),
                                               #Shepard plot
                                               box(solidHeader = TRUE, title = "Shepard plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "info",
                                                   plotlyOutput("plot_tsne_sh") %>% withSpinner(color="#0dc5c1")
                                               )
                                               
                              )
                              
                              
                          )
                          
                          
                   )
                   # ,
                   # column(width=6,
                   #        box(solidHeader = TRUE,title = "Kruskal's Non-metric MDS", collapsible = TRUE, collapsed = TRUE,
                   #            height = NULL, width = NULL, status = "danger",
                   #            
                   #            numericInput("krus_num_dim", "Number of dimensions", 2, min = 1, max = 10),
                   #            uiOutput("krus_dim1"),
                   #            uiOutput("krus_dim2"),
                   #            
                   #            
                   #            br(),
                   #            actionButton("krus_para", "Visualiz plot"),
                   #            br(),
                   #            br(),
                   #            
                   #            #set parameters
                   #            conditionalPanel(condition = "input.krus_para",
                   #                             
                   #                             box(solidHeader = TRUE, title = "Kruskal Plot", collapsible = TRUE, collapsed = TRUE,
                   #                                 height = NULL, width = NULL, status = "danger",
                   #                                 plotlyOutput("plot_krus") %>% withSpinner(color="#0dc5c1")
                   #                                 
                   #                             ),
                   #                             #Shepard plot
                   #                             box(solidHeader = TRUE, title = "Shepard plot", collapsible = TRUE, collapsed = TRUE,
                   #                                 height = NULL, width = NULL, status = "danger",
                   #                                 plotlyOutput("plot_krus_sh") %>% withSpinner(color="#0dc5c1")
                   #                             )
                   #                             
                   #            )
                   #            
                   #            
                   #        )
                   # )
                   
                 )#,#fluidrow
                 # 
                 # fluidRow(
                 #   br(),
                 #   column(width=6,
                 #          
                 #          box(solidHeader = TRUE,title = "T-SNE", collapsible = TRUE, 
                 #              collapsed = TRUE,
                 #              height = NULL, width = NULL, status = "info",
                 #              numericInput("tsne_num_dim", "Number of dimensions", 2, min = 1, max = 10),
                 #              uiOutput("tsne_dim1"),
                 #              uiOutput("tsne_dim2"),
                 #              
                 #              
                 #              br(),
                 #              actionButton("tsne_para", "Visualiz plot"),
                 #              br(),
                 #              br(),
                 #              
                 #              #set parameters
                 #              conditionalPanel(condition = "input.tsne_para",
                 #                               
                 #                               box(solidHeader = TRUE, title = "T-sne Plot", collapsible = TRUE, collapsed = TRUE,
                 #                                   height = NULL, width = NULL, status = "info",
                 #                                   plotlyOutput("plot_tsne") %>% withSpinner(color="#0dc5c1")
                 #                                   
                 #                               ),
                 #                               #Shepard plot
                 #                               box(solidHeader = TRUE, title = "Shepard plot", collapsible = TRUE, collapsed = TRUE,
                 #                                   height = NULL, width = NULL, status = "info",
                 #                                   plotlyOutput("plot_tsne_sh") %>% withSpinner(color="#0dc5c1")
                 #                               )
                 #                               
                 #                               )
                 #              
                 #             
                 #          )
                 #          
                 #          
                 #   )
                 #   # ,
                 #   # 
                 #   # column(width=6,
                 #   #        box(solidHeader = TRUE,title = "Sammon MDS", collapsible = TRUE, collapsed = TRUE,
                 #   #            height = NULL, width = NULL, status = "warning",
                 #   #            
                 #   #            
                 #   #            numericInput("sam_num_dim", "Number of dimensions", 2, min = 1, max = 10),
                 #   #            uiOutput("sam_dim1"),
                 #   #            uiOutput("sam_dim2"),
                 #   #            
                 #   #            
                 #   #            br(),
                 #   #            actionButton("sam_para", "Visualiz plot"),
                 #   #            br(),
                 #   #            br(),
                 #   #            
                 #   #            #set parameters
                 #   #            conditionalPanel(condition = "input.sam_para",
                 #   #                             
                 #   #                             box(solidHeader = TRUE, title = "Sammon Plot", collapsible = TRUE, collapsed = TRUE,
                 #   #                                 height = NULL, width = NULL, status = "warning",
                 #   #                                 plotlyOutput("plot_sam") %>% withSpinner(color="#0dc5c1")
                 #   #                                 
                 #   #                             ),
                 #   #                             #Shepard plot
                 #   #                             box(solidHeader = TRUE, title = "Shepard plot", collapsible = TRUE, collapsed = TRUE,
                 #   #                                 height = NULL, width = NULL, status = "warning",
                 #   #                                 plotlyOutput("plot_sam_sh") %>% withSpinner(color="#0dc5c1")
                 #   #                             )
                 #   #                             
                 #   #            )
                 #   #            
                 #   # 
                 #   #        )
                 #   # )
                 #   
                 #   
                 #   
                 # )#fluidrow
        ),
        tabPanel(div(fa("capsules", fill = "#158cba"), "Clustering"),
                 
                 fluidRow(
                   column(width=6,
                          box(solidHeader = TRUE,  title = "K-means", collapsible = TRUE, collapsed = TRUE,
                              height = NULL, width = NULL, status = "primary",
                              
                              selectInput("df_for_clus", "Select the data type for clustering:",
                                          c("Semantic Similarty matrix" = "df_similarty",
                                            "PCOA components" = "df_pcoa",
                                            "T-SNE projections" = "df_tsne"
                                            #,
                                            #"Sammon projections" = "df_sam",
                                            #"Kruskal projections" = "df_kra"
                                            
                                            )),
                              # Simlarty matrix is already a distance matrix
                              # selectInput("dis_kmeans", "Select method for distance matrix:",
                              #             c("Euclidean" = "euclidean",
                              #               "Manhattan" = "manhattan"
                              #               
                              #             )),
                              numericInput("Kmeans_centers", "Number of clusters", 2, min = 1, max = 20),
                              #numericInput("kmeans_nsstart", "kmeans_nsstart", 5, min = 1, max = 50),
                              br(),
                              actionButton("kmeans_para", "Run Kmeans clustering", status="primary"),
                              br(),
                              br(),
                              
                              conditionalPanel(condition = "input.kmeans_para",
                                               
                                               box(solidHeader = FALSE, title = "Clustering Plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "primary",
                                                   plotlyOutput("plot_kmeans") %>% withSpinner(color="#0dc5c1")
                                                   
                                               ),
                                               #
                                               box(solidHeader = FALSE, title = "Optimal number of clusters", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "primary",
                                                   plotlyOutput("plot_kmeans_opt") %>% withSpinner(color="#0dc5c1")
                                               ),
                                               box(solidHeader = FALSE, title = "Clustering validations (Intrinsic and extrinsic)", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "primary",
                                                   DT::dataTableOutput("table_kmeans_val") %>% withSpinner(color="#0dc5c1")
                                               ),
                                               box(solidHeader = FALSE, title = "Silloutte plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "primary",
                                                   plotlyOutput("plot_kmeans_sil") %>% withSpinner(color="#0dc5c1")
                                               )
                                               )#conditional panel

                          )#first box
                  )#first column
                  ,
                  column(width=6,
                         box(solidHeader = TRUE,  title = "PAM Clustering", collapsible = TRUE, collapsed = TRUE,
                             height = NULL, width = NULL, status = "success",
                             
                             selectInput("df_for_pam", "Select the data type for clustering:",
                                         c("Semantic Similarty matrix" = "df_similarty",
                                           "PCOA components" = "df_pcoa",
                                           "T-SNE projections" = "df_tsne"
                                           #,
                                           #"Sammon projections" = "df_sam",
                                           #"Kruskal projections" = "df_kra"
                                           
                                         )),
                             
                            
                             # Simlarty matrix is already a distance matrix
                             # selectInput("dis_kmeans", "Select method for distance matrix:",
                             #             c("Euclidean" = "euclidean",
                             #               "Manhattan" = "manhattan"
                             #               
                             #             )),
                             numericInput("pam_centers", "Number of clusters", 2, min = 1, max = 20),
                             
                             br(),
                             actionButton("pam_para", "Run PAM clustering"),
                             br(),
                             br(),
                             
                             conditionalPanel(condition = "input.pam_para",
                                              box(solidHeader = FALSE, title = "PAM Clustering Plot", collapsible = TRUE, collapsed = TRUE,
                                                  height = NULL, width = NULL, status = "success",
                                                  plotlyOutput("plot_pam") %>% withSpinner(color="#0dc5c1")
                                                  
                                              ),
                                              #
                                              box(solidHeader = FALSE, title = "Optimal number of clusters", collapsible = TRUE, 
                                                  collapsed = TRUE,
                                                  height = NULL, width = NULL, status = "success",
                                                  plotlyOutput("plot_pam_opt") %>% withSpinner(color="#0dc5c1")
                                              ),
                                              box(solidHeader = FALSE, title = "Clustering validations (Intrinsic and extrinsic)", collapsible = TRUE, collapsed = TRUE,
                                                  height = NULL, width = NULL, status = "success",
                                                  DT::dataTableOutput("table_pam_val") %>% withSpinner(color="#0dc5c1")
                                              ),
                                              box(solidHeader = FALSE, title = "Silloutte plot", collapsible = TRUE, collapsed = TRUE,
                                                  height = NULL, width = NULL, status = "success",
                                                  plotlyOutput("plot_pam_sil") %>% withSpinner(color="#0dc5c1")
                                              )
                                              
                                              
                             )#cp
                             
                            )#first box
                  )#2nd column
                  
                 ),#first row
                 
                 fluidRow(
                   column(width=6,
                          box(solidHeader = TRUE,  title = "Agglomerative Hierarchical Clustering (AHC)", 
                              collapsible = TRUE, collapsed = TRUE,
                              height = NULL, width = NULL, status = "danger",
                              
                              selectInput("df_for_hca", "Select the data type for clustering:",
                                          c("Semantic Similarty matrix" = "df_similarty",
                                            "PCOA components" = "df_pcoa",
                                            "T-SNE projections" = "df_tsne"
                                            # ,
                                            # "Sammon projections" = "df_sam",
                                            # "Kruskal projections" = "df_kra"
                                            
                                          )),
                              selectInput("linkage_hca", "Select linkage method for clustering:",
                                          c("Average" = "average",
                                            "Single" = "single",
                                            "Complete" = "complete",
                                            "Ward" = "ward",
                                            "Flexible" = "flexible",
                                            "gaverage" = "gaverage"
                                            
                                          ), selected="average"),
                              # Simlarty matrix is already a distance matrix
                              # selectInput("dis_kmeans", "Select method for distance matrix:",
                              #             c("Euclidean" = "euclidean",
                              #               "Manhattan" = "manhattan"
                              #               
                              #             )),
                              numericInput("hca_centers", "Number of clusters", 2, min = 1, max = 20),
                              #numericInput("kmeans_nsstart", "kmeans_nsstart", 5, min = 1, max = 50),
                              br(),
                              actionButton("hca_para", "Run AHC clustering"),
                              br(),
                              br(),
                              
                              conditionalPanel(condition = "input.hca_para",
                                               box(solidHeader = FALSE, title = "Clustering Plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "danger",
                                                   plotlyOutput("plot_hca") %>% withSpinner(color="#0dc5c1")
                                                   
                                               ),
                                               #
                                               box(solidHeader = FALSE, title = "Optimal number of clusters", collapsible = TRUE, 
                                                   collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "danger",
                                                   plotlyOutput("plot_hca_opt") %>% withSpinner(color="#0dc5c1")
                                               ),
                                               box(solidHeader = FALSE, title = "Clustering validations (Intrinsic and extrinsic)", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "danger",
                                                   DT::dataTableOutput("table_hca_val") %>% withSpinner(color="#0dc5c1")
                                               ),
                                               box(solidHeader = FALSE, title = "Silloutte plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "danger",
                                                   plotlyOutput("plot_hca_sil") %>% withSpinner(color="#0dc5c1")
                                                   
                                               
                                               
                                               )#cp
                              )
                          )#first box
                   )#first column
                   ,
                   column(width=6,
                          box(solidHeader = TRUE,  title = "Fuzzy Clustering", collapsible = TRUE, collapsed = TRUE,
                              height = NULL, width = NULL, status = "warning",
                              
                              selectInput("df_for_hcd", "Select the data type for clustering:",
                                          c("Semantic Similarty matrix" = "df_similarty",
                                            "PCOA components" = "df_pcoa",
                                            "T-SNE projections" = "df_tsne"
                                            # ,
                                            # "Sammon projections" = "df_sam",
                                            # "Kruskal projections" = "df_kra"
                                            
                                          )),
                              
                              # Simlarty matrix is already a distance matrix
                              selectInput("dis_fuzzy", "Select method for distance matrix:",
                                           c("Euclidean" = "euclidean",
                                             "Manhattan" = "manhattan"
                                             
                                           )),
                              numericInput("fuzzy_centers", "Number of clusters", 2, min = 1, max = 20),
                              
                              br(),
                              actionButton("fuzzy_para", "Run fuzzy clustering"),
                              br(),
                              br(),
                              conditionalPanel(condition = "input.fuzzy_para",
                                               
                                               box(solidHeader = FALSE, title = "Clustering Plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "warning",
                                                   plotlyOutput("plot_fuzzy") %>% withSpinner(color="#0dc5c1")
                                                   
                                               ),
                                               #
                                               box(solidHeader = FALSE, title = "Optimal number of clusters", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "warning",
                                                   plotlyOutput("plot_fuzzy_opt") %>% withSpinner(color="#0dc5c1")
                                               ),
                                               box(solidHeader = FALSE, title = "Clustering validations (Intrinsic and extrinsic)", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "warning",
                                                   DT::dataTableOutput("table_fuzzy_val") %>% withSpinner(color="#0dc5c1")
                                               ),
                                               box(solidHeader = FALSE, title = "Silloutte plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "warning",
                                                   plotlyOutput("plot_fuzzy_sil") %>% withSpinner(color="#0dc5c1")
                                               )
                                               
                                               
                                               )#cp
                          )#first box
                   )#2nd column
                   
                 )#2nd row row
        )#,# clustering tab end
        
        # tabPanel(div(fa("capsules", fill = "#158cba"), "Functional Enrichment Analysis"),
                 
        #          fluidRow(
        #            column(width=4,
        #                   box(solidHeader = TRUE,  title = "Select the clusters of genes", 
        #                       collapsible = TRUE, collapsed = FALSE,
        #                       height = NULL, width = NULL, status = "primary",
        #                       uiOutput("fea_clus_in"),
        #                       uiOutput("fea_clus_num"),
        #                       uiOutput("fea_run_btn")
                              
                              
                             
        #                   )
        #            )
        #          )
                 
        # )#tabpanel FEA
        
                 

      ) # tabeset panel
      

      
      
      
      
      
    ) # fluidpageinstall.packages("packageName")
    
  ))# shiny dashboard




#hsGO2 <- godata('org.Hs.eg.db', keytype = "SYMBOL", ont="BP", computeIC=TRUE) 


#write.csv(hsGO2, "goData")
 
# class(hsGO2)
# str(hsGO2)
# 
# aa <- hsGO2@keys
# class(aa)
# length(aa)
# 
# bb <- head(aa)
# 
# kk <-intersect(bb, aa) 
# 
# # What is in my_list that is not in my_list_2?
# setdiff(my_list, my_list_2) # 45 12 56 14 16


server <- function(input, output) {
  
  params <- reactiveValues(df_in = NULL, # input data model
                           df_similarty = NULL, # semantic similarty matrix
                           df_pcoa = NULL,
                           df_tsne = NULL, 
                           df_sam = NULL,
                           df_kra = NULL,
                           df_clus_genes = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("genes",
                            "cluster", "clus_method")))
  
  
  

  output$outFile1 <- DT::renderDataTable({
    req(input$inFile1)
    
    
    df <- read.csv(input$inFile1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    print(colnames(df))
    
    params$df_in <- df
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  # bar pot
   output$histPlot <- renderPlotly({
       req(input$inFile1$datapath)
       #df <- read.csv(input$inFile1$datapath, header = input$header, sep = input$sep, quote = input$quote)
  
       bar_data <- as.data.frame(table(params$df_in$class))
       p <- ggplot( data = bar_data, aes_string( x = colnames(bar_data)[1], 
           y = colnames(bar_data)[2]))+ geom_bar(stat="identity")
       #convert the plot to plotlyplot
       ggplotly(p)
   })
  
   #venn diagram
   output$venPlot <- renderPlot({
       req(input$inFile1$datapath)
       #df <- read.csv(input$inFile1$datapath, header = input$header, sep = input$sep, quote = input$quote)
       x<-list()
  
       #make list from dataframe using class column
       for(i in unique(params$df_in$class)){
           set1<- params$df_in[params$df_in$class== i,][1]
           names(set1) <- paste("Set=", i, sep = "") 
           x <- append(x, set1)
       }
  
       ggvenn(x)
  })
  

  ### semnatic similarty matrix

   ##add a obsrevent 
   ##make the data frame reactive
   #genrate heatmap with recative datarrame
   # observeEvent(input$ssRun, {
   #   params$df_similarty <- mgeneSim(params$df_in$genes, semData=hsGO2, measure="Wang", combine="BMA", verbose=FALSE) 
   # })

  observeEvent(input$ssRun, {
    
    output$ssm <- DT::renderDataTable({
    
    	#add buttons above table
    	exportOptions = list(
    		select = TRUE,
    		searching = TRUE,
               scrollX = TRUE,
               scrollY = TRUE,
               dom = "BRSpfrti",
               buttons = list(
               	list(
               	extend = "copy",
               	text = 'Copy the selected',
               	exportOptions = list(
               		modifier = list(
               			selected = TRUE, 
               			order='index', 
               			page= 'all', 
               			search='none' 
               				)
               			)
               	),
               	list(
	                   extend = "csv",
	                   text = 'Exported selected to CSV',
	                   exportOptions = list(
	                   modifier = list(selected = TRUE, 
	                   			order    = 'index', 
	                   			page     = 'all', 
	                   			search='none'
	                   				)
	                   			)
                                            ),
                                                         
                       list(
                           extend = "csv",
                           text = 'Exported all to CSV',
                           exportOptions = list(
                           modifier = list(
                           		selected = FALSE, 
                           		order= 'index', page     = 'all', search='none'))
                         )
                         
                       )
                     )
                     
            
      #semsimMat <-as.data.frame(mgeneSim(params$df_in$genes,
       #                                semData=hsGO2,
        #                               measure=input$ssMethod,
        #                               combine=input$ssMeasure,
         #                              verbose=FALSE))
     # 
     # semsimMat$genes <- rownames(semsimMat)
     # 
      #semsimMat <- right_join(semsimMat,
       #                       params$df_in,
        #                         by = c("genes" = "genes"))
     # 
     # #semsimMat <- semsimMat[,c() ]
     # 
      #params$df_similarty <-na.omit(semsimMat)
     
     params$df_similarty <- read.csv("/home/asif/Desktop/CenturiWork/clustering/8Feb2022/ssm_matrix.csv")
     #print( params$df_similarty )
     #write.csv(params$df_similarty, "ssm_matrix.csv")
     
     datatable(
       params$df_similarty,
       #caption = "Differentially expressed genes",
       rownames = TRUE,
       filter = 'top',
       extensions = c("Buttons", "Select"),
       options = exportOptions
       
     )
      #print(class(params$df_similarty))
    })
    
    })
  
  ####principal coordinates analysis
  
  output$pcoa_dim1<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$pcoa_num_dim)){
      selectizeInput(("pcoa_dim_list1"), "Select the dimension to visualize", 
                     seq.int(1, input$pcoa_num_dim), selected = 1, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  output$pcoa_dim2<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$pcoa_num_dim)){
      selectizeInput(("pcoa_dim_list2"), "Select the dimension to visualize", 
                     seq.int(1, input$pcoa_num_dim), selected = 2, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  ##############################################
  ###for next version
  ### pcooa analysis
  pcoa_ploting <- eventReactive(input$pcoa_para, {

    #dist_mt <- as.dist(params$df_similarty)
     dist_mt <- as.dist(params$df_similarty[, -which(names(params$df_similarty) %in% c("genes","class"))])
   # if(!is.null(dist_mt)){
      pcooa<-cmdscale(dist_mt, k = input$pcoa_num_dim)

      #store projectiobs
      params$df_pcoa <-as.data.frame(pcooa)

      print("******************************************")
      params$df_pcoa[['genes']] <- rownames(params$df_pcoa)
      params$df_pcoa[['class']] <- params$df_similarty$class 
      
      print(params$df_pcoa)
      

      print("******************************************")

      #select the data for visualze
      x <- pcooa[,as.numeric(input$pcoa_dim_list1)]
      y <- pcooa[,as.numeric(input$pcoa_dim_list2)]


      plot_df <- cbind(round(x, 3),round(y, 3) , params$df_similarty$class)
      print(plot_df)
      plot_df <- as.data.frame(plot_df)
      names(plot_df)[3] <- "class"

      #print(class(input$pcoa_dim_list1))

      print(plot_df)

      p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
        labs(x = as.character(paste("Dimension", input$pcoa_dim_list1, " ")),
             y = as.character(paste("Dimension", input$pcoa_dim_list2, " ")))  + theme_classic()
      p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions


      #########################################
      # shepard plot

      output$plot_pcoa_sh <- renderPlotly({

        sh<- Shepard(dist_mt, pcooa)
        x <- sh$x
        y <- sh$y

        plot_df2 <- data.frame("x"=x,"y"=y)

        p = ggplot(data = plot_df2, aes(x, y)) + geom_point() +
          labs(x = "projected", y = "Observations",
               title = "Shepard Plot")
        p = p + geom_smooth(method = "lm", se = FALSE) + theme_classic()

        pp = ggplotly(p)

        pp


      })


      ################################
      p2

    #}


  })

  #############################################

   output$plot_pcoa <- renderPlotly({
     pcoa_ploting()

  })


  # ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # 
  # #### Kruskal analysis
  # 
  # output$krus_dim1<- renderUI({
  #   
  #   #ns <- session$ns
  #   
  #   if(!is.null(input$krus_num_dim)){
  #     selectizeInput(("krus_dim_list1"), "Select dimension to visualize", 
  #                    seq.int(1, input$krus_num_dim), selected = 1, multiple = T, 
  #                    options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
  #     
  #   }
  #   
  # }) 
  # 
  # output$krus_dim2<- renderUI({
  #   
  #   #ns <- session$ns
  #   
  #   if(!is.null(input$krus_num_dim)){
  #     selectizeInput(("krus_dim_list2"), "Select dimension to visualize", 
  #                    seq.int(1, input$krus_num_dim), selected = 2, multiple = T, 
  #                    options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
  #     
  #   }
  #   
  # }) 
  # 
  # # ##############################################
  # ### Kruskal analysis
  # kru_ploting <- eventReactive(input$krus_para, {
  # 
  #   dist_mt <- as.dist(params$df_similarty)
  #  
  #   # if(!is.null(dist_mt)){
  #   kru_mds<-isoMDS(dist_mt, k = input$krus_num_dim, trace=F)
  #   
  #   
  #   #store projections
  #   params$df_kra <- kru_mds$points
  #   x <- kru_mds$points[, as.numeric(input$krus_dim_list1)]
  #   print(x)
  #   y <- kru_mds$points[, as.numeric(input$krus_dim_list2)]
  # 
  # 
  #   plot_df <- cbind(round(x, 3),round(y, 3) , params$df_in$class)
  #   print(plot_df)
  #   plot_df <- as.data.frame(plot_df)
  #   names(plot_df)[3] <- "class"
  # 
  #   #print(class(input$pcoa_dim_list1))
  #   
  #   print(plot_df)
  # 
  #   p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
  #     labs(x = as.character(paste("Dimension", input$krus_dim_list1, " ")),
  #          y = as.character(paste("Dimension", input$krus_dim_list2, " ")))  + theme_classic()
  #   p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions
  # 
  # 
  #   #########################################
  #   # shepard plot
  # 
  #   output$plot_krus_sh <- renderPlotly({
  # 
  #     sh<- Shepard(dist_mt, mds$points)
  #     x <- sh$x
  #     y <- sh$y
  # 
  #     plot_df2 <- data.frame("x"=x,"y"=y)
  # 
  #     p = ggplot(data = plot_df2, aes(x, y)) + geom_point() +
  #       labs(x = "projected", y = "Observations",
  #            title = "Shepard Plot")
  #     p = p + geom_smooth(method = "lm", se = FALSE) + theme_classic()
  # 
  #     pp = ggplotly(p)
  # 
  #     pp
  # 
  # 
  #   })
  # 
  # 
  #   ################################
  #   p2
  # 
  #   #}
  # 
  # 
  # })
  # 
  # #############################################
  # 
  # output$plot_krus <- renderPlotly({
  #   kru_ploting()
  # 
  # })
  # 
  # #######
  # 
  # #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # 
  ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  #### TSNE analysis
  
  output$tsne_dim1<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$tsne_num_dim)){
      selectizeInput(("tsne_dim_list1"), "Select dimension to visualize", 
                     seq.int(1, input$tsne_num_dim), selected = 1, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  output$tsne_dim2<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$tsne_num_dim)){
      selectizeInput(("tsne_dim_list2"), "Select dimension to visualize", 
                     seq.int(1, input$tsne_num_dim), selected = 2, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  # ##############################################
  ### Kruskal analysis
  tsne_ploting <- eventReactive(input$tsne_para, {
    
    
    dist_mt <- as.dist(params$df_similarty[, -which(names(params$df_similarty) %in% c("genes","class"))])
    
    # if(!is.null(dist_mt)){
    tsne_mod <- Rtsne(as.matrix(dist_mt), dims = input$tsne_num_dim, perplexity=3, 
                  verbose=FALSE, max_iter = 500, is_distance = TRUE)
    print(tsne_mod$Y)
    
    #to store the projection
    params$df_tsne <-as.data.frame(tsne_mod$Y )

    params$df_tsne[['genes']] <- params$df_similarty$genes
    params$df_tsne[['class']] <- params$df_similarty$class 
   
    print(params$df_tsne)
    x <- tsne_mod$Y[, as.numeric(input$tsne_dim_list1)]
    
    y <- tsne_mod$Y[, as.numeric(input$tsne_dim_list2)]
    
    
    
    plot_df <- cbind(round(x, 3),round(y, 3) , params$df_similarty$class)
    print(plot_df)
    plot_df <- as.data.frame(plot_df)
    names(plot_df)[3] <- "class"
    
    #print(class(input$pcoa_dim_list1))
    
    print(plot_df)
    
    p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
      labs(x = as.character(paste("Dimension", input$tsne_dim1, " ")),
           y = as.character(paste("Dimension", input$tsne_dim2, " ")))  + theme_classic()
    p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions
    
    
    #########################################
    # shepard plot
    
    output$plot_tsne_sh <- renderPlotly({
      
     
      
      sh<- Shepard(dist_mt, tsne_mod$Y)
      x <- sh$x
      y <- sh$y
      
      plot_df2 <- data.frame("x"=x,"y"=y)
      
      p = ggplot(data = plot_df2, aes(x, y)) + geom_point() +
        labs(x = "projected", y = "Observations",
             title = "Shepard Plot")
      p = p + geom_smooth(method = "lm", se = FALSE) + theme_classic()
      
      pp = ggplotly(p)
      
      pp
      
      
    })
    
    
    ################################
    p2
    
    #}
    
    
  })
  
  #############################################
  
  output$plot_tsne <- renderPlotly({
    tsne_ploting()
    
  })
  
  #######
  
  
  
  ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # #### Sammon analysis
  # 
  # output$sam_dim1<- renderUI({
  #   
  #   #ns <- session$ns
  #   
  #   if(!is.null(input$sam_num_dim)){
  #     selectizeInput(("sam_dim_list1"), "Select dimension to visualize", 
  #                    seq.int(1, input$sam_num_dim), selected = 1, multiple = T, 
  #                    options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
  #     
  #   }
  #   
  # }) 
  # 
  # output$sam_dim2<- renderUI({
  #   
  #   #ns <- session$ns
  #   
  #   if(!is.null(input$sam_num_dim)){
  #     selectizeInput(("sam_dim_list2"), "Select dimension to visualize", 
  #                    seq.int(1, input$sam_num_dim), selected = 2, multiple = T, 
  #                    options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
  #     
  #   }
  #   
  # }) 
  # 
  # # ##############################################
  # ### Kruskal analysis
  # sam_ploting <- eventReactive(input$sam_para, {
  #   
  #   dist_mt <- as.dist(params$df_similarty)
  #   sam <-sammon(dist_mt, k=input$sam_num_dim)
  #   
  #   #store projection
  #   params$df_sam <- sam$points
  #   
  #   x <- sam$points[, as.numeric(input$sam_dim_list1)]
  #   
  #   y <- sam$points[, as.numeric(input$sam_dim_list2)]
  #   print(x)
  #   
  #   
  #   plot_df <- cbind(round(x, 3),round(y, 3) , params$df_in$class)
  #   print(plot_df)
  #   plot_df <- as.data.frame(plot_df)
  #   names(plot_df)[3] <- "class"
  #   
  #   #print(class(input$pcoa_dim_list1))
  #   
  #   print(plot_df)
  #   
  #   p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
  #     labs(x = as.character(paste("Dimension", input$sam_dim_list1, " ")),
  #          y = as.character(paste("Dimension", input$sam_dim_list2, " ")))  + theme_classic()
  #   p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions
  #   
  #   
  #   #########################################
  #   # shepard plot
  #   
  #   output$plot_sam_sh <- renderPlotly({
  #     
  #     
  #     
  #     sh<- Shepard(dist_mt, sam$points)
  #     x <- sh$x
  #     y <- sh$y
  #     
  #     plot_df2 <- data.frame("x"=x,"y"=y)
  #     
  #     p = ggplot(data = plot_df2, aes(x, y)) + geom_point() +
  #       labs(x = "projected", y = "Observations",
  #            title = "Shepard Plot")
  #     p = p + geom_smooth(method = "lm", se = FALSE) + theme_classic()
  #     
  #     pp = ggplotly(p)
  #     
  #     pp
  #     
  #     
  #   })
  #   
  #   
  #   ################################
  #   p2
  #   
  #   #}
  #   
  #   
  # })
  # 
  # #############################################
  # 
  # output$plot_sam <- renderPlotly({
  #   sam_ploting()
  #   
  # })
  # 
  ##############################################################################################
  ###########    Clustering part
  ##############################################################################################
  #### K-means clustering

  kmeans_ploting <- eventReactive(input$kmeans_para, { 

       
       if(!is.null(params$df_similarty)){
         
         # print(params$df_similarty)
         #print("===========================")
         df_sel <- input$df_for_clus
         #print(df_sel)
         
         # ifelse(input$df_for_clus == "df_similarty",df_kmeans_in <-as.data.frame(params$df_similarty)
         #        ,
         #        
         # ifelse(input$df_for_clus == "df_pcoa",df_kmeans_in <-as.data.frame(params$df_pcoa),
         #        ifelse(input$df_for_clus == "df_tsne",df_kmeans_in <-as.data.frame(params$df_tsne),
         #        ifelse(input$df_for_clus == "df_umap",df_kmeans_in <-as.data.frame(params$df_umap),
         # 
         # ifelse(input$df_for_clus == "df_sam",df_kmeans_in <-as.data.frame(params$df_sam),
         # ifelse(input$df_for_clus == "df_kra",df_kmeans_in <-as.data.frame(params$df_kra)
         # ))))))
         
         # if(input$df_for_clus == "df_similarty"){
         #   df_kmeans_in <-as.data.frame(params$df_similarty) 
         #   print("in ifffff")
         # 
         # }
         
         
         if(input$df_for_clus == "df_similarty"){
           
           df_kmeans_in <-params$df_similarty
           print("999999999999999999")
           print(df_kmeans_in)
           
         }
         if(input$df_for_clus == "df_pcoa"){
           
           df_kmeans_in <-params$df_pcoa
           print("////////////////////////")
           print(df_kmeans_in)
           
         }
         if(input$df_for_clus == "df_tsne"){
           
           df_kmeans_in <-params$df_tsne
           print("*******************")
           print(df_kmeans_in)
           
         }
         
         
         #df_kmeans_in <-as.data.frame(params$df_pcoa) 
         print("-------------------------------------")
         print(df_kmeans_in)
         
         
         if(!is.null(df_kmeans_in)){
           
           #optimal number of clusters
           ####################################
           output$plot_kmeans_opt <- renderPlotly({
             method_opt_clus <- "silhouette"
             dist_kmeans <- as.dist(df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class"))])
             kmeans_opt_p <- fviz_nbclust(
               df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class"))], 
               kmeans, method = method_opt_clus ,
               diss =dist_kmeans, k.max=10 )
             
             kmeans_opt_p <- ggplotly(kmeans_opt_p)
             kmeans_opt_p
             
           })
           
           
           Kmeans_centers <- input$Kmeans_centers
           #print("//////////////////////////")
           #print(Kmeans_centers)
           
           res_kmeans_clus <- kmeans(df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class"))], 
                                     Kmeans_centers, iter.max = 10, nstart = 25)
           
           df_kmeans_in$cluster <-(unname(res_kmeans_clus$cluster)) 
           
           temp_df_kmeans <- df_kmeans_in$genes
           temp_df_kmeans <- cbind(temp_df_kmeans,  
                                   df_kmeans_in$cluster, rep("kmeans", length(df_kmeans_in$cluster) ) )
           temp_df_kmeans <- as.data.frame(temp_df_kmeans)
           names(temp_df_kmeans) <- c("genes", "cluster", "clus_method")
           
           #print(temp_df_kmeans)
           
           params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_kmeans)
           # print(params$df_clus_genes)
           
           #print(df_kmeans_in)
           
           ################    Table
           
           output$table_kmeans_val <- renderDT({
            print("++++++++++++++++++++++++++++++++++++++")

            print(df_kmeans_in)
                        print("++++++++++++++++++++++++++++++++++++++")
             
             dist_kmeans <- as.dist(df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class", "cluster"))])
             
             kmeans_stat<-cluster.stats(d = dist_kmeans,  res_kmeans_clus$cluster, alt.clustering = NULL,
                                        noisecluster=FALSE, silhouette = TRUE, G2 = FALSE, 
                                        G3 = FALSE, wgap=TRUE, sepindex=TRUE, 
                                        sepprob=0.1, sepwithnoise=TRUE, compareonly = FALSE, aggregateonly = FALSE)
             print(kmeans_stat)
             
             kmeans_table<-c()
             kmeans_table<-rbind(kmeans_table,kmeans_stat$cluster.size, kmeans_stat$diameter,
                                 (kmeans_stat$average.distance), 
                                 kmeans_stat$median.distance,kmeans_stat$separation, kmeans_stat$clus.avg.silwidths
                                 , kmeans_stat$average.between, kmeans_stat$average.within
                                 ,kmeans_stat$max.diameter, kmeans_stat$min.separation, kmeans_stat$avg.silwidth
                                 , kmeans_stat$dunn, kmeans_stat$dunn2)
             
             
             row.names(kmeans_table)<-c("Cluster size", "Diameter",  "Average distance", 
                                        "Median distance","Separation", "Clus avg. silwidths"
                                        , "Average between","Average within"
                                        ,"Max diameter","Min separation", "Avg. silwidth","Dunn","Dunn2"
             )
             colnames(kmeans_table)<-c(paste("cluster",1:input$Kmeans_centers,  sep = "."))
             
             kmeans_table<-round(kmeans_table,3)
             return(kmeans_table)
             
           })
           
           #######################################################
           # Silhouette plot
           
           output$plot_kmeans_sil <- renderPlotly({
             
             # silhouette Plot
             dist_kmeans <- as.dist(df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class", "cluster"))])
             kmeans_sil <- silhouette(df_kmeans_in$cluster, dist_kmeans)
             fviz_silhouette(kmeans_sil)
             
           })
           
           
           
           
           ######################################################################
           
           
           
           
           
           dd <- fviz_cluster(res_kmeans_clus, df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class", "cluster"))], 
                              alpha = 0.9, shape = 19, geom = c("point"), ellipse = FALSE, main = paste("Clustering for ",
                                                                                                        input$df_for_clus,sep = "" ) ) 
           kmeans_plot <- ggplotly(dd)
           kmeans_plot
           
           
           #kmeans_plot <- ggplot(data = df_kmeans_in, aes(df_kmeans_in[, 1], df_kmeans_in[, 2], colour =as.factor(cluster), 
           #      fill = as.factor(cluster))) + geom_point() + theme_classic()
           
           #kmeans_plot <- ggplotly(kmeans_plot)
           #kmeans_plot
           
           # output$plot_kmeans_opt <- renderPlotly({
           #   
           #   kmeans_opt_plot <- fviz_nbclust(df_kmeans_in[-"k_means"], kmeans, method = "wss") +
           #     geom_vline(xintercept = 4, linetype = 2)
           #      kmeans_opt_plot <- ggplotly(kmeans_opt_plot)
           #      kmeans_opt_plot
           
           #})
           
           
         }else{
           NULL
         }
         
         
         
         
         
       }else{
         NULL
       }
       
       
     })
     
     #############################################
     
     output$plot_kmeans <- renderPlotly({
       kmeans_ploting()
       
     })
     
     
  
  # kmeans_ploting <- eventReactive(input$kmeans_para, { 
    
  #   if(!is.null(params$df_similarty)){
      
  #     print(params$df_similarty)
  #     print("===========================")
  #     df_sel <- input$df_for_clus
  #     print(df_sel)
      
  #     ifelse(input$df_for_clus == "df_similarty",df_kmeans_in <-as.data.frame(params$df_similarty),
  #            ifelse(input$df_for_clus == "df_pcoa",df_kmeans_in <-as.data.frame(params$df_pcoa),
  #                   ifelse(input$df_for_clus == "df_umap",df_kmeans_in <-as.data.frame(params$df_umap),
  #                          ifelse(input$df_for_clus == "df_tsne",df_kmeans_in <-as.data.frame(params$df_tsne),
  #                                 ifelse(input$df_for_clus == "df_sam",df_kmeans_in <-as.data.frame(params$df_sam),
  #                                        ifelse(input$df_for_clus == "df_kra",df_kmeans_in <-as.data.frame(params$df_kra)
  #            ))))))
      
  #     # if(input$df_for_clus == "df_similarty"){
  #     #   df_kmeans_in <-as.data.frame(params$df_similarty) 
  #     #   print("in ifffff")
  #     # 
  #     # }
        
  #       #df_kmeans_in <-as.data.frame(params$df_pcoa) 
  #       print(df_kmeans_in)
        
        
  #       if(!is.null(df_kmeans_in)){
          
  #         #optimal number of clusters
  #         ####################################
  #         output$plot_kmeans_opt <- renderPlotly({
  #           method_opt_clus <- "silhouette"
  #           dist_kmeans <- as.dist(df_kmeans_in)
  #           kmeans_opt_p <- fviz_nbclust(df_kmeans_in, kmeans, method = method_opt_clus ,
  #                                   diss =dist_kmeans, k.max=3 )
            
  #           kmeans_opt_p
            
  #         })
          
          
  #         Kmeans_centers <- input$Kmeans_centers
  #         print("//////////////////////////")
  #         print(Kmeans_centers)
          
  #         res_kmeans_clus <- kmeans(df_kmeans_in, Kmeans_centers, iter.max = 10, nstart = 25)
          
  #         df_kmeans_in$cluster <-(unname(res_kmeans_clus$cluster)) 
          
  #         temp_df_kmeans <- rownames(df_kmeans_in)
  #         temp_df_kmeans <- cbind(temp_df_kmeans,  
  #                                 df_kmeans_in$cluster, rep("kmeans", length(df_kmeans_in$cluster) ) )
  #         temp_df_kmeans <- as.data.frame(temp_df_kmeans)
  #         names(temp_df_kmeans) <- c("genes", "cluster", "clus_method")
          
  #         print(temp_df_kmeans)
          
  #         params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_kmeans)
  #         print(params$df_clus_genes)
          
  #         print(df_kmeans_in)
          
  #         ################    Table
          
  #         output$table_kmeans_val <- renderDT({
            
  #           dist_kmeans <- as.dist(df_kmeans_in)
            
  #           kmeans_stat<-cluster.stats(d = dist_kmeans,  df_kmeans_in$cluster, alt.clustering = NULL,
  #                                     noisecluster=FALSE, silhouette = TRUE, G2 = FALSE, 
  #                                     G3 = FALSE, wgap=TRUE, sepindex=TRUE, 
  #                                     sepprob=0.1, sepwithnoise=TRUE, compareonly = FALSE, aggregateonly = FALSE)
            
  #           kmeans_table<-c()
  #           kmeans_table<-rbind(kmeans_table,kmeans_stat$cluster.size, kmeans_stat$diameter,
  #                              (kmeans_stat$average.distance), 
  #                              kmeans_stat$median.distance,kmeans_stat$separation, kmeans_stat$clus.avg.silwidths
  #                              , kmeans_stat$average.between, kmeans_stat$average.within
  #                              ,kmeans_stat$max.diameter, kmeans_stat$min.separation, kmeans_stat$avg.silwidth
  #                              , kmeans_stat$dunn, kmeans_stat$dunn2)
            
            
  #           row.names(kmeans_table)<-c("Cluster size", "Diameter",  "Average distance", 
  #                                     "Median distance","Separation", "Clus avg. silwidths"
  #                                     , "Average between","Average within"
  #                                     ,"Max diameter","Min separation", "Avg. silwidth","Dunn","Dunn2"
  #           )
  #           colnames(kmeans_table)<-c(paste("cluster",1:input$Kmeans_centers,  sep = "."))
            
  #           kmeans_table<-round(kmeans_table,3)
  #           return(kmeans_table)
            
  #         })
          
  #         #######################################################
  #         # Silhouette plot
          
  #         output$plot_kmeans_sil <- renderPlotly({
            
  #           # silhouette Plot
  #           dist_kmeans <- as.dist(df_kmeans_in)
  #           kmeans_sil <- silhouette(df_kmeans_in$cluster, dist_kmeans)
  #           fviz_silhouette(kmeans_sil)
            
  #         })
          
          
          
          
  #         ######################################################################
          
          
          
  #         kmeans_plot <- ggplot(data = df_kmeans_in, aes(df_kmeans_in[, 1], df_kmeans_in[, 2], colour =as.factor(cluster), 
  #                                  fill = as.factor(cluster))) + geom_point() + theme_classic()
          
  #         kmeans_plot <- ggplotly(kmeans_plot)
  #         kmeans_plot
          
  #         # output$plot_kmeans_opt <- renderPlotly({
  #         #   
  #         #   kmeans_opt_plot <- fviz_nbclust(df_kmeans_in[-"k_means"], kmeans, method = "wss") +
  #         #     geom_vline(xintercept = 4, linetype = 2)
  #         #      kmeans_opt_plot <- ggplotly(kmeans_opt_plot)
  #         #      kmeans_opt_plot
            
  #         #})
          

  #       }else{
  #         NULL
  #       }
        
        
      
      
      
  #   }else{
  #     NULL
  #   }
    

  # })
  
  # #############################################
  
  # output$plot_kmeans <- renderPlotly({
  #   kmeans_ploting()
    
  # })
   
  
 ##### PAM clustering
  
  pam_ploting <- eventReactive(input$pam_para, { 
    
       if(!is.null(params$df_similarty)){
         
         # print(params$df_similarty)
         #print("===========================")
         df_sel <- input$df_for_clus
         #print(df_sel)
         
         # ifelse(input$df_for_clus == "df_similarty",df_kmeans_in <-as.data.frame(params$df_similarty)
         #        ,
         #        
         # ifelse(input$df_for_clus == "df_pcoa",df_kmeans_in <-as.data.frame(params$df_pcoa),
         #        ifelse(input$df_for_clus == "df_tsne",df_kmeans_in <-as.data.frame(params$df_tsne),
         #        ifelse(input$df_for_clus == "df_umap",df_kmeans_in <-as.data.frame(params$df_umap),
         # 
         # ifelse(input$df_for_clus == "df_sam",df_kmeans_in <-as.data.frame(params$df_sam),
         # ifelse(input$df_for_clus == "df_kra",df_kmeans_in <-as.data.frame(params$df_kra)
         # ))))))
         
         # if(input$df_for_clus == "df_similarty"){
         #   df_kmeans_in <-as.data.frame(params$df_similarty) 
         #   print("in ifffff")
         # 
         # }
         
         
         if(input$df_for_pam == "df_similarty"){
           
           df_pam_in <-params$df_similarty
           print("999999999999999999")
           print(df_pam_in)
           
         }
         if(input$df_for_pam == "df_pcoa"){
           
           df_pam_in <-params$df_pcoa
           print("////////////////////////")
           print(df_pam_in)
           
         }
         if(input$df_for_pam == "df_tsne"){
           
           df_pam_in <-params$df_tsne
           print("*******************")
           print(df_pam_in)
           
         }
         
         
         #df_kmeans_in <-as.data.frame(params$df_pcoa) 
         print("-------------------------------------")
         print(df_pam_in)

######################################
        
        
        if(!is.null(df_pam_in)){
          
          #optimal number of clusters
          ####################################
          output$plot_pam_opt <- renderPlotly({
            method_opt_clus <- "silhouette"

            
            dist_pamy <- as.dist(df_pam_in[, -which(names(df_pam_in) %in% c("genes","class"))])
            
            pam_opt_p <- fviz_nbclust(df_pam_in[, -which(names(df_pam_in) %in% c("genes","class"))], 
                          pam, method = method_opt_clus ,
                          diss =dist_pamy, k.max=10 )

            pam_opt_p <- ggplotly(pam_opt_p)
             
            
            pam_opt_p
            
          })
          
          #################################################
          
          pam_centers <- input$pam_centers
          print("//////////////////////////")
          print(pam_centers)
          
          res_pam_clus <- pam(df_pam_in[, -which(names(df_pam_in) %in% c("genes","class"))]
                            , pam_centers, metric = "euclidean")
          
          df_pam_in$cluster <-(unname(res_pam_clus$clustering)) 
          
          print(df_pam_in)
          
          temp_df_pam <- rownames(df_pam_in)
          temp_df_pam <- cbind(temp_df_pam,  
                                  df_pam_in$cluster, rep("pam", length(df_pam_in$cluster) ) )
          
          temp_df_pam <- as.data.frame(temp_df_pam)
          
          
          names(temp_df_pam) <- c("genes", "cluster", "clus_method")
          print(temp_df_pam)
          
          params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_pam)
          print(params$df_clus_genes)
          
          
          
          
          ################    Table
          
          output$table_pam_val <- renderDT({
            
            dist_pamy <- as.dist(df_pam_in[, -which(names(df_pam_in) %in% c("genes","class", "cluster"))])
            
            pam_stat<-cluster.stats(d = dist_pamy,  res_pam_clus$clustering, alt.clustering = NULL,
                                      noisecluster=FALSE, silhouette = TRUE, G2 = FALSE, 
                                      G3 = FALSE, wgap=TRUE, sepindex=TRUE, 
                                      sepprob=0.1, sepwithnoise=TRUE, compareonly = FALSE, aggregateonly = FALSE)
            
            pam_table<-c()
            pam_table<-rbind(pam_table,pam_stat$cluster.size, pam_stat$diameter,
                               (pam_stat$average.distance), 
                               pam_stat$median.distance,pam_stat$separation, pam_stat$clus.avg.silwidths
                               , pam_stat$average.between, pam_stat$average.within
                               ,pam_stat$max.diameter, pam_stat$min.separation, pam_stat$avg.silwidth
                               , pam_stat$dunn, pam_stat$dunn2)
            
            
            row.names(pam_table)<-c("Cluster size", "Diameter",  "Average distance", 
                                      "Median distance","Separation", "Clus avg. silwidths"
                                      , "Average between","Average within"
                                      ,"Max diameter","Min separation", "Avg. silwidth","Dunn","Dunn2"
            )
            colnames(pam_table)<-c(paste("cluster",1:input$pam_centers,  sep = "."))
            
            pam_table<-round(pam_table,3)
            return(pam_table)
            
          })
          
          #######################################################
          # Silhouette plot
          
          output$plot_pam_sil <- renderPlotly({
            
            # silhouette Plot
            dist_pam <- as.dist(df_pam_in[, -which(names(df_pam_in) %in% c("genes","class", "cluster"))])
            pam_sil <- silhouette(df_pam_in$cluster, dist_pam)
            fviz_silhouette(pam_sil)
            
          })
          
          
          
          
          ######################################################################
          
          
          dd <- fviz_cluster(res_pam_clus, df_pam_in[, -which(names(df_pam_in) %in% c("genes","class", "cluster"))], 
                              alpha = 0.9, shape = 19, geom = c("point"), ellipse = FALSE, 
                              main = paste("Clustering for ", input$df_for_pam,sep = "" ) ) 
           pam_plot <- ggplotly(dd)
           pam_plot


          

          
        }else{
          NULL
        }
        
        
      
      
      
    }else{
      NULL
    }
    
    
  })
  
  #############################################
  
  output$plot_pam <- renderPlotly({
    pam_ploting()
    
  })
  

  
  ##### Agglomerative Hierarchical Clustering (AHC) clustering
  
  hca_ploting <- eventReactive(input$hca_para, { 


       if(!is.null(params$df_similarty)){
         
         # print(params$df_similarty)
         #print("===========================")
         #df_sel <- input$df_for_clus
         
         
         if(input$df_for_hca == "df_similarty"){
           
           df_hca_in <-params$df_similarty
           print("999999999999999999")
           print(df_hca_in)
           
         }
         if(input$df_for_hca == "df_pcoa"){
           
           df_hca_in <-params$df_pcoa
           print("////////////////////////")
           print(df_hca_in)
           
         }
         if(input$df_for_hca == "df_tsne"){
           
           df_hca_in <-params$df_tsne
           print("*******************")
           print(df_hca_in)
           
         }
    
         print(df_hca_in)
        
        
        if(!is.null(df_hca_in)){
          
          #optimal number of clusters
          ####################################
          output$plot_hca_opt <- renderPlotly({
            method_opt_clus <- "silhouette"

            dist_hca <- as.dist(df_hca_in[, -which(names(df_hca_in) %in% c("genes","class"))])
            
            #dist_hca <- as.dist(df_hca_in)
            hca_opt_p <- fviz_nbclust(df_hca_in[, -which(names(df_hca_in) %in% c("genes","class"))], 
              hcut, method = method_opt_clus ,
                                    diss =dist_hca, k.max=10 )
            
            hca_opt_p <- ggplotly(hca_opt_p)
            hca_opt_p
            
          })
          
          #################################################
          
          linkage_hca <- input$linkage_hca
          print("//////////////////////////")
          print(linkage_hca)
          
          res_hca_clus <- agnes(df_hca_in[, -which(names(df_hca_in) %in% c("genes","class"))], 
                              method = linkage_hca, metric = "euclidean", stand = TRUE)
          clus <- cutree(res_hca_clus, k = input$hca_centers)
          
          df_hca_in$cluster <-(unname(clus)) 
          
          print(df_hca_in)
          
          
          temp_df_hca <- rownames(df_hca_in)
          temp_df_hca <- cbind(temp_df_hca,  
                               df_hca_in$cluster, rep("hca", length(df_hca_in$cluster) ) )
          
          temp_df_hca <- as.data.frame(temp_df_hca)
          names(temp_df_hca) <- c("genes", "cluster", "clus_method")
          
          print(temp_df_hca)
          
          params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_hca)
          print(params$df_clus_genes)
          
          
          
          ################    Table
          
          output$table_hca_val <- renderDT({
            
            dist_hca <- as.dist(df_hca_in[, -which(names(df_hca_in) %in% c("genes","class", "cluster"))])
            
            hca_stat<-cluster.stats(d = dist_hca,  df_hca_in$cluster, alt.clustering = NULL,
                                      noisecluster=FALSE, silhouette = TRUE, G2 = FALSE, 
                                      G3 = FALSE, wgap=TRUE, sepindex=TRUE, 
                                      sepprob=0.1, sepwithnoise=TRUE, compareonly = FALSE, aggregateonly = FALSE)
            
            hca_table<-c()
            hca_table<-rbind(hca_table,hca_stat$cluster.size, hca_stat$diameter,
                               (hca_stat$average.distance), 
                             hca_stat$median.distance,hca_stat$separation, hca_stat$clus.avg.silwidths
                               , hca_stat$average.between, hca_stat$average.within
                               ,hca_stat$max.diameter, hca_stat$min.separation, hca_stat$avg.silwidth
                               , hca_stat$dunn, hca_stat$dunn2)
            
            
            row.names(hca_table)<-c("Cluster size", "Diameter",  "Average distance", 
                                      "Median distance","Separation", "Clus avg. silwidths"
                                      , "Average between","Average within"
                                      ,"Max diameter","Min separation", "Avg. silwidth","Dunn","Dunn2"
            )
            colnames(hca_table)<-c(paste("cluster",1:input$hca_centers,  sep = "."))
            
            hca_table<-round(hca_table,3)
            return(hca_table)
            
          })
          
          #######################################################
          # Silhouette plot
          
          output$plot_hca_sil <- renderPlotly({


            
            # silhouette Plot
            dist_hca <- as.dist(df_hca_in[, -which(names(df_hca_in) %in% c("genes","class", "cluster"))])
            hca_sil <- silhouette(df_hca_in$cluster, dist_hca)
            fviz_silhouette(hca_sil)
            
          })
          
          
          
          
          ######################################################################


          # dd <- fviz_cluster(list(data = df_hca_in[, -which(names(df_hca_in) %in% c("genes","class", "cluster"))], 
          #   cluster = df_hca_in$cluster))
          
          dd <- fviz_cluster(list(data = df_hca_in[, -which(names(df_hca_in) %in% c("genes","class", "cluster"))],

                                cluster = df_hca_in$cluster),

                              alpha = 0.9, shape = 19, geom = c("point"), ellipse = FALSE, 
                              main = paste("Clustering for ",input$df_for_clus,sep = "" ) ) 


          # dd <- fviz_cluster(res_hca_clus, 
          #     df_hca_in[, -which(names(df_hca_in) %in% c("genes","class", "cluster"))],

          #                     alpha = 0.9, shape = 19, geom = c("point"), ellipse = FALSE, 
          #                     main = paste("Clustering for ",input$df_for_clus,sep = "" ) ) 
           hca_plot <- ggplotly(dd)
          
          
          
          hca_plot
          
          
        }else{
          NULL
        }
        
        
      
      
      
    }else{
      NULL
    }
    
    
  })
  
  #############################################
  
  output$plot_hca <- renderPlotly({
    hca_ploting()
    
  })
  
  
  
  ##### fuzzy clustering
  
  fuzzy_ploting <- eventReactive(input$fuzzy_para, { 
    
    

        if(!is.null(params$df_similarty)){
         
         # print(params$df_similarty)
         #print("===========================")
#         df_sel <- input$df_for_clus
         
         
         if(input$df_for_hcd == "df_similarty"){
           
           df_fuzzy_in <-params$df_similarty
           print("999999999999999999")
           print(df_fuzzy_in)
           
         }
         if(input$df_for_hcd == "df_pcoa"){
           
           df_fuzzy_in <-params$df_pcoa
           print("////////////////////////")
           print(df_fuzzy_in)
           
         }
         if(input$df_for_hcd == "df_tsne"){
           
           df_fuzzy_in <-params$df_tsne
           print("*******************")
           print(df_fuzzy_in)
           
         }
         
         
         #df_kmeans_in <-as.data.frame(params$df_pcoa) 
         print("-------------------------------------")
         print(df_fuzzy_in)

      
      # print(params$df_similarty)
      # print("===========================")
      # df_sel <- input$df_for_clus
      # print(df_sel)
      
      # ifelse(input$df_for_clus == "df_similarty",df_fuzzy_in <-as.data.frame(params$df_similarty),
      #        ifelse(input$df_for_clus == "df_pcoa",df_fuzzy_in <-as.data.frame(params$df_pcoa),
      #               ifelse(input$df_for_clus == "df_umap",df_fuzzy_in <-as.data.frame(params$df_umap),
      #                      ifelse(input$df_for_clus == "df_tsne",df_fuzzy_in <-as.data.frame(params$df_tsne),
      #                             ifelse(input$df_for_clus == "df_sam",df_fuzzy_in <-as.data.frame(params$df_sam),
      #                                    ifelse(input$df_for_clus == "df_kra",df_fuzzy_in <-as.data.frame(params$df_kra)
      #                                    ))))))
      
      

      #   print(df_fuzzy_in)
        
        
        if(!is.null(df_fuzzy_in)){
          
          #optimal number of clusters
          ####################################
          output$plot_fuzzy_opt <- renderPlotly({
            method_opt_clus <- "silhouette"

            dist_fanny <- as.dist(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class"))])
            

            f_opt_p <- fviz_nbclust(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class"))], 
                                fanny, method = method_opt_clus ,
                                 diss =dist_fanny, k.max=10 )
            f_opt_p <- ggplotly(f_opt_p)
            
            f_opt_p

          })
          
          #################################################
          dis_fuzzy <- input$dis_fuzzy
          print("//////////////////////////")
          print(dis_fuzzy)
          dist_fanny <- as.dist(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class"))])
          
          res_fanny_clus <- fanny(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class"))],
                                  , metric = dis_fuzzy, k=input$fuzzy_centers)
          

          df_fuzzy_in$cluster <-(unname(res_fanny_clus$clustering)) 
          
          
          #print(df_fuzzy_in)
          
          temp_df_fuzzy <- rownames(df_fuzzy_in)
          temp_df_fuzzy <- cbind(temp_df_fuzzy,  
                               df_fuzzy_in$cluster, rep("fuzzy", length(df_fuzzy_in$cluster) ) )
          
          temp_df_fuzzy <- as.data.frame(temp_df_fuzzy)
          names(temp_df_fuzzy) <- c("genes", "cluster", "clus_method")
          
          #print(temp_df_fuzzy)
          
          params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_fuzzy)
          #print(params$df_clus_genes)
          
          
          
          ################    Table
          
          output$table_fuzzy_val <- renderDT({
            
            
            dist_fanny <- as.dist(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class", "cluster"))])
            
            fuzzy_stat<-cluster.stats(d = dist_fanny,  df_fuzzy_in$cluster, alt.clustering = NULL,
                                noisecluster=FALSE, silhouette = TRUE, G2 = FALSE, 
                                G3 = FALSE, wgap=TRUE, sepindex=TRUE, 
                                sepprob=0.1, sepwithnoise=TRUE, compareonly = FALSE, aggregateonly = FALSE)
            
            fuzzy_table<-c()
            fuzzy_table<-rbind(fuzzy_table,fuzzy_stat$cluster.size, fuzzy_stat$diameter,
                               (fuzzy_stat$average.distance), 
                               fuzzy_stat$median.distance,fuzzy_stat$separation, fuzzy_stat$clus.avg.silwidths
                     , fuzzy_stat$average.between, fuzzy_stat$average.within
                     ,fuzzy_stat$max.diameter, fuzzy_stat$min.separation, fuzzy_stat$avg.silwidth
                     , fuzzy_stat$dunn, fuzzy_stat$dunn2)
            

            row.names(fuzzy_table)<-c("Cluster size", "Diameter",  "Average distance", 
                            "Median distance","Separation", "Clus avg. silwidths"
                            , "Average between","Average within"
                            ,"Max diameter","Min separation", "Avg. silwidth","Dunn","Dunn2"
                            )
            colnames(fuzzy_table)<-c(paste("cluster",1:input$fuzzy_centers,  sep = "."))
            
            fuzzy_table<-round(fuzzy_table,3)
            return(fuzzy_table)

          })
          
          #######################################################
          # Silhouette plot
          output$plot_fuzzy_sil <- renderPlotly({
            
            # silhouette Plot
            dist_fanny <- as.dist(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class", "cluster"))])
            fuzzy_sil <- silhouette(df_fuzzy_in$cluster, dist_fanny)
            fviz_silhouette(fuzzy_sil)
            
          })
          
          
          
          
          ######################################################################
          
          
          
          dd <- fviz_cluster(res_fanny_clus, 
                               
                              alpha = 0.9, shape = 19, geom = c("point"), ellipse = FALSE, 
                              main = paste("Clustering for ", input$df_for_hcd,sep = "" ) ) 

           fuzzy_plot <- ggplotly(dd)
           fuzzy_plot
          

          
          
        }else{
          NULL
        }
        
        
      
      
    }else{
      NULL
    }
    
    
  })
  
  #############################################
  
  output$plot_fuzzy <- renderPlotly({
    fuzzy_ploting()
    
  })
  
  # optimal clusters
  # fuzzy_opt_ploting <- eventReactive(input$fuzzy_para, {
  #   req(fuzzy_ploting())
  #   
  #   output$plot_fuzzy_opt <- renderPlotly({
  #     
  #     method_opt_clus <- "silhouette"
  #     f_opt_p <- fviz_nbclust(USArrests, hcut, method = method_opt_clus ,
  #                        diss =gdist, k.max=7 )
  #     
  #     aa$data$y
  #     
  #     max(aa$data$y)
  #     
  #     
  #   }) 
  # })
  
  
  
  
  ##### Functional enrichemnt analysis
  
  output$fea_clus_in <- renderUI({
    
    cluster_method <-c(unique(params$df_clus_genes[,3]))
    names(cluster_method) <- c(cluster_method)
    #cluster_method <- list(cluster_method)
    print(cluster_method)
    
    # selectizeInput(("fea_clus_type"), label="Select the cluster method:", 
    #                choices=cluster_method, selected = NULL, multiple = F, 
    #                options = list(maxItems = 1, placeholder = 'select 1 method max'))
    
    selectInput("fea_clus_type", label = "Select the cluster method:",
                choices = c(cluster_method))

    print(unique(params$df_clus_genes[,3]))
    
  })
  
  output$fea_clus_num <- renderUI({
    
    #if(!is.null(input$fea_clus_type)){
      checkboxGroupInput("fea_clus_sel", label = "Choose the clusters",
                         choiceNames = c(unique(params$df_clus_genes[,2])),
                         choiceValues = c(unique(params$df_clus_genes[,2])),
                         )
    # print(params$df_clus_genes[1, 
    #                            params$df_clus_genes[2] %in% c(1,2) & 
    #                              params$df_clus_genes[3]== "kmeans"])
    #}
  })
  
    
} # end of servre






# Run the application 
shinyApp(ui = ui, server = server)
