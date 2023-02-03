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
library(org.Hs.eg.db)
library(GOSemSim)
library(tidyr)
library(MASS)
library(Rtsne)
library(cluster)
library(vegan)
library(mclust)
library(plyr)
library(clValid)
library("magrittr")
library(factoextra)
library(fpc)
library(dplyr)
library(shinyalert)
library(umap)


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
        tabPanel(div(fa("syringe", fill = "#158cba"), "Import"),
                 
                 
          #to add the space
          fluidRow(tags$hr(), tags$hr()),

          fluidRow( 
            
            column(width=6,
                        box(title = "Import input file", height = NULL, width = NULL, solidHeader = TRUE, status =
                         "primary",
                            collapsible = T, collapsed = TRUE,

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
              box(title = "Uploaded input data", height = NULL, width = NULL, solidHeader = TRUE, status =
               "primary", 
                           collapsible = T,collapsed = TRUE,
                           
                           DT::dataTableOutput("outFile1"),
                           br(),
                           br()
                       )#box
          )

          
          ),# endo fo fluid row,#endo fo row

          fluidRow(
                     fluidRow(
                       
                       column(width = 6,
                              box(solidHeader = TRUE,title = "Genes per class (N)", collapsible = TRUE, collapsed = TRUE,
                                  height = NULL, width = NULL, status = "primary",
                                  plotlyOutput("histPlot") %>% withSpinner(color="#0dc5c1")
                              )
                       ),
                       column(width = 6,
                              box(solidHeader = TRUE,title = "Shared genes", collapsible = TRUE, collapsed = TRUE,
                                  height = NULL, width = NULL, status = "primary",
                                  plotOutput("venPlot") %>% withSpinner(color="#0dc5c1")
                              )
                              
                       )
                     )# end of row

          )# end of row
                 
                 
        ), #import tabpanle
        tabPanel(div(fa("syringe", fill = "#158cba"), "Functional Similarities (FunSim) Module"),
                 
                 fluidRow(

                  column(width=12,
                        box(title = "Semnatic similarty measures", height = NULL, solidHeader = TRUE, status = "primary",
                            collapsible = T,

                            prettyRadioButtons(

                              inputId = "ssMethod",
                              label = "Select the semantic similarty measure:",
                              outline = TRUE, shape = "round", 
                              choices = c(Resnik="Resnik", Wang="Wang", Lin="Lin", Jiang="Jiang", Rel="Rel"),
                              inline = TRUE
                            ),
                            br(),
                       
                        #aggregating mtheod for genes
                       
                            prettyRadioButtons(
                              inputId = "ssMeasure",
                              label = "Select combining criterion to calculate final semantic similarty score:",
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
                       # conditionalPanel(condition = "input.ssRun",
                                         box(title = "Genes similarty matrix", 
                                             height = NULL, width = NULL, solidHeader = TRUE, status = "primary",
                                             collapsible = T, collapsed = TRUE,
                                             
                                             DT::dataTableOutput("ssm")%>% withSpinner(color="#0dc5c1"),
                                             br(),
                                             br()
                                         )#box
                                         #)#cp
                 )

               ) #fluidRow

        ), # SSM tab
        tabPanel(div(fa("syringe", fill = "#158cba"), 
                     "Dimension Reduction and Visualization (DimReduct) Module"),
                 

                 fluidRow(
                   br(),
                   column(width=6,
                          box(solidHeader = TRUE,title = "Principal Coordinates Analysis (PCoA)", collapsible = TRUE, 
                              collapsed = TRUE,
                              height = NULL, width = NULL, status = "success",
                              numericInput("pcoa_num_dim", "Number of dimensions", 2, min = 1, max = 10),
                              uiOutput("pcoa_dim1"),
                              uiOutput("pcoa_dim2"),
                              
                              
                              br(),
                              actionButton("pcoa_para", "Run"),
                              br(),
                              br(),
                              
                              conditionalPanel(condition = "input.pcoa_para",
                                               #set parameters
                                               box(solidHeader = TRUE, title = " PCoA 2D Plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "success",
                                                   plotlyOutput("plot_pcoa") %>% withSpinner(color="#0dc5c1")
                                                   
                                               ),
                                               
                                               
                                               
                                               
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
                              actionButton("tsne_para", "Run"),
                              br(),
                              br(),
                              
                              #set parameters
                              conditionalPanel(condition = "input.tsne_para",
                                               
                                               box(solidHeader = TRUE, title = "T-SNE 2D Plot", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "info",
                                                   plotlyOutput("plot_tsne") %>% withSpinner(color="#0dc5c1")
                                                   
                                               )
                                               
                              )
                              
                              
                          )
                          
                          
                   )
                   

                 ),#fluidrow
                 
                  fluidRow(
                    br(),
                    column(width=6,
                 
                           box(solidHeader = TRUE,title = "UMAP", collapsible = TRUE,
                               collapsed = TRUE,
                               height = NULL, width = NULL, status = "warning",
                               numericInput("umap_num_dim", "Number of dimensions", 2, min = 1, max = 10),
                               uiOutput("umap_dim1"),
                               uiOutput("umap_dim2"),
                 
                 
                               br(),
                               actionButton("umap_para", "Run"),
                               br(),
                               br(),
                 
                               #set parameters
                               conditionalPanel(condition = "input.umap_para",
                 
                                                box(solidHeader = TRUE, title = "UMAP 2D Plot", collapsible = TRUE, collapsed = TRUE,
                                                    height = NULL, width = NULL, status = "info",
                                                    plotlyOutput("plot_umap") %>% withSpinner(color="#0dc5c1")
                 
                                                )#,
                 
                 
                                                )
                 
                 
                          )
                 
                 
                    )
                    ,
                    column(width=6,
                           box(solidHeader = TRUE,title = "Principal Component Analysis (PCA)", collapsible = TRUE, collapsed = TRUE,
                               height = NULL, width = NULL, status = "danger",
                               
                               numericInput("pca_num_dim", "Number of dimensions", 2, min = 1, max = 50),
                               uiOutput("pca_dim1"),
                               uiOutput("pca_dim2"),
                              
                               
                               
                               br(),
                               actionButton("pca_para", "Run"),
                               br(),
                               br(),
                               
                               #set parameters
                               conditionalPanel(condition = "input.pca_para",
                                                
                                                box(solidHeader = TRUE, title = "PCA 2D Plot", collapsible = TRUE, collapsed = TRUE,
                                                    height = NULL, width = NULL, status = "danger",
                                                    plotlyOutput("plot_pca") %>% withSpinner(color="#0dc5c1")
                                                    
                                                ),
                                                box(solidHeader = TRUE, title = "Elbow Plot", collapsible = TRUE, collapsed = TRUE,
                                                    height = NULL, width = NULL, status = "danger",
                                                    plotOutput("plot_pca_elbow") %>% withSpinner(color="#0dc5c1")
                                                    
                                                )
                                                
                                                
                               )
                               
                               
                           )
                    )
                    
                  )#fluidrow
        ),
        tabPanel(div(fa("syringe", fill = "#158cba"), "Clustering the functional similar genes (ClusFunSim) module"),
                 
                 fluidRow(
                   column(width=6,
                          box(solidHeader = TRUE,  title = "K-means", collapsible = TRUE, collapsed = TRUE,
                              height = NULL, width = NULL, status = "primary",
                              
                              selectInput("df_for_clus", "Select the data type for clustering:",
                                          c("Semantic Similarty matrix" = "df_similarty")),
                              numericInput("Kmeans_centers", "Number of clusters", 2, min = 1, max = 20),
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
                                               ),
                                               box(solidHeader = FALSE, title = "Clustering table", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "primary",
                                                   DT::dataTableOutput("table_kmeans_clus") %>% withSpinner(color="#0dc5c1")
                                               )


                                               )#conditional panel

                          )#first box
                  )#first column
                  ,
                  column(width=6,
                         box(solidHeader = TRUE,  title = "PAM Clustering", collapsible = TRUE, collapsed = TRUE,
                             height = NULL, width = NULL, status = "success",
                             
                             selectInput("df_for_pam", "Select the data type for clustering:",
                                         c("Semantic Similarty matrix" = "df_similarty"
                                           
                                         )),
                             
                            
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
                                              ),
                                              box(solidHeader = FALSE, title = "Clustering table", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "success",
                                                   DT::dataTableOutput("table_pam_clus") %>% withSpinner(color="#0dc5c1")
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
                                          c("Semantic Similarty matrix" = "df_similarty"
                                            
                                            
                                          )),
                              selectInput("linkage_hca", "Select linkage method for clustering:",
                                          c("Average" = "average",
                                            "Single" = "single",
                                            "Complete" = "complete",
                                            "Ward" = "ward",
                                            "Flexible" = "flexible",
                                            "gaverage" = "gaverage"
                                            
                                          ), selected="average"),
                             
                              numericInput("hca_centers", "Number of clusters", 2, min = 1, max = 20),
                              
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
                                                   
                                               
                                               
                                               ),
                                               box(solidHeader = FALSE, title = "Clustering table", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "danger",
                                                   DT::dataTableOutput("table_hca_clus") %>% withSpinner(color="#0dc5c1")
                                               )

                              )#cp
                          )#first box
                   )#first column
                   ,
                   column(width=6,
                          box(solidHeader = TRUE,  title = "Fuzzy Clustering", collapsible = TRUE, collapsed = TRUE,
                              height = NULL, width = NULL, status = "warning",
                              
                              selectInput("df_for_hcd", "Select the data type for clustering:",
                                          c("Semantic Similarty matrix" = "df_similarty"
                                            
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
                                               ),

                                               box(solidHeader = FALSE, title = "Clustering table", collapsible = TRUE, collapsed = TRUE,
                                                   height = NULL, width = NULL, status = "warning",
                                                   DT::dataTableOutput("table_fuzzy_clus") %>% withSpinner(color="#0dc5c1")
                                               )

                                               
                                               
                                               )#cp
                          )#first box
                   )#2nd column
                   
                 )#2nd row row
        )#,# clustering tab end
        

      ) # tabeset panel
      

    ) # fluidpageinstall.packages("packageName")
    
  ))# shiny dashboard




hsGO2 <- godata('org.Hs.eg.db', keytype = "SYMBOL", ont="BP", computeIC=TRUE) 





server <- function(input, output) {
  
  params <- reactiveValues(df_in = NULL, # input data model
                           df_similarty = NULL, # semantic similarty matrix
                           df_pcoa = NULL,
                           df_tsne = NULL, 
                           df_sam = NULL,
                           df_kra = NULL,
                           df_pca=NULL,
                           df_umap=NULL,
                           df_clus_genes = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("genes",
                            "cluster", "clus_method")))
  
 
  output$outFile1 <- DT::renderDataTable({
    req(input$inFile1)
    
    
    df <- read.csv(input$inFile1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    params$df_in <- df
    
    if(input$disp == "head") {
      return(df)
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
           y = colnames(bar_data)[2]))+ geom_bar(stat="identity") + xlab("Class") + ylab("")
       #convert the plot to plotlyplot
       p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
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
	                   modifier = list(selected = FALSE, 
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
                           		order= 'index', page= 'all', search='none'))
                         )
                         
                       )
                     )
                     
            
      semsimMat <-as.data.frame(mgeneSim(unique(params$df_in$genes),
                                       semData=hsGO2,
                                       measure=input$ssMethod,
                                       combine=input$ssMeasure,
                                       verbose=TRUE))
     # 
      sim_dist <- 1- semsimMat
      sim_dist$genes <- rownames(semsimMat)

      semsimMat$genes <- rownames(semsimMat)


      #params$df_in <- aggregate(params$df_in[,2], list(params$df_in[,1]), function(x) paste0(unique(x)))
      #colnames(params$df_in) <- c('genes', 'class')

      params$df_in <- ddply(params$df_in, .(genes), summarize, class=paste(unique(class), collapse = ","))

      sim_dist <- merge(sim_dist, params$df_in, by="genes", all.x=TRUE, all.y=FALSE)

      semsimMat <- merge(semsimMat, params$df_in, by="genes", all.x=TRUE, all.y=FALSE)



      rownames(semsimMat) <- NULL
      params$df_similarty <-as.data.frame(sim_dist) 
      

     datatable(
      
       semsimMat,
       #caption = "Differentially expressed genes",
       rownames = TRUE,
       filter = 'top',
       extensions = c("Buttons", "Select"),
       options = exportOptions
       
     )
     
    })
    
    })
  
  ####principal coordinates analysis
  
  output$pcoa_dim1<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$pcoa_num_dim)){
      selectizeInput(("pcoa_dim_list1"), "Select dimension for x-axis:", 
                     seq.int(1, input$pcoa_num_dim), selected = 1, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  output$pcoa_dim2<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$pcoa_num_dim)){
      selectizeInput(("pcoa_dim_list2"), "Select dimension for y-axis:", 
                     seq.int(1, input$pcoa_num_dim), selected = 2, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  ### pcooa analysis
  pcoa_ploting <- eventReactive(input$pcoa_para, {

    
     dist_mt <- as.dist(params$df_similarty[, -which(names(params$df_similarty) %in% c("genes","class"))])
      pcooa<-cmdscale(dist_mt, k = input$pcoa_num_dim)

      params$df_pcoa <-as.data.frame(pcooa)

      params$df_pcoa[['genes']] <- rownames(params$df_pcoa)
      params$df_pcoa[['class']] <- params$df_similarty$class 
      

      #select the data for visualze
      x <- pcooa[,as.numeric(input$pcoa_dim_list1)]
      y <- pcooa[,as.numeric(input$pcoa_dim_list2)]


      plot_df <- cbind(round(x, 3),round(y, 3) , params$df_similarty$class)
      plot_df <- as.data.frame(plot_df)
      names(plot_df)[3] <- "class"

     

      p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
        labs(x = as.character(paste("Dimension", input$pcoa_dim_list1, " ")),
             y = as.character(paste("Dimension", input$pcoa_dim_list2, " ")))  + theme_classic()
      p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions


      p2



  })

   output$plot_pcoa <- renderPlotly({
     pcoa_ploting()

  })

  ### PCA analysis
  
  
  
  
  output$pca_dim1<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$pca_num_dim)){
      selectizeInput(("pca_dim1List"), "Select PC component to visualize  on x-axis:", 
                     seq.int(1, input$pca_num_dim), selected = 1, multiple = T, 
                     options = list(maxItems = 1, placeholder = 
                                      'select 1 PC component max'))
      
    }
    
  }) 
  
  output$pca_dim2<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$pca_num_dim)){
      selectizeInput(("pca_dim2List"), "Select PC component to visualize  on y-axis:", 
                     seq.int(1, input$pca_num_dim), selected = 2, multiple = T, 
                     options = list(maxItems = 1, placeholder = 
                                      'select 1 PC component max'))
      
    }
    
  }) 
 
  
 
  
  pca_ploting <- eventReactive(input$pca_para, {
    
    
    ss_mt <- (params$df_similarty[, -which(names(params$df_similarty) %in% 
                                             c("genes","class"))])
    
    pca_res<-prcomp(ss_mt, rank = input$pca_num_dim)
    
    
    
    
    all_pcs <- pca_res$x
    
      
    # add variable for pcs component
    temp_1 <-as.data.frame(all_pcs[, 1:input$pca_num_dim])
    
        
    temp_1[['genes']] <- rownames(temp_1)
    temp_1[['class']] <- params$df_similarty$class
    
    
    
    params$df_pca <- temp_1
    
    #select the data for visualze
    x <- temp_1[,paste("PC", input$pca_dim1List, sep = "")]
    y <- temp_1[,paste("PC", input$pca_dim2List, sep = "")]
    
    plot_df <-c()
    plot_df <- cbind(round(x, 3),round(y, 3) , params$df_similarty$class)
    
    
    plot_df <- as.data.frame(plot_df)
    names(plot_df)[3] <- "class"
    
    
    p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
      labs(x = as.character(paste("Dimension", input$pca_dim1List, " ")),
           y = as.character(paste("Dimension", input$pca_dim2List, " ")))  + theme_classic()
    p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions
    
    
 
       output$plot_pca_elbow <- renderPlot({

         pp <-fviz_eig(pca_res, ncp = input$pca_num_dim)
         pp


       })
    p2
    
  })
  

  output$plot_pca <- renderPlotly({
    pca_ploting()
    
  })
  
  
  #### TSNE analysis
  
  output$tsne_dim1<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$tsne_num_dim)){
      selectizeInput(("tsne_dim_list1"), "Select dimension for x-axis:", 
                     seq.int(1, input$tsne_num_dim), selected = 1, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  output$tsne_dim2<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$tsne_num_dim)){
      selectizeInput(("tsne_dim_list2"), "Select dimension for y-axis:", 
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
    
    #to store the projection
    params$df_tsne <-as.data.frame(tsne_mod$Y )

    params$df_tsne[['genes']] <- params$df_similarty$genes
    params$df_tsne[['class']] <- params$df_similarty$class 
   
    
    x <- tsne_mod$Y[, as.numeric(input$tsne_dim_list1)]
    
    y <- tsne_mod$Y[, as.numeric(input$tsne_dim_list2)]
    
    
    
    plot_df <- cbind(round(x, 3),round(y, 3) , params$df_similarty$class)
    
    plot_df <- as.data.frame(plot_df)
    names(plot_df)[3] <- "class"
    
    
    p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
      labs(x = as.character(paste("Dimension", input$tsne_dim_list1, " ")),
           y = as.character(paste("Dimension", input$tsne_dim_list2, " ")))  + theme_classic()
    p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions
    
    
 
    p2
    
    
  })
  
  #############################################
  
  output$plot_tsne <- renderPlotly({
    tsne_ploting()
    
  })
  
  #######
  
  #### UMAP analysis
  
  output$umap_dim1<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$umap_num_dim)){
      selectizeInput(("umap_dim_list1"), "Select dimension for x-axis:", 
                     seq.int(1, input$umap_num_dim), selected = 1, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  output$umap_dim2<- renderUI({
    
    #ns <- session$ns
    
    if(!is.null(input$umap_num_dim)){
      selectizeInput(("umap_dim_list2"), "Select dimension for y-axis:", 
                     seq.int(1, input$umap_num_dim), selected = 2, multiple = T, 
                     options = list(maxItems = 1, placeholder = 'select 1 dimension max'))
      
    }
    
  }) 
  
  # ##############################################
  ### Kruskal analysis
  umap_ploting<- eventReactive(input$umap_para, {
    
    
    dist_mt <- as.dist(params$df_similarty[, -which(names(params$df_similarty) %in% c("genes","class"))])
    
    umap_mod <- umap(as.matrix(dist_mt),  n_components =input$umap_num_dim)
    
    

    #to store the projection
    params$df_umap <-as.data.frame(umap_mod$layout )
    
    params$df_umap[['genes']] <- params$df_similarty$genes
    params$df_umap[['class']] <- params$df_similarty$class 
    
    
    x <- umap_mod$layout[, as.numeric(input$umap_dim_list1)]
    
    y <- umap_mod$layout[, as.numeric(input$umap_dim_list2)]
    
    
    
    plot_df <- cbind(round(x, 3),round(y, 3) , params$df_similarty$class)
    
    plot_df <- as.data.frame(plot_df)
    names(plot_df)[3] <- "class"
    
    
    
    p <- ggplot(data = plot_df, aes(x, y, color=class)) + geom_point(show.legend = F) +
      labs(x = as.character(paste("Dimension", input$umap_dim_list1, " ")),
           y = as.character(paste("Dimension", input$umap_dim_list2, " ")))  + theme_classic()
    p2 <- ggplotly(p, hoverinfo =plot_df)# save it as plotly # let the user to select the dimensions
    
    
   
    p2
    
    #}
    
    
  })
  
  #############################################
  
  output$plot_umap <- renderPlotly({
    umap_ploting()
    
  })
  
  
 

  kmeans_ploting <- eventReactive(input$kmeans_para, { 

       
       if(!is.null(params$df_similarty)){
         
         df_sel <- input$df_for_clus
         
         
         if(input$df_for_clus == "df_similarty"){

          dist_mt <- as.dist(params$df_similarty[, -which(names(params$df_similarty) %in% c("genes","class"))])
           
           df_kmeans_in <-params$df_similarty
           
         }
         if(input$df_for_clus == "df_pcoa"){
           
           df_kmeans_in <-params$df_pcoa
           
         }
         if(input$df_for_clus == "df_tsne"){
           
           df_kmeans_in <-params$df_tsne
 
         }
         
         
         
         if(!is.null(df_kmeans_in)){
           
           #optimal number of clusters
           ####################################
           output$plot_kmeans_opt <- renderPlotly({
             method_opt_clus <- "silhouette"
             dist_kmeans <- as.dist(1- df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class"))])
             kmeans_opt_p <- fviz_nbclust(
               df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class"))], 
               kmeans, method = method_opt_clus ,
               diss =dist_kmeans, k.max=10 )
             
             kmeans_opt_p <- ggplotly(kmeans_opt_p)
             kmeans_opt_p
             
           })
           
           
           Kmeans_centers <- input$Kmeans_centers
           
           
           res_kmeans_clus <- kmeans(df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class"))], 
                                     Kmeans_centers, iter.max = 10, nstart = 25)
           
           df_kmeans_in$cluster <-(unname(res_kmeans_clus$cluster)) 
           
           temp_df_kmeans <- df_kmeans_in$genes
           temp_df_kmeans <- cbind(temp_df_kmeans,  
                                   df_kmeans_in$cluster, rep("kmeans", length(df_kmeans_in$cluster) ) )
           temp_df_kmeans <- as.data.frame(temp_df_kmeans)
           names(temp_df_kmeans) <- c("genes", "cluster", "clus_method")
           
           
           
           params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_kmeans)
           
           
           ################    Table
           
           output$table_kmeans_val <- renderDT({
           
             
             dist_kmeans <- as.dist(df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class", "cluster"))])
             
             kmeans_stat<-cluster.stats(d = dist_kmeans,  res_kmeans_clus$cluster, alt.clustering = NULL,
                                        noisecluster=FALSE, silhouette = TRUE, G2 = FALSE, 
                                        G3 = FALSE, wgap=TRUE, sepindex=TRUE, 
                                        sepprob=0.1, sepwithnoise=TRUE, compareonly = FALSE, aggregateonly = FALSE)
             
             
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
           
           ####clustering table

           

 output$table_kmeans_clus <- renderDT({
            
            
            
            #return(df_fuzzy_in)



      exportOptions = list(
        select = TRUE,
        searching = TRUE,
               scrollX = TRUE,
               scrollY = TRUE,
               dom = "BRSpfrti",
               buttons = list(                           
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

            datatable(
      
                 df_kmeans_in[, c("genes", "class", "cluster")],
                 #caption = "Differentially expressed genes",
                 rownames = TRUE,
                 filter = 'top',
                 extensions = c("Buttons"),
                 options = exportOptions
       
     )

          })
           
           
           ######################################################################
           
           
           
           
           
           dd <- fviz_cluster(res_kmeans_clus, df_kmeans_in[, -which(names(df_kmeans_in) %in% c("genes","class", "cluster"))], 
                              alpha = 0.9, shape = 19, geom = c("point"), ellipse = FALSE, main = paste("Clustering for ",
                                                                                                        input$df_for_clus,sep = "" ) ) 
           kmeans_plot <- ggplotly(dd)
           kmeans_plot
           
           
           
           
           
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
     
     
  
  
  
 ##### PAM clustering
  
  pam_ploting <- eventReactive(input$pam_para, { 
    
       if(!is.null(params$df_similarty)){
         
         
         df_sel <- input$df_for_clus
          
         
         if(input$df_for_pam == "df_similarty"){
           
           df_pam_in <-params$df_similarty
           
           
         }
         if(input$df_for_pam == "df_pcoa"){
           
           df_pam_in <-params$df_pcoa
           
           
         }
         if(input$df_for_pam == "df_tsne"){
           
           df_pam_in <-params$df_tsne
           
           
         }
         
         
        
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
          
          
          res_pam_clus <- pam(df_pam_in[, -which(names(df_pam_in) %in% c("genes","class"))]
                            , pam_centers, metric = "euclidean")
          
          df_pam_in$cluster <-(unname(res_pam_clus$clustering)) 
          
          
          
          temp_df_pam <- rownames(df_pam_in)
          temp_df_pam <- cbind(temp_df_pam,  
                                  df_pam_in$cluster, rep("pam", length(df_pam_in$cluster) ) )
          
          temp_df_pam <- as.data.frame(temp_df_pam)
          
          
          names(temp_df_pam) <- c("genes", "cluster", "clus_method")
         
          
          params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_pam)
          
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
          
          ####clustering table



 output$table_pam_clus <- renderDT({
            
            
            
            
      exportOptions = list(
        select = TRUE,
        searching = TRUE,
               scrollX = TRUE,
               scrollY = TRUE,
               dom = "BRSpfrti",
               buttons = list(                           
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

            datatable(
      
                 df_pam_in[, c("genes", "class", "cluster")],
                 #caption = "Differentially expressed genes",
                 rownames = TRUE,
                 filter = 'top',
                 extensions = c("Buttons"),
                 options = exportOptions
       
     )

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
         
         
         
         
         if(input$df_for_hca == "df_similarty"){
           
           df_hca_in <-params$df_similarty
         
           
         }
         if(input$df_for_hca == "df_pcoa"){
           
           df_hca_in <-params$df_pcoa
         
           
         }
         if(input$df_for_hca == "df_tsne"){
           
           df_hca_in <-params$df_tsne
           
           
         }
    
         
        
        
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
          
          res_hca_clus <- agnes(df_hca_in[, -which(names(df_hca_in) %in% c("genes","class"))], 
                              method = linkage_hca, metric = "euclidean", stand = TRUE)
          clus <- cutree(res_hca_clus, k = input$hca_centers)
          
          df_hca_in$cluster <-(unname(clus)) 
          
          
          
          
          temp_df_hca <- rownames(df_hca_in)
          temp_df_hca <- cbind(temp_df_hca,  
                               df_hca_in$cluster, rep("hca", length(df_hca_in$cluster) ) )
          
          temp_df_hca <- as.data.frame(temp_df_hca)
          names(temp_df_hca) <- c("genes", "cluster", "clus_method")
          
         
          
          params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_hca)
         
          
          
          
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
          ##################### clustering table


          

 output$table_hca_clus <- renderDT({
            
            
            
            #return(df_fuzzy_in)



      exportOptions = list(
        select = TRUE,
        searching = TRUE,
               scrollX = TRUE,
               scrollY = TRUE,
               dom = "BRSpfrti",
               buttons = list(                           
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

            datatable(
      
                 df_hca_in[, c("genes", "class", "cluster")],
                 #caption = "Differentially expressed genes",
                 rownames = TRUE,
                 filter = 'top',
                 extensions = c("Buttons"),
                 options = exportOptions
       
     )

          })
          
          
          ######################################################################



          dd <- fviz_cluster(list(data = df_hca_in[, -which(names(df_hca_in) %in% c("genes","class", "cluster"))],

                                cluster = df_hca_in$cluster),

                              alpha = 0.9, shape = 19, geom = c("point"), ellipse = FALSE, 
                              main = paste("Clustering for ",input$df_for_clus,sep = "" ) ) 


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
         
         
         
         if(input$df_for_hcd == "df_similarty"){
           
           df_fuzzy_in <-params$df_similarty
         
           
         }
         if(input$df_for_hcd == "df_pcoa"){
           
           df_fuzzy_in <-params$df_pcoa
         
           
         }
         if(input$df_for_hcd == "df_tsne"){
           
           df_fuzzy_in <-params$df_tsne
           
           
         }
         
         

        
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
         
          dist_fanny <- as.dist(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class"))])

          
          
          res_fanny_clus <- fanny(df_fuzzy_in[, -which(names(df_fuzzy_in) %in% c("genes","class"))],
                                   k=input$fuzzy_centers)
          

          df_fuzzy_in$cluster <-(unname(res_fanny_clus$clustering)) 
          
          
          
          
          temp_df_fuzzy <- rownames(df_fuzzy_in)
          temp_df_fuzzy <- cbind(temp_df_fuzzy,  
                               df_fuzzy_in$cluster, rep("fuzzy", length(df_fuzzy_in$cluster) ) )
          
          temp_df_fuzzy <- as.data.frame(temp_df_fuzzy)
          names(temp_df_fuzzy) <- c("genes", "cluster", "clus_method")
          
         
          
          params$df_clus_genes <- rbind(params$df_clus_genes,temp_df_fuzzy)
          
          
          
          
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

          output$table_fuzzy_clus <- renderDT({
            
            
            
            #return(df_fuzzy_in)



      exportOptions = list(
        select = TRUE,
        searching = TRUE,
               scrollX = TRUE,
               scrollY = TRUE,
               dom = "BRSpfrti",
               buttons = list(                           
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

            datatable(
      
                 df_fuzzy_in[, c("genes", "class", "cluster")],
                 #caption = "Differentially expressed genes",
                 rownames = TRUE,
                 filter = 'top',
                 extensions = c("Buttons"),
                 options = exportOptions
       
     )

          })



          ##############################################################################
          
          
          
          dd <- fviz_cluster(res_fanny_clus,df_fuzzy_in,
                               
                              alpha = 0.9,, shape = 19,  geom = c("point"), ellipse = FALSE, 
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
  
  
  
  ##### Functional enrichemnt analysis
  
  output$fea_clus_in <- renderUI({
    
    cluster_method <-c(unique(params$df_clus_genes[,3]))
    names(cluster_method) <- c(cluster_method)
 
    selectInput("fea_clus_type", label = "Select the cluster method:",
                choices = c(cluster_method))

    
    
  })
  
  output$fea_clus_num <- renderUI({
    
    #if(!is.null(input$fea_clus_type)){
      checkboxGroupInput("fea_clus_sel", label = "Choose the clusters",
                         choiceNames = c(unique(params$df_clus_genes[,2])),
                         choiceValues = c(unique(params$df_clus_genes[,2])),
                         )
    
  })
  
    
} # end of servre






# Run the application 
shinyApp(ui = ui, server = server)
