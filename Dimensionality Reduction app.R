#Dimensionality Reduction  // Exploratory Analysis
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(readxl)
library(DT)
library(ggcorrplot)
library(SmartEDA)
library(mdatools)
library(ggplot2)
library(dplyr)
library(tibble)
library(plotly)
library(psych)
library(psychTools)
library(ica)
library(rrcov)
library(MASS)
library(Rtsne)
library(vegan)
library(kernlab)
library(DataExplorer)
library(recipes)
library(wavelets)
library(dendextend)
library(factoextra)
library(circlize)
library(NbClust)
library(gplots)
library(EnvStats)
library(outliers)
library(pheatmap)


options(shiny.maxRequestSize=1000*1024^2)
# Define UI for application that draws a histogram
ui <- dashboardPage(title = "UERJ-LEAMS Exploratory Analysis App",
                    
                    dashboardHeader(title = "UERJ Dimensionality reduction", titleWidth = "400"),
                    
                    dashboardSidebar(shiny::hr(), 
                                     sidebarMenu(
                                       #sidebarSearchForm("searchsidebar", "searchButton", "Search a Topic"),
                                       menuItem(tabName = "importtab", text = "Import Data", icon = icon("import", lib = "glyphicon")),
                                       menuItem(tabName = "fastprepro", text = "Pre-processing", icon = icon("chevron-right", lib = "glyphicon")),
                                       menuItem(tabName = "testtab", text = "Data Testing", icon = icon("chevron-right", lib = "glyphicon")),
                                       
                                       menuItem(tabName = "resolutionredctab", text = "Resolution Reduction", icon = icon("chevron-right", lib = "glyphicon")),
                                       
                                       menuItem(tabName = "filtermethodsstab", text = "Filtering and compression", icon = icon("chevron-right", lib = "glyphicon"),
                                                menuSubItem(tabName = "lowvariancetab", text = "Low-Variance Filter", icon = icon("menu-right", lib = "glyphicon")),
                                                menuSubItem(tabName = "highcorrtab", text = "High Correlation Filter", icon = icon("menu-right", lib = "glyphicon")),
                                                menuSubItem(tabName = "wavelettab", text = "Wavelet compression", icon = icon("menu-right", lib = "glyphicon"))
                                       ),
                                       
                                       menuItem(tabName = "clustertab", text = "Cluster Analysis", icon = icon("chevron-right", lib = "glyphicon")),
                                       menuItem(tabName = "redtab", text = "Exploratory analysis", startExpanded = T ,icon = icon("list", lib = "glyphicon"),
                                                menuItem(tabName = "linearmethodstab", text = "Parametric Methods", startExpanded = T ,icon = icon("chevron-right", lib = "glyphicon"),
                                                         menuSubItem(tabName = "pcatab", text = "PCA", icon = icon("menu-right", lib = "glyphicon")),
                                                         menuSubItem(tabName = "robpcatab", text = "Robust PCA", icon = icon("menu-right", lib = "glyphicon")),
                                                         menuSubItem(tabName = "FAtab", text = "FA", icon = icon("menu-right", lib = "glyphicon")),
                                                         menuSubItem(tabName = "ICAtab", text = "ICA", icon = icon("menu-right", lib = "glyphicon"))
                                                ),
                                                menuItem(tabName = "nonlinearmethodstab", text = "Non-Parametric Methods",icon = icon("chevron-right", lib = "glyphicon"),
                                                         menuSubItem(tabName = "kernelpcatab", text = "Kernel PCA", icon = icon("menu-right", lib = "glyphicon")),
                                                         menuSubItem(tabName = "tsnetab", text = "t-SNE", icon = icon("menu-right", lib = "glyphicon")),
                                                         menuSubItem(tabName = "MDStab", text = "MDS", icon = icon("menu-right", lib = "glyphicon")),
                                                         menuSubItem(tabName = "isomaptab", text = "Isomap", icon = icon("menu-right", lib = "glyphicon"))
                                                )
                                       ),
                                       
                                       menuItem(tabName = "linkstab", text = "About the Interface", icon = icon("info-sign", lib = "glyphicon"),
                                                menuSubItem(text = "LEAMS Group",href = 'https://www.leamsuerj.com/home', icon = icon("link", lib = "glyphicon")),
                                                menuSubItem(text = "UERJ Institute of Chemistry", href = 'http://www.iq.uerj.br/', icon = icon("link", lib = "glyphicon")),
                                                menuSubItem(text = "Our GitHub",href = 'https://github.com/Leams-UERJ-apps/Chemometrics-Web-Apps', icon = icon("link", lib = "glyphicon")),
                                                menuSubItem(tabName = "creditstab", text = "Credits", icon = icon("user", lib = "glyphicon")),
                                                menuSubItem(tabName = "referencestab", text = "References", icon = icon("list", lib = "glyphicon"))
                                       ),
                                       menuItem(tabName = "otherinterfaces", text = "Other interfaces by LEAMS", icon = icon("plus-sign", lib = "glyphicon"),
                                                menuSubItem(text = "Data Handling", href = 'https://leams-uerj-chemometrics.shinyapps.io/Data_Handling_app.', icon = icon("link", lib = "glyphicon"))
                                       )
                                       
                                       
                                     )
                    ),
                    
                    dashboardBody(
                      
                      tabItems(
                        
                        tabItem(tabName = "importtab", 
                                h2("Import Data"), 
                                shiny::hr(),
                                fluidRow(
                                  box(title=h3("Enter Data Settings"), 
                                      width = 4, 
                                      status = "primary",
                                      fileInput("file", "Data", buttonLabel = "Upload..."),
                                      shiny::hr(),
                                      checkboxInput("isspectra",strong("Check if the data is of spectral type.")),
                                      shiny::hr(),
                                      checkboxInput("classcol", strong(" Check if the data presents sample classes."), F),
                                      conditionalPanel(condition = "input.classcol == true", numericInput("classpos", "Select the class column position: (for .RData files and DEMOS, please ignore)", 2)),
                                      shiny::hr(),
                                      selectizeInput("datatype", "Select data type:", 
                                                     c(".txt"="txt", ".csv"="csv", "Excel"="xlsx",  "Interface Standard"="itfstd", "DEMOS"="demos","Select file type"=""), 
                                                     selected = ""),
                                      shiny::hr(),
                                      conditionalPanel(condition = "input.datatype == 'demos'",
                                                       selectInput("demodata",
                                                                   "Select a Demo data type:",
                                                                   choices = c("Iris dataset"="irisDEMO","Ceramic dataset"="ceramicDEMO", "UVVis Coffee dataset"="coffeeDEMO"))),
                                      conditionalPanel(condition = "input.datatype == 'txt'||input.datatype == 'csv'||input.datatype == 'csv'||input.datatype == 'xls'|| input.datatype == 'xlsx'",
                                                       checkboxInput("labels","Check if the variables have labels.",  F),
                                                       checkboxInput("namerows", "Check if the first column presents sample names.", F),
                                      ),
                                      
                                      conditionalPanel(condition = "input.datatype == 'txt'||input.datatype == 'csv'||input.datatype == 'csv'",
                                                       selectInput("delim", "Delimiter:",
                                                                   c("Comma"=",", "Semicolon"=";", "Tab" = "\t"), 
                                                                   selected = ","),
                                                       selectInput("dec", "Decimal mark:", 
                                                                   c("Dot"=".", "Comma"=","), 
                                                                   selected = ".")
                                      ),
                                      
                                      conditionalPanel(condition = "input.datatype == 'xls'|| input.datatype == 'xlsx'",
                                                       selectInput("excelsheet", "Select the sheet to be imported:", 
                                                                   choices = "")
                                      ),
                                      
                                      actionButton("preview" , "Preview and Use", class = "btn-block btn-success")
                                      
                                  ),
                                  
                                  box(title = h3("Data Overview"), 
                                      width = 8, 
                                      status = "primary", 
                                      
                                      tabsetPanel(
                                        tabPanel("Data Description",
                                                 tabsetPanel(id="dataPreview",
                                                             type = "hidden",
                                                             tabPanelBody("panelnormaldata", DTOutput("summary")),
                                                             tabPanelBody("panelspectrum", plotOutput("spectrumpreview")%>%withSpinner()
                                                             )
                                                 )
                                        ),
                                        tabPanel("NaN’s values positions", 
                                                 verbatimTextOutput("datanullvalues")
                                        ),
                                        tabPanel("Duplicated Values", 
                                                 verbatimTextOutput("checkdup")
                                        ),
                                        tabPanel("Remove Data",
                                                 shiny::hr(),
                                                 selectizeInput("removedatarow", "Select sample(s) to be removed from the dataset:", 
                                                                choices=NULL, 
                                                                multiple=T),
                                                 actionButton("removedatarowBT", "Remove Sample(s)", 
                                                              class = "btn-block btn-danger", 
                                                              width = "25%"),
                                                 shiny::hr(),
                                                 selectizeInput("removedatacol", "Select variable(s) to be removed from the dataset:", 
                                                                choices=NULL,
                                                                multiple=T),
                                                 actionButton("removedatacolBT", "Remove Variable(s)", class = "btn-block btn-danger", width = "25%")
                                        )
                                        
                                        
                                      )
                                  ),
                                ),
                                
                                fluidRow(box(title=h3("Data preview"), width = 12, status = "primary",
                                             shiny::hr(),
                                             textOutput("spectramsn"),
                                             DTOutput("preview1", width = "100%")
                                )
                                )
                        ),
                        
                        tabItem(tabName = "clustertab", 
                                h2("Cluster Analysis"),
                                shiny::hr(),
                                fluidRow(
                                  box(title = h3("Data settings"),
                                      width = 12,
                                      checkboxInput("typedataCLUSTER", strong("Use a previous data submitted to a dimensionality reduction as data?")),
                                      conditionalPanel("input.typedataCLUSTER == true",
                                                       fileInput("modelCLUSTERdata", "Input a saved model file:")),
                                      shiny::hr(),
                                      selectizeInput("distmethodCLUSTER", 
                                                     "Select the method for distance calculation (HCA):",
                                                     choices = c("Euclidian"="euclidean",
                                                                 "Maximum"="maximum",
                                                                 "Manhattan"="manhattan",
                                                                 "Canberra"="canberra",
                                                                 "Binary"="binary", 
                                                                 "Minkowski"="minkowski")
                                      ),
                                      selectizeInput("HCAmethod",
                                                     "Select the method for the agglomeration(HCA)", 
                                                     choices = c("Complete"="complete", 
                                                                 "ward.D"="ward.D",
                                                                 "ward.D2"="ward.D2",
                                                                 "Single"="single",
                                                                 "UPGMA"="average", 
                                                                 "WPGMA"="mcquitty", 
                                                                 "WPGMC"="median",
                                                                 "UPGMC"="centroid")
                                      ),
                                      numericInput("nClustersHCA", 
                                                   "Select Number of Clusters (K-Means and HCA):",
                                                   value = 3,
                                                   min = 1,
                                                   step = 1),
                                      actionButton("runclusterBT", "Run Cluster Analysis"),
                                      shiny::hr(),
                                      actionButton("useHCAasclass", "Use HCA Clusters as Classes"),
                                      actionButton("useKMasclass", "Use K-Means Clusters as Classes")
                                  )
                                ),
                                fluidRow(
                                  tabBox(width = 12,
                                         tabPanel("Optimal Number of Cluster Analysis", h3(""),
                                                  fluidRow(
                                                    column(plotOutput("ElbowMethod"), width = 4)%>%withSpinner(),
                                                    column(plotOutput("SilhouetteMethod"), width = 4)%>%withSpinner(),
                                                    column(plotOutput("GapMethod"), width = 4)%>%withSpinner()
                                                  )
                                         ),
                                         tabPanel("HCA", h3(""),
                                                  fluidRow(
                                                    column(width = 6, checkboxInput("circlelizeDendo", "Show dendogram as a circle?", value = F))
                                                  ),
                                                  plotOutput("HCAplot")%>%withSpinner()
                                         ),
                                         tabPanel("K-Means", h3(""),
                                                  fluidRow(
                                                    plotOutput("KMplot")%>%withSpinner()
                                                  )
                                         ),
                                         tabPanel("Heatmap", h3(""),
                                                  plotOutput("HCAheatmap")),
                                         
                                         tabPanel("Correlation Heatmap", h3(""),
                                                  plotOutput("HCAcorheatmap")),
                                         
                                         tabPanel("Scatter",h3(""),
                                                  fluidRow(
                                                    column(width = 4, numericInput("siglimHCA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                    column(width = 4, numericInput("npc1scoresplotHCA", "1st Variable to plot:", step = 1, value = 1, min = 1)),
                                                    column(width = 4, numericInput("npc2scoresplotHCA", "2nd Variable to plot:", step = 1, value = 2, min = 1))
                                                  ),
                                                  plotlyOutput("HCAscores", height = 650)
                                         ),
                                         tabPanel("3D Scatter", h3(""),
                                                  fluidRow(
                                                    column(width = 3, numericInput("siglim3DHCA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                    column(width = 3, numericInput("npc1scores3dplotHCA", "1st Variable to plot:", step = 1, value = 1, min = 1)),
                                                    column(width = 3, numericInput("npc2scores3dplotHCA", "2nd Variable to plot:", step = 1, value = 2, min = 1)),
                                                    column(width = 3, numericInput("npc3scores3dplotHCA", "3rd Variable to plot:", step = 1, value = 3, min = 1))
                                                  ),
                                                  plotlyOutput("HCA3dscores", height = 650)%>%withSpinner()
                                         ),
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "fastprepro", 
                                h2("Pre-Processing"), 
                                shiny::hr(),
                                fluidRow(
                                  box(h3("Select a pre-processing method"),
                                      shiny::hr(),
                                      width = 12,
                                      p("Selecting a method will apply it in every linear and non-linear models generated. For more complete pre-treatment capabilities please visit:"),
                                      p("https://leams-uerj-chemometrics.shinyapps.io/Data_Handling_app or download the data handling app at our GitHub at the 'about the interface panel'."),
                                      shiny::hr(),
                                      selectInput("prepropca","Fast Pre-processing:", c("None" = "none","Center" = "center","Autoscale" = "scale"), selected = "center")
                                  )),
                                fluidRow(
                                  box(width = 12,
                                      h3("View Options"),
                                      shiny::hr(),
                                      fluidRow(
                                      column(radioButtons("plotpretreatclasses" ,"Type of Coloring:",
                                                   inline = T,
                                                   choices = c("Show individually"="indpretreatplot", "Show by class"="classpretreatplot")
                                      ), width = 4),
                                      column(checkboxInput("selectpreprosamples", "Select some samples to plot? (Beware that classes will not be correctly colored)"), width = 4),
                                      column(conditionalPanel("input.selectpreprosamples == true",selectizeInput("whichsamplesprepro", "Which Samples?", choices = c(), multiple=T)), width = 4)
                                      ),
                                      #actionButton("pretreatcopyBT", "Update Image Settings"),
                                      
                                      fluidRow(
                                        column(width = 4, plotOutput("normaldata")),
                                        column(width = 4, plotOutput("meancenterdata")),
                                        column(width = 4, plotOutput("scaleddata"))
                                      ),
                                  )
                                )
                                
                        ),
                        
                        tabItem(tabName = "testtab",
                                h2("Data Adequacy Testing"),
                                shiny::hr(),
                                tabsetPanel(
                                  tabPanel("Dimensionality reduction adequacy",
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Bartlett Sphericity Test"),
                                                 p("The null hypothesis of the test is that the variables are orthogonal, i.e. not correlated. Useful to performed before use a data reduction technique to verify that a data reduction technique can actually compress the data in a meaningful way. If p-value is lower than the admited significance the null hypothesis is rejected."),                        shiny::hr(),
                                                 actionButton("barttestBT", "Run Bartelett test"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("Barteletttest", placeholder = T)  
                                             )
                                           ),
                                           
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Kaiser–Meyer–Olkin Adequacy Test"),
                                                 p("The Kaiser–Meyer–Olkin (KMO) test is a statistical measure to determine how suited data is for factor analysis.  The statistic is a measure of the proportion of variance among variables that might be common variance. The KMO represents the degree to which each observed variable is predicted by the other variables in the dataset and with this indicates the suitability for factor analysis. KMO values between 0.6 and 1 indicate the sampling is adequate. KMO values less than 0.5 indicate the sampling is not adequate and that remedial action should be taken. A KMO value close to zero means that there are large partial correlations compared to the sum of correlations. In other words, there are widespread correlations which would be a large problem for factor analysis."),                          shiny::hr(),
                                                 actionButton("KMOBT", "Run KMO test"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("KMOtest", placeholder = T)  
                                             )
                                           ),
                                           ),
                                  
                                  tabPanel("Homogeneity test",
                                           
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Chi-square test"),
                                                 p("Allows to evaluate whether or not samples are distributed equally across various levels/categories.The p-value indicates the level of statistical significance of the difference between the observed & expected distributions. Lower p-value = More Heterogeneous Higher p-value = More Homogeneous."),                          shiny::hr(),
                                                 actionButton("ChiBT", "Run Chi-square test"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("Chitest", placeholder = T)  
                                             )
                                           ),
                                           
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Fligner-Killeen’s Test"),
                                                 p("A non-parametric test which is very robust against departures from normality. 
                                                 Null Hypothesis: All populations variances are equal
                                                   Alternative Hypothesis: At least two of them differ"),
                                                 shiny::hr(),
                                                 actionButton("FlKBT", "Run Fligner-Killeen’s test"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("FlKtest", placeholder = T)  
                                             )
                                           ),
                                           
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Levene’s Test"),
                                                 p("A robust alternative to the Bartlett’s test that is less sensitive to departures from normality.
                                                   Null Hypothesis: All populations variances are equal Alternative Hypothesis: The variances are not equal for at least one pair."),
                                                 shiny::hr(),
                                                 actionButton("LevBT", "Run Levene’s Test"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("Levtest", placeholder = T)  
                                             )
                                             ),
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Bartlett homogeneity Test"),
                                                 p("Bartlett’s Test is a statistical test that is used to determine whether or not the variances between several groups are equal. (Don’t confuse this test with Bartlett’s Test of Sphericity!).
                                                 Bartlett's test is sensitive to departures from normality. That is, if the samples come from non-normal distributions, then Bartlett's test may simply be testing for non-normality. Levene's test is an alternative to the Bartlett test that are less sensitive to departures from normality.
                                                 H0: The variance among each group is equal.
                                                   HA: At least one group has a variance that is not equal to the rest."),
                                                 shiny::hr(),
                                                 actionButton("BartHomoBT", "Run Bartelett homogeneity Test"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("BartHomotest", placeholder = T)  
                                             )
                                           )
                                           ),
                                  tabPanel("Outlier Test",
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Chi-squared test for outlier"),
                                                 p("This function performs a simple test for one outlier, based on chisquared distribution of squared differences between data and sample mean. It assumes known variance of population."),
                                                 shiny::hr(),
                                                 actionButton("ChisOutBT", "Run Chi-squared test"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("ChisOuttest", placeholder = T)  
                                             )
                                           ),
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Dixon tests for outlier"),
                                                 p("This test can be used to detect an outlier that is either much smaller or much larger than the rest of the selected data values. Dixon's test is appropriate only when the data, excluding the suspected outlier, are approximately normally distributed, and when the sample size is less than or equal to 30."),
                                                 shiny::hr(),
                                                 actionButton("DixonBT", "Run Dixon tests"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("Dixontest", placeholder = T)  
                                             )
                                           ),
                                           fluidRow(
                                             box(width = 12,
                                                 h3("Grubbs tests for one or two outliers in data sample"),
                                                 p("Grubbs' test is used to detect a single outlier in a data set that follows an approximately normal distribution. The Grubbs test is used to check for a single outlier. If there are in fact multiple outliers, the results of the Grubbs test can be distorted.
                                                 If you suspect more than one outlier may be present, it is recommended that you use either the Tietjen-Moore test or the generalized extreme studentized deviate test instead of the Grubbs' test.
                                                 Grubbs' test is also known as the maximum normed residual test.
                                                 H0:	There are no outliers in the data set
                                                   H1:	There is exactly one outlier in the data set"),
                                                 shiny::hr(),
                                                 actionButton("GrubbsBT", "Run Grubbs tests"),
                                                 shiny::hr(),
                                                 verbatimTextOutput("Grubbstest", placeholder = T)  
                                             )
                                           )
                                           ),
                                  ),
                                ),
                        
                        
                        
                        tabItem(tabName = "pcatab", h2("Principal Components Analysis:"), shiny::hr(),
                                tabsetPanel( 
                                  
                                  tabPanel("PCA Model",
                                           fluidRow(
                                             box(title=h3("PCA Configurations"),width=6, status = "primary",
                                                 numericInput("ncomppca","Number of components", min = 1, max = 20, value = 10),
                                                 checkboxInput("removepcacolask", strong("Remove any columns from calculations? (All non-numeric columns are automatically removed)")),
                                                 conditionalPanel("input.removepcacolask == true", selectizeInput("pcaremovecol", "Wich columns?", choices = NULL, multiple=T)),
                                                 selectInput("methodpca", "Method to compute PCA:", c("svd"="svd","NIPALS"="nipals")),
                                                 #selectInput("limtypepca","Method applied to calculate outliers:", c( "Classical" = "ddmoments", "Robust" = "ddrobust", "chi-square distribution" = "chisq","Jackson-Mudholkar" = "jm"), selected = "ddmoments"),
                                                 actionButton("bPCA", "Run PCA",class = "btn-success",width = "25%")),
                                             
                                             box(title=h3("Model summary"),width = 6,verbatimTextOutput("SumPCA", placeholder = T), status = "primary")),
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcorespca", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                                        tabsetPanel(
                                                          tabPanel("Variance",plotlyOutput("pcav", height = 650)),
                                                          tabPanel("Cumulative Variance",plotlyOutput("pcacv", height = 650)),
                                                          tabPanel("Loadings",
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1loadplotPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2loadplotPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("pcaload", height = 650)),
                                                          tabPanel("Spectral Loadings",h3(""),numericInput("numnpcpca","Show n components", min = 1, max = 15, value = 2),plotlyOutput("pcaload1", height = 650)),
                                                          tabPanel("Scores", h3(""),
                                                                   fluidRow(
                                                                     column(numericInput("siglimpca", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01), width = 4),
                                                                     column(numericInput("npc1scoresplotpca", "1st PC to plot:", step = 1, value = 1, min = 1), width = 4),
                                                                     column(numericInput("npc2scoresplotpca", "2nd PC to plot:", step = 1, value = 2, min = 1), width = 4)
                                                                   ),
                                                                   plotlyOutput("pcascores", height = 650)
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   fluidRow(
                                                                     column(numericInput("siglim3Dpca", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01), width = 3),
                                                                     column(numericInput("npc1scores3dplotpca", "1st PC to plot:", step = 1, value = 1, min = 1), width = 3),
                                                                     column(numericInput("npc2scores3dplotpca", "2nd PC to plot:", step = 1, value = 2, min = 1), width = 3),
                                                                     column(numericInput("npc3scores3dplotpca", "3rd PC to plot:", step = 1, value = 3, min = 1), width = 3)
                                                                   ),
                                                                   plotlyOutput("pca3dscores", height = 650)%>%withSpinner()
                                                          ),
                                                          tabPanel("Residuals",h3(""),fluidRow(
                                                            column(6,numericInput("numPCresipca","Principal Component Number",min = 1,value = 1),checkboxInput("logresidualspca","Apply Log", F)), plotOutput("pcaresiduals"))
                                                          ),
                                                          
                                                          tabPanel("BiPlot", h3(""), 
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1biplotpca", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2biplotpca", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("biplotpca", height = 650)
                                                          ),
                                                          tabPanel("Outliers",
                                                                   numericInput("numPCoutipca","Principal Component Number",min = 1,value = 1),
                                                                   plotOutput("OutPCA", height = 650)),
                                                        )
                                           )
                                           )
                                  ),
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 numericInput("numcompselpca", "Select final number of components:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelpca", "Choose file name (will have 'PCA model' as ending)", placeholder = "My model name"),
                                                 downloadButton("savepcamodel", "Download model into .Rdata"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscorespca", "Download Scores from PCA into '.csv'", class = "btn-block"),
                                                 downloadButton("downloadloadpca","Download Loadings from PCA into '.csv'", class = "btn-block")),
                                             
                                             box(title = h3("Import previous PCA model"), width = 6,
                                                 fileInput("searchmodelpca", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importpcamodel", "Import"))
                                           ))
                                )
                        ),
                        
                        tabItem(tabName = "robpcatab", h2("Robust Principal Component Analysis"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Robust PCA model",
                                           fluidRow(
                                             box(title = h3("Robust PCA Configurations"), width = 6, status = "primary",
                                                 numericInput("ncompROBPCA","Number of components", min = 1, max = 20, value = 3),
                                                 checkboxInput("removeROBPCAcolask", "Remove any columns?"),
                                                 #selectInput("prepropca","Preprocessing method:", c("None" = "none","Center" = "center","Scale" = "scale"), selected = "scale"),
                                                 conditionalPanel("input.removeROBPCAcolask == true", selectInput("ROBPCAremovecol", "Wich columns?", choices = NULL, multiple=T)),
                                                 actionButton("bROBPCA", "Run Robust PCA",class = "btn-success",width = "25%")
                                             ),
                                             
                                             box(title=h4("Model summary"),width = 6,verbatimTextOutput("SumROBPCA", placeholder = T), status = "primary"),
                                           ),
                                           
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcoresROBPCA", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                                        tabsetPanel(
                                                          tabPanel("Variance",plotlyOutput("ROBPCAv", height = 650)),
                                                          tabPanel("Cumulative Variance",plotlyOutput("ROBPCAcv", height = 650)),
                                                          tabPanel("Loadings", h3(""),
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1loadplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2loadplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("ROBPCAload", height = 650)),
                                                          tabPanel("Spectral Loadings",h3(""),plotlyOutput("ROBPCAload1", height = 650)),
                                                          tabPanel("Scores",h3(""),
                                                                   fluidRow(
                                                                     column(width = 4,numericInput("siglimROBPCA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                                     column(width = 4,numericInput("npc1scoresplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 4,numericInput("npc2scoresplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("ROBPCAscores", height = 650)
                                                                   
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   fluidRow(
                                                                     column(width = 3,numericInput("siglim3DROBPCA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                                     column(width = 3,numericInput("npc1scores3dplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 3,numericInput("npc2scores3dplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1)),
                                                                     column(width = 3,numericInput("npc3scores3dplotROBPCA", "3rd Component to plot:", step = 1, value = 3, min = 1))
                                                                   ),
                                                                   plotlyOutput("ROBPCA3dscores", height = 650)%>%withSpinner()
                                                          ),
                                                          tabPanel("Biplot", h3(""),
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1biplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2biplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("ROBPCAbiplot", height = 650)
                                                          ),
                                                          
                                                          tabPanel("Outliers",
                                                                   numericInput("numROBPCoutipca","Principal Component Number",min = 1,value = 1),
                                                                   plotOutput("OutROBPCA", height = 650)),
                                                          
                                                        )))
                                           
                                           #input$npc1loadplotROBPCA
                                           
                                  ),
                                  
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 numericInput("numcompselROBPCA", "Select final number of components:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelROBPCA", "Choose file name (will have 'RobPCA model' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveROBPCAmodel", "Download model into .Rdata"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscoresROBPCA", "Download Scores from ROBPCA into '.csv'", class = "btn-block"),
                                                 downloadButton("downloadloadROBPCA","Download Loadings from ROBPCA into '.csv'", class = "btn-block")),
                                             box(title = h3("Import previous ROBPCA model"), width = 6,
                                                 fileInput("searchmodelROBPCA", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importROBPCAmodel", "Import")),
                                           ))
                                  
                                )),
                        
                        tabItem(tabName = "FAtab", h2("Factorial Analysis"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("FA model",
                                           fluidRow(
                                             box(title = h3("FA Configurations"), width = 6, status = "primary",
                                                 numericInput("ncompFA","Number of components", min = 1, max = 20, value = 3),
                                                 checkboxInput("removeFAcolask", "Remove any columns?"),
                                                 conditionalPanel("input.removeFAcolask == true", selectInput("FAremovecol", "Wich columns?", choices = NULL, multiple=T)),
                                                 actionButton("bFA", "Run FA",class = "btn-success",width = "25%")
                                             ),
                                             
                                             box(title=h4("Model summary"),width = 6,verbatimTextOutput("SumFA", placeholder = T), status = "primary"),
                                           ),
                                           
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcoresFA", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                                        tabsetPanel(
                                                          tabPanel("Variance",plotlyOutput("FAv", height = 650)),
                                                          tabPanel("Cumulative Variance",plotlyOutput("FAcv", height = 650)),
                                                          tabPanel("Loadings", h3(""),
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1loadplotFA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2loadplotFA", "1st Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("FAload", height = 650)),
                                                          tabPanel("Spectral Loadings",h3(""),plotlyOutput("FAload1", height = 650)),
                                                          tabPanel("Scores",h3(""),
                                                                   fluidRow(
                                                                     column(width = 4,numericInput("siglimFA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                                     column(width = 4,numericInput("npc1scoresplotFA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 4,numericInput("npc2scoresplotFA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("FAscores", height = 650)
                                                                   
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   fluidRow(
                                                                     column(width = 3,numericInput("siglim3DFA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                                     column(width = 3,numericInput("npc1scores3dplotFA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 3,numericInput("npc2scores3dplotFA", "2nd Component to plot:", step = 1, value = 2, min = 1)),
                                                                     column(width = 3,numericInput("npc3scores3dplotFA", "3rd Component to plot:", step = 1, value = 3, min = 1))
                                                                   ),
                                                                   plotlyOutput("FA3dscores", height = 650)%>%withSpinner()
                                                          ),
                                                          tabPanel("Biplot", h3(""),
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1biplotFA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2biplotFA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("FAbiplot", height = 650)
                                                          ),
                                                          
                                                        )))
                                           
                                           
                                           
                                  ),
                                  
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 numericInput("numcompselFA", "Select final number of factors:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelFA", "Choose file name (will have 'FA model' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveFAmodel", "Download model into .Rdata"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscoresFA", "Download Scores from FA into '.csv'", class = "btn-block"),
                                                 downloadButton("downloadloadFA","Download Loadings from FA into '.csv'", class = "btn-block")),
                                             box(title = h3("Import previous FA model"), width = 6,
                                                 fileInput("searchmodelFA", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importFAmodel", "Import")),
                                           ))
                                  
                                )),
                        
                        tabItem(tabName = "ICAtab", h2("Independent Component Analysis"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("ICA model",
                                           fluidRow(
                                             box(title = h3("ICA Configurations"), width = 6, status = "primary",
                                                 numericInput("ncompICA","Number of components", min = 1, max = 20, value = 3),
                                                 checkboxInput("removeICAcolask", "Remove any columns?"),
                                                 selectInput("icamethod", "Select ICA method:", choices = c("Fast ICA"="fastica", "Jade ICA"="jadeica")),
                                                 conditionalPanel("input.removeICAcolask == true", selectInput("ICAremovecol", "Wich columns?", choices = NULL, multiple=T)),
                                                 actionButton("bICA", "Run ICA",class = "btn-success",width = "25%")
                                             ),
                                             
                                             box(title=h4("Model summary"),width = 6,verbatimTextOutput("SumICA", placeholder = T), status = "primary"),
                                           ),
                                           
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcoresICA", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                                        tabsetPanel(
                                                          tabPanel("Variance",plotlyOutput("ICAv", height = 650)),
                                                          tabPanel("Cumulative Variance",plotlyOutput("ICAcv", height = 650)),
                                                          tabPanel("Loadings",h3(""),
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1loadplotICA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2loadplotICA", "1st Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("ICAload", height = 650)),
                                                          tabPanel("Spectral Loadings",h3(""),plotlyOutput("ICAload1", height = 650)),
                                                          tabPanel("Scores",h3(""),
                                                                   fluidRow(
                                                                     column(width = 4,numericInput("siglimICA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                                     column(width = 4,numericInput("npc1scoresplotICA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 4,numericInput("npc2scoresplotICA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("ICAscores", height = 650)
                                                                   
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   fluidRow(
                                                                     column(width = 3,numericInput("siglim3DICA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                                     column(width = 3,numericInput("npc1scores3dplotICA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 3,numericInput("npc2scores3dplotICA", "2nd Component to plot:", step = 1, value = 2, min = 1)),
                                                                     column(width = 3,numericInput("npc3scores3dplotICA", "3rd Component to plot:", step = 1, value = 3, min = 1))
                                                                   ),
                                                                   plotlyOutput("ICA3dscores", height = 650)%>%withSpinner()
                                                          ),
                                                          tabPanel("Biplot", h3(""),
                                                                   fluidRow(
                                                                     column(width = 6,numericInput("npc1biplotICA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                                     column(width = 6,numericInput("npc2biplotICA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                                   ),
                                                                   plotlyOutput("ICAbiplot", height = 650)
                                                          ),
                                                        )))
                                           
                                           
                                           
                                  ),
                                  
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 numericInput("numcompselICA", "Select final number of Components:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelICA", "Choose file name (will have 'ICA model' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveICAmodel", "Download model into .Rdata", class = "btn-block"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscoresICA", "Download Scores from ICA into '.csv'", class = "btn-block"),
                                                 downloadButton("downloadloadICA","Download Loadings from ICA into '.csv'", class = "btn-block")),
                                             box(title = h3("Import previous ICA model"), width = 6,
                                                 fileInput("searchmodelICA", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importICAmodel", "Import")),
                                           ))
                                  
                                )),
                        
                        tabItem(tabName = "tsnetab", h2("t-distributed Stochastic Neighbour Embedding"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("t-SNE projection",
                                           fluidRow(
                                             box(title = h3("t-SNE Configurations"), width = 6, status = "primary",
                                                 numericInput("ncompTSNE","Number of dimensions:", min = 1, max = 3, value = 3),
                                                 numericInput("perplexityTSNE", "Perplexity (perplexity must be less than '1/3(number of samples - 1)'):", min = 5, value = 20, step = 1),
                                                 numericInput("niterTSNE", "Number of iterations:", min = 100, value = 1000, step = 200),
                                                 numericInput("thetaTSNE", "Theta Value (speed/accuracy tradeoff):", min = 0.0, max = 1, value = 0.5, step = 0.01),
                                                 checkboxInput("removeTSNEcolask", "Remove any columns?"),
                                                 conditionalPanel("input.removeTSNEcolask == true", selectInput("TSNEremovecol", "Wich columns?", choices = NULL, multiple=T)),
                                                 actionButton("bTSNE", "Run t-SNE",class = "btn-success",width = "25%")
                                             ),
                                             
                                             box(title=h3("New Values"),width = 6, DTOutput("SumTSNE"), status = "primary"),
                                           ),
                                           
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcoresTSNE", strong("Show classes on projection plot:")),
                                                        tabsetPanel(
                                                          tabPanel("Scores", h3(""),
                                                                   numericInput("siglimTSNE", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scoresplotTSNE", "1st Component to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scoresplotTSNE", "2nd Component to plot:", step = 1, value = 2, min = 1),
                                                                   plotlyOutput("TSNEscores", height = 650)%>%withSpinner()
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   numericInput("siglim3DTSNE", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scores3dplotTSNE", "1st Component to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scores3dplotTSNE", "2nd Component to plot:", step = 1, value = 2, min = 1),
                                                                   numericInput("npc3scores3dplotTSNE", "3rd Component to plot:", step = 1, value = 3, min = 1),
                                                                   plotlyOutput("TSNE3dscores", height = 650)%>%withSpinner()
                                                                   
                                                          )
                                                          
                                                        )
                                           )
                                           )
                                  ),
                                  
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 #numericInput("numcompselTSNE", "Select final number of Components:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelTSNE", "Choose file name (will have 'TSNE model' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveTSNEmodel", "Download model into .Rdata", class = "btn-block"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscoresTSNE", "Download projected data from TSNE into '.csv'", class = "btn-block"),
                                                 #downloadButton("downloadloadTSNE","Download Loadings from TSNE into '.csv'", class = "btn-block")),
                                             ),
                                             box(title = h3("Import previous TSNE model"), width = 6,
                                                 fileInput("searchmodelTSNE", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importTSNEmodel", "Import")),
                                           ))
                                  
                                )),
                        tabItem(tabName = "MDStab", h2("Classical Multidimensional Scaling"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("MDS Model",
                                           fluidRow(
                                             box(title = h3("MDS Configurations"), width = 6, status = "primary",
                                                 numericInput("ncompMDS","Number of dimensions:", min = 1, max = 20, value = 3),
                                                 selectInput("MDSdistmethod", "Select method for distance calculation:", choices = c("Euclidian"="euclidian", "Manhattan"="manhattan", "Minkowski"="minkowski")),
                                                 conditionalPanel("input.MDSdistmethod == 'minkowski'", numericInput("minkpMDS", "Minkowski distance power:", value = 1, step = 1)),
                                                 actionButton("bMDS", "Run MDS",class = "btn-success",width = "25%")
                                             ),
                                             
                                             box(title=h4("Model summary"),width = 6,verbatimTextOutput("SumMDS", placeholder = T), status = "primary"),
                                           ),
                                           
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcoresMDS", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                                        tabsetPanel(
                                                          
                                                          tabPanel("Scores",h3(""),
                                                                   numericInput("siglimMDS", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scoresplotMDS", "1st Dimension to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scoresplotMDS", "2nd Dimension to plot:", step = 1, value = 2, min = 1),
                                                                   plotlyOutput("MDSscores", height = 650)
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   numericInput("siglim3DMDS", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scores3dplotMDS", "1st Dimension to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scores3dplotMDS", "2nd Dimension to plot:", step = 1, value = 2, min = 1),
                                                                   numericInput("npc3scores3dplotMDS", "3rd Dimension to plot:", step = 1, value = 3, min = 1),
                                                                   plotlyOutput("MDS3dscores", height = 650)%>%withSpinner()
                                                          )
                                                          
                                                        )))
                                           
                                           
                                           
                                  ),
                                  
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 numericInput("numcompselMDS", "Select final number of dimensions:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelMDS", "Choose file name (will have 'MDS model' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveMDSmodel", "Download model into .Rdata"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscoresMDS", "Download Scores from MDS into '.csv'", class = "btn-block")
                                                 #downloadButton("downloadloadMDS","Download Loadings from MDS into '.csv'", class = "btn-block")
                                             ),
                                             box(title = h3("Import previous MDS model"), width = 6,
                                                 fileInput("searchmodelMDS", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importMDSmodel", "Import")),
                                           ))
                                  
                                )),
                        
                        tabItem(tabName = "isomaptab", h2("Isometric Feature Mapping Ordination"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Isomap Model",
                                           fluidRow(
                                             box(title = h3("ISOMAP Configurations"), width = 6, status = "primary",
                                                 numericInput("ncompISOMAP","Number of dimensions:", min = 1, max = 20, value = 3),
                                                 numericInput("epsilonISOMAP", "Shortest dissimilarity retained:", value = 20, min = 1, step = 1),
                                                 selectInput("ISOMAPdistmethod", "Select method for distance calculation:", choices = c("Euclidian"="euclidian", "Manhattan"="manhattan", "Minkowski"="minkowski")),
                                                 conditionalPanel("input.ISOMAPdistmethod == 'minkowski'", numericInput("minkpISOMAP", "Minkowski distance power:", value = 1, step = 1)),
                                                 actionButton("bISOMAP", "Run ISOMAP",class = "btn-success",width = "25%")
                                             ),
                                             
                                             box(title=h4("Model summary"),width = 6,verbatimTextOutput("SumISOMAP", placeholder = T), status = "primary"),
                                           ),
                                           
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcoresISOMAP", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                                        tabsetPanel(
                                                          
                                                          tabPanel("Scores",h3(""),
                                                                   numericInput("siglimISOMAP", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scoresplotISOMAP", "1st Dimension to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scoresplotISOMAP", "2nd Dimension to plot:", step = 1, value = 2, min = 1),
                                                                   plotlyOutput("ISOMAPscores", height = 650)
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   numericInput("siglim3DISOMAP", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scores3dplotISOMAP", "1st Dimension to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scores3dplotISOMAP", "2nd Dimension to plot:", step = 1, value = 2, min = 1),
                                                                   numericInput("npc3scores3dplotISOMAP", "3rd Dimension to plot:", step = 1, value = 3, min = 1),
                                                                   plotlyOutput("ISOMAP3dscores", height = 650)%>%withSpinner()
                                                          ),
                                                          tabPanel("Iso Mapping",h3(""),
                                                                   plotOutput("isomapISOMAP"))
                                                          
                                                        )))
                                           
                                           
                                           
                                  ),
                                  
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 numericInput("numcompselISOMAP", "Select final number of dimensions:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelISOMAP", "Choose file name (will have 'ISOMAP model' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveISOMAPmodel", "Download model into .Rdata"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscoresISOMAP", "Download Scores from ISOMAP into '.csv'", class = "btn-block")
                                                 #downloadButton("downloadloadISOMAP","Download Loadings from ISOMAP into '.csv'", class = "btn-block")
                                             ),
                                             box(title = h3("Import previous ISOMAP model"), width = 6,
                                                 fileInput("searchmodelISOMAP", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importISOMAPmodel", "Import")),
                                           ))
                                  
                                )),
                        
                        tabItem(tabName = "kernelpcatab", h2("Kernel Principal Components Analysis"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Kernel PCA Model",
                                           fluidRow(
                                             box(title = h3("Kernel PCA Configurations"), width = 6, status = "primary",
                                                 numericInput("ncompkernelpca","Number of features (principal components):", min = 1, max = 20, value = 3),
                                                 selectInput("kernelpcamethod", "Select kernel function for calculation:", choices = c("Radial Basis kernel function 'Gaussian'"="rbfdot", 
                                                                                                                                       "Polynomial kernel function"="polydot",
                                                                                                                                       "Linear kernel function"="vanilladot",
                                                                                                                                       "Hyperbolic tangent kernel function"="tanhdot",
                                                                                                                                       "Laplacian kernel function"="laplacedot",
                                                                                                                                       "Bessel kernel function"="besseldot",
                                                                                                                                       "ANOVA RBF kernel function"="anovadot",
                                                                                                                                       "Spline kernel"="splinedot")),
                                                 actionButton("bkernelpca", "Run kernelpca",class = "btn-success",width = "25%")
                                             ),
                                             
                                             box(title=h4("Model summary"),width = 6,verbatimTextOutput("Sumkernelpca", placeholder = T), status = "primary"),
                                           ),
                                           
                                           
                                           fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                                        checkboxInput("classcoreskernelpca", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                                        tabsetPanel(
                                                          
                                                          tabPanel("Scores",h3(""),
                                                                   numericInput("siglimkernelpca", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scoresplotkernelpca", "1st Dimension to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scoresplotkernelpca", "2nd Dimension to plot:", step = 1, value = 2, min = 1),
                                                                   plotlyOutput("kernelpcascores", height = 650)
                                                          ),
                                                          tabPanel("3D Scores", h3(""),
                                                                   numericInput("siglim3Dkernelpca", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                                   numericInput("npc1scores3dplotkernelpca", "1st Dimension to plot:", step = 1, value = 1, min = 1),
                                                                   numericInput("npc2scores3dplotkernelpca", "2nd Dimension to plot:", step = 1, value = 2, min = 1),
                                                                   numericInput("npc3scores3dplotkernelpca", "3rd Dimension to plot:", step = 1, value = 3, min = 1),
                                                                   plotlyOutput("kernelpca3dscores", height = 650)%>%withSpinner()
                                                          ),
                                                          # tabPanel("Loadings", h3(""),
                                                          #          fluidRow(
                                                          #            column(width = 6,numericInput("npc1loadplotkernelpca", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                          #            column(width = 6,numericInput("npc2loadplotkernelpca", "1st Component to plot:", step = 1, value = 2, min = 1))
                                                          #          ),
                                                          #          plotlyOutput("kernelpcaload", height = 650)),
                                                          tabPanel("Eigenvectors",h3(""),plotlyOutput("kernelpcaEigenvectors", height = 650)),
                                                          
                                                        )))
                                           
                                           
                                           
                                  ),
                                  
                                  tabPanel("Save and load model",
                                           fluidRow(
                                             box(title = h3("Save Model"), width = 6,
                                                 numericInput("numcompselkernelpca", "Select final number of dimensions:", value = 3, step = 1, min = 1),
                                                 textInput("namemodelkernelpca", "Choose file name (will have 'kernelpca model' as ending)", placeholder = "My model name"),
                                                 downloadButton("savekernelpcamodel", "Download model into .Rdata"),
                                                 shiny::hr(),
                                                 downloadButton("downloadscoreskernelpca", "Download Scores from kernelpca into '.csv'", class = "btn-block")
                                             ),
                                             box(title = h3("Import previous kernelpca model"), width = 6,
                                                 fileInput("searchmodelkernelpca", "Select Model:", buttonLabel = "Search file..."),
                                                 actionButton("importkernelpcamodel", "Import")),
                                           ))
                                  
                                )),
                        
                        tabItem(tabName = "lowvariancetab", h2("Low-Variance Filter"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Apply Low-Var Filter",
                                           fluidRow(
                                             box(title = h3("Low-Var Configurations"), width = 6, status = "primary",
                                                 selectInput("lowvarmethod", "Type of threshhold to use:", choices = c("Frequency Ratio"="freqcut", "Percent of unique values"="uniquecut"), selected = "uniquecut"),
                                                 
                                                 conditionalPanel(condition = "input.lowvarmethod == 'uniquecut' ", numericInput("uniquecutval", "Percent of Unique Values:", value = 0.2, step = 1)),
                                                 conditionalPanel("input.lowvarmethod == 'freqcut' ", numericInput("freqcutval", "Frequency Ratio:", value = 30, step = 1)),
                                                 shiny::hr(),
                                                 actionButton("blowvar", "Run Low-Var",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 actionButton("confirmlowvar", "Confirm Changes",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 textInput("namelowvar", "Choose file name (will have 'filtered data' as ending)", placeholder = "My model name"),
                                                 downloadButton("savelowvar", "Download into .Rdata"),
                                                 downloadButton("savelowvarcsv", "Download into .csv")
                                             ),
                                             
                                             box(title=h3("Variables Status"),width = 6,verbatimTextOutput("Sumlowvar", placeholder = T), status = "primary"),
                                           )
                                  )
                                  
                                )),
                        
                        tabItem(tabName = "highcorrtab", h2("High Covariance Filter"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Apply High Covariance Filter",
                                           fluidRow(
                                             box(title = h3("High-Corr Configurations"), width = 6, status = "primary",
                                                 numericInput("highcorrthreshold" ,"Select correlation threshold:", value = 0.8, step = 0.1, max = 1, min = 0.01),
                                                 shiny::hr(),
                                                 actionButton("bhighcorr", "Run High-Corr",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 actionButton("confirmhighcorr", "Confirm Changes",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 textInput("namehighcorr", "Choose file name (will have 'filtered data' as ending)", placeholder = "My model name"),
                                                 downloadButton("savehighcorr", "Download into .Rdata"),
                                                 downloadButton("savehighcorrcsv", "Download into .csv")
                                             ),
                                             
                                             box(title=h3("Variables Status"),width = 6,verbatimTextOutput("Sumhighcorr", placeholder = T), status = "primary"),
                                           ),
                                           
                                           fluidRow(
                                             box(title = h3("Correlation Heatmap"), width = 12, status = "danger",
                                                 "Will only show for non-continuous data",
                                                 shiny::hr(),
                                                 plotOutput("corrheatmapplot"))
                                           )
                                  )
                                  
                                )),
                        
                        
                        tabItem(tabName = "wavelettab", h2("Wavelet Transform"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Apply Wavelet",
                                           fluidRow(
                                             box(title = h3("Wavelet Configurations"), width = 6, status = "primary",
                                                 numericInput("nlevelsWAVELET" ,"Maximum number of compressions:", value = 4, step = 1, max = 18, min = 1),
                                                 selectInput("WAVELETfiltertype", "Select a wavelet class:", choices = c("Daubechies"="d", "Least Asymetric"="la", "Best Localized"="bl", "Coiflet"="c"), selected = "la"),
                                                 numericInput("filterlenght"," Select wavelet filter length (use of the interface selective arrows are encouraged):", value = 2, max = 20, min = 2, step = 2),
                                                 shiny::hr(),
                                                 actionButton("bWAVELET", "Run Wavelet",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 numericInput("nwselectWAVELET", "Select the number of  the compression to save and view:", value = 1, min = 1, max = 18, step = 1),
                                                 radioButtons("highlowWAVELET", "Type of coefficients:", choices = c("High pass"="hp", "Low pass"="lp")),
                                                 shiny::hr(),
                                                 actionButton("confirmWAVELET", "Confirm Changes",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 textInput("nameWAVELET", "Choose file name (will have 'filtered data' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveWAVELET", "Download into .Rdata"),
                                                 downloadButton("saveWAVELETcsv", "Download into .csv")
                                             ),
                                             
                                             box(title=h3("Variables Status"),width = 6,verbatimTextOutput("SumWAVELET", placeholder = T), status = "primary"),
                                           ),
                                           
                                           fluidRow(
                                             box(title = h3("Compressed data"), width = 12, status = "danger",
                                                 shiny::hr(),
                                                 checkboxInput("classcoreswavelet", strong("Show classes on plot (attention: colors of each class may differ in each graph):")),
                                                 shiny::hr(),
                                                 plotlyOutput("compressedwaveletplot"))
                                           ),
                                           
                                           fluidRow(
                                             box(title = h3("Original data"), width = 12, status = "danger",
                                                 shiny::hr(),
                                                 plotlyOutput("originalwaveletplot"))
                                           )
                                  )
                                  
                                )),
                        
                        tabItem(tabName = "resolutionredctab", h2("Spectral Resolution Reduction"), shiny::hr(),
                                tabsetPanel(
                                  
                                  tabPanel("Apply Reduction",
                                           fluidRow(
                                             box(title = h3("Reduction Configurations"), width = 12, status = "primary",
                                                 numericInput("ntimesREDC" ,"Before/After number of variables ratio:", value = 2, step = 1, max = 15, min = 1),
                                                 actionButton("bREDC", "Run Reduction",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 actionButton("confirmREDC", "Confirm Changes",class = "btn-success",width = "25%"),
                                                 shiny::hr(),
                                                 textInput("nameREDC", "Choose file name (will have 'reducted' as ending)", placeholder = "My model name"),
                                                 downloadButton("saveREDC", "Download into .Rdata"),
                                                 downloadButton("saveREDCcsv", "Download into .csv")
                                             ),
                                             
                                             #box(title=h3("Variables Status"),width = 6,verbatimTextOutput("SumREDC", placeholder = T), status = "primary"),
                                           ),
                                           
                                           fluidRow(
                                             box(title = h3("Compressed data"), width = 12, status = "danger",
                                                 shiny::hr(),
                                                 checkboxInput("classcoresREDC", strong("Show classes on plot (attention: colors of each class may differ in each graph):")),
                                                 shiny::hr(),
                                                 plotlyOutput("compressedREDCplot"))
                                           ),
                                           
                                           fluidRow(
                                             box(title = h3("Original data"), width = 12, status = "danger",
                                                 shiny::hr(),
                                                 plotlyOutput("originalREDCplot"))
                                           )
                                  )
                                  
                                )),
                        
                        tabItem("creditstab", h2("Credits"),
                                fluidRow(box(width = 12, h3("Credits"), shiny::hr(),
                                             p(h4(strong("Bernardo Cardeal Goulart Darzé Santos"))),
                                             p(a(href="http://lattes.cnpq.br/0590620499595344", "http://lattes.cnpq.br/0590620499595344",target="_blank")),
                                             p("bernardocardeal@outlook.com"),
                                             
                                             shiny::hr(),
                                             
                                             p(h4(strong("José Licarion Pinto Segundo Neto, Dsc."))),
                                             p(a(href="http://lattes.cnpq.br/5267552018296169", "http://lattes.cnpq.br/5267552018296169",target="_blank")),
                                             p("licarion@gmail.com"),
                                             
                                             shiny::hr(),
                                             
                                             p(h4(strong("Aderval Severino Luna, PhD"))),
                                             p(a(href="http://lattes.cnpq.br/0294676847895948", "http://lattes.cnpq.br/0294676847895948",target="_blank")),
                                             p("adsluna@gmail.com")
                                             
                                )
                                ),
                                fluidRow(box(width = 12, h3("Acknowledgements"),
                                             fluidRow(column(width=12 ,
                                                             "The authors are thankful to Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq), Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) Finance Code 001, Fundação de Amparo à Pesquisa no Rio de Janeiro (FAPERJ) (grant number E-26/201.928/2020), and Universidade do Estado do Rio de Janeiro for their financial suppport. ASL has research scholarship from UERJ (Programa Pró-Ciência), FAPERJ (grant number E-26/202.552/2019), and CNPq (grant number 302465/2018-9)."                                        )) 
                                ))
                        )
                        
                        
                        
                        
                        
                      )
                    )
                    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #------Global Variables
  
  data<-reactiveVal()
  variables<-reactiveVal()
  numvar<-reactiveVal(1)
  id<-reactiveVal()
  sampleclasscolour<-reactiveVal()
  sampleclass<-reactiveVal()
  
  HCAclass<-reactiveVal()
  KMclass<-reactiveVal()
  
  PCA<-reactiveVal()
  FA<-reactiveVal()
  ICA<-reactiveVal()
  ROBPCA<-reactiveVal()
  TSNE<-reactiveVal()
  MDS<-reactiveVal()
  ISOMAP<-reactiveVal()
  KERNELPCA<-reactiveVal()
  LOWVAR<-reactiveVal()
  HIGHCORR<-reactiveVal()
  WAVELET<-reactiveVal()
  REDC<-reactiveVal()
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  #----Iris Dataset
  observeEvent(c(input$demodata, input$datatype), {
    req(input$datatype=='demos')
    
    if (input$demodata=='irisDEMO'){
      updateCheckboxInput(inputId = "isspectra", value = F)
      updateCheckboxInput(inputId = "classcol", value = T)
    }
  })
  
  observeEvent(input$preview, {
    
    req(input$datatype=='demos')
    req(input$demodata=='irisDEMO')
    
    data(data.frame(matrix(c(5.1,4.9,4.7,4.6,5.0,5.4,4.6,5.0,4.4,4.9,5.4,4.8,4.8,4.3,5.8,5.7,5.4,5.1,5.7,5.1,5.4,5.1,4.6,5.1,4.8,5.0,5.0,5.2,5.2,4.7,4.8,5.4,5.2,5.5,4.9,5.0,5.5,4.9,4.4,5.1,5.0,4.5,4.4,5.0,5.1,4.8,5.1,4.6,5.3,5.0,7.0,6.4,6.9,5.5,6.5,5.7,6.3,4.9,6.6,5.2,5.0,5.9,6.0,6.1,5.6,6.7,5.6,5.8,6.2,5.6,5.9,6.1,6.3,6.1,6.4,6.6,6.8,6.7,6.0,5.7,5.5,5.5,5.8,6.0,5.4,6.0,6.7,6.3,5.6,5.5,5.5,6.1,5.8,5.0,5.6,5.7,5.7,6.2,5.1,5.7,6.3,5.8,7.1,6.3,6.5,7.6,4.9,7.3,6.7,7.2,6.5,6.4,6.8,5.7,5.8,6.4,6.5,7.7,7.7,6.0,6.9,5.6,7.7,6.3,6.7,7.2,6.2,6.1,6.4,7.2,7.4,7.9,6.4,6.3,6.1,7.7,6.3,6.4,6.0,6.9,6.7,6.9,5.8,6.8,6.7,6.7,6.3,6.5,6.2,5.9,3.5,3.0,3.2,3.1,3.6,3.9,3.4,3.4,2.9,3.1,3.7,3.4,3.0,3.0,4.0,4.4,3.9,3.5,3.8,3.8,3.4,3.7,3.6,3.3,3.4,3.0,3.4,3.5,3.4,3.2,3.1,3.4,4.1,4.2,3.1,3.2,3.5,3.1,3.0,3.4,3.5,2.3,3.2,3.5,3.8,3.0,3.8,3.2,3.7,3.3,3.2,3.2,3.1,2.3,2.8,2.8,3.3,2.4,2.9,2.7,2.0,3.0,2.2,2.9,2.9,3.1,3.0,2.7,2.2,2.5,3.2,2.8,2.5,2.8,2.9,3.0,2.8,3.0,2.9,2.6,2.4,2.4,2.7,2.7,3.0,3.4,3.1,2.3,3.0,2.5,2.6,3.0,2.6,2.3,2.7,3.0,2.9,2.9,2.5,2.8,3.3,2.7,3.0,2.9,3.0,3.0,2.5,2.9,2.5,3.6,3.2,2.7,3.0,2.5,2.8,3.2,3.0,3.8,2.6,2.2,3.2,2.8,2.8,2.7,3.3,3.2,2.8,3.0,2.8,3.0,2.8,3.8,2.8,2.8,2.6,3.0,3.4,3.1,3.0,3.1,3.1,3.1,2.7,3.2,3.3,3.0,2.5,3.0,3.4,3.0,1.4,1.4,1.3,1.5,1.4,1.7,1.4,1.5,1.4,1.5,1.5,1.6,1.4,1.1,1.2,1.5,1.3,1.4,1.7,1.5,1.7,1.5,1.0,1.7,1.9,1.6,1.6,1.5,1.4,1.6,1.6,1.5,1.5,1.4,1.5,1.2,1.3,1.5,1.3,1.5,1.3,1.3,1.3,1.6,1.9,1.4,1.6,1.4,1.5,1.4,4.7,4.5,4.9,4.0,4.6,4.5,4.7,3.3,4.6,3.9,3.5,4.2,4.0,4.7,3.6,4.4,4.5,4.1,4.5,3.9,4.8,4.0,4.9,4.7,4.3,4.4,4.8,5.0,4.5,3.5,3.8,3.7,3.9,5.1,4.5,4.5,4.7,4.4,4.1,4.0,4.4,4.6,4.0,3.3,4.2,4.2,4.2,4.3,3.0,4.1,6.0,5.1,5.9,5.6,5.8,6.6,4.5,6.3,5.8,6.1,5.1,5.3,5.5,5.0,5.1,5.3,5.5,6.7,6.9,5.0,5.7,4.9,6.7,4.9,5.7,6.0,4.8,4.9,5.6,5.8,6.1,6.4,5.6,5.1,5.6,6.1,5.6,5.5,4.8,5.4,5.6,5.1,5.1,5.9,5.7,5.2,5.0,5.2,5.4,5.1,0.2,0.2,0.2,0.2,0.2,0.4,0.3,0.2,0.2,0.1,0.2,0.2,0.1,0.1,0.2,0.4,0.4,0.3,0.3,0.3,0.2,0.4,0.2,0.5,0.2,0.2,0.4,0.2,0.2,0.2,0.2,0.4,0.1,0.2,0.1,0.2,0.2,0.1,0.2,0.2,0.3,0.3,0.2,0.6,0.4,0.3,0.2,0.2,0.2,0.2,1.4,1.5,1.5,1.3,1.5,1.3,1.6,1.0,1.3,1.4,1.0,1.5,1.0,1.4,1.3,1.4,1.5,1.0,1.5,1.1,1.8,1.3,1.5,1.2,1.3,1.4,1.4,1.7,1.5,1.0,1.1,1.0,1.2,1.6,1.5,1.6,1.5,1.3,1.3,1.3,1.2,1.4,1.2,1.0,1.3,1.2,1.3,1.3,1.1,1.3,2.5,1.9,2.1,1.8,2.2,2.1,1.7,1.8,1.8,2.5,2.0,1.9,2.1,2.0,2.4,2.3,1.8,2.2,2.3,1.5,2.3,2.0,2.0,1.8,2.1,1.8,1.8,1.8,2.1,1.6,1.9,2.0,2.2,1.5,1.4,2.3,2.4,1.8,1.8,2.1,2.4,2.3,1.9,2.3,2.5,2.3,1.9,2.0,2.3,1.8
    ),
    nrow = 150, ncol = 4, byrow = F)
    ))
    matrix<-data()
    
    sampleclassX<-c("setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","setosa","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","versicolor","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica",'virginica',"virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica","virginica"
    )
    sampleclass((sampleclassX))
    
    
    sampleclasscolourX<-c("100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","98","98","98","98","98","98","98","98","98",'98',"98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98","98"
                          
    )
    sampleclasscolour((sampleclasscolourX))
    
    
    variablesX<-c("sepal_length",	"sepal_width",	"petal_length",	"petal_width"
    )
    variables(t(variablesX))
    colnames(matrix)<-t(variables())
    
    
    idX<-c('Sample 1','Sample 2','Sample 3','Sample 4','Sample 5','Sample 6','Sample 7','Sample 8','Sample 9','Sample 10','Sample 11','Sample 12','Sample 13','Sample 14','Sample 15','Sample 16','Sample 17','Sample 18','Sample 19','Sample 20','Sample 21','Sample 22','Sample 23','Sample 24','Sample 25','Sample 26','Sample 27','Sample 28','Sample 29','Sample 30','Sample 31','Sample 32','Sample 33','Sample 34','Sample 35','Sample 36','Sample 37','Sample 38','Sample 39','Sample 40','Sample 41','Sample 42','Sample 43','Sample 44','Sample 45','Sample 46','Sample 47','Sample 48','Sample 49','Sample 50','Sample 51','Sample 52','Sample 53','Sample 54','Sample 55','Sample 56','Sample 57','Sample 58','Sample 59','Sample 60','Sample 61','Sample 62','Sample 63','Sample 64','Sample 65','Sample 66','Sample 67','Sample 68','Sample 69','Sample 70','Sample 71','Sample 72','Sample 73','Sample 74','Sample 75','Sample 76','Sample 77','Sample 78','Sample 79','Sample 80','Sample 81','Sample 82','Sample 83','Sample 84','Sample 85','Sample 86','Sample 87','Sample 88','Sample 89','Sample 90','Sample 91','Sample 92','Sample 93','Sample 94','Sample 95','Sample 96','Sample 97','Sample 98','Sample 99','Sample 100','Sample 101','Sample 102','Sample 103','Sample 104','Sample 105','Sample 106','Sample 107','Sample 108','Sample 109','Sample 110','Sample 111','Sample 112','Sample 113','Sample 114','Sample 115','Sample 116','Sample 117','Sample 118','Sample 119','Sample 120','Sample 121','Sample 122','Sample 123','Sample 124','Sample 125','Sample 126','Sample 127','Sample 128','Sample 129','Sample 130','Sample 131','Sample 132','Sample 133','Sample 134','Sample 135','Sample 136','Sample 137','Sample 138','Sample 139','Sample 140','Sample 141','Sample 142','Sample 143','Sample 144','Sample 145','Sample 146','Sample 147','Sample 148','Sample 149','Sample 150'
    )
    id(t(idX))
    rownames(matrix)<-t(id())
    
    data(matrix)
    
    
    
  })
  
  #----Ceramic
  observeEvent(c(input$demodata, input$datatype), {
    
    req(input$datatype=='demos')
    
    if (input$demodata=='ceramicDEMO'){
      updateCheckboxInput(inputId = "isspectra", value = F)
      updateCheckboxInput(inputId = "classcol", value = T)
    }
  })
  
  observeEvent(input$preview, {
    
    req(input$datatype=='demos')
    req(input$demodata=='ceramicDEMO')
    
    data(data.frame(matrix(c(0.62,0.57,0.49,0.89,0.03,0.62,0.45,0.59,0.42,0.56,0.35,0.43,0.76,0.03,0.71,0.25,0.43,0.28,0.25,0.16,0.03,0.24,0.29,0.45,0.29,0.41,0.34,0.71,0.3,0.5,0.31,0.2,0.26,0.86,0.17,0.32,0.39,0.18,0.42,0.29,0.55,0.64,0.14,0.31,0.97,1.46,1.05,0.14,0.37,1.09,1.16,1.01,1.88,0.73,0.68,1.29,1.27,0.28,0.34,0.5,0.51,0.14,0.38,0.25,0.2,0.19,0.69,0.65,0.03,0.61,0.03,0.31,0.25,0.11,0.24,0.15,1,0.66,0.32,0.71,0.4,0.03,0.37,0.34,0.72,0.23,0.14,0.14,0.38,0.47,0.19,0.3,0.36,0.18,0.33,0.45,0.53,0.49,0.23,0.7,0.44,0.26,0.31,0.24,0.47,0.22,0.49,0.34,0.36,0.55,0.33,0.46,0.23,0.25,0.47,0.25,0.37,0.32,0.41,0.49,0.32,0.19,0.53,0.22,0.35,0.18,0.18,0.21,0.27,0.19,0.27,0.28,0.07,0.47,0.23,0.41,1.03,0.5,0.58,0.1,0.58,0.25,0.27,0.32,0.54,0.52,0.97,0.66,0.4,0.5,1.04,0.47,0.53,0.57,0.35,0.78,1.01,0.35,0.54,0.53,0.5,0.32,0.39,0.33,0.52,0.53,0.58,0.57,0.47,1.32,0.47,0.55,0.34,0.24,0.46,0.63,19.61,21.19,18.6,18.01,18.41,18.82,17.65,21.42,23.12,19.86,19.53,19.35,19.45,18.34,24.47,23.07,20.67,20.89,20.79,23.77,25.13,22.81,23.49,25.8,22.26,19.48,22.48,19.49,25,25.15,22.77,24.07,26.48,21.54,21.4,22.34,23.2,23.25,22.09,24.35,21.58,21.31,24.01,23.23,11.42,12.96,13.64,12.42,13.15,13.47,13.83,11.84,12.95,13,12.74,12.83,13.01,14.76,13.76,11.3,13.54,14.15,12.37,14.09,12.83,13.61,13.86,13.81,13.05,12.73,13.97,14.63,12.93,11.33,12.64,13.14,13.7,12.95,12.89,11.61,14.38,13.55,13.56,12.37,12.2,12.99,12.62,14.25,71.99,70.09,74.7,74.19,73.99,73.79,74.99,71.46,67.41,72,72.87,71.21,72.52,73.26,65.2,67.37,70.07,70.77,69.92,66.31,64.58,66.31,67.94,64.42,68.93,71.97,67.8,70.85,65.09,65.37,67.75,66.18,63.88,68.95,71.02,68.86,66.4,67.86,69.03,65.43,69.91,69.34,66.7,67.08,74.41,68.79,69.9,67.24,68.98,68.51,71.37,71.13,67.58,71.01,71.93,68.81,73.11,68.65,65.53,69.9,71.35,68.91,67.7,68.46,72.24,70.06,71.38,70.37,72.28,71.6,67.3,68.22,71.59,75.95,74.08,73.76,70.52,72.16,70.6,71.04,66.59,67.66,72.77,70.7,72.19,71.81,69.16,71.55,4.84,4.98,3.47,4.01,4.33,4.28,3.53,3.47,3.81,4.51,4.62,4.77,3.94,5.11,6.16,5.8,5.27,4.79,5.37,6.05,6.56,5.59,4.46,4.57,4.19,5.03,5.45,5.43,6.17,5.34,4.5,4.66,5.62,5.59,2.73,5.28,6.25,5.37,5.17,6.07,4.61,4.9,5.47,5.63,5.7,4.85,4.46,4.29,5.58,5.97,5.14,3.84,2.98,5.78,6.16,5.8,5.46,3.63,3.57,3.88,4.14,5.1,3.89,4.71,5.03,4.7,4.94,5.91,5.56,5.79,3.56,3.6,5.5,5.87,5.11,4.87,6.74,6.57,4.27,5.31,4.16,5.41,6.54,5.33,6.19,5.25,4.34,4.87,0.31,0.49,0.43,0.27,0.65,0.3,0.7,0.35,0.74,0.25,0.28,1.26,0.58,0.14,0.17,0.18,0.13,0.15,0.18,0.16,0.17,0.2,0.17,0.15,0.12,0.28,0.49,0.27,0.18,0.12,0.18,0.2,0.13,0.16,0.14,0.13,0.21,0.14,0.17,0.13,0.13,0.22,0.23,0.16,5.34,8.88,8.43,12.86,7.91,7.23,5.99,9.4,10.28,6.43,6.1,7.92,4.12,10.46,13.69,11.72,8.21,9.21,12.17,9.81,6.92,9.14,6.71,6.14,5.77,6.96,12.53,11.06,6.99,4.37,5.76,6.03,5.34,4.29,8.7,8.13,12.16,8.91,4.12,8.06,6.06,7.15,11.03,6.43,0.07,0.09,0.06,0.09,0.05,0.04,0.07,0.05,0.16,0.23,0.07,0.04,0.07,0.12,0.09,0.14,0.2,0.13,0.15,0.16,0.07,0.18,0.16,0.19,0.29,0.04,0.14,0.14,0.07,0.1,0.24,0.21,0.15,0.08,0.28,0.12,0.1,0.11,0.07,0.1,0.1,0.14,0.09,0.13,0.05,0.11,0.07,0.06,0.08,0.19,0.08,0.1,0.12,0.1,0.09,0.07,0.13,0.07,0.06,0.06,0.07,0.08,0.07,0.07,0.07,0.07,0.16,0.08,0.07,0.07,0.1,0.06,0.06,0.09,0.08,0.05,0.05,0.14,0.05,0.06,0.07,0.11,0.08,0.06,0.04,0.05,0.05,0.08,1.18,1.12,1.07,1.23,1.19,0.96,1.28,1.2,2.81,1.1,1.05,1.23,1.24,1.74,1.89,1.94,1.77,1.77,1.86,2.05,2.11,3.11,2.15,2.97,2.68,1.53,1.83,1.88,1.83,2.1,2.85,2.98,2.17,1.63,2.73,1.73,2.09,1.92,1.86,2.41,1.86,2.27,2.08,2.18,1.04,1.49,1.22,1.58,1.9,2.05,0.85,1.58,2.61,1.71,1.04,1.97,1.36,0.64,1.07,0.98,0.78,0.91,1.38,1.14,1.18,0.66,0.91,1.27,1.24,0.88,0.97,0.58,1.18,0.96,0.71,0.68,1.11,1.69,1.61,1.58,0.77,2,1.09,1.61,1.27,1.29,1.2,1.05,630,380,420,460,380,350,650,500,340,330,320,420,420,480,430,280,300,230,470,280,400,350,310,240,250,490,310,370,360,400,350,410,370,620,180,300,470,390,510,420,330,420,420,360,550,950,590,960,800,870,1050,520,590,1090,810,680,660,2000,2190,1270,910,1410,1830,1460,950,640,770,1500,1750,1740,1340,1950,1290,1600,620,700,1510,2250,880,2450,870,2970,2560,1250,1700,750,920,800,10,20,20,20,40,20,20,10,40,20,70,0,50,0,40,0,10,60,30,40,20,20,0,60,50,80,10,30,30,30,10,20,40,30,30,0,40,20,50,20,40,40,30,20,20,30,20,80,60,20,60,40,80,40,20,30,10,30,20,30,20,10,40,10,10,20,30,30,20,40,40,40,20,50,40,20,70,50,20,40,0,60,20,10,60,40,40,40,70,80,50,70,90,80,90,70,120,70,40,90,100,100,110,70,80,90,120,80,90,110,80,110,120,110,120,110,80,90,100,120,70,120,80,100,110,120,120,100,110,120,70,90,60,40,90,70,120,50,90,50,90,70,20,40,40,120,70,130,120,50,140,100,50,70,120,110,150,140,70,230,90,200,100,110,140,90,70,150,80,180,80,90,110,100,90,90
    ),
    nrow = 88, ncol = 11, byrow = F)
    ))
    matrix<-data()
    
    sampleclassX<-c("Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Body","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze","Glaze"
    )
    sampleclass((sampleclassX))
    
    
    sampleclasscolourX<-c("100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","100","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99","99"
    )
    sampleclasscolour((sampleclasscolourX))
    
    
    variablesX<-c("Na2O","MgO","Al2O3","SiO2","K2O","CaO","TiO2","Fe2O3","MnO","CuO","ZnO")
    variables(t(variablesX))
    colnames(matrix)<-t(variables())
    
    
    idX<-c("FLQ-1-b","FLQ-2-b","FLQ-3-b","FLQ-4-b","FLQ-5-b","FLQ-6-b","FLQ-7-b","FLQ-8-b","FLQ-9-b","FLQ-10-b","FLQ-11-b","FLQ-12-b","FLQ-13-b","DY-BS-1-b","DY-BS-2-b","DY-BS-3-b","DY-BS-4-b","DY-BS-5-b","DY-BS-6-b","DY-BS-7-b","DY-NS-1-b","DY-NS-2-b","DY-NS-3-b","DY-NS-4-b","DY-NS-5-b","DY-NS-6-b","DY-NS-7-b","DY-NS-8-b","DY-Y-1-b","DY-Y-2-b","DY-Y-3-b","DY-Y-4-b","DY-Y-5-b","DY-Y-6-b","DY-Y-7-b","DY-Y-8-b","DY-Y-9-b","DY-M-1-b","DY-M-2-b","DY-M-3-b","DY-QC-1-b","DY-QC-2-b","DY-QC-3-b","DY-QC-4-b","FLQ-1-g","FLQ-2-g","FLQ-3-g","FLQ-4-g","FLQ-5-g","FLQ-6-g","FLQ-7-g","FLQ-8-g","FLQ-9-g","FLQ-10-g","FLQ-11-g","FLQ-12-g","FLQ-13-g","DY-BS-1-g","DY-BS-2-g","DY-BS-3-g","DY-BS-4-g","DY-BS-5-g","DY-BS-6-g","DY-BS-7-g","DY-NS-1-g","DY-NS-2-g","DY-NS-3-g","DY-NS-4-g","DY-NS-5-g","DY-NS-6-g","DY-NS-7-g","DY-NS-8-g","DY-Y-1-g","DY-Y-2-g","DY-Y-3-g","DY-Y-4-g","DY-Y-5-g","DY-Y-6-g","DY-Y-7-g","DY-Y-8-g","DY-Y-9-g","DY-M-1-g","DY-M-2-g","DY-M-3-g","DY-QC-1-g","DY-QC-2-g","DY-QC-3-g","DY-QC-4-g"
    )
    id(t(idX))
    rownames(matrix)<-t(id())
    
    data(matrix)
    
  })
  
  
  #--------Coffee
  
  observeEvent(input$demodata, {
    if (input$demodata=='coffeeDEMO'){
      updateCheckboxInput(inputId = "isspectra", value = T)
      updateCheckboxInput(inputId = "classcol", value = T)
    }
  })
  
  
  observeEvent(input$preview, {
    
    req(input$datatype=='demos')
    req(input$demodata=='coffeeDEMO')
    
    updateCheckboxInput(inputId = "isspectra", value = T)
    
    data(data.frame(matrix(c(1.65716004371643,2.1431999206543,2.30397009849548,1.30031001567841,1.54627001285553,2.21359992027283,1.86419999599457,1.44939005374908,1.80877995491028,1.62314999103546,1.99026000499725,1.72994005680084,2.17861008644104,1.29288005828857,1.5009800195694,2.84717011451721,2.48274993896484,2.39531993865967,2.10507011413574,2.16958999633789,2.12119007110596,2.43532991409302,2.03776001930237,2.26879000663757,2.36180996894836,2.65240001678467,2.26901006698608,1.78468000888824,2.11086010932922,2.57463002204895,1.46361005306244,1.89471995830536,2.03759002685547,1.15733003616333,1.37097001075745,1.98488998413086,1.66676998138428,1.29296004772186,1.61327004432678,1.44709002971649,1.775750041008,1.5401599407196,1.93257999420166,1.13378000259399,1.31477999687195,2.52414989471436,2.17828989028931,2.10759997367859,1.85494995117188,1.91709995269775,1.88270998001099,2.15132999420166,1.80304002761841,2.00809001922607,2.08213996887207,2.34843993186951,1.98685002326965,1.57296001911163,1.86054003238678,2.27828001976013,1.32458996772766,1.71501994132996,1.83981001377106,1.04908001422882,1.23711001873016,1.82289004325867,1.53310000896454,1.18203997612,1.47652995586395,1.32465004920959,1.62916994094849,1.40906000137329,1.76349997520447,1.01266002655029,1.17417001724243,2.23839998245239,1.95711994171143,1.88788998126984,1.65586996078491,1.71566998958588,1.68849003314972,1.9291900396347,1.61418998241425,1.7972799539566,1.86214005947113,2.09321999549866,1.77850997447968,1.41570997238159,1.66841995716095,2.02390003204346,1.21580004692078,1.5752500295639,1.69530999660492,0.965582013130188,1.13863003253937,1.69950997829437,1.42952001094818,1.09872996807098,1.37323999404907,1.23343002796173,1.51604998111725,1.30884003639221,1.64067995548248,0.927905023097992,1.07612001895905,1.998379945755,1.75654995441437,1.69173002243042,1.48171997070312,1.54001998901367,1.52151000499725,1.73403000831604,1.447350025177,1.61704003810883,1.66977000236511,1.87202000617981,1.59491002559662,1.2765599489212,1.50107002258301,1.80987000465393,1.14163994789124,1.47943997383118,1.5963100194931,0.907582998275757,1.07071995735168,1.61211001873016,1.35678994655609,1.04186999797821,1.30432999134064,1.17261004447937,1.43623995780945,1.23973000049591,1.55648994445801,0.874164998531342,1.01438999176025,1.81947004795074,1.60625004768372,1.54560005664825,1.35020995140076,1.40610003471375,1.39451003074646,1.58653998374939,1.3218799829483,1.48355996608734,1.52614998817444,1.70643997192383,1.45729994773865,1.17181003093719,1.37821996212006,1.65235996246338,1.0833899974823,1.40670001506805,1.51708996295929,0.858729004859924,1.01627004146576,1.53541004657745,1.28936994075775,0.992810010910034,1.24486994743347,1.11916995048523,1.36526000499725,1.18124997615814,1.48499000072479,0.836740970611572,0.972424983978271,1.70677995681763,1.51250004768372,1.45295000076294,1.26816999912262,1.32088994979858,1.31132996082306,1.48942995071411,1.24050998687744,1.39593994617462,1.43113994598389,1.59573996067047,1.3717600107193,1.10508000850677,1.29978001117706,1.55074000358582,1.02625000476837,1.33580005168915,1.44351994991302,0.809723973274231,0.963981986045837,1.44595003128052,1.21456003189087,0.938601016998291,1.17999005317688,1.05825996398926,1.28505003452301,1.11643004417419,1.40782999992371,0.804751992225647,0.936644017696381,1.65601003170013,1.45799005031586,1.40734994411469,1.22990000247955,1.27542996406555,1.26473999023438,1.43418002128601,1.19719004631042,1.3474600315094,1.38082003593445,1.53840005397797,1.32702004909515,1.06481003761292,1.257159948349,1.50078999996185,0.967621982097626,1.2661600112915,1.36922001838684,0.759730994701385,0.909105002880096,1.34706997871399,1.12992000579834,0.878778994083405,1.1073499917984,0.990642011165619,1.19593000411987,1.04455995559692,1.32366001605988,0.771602988243103,0.899030983448029,1.64846003055573,1.44379997253418,1.39869999885559,1.223140001297,1.26223003864288,1.2446700334549,1.40701997280121,1.1810200214386,1.3277200460434,1.36500000953674,1.51933002471924,1.31691002845764,1.04867005348206,1.2433500289917,1.49171996116638,0.920408010482788,1.20911002159119,1.3077700138092,0.722994029521942,0.868803024291992,1.27907001972198,1.07149004936218,0.836405992507935,1.05748999118805,0.942740023136139,1.13345003128052,0.992842972278595,1.26364004611969,0.748003005981445,0.87296599149704,1.67865002155304,1.45448994636536,1.41385996341705,1.2393000125885,1.27338004112244,1.25069999694824,1.41066002845764,1.19245994091034,1.33571004867554,1.38087999820709,1.53488004207611,1.33212995529175,1.05140995979309,1.25012004375458,1.51296997070312,0.896179020404816,1.175989985466,1.27225005626678,0.706366002559662,0.848994970321655,1.25742995738983,1.05466997623444,0.821762979030609,1.04024994373322,0.92704701423645,1.1156200170517,0.976171970367432,1.24539005756378,0.729393005371094,0.852733016014099,1.73171997070312,1.49911999702454,1.4595400094986,1.27488994598389,1.30533003807068,1.28358995914459,1.44644999504089,1.2248899936676,1.36614000797272,1.43115997314453,1.58540999889374,1.37223994731903,1.081169962883,1.2844500541687,1.55467998981476,0.906661987304688,1.1826000213623,1.28328001499176,0.714451014995575,0.856656014919281,1.2878999710083,1.07837998867035,0.836863994598389,1.05788004398346,0.945971012115479,1.14247000217438,0.993339002132416,1.26936995983124,0.73053902387619,0.853434026241302,1.82625997066498,1.56263995170593,1.52838003635406,1.33054995536804,1.36454999446869,1.34420001506805,1.51591002941132,1.28418004512787,1.42920994758606,1.5171400308609,1.67527997493744,1.43597996234894,1.13187003135681,1.33955001831055,1.61836004257202,0.922483026981354,1.19419002532959,1.29495000839233,0.731274008750916,0.871276021003723,1.33684003353119,1.12093997001648,0.863358020782471,1.08498001098633,0.976611971855164,1.18912994861603,1.02550005912781,1.30288994312286,0.726266980171204,0.848285019397736,1.89271998405457,1.61928999423981,1.58006000518799,1.36940002441406,1.40789997577667,1.39303994178772,1.57583999633789,1.33229994773865,1.47614002227783,1.58855998516083,1.74583995342255,1.48241996765137,1.17540001869202,1.38142001628876,1.6674200296402,0.961769998073578,1.23637998104095,1.34139001369476,0.762413024902344,0.901866972446442,1.40448999404907,1.17954003810883,0.901952981948853,1.12928998470306,1.02304995059967,1.25278997421265,1.07471001148224,1.35652995109558,0.739058971405029,0.861082017421722,1.95033001899719,1.66888999938965,1.62767994403839,1.40503001213074,1.44963002204895,1.44247996807098,1.63921999931335,1.37850999832153,1.52296996116638,1.65520000457764,1.81726002693176,1.52358996868134,1.21695005893707,1.42299997806549,1.7104400396347,0.992411017417908,1.26945996284485,1.37582004070282,0.792488992214203,0.929543972015381,1.46326005458832,1.23002994060516,0.935310006141663,1.16411995887756,1.059730052948,1.30729997158051,1.11564004421234,1.3965300321579,0.749305009841919,0.871272027492523,1.94634997844696,1.66788995265961,1.62218999862671,1.40062999725342,1.45318996906281,1.45204997062683,1.65182995796204,1.3846800327301,1.53292000293732,1.66945004463196,1.83542001247406,1.5181599855423,1.21940004825592,1.4180999994278,1.70280003547668,1.02805995941162,1.31014001369476,1.42251002788544,0.823800981044769,0.958513021469116,1.53504002094269,1.29162001609802,0.976395010948181,1.21340000629425,1.10837996006012,1.37668001651764,1.16751003265381,1.45648002624512,0.761059999465942,0.884036004543304,1.89303004741669,1.63082003593445,1.58078002929688,1.36171996593475,1.42530000209808,1.43078005313873,1.63021004199982,1.35885000228882,1.50441002845764,1.63863003253937,1.79692995548248,1.47696995735168,1.19966995716095,1.38457000255585,1.65654003620148,1.05335998535156,1.33689999580383,1.45609998703003,0.843342006206512,0.974250018596649,1.59336996078491,1.34300005435944,1.00850999355316,1.2533700466156,1.14877998828888,1.43216001987457,1.2079199552536,1.507159948349,0.762884974479675,0.884730994701385,1.78201997280121,1.54410004615784,1.48969995975494,1.28232002258301,1.35476005077362,1.36892998218536,1.5648900270462,1.29056000709534,1.43629002571106,1.5529899597168,1.70451998710632,1.39160001277924,1.14660000801086,1.31093001365662,1.56121003627777,1.06160998344421,1.34519004821777,1.46750998497009,0.847329020500183,0.973796010017395,1.62506997585297,1.36750996112823,1.02241003513336,1.27026998996735,1.16867995262146,1.46124994754791,1.22651994228363,1.53036999702454,0.751792013645172,0.871425986289978,1.62993001937866,1.42209994792938,1.36573004722595,1.17446994781494,1.25473999977112,1.2791600227356,1.45955002307892,1.1942800283432,1.33433997631073,1.433109998703,1.57061994075775,1.2754100561142,1.06781005859375,1.21484994888306,1.42926001548767,1.04778003692627,1.32788002490997,1.45003998279572,0.835447013378143,0.956369996070862,1.62425994873047,1.3701000213623,1.01894998550415,1.26772999763489,1.16656005382538,1.46336996555328,1.22238004207611,1.52996003627777,0.72986102104187,0.845438003540039,1.47283005714417,1.29611003398895,1.23729002475739,1.06403994560242,1.15085005760193,1.18156003952026,1.35054004192352,1.09257996082306,1.23245000839233,1.30934000015259,1.43355000019073,1.15479004383087,0.9845330119133,1.11299002170563,1.29395997524261,1.028480052948,1.30238997936249,1.42510998249054,0.818801999092102,0.934651970863342,1.60771000385284,1.35596001148224,1.00576996803284,1.25134003162384,1.1538200378418,1.44681000709534,1.20916998386383,1.50855004787445,0.706775009632111,0.81870698928833,1.36652004718781,1.21356999874115,1.15321004390717,0.985283017158508,1.0749100446701,1.11300003528595,1.2700799703598,1.02037000656128,1.15749001502991,1.22625994682312,1.33758997917175,1.07740998268127,0.930568993091583,1.04560995101929,1.20069003105164,1.00639998912811,1.26877999305725,1.38970994949341,0.799758970737457,0.913048028945923,1.58106005191803,1.33657002449036,0.988300979137421,1.23117995262146,1.13568997383118,1.42481005191803,1.18441998958588,1.48573005199432,0.684307992458344,0.792475998401642,1.29647994041443,1.15620994567871,1.09635996818542,0.935584008693695,1.02708005905151,1.06581997871399,1.21781003475189,0.973990976810455,1.11208999156952,1.17200005054474,1.27320003509521,1.0237900018692,0.893148005008698,0.998081028461456,1.13734996318817,0.989546000957489,1.24597001075745,1.36406004428864,0.786696970462799,0.897364974021912,1.56900000572205,1.32513999938965,0.976373970508575,1.21774005889893,1.12312996387482,1.41057002544403,1.17367005348206,1.46886003017426,0.66745799779892,0.77233099937439,1.25775003433228,1.12559998035431,1.06591999530792,0.904071986675262,0.996258974075317,1.03826999664307,1.18708002567291,0.946636974811554,1.08083999156952,1.14305996894836,1.23786997795105,0.994741976261139,0.874275028705597,0.969821989536285,1.09879994392395,0.983780026435852,1.23178994655609,1.35406005382538,0.781560003757477,0.8887619972229,1.5629700422287,1.32148003578186,0.971433997154236,1.21177995204926,1.11794996261597,1.40548002719879,1.16814994812012,1.46017003059387,0.656136989593506,0.75840300321579,1.23515999317169,1.11018002033234,1.05025005340576,0.884571015834808,0.979174971580505,1.02389001846313,1.17097997665405,0.932132005691528,1.06436002254486,1.13136994838715,1.2232700586319,0.980937004089355,0.866775989532471,0.957861006259918,1.07814002037048,0.986588001251221,1.22949004173279,1.3546199798584,0.783984005451202,0.891333997249603,1.57765996456146,1.33704996109009,0.978995978832245,1.22442996501923,1.12933003902435,1.42207002639771,1.17726004123688,1.47339999675751,0.651404023170471,0.753304004669189,1.2284699678421,1.1068400144577,1.04539000988007,0.873831987380981,0.973101019859314,1.0200400352478,1.17102003097534,0.927156984806061,1.06136000156403,1.13162994384766,1.22109997272491,0.97638601064682,0.86744898557663,0.951004028320312,1.06679999828339,0.993848025798798,1.2333699464798,1.3600800037384,0.787265002727509,0.892409980297089,1.59921002388,1.35357999801636,0.987563014030457,1.23519003391266,1.14086997509003,1.43871998786926,1.1886899471283,1.48767995834351,0.647190988063812,0.747848987579346,1.22783005237579,1.10467004776001,1.04443001747131,0.868080019950867,0.971839010715485,1.02132999897003,1.17182004451752,0.92662501335144,1.06246995925903,1.1380900144577,1.22493994235992,0.973680973052979,0.870532989501953,0.950308978557587,1.06017994880676,0.993447005748749,1.22511994838715,1.35555994510651,0.783187985420227,0.886218011379242,1.59582996368408,1.35124003887177,0.986064016819,1.23017001152039,1.13882994651794,1.43781995773315,1.1842600107193,1.48370003700256,0.63695502281189,0.736451029777527,1.21658003330231,1.09862005710602,1.03778004646301,0.858264029026031,0.964478015899658,1.01688003540039,1.16908001899719,0.921037018299103,1.05773997306824,1.13521003723145,1.21886003017426,0.966701984405518,0.869916975498199,0.94336199760437,1.04783999919891,0.977084994316101,1.20292997360229,1.33262002468109,0.765567004680634,0.864602029323578,1.56639003753662,1.32771003246307,0.965943992137909,1.20753002166748,1.11825001239777,1.4130300283432,1.16217005252838,1.45553004741669,0.618619978427887,0.714754998683929,1.19132995605469,1.07647001743317,1.01657998561859,0.837224006652832,0.944181978702545,0.997461974620819,1.14617002010345,0.902040004730225,1.0362800359726,1.1153199672699,1.19695997238159,0.94725102186203,0.85461300611496,0.923826992511749,1.02418994903564,0.938889026641846,1.15670001506805,1.27837002277374,0.731831014156342,0.825498998165131,1.49005997180939,1.26733005046844,0.921783983707428,1.15225994586945,1.0660799741745,1.34791004657745,1.10852003097534,1.386549949646,0.588612020015717,0.678637981414795,1.14441001415253,1.03424000740051,0.977081000804901,0.805779993534088,0.909088015556335,0.961371004581451,1.10555994510651,0.86939400434494,0.998335003852844,1.0721800327301,1.15029001235962,0.909657001495361,0.822901010513306,0.888451993465424,0.983947992324829,0.881097018718719,1.08587002754211,1.19919002056122,0.683188021183014,0.770159006118774,1.38920998573303,1.1774799823761,0.858512997627258,1.07267999649048,0.990610003471375,1.251620054245,1.03363001346588,1.28933000564575,0.548416972160339,0.632337987422943,1.07553994655609,0.974044024944305,0.917912006378174,0.758700013160706,0.856604993343353,0.904030025005341,1.03928005695343,0.818180978298187,0.938682973384857,1.00567996501923,1.08016002178192,0.85638701915741,0.774107992649078,0.835596978664398,0.927552998065948,0.803108990192413,0.994634985923767,1.09662997722626,0.621281981468201,0.700568974018097,1.25297999382019,1.06304001808167,0.776127994060516,0.971715986728668,0.894044995307922,1.12917995452881,0.932928025722504,1.16550004482269,0.50008898973465,0.576313972473145,0.989242017269135,0.895909011363983,0.844461977481842,0.700487971305847,0.788896024227142,0.831573009490967,0.954430997371674,0.753026008605957,0.864286005496979,0.919721007347107,0.990360021591187,0.787729024887085,0.709918022155762,0.769640028476715,0.857609987258911,0.714931011199951,0.891220986843109,0.977379977703094,0.550913989543915,0.623049974441528,1.0983099937439,0.931586980819702,0.68266499042511,0.854318976402283,0.784089028835297,0.988972008228302,0.820807993412018,1.02254998683929,0.446871995925903,0.514819979667664,0.890693008899689,0.807363986968994,0.760468006134033,0.636007010936737,0.712137997150421,0.747925996780396,0.857594013214111,0.678179025650024,0.778301000595093,0.820931971073151,0.887512981891632,0.711133003234863,0.63639897108078,0.693364977836609,0.778252005577087,0.619356989860535,0.779862999916077,0.850170016288757,0.477708995342255,0.541545987129211,0.936079025268555,0.792825996875763,0.584034979343414,0.73123300075531,0.667931973934174,0.840296983718872,0.702261984348297,0.87439101934433,0.391934990882874,0.451626986265182,0.785089015960693,0.713398993015289,0.671248018741608,0.566681981086731,0.630325973033905,0.658201992511749,0.752739012241364,0.598783016204834,0.686269998550415,0.715655982494354,0.778191983699799,0.629046022891998,0.557363986968994,0.613120019435883,0.694739997386932,0.525417983531952,0.670053005218506,0.725153028964996,0.406347006559372,0.463001012802124,0.777315974235535,0.658307015895844,0.488572001457214,0.611398994922638,0.554974019527435,0.697206020355225,0.587710022926331,0.729615986347198,0.339067012071609,0.391007989645004,0.681433975696564,0.620316982269287,0.583136975765228,0.498093008995056,0.549359023571014,0.568989992141724,0.648669004440308,0.519979000091553,0.594998002052307,0.610705018043518,0.669911026954651,0.548309028148651,0.479449987411499,0.533208012580872,0.611257016658783,0.438284993171692,0.568327009677887,0.610046982765198,0.341122001409531,0.391364991664886,0.633695006370544,0.535999000072479,0.401510000228882,0.502646028995514,0.452562004327774,0.566601991653442,0.482879996299744,0.597947001457214,0.290899008512497,0.335278987884521,0.585319995880127,0.534103989601135,0.501604974269867,0.43470698595047,0.473648995161057,0.486696004867554,0.552488029003143,0.447338998317719,0.51061999797821,0.514301002025604,0.569530010223389,0.473814994096756,0.40743699669838,0.459975004196167,0.533879995346069
    ),
    
    nrow = 30, ncol = 33, byrow = F)
    ))
    matrix<-data()
    
    sampleclassX<-c("1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","2","2","2","2","2"
    )
    sampleclass((sampleclassX))
    
    
    sampleclasscolourX<-c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99
    )
    sampleclasscolour((sampleclasscolourX))
    
    
    variablesX<-c(200,204,208,212,216,220,224,228,232,236,240,244,248,252,256,260,264,268,272,276,280,284,288,292,296,300,304,308,312,316,320,324,328
    )
    variables(data.frame((variablesX)))
    colnames(matrix)<-t(variables())
    
    
    idX<-c("Caffeine_1","Caffeine_2","Caffeine_3","Caffeine_4","Caffeine_5","Caffeine_6","Caffeine_7","Caffeine_8","Caffeine_9","Caffeine_10","Caffeine_11","Caffeine_12","Caffeine_13","Caffeine_14","Caffeine_15","Decaffeinated_1","Decaffeinated_2","Decaffeinated_3","Decaffeinated_4","Decaffeinated_5","Decaffeinated_6","Decaffeinated_7","Decaffeinated_8","Decaffeinated_9","Decaffeinated_10","Decaffeinated_11","Decaffeinated_12","Decaffeinated_13","Decaffeinated_14","Decaffeinated_15"
    )
    id(data.frame((idX)))
    rownames(matrix)<-t(id())
    
    data(matrix)
    
  })
  
  
  
  #-----------------------------------------------Standard Importation
  
  observeEvent(input$preview, {
    validate(need(input$datatype == "itfstd", message = "nope2"))
    
    req(input$datatype == "itfstd")
    req(input$file)
    load(input$file$datapath)
    
    data(data)
    variables(variables)
    id(id)
    sampleclasscolour(sampleclasscolour)
    sampleclass(sampleclass)
    
  })
  
  #------------------------------------------------------Start importation----------------------------------------------------------------------  
  
  {observeEvent(c(input$datatype, input$file),{
    req(input$file)
    
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    if (input$datatype=='xlsx' || input$datatype == 'xls')
    {updateSelectInput(inputId = "excelsheet", choices = c(""))
      validate(need(grepl(".xls", input$file$datapath)==T, message = "Wrong type of file"))
      updateSelectInput(inputId = "excelsheet", choices = excel_sheets(input$file$datapath))}
  }
  
  )}
  
  {observeEvent(input$datatype,{
    req(input$file)
    if (input$datatype=='txt')
      updateSelectInput(inputId = "delim", selected = '\t')
  }
  )}
  
  
  newdata<-eventReactive(input$preview,{
    
    req(input$file)
    data<-reactiveVal()
    pretreatdata<-reactiveVal()
    
    
    validate(need(input$datatype!="sas", message = "nope"))
    validate(need(input$datatype!="itfstd", message = "nope"))
    
    
    #-----------------------------------------------------------------Import csv, txt--------------------------------------
    
    
    if (input$datatype=="txt" || input$datatype == "csv")
    {decimal <- input$dec
    delim <- input$delim
    
    b <- read.table(input$file$datapath, fill = T, sep = delim, dec = decimal, header = input$labels, check.names = F)
    
    if (input$namerows == T)
    {c <- duplicated(b[,1])
    validate(need(TRUE%in%c != T, message = "Duplicated sample names are not permitted. Check the sample name option"),
             (need(ncol(b)>1, message = "There is probably a delimiter error, only 1 column has been detected")))
    
    a <- t(b[,1])
    b <- b[2:ncol(b)]
    validate(need(nrow(b)==ncol(a), message = "Check the delimiter selected"))
    rownames(b)<-a
    
    b
    }
    
    if (input$namerows == F)
    {validate(need(ncol(b)>1, message = "There is probably a delimiter error, only 1 column has been detected"))
      rownames(b)<-paste0("Sample ",1:nrow(b)) 
      b}
    
    b  }
    
    
    #------------------------------------------------------------------------Excel---------------------------------------------------------------------    
    if (input$datatype=="xlsx")
    {
      validate(need(grepl(".xls", input$file$datapath)==T, message = "Wrong type of file"))
      
      b <- read_excel(input$file$datapath, sheet = input$excelsheet, col_names = input$labels)
      
      if (input$namerows == T)
      {b<-column_to_rownames(b,var = colnames(b)[1])
      
      b
      }
      
      else
      {validate(need(ncol(b)>1, message = "There is probably a delimiter error, only 1 column has been detected"))
        colnames<-colnames(b)
        b<-data.frame(paste0("Sample ",1:nrow(b)),b)
        colnames(b)<-c("Name",colnames)
        b<-column_to_rownames(b,var = colnames(b)[1])
        #rownames(b)<-paste0("Sample ",1:nrow(b)) 
        b}
      
      
      b  }
    #-----------------------------------------------------------------------Final----------------------------------------------------------------------
    
    if(input$labels==F)
    {colnames(b)<-1:ncol(b)
    b}
    
    b
  })
  
  #-------------------------------------------------------------------------Class--------------------------------------------------------------------
  
  observeEvent(input$preview, {
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    req(input$file)
    
    
    if (input$classcol==T)
    {
      
      if(input$namerows == T){
        classpos<-input$classpos-1
      }
      
      if(input$namerows == F){
        classpos<-input$classpos
      }  
      
      matrix<-newdata()
      unique<-unique(matrix[[classpos]])
      length<-length(unique)
      numbers<-c(100:length)
      
      for (i in 1:length)
        matrix[[classpos]][matrix[[classpos]]==unique[[i]]]<-numbers[[i]]
      matrix[[classpos]]
      
      sampleclasscolour(matrix[[classpos]])}
    
    else
      sampleclasscolour(0)
    {}
    
    
  })
  
  observeEvent(input$preview,{
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    req(input$file)
    
    if(input$namerows == T){
      classpos<-input$classpos-1
    }
    
    if(input$namerows == F){
      classpos<-input$classpos
    }
    
    
    
    if (input$classcol==T)
    {matrix<-newdata()[,classpos]
    
    if (is.data.frame(matrix)==F)
      sampleclass(matrix)
    
    if(is.data.frame(matrix)==T)
      sampleclass(matrix[[classpos]]) 
    
    }
    
    else
      sampleclass(0)
    {}
  })
  
  
  
  #----------------------------------------------------------------------------------End of data importation--------------------------------------------  
  observeEvent(input$preview,{req(input$file)
    
    if (input$classcol==T)
    {data(newdata()[,-1])}
    
    
    else
    {data(newdata())}
    
  })
  
  observeEvent(input$preview,{req(input$file)
    
    if(input$isspectra==T)
    {matrix<-data.frame(as.double(sub(',','.',colnames(data()))))
    }
    
    else
    {matrix<-data.frame(colnames(data()))}
    
    variables(matrix)
  })
  
  observeEvent(input$preview,{req(input$file)
    
    id<-data.frame(rownames(data()))
    rownames(id)<-rownames(data())
    id
    
    id(id)
    
  })
  
  observeEvent(input$preview, {
    req(input$file)
    
    if (input$isspectra==T)
    {validate(need(ncol(data())>1, message = "There is probably a delimiter error, only 1 column has been detected"))
      
      matrix<-data()
      colnames(matrix)<-t(variables())
      data(matrix)}
    
    else
    {}
  })
  
  #----------------------------------------------------------------------------Remove data option----------------------------------------------------------
  observeEvent(data(), {
    req(data())
    
    updateSelectizeInput(inputId = "removedatacol", choices = list(as.character(t(variables()))), selected = NULL, server = T)
    updateSelectizeInput(inputId = "removedatarow", choices = list(as.character(t(id()))), selected = NULL, server = T)
    
  })
  
  
  observeEvent(input$removedatacolBT, {
    req(data())
    newdata<-data()[,-which(dimnames(data())[[2]] %in% input$removedatacol)]
    
    data(newdata)
    variables(colnames(data()))
    
  })
  
  observeEvent(input$removedatarowBT, {
    req(data())
    
    newdata<-data()[-which(dimnames(data())[[1]] %in% input$removedatarow),]
    matrix<-sampleclass()[-which(rownames(data()) %in% input$removedatarow)]
    matrix2<-sampleclasscolour()[-which(rownames(data()) %in% input$removedatarow)]
    
    data(newdata)
    id(rownames(data()))
    
    sampleclass(matrix)
    sampleclasscolour(matrix2)
    
  })
  
  #-----------------------------------------------------------------------------------------Summary and plot----------------------------------------------------------  
  
  observeEvent(input$preview, {
    req(data())
    
    output$datanullvalues<-renderPrint(which(is.na(data()), arr.ind = T))
    
    if(length(unique(duplicated(data())))!=1)
    {
      
      if(all(sapply (data(), is.numeric)))  
      {cor<-cor(t(data()))
      p<-which(cor>0.9999, arr.ind = T, useNames = F)
      dup<-p[which(p[1:nrow(p),1]!=p[1:nrow(p),2], useNames = T),]
      
      dupmatrix2<-matrix(c(rownames(data()[dup[,1],]),
                           rownames(data()[dup[,2],])),
                         nrow(dup),
                         2,
      )
      
      dupmatrix3<-dupmatrix2[order(dupmatrix2[,1]),]
      output$checkdup<-renderPrint(paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " are duplicate of others. It is advised to remove them"))  
      }
      
      showModal(modalDialog(title = "Warning" ,paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " have/are duplicates. Check which of then relate by viewing the 'Duplicated Values' tab at 'Data Overview'")
                            ,easyClose = T, footer = "Click anywhere to dismiss")) 
      
    }
    
  })
  
  observeEvent(input$preview,{
    req(data())
    
    if (input$isspectra == F)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelnormaldata")
      output$preview1 <- renderDT(data(),rownames = T,options = list(pageLength = 10), width = "200px")
      output$summary <- renderDT(data.frame(ExpData(data())),options = list(pageLength = 100))
    }
    if (input$isspectra == T)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelspectrum")
      updateSelectInput(inputId ="methodpca", selected = "nipals")
      if (input$classcol==T)
      {output$spectrumpreview <- renderPlot({matplot(y=t(data()), x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = sampleclasscolour())
        legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)})}
      
      if (input$classcol==F)
      {output$spectrumpreview <- renderPlot(matplot(y=t(data()),x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = ))}
      
      output$spectramsn <- renderText("Showing first 5 variables.")
      output$preview1 <- renderDT(data()[,1:10],rownames = T, options = list(pageLength = 10), width = 200)}
    
    
  })
  
  
  
  #---------------------------------------------------------------------------------------------Pretreat Plots---------------------------------------------------------------------  
  observeEvent(data(),{
    req(data())
    updateSelectizeInput(inputId = "whichsamplesprepro", choices = list(as.character(t(id()))), selected = NULL, server = T)
  })
  
  observeEvent(c(input$pretreatcopyBT, input$preview, input$plotpretreatclasses, input$whichsamplesprepro), ignoreInit = T,{
    
    lwd=1
    cex.axis=1
    # column(width = 4, plotlyOutput("normaldata")),
    # column(width = 4, plotlyOutput("meancenterdata")),
    # column(width = 4, plotlyOutput("scaleddata"))
    
  #   column(radioButtons("plotpretreatclasses" ,"Type of Coloring:",
  #                       inline = T,
  #                       choices = c("Show individually"="indpretreatplot", "Show by class"="classpretreatplot")
  #   ), width = 4),
  #   column(checkboxInput("selectpreprosamples", "Select some samples to plot?"), width = 4),
  #   column(conditionalPanel("input.selectpreprosamples == true",selectizeInput("whichsamplesprepro", "Which Samples?", choices = c(), multiple=T)), width = 4)
  # ),
    
    

    if(input$selectpreprosamples == F){
      
      data<-data()
      
    }
    
    if(input$selectpreprosamples == T){
      
      req(input$whichsamplesprepro!="")
      
      data<-data()[input$whichsamplesprepro,]
      
    }
    
    
    if(input$isspectra == T){  
      
      if (input$plotpretreatclasses == "indpretreatplot")
      {
        output$normaldata<-renderPlot({matplot(y=t(data), x=variables(),lty = 1, col = , type = "l", main = "Data", xlab = "", ylab = "")
        })
        
        output$meancenterdata<-renderPlot({matplot(y=t(scale(data, scale = F)), x=variables(),lty = 1, col = , type = "l", main = "Mean Centered", xlab = "", ylab = "")
        })
        
        output$scaleddata<-renderPlot({matplot(y=t(scale(data)), x=variables(),lty = 1, col = , type = "l", main = "Scaled", xlab = "", ylab = "")
        })
        
      }
      
      
      if (input$plotpretreatclasses == "classpretreatplot")
      {
        output$normaldata<-renderPlot({
          
          matplot(y=t(data), x=variables(),lty = 1, col = sampleclasscolour(), type = "l", main = "Data", xlab = "", ylab = "")
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)}
        )
        
        output$meancenterdata<-renderPlot({
          
          matplot(y=t(scale(data, scale = F)), x=variables(),lty = 1, col = sampleclasscolour(), type = "l", main = "Mean Centered", xlab = "", ylab = "")
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)}
        )
        
        output$scaleddata<-renderPlot({
          
          matplot(y=t(scale(data)), x=variables(),lty = 1, col = sampleclasscolour(), type = "l", main = "Scaled", xlab = "", ylab = "")
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)}
        )
        
      }
      
      
      {    # if (input$plotpretreatclasses == "classmeanspretreatplot")
        # {
        #   output$normaldata<-renderPlot({
        #   
        #       matplot(t(data()), x=variables(), col = scales::alpha("white", 0.1), type = "l", lty = 1, main = "Data", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
        #       for (i in 1:length(unique(sampleclass()))) {
        #         matplot(y=t(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]), x=variables(),type="l", col=scales::alpha(unique(sampleclasscolour())[[i]],0.25), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = lwd/2, cex.axis = cex.axis)
        #         }
        #       
        #       for (i in 1:length(unique(sampleclass()))) {
        #        matplot(colMeans(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]),  x=variables(), col = scales::alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
        #       }
        #       
        #     legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
        #       
        #     })
        #   
        #   
        #   output$scaleddata<-renderPlot({
        #     
        #     matplot(scale(t(data())), x=variables(), col = scales::alpha("white", 0.1), type = "l", lty = 1, main = "Scaled", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(y=scale(t(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),])), x=variables(),type="l", col=scales::alpha(unique(sampleclasscolour())[[i]],0.25), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = lwd/2, cex.axis = cex.axis)
        #     }
        #     
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(colMeans(scale(t(data())[,which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T))])),  x=variables(), col = scales::alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
        #     }
        #     
        #     legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
        #     
        #   })
        #   
        #   output$meancenterdata<-renderPlot({
        #     
        #     matplot(scale(t(data()), scale = F), x=variables(), col = scales::alpha("white", 0.1), type = "l", lty = 1, main = "Mean Centered", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(y=scale(t(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]), scale = F), x=variables(),type="l", col=scales::alpha(unique(sampleclasscolour())[[i]],0.25), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = lwd/2, cex.axis = cex.axis)
        #     }
        #     
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(colMeans(scale(t(data())[,which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T))], scale = F)),  x=variables(), col = scales::alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
        #     }
        #    
        #     legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
        #     
        #   })
        # }
      }
    }
    
    if(input$isspectra == F){  
      
      if (input$plotpretreatclasses == "indpretreatplot")
      {
        output$normaldata<-renderPlot({matplot(y=t(data), lty = 1, col = , type = "l", main = "Data", xlab = "", ylab = "")
          #axis(1, at = 1:length(variables()), labels = t(variables()))
        })
        
        output$meancenterdata<-renderPlot({matplot(y=t(scale(data, scale = F)),lty = 1, col = , type = "l", main = "Mean Centered", xlab = "", ylab = "")
          #axis(1, at = 1:length(variables()), labels = t(variables()))
        })
        
        output$scaleddata<-renderPlot({matplot(y=t(scale(data, scale = T)),lty = 1, col = , type = "l", main = "Scaled", xlab = "", ylab = "")
          #axis(1, at = 1:length(variables()), labels = t(variables()))
        })
        
      }
      
      
      if (input$plotpretreatclasses == "classpretreatplot")
      {
        output$normaldata<-renderPlot({
          
          matplot(y=t(data), lty = 1, col = sampleclasscolour(), type = "l", main = "Data", xlab = "", ylab = "")
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
          #axis(1, at = 1:length(variables()), labels = t(variables()))
        }
        )
        
        output$meancenterdata<-renderPlot({
          
          matplot(y=t(scale(data, scale = F)), lty = 1, col = sampleclasscolour(), type = "l", main = "Mean Centering", xlab = "", ylab = "")
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
          #axis(1, at = 1:length(variables()), labels = t(variables()))
        }
        )
        
        output$scaleddata<-renderPlot({
          
          matplot(y=t(scale(data)), lty = 1, col = sampleclasscolour(), type = "l", main = "Scaled", xlab = "", ylab = "")
          legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
          #axis(1, at = 1:length(variables()), labels = t(variables()))
        }
        )
        
      }
      
      
      {    # if (input$plotpretreatclasses == "classmeanspretreatplot")
        # {
        #   output$normaldata<-renderPlot({
        #   
        #       matplot(t(data()), x=variables(), col = scales::alpha("white", 0.1), type = "l", lty = 1, main = "Data", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
        #       for (i in 1:length(unique(sampleclass()))) {
        #         matplot(y=t(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]), x=variables(),type="l", col=scales::alpha(unique(sampleclasscolour())[[i]],0.25), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = lwd/2, cex.axis = cex.axis)
        #         }
        #       
        #       for (i in 1:length(unique(sampleclass()))) {
        #        matplot(colMeans(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]),  x=variables(), col = scales::alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
        #       }
        #       
        #     legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
        #       
        #     })
        #   
        #   
        #   output$scaleddata<-renderPlot({
        #     
        #     matplot(scale(t(data())), x=variables(), col = scales::alpha("white", 0.1), type = "l", lty = 1, main = "Scaled", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(y=scale(t(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),])), x=variables(),type="l", col=scales::alpha(unique(sampleclasscolour())[[i]],0.25), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = lwd/2, cex.axis = cex.axis)
        #     }
        #     
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(colMeans(scale(t(data())[,which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T))])),  x=variables(), col = scales::alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
        #     }
        #     
        #     legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
        #     
        #   })
        #   
        #   output$meancenterdata<-renderPlot({
        #     
        #     matplot(scale(t(data()), scale = F), x=variables(), col = scales::alpha("white", 0.1), type = "l", lty = 1, main = "Mean Centered", xlab = "", ylab = "", lwd = 2*lwd/3, cex.axis = cex.axis)
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(y=scale(t(data()[which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T)),]), scale = F), x=variables(),type="l", col=scales::alpha(unique(sampleclasscolour())[[i]],0.25), lty=1, alpha = 1, add = T, main = "", xlab = "", ylab = "", lwd = lwd/2, cex.axis = cex.axis)
        #     }
        #     
        #     for (i in 1:length(unique(sampleclass()))) {
        #       matplot(colMeans(scale(t(data())[,which(grepl(unique(sampleclass())[[i]], sampleclass(), fixed = T))], scale = F)),  x=variables(), col = scales::alpha(unique(sampleclasscolour())[[i]],1) ,type="l", lty = 1, add=T, main = "", xlab = "", ylab = "", lwd = lwd+2, cex.axis = cex.axis)
        #     }
        #    
        #     legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)
        #     
        #   })
        # }
      }
    }
    
    
    
    
    
    
  })
  
  #---------------------------------------------------------------------------------------------Adequacy Tests----------------------------------------------------------------------
  observeEvent(c(input$barttestBT),ignoreInit = T, {
    req(data())
    
    output$Barteletttest<-renderPrint(cortest.bartlett(cor(data()), n = nrow(data()), diag = T))
  })
  
  observeEvent(c(input$KMOBT),ignoreInit = T, {
    req(data())
    
    output$KMOtest<-renderPrint(KMO(cor(data())))
  })
  
  observeEvent(c(input$ChiBT),ignoreInit = T, {
    req(data())
    req(length(which(data()<0)))
    
    output$Chitest<-renderPrint(chisq.test(data()))
  })
  
  observeEvent(c(input$FlKBT),ignoreInit = T, {
    req(data())
    
    output$FlKtest<-renderPrint(fligner.test(data()))
  })
  
  observeEvent(c(input$LevBT),ignoreInit = T, {
    req(data())
    
    output$Levtest<-renderPrint(varGroupTest(data(), test = "Levene"))
  })
 
  observeEvent(c(input$BartHomoBT),ignoreInit = T, {
    req(data())
    
    output$BartHomotest<-renderPrint(bartlett.test(data()))
  })
  
  observeEvent(c(input$ChisOutBT),ignoreInit = T, {
    req(data())
    
    output$ChisOuttest<-renderPrint(chisq.out.test(data.matrix(data())))
  })
  
  observeEvent(c(input$DixonBT),ignoreInit = T, {
    req(data())
    
    output$Dixontest<-renderPrint(dixon.test(data.matrix(data())))
  })
  
  observeEvent(c(input$GrubbsBT),ignoreInit = T, {
    req(data())
    
    output$Grubbstest<-renderPrint(grubbs.test(data.matrix(data())))
  })
  
  #---------------------------------------------------------------------------------------------Cluster Analysis--------------------------------------------------------------------
  observeEvent(input$useHCAasclass, {
    req(data())
    req(HCAclass())
    
    sampleclass(as.vector(HCAclass()))
  })
  
  observeEvent(input$useKMasclass, {
    req(data())
    req(KMclass())
    
    sampleclass(as.character(KMclass()))
  })
  
  
  #-------------------Cluster optimization
  observeEvent(data(), {
    req(data())
    
    if(nrow(data()) > 20){
      nmax <- 20}
    if(nrow(data()) < 20){
      nmax <- nrow(data())-1}

    
  output$ElbowMethod <- renderPlot(fviz_nbclust(data(), kmeans ,method = "wss", k.max = nmax, diss = dist(data(), method = input$distmethodCLUSTER))+labs(title = "Elbow method"))
  output$SilhouetteMethod <- renderPlot(fviz_nbclust(data(), kmeans ,method = "silhouette", k.max = nmax, diss = dist(data(), method = input$distmethodCLUSTER))+labs(title = "Silhouette method"))
  output$GapMethod <- renderPlot(fviz_nbclust(data(), kmeans ,method = "gap_stat", k.max = nmax, diss = dist(data(), method = input$distmethodCLUSTER))+labs(title = "Gap statistic method"))
  })
  
  observeEvent(c(input$runclusterBT),ignoreInit = T, {
    req(data())
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    if (input$typedataCLUSTER==T){
      data <- {
        load(input$modelCLUSTERdata$datapath)
        data2<-data
        data2
      }
    }
    
    if (input$typedataCLUSTER==F){
      data<-data()
    }
    
    
    #--------------------Calculations    
    dist<-dist(data, method = input$distmethodCLUSTER)
    hc_values<-hclust(dist, method = input$HCAmethod)
    
    dend<-as.dendrogram(hc_values)
    dend<-color_branches(dend,k=input$nClustersHCA, h=NULL, groupLabels = T)
    dend <- set(dend, "labels_cex", 0.7)
    labels_colors(dend)<-get_leaves_branches_col(dend)
    
    
    KM<-kmeans(data, centers = input$nClustersHCA)
    
    
    
    #---------------------Get classes   
    HCAclass<-get_leaves_branches_col(dend)
    unique<-unique(as.vector(HCAclass))
    length<-length(unique)
    numbers<-paste("Cluster",1:length)
    
    
    for (i in 1:length){
      HCAclass[HCAclass==unique[[i]]]<-numbers[[i]]}
    
    HCAclass(HCAclass)
    
    KMclass<-KM$cluster
    unique<-unique(as.vector(KMclass))
    length<-length(unique)
    numbers<-paste("Cluster",1:length)
    
    
    for (i in 1:length){
      KMclass[KMclass==unique[[i]]]<-numbers[[i]]}
    
    KMclass(KMclass)

    
    removeModal()
    
    
    #---------------------Dendogram  
    output$HCAplot <- renderPlot(height = 750,{
      
      if (input$circlelizeDendo == F){
        plot(dend, main = "Dendogram of clustered data", horiz = F)}
      
      
      if (input$circlelizeDendo == T){
        plot(dend, main = "Dendogram of clustered data", horiz = F)
        circlize_dendrogram(dend)}
      
    })
    
    #------------------------Kmeans
    output$KMplot<-renderPlot(fviz_cluster(KM, data()))
    
    #------------------------Scatter
    
    output$HCAscores <- renderPlotly({
      
      PlotScoresClass(data[order.dendrogram(dend),],input$npc1scoresplotHCA, input$npc2scoresplotHCA, input$siglimHCA, HCAclass, rownames(data[order.dendrogram(dend),]), SpecialLabel = "Variable ")
    })
    
    
    #-------------------------------------------3D Scatter
    
    output$HCA3dscores <- renderPlotly({
      
      Plot3DscoresClasses(data[order.dendrogram(dend),], input$npc1scores3dplotHCA, input$npc2scores3dplotHCA, input$npc3scores3dplotHCA, input$siglim3DHCA, HCAclass, rownames(data[order.dendrogram(dend),]), SpecialLabel = "Cluster ", LabelABV = " Var ")
      
    })
    
    #--------------------------Heatmap
    output$HCAheatmap <- renderPlot({
      
      req(ncol(data())<200)
      
      pheatmap(data)
      
      #heatmap(data.matrix(data))
      
      # #crpfun<-colorRampPalette(c("white","grey", "black"))
      # #crp<-crpfun(100)
      # gplots::heatmap.2(data.matrix(data),
      #                   main = "Heatmap with Dendogram",
      #                   srtCol = 20,
      #                   #dendrogram = "both",
      #                   Rowv = dend,
      #                   #Colv = "NA", # this to make sure the columns are not ordered
      #                   trace="none",
      #                   #col = crp,
      #                   margins =c(5,0.1),
      #                   #key.xlab = "Cm",
      #                   #denscol = "red",
      #                   density.info = "none",
      #                   #RowSideColors = get_leaves_branches_col(dend), # to add nice colored strips
      #                   )
    },
    
    height = 1300
    )
    
    #---------Cor Heatmap
    # tabPanel("Correlation Heatmap", h3(""),
    #          plotOutput("HCAcorheatmap")),
    
    output$HCAcorheatmap <- renderPlot({
      req(ncol(data())<200)
      pheatmap(cor(data()))
    })
    
  })
  #----------------------------------------------------------------------------------------------PCA---------------------------------------------------------------------------------
  
  observeEvent(input$preview,{
    if(isTRUE(ncol(data())<10)==T)
    {updateNumericInput(inputId = "ncomppca", value = ncol(data()))}
    
    updateNumericInput(inputId = "ncomppca", max = ncol(data()))
    updateSelectizeInput(inputId = "pcaremovecol", choices = list(as.character(t(variables()))), server = T)
  })
  
  
  observeEvent(input$bPCA, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    validate(need(input$ncomppca!="", message = "Need to select de number of components")) #checar input ncomp
    
    
    if(input$removepcacolask==T)
    {pcaremovecolvar<-eventReactive(input$pcaremovecol,{input$pcaremovecol})
    pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
    
    if(input$removepcacolask==F)
    {pcadata1<-reactive(data())}
    
    
    matrix<-pcadata1()
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-pcadata1()[,matrix2]
    
    
    if(input$prepropca == "none")
    {PCA <- mdatools::pca(pcadata, method = input$methodpca, lim.type = "ddmoments", center = F, scale = F, ncomp = input$ncomppca)}
    
    if (input$prepropca == "center")
    {PCA <- mdatools::pca(pcadata, method = input$methodpca, lim.type = "ddmoments", center = T, scale = F, ncomp = input$ncomppca)}
    
    if (input$prepropca == "scale")
    {PCA <- mdatools::pca(pcadata, scale=T, method = input$methodpca, lim.type = "ddmoments", ncomp = input$ncomppca)}
    
    numvar(colnames(pcadata))
    PCA(PCA)
    
    removeModal()
    
  })
  
  observeEvent(c(input$importpcamodel,input$bPCA, input$classcorespca), ignoreInit = T ,{
    req()
    
    #Matrices list dowload tab
    {
      
      updateNumericInput(inputId = "numPCresi", value = PCA()$ncomp)
      updateNumericInput(inputId = "numPCresi", max = PCA()$ncomp)} 
    
    output$SumPCA <- renderPrint(summary(PCA()), width = 500)
    
    #output$preview2 <- renderDT(PCA()data[,1:2])
    
    ##--------------------------Plots--------------------------------------------------------------
    { 
      
      #-----------------------------------------Variancia
      
      #output$pcav <- renderPlot(plotVariance(PCA(),show.labels=T),res = 96, height = 800)
      output$pcav<- renderPlotly({
        
        variance <- PCA()$calres$expvar
        
        t <- list(
          family = "times",
          size = 12,
          color = toRGB("black"))
        
        plot_ly(type="scatter",
                x=1:length(variance),
                y=variance,
                mode="lines+markers",
                showlegend=F,
                text=paste("Component ", 1:length(variance), sep=""),
                hoverinfo="text y")%>%
          layout(title="<b>Variance</b>",
                 xaxis=list(title=paste0("Component"),zerolinecolor="black"),
                 yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
        
      })
      
      #----------------------------------------Variancia acumulada
      
      #output$pcacv <- renderPlot(plotCumVariance(PCA(),show.labels=T),res = 96, height = 800)
      output$pcacv<- renderPlotly({
        
        variance <- PCA()$calres$cumexpvar
        
        t <- list(
          family = "times",
          size = 12,
          color = toRGB("black"))
        
        plot_ly(type="scatter",
                x=1:length(variance),
                y=variance,
                mode="lines+markers",
                showlegend=F,
                text=paste("Component ", 1:length(variance), sep=""),
                hoverinfo="text y")%>%
          layout(title="<b>Variance</b>",
                 xaxis=list(title=paste0("Component"),zerolinecolor="black"),
                 yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
        
      })
      
      #---------------------------------------Loadings
      
      #output$pcaload <- renderPlot(plotLoadings(PCA(),show.labels=T),res = 96, height = 800)
      output$pcaload<-renderPlotly({
        
        loadings<-PCA()$loadings
        sampleclass<-sampleclass()
        id<-id()
        variables<-variables()
        data2<-numvar()
        
        if(input$isspectra == F){
          plot<-PlotNormalLoadings(loadings, input$npc1loadplotPCA, input$npc2loadplotPCA, variables, data2, "Principal Component ")
        }
        
        if(input$isspectra == T){
          plot<-PlotSpectralLoadings(loadings, input$npc1loadplotPCA, input$npc2loadplotPCA, variables, data2, "Principal Component ")
        }
        
        plot
        
      })
      
      #-------------------------------------Loadings individuais por PC
      
      #output$pcaload1 <- renderPlot(plotLoadings(PCA(),1:input$numnpcpca,type = "l"), res = 96, height = 800)
      output$pcaload1 <- renderPlotly({
        
        loadings<-PCA()$loadings
        sampleclass<-sampleclass()
        id<-id()
        variables<-variables()
        data2<-numvar()
        
        
        if(input$isspectra == F){
          plot<-PlotNormalNLoadings(loadings, variables, data2, "Principal Component ")
        }
        
        if(input$isspectra == T){
          plot<-PlotSpectralNLoadings(loadings, variables, data2, "Principal Component ")
        }
        
        plot
        
      })
      
      #---------------------------------------Scores
      
      if(input$classcorespca==F){
        output$pcascores <- renderPlotly({
          scores<-PCA()$calres$scores
          sampleclass<-sampleclass()
          id<-id()
          
          PlotScoresGeneral(scores,input$npc1scoresplotpca, input$npc2scoresplotpca, input$siglimpca, sampleclass, id, "Principal Component ", " PC")
        })
      }
      
      if(input$classcorespca==T){
        output$pcascores <- renderPlotly({
          scores<-PCA()$calres$scores
          sampleclass<-sampleclass()
          id<-id()
          
          PlotScoresClass(scores,input$npc1scoresplotpca, input$npc2scoresplotpca, input$siglimpca, sampleclass, id, "Principal Component ", " PC")
        })
      }
      
      #---------------------------------------Scores 3d  
      
      if(input$classcorespca==F){
        output$pca3dscores<- renderPlotly({
          
          scores<-PCA()$calres$scores
          sampleclass<-sampleclass()
          id<-id()
          
          Plot3DscoresGeneral(scores,input$npc1scores3dplotpca, input$npc2scores3dplotpca, input$npc3scores3dplotpca, input$siglim3Dpca, sampleclass, id, "Principal Component ", " PC")
        })
      }
      
      
      if(input$classcorespca==T){
        output$pca3dscores<- renderPlotly({
          
          scores<-PCA()$calres$scores
          sampleclass<-sampleclass()
          id<-id()
          
          Plot3DscoresClasses(scores,input$npc1scores3dplotpca, input$npc2scores3dplotpca, input$npc3scores3dplotpca, input$siglimpca, sampleclass, id, "Principal Component ", " PC")
        })
      }
      
      
      ##Residuals
      output$pcaresiduals <- renderPlot(plotResiduals(PCA(),show.labels=T, ncomp = input$numPCresipca, log = input$logresidualspca, lim.col = c("orange", "red"), lim.lwd = c(3,3),lim.lty = c(2, 2)),res = 96, height = 800)
      output$pcaLimleg <- renderText("Info,  Outside of red = Outlier & Outside of orange = extreme value")
      
      ##BiPlot
      output$biplotpca <- renderPlotly({
        
        scores<-PCA()$calres$scores
        id<-id()
        loadings<-PCA()$loadings
        variables<-variables()
        
        PlotBIPLOT(scores, loadings,input$npc1biplotpca, input$npc2biplotpca, id, variables, "Principal Component ", " PC")
        
        
        
      })
      
      #-----Outliers
      output$OutPCA <- renderPlot({
        
        if(input$removeROBPCAcolask==T)
        {pcaremovecolvar<-eventReactive(input$ROBPCAremovecol,{input$ROBPCAremovecol})
        pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
        
        if(input$removeROBPCAcolask==F)
        {pcadata1<-reactive(data())}
        
        
        if(input$prepropca == "none"){
          matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
        
        if(input$prepropca == "center"){
          matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
        }
        
        if(input$prepropca == "scale"){
          matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
        }
        
        
        matrix2<-which(sapply(matrix, is.numeric))
        pcadata<-matrix[,matrix2]
        
        #plot(pca.distances(ROBPCA(), pcadata, rankMM(pcadata)))
        plot(pca.distances(rrcov::PcaClassic(pcadata, k = input$numPCoutipca), pcadata, rankMM(pcadata)))
        
      })
      
      
    }
    
    
    output$pcamatrices<-renderPrint("Run PCA to view")
    
    
    
    
  })
  
  #-------------------------Download Handlers-----------------------------------------------------
  
  {namefilespca <- reactive(input$downloadfilenamepca)
  
  output$downloadloadpca <- downloadHandler(
    filename = function() {
      paste0(input$namemodelpca,"_loadingsPCA", ".csv")
    },
    content = function(file) {
      write.csv(PCA()$loadings[,1:input$numcompselpca], file)
    }
  )
  
  output$downloadscorespca <- downloadHandler(
    filename = function() {
      paste0(input$namemodelpca,"_scoresPCA", ".csv")
    },
    content = function(file) {
      write.csv(PCA()$res$cal$scores[,1:input$numcompselpca], file)
    }
  )
  
  }
  
  ##Export model
  output$savepcamodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelpca, " PCA model", ".Rdata")
    },
    content = function(file){
      PCA1<-PCA()
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data<-data.frame(PCA()$loadings[,1:input$numcompselpca])
      variables<-data.frame(colnames(data))
      PCAmodel<-selectCompNum(PCA1, input$numcompselpca)
      
      save(data,PCAmodel,sampleclass,id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importpcamodel, {validate(need(grepl(".Rdata", input$searchmodelpca$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelpca$datapath)
    PCA(PCAmodel)
    sampleclass(sampleclass)
  })
  
  #-----------------------------------------------------------------------------------Robust PCA
  
  observeEvent(input$preview,{
    updateSelectizeInput(inputId = "ROBPCAremovecol", choices = list(as.character(t(variables()))), server = T)
  })
  
  
  observeEvent(input$bROBPCA, {
    
    #selectInput("preproROBPCA","Preprocessing method:", c("None" = "none","Center" = "center","Scale" = "scale"), selected = "scale"),
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    
    if(input$removeROBPCAcolask==T)
    {pcaremovecolvar<-eventReactive(input$ROBPCAremovecol,{input$ROBPCAremovecol})
    pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
    
    if(input$removeROBPCAcolask==F)
    {pcadata1<-reactive(data())}
    
    
    if(input$prepropca == "none"){
      matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
    
    if(input$prepropca == "center"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
    }
    
    if(input$prepropca == "scale"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
    }
    
    
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-matrix[,matrix2]
    
    
    ROBPCA <- PcaHubert(pcadata, k = input$ncompROBPCA)
    
    numvar(colnames(pcadata))
    ROBPCA(ROBPCA)
    
    removeModal()
    
  })
  
  
  observeEvent(c(input$importROBPCAmodel,input$bROBPCA, input$classcoresROBPCA), ignoreInit = T ,{req()
    
    output$SumROBPCA <- renderPrint(summary(ROBPCA()))
    
    #----------------------------Variance
    output$ROBPCAv <- renderPlotly({
      
      variance<-summary(ROBPCA())@importance[2,]
      
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:length(variance),
              y=variance,
              mode="lines+markers",
              showlegend=F,
              text=paste("Component ", 1:length(variance), sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Variance</b>",
               xaxis=list(title=paste0("Component"),zerolinecolor="black"),
               yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
    })
    
    
    #----------------------------Cumulative Variance
    
    output$ROBPCAcv <- renderPlotly({
      
      variance<-summary(ROBPCA())@importance[3,]
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:length(variance),
              y=variance,
              mode="lines+markers",
              showlegend=F,
              text=paste("Component ", 1:length(variance), sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Variance</b>",
               xaxis=list(title=paste0("Component"),zerolinecolor="black"),
               yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
    })
    
    
    #------------------------Loadings
    
    output$ROBPCAload<-renderPlotly({
      
      loadings<-ROBPCA()$loadings
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      if(input$isspectra == F){
        plot<-PlotNormalLoadings(loadings, input$npc1loadplotROBPCA, input$npc2loadplotROBPCA, variables, data2, "Principal Component ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralLoadings(loadings, input$npc1loadplotROBPCA, input$npc2loadplotROBPCA, variables, data2, "Principal Component ")
      }
      
      plot
      
    })
    
    #------------------------nLoadings
    
    output$ROBPCAload1<-renderPlotly({
      
      #PlotSpectralNLoadings<-function(loadings, variables, data2, SpecialLabels){
      
      loadings<-ROBPCA()$loadings
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      
      if(input$isspectra == F){
        plot<-PlotNormalNLoadings(loadings, variables, data2, "Principal Component ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralNLoadings(loadings, variables, data2, "Principal Component ")
      }
      
      plot
      
    })
    
    #------------------------Scores
    
    if(input$classcoresROBPCA==T){
      output$ROBPCAscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        PlotScoresClass(scores,input$npc1scoresplotROBPCA, input$npc2scoresplotROBPCA, input$siglimROBPCA, sampleclass, id, "Principal Component ", " PC")
      })
      
    }
    
    
    if(input$classcoresROBPCA==F){
      output$ROBPCAscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        PlotScoresGeneral(scores,input$npc1scoresplotROBPCA, input$npc2scoresplotROBPCA, input$siglimROBPCA, sampleclass, id, "Principal Component ", " PC")
        
      })
    }
    
    
    #-------------------------------------------3D scores
    
    
    if(input$classcoresROBPCA==T){
      output$ROBPCA3dscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotROBPCA, input$npc2scores3dplotROBPCA, input$npc3scores3dplotROBPCA, input$siglim3DROBPCA, sampleclass, id, "Principal Component ", " PC")
        
      })
    }
    
    
    if(input$classcoresROBPCA==F){
      output$ROBPCA3dscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotROBPCA, input$npc2scores3dplotROBPCA, input$npc3scores3dplotROBPCA, input$siglim3DROBPCA, sampleclass, id, "Principal Component ", " PC")
        
      }
      )}
    
    #-----------------------------------Biplot
    
    output$ROBPCAbiplot<-renderPlotly({
      scores<-ROBPCA()$scores
      id<-id()
      loadings<-ROBPCA()$loadings
      variables<-variables()
      
      PlotBIPLOT(scores, loadings,input$npc1biplotROBPCA, input$npc2biplotROBPCA, id, variables, "Principal Component ", " PC")
      
      
    })
    
    #-----------------------------------Outliers
    
    #tabPanel("Outliers",plotOutput("OutROBPCA", height = 650)),
    
    output$OutROBPCA <- renderPlot({
      
      if(input$removeROBPCAcolask==T)
      {pcaremovecolvar<-eventReactive(input$ROBPCAremovecol,{input$ROBPCAremovecol})
      pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
      
      if(input$removeROBPCAcolask==F)
      {pcadata1<-reactive(data())}
      
      
      if(input$prepropca == "none"){
        matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
      
      if(input$prepropca == "center"){
        matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
      }
      
      if(input$prepropca == "scale"){
        matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
      }
      
      
      matrix2<-which(sapply(matrix, is.numeric))
      pcadata<-matrix[,matrix2]
      
      plot(pca.distances(rrcov::PcaHubert(pcadata, k = input$numROBPCoutipca), pcadata, rankMM(pcadata)))
      
    })
   #numericInput("numROBPCoutipca","Principal Component Number",min = 1,value = 1), 
    
  })
  
  
  
  ##Export model
  output$saveROBPCAmodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelROBPCA, " RobPCA model", ".Rdata")
    },
    content = function(file){
      FA1<-ROBPCA()
      sampleclass<-sampleclass()
      ROBPCAmodel<-FA1
      id<-id()
      variables<-variables()
      scores<-ROBPCA()$scores[,1:input$numcompselROBPCA]
      data<-data.frame(scores)
      variables<-data.frame(colnames(data))
      save(data, ROBPCAmodel, sampleclass, id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importROBPCAmodel, {validate(need(grepl(".Rdata", input$searchmodelROBPCA$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelROBPCA$datapath)
    ROBPCA(ROBPCAmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  #------export csv
  {namefileROBPCA <- reactive(input$namemodelROBPCA)
    
    output$downloadloadROBPCA <- downloadHandler(
      filename = function() {
        paste0(namefileROBPCA(),"_loadingsRobPCA", ".csv")
      },
      content = function(file) {
        load<-ROBPCA()$loadings[,1:input$numcompselFA]
        rownames(load)<-t(variables()[which(sapply(data(), is.numeric)),])
        write.csv(load, file)
      }
    )
    
    output$downloadscoresROBPCA <- downloadHandler(
      filename = function() {
        paste0(namefileROBPCA(),"_scoresRobPCA", ".csv")
      },
      content = function(file) {
        scores<-ROBPCA()$scores[,1:input$numcompselROBPCA]
        write.csv(scores, file)
      }
    )
  }
  #-----------------------------------------------------------------------------------FACTOR ANALYSIS 
  observeEvent(input$preview,{
    updateSelectizeInput(inputId = "FAremovecol", choices = list(as.character(t(variables()))), server = T)
  })
  
  
  observeEvent(input$bFA, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    
    if(input$removeFAcolask==T)
    {pcaremovecolvar<-eventReactive(input$FAremovecol,{input$FAremovecol})
    pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
    
    if(input$removeFAcolask==F)
    {pcadata1<-reactive(data())}
    
    if(input$prepropca == "none"){
      matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
    
    if(input$prepropca == "center"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
    }
    
    if(input$prepropca == "scale"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
    }
    
    
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-matrix[,matrix2]
    numvar(colnames(pcadata))
    
    
    FA <- fa(pcadata, nfactors = input$ncompFA)
    
    
    FA(FA)
    
    removeModal()
    
  })
  
  
  observeEvent(c(input$importFAmodel,input$bFA, input$classcoresFA), ignoreInit = T ,{req()
    
    output$SumFA <- renderPrint(summary(FA()))
    
    #----------------------------Variance
    output$FAv <- renderPlotly({
      
      
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:input$ncompFA,
              y=FA()$Vaccounted[4,],
              mode="lines+markers",
              showlegend=F,
              text=paste("Factor ", 1:input$ncompFA, sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Variance</b>",
               xaxis=list(title=paste0("Factor"),zerolinecolor="black"),
               yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
    })
    
    
    #----------------------------Cumulative Variance
    
    output$FAcv <- renderPlotly({
      
      
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:input$ncompFA,
              y=FA()$Vaccounted[3,],
              mode="lines+markers",
              showlegend=F,
              text=paste("Factor ", 1:input$ncompFA, sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Cumulative Variance</b>",
               xaxis=list(title=paste0("Factor"),zerolinecolor="black"),
               yaxis=list(title=paste0("Cumulative Variance"),zerolinecolor="black"))
    })
    
    
    #------------------------Loadings
    
    output$FAload<-renderPlotly({
      
      loadings<-FA()$loadings
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      if(input$isspectra == F){
        plot<-PlotNormalLoadings(loadings, input$npc1loadplotFA, input$npc2loadplotFA, variables, data2, "Factor ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralLoadings(loadings, input$npc1loadplotFA, input$npc2loadplotFA, variables, data2, "Factor ")
      }
      
      plot
      
    })
    
    
    #------------------------nLoadings
    
    output$FAload1<-renderPlotly({
      
      loadings<-FA()$loadings
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      
      if(input$isspectra == F){
        plot<-PlotNormalNLoadings(loadings, variables, data2, "Factor ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralNLoadings(loadings, variables, data2, "Factor ")
      }
      
      plot
      
    })
    
    #------------------------Scores
    
    if(input$classcoresFA==T)
    {output$FAscores <- renderPlotly({
      scores<-FA()$scores
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresClass(scores,input$npc1scoresplotFA, input$npc2scoresplotFA, input$siglimFA, sampleclass, id, "Factor ", " FA" )
    })
    }
    
    
    if(input$classcoresFA==F)
    {output$FAscores <- renderPlotly({
      
      scores<-FA()$scores
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresGeneral(scores,input$npc1scoresplotFA, input$npc2scoresplotFA, input$siglimFA, sampleclass, id, "Factor ", " FA" )
    })
    
    }
    
    
    #-------------------------------------------3D scores
    
    
    
    if(input$classcoresFA==T){
      output$FA3dscores <- renderPlotly({
        scores<-FA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotFA, input$npc2scores3dplotFA, input$npc3scores3dplotFA, input$siglim3DFA, sampleclass, id, "Factor ", " FA" )
        
      })
    }
    
    
    if(input$classcoresFA==F){
      output$FA3dscores <- renderPlotly({
        scores<-FA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotFA, input$npc2scores3dplotFA, input$npc3scores3dplotFA, input$siglim3DFA, sampleclass, id, "Factor ", " FA" )
        
      })
    }
    
    #-----------------------------------Biplot
    
    output$FAbiplot<-renderPlotly({
      scores<-FA()$scores
      id<-id()
      loadings<-FA()$loadings
      variables<-variables()
      
      PlotBIPLOT(scores, loadings,input$npc1biplotFA, input$npc2biplotFA, id, variables, "Factor ", " FA" )
      
      
    })
    
    
    
  })
  
  
  ##Export model
  output$saveFAmodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelFA, " FA model", ".Rdata")
    },
    content = function(file){
      FA1<-FA()
      sampleclass<-sampleclass()
      FAmodel<-FA1
      id<-id()
      variables<-variables()
      scores<-FA()$scores[,1:input$numcompselFA]
      colnames(scores)<-paste0("FA", 1:input$numcompselFA)
      data<-data.frame(scores)
      variables<-data.frame(colnames(data))
      save(data, FAmodel, sampleclass, id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importFAmodel, {validate(need(grepl(".Rdata", input$searchmodelFA$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelFA$datapath)
    FA(FAmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  #------export csv
  {namefileFA <- reactive(input$namemodelFA)
    
    output$downloadloadFA <- downloadHandler(
      filename = function() {
        paste0(namefileFA(),"_loadingsFA", ".csv")
      },
      content = function(file) {
        load<-FA()$loadings[,1:input$numcompselFA]
        rownames(load)<-t(variables()[which(sapply(data(), is.numeric)),])
        colnames(load)<-paste0("FA", 1:input$numcompselFA)
        write.csv(load, file)
      }
    )
    
    output$downloadscoresFA <- downloadHandler(
      filename = function() {
        paste0(namefileFA(),"_scoresFA", ".csv")
      },
      content = function(file) {
        scores<-FA()$scores[,1:input$numcompselFA]
        colnames(scores)<-paste0("FA", 1:input$numcompselFA)
        write.csv(scores, file)
      }
    )
  }
  
  
  #-----------------------------------------------------------------------------------ICA------------------------------------------------------------
  
  observeEvent(input$bICA, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    
    if(input$removeICAcolask==T)
    {pcaremovecolvar<-eventReactive(input$ICAremovecol,{input$ICAremovecol})
    pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
    
    if(input$removeICAcolask==F)
    {pcadata1<-reactive(data())}
    
    
    
    if(input$prepropca == "none"){
      matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
    
    if(input$prepropca == "center"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
    }
    
    if(input$prepropca == "scale"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
    }
    
    
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-matrix[,matrix2]
    
    if(input$icamethod == "fastica")
    {ICA <- icajade(pcadata, nc=input$ncompICA, center = F)}
    
    if (input$icamethod == "jadeica")
    {ICA <- icafast(pcadata, nc=input$ncompICA, center = F)}
    
    
    
    numvar(colnames(pcadata))
    
    ICA(ICA)
    
    removeModal()
    
  })
  
  
  
  observeEvent(c(input$importICAmodel,input$bICA, input$classcoresICA), ignoreInit = T ,{req()
    
    
    
    
    
    #------------------------------------VARIANCE
    
    output$ICAv <- renderPlotly({
      
      
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:length(ICA()$vafs),
              y=ICA()$vafs,
              mode="lines+markers",
              showlegend=F,
              text=paste("Component ", 1:length(ICA()$vafs), sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Variance</b>",
               xaxis=list(title=paste0("Component"),zerolinecolor="black"),
               yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
    })
    
    
    #------------------------------------CUM VARIANCE
    output$ICAcv <- renderPlotly({
      
      
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:length(ICA()$vafs),
              y=cumsum(ICA()$vafs),
              mode="lines+markers",
              showlegend=F,
              text=paste("Component ", 1:length(ICA()$vafs), sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Cumulative Variance</b>",
               xaxis=list(title=paste0("Component"),zerolinecolor="black"),
               yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
      
    })
    #------------------------------------LOADINGS
    
    output$ICAload<-renderPlotly({
      
      loadings<-ICA()$M
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      if(input$isspectra == F){
        plot<-PlotNormalLoadings(loadings, input$npc1loadplotICA, input$npc2loadplotICA, variables, data2, "Component ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralLoadings(loadings, input$npc1loadplotICA, input$npc2loadplotICA, variables, data2, "Component ")
      }
      
      plot
      
      
    })
    
    #------------------------nLoadings
    
    output$ICAload1<-renderPlotly({
      
      loadings<-ICA()$M
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      
      if(input$isspectra == F){
        plot<-PlotNormalNLoadings(loadings, variables, data2, "Principal Component ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralNLoadings(loadings, variables, data2, "Principal Component ")
      }
      
      plot
    })
    
    
    #-------------------------------------SCORES
    
    if(input$classcoresICA==T)
    {output$ICAscores <- renderPlotly({
      
      scores<-ICA()$S
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresClass(scores, input$npc1scoresplotICA, input$npc2scoresplotICA, input$siglimICA, sampleclass, id, "Component ", " IC")
    })
    
    }
    
    
    
    if(input$classcoresICA==F)
      
    {output$ICAscores <- renderPlotly({
      
      scores<-ICA()$S
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresGeneral(scores, input$npc1scoresplotICA, input$npc2scoresplotICA, input$siglimICA, sampleclass, id, "Component ", " IC")
      
    })
    
    
    }
    
    
    #---------3D scores
    
    if(input$classcoresICA==T){
      output$ICA3dscores <- renderPlotly({
        scores<-ICA()$S
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotICA, input$npc2scores3dplotICA, input$npc3scores3dplotICA, input$siglim3DICA, sampleclass, id, "Component ", " IC")
        
      })
    }
    
    
    if(input$classcoresICA==F){
      output$ICA3dscores <- renderPlotly({
        scores<-ICA()$S
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotICA, input$npc2scores3dplotICA, input$npc3scores3dplotICA, input$siglim3DICA, sampleclass, id, "Component ", " IC")
        
      })
    }
    
    #-----------------------------------Biplot
    
    output$ICAbiplot<-renderPlotly({
      scores<-ICA()$S
      id<-id()
      loadings<-ICA()$M
      variables<-variables()
      
      PlotBIPLOT(scores, loadings,input$npc1biplotICA, input$npc2biplotICA, id, variables, "Component ", " IC")
      
      
    })
    
    
    
  }) 
  
  ##Export model
  output$saveICAmodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelICA, " ICA model", ".Rdata")
    },
    content = function(file){
      FA1<-ICA()
      sampleclass<-sampleclass()
      ICAmodel<-FA1
      id<-id()
      variables<-variables()
      scores<-data.frame(ICA()$S[,1:input$numcompselICA])
      data<-scores
      variables<-data.frame(colnames(data))
      save(data, ICAmodel, sampleclass, id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importICAmodel, {validate(need(grepl(".Rdata", input$searchmodelICA$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelICA$datapath)
    ICA(ICAmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  #--------export csv
  
  {namefileICA <- reactive(input$namemodelICA)
    
    output$downloadloadICA <- downloadHandler(
      filename = function() {
        paste0(namefileICA(),"_loadingsICA", ".csv")
      },
      content = function(file) {
        load<-ICA()$M[,1:input$numcompselICA]
        rownames(load)<-t(variables()[which(sapply(data(), is.numeric)),])
        write.csv(load, file)
      }
    )
    
    output$downloadscoresICA <- downloadHandler(
      filename = function() {
        paste0(namefileICA(),"_scoresICA", ".csv")
      },
      content = function(file) {
        scores<-ICA()$S[,1:input$numcompselICA]
        write.csv(scores, file)
      }
    )
  }
  
  #------------------------------------------------------------------------------------------------TSNE
  
  
  observeEvent(input$preview,{
    updateSelectizeInput(inputId = "FAremovecol", choices = list(as.character(t(variables()))), server = T)
  })
  
  
  observeEvent(input$bTSNE, { 
    
    validate(need(3*input$perplexityTSNE<nrow(data()[,which(sapply(data(), is.numeric))]-1), message = "Perplexity is too high"))
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    
    
    
    if(input$removeTSNEcolask==T)
    {pcaremovecolvar<-eventReactive(input$FAremovecol,{input$FAremovecol})
    pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
    
    if(input$removeTSNEcolask==F)
    {pcadata1<-reactive(data())}
    
    
    if(input$prepropca == "none"){
      matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
    
    if(input$prepropca == "center"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
    }
    
    if(input$prepropca == "scale"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
    }
    
    
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-matrix[,matrix2]
    
    TSNE <- Rtsne(pcadata, dims=input$ncompTSNE, perplexity=input$perplexityTSNE, max_iter=input$niterTSNE, theta=input$thetaTSNE)
    
    
    TSNE(TSNE)
    
    removeModal()
    
  })
  
  #---------------------------------------PLOTS
  
  observeEvent(c(input$importTSNEmodel,input$bTSNE, input$classcoresTSNE), ignoreInit = T ,{req()
    
    
    #---------------Novos valores
    
    output$SumTSNE<-renderDT({table<-data.frame(TSNE()$Y)
    rownames(table)<-t(id())
    table
    }, options = list(pageLength = 7))
    
    
    #---------------Projection
    
    if(input$classcoresTSNE==T)
    {output$TSNEscores <- renderPlotly({
      scores<-data.frame(TSNE()$Y)
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresClass(scores,input$npc1scoresplotTSNE, input$npc2scoresplotTSNE, input$siglimTSNE, sampleclass, id, "Dimension", " DIM")
    })
    }
    
    
    if(input$classcoresTSNE==F)
    {output$TSNEscores <- renderPlotly({
      
      scores<-data.frame(TSNE()$Y)
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresGeneral(scores,input$npc1scoresplotTSNE, input$npc2scoresplotTSNE, input$siglimTSNE, sampleclass, id, "Dimension", " DIM")
    })
    
    }
    
    
    #--------------3D scores
    
    if(input$classcoresTSNE==T){
      output$TSNE3dscores <- renderPlotly({
        scores<-data.frame(TSNE()$Y)
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotTSNE, input$npc2scores3dplotTSNE, input$npc3scores3dplotTSNE, input$siglim3DTSNE, sampleclass, id, "Dimension", " DIM")
        
      })
    }
    
    
    if(input$classcoresTSNE==F){
      output$TSNE3dscores <- renderPlotly({
        scores<-data.frame(TSNE()$Y)
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotTSNE, input$npc2scores3dplotTSNE, input$npc3scores3dplotTSNE, input$siglim3DTSNE, sampleclass, id, "Dimension", " DIM")
        
      })
    }
    
    
    
    
    
    
  })
  
  ##-------------------Export model
  output$saveTSNEmodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelTSNE, " TSNE model", ".Rdata")
    },
    content = function(file){
      FA1<-TSNE()
      sampleclass<-sampleclass()
      TSNEmodel<-FA1
      id<-id()
      variables<-variables()
      scores<-data.frame(TSNE()$Y)
      rownames(scores)<-t(id())
      data<-scores
      variables<-data.frame(colnames(data))
      save(data, TSNEmodel, sampleclass, id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importTSNEmodel, {validate(need(grepl(".Rdata", input$searchmodelTSNE$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelTSNE$datapath)
    TSNE(TSNEmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  #--------export csv
  
  {namefileTSNE <- reactive(input$namemodelTSNE)
    
    # output$downloadloadTSNE <- downloadHandler(
    #   filename = function() {
    #     paste0(namefileTSNE(),"_loadingsTSNE", ".csv")
    #   },
    #   content = function(file) {
    #     load<-TSNE()$Y
    #     rownames(load)<-t(variables()[which(sapply(data(), is.numeric)),])
    #     write.csv(load, file)
    #   }
    # )
    
    output$downloadscoresTSNE <- downloadHandler(
      filename = function() {
        paste0(namefileTSNE(),"_projectionTSNE", ".csv")
      },
      content = function(file) {
        scores<-data.frame(TSNE()$Y)
        rownames(scores)<-t(id())
        write.csv(scores, file)
      }
    )
  }
  
  
  #-------------------------------------------------------------------------MDS------------------------------------------------------------------------------------------
  
  
  
  observeEvent(input$bMDS, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    pcadata1 <- reactive(data())
    
    if(input$prepropca == "none"){
      matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
    
    if(input$prepropca == "center"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
    }
    
    if(input$prepropca == "scale"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
    }
    
    
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-matrix[,matrix2]
    
    if(input$MDSdistmethod!="minkowski")
    {MDS <- data.frame(cmdscale(dist(pcadata, method = input$MDSdistmethod), k=input$ncompMDS))}
    
    else
    {MDS <- data.frame(cmdscale(dist(pcadata, method = input$MDSdistmethod, p = input$minkpMDS), k=input$ncompMDS))}
    
    MDS(MDS)
    
    removeModal()
    
  })
  
  
  observeEvent(c(input$importMDSmodel,input$bMDS, input$classcoresMDS), ignoreInit = T ,{req()
    
    output$SumMDS<-renderPrint(summary(MDS()))
    
    if(input$classcoresMDS==T)
    {output$MDSscores <- renderPlotly({
      scores<-data.frame(MDS())
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresClass(scores,input$npc1scoresplotMDS, input$npc2scoresplotMDS, input$siglimMDS, sampleclass, id, "Dimension", " DIM")
    })
    }
    
    
    if(input$classcoresMDS==F)
    {output$MDSscores <- renderPlotly({
      
      scores<-data.frame(MDS())
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresGeneral(scores,input$npc1scoresplotMDS, input$npc2scoresplotMDS, input$siglimMDS, sampleclass, id, "Dimension", " DIM")
    })
    
    }
    
    
    #--------------3D scores
    
    if(input$classcoresMDS==T){
      output$MDS3dscores <- renderPlotly({
        scores<-data.frame(MDS())
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotMDS, input$npc2scores3dplotMDS, input$npc3scores3dplotMDS, input$siglim3DMDS, sampleclass, id, "Dimension", " DIM")
        
      })
    }
    
    
    if(input$classcoresMDS==F){
      output$MDS3dscores <- renderPlotly({
        scores<-data.frame(MDS())
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotMDS, input$npc2scores3dplotMDS, input$npc3scores3dplotMDS, input$siglim3DMDS, sampleclass, id, "Dimension", " DIM")
        
      })
    }
    
    # if(input$classcoresMDS==T)
    # {output$MDSscores <- renderPlotly({
    #   
    #   scores<-MDS()
    #   
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   
    #   
    #   
    #   {plot_ly(type="scatter", 
    #            x=scores[,input$npc1scoresplotMDS],
    #            y=scores[,input$npc2scoresplotMDS],
    #            mode="markers",
    #            showlegend=T,
    #            color = factor(sampleclass()),
    #            colors = "Set1",
    #            text=as.character(t(id())),
    #            hoverinfo="skip")%>%
    #       add_text(textposition="top", textfont=t, showlegend=FALSE)%>%
    #       layout(title="<b>Scores</b>",
    #              xaxis=list(title=paste0("Dimension ",input$npc1scoresplotMDS),zerolinecolor="lightgrey"),
    #              yaxis=list(title=paste0("Dimension ",input$npc2scoresplotMDS),zerolinecolor="lightgrey"))}
    # })}
    # 
    # 
    # if(input$classcoresMDS==F)
    # {output$MDSscores <- renderPlotly({
    #   
    #   scores<-MDS()
    #   
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   
    #   
    #   
    #   {plot_ly(type="scatter", 
    #            x=scores[,input$npc1scoresplotMDS],
    #            y=scores[,input$npc2scoresplotMDS],
    #            mode="markers",
    #            showlegend=F,
    #            text=as.character(t(id())),
    #            hoverinfo="skip")%>%
    #       add_text(textposition="top", textfont=t)%>%
    #       layout(title="<b>Scores</b>",
    #              xaxis=list(title=paste0("Dimension ",input$npc1scoresplotMDS),zerolinecolor="lightgrey"),
    #              yaxis=list(title=paste0("Dimension ",input$npc2scoresplotMDS),zerolinecolor="lightgrey"))}
    # })}
    # 
    # 
    # #---------3D scores
    # if(input$classcoresMDS==T)
    # {output$MDS3dscores <- renderPlotly({
    #   
    #   scores<-MDS()
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   plot_ly(type="scatter3d", 
    #           x=scores[,input$npc1scores3dplotMDS],
    #           y=scores[,input$npc2scores3dplotMDS],
    #           z=scores[,input$npc3scores3dplotMDS],
    #           mode="markers",
    #           showlegend=T,
    #           color = factor(sampleclass()),
    #           colors = "Set1",
    #           text=as.character(t(id())),
    #           marker = list(symbol = 'circle', sizemode = 'diameter', size=5),
    #           hoverinfo="skip")%>%
    #     
    #     add_text(textposition="top", textfont=t, showlegend=FALSE)%>%
    #     
    #     layout(title="<b>Scores</b>", scene= list(
    #       xaxis=list(title=paste0("Dimension ",input$npc1scores3dplotMDS),zerolinecolor="lightgrey"),
    #       yaxis=list(title=paste0("Dimension ",input$npc2scores3dplotMDS),zerolinecolor="lightgrey"),
    #       zaxis=list(title=paste0("Dimension ",input$npc3scores3dplotMDS),zerolinecolor="lightgrey"))
    #     )
    # })}
    # 
    # 
    # if(input$classcoresMDS==F)
    # {output$MDS3dscores <- renderPlotly({
    #   
    #   scores<-MDS()
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   plot_ly(type="scatter3d", 
    #           x=scores[,input$npc1scores3dplotMDS],
    #           y=scores[,input$npc2scores3dplotMDS],
    #           z=scores[,input$npc3scores3dplotMDS],
    #           mode="markers",
    #           showlegend=F,
    #           text=as.character(t(id())),
    #           marker = list(symbol = 'circle', sizemode = 'diameter', size=5),
    #           hoverinfo="skip")%>%
    #     
    #     add_text(textposition="top", textfont=t, showlegend=FALSE)%>%
    #     
    #     layout(title="<b>Scores</b>", scene= list(
    #       xaxis=list(title=paste0("Dimension ",input$npc1scores3dplotMDS),zerolinecolor="lightgrey"),
    #       yaxis=list(title=paste0("Dimension ",input$npc2scores3dplotMDS),zerolinecolor="lightgrey"),
    #       zaxis=list(title=paste0("Dimension ",input$npc3scores3dplotMDS),zerolinecolor="lightgrey"))
    #     )
    # })}
    
    
    
    
  }) 
  
  
  ##Export model
  output$saveMDSmodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelMDS, " MDS model", ".Rdata")
    },
    content = function(file){
      FA1<-MDS()
      sampleclass<-sampleclass()
      MDSmodel<-FA1
      id<-id()
      variables<-variables()
      scores<-data.frame(MDS()[,1:input$numcompselMDS])
      rownames(scores)<-t(id())
      data<-scores
      variables<-data.frame(colnames(data))
      save(data, MDSmodel, sampleclass, id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importMDSmodel, {validate(need(grepl(".Rdata", input$searchmodelMDS$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelMDS$datapath)
    MDS(MDSmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  #--------export csv
  
  {namefileMDS <- reactive(input$namemodelMDS)
    
    output$downloadscoresMDS <- downloadHandler(
      filename = function() {
        paste0(namefileMDS(),"_projectionMDS", ".csv")
      },
      content = function(file) {
        scores<-data.frame(MDS()[,1:input$numcompselMDS])
        rownames(scores)<-t(id())
        write.csv(scores, file)
      }
    )
  }
  
  
  
  #---------------------------------------------------------------------ISOMAP
  
  
  
  observeEvent(input$bISOMAP, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    pcadata1<-reactive(data())
    
    if(input$prepropca == "none"){
      matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
    
    if(input$prepropca == "center"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
    }
    
    if(input$prepropca == "scale"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
    }
    
    
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-matrix[,matrix2]
    
    
    if(input$ISOMAPdistmethod!="minkowski")
    {ISOMAP <- isomap(dist(pcadata, method = input$ISOMAPdistmethod), ndim = input$ncompISOMAP, epsilon = input$epsilonISOMAP)}
    
    else
    {ISOMAP <- isomap(dist(pcadata, method = input$ISOMAPdistmethod, p = input$minkpISOMAP), ndim = input$ncompISOMAP, epsilon = input$epsilonISOMAP)}
    
    ISOMAP(ISOMAP)
    
    removeModal()
    
  })
  
  
  observeEvent(c(input$importISOMAPmodel,input$bISOMAP, input$classcoresISOMAP), ignoreInit = T ,{req()
    
    #output$SumISOMAP<-verbatimTextOutput(summary(ISOMAP()))
    
    if(input$classcoresISOMAP==T)
    {output$ISOMAPscores <- renderPlotly({
      scores<-data.frame(ISOMAP()$points)
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresClass(scores,input$npc1scoresplotISOMAP, input$npc2scoresplotISOMAP, input$siglimISOMAP, sampleclass, id, "Dimension ", " DIM")
    })
    }
    
    
    if(input$classcoresISOMAP==F)
    {output$ISOMAPscores <- renderPlotly({
      
      scores<-data.frame(ISOMAP()$points)
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresGeneral(scores,input$npc1scoresplotISOMAP, input$npc2scoresplotISOMAP, input$siglimISOMAP, sampleclass, id, "Dimension ", " DIM")
    })
    
    }
    
    
    #--------------3D scores
    
    if(input$classcoresISOMAP==T){
      output$ISOMAP3dscores <- renderPlotly({
        scores<-data.frame(ISOMAP()$points)
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotISOMAP, input$npc2scores3dplotISOMAP, input$npc3scores3dplotISOMAP, input$siglim3DISOMAP, sampleclass, id, "Dimension ", " DIM")
        
      })
    }
    
    
    if(input$classcoresISOMAP==F){
      output$ISOMAP3dscores <- renderPlotly({
        scores<-data.frame(ISOMAP()$points)
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotISOMAP, input$npc2scores3dplotISOMAP, input$npc3scores3dplotISOMAP, input$siglim3DISOMAP, sampleclass, id, "Dimension ", " DIM")
        
      })
    }
    
    # if(input$classcoresISOMAP==T)
    # {output$ISOMAPscores <- renderPlotly({
    #   
    #   scores<-ISOMAP()$points
    #   
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   
    #   
    #   
    #   {plot_ly(type="scatter", 
    #            x=scores[,input$npc1scoresplotISOMAP],
    #            y=scores[,input$npc2scoresplotISOMAP],
    #            mode="markers",
    #            showlegend=T,
    #            color = factor(sampleclass()),
    #            colors = "Set1",
    #            text=as.character(t(id())),
    #            hoverinfo="skip")%>%
    #       add_text(textposition="top", textfont=t, showlegend=FALSE)%>%
    #       layout(title="<b>Scores</b>",
    #              xaxis=list(title=paste0("Dimension ",input$npc1scoresplotISOMAP),zerolinecolor="lightgrey"),
    #              yaxis=list(title=paste0("Dimension ",input$npc2scoresplotISOMAP),zerolinecolor="lightgrey"))}
    # })}
    # 
    # 
    # if(input$classcoresISOMAP==F)
    # {output$ISOMAPscores <- renderPlotly({
    #   
    #   scores<-ISOMAP()$points
    #   
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   
    #   
    #   
    #   {plot_ly(type="scatter", 
    #            x=scores[,input$npc1scoresplotISOMAP],
    #            y=scores[,input$npc2scoresplotISOMAP],
    #            mode="markers",
    #            showlegend=F,
    #            text=as.character(t(id())),
    #            hoverinfo="skip")%>%
    #       add_text(textposition="top", textfont=t)%>%
    #       layout(title="<b>Scores</b>",
    #              xaxis=list(title=paste0("Dimension ",input$npc1scoresplotISOMAP),zerolinecolor="lightgrey"),
    #              yaxis=list(title=paste0("Dimension ",input$npc2scoresplotISOMAP),zerolinecolor="lightgrey"))}
    # })}
    # 
    # 
    # #---------3D scores
    # if(input$classcoresISOMAP==T)
    # {output$ISOMAP3dscores <- renderPlotly({
    #   
    #   scores<-ISOMAP()$points
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   plot_ly(type="scatter3d", 
    #           x=scores[,input$npc1scores3dplotISOMAP],
    #           y=scores[,input$npc2scores3dplotISOMAP],
    #           z=scores[,input$npc3scores3dplotISOMAP],
    #           mode="markers",
    #           showlegend=T,
    #           color = factor(sampleclass()),
    #           colors = "Set1",
    #           text=as.character(t(id())),
    #           marker = list(symbol = 'circle', sizemode = 'diameter', size=5),
    #           hoverinfo="skip")%>%
    #     
    #     add_text(textposition="top", textfont=t, showlegend=FALSE)%>%
    #     
    #     layout(title="<b>Scores</b>", scene= list(
    #       xaxis=list(title=paste0("Dimension ",input$npc1scores3dplotISOMAP),zerolinecolor="lightgrey"),
    #       yaxis=list(title=paste0("Dimension ",input$npc2scores3dplotISOMAP),zerolinecolor="lightgrey"),
    #       zaxis=list(title=paste0("Dimension ",input$npc3scores3dplotISOMAP),zerolinecolor="lightgrey"))
    #     )
    # })}
    # 
    # 
    # if(input$classcoresISOMAP==F)
    # {output$ISOMAP3dscores <- renderPlotly({
    #   
    #   scores<-ISOMAP()$points
    #   t <- list(
    #     family = "times",
    #     size = 9.5,
    #     color = toRGB("grey50"))
    #   
    #   plot_ly(type="scatter3d", 
    #           x=scores[,input$npc1scores3dplotISOMAP],
    #           y=scores[,input$npc2scores3dplotISOMAP],
    #           z=scores[,input$npc3scores3dplotISOMAP],
    #           mode="markers",
    #           showlegend=F,
    #           text=as.character(t(id())),
    #           marker = list(symbol = 'circle', sizemode = 'diameter', size=5),
    #           hoverinfo="skip")%>%
    #     
    #     add_text(textposition="top", textfont=t, showlegend=FALSE)%>%
    #     
    #     layout(title="<b>Scores</b>", scene= list(
    #       xaxis=list(title=paste0("Dimension ",input$npc1scores3dplotISOMAP),zerolinecolor="lightgrey"),
    #       yaxis=list(title=paste0("Dimension ",input$npc2scores3dplotISOMAP),zerolinecolor="lightgrey"),
    #       zaxis=list(title=paste0("Dimension ",input$npc3scores3dplotISOMAP),zerolinecolor="lightgrey"))
    #     )
    # })}
    
    
    ##########################################################
    if(input$classcoresISOMAP==F)
    {output$isomapISOMAP <- renderPlot(plot(ISOMAP()),res = 96, height = 800)}
    
    if(input$classcoresISOMAP==T)
    {output$isomapISOMAP <- renderPlot(plot(ISOMAP(), n.col=sampleclasscolour()),res = 96, height = 800)}
    
    
    
    
  }) 
  
  
  ##Export model
  output$saveISOMAPmodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelISOMAP, " ISOMAP model", ".Rdata")
    },
    content = function(file){
      FA1<-ISOMAP()
      sampleclass<-sampleclass()
      ISOMAPmodel<-FA1
      id<-id()
      variables<-variables()
      scores<-data.frame(ISOMAP()$points[,1:input$numcompselISOMAP])
      rownames(scores)<-t(id())
      data<-scores
      variables<-data.frame(colnames(data))
      save(data, ISOMAPmodel, sampleclass, id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importISOMAPmodel, {validate(need(grepl(".Rdata", input$searchmodelISOMAP$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelISOMAP$datapath)
    ISOMAP(ISOMAPmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  #--------export csv
  
  {namefileISOMAP <- reactive(input$namemodelISOMAP)
    
    output$downloadscoresISOMAP <- downloadHandler(
      filename = function() {
        paste0(namefileISOMAP(),"_projectionISOMAP", ".csv")
      },
      content = function(file) {
        scores<-data.frame(ISOMAP()$points[,1:input$numcompselISOMAP])
        rownames(scores)<-t(id())
        write.csv(scores, file)
      }
    )
  }
  
  
  #-----------------------------------------------------------------------------------KERNEL PCA 
  
  observeEvent(input$bkernelpca, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    pcadata1<-reactive(data())
    
    if(input$prepropca == "none"){
      matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
    
    if(input$prepropca == "center"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
    }
    
    if(input$prepropca == "scale"){
      matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
    }
    
    
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-matrix[,matrix2]
    
    if(input$kernelpcamethod=='rbfdot'||input$kernelpcamethod=='besseldot'||input$kernelpcamethod=='anovadot')
    {kernelpca <- kpca(data.matrix(pcadata), kernel = input$kernelpcamethod, features = input$ncompkernelpca)}
    
    else
    {kernelpca <- kpca(data.matrix(pcadata), kernel = input$kernelpcamethod, features = input$ncompkernelpca, kpar=list())}
    
    KERNELPCA(kernelpca)
    
    removeModal()
    
  })
  
  
  observeEvent(c(input$importkernelpcamodel,input$bkernelpca, input$classcoreskernelpca), ignoreInit = T ,{req()
    
    #output$Sumkernelpca<-verbatimTextOutput(summary(kernelpca()))
    
    
    if(input$classcoreskernelpca==T)
    {output$kernelpcascores <- renderPlotly({
      scores<-data.frame(KERNELPCA()@rotated)
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresClass(scores,input$npc1scoresplotkernelpca, input$npc2scoresplotkernelpca, input$siglimkernelpca, sampleclass, id, "Component ", " PC")
    })
    }
    
    
    if(input$classcoreskernelpca==F)
    {output$kernelpcascores <- renderPlotly({
      
      scores<-data.frame(KERNELPCA()@rotated)
      sampleclass<-sampleclass()
      id<-id()
      
      PlotScoresGeneral(scores,input$npc1scoresplotkernelpca, input$npc2scoresplotkernelpca, input$siglimkernelpca, sampleclass, id, "Component ", " PC")
    })
    
    }
    
    
    #--------------3D scores
    
    if(input$classcoreskernelpca==T){
      output$kernelpca3dscores <- renderPlotly({
        scores<-data.frame(KERNELPCA()@rotated)
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotkernelpca, input$npc2scores3dplotkernelpca, input$npc3scores3dplotkernelpca, input$siglim3Dkernelpca, sampleclass, id, "Component ", " PC")
        
      })
    }
    
    
    if(input$classcoreskernelpca==F){
      output$kernelpca3dscores <- renderPlotly({
        scores<-data.frame(KERNELPCA()@rotated)
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotkernelpca, input$npc2scores3dplotkernelpca, input$npc3scores3dplotkernelpca, input$siglim3Dkernelpca, sampleclass, id, "Component ", " PC")
        
      })
    }
    
    
    #------------------------Loadings
    
    output$kernelpcaload<-renderPlotly({
      
      loadings<-KERNELPCA()@pcv
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      if(input$isspectra == F){
        plot<-PlotNormalLoadings(loadings, input$npc1loadplotkernelpca, input$npc2loadplotkernelpca, variables, data2, "Component ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralLoadings(loadings, input$npc1loadplotkernelpca, input$npc2loadplotkernelpca, variables, data2, "Component ")
      }
      
      plot
      
    })
    
    
    #------------------------nLoadings
    
    output$kernelpcaEigenvectors<-renderPlotly({
      
      variance<-KERNELPCA()@eig
      
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:length(variance),
              y=variance,
              mode="lines+markers",
              showlegend=F,
              text=paste("Component ", 1:length(variance), sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Eigenvectors</b>",
               xaxis=list(title=paste0("Component"),zerolinecolor="black"),
               yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
      
    })
    
    
  }) 
  
  
  ##Export model
  output$savekernelpcamodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelkernelpca, " kernelpca model", ".Rdata")
    },
    content = function(file){
      FA1<-KERNELPCA()
      sampleclass<-sampleclass()
      kernelpcamodel<-FA1
      id<-id()
      variables<-variables()
      scores<-data.frame(KERNELPCA()@rotated[,1:input$numcompselkernelpca])
      rownames(scores)<-t(id())
      data<-scores
      variables<-data.frame(colnames(data))
      save(data, kernelpcamodel, sampleclass, id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importkernelpcamodel, {validate(need(grepl(".Rdata", input$searchmodelkernelpca$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelkernelpca$datapath)
    KERNELPCA(kernelpcamodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  #--------export csv
  
  {namefilekernelpca <- reactive(input$namemodelkernelpca)
    
    output$downloadscoreskernelpca <- downloadHandler(
      filename = function() {
        paste0(namefilekernelpca(),"_rotatedkernelpca", ".csv")
      },
      content = function(file) {
        scores<-data.frame(KERNELPCA()@rotated[,1:input$numcompselkernelpca])
        rownames(scores)<-t(id())
        write.csv(scores, file)
      }
    )
  }
  
  
  
  
  #--------------------------------------------------------------------------------------------Filter Methods---------------------
  
  
  #----------------------------------------------------------Low-Var 
  observeEvent(input$blowvar, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    pcadata1<-data()
    matrix<-pcadata1
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-pcadata1[,matrix2]
    data<-pcadata
    
    if(input$lowvarmethod == 'uniquecut')
    {lowvarfiltered <- {rec<-recipe(~.,data=data)%>%
      step_nzv(all_predictors(), unique_cut = input$uniquecutval)%>%
      step_dummy(all_nominal())
    
    newdata<-rec%>%
      prep()%>%
      bake(new_data=data)
    
    newdata}
    }
    
    if(input$lowvarmethod == 'freqcut')
    {lowvarfiltered<-{rec<-recipe(~.,data=data)%>%
      step_nzv(all_predictors(), freq_cut = input$freqcutval)%>%
      step_dummy(all_nominal())
    newdata<-rec%>%
      prep()%>%
      bake(new_data=data)
    
    newdata}
    }
    
    rownames(lowvarfiltered)<-t(id())
    LOWVAR(lowvarfiltered)
    
    removeModal()
    
  })
  
  observeEvent(c(input$blowvar), ignoreInit = T, {
    
    output$Sumlowvar<-renderPrint({
      
      samevars<-data.frame(colnames(LOWVAR()))
      colnames(samevars)<-"Retained Variables"
      
      oldvars<-data.frame(setdiff(colnames(data()), colnames(LOWVAR())))
      colnames(oldvars)<-"Eliminated Variables"
      
      
      print(c(samevars, oldvars))
      
    })
    
  })
  
  observeEvent(c(input$confirmlowvar), ignoreInit = T, {
    
    data(LOWVAR())
    variables(data.frame(colnames(LOWVAR())))
  })
  
  
  
  output$savelowvarcsv<-downloadHandler(
    filename = function() {
      paste0(input$namelowvar, " filtered data", ".csv")
    },
    content = function(file){
      data<-data()
      write.csv2(data, file)
    }
  )
  
  output$savelowvar<-downloadHandler(
    filename = function() {
      paste0(input$namelowvar, " filtered data", ".Rdata")
    },
    content = function(file){
      data<-data()
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      save(data, sampleclass, id, variables, file = file)
    }
  )
  
  #-----------------------------------------------------------High Corr    
  
  observeEvent(input$bhighcorr, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    pcadata1<-data()
    matrix<-pcadata1
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-pcadata1[,matrix2]
    data<-pcadata
    
    
    {highcorrfiltered <- {rec<-recipe(~.,data=data)%>%
      step_corr(all_predictors(), threshold = input$highcorrthreshold)%>%
      step_dummy(all_nominal())
    
    newdata<-rec%>%
      prep()%>%
      bake(new_data=data)
    
    newdata}
      
    }
    
    
    HIGHCORR(highcorrfiltered)
    
    removeModal()
    
  })
  
  observeEvent(c(input$bhighcorr, HIGHCORR()), ignoreInit = T, {
    
    output$Sumhighcorr<-renderPrint({
      
      samevars<-data.frame(colnames(HIGHCORR()))
      colnames(samevars)<-"Retained Variables"
      
      oldvars<-data.frame(setdiff(colnames(data()), colnames(HIGHCORR())))
      colnames(oldvars)<-"Eliminated Variables"
      
      
      print(c(samevars, oldvars))
      
    })
    
  })
  
  observeEvent(c(input$confirmhighcorr), ignoreInit = T, {
    
    data(HIGHCORR())
    variables(data.frame(colnames(HIGHCORR())))
    
  })
  
  
  output$savehighcorrcsv<-downloadHandler(
    filename = function() {
      paste0(input$namehighcorr, " filtered data", ".csv")
    },
    content = function(file){
      data<-data()
      write.csv2(data, file)
    }
  )
  
  output$savehighcorr<-downloadHandler(
    filename = function() {
      paste0(input$namehighcorr, " filtered data", ".Rdata")
    },
    content = function(file){
      data<-data()
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      save(data, sampleclass, id, variables, file = file)
    }
  )
  
  
  #---------------------------------------------------------------------WAVELET
  
  
  observeEvent(input$WAVELETfiltertype, {
    
    if(input$WAVELETfiltertype == 'd')
    {updateNumericInput(inputId = "filterlenght", value = 12, min = 2, max = 20, step = 2)}
    
    if(input$WAVELETfiltertype == 'la')
    {updateNumericInput(inputId = "filterlenght", value = 14, min = 8, max = 20, step = 2)}
    
    if(input$WAVELETfiltertype == 'bl')
    {updateNumericInput(inputId = "filterlenght", value = 18, min = 14, max = 20, step = 2)}
    
    if(input$WAVELETfiltertype == 'c')
    {updateNumericInput(inputId = "filterlenght", value = 18, min = 6, max = 30, step = 6)}
    
  })
  
  
  
  observeEvent(input$bWAVELET, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    pcadata1<-data()
    matrix<-pcadata1
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-pcadata1[,matrix2]
    data<-pcadata
    
    
    #waveletfunc <- wavelets::dwt(data.matrix(data), filter = paste(input$WAVELETfiltertype, input$filterlenght, sep = ""))
    waveletfunc <- wavelets::dwt(t(data), filter = paste(input$WAVELETfiltertype, input$filterlenght, sep = ""))
    waveletfunc2 <-align(waveletfunc)
    
    
    WAVELET(waveletfunc2)
    
    removeModal()
    
  })
  
  
  observeEvent(c(input$bWAVELET, WAVELET(), input$nwselectWAVELET, input$highlowWAVELET, input$classcoreswavelet), ignoreInit = T, {req(WAVELET())
    
    if (input$highlowWAVELET == 'hp')
    {
      #wavelet<-data.frame(matrix(WAVELET()@W[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@W[[input$nwselectWAVELET]])/nrow(data())))
      wavelet<-data.frame(matrix(WAVELET()@W[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@W[[input$nwselectWAVELET]])/nrow(data()), byrow = T))
    }
    
    if (input$highlowWAVELET == 'lp')
    {
      #wavelet<-data.frame(matrix(WAVELET()@V[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@V[[input$nwselectWAVELET]])/nrow(data())))
      wavelet<-data.frame(matrix(WAVELET()@V[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@V[[input$nwselectWAVELET]])/nrow(data()), byrow = T))
    }
    
    
    if(input$classcoreswavelet == T)
    {
      output$compressedwaveletplot <- renderPlotly({t <- list(
        family = "times",
        size = 12,
        color = toRGB("grey50"))
      
      tdata<-t(wavelet)
      
      id<-id()
      variables<-variables()
      
      Plot <-  plot_ly(type="scatter",
                       #data=tdata,
                       x=c(1:ncol(wavelet)),
                       y=tdata[,1],
                       mode="lines",
                       #options=c(),
                       showlegend=T,
                       #text=data2,
                       text="",
                       name = data.frame(t(id))[,1],
                       line = list(width = 0.8),
                       color = t(sampleclass())[[1]],
                       colors = "Set1",
                       hoverinfo="skip")%>%
        layout(title="<b>Compressed data</b>",
               xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
      
      for (i in 2:ncol(tdata)) {
        
        Plot<-add_trace(Plot,
                        y=tdata[,i],
                        mode="lines",
                        showlegend=T,
                        #text=data2,
                        text="",
                        name = data.frame(t(id))[,i],
                        color = t(sampleclass())[[i]],
                        colors = "Set1",
                        hoverinfo="skip")
        #add_text(textposition="topright", textfont=t)%>%
        
        
      }
      
      
      Plot
      })
      #output$compressedwaveletplot <- renderPlot(matplot(t(wavelet), type = "l", lty = 1, ylab = ""))
      
    }
    
    if(input$classcoreswavelet == F)
    {
      output$compressedwaveletplot <- renderPlotly({t <- list(
      family = "times",
      size = 12,
      color = toRGB("grey50"))
        
     tdata<-t(wavelet)
        
     id<-id()
     variables<-variables()
    
    Plot <-  plot_ly(type="scatter",
                     #data=tdata,
                     x=c(1:ncol(wavelet)),
                     y=tdata[,1],
                     mode="lines",
                     #options=c(),
                     showlegend=T,
                     #text=data2,
                     text="",
                     name = data.frame(t(id))[,1],
                     line = list(width = 0.8),
                     #color = colnames(loadings)[[1]],
                     hoverinfo="skip")%>%
      layout(title="<b>Compressed data</b>",
             xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
             yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
    
    for (i in 2:ncol(tdata)) {
      
      Plot<-add_trace(Plot,
                      y=tdata[,i],
                      mode="lines",
                      showlegend=T,
                      #text=data2,
                      text="",
                      name = data.frame(t(id))[,i],
                      #color = colnames(loadings)[[i]],
                      hoverinfo="skip")
      #add_text(textposition="topright", textfont=t)%>%
      
      
    }
    
    
    Plot
    })
      #output$compressedwaveletplot <- renderPlot(matplot(t(wavelet), type = "l", lty = 1, ylab = ""))
      
      }
    
    if(input$classcoreswavelet == T)
    {
      #output$originalwaveletplot <- renderPlot(matplot(t(data()), type = "l", lty = 1, ylab = "", col = sampleclasscolour()))
      
      output$originalwaveletplot <- renderPlotly({t <- list(
        family = "times",
        size = 12,
        color = toRGB("grey50"))
      
      tdata<-t(data())
      
      id<-id()
      variables<-variables()
      
      Plot <-  plot_ly(type="scatter",
                       #data=tdata,
                       x=c(1:ncol(data())),
                       y=tdata[,1],
                       mode="lines",
                       #options=c(),
                       showlegend=T,
                       #text=data2,
                       text="",
                       name = data.frame(t(id))[,1],
                       line = list(width = 0.8),
                       color = t(sampleclass())[[1]],
                       colors = "Set1",
                       hoverinfo="skip")%>%
        layout(title="<b>Original Spectra</b>",
               xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
      
      for (i in 2:ncol(tdata)) {
        
        Plot<-add_trace(Plot,
                        y=tdata[,i],
                        mode="lines",
                        showlegend=T,
                        #text=data2,
                        text="",
                        name = data.frame(t(id))[,i],
                        color = t(sampleclass())[[i]],
                        colors = "Set1",
                        hoverinfo="skip")
        #add_text(textposition="topright", textfont=t)%>%
        
        
      }
      
      
      Plot
      })
      }
    
    if(input$classcoreswavelet == F)
    {
      #output$originalwaveletplot <- renderPlot(matplot(t(data()), type = "l", lty = 1, ylab = ""))
      
      output$originalwaveletplot <- renderPlotly({
        t <- list(
        family = "times",
        size = 12,
        color = toRGB("grey50"))
      
      tdata<-t(data())
      
      id<-id()
      variables<-variables()
      
      Plot <-  plot_ly(type="scatter",
                       #data=tdata,
                       x=c(1:ncol(data())),
                       y=tdata[,1],
                       mode="lines",
                       #options=c(),
                       showlegend=T,
                       #text=data2,
                       text="",
                       name = data.frame(t(id))[,1],
                       line = list(width = 0.8),
                       # color = t(sampleclass())[[1]],
                       # colors = "Set1",
                       hoverinfo="skip")%>%
        layout(title="<b>Original Spectra</b>",
               xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
      
      for (i in 2:ncol(tdata)) {
        
        Plot<-add_trace(Plot,
                        y=tdata[,i],
                        mode="lines",
                        showlegend=T,
                        #text=data2,
                        text="",
                        name = data.frame(t(id))[,i],
                        # color = t(sampleclass())[[i]],
                        # colors = "Set1",
                        hoverinfo="skip")
        #add_text(textposition="topright", textfont=t)%>%
        
        
      }
      
      
      Plot
      })
      }
    
    
  })
  
  
  
  observeEvent(c(input$confirmWAVELET), ignoreInit = T, {
    
    if (input$highlowWAVELET == 'hp')
    {
      #wavelet<-data.frame(matrix(WAVELET()@W[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@W[[input$nwselectWAVELET]])/nrow(data())))
      wavelet<-data.frame(matrix(WAVELET()@W[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@W[[input$nwselectWAVELET]])/nrow(data()), byrow = T))
    }
    
    if (input$highlowWAVELET == 'lp')
    {
      #wavelet<-data.frame(matrix(WAVELET()@V[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@V[[input$nwselectWAVELET]])/nrow(data())))
      wavelet<-data.frame(matrix(WAVELET()@V[[input$nwselectWAVELET]], nrow = nrow(data()), ncol = length(WAVELET()@V[[input$nwselectWAVELET]])/nrow(data()), byrow = T))
    }
    
    colnames(wavelet)<-c(1:ncol(wavelet))
    data(wavelet)
    variables(data.frame(colnames(wavelet)))
    
  })
  
  
  output$saveWAVELETcsv<-downloadHandler(
    filename = function() {
      paste0(input$nameWAVELET, " compressed", ".csv")
    },
    content = function(file){
      data<-data()
      write.csv2(data, file)
    }
  )
  
  output$saveWAVELET<-downloadHandler(
    filename = function() {
      paste0(input$nameWAVELET, " compressed", ".Rdata")
    },
    content = function(file){
      data<-data()
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      save(data, sampleclass, id, variables, file = file)
    }
  )
  
  
  #-------------------------------------------------------------------Resolution Reduction
  
  observeEvent(input$bREDC, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    pcadata1<-data()
    matrix<-pcadata1
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-pcadata1[,matrix2]
    data<-pcadata
    
    kept<-c()
    
    for (i in 1:ncol(data)) {
      
      if((i*input$ntimesREDC)<ncol(data)){
        kept[[i]]<-colnames(data)[(1+(input$ntimesREDC*i))]
      }
      
      else
      {kept<-as.character(t(kept))
      
      break}
    }
    
    # variables2<-colnames(data[,kept])
    data2<-data[,(kept)]
    
    
    REDC(data2)
    
    removeModal()
    
  })
  
  observeEvent(c(input$bREDC, REDC(), input$nwselectREDC, input$highlowREDC, input$classcoresREDC, data()), ignoreInit = T, {
    
    req(REDC())
    req(data())
    
    if(input$classcoresREDC == T)
    {
      #output$compressedREDCplot <- renderPlot(matplot(t(REDC()), type = "l", lty = 1, ylab = "", col = sampleclasscolour()))
      
      output$compressedREDCplot <- renderPlotly({
        
        t <- list(
          family = "times",
          size = 12,
          color = toRGB("grey50"))
        
        tdata<-t(REDC())
        
        id<-id()
        variables<-data.frame(colnames(REDC()), check.names = F)
        
        Plot <-  plot_ly(type="scatter",
                         #data=tdata,
                         x=t(variables),
                         y=tdata[,1],
                         mode="lines",
                         #options=c(),
                         showlegend=T,
                         #text=data2,
                         text="",
                         name = data.frame(t(id))[,1],
                         line = list(width = 0.8),
                         color = t(sampleclass())[[1]],
                         colors = "Set1",
                         hoverinfo="skip")%>%
          layout(title="<b>Compressed data</b>",
                 xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
                 yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
        
        for (i in 2:ncol(tdata)) {
          
          Plot<-add_trace(Plot,
                          y=tdata[,i],
                          mode="lines",
                          showlegend=T,
                          #text=data2,
                          text="",
                          name = data.frame(t(id))[,i],
                          color = t(sampleclass())[[i]],
                          colors = "Set1",
                          hoverinfo="skip")
          #add_text(textposition="topright", textfont=t)%>%
          
          
        }
        
        
        Plot
      })
      
      }
    
    if(input$classcoresREDC == F)
    {
      
      #output$compressedREDCplot <- renderPlot(matplot(t(REDC()), type = "l", lty = 1, ylab = ""))
      
      output$compressedREDCplot <- renderPlotly({
        
          t <- list(
          family = "times",
          size = 12,
          color = toRGB("grey50"))
        
        tdata<-t(REDC())
        
        id<-id()
        variables<-data.frame(colnames(REDC()), check.names = F)
        
        Plot <-  plot_ly(type="scatter",
                         #data=tdata,
                         x=as.double(data.matrix(t(variables))),
                         y=tdata[,1],
                         mode="lines",
                         #options=c(),
                         showlegend=T,
                         #text=data2,
                         text="",
                         name = data.frame(t(id))[,1],
                         line = list(width = 0.8),
                         # color = t(sampleclass())[[1]],
                         # colors = "Set1",
                         hoverinfo="skip")%>%
          layout(title="<b>Compressed data</b>",
                 xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
                 yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
        
        for (i in 2:ncol(tdata)) {
          
          Plot<-add_trace(Plot,
                          y=tdata[,i],
                          mode="lines",
                          showlegend=T,
                          #text=data2,
                          text="",
                          name = data.frame(t(id))[,i],
                          # color = t(sampleclass())[[i]],
                          # colors = "Set1",
                          hoverinfo="skip")
          #add_text(textposition="topright", textfont=t)%>%
          
          
        }
        
        
        Plot
        })
      
      
      }
    
    if(input$classcoresREDC == T)
    {
      #output$originalREDCplot <- renderPlot(matplot(t(data()), type = "l", lty = 1, ylab = "", col = sampleclasscolour()))
    
    output$originalREDCplot <- renderPlotly({
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("grey50"))
      
      tdata<-t(data())
      
      id<-id()
      variables<-variables()
      
      Plot <-  plot_ly(type="scatter",
                       #data=tdata,
                       x=as.double(data.matrix(data.frame(t(variables), check.names = F))),
                       y=tdata[,1],
                       mode="lines",
                       #options=c(),
                       showlegend=T,
                       #text=data2,
                       text="",
                       name = data.frame(t(id))[,1],
                       line = list(width = 0.8),
                       color = t(sampleclass())[[1]],
                       colors = "Set1",
                       hoverinfo="skip")%>%
        layout(title="<b>Original Spectra</b>",
               xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
      
      for (i in 2:ncol(tdata)) {
        
        Plot<-add_trace(Plot,
                        y=tdata[,i],
                        mode="lines",
                        showlegend=T,
                        #text=data2,
                        text="",
                        name = data.frame(t(id))[,i],
                        color = t(sampleclass())[[i]],
                        colors = "Set1",
                        hoverinfo="skip")
        #add_text(textposition="topright", textfont=t)%>%
        
        
      }
      
      
      Plot
    })
    
    }
    
    if(input$classcoresREDC == F)
    {
      #output$originalREDCplot <- renderPlot(matplot(t(data()), type = "l", lty = 1, ylab = ""))
      output$originalREDCplot <- renderPlotly({
        t <- list(
          family = "times",
          size = 12,
          color = toRGB("grey50"))
        
        tdata<-t(data())
        
        id<-id()
        variables<-variables()
        
        Plot <-  plot_ly(type="scatter",
                         #data=tdata,
                         x=as.double(data.matrix(data.frame(t(variables), check.names = F))),
                         y=tdata[,1],
                         mode="lines",
                         #options=c(),
                         showlegend=T,
                         #text=data2,
                         text="",
                         name = data.frame(t(id))[,1],
                         line = list(width = 0.8),
                         # color = t(sampleclass())[[1]],
                         # colors = "Set1",
                         hoverinfo="skip")%>%
          layout(title="<b>Original Spectra</b>",
                 xaxis=list(title=paste0(""),zerolinecolor="lightgrey"),
                 yaxis=list(title=paste0(""),zerolinecolor="lightgrey"))
        
        for (i in 2:ncol(tdata)) {
          
          Plot<-add_trace(Plot,
                          y=tdata[,i],
                          mode="lines",
                          showlegend=T,
                          #text=data2,
                          text="",
                          name = data.frame(t(id))[,i],
                          # color = t(sampleclass())[[i]],
                          # colors = "Set1",
                          hoverinfo="skip")
          #add_text(textposition="topright", textfont=t)%>%
          
          
        }
        
        
        Plot
      })
      }
    
    
  })
  
  observeEvent(c(input$confirmREDC), ignoreInit = T, {
    
    data(REDC())
    variables(data.frame(colnames(REDC()), check.names = F))
    
  })
  
  
  
  output$saveREDCcsv<-downloadHandler(
    filename = function() {
      paste0(input$nameREDC, " reducted", ".csv")
    },
    content = function(file){
      data<-data()
      write.csv2(data, file)
    }
  )
  
  output$saveREDC<-downloadHandler(
    filename = function() {
      paste0(input$nameREDC, " reducted", ".Rdata")
    },
    content = function(file){
      data<-data()
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      save(data, sampleclass, id, variables, file = file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

# Internal Functions


#---Scores
PlotScoresGeneral<-function(scores, PC1, PC2, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  t <- list(
    family = "times",
    size = 9.5,
    color = toRGB("grey50"))
  
  
  ellipse <- ellipse::ellipse(cov(x=scores[,PC1], y=scores[,PC2]
  ),
  scale = c(sd(scores[,PC1]),
            sd(scores[,PC2])
  ),
  level = confidenceinterval,
  centre = c(mean(scores[,PC1]),
             mean(scores[,PC2]))
  )
  
  
  
  
  {plot_ly(type="scatter", 
           x=scores[,PC1],
           y=scores[,PC2],
           mode="markers",
           showlegend=T,
           name = "Markers",
           text=as.character(t(id)),
           hoverinfo="skip")%>%
      add_text(textposition="top", textfont=t, name = "Labels", showlegend=T)%>%
      layout(title="<b>Scores</b>",
             xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
             yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey")
      )%>%
      
      add_trace(x=ellipse[,1], 
                y=ellipse[,2],
                type="scatter",
                mode="lines",
                colors="black",
                text = "",
                hoverinfo = "skip",
                line = list(color="red", width=.8),
                inherit = F,
                showlegend = T,
                name = "Ellipse"
      )
  }
}

#---Scores with classes
PlotScoresClass<-function(scores, PC1, PC2, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  cov<-cov(x=data.frame(scores))
  ellipseclass<-list()
  convexhullpos<-list()
  con.hull<-list()
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]])
    ellipseclass[[i]]<-ellipse::ellipse(cov(x=scores[pos, c(PC1,PC2)]),
                                        level = confidenceinterval,
                                        centre = c(mean(scores[pos,PC1]),
                                                   mean(scores[pos,PC2])
                                        ),
    )
    
  }
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  Plot<-plot_ly(type="scatter", 
                x=scores[,PC1],
                y=scores[,PC2],
                mode="markers",
                showlegend=T,
                color = factor(sampleclass),
                symbol = factor(sampleclass),
                colors = "Set1",
                text=as.character(t(id)),
                hoverinfo="")%>%
    
    add_text(textposition="top", 
             textfont=t, 
             showlegend=T, 
             inherit = F, 
             x=scores[,PC1], 
             y=scores[,PC2], 
             text=as.character(t(id)),
             color = factor(sampleclass))%>%
    
    layout(title="<b>Scores</b>",
           xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey")
    )
  
  for (i in 1:length(unique(sampleclass))) {
    
    Plot<-add_trace(Plot,
                    x=ellipseclass[[i]][,1], 
                    y=ellipseclass[[i]][,2],
                    type="scatter",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    hoverinfo = "skip",
                    #text = unique(sampleclass)[[i]],
                    line = list(color=factor(unique(sampleclass))[[i]], width=.8, colors = "Set1"),
                    name=paste0("HoT Ellipse ",unique(sampleclass)[[i]]),
                    inherit = F
    )
    
    
  }
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]]) 
    
    convexhullpos[[i]]<-pos[chull(scores[pos,PC1], scores[pos,PC2])]
    
    con.hull[[i]]<-rbind(scores[convexhullpos[[i]],],scores[convexhullpos[[i]],]) # get coordinates for convex hull
  }
  
  
  
  for (i in 1:length(unique(sampleclass))) {
    Plot<-add_trace(Plot,
                    x=con.hull[[i]][,PC1],
                    y=con.hull[[i]][,PC2],
                    type="scatter",
                    mode="lines+markers",
                    inherit = F,
                    color = factor(unique(sampleclass))[[i]],
                    line = list(color=factor(unique(sampleclass))[[i]], width=.8, colors = "Set1"),
                    name=paste0("Convex Hull ",unique(sampleclass)[[i]]),
                    visible = "legendonly"
    )
    
    
  }
  
  Plot}
#---Plot Scores 3D

Plot3DscoresGeneral<-function(scores, PC1, PC2, PC3, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  cov<-cov(x=data.frame(scores))
  
  ellipse1<- ellipse::ellipse(cov(x=scores), scale = c(sd(scores[,PC1]), sd(scores[,PC2])),level = confidenceinterval
  )
  ellipse2<- ellipse::ellipse(cov(x=scores), scale = c(sd(scores[,PC1]), sd(scores[,PC3])),level = confidenceinterval
  )
  ellipse3<- ellipse::ellipse(cov(x=scores), scale = c(sd(scores[,PC2]), sd(scores[,PC3])),level = confidenceinterval
  )
  
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  
  Plot<-plot_ly(type="scatter3d", 
                x=scores[,PC1],
                y=scores[,PC2],
                z=scores[,PC3],
                mode="markers",
                showlegend=T,
                #color = factor(sampleclass),
                colors = "Set1",
                name = "Markers",
                text=as.character(t(id)),
                marker = list(size=4, showlegend=T),
                hoverinfo="")
  
  Plot<-add_text(Plot,
                 textposition="top", textfont=t, showlegend=T, mode="text", inherit = F,
                 x=scores[,PC1],
                 y=scores[,PC2],
                 z=scores[,PC3],
                 text=as.character(t(id)),
                 #color = factor(sampleclass),
                 name = "Labels",
                 colors = "Set1"
  )
  
  Plot<-layout(Plot,
               title="<b>Scores</b>",
               xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC3),zerolinecolor="lightgrey")
  )
  
  Plot<-add_trace(Plot,
                  x=ellipse1[,1],
                  y=ellipse1[,2],
                  z=0,
                  type="scatter3d",
                  mode="lines",
                  colors="black",
                  name=paste0(LabelABV,PC1),
                  text = "",
                  hoverinfo = "skip",
                  line = list(color="red", width=1.5),
                  inherit = F
  )
  Plot<-add_trace(Plot,
                  x=ellipse2[,1],
                  y=0,
                  z=ellipse2[,2],
                  type="scatter3d",
                  mode="lines",
                  colors="black",
                  name=paste0(LabelABV,PC2),
                  hoverinfo = "skip",
                  line = list(color="red", width=1.5),
                  inherit = F
  )
  
  Plot<-add_trace(Plot,
                  x=0,
                  y=ellipse3[,1],
                  z=ellipse3[,2],
                  type="scatter3d",
                  mode="lines",
                  colors="black",
                  name=paste0(LabelABV,PC3),
                  text = "",
                  hoverinfo = "skip",
                  line = list(color="red", width=1.5),
                  inherit = F
  )
  
  
  Plot
}

#---Plot Scores 3D Class

Plot3DscoresClasses<-function(scores, PC1, PC2, PC3, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  cov<-cov(x=data.frame(scores))
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  ellipseclass1<- c()
  ellipseclass2<- c()
  ellipseclass3<- c()
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]])
    
    ellipseclass1[[i]]<-ellipse::ellipse(
      cov(x=scores[pos, c(PC1,PC2)]),
      level = confidenceinterval,
      centre = c(mean(scores[pos,PC1]),
                 mean(scores[pos,PC2])
      ),
    )
    
    ellipseclass2[[i]]<-ellipse::ellipse(
      cov(x=scores[pos, c(PC1,PC3)]),
      level = confidenceinterval,
      centre = c(mean(scores[pos,PC1]),
                 mean(scores[pos,PC3])
      ),
    )
    
    
    ellipseclass3[[i]]<-ellipse::ellipse(
      cov(x=scores[pos, c(PC2,PC3)]),
      level = confidenceinterval,
      centre = c(mean(scores[pos,PC2]),
                 mean(scores[pos,PC3])
      ),
    )
    
  }
  
  
  Plot<-plot_ly(type="scatter3d", 
                x=scores[,PC1],
                y=scores[,PC2],
                z=scores[,PC3],
                mode="markers",
                showlegend=T,
                color = factor(sampleclass),
                colors = "Set1",
                text=as.character(t(id)),
                marker = list(size=4, showlegend=T),
                hoverinfo="")
  
  Plot<-add_text(Plot,
                 textposition="top", textfont=t, showlegend=T, mode="text", inherit = F,
                 x=scores[,PC1],
                 y=scores[,PC2],
                 z=scores[,PC3],
                 text=as.character(t(id)),
                 color = factor(sampleclass),
                 colors = "Set1"
  )
  
  Plot<-layout(Plot,
               title="<b>Scores</b>",
               xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC3),zerolinecolor="lightgrey")
  )
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]])
    
    Plot<-add_trace(Plot,
                    x=ellipseclass1[[i]][,1],
                    y=ellipseclass1[[i]][,2],
                    z=mean(scores[pos,PC3]),
                    type="scatter3d",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    name=paste0(unique(sampleclass)[[i]],LabelABV,PC1),
                    text = "",
                    hoverinfo = "skip",
                    line = list(color=factor(unique(sampleclass)[[i]]), width=1.5),
                    inherit = F
    )
    Plot<-add_trace(Plot,
                    x=ellipseclass2[[i]][,1],
                    y=mean(scores[pos,PC2]),
                    z=ellipseclass2[[i]][,2],
                    type="scatter3d",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    name=paste0(unique(sampleclass)[[i]],LabelABV,PC2),
                    hoverinfo = "skip",
                    line = list(color=factor(unique(sampleclass)[[i]]), width=1.5),
                    inherit = F
    )
    # 
    Plot<-add_trace(Plot,
                    x=mean(scores[pos,PC1]),
                    y=ellipseclass3[[i]][,1],
                    z=ellipseclass3[[i]][,2],
                    type="scatter3d",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    name=paste0(unique(sampleclass)[[i]],LabelABV,PC3),
                    text = "",
                    hoverinfo = "skip",
                    line = list(color=factor(unique(sampleclass)[[i]]), width=1.5),
                    inherit = F
    )
    
    
  }
  
  Plot
  
}


#---Biplot
PlotBIPLOT<-function(scores, loadings, PC1, PC2, id, variables, SpecialLabel, LabelABV){
  
  full<-rbind((scores), (loadings))
  names<-c(t(id), t(variables))
  
  ScoresLAB<-rep("Scores", nrow(scores))
  LoadLAB<-rep("Loadings", nrow(loadings))
  LABs<-t(c(ScoresLAB, LoadLAB))
  
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  plot<-plot_ly(
    
    type="scatter", 
    x=full[,PC1],
    y=full[,PC2],
    mode="markers",
    showlegend=T,
    color = factor(LABs),
    colors = "Set1",
    text=as.character(names),
    hoverinfo="")%>%
    
    add_text(textposition="top", textfont=t, showlegend=T, mode="text", inherit = T)%>%
    
    layout(title="<b>Biplot</b>",
           xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey"))
  
  plot<-add_segments(plot,
                     x=0,
                     y=0,
                     xend=full[((nrow(scores)+1):nrow(full)),PC1],
                     yend=full[((nrow(scores)+1):nrow(full)),PC2],
                     inherit = F,
                     line = list(color="red", width=1.5),
                     name="Loadings Lines"
  )
  
  
  plot
}


#----Loadings PC1 x PC2


PlotSpectralLoadings<-function(loadings, PC1, PC2, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  plot_ly(type="scatter", 
          x=loadings[,PC1],
          y=loadings[,PC2],
          mode="lines",
          showlegend=F,
          text="",
          hoverinfo="x , y")%>%
    add_text(textposition="top", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0(SpecialLabels,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabels,PC2),zerolinecolor="lightgrey"))
} 


PlotNormalLoadings<-function(loadings, PC1, PC2, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  plot_ly(type="scatter", 
          x=loadings[,PC1],
          y=loadings[,PC2],
          mode="markers",
          showlegend=F,
          text = data2,
          hoverinfo="skip")%>%
    add_text(textposition="top", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0(SpecialLabels,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabels,PC2),zerolinecolor="lightgrey"))
  
  
}

PlotSpectralNLoadings<-function(loadings, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  Plot <-  plot_ly(type="scatter",
                   x=1:nrow(loadings),
                   y=loadings[,1],
                   mode="lines",
                   showlegend=T,
                   #text=data2,
                   text="",
                   color = colnames(loadings)[[1]],
                   hoverinfo="x , y")%>%
    #add_text(textposition="topright", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
  
  
  for (i in 2:ncol(loadings)) {
    
    Plot<-add_trace(Plot,
                    x=1:nrow(loadings),
                    y=loadings[,i],
                    mode="lines",
                    showlegend=T,
                    #text=data2,
                    text="",
                    color = colnames(loadings)[[i]],
                    hoverinfo="x , y")%>%
      #add_text(textposition="topright", textfont=t)%>%
      layout(title="<b>Loadings</b>",
             xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey"),
             yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
    
    
  }
  
  
  Plot
}

PlotNormalNLoadings<-function(loadings, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  Plot <-  plot_ly(type="scatter",
                   x=data2,
                   y=loadings[,1],
                   mode="markers+lines",
                   showlegend=T,
                   #text=data2,
                   text="",
                   color = colnames(loadings)[[1]],
                   hoverinfo="text, y")%>%
    # add_text(textposition="topright", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey", categoryorder="array" ,categoryarray=data2),
           yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
  
  
  for (i in 2:ncol(loadings)) {
    
    Plot<-add_trace(Plot,
                    x=data2,
                    y=loadings[,i],
                    mode="markers+lines",
                    showlegend=T,
                    #text=data2,
                    text="",
                    color = colnames(loadings)[[i]],
                    hoverinfo="text, y")%>%
      #add_text(textposition="topright", textfont=t)%>%
      layout(title="<b>Loadings</b>",
             xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey", categoryorder="array" ,categoryarray=data2),
             yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
    
    
  }
  
  
  Plot
}



# Run the application 
shinyApp(ui = ui, server = server)
