## Library
suppressWarnings(library(shinyWidgets))
suppressWarnings(library(shinythemes))
suppressWarnings(library(shiny))
suppressWarnings(library(shinyjs))
suppressWarnings(library(waiter))
suppressWarnings(library(rmarkdown))
suppressWarnings(library(markdown))
suppressWarnings(library(bslib))
suppressWarnings(library(shinycustomloader))
suppressWarnings(library(shinycssloaders))
suppressWarnings(library(DiagrammeR))
suppressWarnings(library(DiagrammeRsvg))
suppressWarnings(library(rsvg))
suppressWarnings(library(shinyalert))
options(shiny.maxRequestSize=100*1024^2)

## Creating multiple pages for the app
dat.name <- c("Nilsson", "Mosmann", "SIN3 Network", "Select your own")


## Normalization techniques
norm.name = c("Min-max", "Log Scaling", "Standard Scaling", "Arcsine", "TopS", "Percentage Row", "No Normalization")

## ML algorithms
TB <- c("Decision Tree", "Random Forest","XGBoost", "AdaBoost")
NTB <- c("Naive Bayes", "Linear SVM", "Non-linear SVM", "Polynomial SVM") 
LM <- c("LDA", "Logistic Regression", "Lasso", "Ridge")


# ## VI methods
# VI = c("varImp", "Stepwise ROC")
# 
# ## Scoring method
# Scoring = c("Entropy Score", "Rank Score")

fluidPage( #creates the page
  useWaiter(),
  tags$head(tags$style(".butt{background:#faf8f7; color: #f05305; border: 2px solid #f05305} .nav{font-size: 18px; font-weight: bold;} .tab-panel {font-size: 20px; font-weight: bold}
                       "),
            ),
  theme = bs_theme(version=5, bootswatch = "united"),
  tags$img( # adds the jayhawk
    src="https://kuathletics.com/wp-content/uploads/2019/05/1200px-Kansas_Jayhawks_logo.png",
    style='float: right',
    width="110", height="100"
  ),
  div(titlePanel("PerSEveML: A Web-Based Tool to Identify Persistent Biomarker Structure for Rare Events Using Integrative Machine Learning Approach"),
      style = "color: #00004c", align = "center"), # defining the title
  tags$style(HTML("body{font-family: sans-serif; }")), ## changes color of everything, font: Geneva Tahoma 
  navbarPage( #allows using multiple tabs
      "",
      tabPanel( ## first panel
        "Instructions",
        br(),
        includeMarkdown("Instruction_app.Rmd")
        ),
      tabPanel(## second panel
        "Computational Information",
        tabsetPanel(## 1st panel within computation
          id="tabA",
          type = "tabs",
          tabPanel("Data and Computational Preferences",
                   fluidRow(
                     column(3,
                            h4(),
                            splitLayout(cellWidths = c("40%", "45%"),
                            div(actionButton("resetAll", "Reset", icon("arrows-rotate"), 
                                         style="color: #fff; background-color: #2880bf; border-color: #2e6da4; font-size: 15px"),
                                tags$head(
                                  tags$style(".selectize-dropdown {position: static}")
                                )),
                            # hr(),
                            div(actionButton("SubmitAll", "Submit", icon("play"), 
                                         style="color: #fff; background-color: #88bf28; border-color: #2e6da4; font-size: 15px")),
                            tags$head(
                              tags$style(".selectize-dropdown {position: static}")
                            )),
                            br(),
                            div(h4("Select Input File"),style = "color: #00004c"),
                            selectInput('dat', '', choices = dat.name),
                            # br(),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                                       ),
                            conditionalPanel( 
                              condition = "input.dat == 'Select your own'",
                              radioButtons("file_type","Choose file type", c("csv", "tab", "excel", "RDS"), 
                                           inline=T),
                              conditionalPanel(
                                condition = "input.file_type == 'csv' ",
                                fileInput('dat3_csv',
                                      "Upload your .csv file",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv"))),
                              conditionalPanel(
                                condition = "input.file_type == 'tab' ",
                                fileInput('dat3_tab',
                                          "Upload your tab file",
                                          multiple = FALSE,
                                          accept = c('text', 'tab-separated-values'))),
                              conditionalPanel(
                                condition = "input.file_type == 'excel' ",
                                fileInput('dat3_excel',
                                          "Upload your .xlsx file",
                                          multiple = FALSE,
                                          accept = c(".xlsx"))),
                              conditionalPanel(
                                condition = "input.file_type == 'RDS' ",
                                fileInput('dat3_rds',
                                          "Upload your .rds file",
                                          multiple = FALSE,
                                          accept = c(".rds", ".RDS"))),
                              
                              ),
                            #checkboxInput("header", "Header", TRUE),
                            textOutput("filewarning"),
                            uiOutput('var_ui'),
                            uiOutput('unwanted'),
                            ),
                     column(3,
                            # br(),
                            # actionButton("reload", "Reload App"),
                            div(h4("Normalization Technique"),style = "color: #00004c"),
                            br(),
                            radioButtons('norm', 'Select One', choices = norm.name, selected = "No Normalization"),
                            # splitLayout(cellWidths = c("40%", "40%"),
                            conditionalPanel( 
                              condition = "input.norm == 'Arcsine'",
                              checkboxInput('arcsine_h', 'Arcsineh', value = F)),
                            conditionalPanel(
                              condition = ("input.norm == 'Arcsine' && input.arcsine_h"),
                              numericInputIcon('cofactor', "Enter Cofactor",
                                           value="150",
                                           step = 10,
                                           width="80%")),
                            conditionalPanel(
                              condition = ("input.norm == 'Log Scaling'"),
                              numericInput('logc', 'Constant value',
                                           value = 0.0001,
                                           width="80%"
                                           )
                            )
                            ),
                     column(3,
                            div(h4("Choose ML algorithms"),style = "color: #00004c"),
                            br(),
                            checkboxGroupInput('tree', 'Tree based', TB),
                            checkboxGroupInput('nontree', 'Non-tree based', NTB),
                            checkboxGroupInput('linear', 'Linear', LM),
                            # div(checkboxInput('nestedcv', "Use nestedCV", value=FALSE, width='80%'),
                            #      style="color:  #FF5733; font-weight: bold; border-color: #FF5733")
                            ),
                     column(3,
                            div(h4("Insert train-test ratio"),style = "color: #00004c"),
                            br(),
                            numericInputIcon('ratio', "", 
                                         value="80",
                                         min=0,
                                         max=100,
                                         step=10,
                                         width='80%',
                                         icon=list(NULL, icon("percent"))),
                            div(h4("Value of k for cross validation"),style = "color: #00004c"),
                            br(),
                            numericInput('kval', "", 
                                         value="5",
                                         min=1,
                                         max=10,
                                         width='80%',
                                         step=1),
                            div(h4("Insert cut-point analysis cutoff"),style = "color: #00004c"),
                            br(),
                            numericInputIcon('Cutpt', "", 
                                         value="40",
                                         min=1,
                                         max=100,
                                         step=10,
                                         width='80%',
                                         icon=list(NULL, icon("percent")))
                     ),
                     
                   )),
       )),
      tabPanel( ## third panel
        "Output",
        tabsetPanel(## 1st panel within computation
          id="tabB",
          type = "tabs",
          tabPanel("Tables",
                   div(h4("Download normalized data"),style = "color: black"),
                   downloadButton("Normal_Table", "Download", class = "butt"),
                   shinycssloaders::withSpinner(tableOutput('norm_col'),color='#ee6a50'),
                   div(h4("Download entropy and rank scores based on variable importance"),
                       style = "color: black"),
                   downloadButton("Scores_Data", "Download", class = "butt"),
                   shinycssloaders::withSpinner(tableOutput('scores'),color='#ee6a50'),
                   # withLoader(tableOutput('scores'), type="html", loader="myloader"),
                   div(h4("Download model metrics"),style = "color: black"),
                   downloadButton("Eval_Tab", "Download", class = "butt"),
                   shinycssloaders::withSpinner(tableOutput('eval'),color='#ee6a50'),
                   div(h4("Download Persistent Feature Stucture Calculation"),
                       style = "color: black"),
                   downloadButton("Feature_Structure", "Download", class = "butt"),
                   shinycssloaders::withSpinner(tableOutput('PFS'),color='#ee6a50')),
          tabPanel("Figures", 
          div(h4("Normalized data"),style = "color: black"),
          downloadButton('plot1download','Download Plot', class = "butt"),
          shinycssloaders::withSpinner(plotOutput("plot1"), type="2", color.background = "#ee6a50", color = "#ffffff"),
          div(h4("Correlation plot for normalized features (Spearman's)"),style = "color: black"),
          div(style="display:inline-block",downloadButton('plot2download','Download Plot', class = "butt")),
          div(style="display:inline-block", downloadButton('corrtable', 'Download Table', class = "butt")),
          # shinycssloaders::withSpinner(plotOutput("plot2"), type="2", color.background = "#ee6a50", color = "#ffffff"),
          withLoader(plotOutput("plot2"), type="html", loader="loader5"),
          #highchartOutput("plot1"),
          div(h4("Dynamical Persistent biomarker Structure"),style = "color: black"),
          downloadButton('plot3download','Download Plot', class = "butt"),
          # shinycssloaders::withSpinner(grVizOutput("plot3", width="100%", height="90vh"), type="2", color.background = "#ee6a50", color = "#ffffff"),
          withLoader(grVizOutput("plot3", width="100%", height="90vh"), type="html", loader="dnaspin")
                   )
          ),            
        )
   )
)

