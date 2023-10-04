# PerSEveML User Manual

# 1. Installation
## 1.1	Download/Install R
Go to https://www.r-project.org/ and follow the instructions to download the latest version of R.

## 1.2	Download/Install R Studio
Go to https://posit.co/download/rstudio-desktop/ and follow the instructions to install RStudio.

## 1.3 Download App Files from Github
Go to https://github.com/sreejatadutta/PerSEveML and download/clone all files from the Github repository.

## 1.4 Install Required R Packages
Install the required R packages by opening R/RStudio console and executing the following code via console:
>> install.packages("c("alookr","shiny","caret","'highcharter'","rpart","plyr","dplyr","pROC","xgboost","sqldf","tidyverse","DiagrammeR","DiagrammeRsvg","rsvg","matrixStats","adabag","klaR","ggplot2","plotly","magrittr","kernlab","shinyalert","shinyWidgets","shinythemes","shiny","shinyjs","waiter","rmarkdown","markdown","bslib","shinycustomloader","shinycssloaders","DiagrammeR","DiagrammeRsvg","rsvg","shinyalert"))

## 1.5	Run Application
Open "server.R", "ui.R", and "app.R" in RStudio (recommended) and click on the upper right corner "Run App". A new window should now appear.

# 2. Structure of App
## 2.1 Instruction Tab
This tab contains similar information as section 3 and 4 of this document. The purpose of this tab is to give users important app information that might come up while working on the app.

## 2.2 Computational Information Tab
This tab is dedicated to user preferences while running the app. All input information can be found in this tab.

## 2.3 Outputs Tab
PerSEveML generates two different kinds of outputs, tables and figures, that are all downloadable.

# 3. Explanation of inputs
## 3.1 Input file Options
1. The users have the option to select your own file in .csv, tab-delimited, excel and RDS format, or use one of the example data set, Nilsson rare, Mosmann rare, and SIN3 network.
2. Users can also select the binary response variable (formatted as 0 and 1) signifying rare events using "**Choose your response**" drop-down menu. 
3. Users can omit the variables that they do not want to be included in the data analysis using the "**Select unwanted variables**" option.

## 3.2 Normalization
The users have the option to select one of the following normalization techniques from the "**Normalization Technique**" option:

1. *Min-max*

2. *Log Scaling*

3. *Standard Scaling*

4. *Arcsine*

5. *TopS* 

6. *Percentage Row*

In case the user do not want to use the normalization option, they can click on "*No Normalization*" option. 

##3.3 Selecting Machine Learning Models for Rare Events
The user can select one or more machine learning (ML) algorithms based on your research question from the "**Choose preferred algorithms**" option. The options include:

* Tree-based algorithms:

1. *Decision Tree*
2. *Random Forest*
3. *XgBoost*
4. *AdaBoost*

* Non-tree based algorithms:

1. *Naive Bayes*
2. *Linear SVM*
3. *Non-linear SVM*
4. *Polynomial SVM*

* Linear classifiers:

1. *LDA*
2. *Logistic Regression*
3. *Lasso Regression*
4. *Ridge Regression*

## 3.4 Selecting Train-Test Percentage
The user can select can either enter or select any percentage between 0-100 to split the normalized/raw data into training and test sets using the "**Insert train-test ratio**" tab. Note that test set should have at least some data points for the app to run successfully.

## 3.5 Selecting the Value of *k* for Cross-Validation
This app heavily rely on the performance of the ML methods. Since the algorithm is based on hyper-parameter tuning using grid search or cross-validation, using an optimum value of *k* is crucial. This app allows the user to select the value of *k* between 1 to 10 in the "**Value of k for cross validation**" tab.

## 3.6 Selecting the Threshold for Cutpoint Analysis
Based on the user defined cut-point in the "**Insert cut-point analysis cutoff**", this app will formulate the persistent biomarker (or feature) structure. An user can iteratively run different combinations of normalizations, ML methods, and thresholds to come up with the most peristent structure.

## 3.7 Run the PerSEveML
After the user have successfullly selected the preferred input options, they can go ahead and click on the gree "Submit" button of the upper left corner.

## 3.8 Resetting the app
In case the user wants to reset the app, the user can click on the blue "Reset" button of the upper left corner located next to the submit button.

# 4. Explanation of outputs
## 4.1 Tables
1. Normalized data: The users can download the normalized data in .csv format and use it for further data exploration.
2. Entropy and rank scores: Entropy and rank scores are calculated using variable importance from individual machine learning models. The users can download/view this table to understand how each of the selected model contributed towards the scores.
3. Model metrics: This table help users to understand performance of the trained models. These metrices are calculated based on variable importance using caret package *varimp* function.
4. The users can also download the table for persistent feature structure showing which features are categorized as selected, unselected, and fluctuating.

## 4.2 Figures
1. Normalization: Boxplots to explore the data distribution across individual variables in the data set.
2. Correlation plot/table: This helps users understand the correlation among features in a data set.
3. Dynamical persistent structure: This plot is a visual representation of selected, unselected, and fluctuating features. 
