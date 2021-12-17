###################################
#    Liver Disease Prediction     #
#         Web aplication          #
#                                 #
#  By Santiago Gonzalez Berruga   #  
###################################


# Load packages
library(shiny)
library(shinydashboard)
# library(shinyjs) # we use shinyjs:: in the code
library(shinyFeedback)
library(shinyalert)
library(caret)
library(recipes)
library(dplyr)
library(ggplot2)
library(plotly)
library(rlang)
library(waiter)

# Data and machine learning model
load("data/train_models.RData")


# Load R scripts:
source("scripts/Script_Graficos_Web.R")


# Required variables to make a prediction:
variables <- c("Id","Age","Gender","TB","DB","Alkphos","Sgpt","Sgot","TP","ALB","AG")


# Render report function:
render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}



# User Interface (UI)
ui <- dashboardPage(
  
  # General theme for the app
  skin = "blue",
  
  # Header
  dashboardHeader(
    
    # Title
    title = "Liver Disease Prediction", 
    titleWidth = 300,
    
    # Help button
    tags$li(class = "dropdown", actionLink("help", "Help")),
    
    # Download report button
    tags$li(class = "dropdown", shinyjs::hidden(downloadLink("downloadReport", "Generate report")))
    
  ), # dashboardHeader final
  
  
  
  # Side Bar
  dashboardSidebar(
    
    # Width of the sidebar in pixels
    width = 300,
    
    # Form
    div(
      # Link for app information and templates
      p(class="p-form", "Information and templates", 
        actionLink("info",  label = "", icon = icon("info-circle")))
    ),
    
    # STEP 1: Type of prediction
    radioButtons("radioPred", label = "1. Number of predictions:",
                 inline = TRUE,
                 choices = list("Unique" = "unique", 
                                "Multiple" = "multiple"), 
                 selected = "unique"),
    
    # STEP 2: Upload data
    conditionalPanel('input.radioPred == "unique"',
                     
                     radioButtons("radioData", label = "2. Upload data:",
                                  inline = TRUE,
                                  choices = list("CSV file" = "file",
                                                 "Manually" = "manual"), 
                                  selected = "file")
    ),
    
    
    div(id = "form",
        
        # Load a CSV file
        conditionalPanel('input.radioPred == "multiple"  || (input.radioPred == "unique"  && input.radioData == "file")',
                         
                         fileInput("DataFile", "Choose CSV File:",
                                   accept = ".csv")
        ),
        
        # Load data manually
        conditionalPanel('input.radioPred == "unique"  && input.radioData == "manual"',
                         
                         # Fullfill the form:
                         div(id="manual-form",
                             
                             textInput("Id", label = "ID:", placeholder = "For example: 1, sample1,..."),
                             numericInput("Age", label = "Age:", value = NULL, min = 1, max = 130),
                             
                             selectInput("Gender", label = "Gender:",
                                         choices = list("Select a gender" = "","Male"="Male", "Female"="Female"),
                                         selected = ""),
                             
                             numericInput("TB", label = "Total Bilirubin (mg/dL):", value = NULL, min = 0, max = NA),
                             numericInput("DB", label = "Direct Bilirubin (mg/dL):", value = NULL, min = 0, max = NA),
                             numericInput("Alkphos", label = "Alkaline Phosphotase (units/L):", value = NULL, min = 0, max = NA),
                             numericInput("Sgpt", label = "Alamine Aminotransferase (units/L):", value = NULL, min = 0, max = NA),
                             numericInput("Sgot", label = "Aspartate Aminotransferase (units/L):", value = NULL, min = 0, max = NA),
                             numericInput("TP", label = "Total Protein (g/dL):", value = NULL, min = 0, max = NA),
                             numericInput("ALB", label = "Albumin (g/dL):", value = NULL, min = 0, max = NA),
                             numericInput("AG", label = "Albumin (g/dL) and Globulin (g/dL) Ratio:", value = NULL, min = 0, max = NA)
                         )
        )
    ),
    
    div(class="form-buttons",
        actionButton("clear", "Clear"),             # Clear form button
        actionButton("submit", "Make prediction")   # Submit button
    )
    
  ), # dashboardSidebar final
  
  
  
  
  # Body
  dashboardBody(
    
    # header HTML tag
    tags$head(
      tags$link(rel="icon", href="favicon.svg"), # favicon
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # CSS
    ),
    
    
    # This function must be called from a Shiny app's UI in order for all other shinyjs functions to work.
    shinyjs::useShinyjs(),
    
    # This function must be called from a Shiny app's UI in order for all other shinyalert functions to work.
    shinyalert::useShinyalert(),
    
    # This function must be called from a Shiny app's UI in order for all other waiter functions to work.
    waiter::useWaitress(),
    
    # This function must be called from a Shiny app's UI in order for all other shinyFeedback functions to work.
    shinyFeedback::useShinyFeedback(), 
    
    div (class="boxMain",
         
         # Predictions results
         fluidRow(
           box(title = "Prediction", status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width=12,
               
               uiOutput("resultsUI")
           )
         ),
         
         
         # Data Tables
         fluidRow(
           box(title = "Data", status = "primary", solidHeader = TRUE,
               collapsible = TRUE, width=12,
               
               uiOutput("tablesUI") 
           )
         ),
         
         
         # Graphs of data
         fluidRow(
           box(title = "Distplots", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               
               uiOutput("distplotUI")
           ),
           
           box(title = "Boxplots", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               
               uiOutput("boxplotUI")
           )
         ),
         
         fluidRow(
           box(title = "Pie Plots", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               
               uiOutput("piechartUI")
           )
         )
    ),
    
    tags$footer(p("Liver Disease Prediction by Santiago González Berruga"),
                div(class="footerLinks",
                    actionLink("linkedin", label= a(href="https://www.linkedin.com/in/santiagogonzalezberruga", 
                                                    div(class="linkedinLink" ,icon("linkedin"), p("santiagogonzalezberruga")), target="_blank")),
                    actionLink("github",   label= a(href="https://github.com/santigberruga", 
                                                    div(icon("github"),   p("santigberruga")), target="_blank"))
                ),
                HTML('<div class="CreativeCommons"><a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">
                     <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" />
                     </a>This work is licensed under a<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>
                     </div>'),
    )
    
  ) # dashboardBody final
  
  
) # UI final


# Define server
server <- function(input, output) {
  
  
  #################
  #  Manual form  #
  #  Validations  #
  #################
  
  # Check success and warmings
  
  ## Input ID
  observeEvent(input$Id, {    ## Input ID
    if (nchar(input$Id) > 0) {
      hideFeedback("Id")
      showFeedbackSuccess(
        inputId = "Id"
      )
    } else {
      hideFeedback("Id")
    }
  })
  
  ## Input Age
  observeEvent(input$Age, {
    if (!is.na(input$Age) & input$Age >= 1 & input$Age <= 130) {
      hideFeedback("Age")
      showFeedbackSuccess(
        inputId = "Age"
      )
    } else if (!is.na(input$Age) & (input$Age < 1 | input$Age > 130)) {
      hideFeedback("Age")
      showFeedbackWarning(
        inputId = "Age",
        text = "Must be between 1 and 130"
      )
    } else {
      hideFeedback("Age")
    }
  })
  
  ## Input Gender
  observeEvent(input$Gender, {
    if (input$Gender != "") {
      hideFeedback("Gender")
      showFeedbackSuccess(
        inputId = "Gender"
      )
    } else {
      hideFeedback("Gender")
    }
  })
  
  ## Input TB
  observeEvent(input$TB, {
    if (!is.na(input$TB) & input$TB >= 0) {
      hideFeedback("TB")
      showFeedbackSuccess(
        inputId = "TB"
      )
    } else if (!is.na(input$TB) & input$TB < 0) {
      hideFeedback("TB")
      showFeedbackWarning(
        inputId = "TB",
        text = "Must be positive"
      )
    } else {
      hideFeedback("TB")
    }
  })
  
  ## Input DB
  observeEvent(input$DB, {
    if (!is.na(input$DB) & input$DB >= 0) {
      hideFeedback("DB")
      showFeedbackSuccess(
        inputId = "DB"
      )
    } else if (!is.na(input$DB) & input$DB < 0) {
      hideFeedback("DB")
      showFeedbackWarning(
        inputId = "DB",
        text = "Must be positive"
      )
    } else {
      hideFeedback("DB")
    }
  })
  
  ## Input Alkphos
  observeEvent(input$Alkphos, {
    if (!is.na(input$Alkphos) & input$Alkphos >= 0) {
      hideFeedback("Alkphos")
      showFeedbackSuccess(
        inputId = "Alkphos"
      )
    } else if (!is.na(input$Alkphos) & input$Alkphos < 0) {
      hideFeedback("Alkphos")
      showFeedbackWarning(
        inputId = "Alkphos",
        text = "Must be positive"
      )
    } else {
      hideFeedback("Alkphos")
    }
  })
  
  ## Input Sgpt
  observeEvent(input$Sgpt, {
    if (!is.na(input$Sgpt) & input$Sgpt >= 0) {
      hideFeedback("Sgpt")
      showFeedbackSuccess(
        inputId = "Sgpt"
      )
    } else if (!is.na(input$Sgpt) & input$Sgpt < 0) {
      hideFeedback("Sgpt")
      showFeedbackWarning(
        inputId = "Sgpt",
        text = "Must be positive"
      )
    } else {
      hideFeedback("Sgpt")
    }
  })
  
  ## Input Sgot
  observeEvent(input$Sgot, {
    if (!is.na(input$Sgot) & input$Sgot >= 0) {
      hideFeedback("Sgot")
      showFeedbackSuccess(
        inputId = "Sgot"
      )
    } else if (!is.na(input$Sgot) & input$Sgot < 0) {
      hideFeedback("Sgot")
      showFeedbackWarning(
        inputId = "Sgot",
        text = "Must be positive"
      )
    } else {
      hideFeedback("Sgot")
    }
  })
  
  ## Input TP
  observeEvent(input$TP, {
    if (!is.na(input$TP) & input$TP >= 0) {
      hideFeedback("TP")
      showFeedbackSuccess(
        inputId = "TP"
      )
    } else if (!is.na(input$TP) & input$TP < 0) {
      hideFeedback("TP")
      showFeedbackWarning(
        inputId = "TP",
        text = "Must be positive"
      )
    } else {
      hideFeedback("TP")
    }
  })
  
  ## Input ALB
  observeEvent(input$ALB, {
    if (!is.na(input$ALB) & input$ALB >= 0) {
      hideFeedback("ALB")
      showFeedbackSuccess(
        inputId = "ALB"
      )
    } else if (!is.na(input$ALB) & input$ALB < 0) {
      hideFeedback("ALB")
      showFeedbackWarning(
        inputId = "ALB",
        text = "Must be positive"
      )
    } else {
      hideFeedback("ALB")
    }
  })
  
  ## Input AG
  observeEvent(input$AG, {
    if (!is.na(input$AG) & input$AG >= 0) {
      hideFeedback("AG")
      showFeedbackSuccess(
        inputId = "AG"
      )
    } else if (!is.na(input$AG) & input$AG < 0) {
      hideFeedback("AG")
      showFeedbackWarning(
        inputId = "AG",
        text = "Must be positive"
      )
    } else {
      hideFeedback("AG")
    }
  })
  
  
  # Check required inputs
  observeEvent(input$submit, {
    
    if (input$radioPred == "unique" & input$radioData == "manual") {
      
      if (nchar(input$Id) == 0) { ## Input ID
        showFeedbackDanger(
          inputId = "Id",
          text = ""
        )
      }
      
      if (is.na(input$Age)) { ## Input Age
        showFeedbackDanger(
          inputId = "Age",
          text = ""
        )
      }
      
      if (input$Gender == "") { ## Input Gender
        showFeedbackDanger(
          inputId = "Gender",
          text = ""
        )
      }
      
      if (is.na(input$TB)) { ## Input TB
        showFeedbackDanger(
          inputId = "TB",
          text = ""
        )
      }
      
      if (is.na(input$DB)) { ## Input DB
        showFeedbackDanger(
          inputId = "DB",
          text = ""
        )
      }
      
      if (is.na(input$Alkphos)) { ## Input Alkphos
        showFeedbackDanger(
          inputId = "Alkphos",
          text = ""
        )
      }
      
      if (is.na(input$Sgpt)) { ## Input Sgpt
        showFeedbackDanger(
          inputId = "Sgpt",
          text = ""
        )
      }
      
      if (is.na(input$Sgot)) { ## Input Sgot
        showFeedbackDanger(
          inputId = "Sgot",
          text = ""
        )
      }
      
      if (is.na(input$TP)) { ## Input TP
        showFeedbackDanger(
          inputId = "TP",
          text = ""
        )
      }
      
      if (is.na(input$ALB)) { ## Input ALB
        showFeedbackDanger(
          inputId = "ALB",
          text = ""
        )
      }
      
      if (is.na(input$AG)) { ## Input AG
        showFeedbackDanger(
          inputId = "AG",
          text = ""
        )
      }
    }
  })
  
  
  
  
  ##################
  #  Storage Data  #
  ##################
  
  # Reactivevaules for data storage
  storageData <- reactiveValues(data = NULL)
  
  # Save data
  observeEvent(input$submit, {
    
    if (input$radioPred == "multiple" | (input$radioPred == "unique" & input$radioData == "file")) {
      # From CSV file
      req(input$DataFile)
      
      storageData$data <- read.csv(input$DataFile$datapath)
      
    } else if (input$radioPred == "unique" & input$radioData == "manual") {
      # Load data manually
      req(input$Id,
          input$Age,
          input$Gender,
          input$TB,
          input$DB,
          input$Alkphos,
          input$Sgpt,
          input$Sgot,
          input$TP,
          input$ALB,
          input$AG) 
      
      storageData$data <- data.frame(Id      = input$Id,
                                     Age     = input$Age,
                                     Gender  = input$Gender,
                                     TB      = input$TB,
                                     DB      = input$DB,
                                     Alkphos = input$Alkphos,
                                     Sgpt    = input$Sgpt,
                                     Sgot    = input$Sgot,
                                     TP      = input$TP,
                                     ALB     = input$ALB,
                                     AG      = input$AG)
    }
  })
  
  
  
  #################
  #    General    #
  #  Validations  #
  #################
  
  observeEvent(input$submit, {
    
    # 1) Check that all variables are present
    req(storageData$data)
    
    var_error <- NULL
    
    for (i in variables) {
      if (!(i %in% names(storageData$data))) {
        var_error <- c(var_error, i)
      }
    }
    
    if (!is.null(var_error)) {
      var_error <- paste0("Variable/s required: ", paste(var_error, collapse = ", "),".")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problems:",
                   br(),
                   tags$ul(
                     tags$li(var_error)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    }
    
    
    # 2) Check NAs
    req(storageData$data)
    
    nas <- apply(storageData$data, 2, function(x){sum(is.na(x))}) # NAs per vararible
    nas_var <- NULL
    
    for (i in variables) {
      if (nas[[i]] > 0) {
        nas_var <- c(nas_var, i)
      }
    }
    
    if (!is.null(nas_var)) {
      nas_var <- paste0("Missing values in Variable/s: ", paste(nas_var, collapse = ", "), ".")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(nas_var)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    }
    
    
    
    # 3) Check the variable type (numeric or character)
    #    At this point we don't have to check the Id variable 
    #    because it can be character or numeric.
    req(storageData$data)
    
    type_var1 <- NULL # Check numeric variables
    type_var2 <- NULL # Check character variables (only Gender)
    
    for (i in variables) {
      if (i != "Id" & i != "Gender" & !is.numeric(storageData$data[,i])) {
        type_var1 <- c(type_var1, i)
      }
      
      if (i == "Gender" & !is.character(storageData$data[,i])) {
        
        type_var2 <- c("Variable Gender must be character with labels Female/Male.")
        
      } 
      else if (i == "Gender" & is.character(storageData$data$Gender)) {
        
        gender_levels <- sort(unique(storageData$data$Gender))
        
        if (length(gender_levels) > 2) {
          type_var2 <- c("Variable Gender must be character with labels Female/Male.")
          
        } else if (length(gender_levels) == 2 & (gender_levels[1] != "Female" | gender_levels[2] != "Male")) {
          
          type_var2 <- c("Variable Gender must be character with labels Female/Male.")
          
        } else if (length(gender_levels) == 1 & !(gender_levels[1] %in% c("Female","Male"))) {
          
          type_var2 <- c("Variable Gender must be character with labels Female/Male.")
        }
      }
    }
    
    
    if (!is.null(type_var1)) {
      type_var1 <- paste0("Variable/s: ", paste(type_var1, collapse = ", "), " must be numeric.")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(type_var1)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    } else if (!is.null(type_var2)) {
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(type_var2)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
    }
    
    
    
    # 4) Check Id not repeated
    req(storageData$data)
    
    rep_id <- NULL
    
    if (length(unique(storageData$data$Id)) != length(storageData$data$Id)) {
      rep_id <- names(which(table(storageData$data$Id)>1))
    }
    
    if (!is.null(rep_id)) {
      rep_id <- paste0("Repeated IDs: ", paste(rep_id, collapse = ", "), ".")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(rep_id)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    }
    
    
    # 5) Check variable range
    req(storageData$data)
    
    range_age <- NULL # Check Variable Age Range
    range_var <- NULL # Check range of the rest of numeric variables
    
    for (i in variables) {
      if (i == "Age" & ((TRUE %in% unique(names(table(storageData$data[,i] < 1)))) | (TRUE %in% unique(names(table(storageData$data[,i] > 130)))))) {
        range_age <- "Age must be between 1 and 130 years"
      }
      
      if (i != "Age" & i != "Id" & i != "Gender" & (TRUE %in% unique(names(table(storageData$data[,i]< 0))))) {
        range_var <- c(range_var, i)
      }
    }
    
    
    if (!is.null(range_age)) {
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(range_age)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    } else if (!is.null(range_var)) {
      range_var <- paste0("Variable/s: ", paste(range_var, collapse = ", "), " must be greater than 0.")
      
      # Pop up alert
      shinyalert(
        title = "Check input file",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   "Please fix the following problem:",
                   br(),
                   tags$ul(
                     tags$li(range_var)
                   )
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      storageData$data <- NULL
      
    } else {
      # Success: Data is correct
      
      # Pop up alert
      shinyalert(
        title = "Prediction in progress",
        text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                   
                   "Possible results:",
                   br(),
                   tags$ul(
                     tags$li("Liver Disease patient (LD)"),
                     tags$li("Non Liver Disease patient (H)")
                   ),
                   br(),
                   "Plots:",
                   br(),
                   tags$ol(
                     tags$li("They graph the information of the data used for the training of the predictive model."),
                     tags$li("They overlap the data used for the prediction making it easy to understand the results."),
                     tags$li("You can hover over the plots to get more information about each point.")
                   )
        ),
        size = "m",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
    }
    
  })
  
  
  
  
  ####################
  #  Data structure  #
  ####################
  
  # Transform variables correctly
  observeEvent(input$submit, {
    req(storageData$data)
    
    # Any patient whose age exceeded 89 is listed as being of age 90.
    storageData$data$Age    <- ifelse(storageData$data$Age<=89,storageData$data$Age,90)
    
    # Gender must be a factor with two levels (Female/Male)
    storageData$data$Gender <- factor(storageData$data$Gender, levels = c("Female","Male"))
  })
  
  
  
  
  ################
  #  Clear data  #
  ################
  
  # Reset form when changing  RadioButtons
  observeEvent(input$radioPred, {
    shinyjs::reset("form")
  })  
  
  observeEvent(input$radioData, {
    shinyjs::reset("form")
  })  
  
  # Clear button for all imputs
  observeEvent(input$clear, {
    shinyjs::reset("form")
  })
  
  
  
  
  ################
  # Prediction/s #
  ################
  
  # Make the prediction
  prediction <- eventReactive(input$submit, {
    req(storageData$data)
    
    # Data preprocesing
    data_trained <- bake(trained_recipe, new_data = storageData$data)
    
    # Prediction/s
    predict(modelo_ann, newdata = data_trained, type = "raw")
  })
  
  # Dataframe with data and predictions
  data_pred <-eventReactive(input$submit, {
    req(prediction())
    
    storageData$data %>%
      mutate(Class = factor(prediction(), levels = c("LD","H")))
  })
  
  
  
  
  ####################
  #    hide/show     #
  #  Report Button   #
  ####################
  
  # Show
  observeEvent(input$submit, {
    req(storageData$data, prediction(), data_pred())
    
    shinyjs::show("downloadReport") 
  })
  
  # Hide
  observeEvent(input$submit, {
    if (is.null(storageData$data)) {
      shinyjs::hide("downloadReport") 
    }
  })
  
  
  ###########
  # Outputs #
  ###########
  
  # Data and predictions:
  observeEvent(input$submit,{
    req(prediction(), data_pred())
    
    # Depending on input$radioPred we'll generate 
    # different UI components and outputs:
    
    if (input$radioPred == "unique") { # Unique prediction
      
      output$resultsUI <- renderUI({
        textOutput("uni_class_predict")
      })
      
      output$tablesUI <- renderUI({
        tableOutput("unitable")
      })
      
      # Result unique
      output$uni_class_predict <- renderText({paste("The result of the prediction is:", prediction()[1])})
      
      # Table unique
      output$unitable <- renderTable({storageData$data[1,]})
      
    } else { # Multiple prediction
      
      output$resultsUI <- renderUI({
        req(prediction(), data_pred()) # So that when giving an error the option to download data also disappears
        div(
          DT::dataTableOutput("multi_class_predict"),
          br(),
          downloadButton("downloadPred", "Download results")
        )
      })
      
      output$tablesUI <- renderUI({
        DT::dataTableOutput("multitable")
      })
      
      # Results multiple
      output$multi_class_predict <- DT::renderDataTable({data_pred()},
                                                        rownames= FALSE)
      
      # Table multiple
      output$multitable <- DT::renderDataTable({storageData$data},
                                               rownames= FALSE)
      
      # Download results multiple
      output$downloadPred <- downloadHandler(
        filename = function() {
          paste0(gsub(".csv", "", input$DataFile$name), "_Predictions.csv")
        },
        content = function(file) {
          write.csv(data_pred(), file, row.names = FALSE)
        }
      )
    }
  })
  
  
  
  
  # Plots UI
  
  ## Distplot UI (Histogram + Rug)
  output$distplotUI <- renderUI({
    req(data_pred())
    
    div(
      selectInput("var1", label = NULL,
                  choices = list("Select a variable" = "",
                                 "Age"="Age", 
                                 "TB"="TB",
                                 "DB"="DB", 
                                 "Alkphos"="Alkphos",
                                 "Sgpt"="Sgpt", 
                                 "Sgot"="Sgot",
                                 "TP"="TP",
                                 "ALB"="ALB",
                                 "AG"="AG"),
                  selected = "Age"),
      
      plotlyOutput("distplot")
      
    )
  })
  
  ## Boxplot UI
  output$boxplotUI <- renderUI({
    req(data_pred())
    
    div(
      selectInput("var2", label = NULL,
                  choices = list("Select a variable" = "",
                                 "Age"="Age", 
                                 "TB"="TB",
                                 "DB"="DB", 
                                 "Alkphos"="Alkphos",
                                 "Sgpt"="Sgpt", 
                                 "Sgot"="Sgot",
                                 "TP"="TP",
                                 "ALB"="ALB",
                                 "AG"="AG"),
                  selected = "Age"),
      
      plotlyOutput("boxplot")
      
    )
  })
  
  ## Pie chart UI
  output$piechartUI <- renderUI({
    req(data_pred())
    
    div(
      selectInput("var3", label = NULL,
                  choices = list("Select a variable" = "",
                                 "Gender" = "Gender", 
                                 "Class"  = "Class"),
                  selected = "Gender"),
      
      plotlyOutput("pie_chart")
      
    )
  })
  
  
  
  # Plots outputs
  
  ## Distplot output
  output$distplot <- renderPlotly({
    req(data_pred(), input$var1)
    
    # Arguments distplot
    args1 <- switch(input$var1,
                    "Age"     = list(data_pred(), sym("Age"), "Age"),
                    "TB"      = list(data_pred(), sym("TB"), "TB"),
                    "DB"      = list(data_pred(), sym("DB"), "DB"),
                    "Alkphos" = list(data_pred(), sym("Alkphos"), "Alkphos"),
                    "Sgpt"    = list(data_pred(), sym("Sgpt"), "Sgpt"),
                    "Sgot"    = list(data_pred(), sym("Sgot"), "Sgot"),
                    "TP"      = list(data_pred(), sym("TP"), "TP"),
                    "ALB"     = list(data_pred(), sym("ALB"), "ALB"),
                    "AG"      = list(data_pred(), sym("AG"), "AG"))
    
    # Distplot
    do.call(plot_distplot, args1)
  })
  
  ## Boxplot output
  output$boxplot <- renderPlotly({
    req(data_pred(), input$var2)
    
    # Arguments Boxplot
    args2 <- switch(input$var2,
                    "Age"     = list(data_pred(), sym("Age"), "Age"),
                    "TB"      = list(data_pred(), sym("TB"), "TB"),
                    "DB"      = list(data_pred(), sym("DB"), "DB"),
                    "Alkphos" = list(data_pred(), sym("Alkphos"), "Alkphos"),
                    "Sgpt"    = list(data_pred(), sym("Sgpt"), "Sgpt"),
                    "Sgot"    = list(data_pred(), sym("Sgot"), "Sgot"),
                    "TP"      = list(data_pred(), sym("TP"), "TP"),
                    "ALB"     = list(data_pred(), sym("ALB"), "ALB"),
                    "AG"      = list(data_pred(), sym("AG"), "AG"))
    
    # Boxplot
    do.call(plot_boxplot, args2)
  })
  
  ## Pie chart output
  output$pie_chart <- renderPlotly({
    req(data_pred(), input$var3)
    
    if (input$var3 == "Gender") {
      
      # Pie chart Gender
      plot_gender(data_pred()) 
      
    } else if (input$var3 == "Class") {
      
      # Pie chart Class
      plot_class(data_pred()) 
      
    }
  })
  
  
  
  
  ############
  #  Report  #
  ############
  
  # Rmarkdown Report
  output$downloadReport <- downloadHandler(
    
    filename = "Liver Disease Prediction Report.pdf",
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir())
      my_files <- list.files("reports")
      file.copy(paste0("reports/", my_files), tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      if (nrow(data_pred()) > 1) {
        markdown <- file.path(tempReport, "report_multiple.Rmd")
        
        params <- list(data_raw   = storageData$data,
                       data_pred  = data_pred())
        
      } else {
        markdown <- file.path(tempReport, "report_unique.Rmd")
        
        params <- list(prediction = prediction(),
                       data_raw   = storageData$data,
                       data_pred  = data_pred())
      }
      
      # Notification of rendering report
      notify <- showNotification(
        "Rendering report...",
        duration = NULL,
        closeButton = TRUE,
        type = "warning"
      )
      on.exit(removeNotification(notify), add = TRUE)
      
      
      # Render report and download progress bar
      withProgressWaitress({
        
        for (i in 1:10) {
          
          incProgressWaitress(2)
          
          if (i == 6) {
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            callr::r(render_report,
                     list(input = markdown, output = file, params = params))
          } else {
            Sys.sleep(0.25)
          }
        }
      }, selector = "#downloadReport", max = 15, theme = "overlay-percent")
    })
  
  
  
  
  
  ##########
  #  Info  #
  ##########
  
  # Pop up with info
  observeEvent(input$info, {
    shinyalert(
      title = "Information and templates",
      text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                 
                 "How works this app?",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Web_Aplication_Guide.pdf", "Web Aplication Guide", download=NA, target="_blank"))
                 ),
                 br(),
                 "Templates and examples of", tags$em("CSV"), "files:",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Example_unique_pred.csv", "Example for unique predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Example_multiple_pred.csv", "Example for multiple predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Template_pred.csv", "Template for predictions", download=NA, target="_blank"))
                 )
      ),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Ok",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
  
  
  ##########
  #  Help  #
  ##########
  
  # Pop up with info
  observeEvent(input$help, {
    shinyalert(
      title = "Information about the app",
      text = div(style = "width: 83%;
                          text-align: left;
                          margin: auto;",
                 
                 "How works this app?",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Web_Aplication_Guide.pdf", "Web Aplication Guide", download=NA, target="_blank"))
                 ),
                 br(),
                 "Templates and examples of", tags$em("CSV"), "files:",
                 br(),
                 tags$ul(
                   tags$li(a(href="info_files/Example_unique_pred.csv", "Example for unique predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Example_multiple_pred.csv", "Example for multiple predictions", download=NA, target="_blank")),
                   tags$li(a(href="info_files/Template_pred.csv", "Template for predictions", download=NA, target="_blank"))
                 ),
                 br(),
                 "Possible results:",
                 br(),
                 tags$ul(
                   tags$li("Liver Disease patient (LD)"),
                   tags$li("Non Liver Disease patient (H)")
                 ),
                 br(),
                 "Plots:",
                 br(),
                 tags$ol(
                   tags$li("They graph the information of the data used for the training of the predictive model."),
                   tags$li("They overlap the data used for the prediction making it easy to understand the results."),
                   tags$li("You can hover over the plots to get more information about each point.")
                 )
      ),
      size = "m", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Ok",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
}  # server final




# Run the application 

## Opcion 1
shinyApp(ui = ui, server = server)

