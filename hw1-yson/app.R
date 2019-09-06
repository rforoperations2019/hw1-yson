library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tools)

# Set working directory
#setwd("~/Documents/GitHub/hw1-yson/hw1-yson")

# Read data
cope <- read.csv("COPE.csv", head = T) 

# Three visual satisfaction + Demographic
cope <- na.omit(select(cope,c("X1", "X6","X10", "X16","X18","X22","X23"))) 
names(cope) <- c("light_level_for_paper_work", "visual_privacy",
                 "light_level_for_computer_work", "access_to_a_seated_view",
                 "overall_lighting_quality","age","gender")
# Vector
for (i in 1:5){
    cope[,i] <- as.vector(cope[,i])
}

# Factor
for (i in 6:7){
    cope[,i] <- factor(cope[,i])
}

# Axis titles for plot
axisTitles.7sat <- c('1'='Very Dissatisfied','2'='Dissatisfied ',
                    '3'='Somewhat Dissatisfied', '4' = 'Neutral', 
                    '5' = 'Somewhat Satisfied', '6'='Satisfied',
                    '7'='Very Satisfied')

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Survey Results"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
      sidebarPanel(
            
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                   label = "Y-axis:",
                  choices = c("Satisfaction with light level for computer work" = "light_level_for_computer_work", 
                              "Satisfaction with seated view"= "access_to_a_seated_view",
                              "Satisfaction with visual privacy" = "visual_privacy",
                              "Satisfaction with light level for paper-based work" = "light_level_for_paper_work", 
                              "Satisfaction with overall lighting quality" = "overall_lighting_quality"), 
                  selected = "overall_lighting_quality"),
            
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Satisfaction with light level for computer work" = "light_level_for_computer_work",
                              "Satisfaction with seated view"= "access_to_a_seated_view", 
                              "Satisfaction with visual privacy" = "visual_privacy",
                              "Satisfaction with light level for paper-based work" = "light_level_for_paper_work", 
                              "Satisfaction with overall lighting quality" = "overall_lighting_quality"), 
                  selected = "light_level_for_computer_work"),
      
      # Enter text for plot title ---------------------------------------------
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as plot title"),
      
      # Select sample size ----------------------------------------------------
      sliderInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 50, max = nrow(cope), 
                   value = 100),
      
      # Show raw data
      checkboxInput(inputId = "view_data",
                    label = "View sampled data",
                    value = FALSE),
      
      # action button
      actionButton(inputId = "go",
                   label = "View data description!"),
      
      # download button
      downloadButton(outputId = "downloadData",
                     label = "Download current sampled data!")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # text
      verbatimTextOutput(outputId = "text"),
      
      # Box plot
      plotOutput(outputId = "boxplot"),
      br(), br(),    # visual separation
      
      # Bar chart
      fluidRow(
        column(6,plotOutput("barGender")
        ),
        column(6,plotOutput("barAge")
        )
      ),
      br(), br(),    # visual separation
      
      # Data table
      DT::dataTableOutput(outputId = "copetable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  #download data
  output$downloadData <- downloadHandler(
    filename = "surveydata.csv",
    content = function(file) {
      write.csv(cope_sample(), file)
    })
  
  # event
  observeEvent(input$go, {
    output$text <- renderText({"These are survey responses collected in office. 
      Workers were asked how they felt about visual quality."})
  })
  
  # Update the maximum allowed n_samp for selected type movies ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(100, nrow(cope)),
                       max = nrow(cope)
    )
  })
  
  # Create sampled dataframe
  cope_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(cope, input$n_samp)
  })  
  
  # show bar plot _ gender distribution 
  output$barGender <- renderPlot({
    ggplot(cope_sample()) + geom_bar(aes(factor(gender))) +
      scale_x_discrete("Gender", labels = c("Female","Male")) +
      labs(title = "Gender Distribution in Sampled Data")
  })
    
  # show bar plot _ age distribution
  output$barAge <- renderPlot({
    ggplot(cope_sample()) + geom_bar(aes(factor(age))) +
      scale_x_discrete("Age", labels = c("18-29","30-39","40-49","50-59","60-69","70+")) +
      labs(title = "Age Distribution in Sampled Data")
  })
    
  # Convert plot_title toTitleCase
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
    
  # Show boxplot
  output$boxplot <- renderPlot({
    ggplot(data = cope_sample()) +
      geom_boxplot(aes_string(input$x, input$y, group = input$x)) +
      scale_x_discrete(paste("Satisfaction with", toTitleCase(str_replace_all(input$x, "_", " "))),
                       labels= axisTitles.7sat, 
                       limit = c("1","2","3","4","5","6","7")) +
      scale_y_continuous(paste("Satisfaction with", toTitleCase(str_replace_all(input$y, "_", " "))),
                         breaks = c(1,2,3,4,5,6,7), label = axisTitles.7sat) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = pretty_plot_title())
    })
    
  # Print data table
  output$copetable <- DT::renderDataTable(
    if(input$view_data){
      DT::datatable(data = cope_sample(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)      
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


