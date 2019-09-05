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
cope <- na.omit(select(cope,c("X1","X10","X18","X22","X23"))) 
names(cope) <- c("light_level_for_paper_work","light_level_for_computer_work",
               "overall_lighting_quality","age","gender")
# Vector
for (i in 1:3){
    cope[,i] <- as.vector(cope[,i])
}

# Factor
for (i in 4:5){
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
  titlePanel("Visual Satisfaction Responses"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
      sidebarPanel(
            
        # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                    label = "Y-axis:",
                    choices = c("Satisfaction with light level for computer work" = "light_level_for_computer_work", 
                                "Satisfaction with light level for paper-based work" = "light_level_for_paper_work", 
                                "Satisfaction wiht overall lighting quality" = "overall_lighting_quality"), 
                    selected = "overall_lighting_quality"),
            
        # Select variable for x-axis ----------------------------------
       selectInput(inputId = "x", 
                    label = "X-axis:",
                    choices = c("Satisfaction with light level for computer work" = "light_level_for_computer_work", 
                                "Satisfaction with light level for paper-based work" = "light_level_for_paper_work", 
                                "Satisfaction wiht overall lighting quality" = "overall_lighting_quality"), 
                    selected = "light_level_for_computer_work"),
      
      # Enter text for plot title ---------------------------------------------
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as plot title")

    ),

    # Show a plot of the generated distribution
    mainPanel(
        
      # Box plot
      plotOutput("boxplot"),
      br(), br(),    # a little bit of visual separation
      
      # Bar chart_ gender
      plotOutput("barGender"),
      br(), br(),    # a little bit of visual separation      
      
      # Bar chart_ age
      plotOutput("barAge")
    )
  )
)

# Define server logic
server <- function(input, output) {

    output$barGender <- renderPlot({
        ggplot(cope) + geom_bar(aes(factor(gender))) +
            scale_x_discrete("Gender", labels = c("Female","Male"))
    })
    
    output$barAge <- renderPlot({
        ggplot(cope) + geom_bar(aes(factor(age))) +
            scale_x_discrete("Age", labels = c("18-29","30-39","40-49","50-59","60-69","70+"))
    })
    
    # Convert plot_title toTitleCase ----------------------------------
    pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
    
    # Show histogram
    output$boxplot <- renderPlot({
        ggplot(data = cope) +
          geom_boxplot(aes_string(input$x, input$y, group = input$x)) +
            scale_x_discrete(toTitleCase(str_replace_all(input$x, "_", " ")),
                              labels= axisTitles.7sat, limit = c("1","2","3","4","5","6","7")) +
            scale_y_continuous(toTitleCase(str_replace_all(input$y, "_", " ")),
                               breaks = c(1,2,3,4,5,6,7), label = axisTitles.7sat) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = pretty_plot_title())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


