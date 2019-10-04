library(shiny)
library (car)
library(lme4)
library(readr)
library(tidyverse)
library(sjPlot)
library(ggthemes)
library(ggplot2)
library(scales)
library(gridExtra)
library(psych)
library(summarytools)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

orig.data <- read.table(file="data/ET_lt_memory_AOIreport.txt", header=T, sep="\t")


all.data<-orig.data%>%
  filter(IA_DWELL_TIME__prop!=".")%>%
  filter(IA_AVERAGE_FIX_PUPIL_SIZE!=".")%>%
  filter(Subject!="lara")%>% #completed only session 2
  droplevels()%>%
  mutate(target = ifelse(location=="left" & IA_LABEL=="AOI_object_left","target",
                         (ifelse(location=="right" & IA_LABEL=="AOI_object_left","distractor",
                                 (ifelse(location=="right" & IA_LABEL=="AOI_object_right","target",
                                         (ifelse(location=="left" & IA_LABEL=="AOI_object_right","distractor",
                                                 (ifelse(location=="left" & IA_LABEL=="Left_half_display","target",
                                                         (ifelse(location=="right" & IA_LABEL=="Left_half_display","distractor",
                                                                 (ifelse(location=="right" & IA_LABEL=="Right_half_display","target",
                                                                         (ifelse(location=="left" & IA_LABEL=="Right_half_display","distractor",""
                                                                         ))))))))))))))))
table(all.data$target,all.data$IA_LABEL)
all.data$IA_DWELL_TIME__prop=as.numeric(as.character(all.data$IA_DWELL_TIME__prop))
all.data$IA_AVERAGE_FIX_PUPIL_SIZE=as.numeric(as.character(all.data$IA_AVERAGE_FIX_PUPIL_SIZE))




# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("ET_LT"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("IA_LABEL", "Variable:",
                  c("Hands" = "AOI_agent_hands",
                    "Head" = "AOI_agent_head",
                    "Right display" = "Right_half_display")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("ETPlot")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("all.data ~", input$IA_LABEL)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$ETPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = all.data,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)