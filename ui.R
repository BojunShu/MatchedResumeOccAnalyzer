library(shiny)
library(formattable)

fluidPage(
  #header
  titlePanel("Matched resume Occupation analyzer"),
  
  #input
  sidebarPanel(
    fileInput(
      inputId = "resume",
      "Upload your resume here",
      accept = c(".txt")
    ),
    helpText(".txt only"),
    submitButton("Match my resume"),
    br(),
    actionButton(
      inputId = "analyzer",
      label = " Most Frequency Words",
      width = "100%"
    ),
    br(),
    actionButton(
      inputId = "tech",
      label = "Hot Technology",
      width = "100%"
    ),
    br()
  ),
  
  #output
  mainPanel(tabsetPanel(
    type = "tabs",
    tabPanel("Matched Occupations", dataTableOutput("upload")),
    tabPanel("Words for your resume", dataTableOutput("word")),
    tabPanel("Hot Technology", plotOutput("cloud"))
  ),)
  
)
