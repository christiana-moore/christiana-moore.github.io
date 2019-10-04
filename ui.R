require(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Average Component Interaction Effect"),
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Respondent Gender" = "Q1", 
                     "Respondent Political Party" = "Q8", 
                     "Respondent Ethnicity" = "Q3"))
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("gender")
  )
))

shinyApp(ui = ui, server = server)
