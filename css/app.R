require(cjoint)
require(shiny)
require(ggplot2)



server <- shinyServer(function(input, output, results){
  formulaText <- reactive({
    paste("Politician Gender ~", input$variable)
  })
  
  output$caption <- renderText({
    formulaText()
  })
    attribute_list <- list ()
    attribute_list [["Gender"]] <- c("Male", "Female")
    attribute_list [["Ethnicity"]] <- c("White","Asian","Hispanic or Latino","Black or African American","Native American or American Indian")
    attribute_list[["Political.Party"]] <- c("Democrat", "Republican", "Independent")
    attribute_list[["Years.of.Experience"]] <-c("4 years", "8 years", "10 years")
    attribute_list[["Policy Priority"]] <- c("Healthcare", "Economic Policy", "Foreign Policy")
    
    conjoint_design <- makeDesign(type="constraints", attribute.levels=attribute_list)
    
    df <- read.csv("conjoint_data.csv")
    
    
    df <- within(df, Ethnicity <- relevel(Ethnicity, ref = "White"))
    df <- within(df, Gender <- relevel(Gender, ref = "Male"))
    df <- within(df, Political.Party <- relevel(Political.Party, ref = "Independent"))     
    df <- within(df, Policy.Priority <- relevel(Policy.Priority, ref = "Economic Policy")) 
    df <- within(df, Years.of.Experience <- relevel(Years.of.Experience, ref = "8 years"))

output$sum <- renderPrint({
    df$x <- df[[input$variable]]
    results <- amce(selected ~ Gender + x + x:Gender + Ethnicity + Policy.Priority +
                      Political.Party + Years.of.Experience, data = df, design = conjoint_design, 
                    cluster = T, respondent.varying = "x", respondent.id = "Response.ID")  
    r <- summary(results)
    print(r)
})
    
output$gender <- renderPlot({
  df$x <- df[[input$variable]]
  results <- amce(selected ~ Gender + x + x:Gender + Ethnicity + Policy.Priority +
         Political.Party + Years.of.Experience, data = df, design = conjoint_design, 
       cluster = T, respondent.varying = "x", respondent.id = "Response.ID")  
print(results)
p <- plot(results, main="Conditional Average Marginal Component Effect", 
          xlab="Change in Pr(Politician Chosen As Credible)", label.baseline = T, plot.display="interaction", text.size = 9)
print(p)
}
)
}
)



ui <- shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Conditional Average Marginal Component Effect"),
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Respondent Gender" = "Q1", 
                     "Respondent Political Party" = "Q8", 
                     "Respondent Ethnicity" = "Q3",
                     "Respondent Age" = "Q2",
                     "Respondent Education" = "Q7")), width = 4
  ),
  
  mainPanel(
    h4("The conditional average marginal component effect represents the change in the probability of a male or female politician being chosen as credible, dependent on key respondent characteristics."),
    h5(textOutput("caption")),
    plotOutput("gender"),
    verbatimTextOutput("sum")
  )
))

shinyApp(ui = ui, server = server)



