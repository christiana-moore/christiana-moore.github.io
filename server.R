install.packages ("shiny",repos = "http://cran.us.r-project.org")
install.packages ("cjoint",repos = "http://cran.us.r-project.org")

library(cjoint)
library(shiny)


attribute_list <- list ()
attribute_list [["Gender"]] <- c("Male", "Female")
attribute_list [["Ethnicity"]] <- c("White","Asian","Hispanic or Latino","Black or African American","Native American or American Indian")
attribute_list[["Political Party"]] <- c("Democrat", "Republican", "Independent")
attribute_list[["Years of Experience"]] <-c("4 years", "8 years", "10 years")
attribute_list[["Policy Priority"]] <- c("Healthcare", "Economic Policy", "Foreign Policy")

conjoint_design <- makeDesign(type="constraints", attribute.levels=attribute_list)

df <- read.qualtrics ("conjoint.csv", new.format=T, responses= c("Q4", "Q21", "Q27", "Q32", "Q37"), 
                       covariates= c("Q1", "Q2", "Q3", "Q7", "Q8"), respondentID = "ResponseId")

colnames(df)[colnames(df)=="Political.Party"] <- "Political Party"
colnames(df)[colnames(df)=="Policy.Priority"] <- "Policy Priority"
colnames(df)[colnames(df)=="Years.of.Experience"] <- "Years of Experience"

df <- within(df, Ethnicity <- relevel(Ethnicity, ref = "White"))
df <- within(df, Gender <- relevel(Gender, ref = "Male"))
df <- within(df, `Political Party` <- relevel(`Political Party`, ref = "Independent"))     
df <- within(df, `Policy Priority` <- relevel(`Policy Priority`, ref = "Economic Policy")) 
df <- within(df, `Years of Experience` <- relevel(`Years of Experience`, ref = "8 years"))

#Cleaning
df <- df[nchar(df$Q1)>1,] 
df <- df[nchar(df$Q8)>1,] 
df$Q1 <- as.factor(df$Q1) 

df$Q8 <- as.character (df$Q8)
df$Q8[df$Q8=="Strong Democrat"] <- 1
df$Q8[df$Q8=="Not very strong Democrat"] <- 1
df$Q8[df$Q8=="Lean Democrat"] <- 1
df$Q8[df$Q8=="Independent"] <- 2
df$Q8[df$Q8=="Lean Republican"] <- 3
df$Q8[df$Q8=="Not very strong Republican"] <- 3
df$Q8[df$Q8=="Strong Republican"] <- 3
df$Q8[df$Q8==""] <- NA 
df$Q8 <- as.factor(df$Q8)
levels(df$Q8) <- c("Democrat", "Independent", "Republican")

results <- reactive({amce(reformulate(selected ~ Gender + input$selected + input$selected:Gender + Ethnicity + `Policy Priority` +
                        `Political.Party` + `Years.of.Experience`, data = df, design = conjoint_design, 
                       cluster = T, respondent.varying = input$selected, respondent.id = "Response.ID"))})

shinyServer(function(input, output){
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste("Gender ~", input$selected)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$gender <- renderPlot({
    plot(results, data = df)
  })
})

shinyApp(ui = ui, server = server)