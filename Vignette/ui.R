library(shiny)
library(MetExpR)

geny <- sort(levels(as.factor(test_expr_brca$id)))

shinyUI(fluidPage(
  titlePanel("Analiza wybranego genu"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "wybranygen", 
                  label = "Wybierz gen",
                  choices = geny,
                  selected = "BRCA1")
    ),
    mainPanel(
      plotOutput("road", height = "700px"),
      plotOutput("plot", height = "500px"),
      width = 20
    )
  )
))