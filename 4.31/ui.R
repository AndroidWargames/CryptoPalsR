library(shiny)

shinyUI(bootstrapPage(
        h3("URL components"),
        verbatimTextOutput("urlText"),

        h3("Parsed query string"),
        verbatimTextOutput("queryText")
))