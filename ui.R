
# 1. Required libraries
library(shiny)

# 2. User interface definition
library(shiny)

shinyUI(
        pageWithSidebar(
            headerPanel("Predict next word..."),
            sidebarPanel(
                textInput("words", "Type some words..."),
                helpText("You can type more than two words, but the prediction will always be based on the last two..."),
                helpText("The first time loading the data takes a few seconds, please be patient...")),
            mainPanel(
                textOutput("recWords")
                )
        )
    )