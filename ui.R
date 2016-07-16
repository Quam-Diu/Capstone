
# 1. Required libraries
library(shiny)

# 2. User interface definition
library(shiny)

shinyUI(
        pageWithSidebar(
            headerPanel("Predict next word..."),
            sidebarPanel(textInput("words", "Type one or two words...")),
            mainPanel( textOutput("recWords"))
        )
    )