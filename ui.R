
# 1. Required libraries
library(shiny)

# 2. User interface definition
shinyUI(
        
    fluidPage(
        # 2.1 Main title
        titlePanel("Next word prediction..."),
        sidebarLayout(
                # 2.2 User input
                sidebarPanel(
                    textInput('textInput', "Type one or two words...")
                ),
                # 2.3 Graphs and other outputs
                mainPanel(
                    textOutput("wordRec")
                )
            )
        )
)