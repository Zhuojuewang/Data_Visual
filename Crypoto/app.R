#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)

# Define UI for 
ui <- dashboardPage(
    dashboardHeader(title = "Cryptocurrency Dashboard"),
    #side bar menu
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
    ),
    dashboardBody(
        #basic price dashboard
        fluidRow(
            # A static valueBox
            infoBox("New Orders", 10 * 2, icon = icon("credit-card"),fill =FALSE),
            # Dynamic infoBoxes
            infoBoxOutput("progressBox"),
            infoBoxOutput("approvalBox")
            ),
        fluidRow(
            infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
            infoBoxOutput("progressBox2"),
            infoBoxOutput("approvalBox2")
        ),
        ),
    skin = c("black")
)

    

server <- function(input, output) {
    output$progressBox <- renderInfoBox({
        infoBox(
            "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
            color = "purple"
        )
    })
    output$approvalBox <- renderInfoBox({
        infoBox(
            "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    # Same as above, but with fill=TRUE
    output$progressBox2 <- renderInfoBox({
        infoBox(
            "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox2 <- renderInfoBox({
        infoBox(
            "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow", fill = TRUE
        )
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
