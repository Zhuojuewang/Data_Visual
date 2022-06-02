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
library(coinmarketcapr)




# sidebar def
sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput(inputId = "CryptoType",label = "Cryptocurrencies",
                choices = c("Bitcoin"),
                selected = "Bitcoin"
    ),
    selectInput(inputId = "Currency",label = "Currencies",
                choices = c("USD"),
                selected = "USD"
    ),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem("About", tabName = "About", icon = icon("pushpin",lib ="glyphicon"))
  )
)



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
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
            )
            
    ),
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    ),
    # member table 
    tabItem(tabName = "About",
            box( title = h1("Members"),status = "warning",solidHeader = TRUE, width = 12,
              box(
                h3("Zhuojue Wang"), br(), "he is abcde", width = 12
              ),
              box(
                h3("Xuanyu Chen"), br(), "he is abcde", width = 12
              ),
              box(
                h3("Xin Kang"), br(), "he is abcde", width = 12
              ),
              box(
                h3("Bowen Tan"), br(), "he is abcde", width = 12
              ),
              box(
                h3("Yitong Fu"), br(), "he is abcde", width = 12
              )
            )
    )
  )
)



# Define UI for 
ui <- dashboardPage(
    dashboardHeader(title = span(tagList(icon("bitcoin"),"Cryptocurrency Dashboard")),
                    titleWidth = 350),
    #side bar menu
    sidebar,
    body,
    skin = c("red"),
)

    

server <- function(input, output, session) {
  
  # connected to server
  key ="33c270b5-e528-44fd-8443-0b4493f76019"
  coinmarketcapr::setup(key)
  
  # fill the sidebar Input CryptoType
  observeEvent(
    input$CryptoType,
    updateSelectInput(session, "CryptoType", label = "Cryptocurrency Name",
                      choices = coinmarketcapr::get_crypto_listings() %>% select(name) %>% as.list() %>% unlist(use.names=FALSE),
                      selected = input$CryptoType))
  # fill the sidebar Input Currencies
  observeEvent(
    input$Currency,
    updateSelectInput(session, "Currency", label = "Currency Name",
                      choices = c(coinmarketcapr::get_valid_currencies()),
                      selected = input$Currency))

  
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
