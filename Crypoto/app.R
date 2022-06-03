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
    selectInput(inputId = "CryptoType",label = " Cryptocurrencies",
                choices = c("Bitcoin"),
                selected = "Bitcoin"
    ),
    selectInput(inputId = "Currency",label = "Currencies",
                choices = c("USD"),
                selected = "USD"
    ),
    #side bar name and icon
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard",lib ="font-awesome")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem("About", tabName = "About", icon = icon("pushpin",lib ="glyphicon"))
  )
)



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              # Dynamic infoBoxes
              infoBoxOutput("CurrencyNameBox"),
              infoBoxOutput("PriceBox"),
              infoBoxOutput("approvalBox")
            ),
            fluidRow(
              infoBoxOutput("TotalSupplyBox2"),
              infoBoxOutput("MarketCapBox2"),
              infoBoxOutput("VolumeBox2")
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
  
  # get data for all Box
  dataInputBox <- reactive({
    get_crypto_listings(currency = input$Currency) %>% filter(name == input$CryptoType) 
  })
  # Dashboard top box summary
  output$CurrencyNameBox <- renderInfoBox({
    infoBox("Currency Name", h3(input$CryptoType), 
            icon = icon("btc"),
            color = "blue"
    )
  })
  output$PriceBox <- renderInfoBox({
    infoBox("Price",  dataInputBox() %>% select(paste0(input$Currency,"_price")) %>% format(big.mark = ",", scientific = FALSE) %>% h3(), 
            icon = icon("usd",lib ="glyphicon"),
            color = "purple"
    )
  })
  
  #observe the change and change the icon direction accordingly
  obs_percent_change_24h <- reactive({dataInputBox() %>% select(paste0(input$Currency,"_percent_change_24h"))})
  
  output$approvalBox <- renderInfoBox({
    if(obs_percent_change_24h()>0){
      infoBox("Percent Change(24HR)", paste0(dataInputBox() %>% select(paste0(input$Currency,"_percent_change_24h")) %>% format(big.mark = ",", scientific = FALSE),"%") %>% h3(),
                             icon = icon("arrow-up"),
                             color = "yellow")
    } else{
      infoBox("Percent Change(24HR)", paste0(dataInputBox() %>% select(paste0(input$Currency,"_percent_change_24h")) %>% format(big.mark = ",", scientific = FALSE),"%") %>% h3(),
                              icon = icon("arrow-down"),
                              color = "yellow")
    }
  })
    
    
  # Same as above, but with fill=TRUE
  output$TotalSupplyBox2 <- renderInfoBox({
    infoBox("Total Supply", dataInputBox() %>% select(total_supply) %>% format(big.mark = ",", scientific = FALSE) %>% h3(),
            icon = icon("piggy-bank"),
            color = "blue", fill = TRUE
    )
  })
  output$MarketCapBox2 <- renderInfoBox({
    infoBox( "Market Cap", dataInputBox() %>% select(paste0(input$Currency,"_market_cap")) %>% format(big.mark = ",", scientific = FALSE) %>% h3(),
             icon = icon("tags", lib = "glyphicon"),
             color = "purple", fill = TRUE
    )
  })
  output$VolumeBox2 <- renderInfoBox({
      infoBox("Volume(24hr)",dataInputBox() %>% select(paste0(input$Currency,"_volume_24h")) %>% format(big.mark = ",", scientific = FALSE) %>% h3(),
              icon = icon("list-alt", lib = "glyphicon"),
              color = "yellow", fill = TRUE
    )
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
