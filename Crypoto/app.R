#
# This is a Shiny web application. You can run the application by clicking
# We work with two public api due to limited credits for each api
# API: CoinMarketCap
# API: HitBTC

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
# data manipulation and plot
library(tidyverse)
library(ggthemes)
library(ggforce)
# api package
library(coinmarketcapr)
library(cryptowatchR)
# connect to API tool
library(httr)
#library(jsonlite)
# work with Epoch time to UTC
library(lubridate)
#for word cloud
library(tm)
library(wordcloud)
library(memoise)
library(reshape2)





# Word Cloud function

# The list of valid books
subreddits <<- list("BITCOINBEGINNERS", "CRYPTOCURRENCIES", "CRYPTOMARKETS")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(subreddit) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(subreddit %in% subreddits))
     stop("Unknown subreddit")
  
  
  #allComments_df <- (sprintf("./%s.txt.gz", subreddit),encoding="UTF-8")
  
  allComments_df = find_thread_urls(subreddit=subreddit, sort_by="top") %>% as_tibble() %>% select(text) %>% unique()
  
  commentCorpus <- Corpus( VectorSource( allComments_df ) )
  #We pipe the corpus through several tm_map() methods
  commentCorpus <- commentCorpus %>%
    tm_map(removePunctuation) %>% ##eliminate punctuation
    tm_map(removeNumbers) %>% #no numbers
    tm_map(stripWhitespace) %>%#white spaces
    tm_map(tolower)%>% ##make all words lowercase
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("SMART")) %>%
    tm_map(removeWords, c("ive","dont"))
  
  #convert the corpus to a matrix to facilitate fursther analysis
  commentCorpus_mat <-as.matrix(TermDocumentMatrix( commentCorpus ))
  commentCorpus_wordFreq <-sort(rowSums(commentCorpus_mat), decreasing=TRUE)
})




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
    menuItem("Community", tabName = "Community", icon = icon("comment")),
    menuItem("About", tabName = "About", icon = icon("pushpin",lib ="glyphicon"))
  )
)



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2(strong("Cryptocurrency Dashborad"),align = "center"),
            br(),
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
            ),
            # 24 hr plot of currency change
            plotOutput("LinePlot_24hr", click = "plot_click"),
            
            # main content
            h2("Introduction"),
            h2("Research Questions"),
            p("Some Question"),
            h2("Data Source"),
            p("We work with data from two different API, CoinMarketCap and Cryptowatch."),
            
            
    ),
    tabItem(tabName = "widgets",
            h2("Widgets tab content"),
            # top currecy
            plotOutput("Top5MarketCap", click = "plot_click"),
            plotOutput("TreeMarketCap", click = "plot_click")

    ),
    tabItem(tabName = "Community",
            
            
            fluidPage(
              # Application title
              titlePanel("Reddit Top Comment Word Cloud"),
              
              sidebarLayout(
                # Sidebar with a slider and selection inputs
                sidebarPanel(
                  selectInput("selection", "Choose a Subreddit:",
                              choices = subreddits),
                  actionButton("update", "Change"),
                  hr(),
                  sliderInput("freq",
                              "Minimum Frequency:",
                              min = 1,  max = 50, value = 15),
                  sliderInput("max",
                              "Maximum Number of Words:",
                              min = 1,  max = 300,  value = 100)
                ),
                
                # Show Word Cloud
                mainPanel(
                  plotOutput("wordcloudplot")
                )
              )
            )
    ),
    # member table
    tabItem(tabName = "About",
      fluidRow(
        column(width = 6,
          h2("Team Members")
          )
      ),
      fluidRow(
        userBox(
          title = userDescription(
            title = "Zhuojue Wang",
            subtitle = "lead Developer",
            type = 2,
            image = "https://media-exp2.licdn.com/dms/image/C5603AQEKOqUz_wc8ag/profile-displayphoto-shrink_800_800/0/1541652423442?e=1660176000&v=beta&t=7GV52KgatnGfBTuBLSIFu_sqobhWZokAW4OqTJ7SRMg",
            backgroundImage = "https://love2dev.com/img/html-space-640x426.jpg"
          ),
          #status = "maroon",
          p("Zhuojue is a Master of Business Analytics Risk Management Candidate at Johns Hopkins Carey Business School. He received his bachelor degree from UC Davis with a double major in Statistics and Economics. He is passionate in machine learning applications in business analytics with causal analysis and on his way to become a researcher in the field.")
        ),
        userBox(
          title = userDescription(
            title = "Xuanyu Chen",
            subtitle = "lead Developer",
            type = 2,
            image = "https://media-exp2.licdn.com/dms/image/C5603AQGoVZTASX00hA/profile-displayphoto-shrink_400_400/0/1654536542969?e=1660176000&v=beta&t=hnYpWoYtwOxflCnPS5sVqzXhZfXJXVMbOUzf4GeNMTg",
            backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
          ),
          #status = "warning",
          p("Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. ")
        )
      ),
      fluidRow(
        userBox(
          title = userDescription(
            title = "Xin Kang",
            subtitle = "lead Developer",
            type = 2,
            image = "https://media-exp2.licdn.com/dms/image/C4E03AQHqkeN8PT-W0g/profile-displayphoto-shrink_400_400/0/1627371502253?e=1660176000&v=beta&t=w9-TpI7oZVcZfucwW5QRSv9xAu_yoETz0aQabgyy7RE",
            backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
          ),
          #status = "warning",
          p("Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. ")
        ),
        userBox(
          title = userDescription(
            title = "Bowen Tan",
            subtitle = "lead Developer",
            type = 2,
            image = "https://media-exp2.licdn.com/dms/image/C5603AQF1J_c3-GzCZQ/profile-displayphoto-shrink_400_400/0/1609620802392?e=1660176000&v=beta&t=daJtmVYNWtUuECVCXs3WApYx2gRzJaezvER44NlatjI",
            backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
          ),
          #status = "warning",
          p("Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. ")
        )
      ),
      fluidRow(
        userBox(
          title = userDescription(
            title = "Yitong Fu",
            subtitle = "lead Developer",
            type = 2,
            image = "https://media-exp2.licdn.com/dms/image/C4E03AQE_TGsVDLzI4Q/profile-displayphoto-shrink_400_400/0/1638992793893?e=1660176000&v=beta&t=qXG--KYg68dze5QG8FR-Oqky20CqG3R3VZiyYuPuLeY",
            backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
          ),
          #status = "warning",
          p("Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. ")
        )
      )
    )
    
    
    # tabItem(tabName = "About",
    #         box( title = h1("Members"),status = "warning",solidHeader = TRUE, width = 12,collapsible = TRUE,
    #           box(width = 12,
    #             fluidRow(column(width = 2, align = "center" ,img(src = "ZhuojueWang.jpg", height = 150, width = 100)),
    #                      column(width = 10, 
    #                             h3("Zhuojue Wang"),
    #                             p("Zhuojue is a Master of Business Analytics Risk Management Candidate at Johns Hopkins Carey Business School. He received his bachelor degree from UC Davis with a double major in Statistics and Economics. He is passionate in machine learning applications in business analytics with causal analysis and on his way to become a researcher in the field.") 
    #                       )
    #             
    #             )
    #           ),
    #           box(
    #             h3("Xuanyu Chen"), br(), "he is abcde", width = 12
    #           ),
    #           box(
    #             h3("Xin Kang"), br(), "he is abcde", width = 12
    #           ),
    #           box(
    #             h3("Bowen Tan"), br(), "he is abcde", width = 12
    #           ),
    #           box(
    #             h3("Yitong Fu"), br(), "he is abcde", width = 12
    #           )
    #         )
    # )
  )
)



# Define UI for 
ui <- dashboardPage(
    dashboardHeader(title = span(tagList(icon("bitcoin")," Cryptocurrency Dashboard")),
                    titleWidth = 350),
    #side bar menu
    sidebar,
    body,
    skin = c("red")
)

    

server <- function(input, output, session) {
  
  # connected to server
  key ="33c270b5-e528-44fd-8443-0b4493f76019"
  coinmarketcapr::setup(key)
  
  # list of not supported crypto
  list_Supported_Cryptocurrency <- c("BNB","Binance USD","UNUS SED LEO","Cronos","NEAR Protocol","FTX Token","VeChain","Hedera","KuCoin Token","Elrond","TrueUSD",
                                     "Theta Network","Helium","Huobi Token","Klaytn","eCash","BitTorrent-New","IOTA","Pax Dollar","Neo","Neutrino USD","PancakeSwap",
                                     "Stacks","USDD","Nexo","OKB","Zilliqa","Celo","Decred","Harmony","Amp","Arweave","NEM","XDC Network","Holo","Gate Token","Fei USD","Bitcoin Gold","Kadena")
  # fill the sidebar Input CryptoType
  observeEvent(
    input$CryptoType,
    updateSelectInput(session, "CryptoType", label = "Cryptocurrency Name",
                      choices = coinmarketcapr::get_crypto_listings() %>% filter(!name %in% list_Supported_Cryptocurrency) %>% select(name) %>% as.list() %>% unlist(use.names=FALSE),
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
    
    
  # Second line of Box but with fill=TRUE
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
  
  
  #Because the two api doesn't support the same currency, we have to eliminated the issues
  # validate function
  not_supported_currency <- function(input) {
    list_Supported_Currency <- c("AUD","CAD","CHF","EUR","GBP","JPY","USD")
    if (!(input %in% list_Supported_Currency)) {
      "The choosen Currency is not support for the graph, Please choose another Currency."
    } else {
      NULL
      }
  }

  
  
  # get the three letter name of crytocurrcy
  selected_symbol <- reactive({get_crypto_listings() %>% filter(name==input$CryptoType) %>% select(symbol)})
  # get data for the plot from crytowatch API
  crytowatch_data <- reactive({
    validate(not_supported_currency(input$Currency))
    
    
    # endpoint <- str_glue("https://api.cryptowat.ch/markets/kraken/{cryptocurrency}{currency}/ohlc",
    #                      cryptocurrency = "btc",
    #                      currency = "usd")
                         
                         # cryptocurrency = get_crypto_listings() %>% filter(name==input$CryptoType) %>% select(symbol) %>% tolower(),
                         # currency = tolower(input$Currency))
    # endpoint <-("https://api.cryptowat.ch/pairs")
    # w <- GET(endpoint)
    # fromJSON(content(w, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    # Settings
    exchange <- "kraken"
    pair <- paste0(selected_symbol() %>% tolower(),tolower(input$Currency))
    route <- "ohlc"
    api_key ='6NO31ET00O220TWYIMDR'
    
    # Daily prices for longest possible time period
    get_markets(route, pair, exchange,api_key=api_key,allowance = TRUE)
    
  })

  
  output$LinePlot_24hr <- renderPlot({
    # select last 24 hr data with 3 mins inteval
    closeprice_24 <- crytowatch_data()$result$`180` %>% as_tibble() %>% tail(480) %>%
      rename(CloseTime=V1,OpenPrice=V2,HighPrice=V3,LowPrice=V4,ClosePrice=V5,Volume=V6,QuoteVolume=V7)
    
    closeprice_24$CloseTime <- closeprice_24$CloseTime %>% as_datetime()
    
    #need to change the date in english form since my computer is in Chinese :(
    Sys.setlocale("LC_TIME", "C")
    
    ggplot(data = closeprice_24, 
           aes(x = CloseTime, y = ClosePrice)) + 
      xlab('Date Time (UTC)') +
      ylab('Price') +
      ggtitle(paste('Price Change Over Last 24 Hours -', selected_symbol()),
              subtitle = paste('Most recent data collected on:', closeprice_24$CloseTime %>% tail(1) ,'(UTC)')) +
      geom_line() + stat_smooth(formula = y ~ x, method = "loess") + theme_economist()+
      # labal max price
      geom_mark_ellipse(aes(filter = ClosePrice == max(ClosePrice),
                            label = CloseTime,
                            description = paste0('Price spike to $', ClosePrice)),con.colour = "red") +
      # Now the same to circle the minimum price:
      geom_mark_ellipse(aes(filter = ClosePrice == min(ClosePrice),
                            label = CloseTime,
                            description = paste0('Price drop to $', ClosePrice)),con.colour = "red")
  })
  
  # second page
  output$Top5MarketCap <- renderPlot({
    plot_top_currencies(input$Currency)+theme_economist()
  }, res = 96)
  
  output$TreeMarketCap <- renderPlot({
    market_today <- get_crypto_listings()
    
    df1 <- na.omit(market_today[,c('name','USD_market_cap')])
    df1$USD_market_cap <- as.numeric(df1$USD_market_cap)
    df1$formatted_market_cap <-  paste0(df1$name,'\n','$',format(df1$USD_market_cap,big.mark = ',',scientific = F, trim = T))
    # plot the market share of coins
    treemap::treemap(df1, index = 'formatted_market_cap', vSize = 'USD_market_cap', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette="RdYlBu")
    
  }, res = 96,width = "auto",height = "auto")
  
  
  
  # community page
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wordcloudplot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  # last page

}



# Run the application 
shinyApp(ui = ui, server = server)
