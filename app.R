# Shiny libraries
library(shiny)
library(argonR)
library(argonDash)
library(magrittr)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(dygraphs)
library(shinydisconnect)
library(shinyscreenshot)
library(shinyBS)

# template
source("sidebar.R")
source("header.R")
source("footer.R")

# R scripts
source("technical_analysis.R")
source("twitter.R")
source("reddit.R")

# elements
source("tabs/tabsets_tab.R")
source("tabs/tabsets_tab2.R")
source("sections/sections_tab.R")
source("sections/about_us_tab.R")

# App
bsModalNoClose <-function(...) {
  b = bsModal(...)
  b[[2]]$`data-backdrop` = "static"
  b[[2]]$`data-keyboard` = "false"
  return(b)
}
shiny::shinyApp(
  ui = argonDashPage(
      disconnectMessage(),
      title = "Decrypt Dashboard",
      author = "Group 9",
      description = "Analyze Cryptocurrency",
      sidebar = argonSidebar,
      header = argonHeader,
      body = argonDashBody(
        useShinyjs(),
        bsModal("window", "Window",
                       img = img(src="https://i.ibb.co/Y0X4WkD/facebook-cover-photo-1-v1.png"),
                       text = h1("Welcome to Decrypt!", align='center'),
                       text = h3("Here, you can find insights about cryptocurrency sentiments on Twitter and Reddit, as well as conduct some basic technical analysis.", align='center'),
                       title="",size='large',
                       footer = h3("Do remember not to click other buttons while the page is loading any content! There will be no response.",align='center'),
                       text = h3("Enjoy:)", align='center'),
                       tags$head(tags$style("#window .modal-footer{margin:auto}
                                       .modal-header .close{display:none}"),
                                 tags$script("$(document).ready(function(){
                                        $('#window').modal();
                                        });")
                       )),
        tags$style(".fa-arrow-down {color:#f5365c}"),
        tags$style(".fa-arrow-up {color:#2dce89}"),
        tags$head(tags$style("#render_stats{color: white;
                                 font-size: 15px;
                                 }"
                             ),
                  tags$style("#render_stats_comments{color: white;
                                 font-size: 15px;
                                 }"
                  ),
                  tags$style("#render_prices{color: white;
                                           font-size: 15px;
                                           }"
                  )
                ),
        argonTabItems(
          tabsets_tab,
          tabsets_tab2,
          sections_tab,
          about_us_tab
        )
      ),
      footer = argonFooter
    ),
  
  # Server starts here
  server = function(input, output, session) {
    toggleModal(session, "startupModal", toggle = "open")
    
    # Grab top 20 crypto-currencies from Coingeckor
    top_20_cryptos <- getTopCoins()
    top_20_cryptos_full <- getTopCoinsFull()
    
    updateSelectInput(inputId = "cryptocurrency", selected = "bitcoin", choices = top_20_cryptos$id)
    
    # Reactive Pull and Process twitter tweets
    processedTweets <- eventReactive(input$updatecrypto,{
        req(input$cryptocurrency)
        processTweets(pullTweets(input$cryptocurrency, 10)) 
      }, ignoreNULL = FALSE)
    unnestedTweets <- reactive({ 
        req(processedTweets())
        unnestSentiments(processedTweets()) 
      })
    mergedTweets <- reactive({ 
        req(unnestedTweets())
        mergeSentiments(processedTweets(), unnestedTweets()) 
      })
    additionalFilter <- reactive({
      req(mergedTweets())
      req(input$coinChoice)
      return(mergedTweets() %>% filter(currency %in% input$coinChoice))
    })
    
    mostRecentSentimentValues <- reactive({
      req(mergedTweets())
      getUpdatedSentimentValues(mergedTweets())
    })
    
    # Reactive Pull and Process Reddit Comments
    pulledComments <- reactive({
      req(input$coinChoiceComments)
      pullComments(input$coinChoiceComments)
    })
    
    cleanedComments <- reactive({ 
      req(pulledComments())
      cleanComments(pulledComments()) 
    })
    
    unnestComments <- reactive({ 
      req(cleanedComments())
      unnest_reddit(cleanedComments()) 
    })
    
    mergedComments <- reactive({ 
      req(unnestComments())
      mergecommentSentiments(cleanedComments(), unnestComments()) 
    })
    
    mostRecentSentimentValuesComments <- reactive({
      req(mergedComments())
      getUpdatedCommentSentimentValues(mergedComments())
    })
    
    # Pulling and Processing of Market Information
    currentMarketInfo <- eventReactive(input$updatecrypto,{
      req(input$cryptocurrency)
      getCurrentMarketData() 
    }, ignoreNULL = FALSE)
    
    historicalMarketInfo <- eventReactive(input$updatecrypto,{
      req(input$cryptocurrency)
      getHistoricalMarketData(input$cryptocurrency) 
    }, ignoreNULL = FALSE)
    
    prophet <- reactive({
      req(input$coinPrice)
      predictPrice(input$coinPrice)
    })
    
    optimizePort <- eventReactive(input$updatecrypto,{
      req(input$cryptocurrency)
      req(currentMarketInfo())
      newlist = list()
      for (i in input$cryptocurrency) {
        newlist <- append(newlist, convertCoinNameToSymbol(i, currentMarketInfo()))
      }
      if (length(newlist) > 1) {
       return(optimizePortfolio(newlist))
      }
    }, ignoreNULL = FALSE)
    
    # Twitter Sentiment Analysis Renders
    output$render_stats <- renderUI({
      req(mostRecentSentimentValues())
        lapply(seq_len(nrow(mostRecentSentimentValues())), function(s) {
          str <- paste(mostRecentSentimentValues()[s,1]$currency, ": ", mostRecentSentimentValues()[s,2]$mean_sentiment, '&emsp;')
          str2 <- ""
          if (mostRecentSentimentValues()[s,4]$up) {
            str2 <- paste("<span style = 'color:#2dce89'>", icon("arrow-up"), mostRecentSentimentValues()[s,5]$diff, "%", "</span>")
          } else {
            str2 <- paste("<span style = 'color:#f5365c'>", icon("arrow-down"), mostRecentSentimentValues()[s,5]$diff, "%", "</span>")
          }
          str3 <- " Since Yesterday"
          HTML("<strong>", paste(str, str2, '&emsp;', str3, "</strong> <br/>"))
        })
    })
    
    output$coinChoice <- renderUI({
      input$updatecrypto
      req(isolate(input$cryptocurrency))
      checkboxGroupButtons(
        inputId = "coinChoice",
        label = strong("Choose Cryptocurrencies:"), 
        choices = isolate(input$cryptocurrency),
        selected = isolate(input$cryptocurrency),
        status = "primary"
      )
    })
    
    output$coinChoiceTechnical <- renderUI({
      input$updatecrypto
      req(isolate(input$cryptocurrency))
      radioGroupButtons(
        inputId = "coinChoiceTechnical",
        label = strong("Choose a Cryptocurrency:"), 
        choices = isolate(input$cryptocurrency),
        selected = isolate(input$cryptocurrency)[1],
        status = "primary"
      )
    })
    
    output$coinPrice <- renderUI({
      input$updatecrypto
      req(isolate(input$cryptocurrency))
      radioGroupButtons(
        inputId = "coinPrice",
        label = strong("Choose a Cryptocurrency:"), 
        choices = isolate(input$cryptocurrency),
        selected = isolate(input$cryptocurrency)[1],
        status = "primary"
      )
    })
    
    output$topOccuringCoinChoice <- renderUI({
      input$updatecrypto
      req(isolate(input$cryptocurrency))
      radioGroupButtons(
        inputId = "topOccuringCoinChoice",
        label = strong("Choose a Cryptocurrency"), 
        choices = isolate(input$cryptocurrency),
        selected = isolate(input$cryptocurrency)[1],
        status = "primary"
      )
    })
    
    output$dateText <- renderText({ 
      paste0(paste("Tweets collected from", (Sys.Date() - 6)) , " to " , Sys.Date())
    })

    output$sentimentTime <- renderPlotly({
      req(mergedTweets())
      plotSentimentByTime(mergedTweets())
    })
    
    observe({
      observeEvent(input$wordcloudSwitch,{
        if(input$wordcloudSwitch == FALSE){
          shinyjs::show("wordCloudplot")
          shinyjs::hide("topWordsPlot")
          shinyjs::hide("sentimentPlot")
        }else {
          shinyjs::hide("wordCloudplot")
          if (input$sentimentOrTopWords == "Top Words") {
            shinyjs::show("topWordsPlot")
          } else {
            shinyjs::show("sentimentPlot")
          }
        }
      })
    })
    
    observe({
      observeEvent(input$wordcloudSwitchComments,{
        if(input$wordcloudSwitchComments == FALSE){
          shinyjs::show("wordcloudCommentsPlot")
          shinyjs::hide("topOccuringWordsComments ")
        }else {
          shinyjs::hide("wordcloudCommentsPlot")
          shinyjs::show("topOccuringWordsComments ")
        }
      })
    })
    
    observe({
      observeEvent(input$sentimentOrTopWords,{
        if (input$wordcloudSwitch == TRUE) {
          if(input$sentimentOrTopWords == "Top Words"){
            shinyjs::show("topWordsPlot")
            shinyjs::hide("sentimentPlot")
          } else {
            shinyjs::hide("topWordsPlot")
            shinyjs::show("sentimentPlot")
          }
        }
        return()
      })
    })
    
    output$wordCloudplot <- renderWordcloud2({
      req(unnestedTweets())
      req(input$topOccuringCoinChoice)
      wordCloud(input$topOccuringCoinChoice, unnestedTweets())
    })
    
    output$topWordsPlot <- renderPlot({
      req(unnestedTweets())
      req(input$topOccuringCoinChoice)
      plotTopOccuringWords(input$topOccuringCoinChoice, unnestedTweets())
    })
    
    output$sentimentPlot <- renderPlot({
      req(unnestedTweets())
      req(input$topOccuringCoinChoice)
      plotSentimentWords(input$topOccuringCoinChoice, unnestedTweets())
    })
  
    
    output$sentimentTweetId <- renderPlotly({ 
      req(input$coinChoice)
      plot <- plotSentimentByTweetId(additionalFilter())
      if (length(input$coinChoice) == 1) {
        ggplotly(plot, tooltip = "text") %>% layout(margin = list(b = 50, l = 50, r = 20), legend = list(orientation = 'h', xanchor = "center", x = 0.5, y=  1.4), xaxis = list( rangeslider = list(type="date")))
      } else {
        ggplotly(plot, tooltip = "text") %>% layout(margin = list(b = 50, l = 50, r = 20), legend = list(orientation = 'h', xanchor = "center", x = 0.5, y=  1.4))
      }
    }) 
    
    output$networkmap <- renderVisNetwork({
      req(unnestedTweets())
      network(top_20_cryptos_full,unnestedTweets())
    })
    
    # Reddit Renders
    
    output$render_stats_comments <- renderUI({
      req(input$coinChoiceComments)
      req(mostRecentSentimentValuesComments())
      str <- paste(input$coinChoiceComments, ": ", mostRecentSentimentValuesComments()[1,1]$mean_sentiment, '&emsp;')
      str2 <- ""
      shiny::validate(
        need(try(!is.na(mostRecentSentimentValuesComments()[1,3]$up)), "There is insufficient Reddit data for this coin. Please try another coin.")
      )
      if (mostRecentSentimentValuesComments()[1,3]$up) {
        str2 <- paste("<span style = 'color:#2dce89'>", icon("arrow-up"), mostRecentSentimentValuesComments()[1,4]$diff, "%", "</span>")
      } else {
        str2 <- paste("<span style = 'color:#f5365c'>", icon("arrow-down"), mostRecentSentimentValuesComments()[1,4]$diff, "%", "</span>")
      }
      str3 <- " Since Yesterday"
      HTML("<strong>", paste(str, str2, '&emsp;', str3, "</strong> <br/>"))
    })
    output$coinChoiceCommentsUI <- renderUI({
      input$updatecrypto
      req(isolate(input$cryptocurrency))
      radioGroupButtons(
        inputId = "coinChoiceComments",
        label = strong("Choose Cryptocurrency:"), 
        choices = isolate(input$cryptocurrency),
        selected = isolate(input$cryptocurrency)[1],
        status = "primary"
      )
    })
    
    output$plotCommentsSentimentCommentId <- renderPlotly({
      req(mergedComments())
      plotSentimentByCommentId(mergedComments())
    })
    
    output$plotCommentsSentimentTime <- renderPlot({
      req(mergedComments())
      req(input$coinChoiceComments)
      plotCommentSentimentByTime(mergedComments(), input$coinChoiceComments)
    })
    
    output$topOccuringWordsComments <- renderPlot({
      req(unnestComments())
      plotTopOccuringWordsComments(unnestComments())
    })
    
    output$wordcloudCommentsPlot <- renderPlot({
      req(unnestComments())
      wordCloudComments(unnestComments())
    })
  
    # Technical Analysis Renders
    output$currentMarket <- renderPlot({
      req(currentMarketInfo())
      plotTreeMap(currentMarketInfo())
    }, bg="transparent")
    
    output$plotPrediction  <- renderDygraph({
      req(prophet())
      plotProphet(prophet())
    })
    
    output$plotForecast  <- renderPlot({
      req(prophet())
      plotForecast(prophet())
    })
    
    output$historicalMarket <- renderPlot({
      req(historicalMarketInfo())
      plotHistoricalMarketCapLine(historicalMarketInfo())
    })
    
    output$technicalTrends <- renderPlot({
      req(input$coinChoiceTechnical)
      req(isolate(input$typechart))
      req(isolate(input$chartTheme))
      req(isolate(currentMarketInfo()))
      req(isolate(input$lastmonths))
      input$goButton
      symbol <- convertCoinNameToSymbol(input$coinChoiceTechnical, currentMarketInfo())
      chartSeriesFunc(symbol, isolate(input$chartTheme), isolate(input$lastmonths), isolate(input$typechart))
    })
    
    output$render_prices <- renderUI({
      input$updatecrypto
      req(currentMarketInfo())
      filtered <- filterCurrentMarketDate(isolate(input$cryptocurrency), currentMarketInfo())
      lapply(seq_len(nrow(filtered)), function(s) {
        str <- paste(filtered[s,1], ": $", filtered[s,2], '&emsp;')
        str2 <- ""
        if (filtered[s,3] > 0) {
          str2 <- paste("<span style = 'color:#2dce89'>", icon("arrow-up"), filtered[s,3], "%", "</span>")
        } else {
          str2 <- paste("<span style = 'color:#f5365c'>", icon("arrow-down"), filtered[s,3], "%", "</span>")
        }
        str3 <- " Since 24 hours ago"
        HTML("<strong>", paste(str, str2, '&emsp;', str3, "</strong> <br/>"))
      })
    })
    
    output$render_optimization <- renderUI({
      input$optimizeButton
      req(isolate(input$cryptocurrency))
      if (length(isolate(input$cryptocurrency)) > 1) {
        req(isolate(optimizePort()))
        df <- as.data.frame(extractWeights(isolate(optimizePort())))
        lapply(seq_len(nrow(df)), function(s) {
          str <- paste(substr(row.names(df)[s], 1, nchar(row.names(df)[s]) - 10), " Weights: ", df[s,1], '&emsp;')
          HTML("<strong>", paste(str,"</strong> <br/>"))
        })
      } else {
        h1("Select at least two cryptocurrencies to get an optimization.")
      }
    })
    
    output$efficient_frontier <- renderUI({
      input$optimizeButton
      req(isolate(optimizePort()))
      req(isolate(input$cryptocurrency))
      if (length(isolate(input$cryptocurrency)) > 1) {
        output$plotef <- renderPlot({
          plotEfficientFrontier(isolate(optimizePort()))
        })
        plotOutput("plotef")
      } else {
        h1("Select at least two cryptocurrencies to get an optimization.", style = "font-size:13px;")
      }
    })
    
    
    # Allow Screenshots
    observeEvent(input$screenshot, {
      screenshot()
    })
    
  }
)