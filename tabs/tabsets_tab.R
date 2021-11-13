tabsets_tab <- argonTabItem(
  tabName = "tabs",
  argonRow(
    # Horizontal Tabset
    argonColumn(
      width = 7,
      argonH1("Twitter Sentiment Analysis", display = 4),
      argonTabSet(
        id = "tab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Sentiment Polarity [Twitter]",
          active = TRUE,
          argonH1("Mean Sentiment Score By Tweet ID", display = 4),
          uiOutput("coinChoice"),
          withSpinner(plotlyOutput("sentimentTweetId"))
        ),
        argonTab(
          tabName = "Top-Occuring Words [Twitter]",
          active = FALSE,
           tags$div(
             materialSwitch(
               inputId = "wordcloudSwitch", 
               label = "Wordcloud", 
               inline = TRUE,
               status = "primary"),
             tags$span("Highest Frequency Words Plot"),
           ),
          conditionalPanel(
            condition = "input.wordcloudSwitch == true",
            prettyRadioButtons(
              inputId = "sentimentOrTopWords",
              label = "Choose type of graph:", 
              choices = c("Top Words", "Exact Sentiments"),
              icon = icon("check"), 
              selected = "Top Words",
              bigger = TRUE,
              status = "info",
              animation = "jelly"
            )
          ),
          p("Take note that the sentiment plot can take some time to generate."),
          uiOutput("topOccuringCoinChoice"),
          withSpinner(wordcloud2Output("wordCloudplot")),
          plotOutput("topWordsPlot"),
          plotOutput("sentimentPlot"),
        )
      )
  ),
    argonColumn(
      width = 5,
      title = "Current Sentiment Scores",
      argonCard(
        width = 12,
        background_color = "default",
        hover_lift = TRUE,
        hover_shadow = TRUE,
        argonH1("Current Sentiment Scores", display = 4) %>% argonTextColor(color = "white"),
        withSpinner(htmlOutput("render_stats"))
      ),
      argonCard(
        width = 12,
        argonH1("Mean Sentiment Score By Date", display = 4),
        src = NULL,
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        strong(textOutput("dateText")),
        withSpinner(plotlyOutput("sentimentTime")),
      ),
      argonCard(
        width = 12,
        argonH1("3D Network Visualisation", display = 4),
        src = NULL,
        hover_lift = TRUE,
        shadow = TRUE,
        shadow_size = NULL,
        hover_shadow = FALSE,
        border_level = 0,
        icon = argonIcon("atom"),
        status = "primary",
        background_color = NULL,
        gradient = FALSE, 
        floating = FALSE,
        p("Feel free to zoom in or use your cursor to browse the 3D visualisation."),
        withSpinner(visNetworkOutput("networkmap")),
      )
    )
  )
)
