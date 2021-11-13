tabsets_tab2 <- argonTabItem(
  tabName = "tabs-2",
  argonRow(
    width = 12,
    argonColumn(
      argonH1("Choose the cryptocurrency to analyze here: ", display = 4),
      p("Take note that Reddit analysis only supports single coin at any point in time due to latency. It might also take slightly longer than Twitter to graph."),
      uiOutput("coinChoiceCommentsUI")
    )
  ),
  argonRow(
    # Horizontal Tabset
    argonColumn(
      width = 6,
      argonH1("Reddit Sentiment Analysis", display = 4),
      argonTabSet(
        id = "tab-3",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1:2, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Sentiment Polarity [Reddit]",
          active = TRUE,
          argonH1("Mean Sentiment Score By Comment ID", display = 4),
          withSpinner(plotlyOutput("plotCommentsSentimentCommentId"))
        ),
        argonTab(
          tabName = "Top-Occuring Words [Reddit]",
          active = FALSE,
          tags$div(
            materialSwitch(
              inputId = "wordcloudSwitchComments", 
              label = "Wordcloud and Highest Freq Words Plot", 
              inline = TRUE,
              status = "primary"),
            tags$span("Highest Frequency Words Plot"),
          ),
          withSpinner(plotOutput("wordcloudCommentsPlot")),
          plotOutput("topOccuringWordsComments"),
        )
      )
  ),
    argonColumn(
      width = 6,
      argonCard(
        width = 12,
        background_color = "default",
        hover_lift = TRUE,
        hover_shadow = TRUE,
        argonH1("Current Sentiment Scores", display = 4) %>% argonTextColor(color = "white"),
        withSpinner(htmlOutput("render_stats_comments"))
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
        withSpinner(plotOutput("plotCommentsSentimentTime")),
      )
    )
  )
)
