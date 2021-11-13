sections_tab <- argonTabItem(
  tabName = "sections",
  argonDashHeader(
    gradient = TRUE,
    color = "warning",
    separator = TRUE,
    separator_color = "info",
    top_padding = 8,
    bottom_padding = 8,
    argonRow(
      width = 12,
      argonColumn(
        width = 7,
      argonCard(
        width = 12,
        status = "success",
        border_level = 0,
        hover_shadow = TRUE,
        argonH1("Technical Price Trends", display = 4),
        uiOutput("coinChoiceTechnical"),
        dropdown(
          tags$h3("Plot Options"),
          
          radioGroupButtons(
            inputId = "chartTheme",
            label = strong("Chart Theme"),
            choices = c("white", 
                        "black"),
            selected = "white",
            status = "primary",
            checkIcon = list(
              yes = icon("ok", 
                         lib = "glyphicon"),
              no = icon("remove",
                        lib = "glyphicon"))
          ),
          pickerInput(
            inputId = "typechart",
            label = "Select type of chart:", 
            choices = c("candlesticks", "matchsticks", "bars", "line"),
            selected = "candlesticks",
            options = list(
              style = "btn-primary")
          ),
          sliderTextInput(
            inputId = "lastmonths",
            label = strong("Pull data from last no. of months:"), 
            choices = c(3, 6, 9, 12, 15),
            grid = TRUE
          ),
          actionButton("goButton", "Update Chart!"),
          
          label = "Click for more plot options:",
          margin = "30px",
          circle = TRUE, status = "danger",
          icon = argonIcon("bold-right"), width = "300px",
          tooltip = tooltipOptions(title = "Click to see inputs !"),
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig
          )
        ),
        br(),
        withSpinner(plotOutput("technicalTrends")),
        br(),
        argonButton(
          name = "Explain Chart",
          status = "primary",
          icon = argonIcon("atom"),
          size = "lg",
          toggle_modal = TRUE,
          modal_id = "modal1"
        ), 
        argonModal(
          id = "modal1",
          title = "Explanation on Technical Trends Included (Credits to Investopedia)",
          status = "primary",
          gradient = TRUE,
          strong("Bollinger Bands:"),
          p("They are the two dotted red lines which serve as an indication of whether the cryptocurrency is in overbought / oversold conditions."),
          p("If price deviates too significantly from the mean, then it is likely to correct itself."),
          p(strong("Below the lower band: Crypto is oversold and should rebound.")),
          p(strong("Above the upper band: Crypto is overbought and is due for a pullback.")),
          strong("Simple Moving Average:"),
          p("It is the blue and red lines that calculates the average of a selected range of prices by 20 and 30 day intervals respectively. The SMA helps to see if prices will continue its trend or reverse."),
          strong("Relative Strength Index (RSI):"),
          p("RSI measures the magnitude of recent price changes. "),
          p(strong(">70: Crypto is overbought and primed for a reversal.")),
          p(strong("<30: Crypto is oversold / underbought and could rebound.")),
          strong("Scholastic Momentum Index:"),
          p("The SMI measures the momentum of closing prices. The price of crypto closes at its high in a market with an uptrend, and similarly, closes at its low in a market with a downtrend."),
        )

      ) %>% argonMargin(orientation = "t", value = -100),
    ),
      argonColumn(
        width = 5,
        argonCard(
          width = 12,
          background_color = "default",
          hover_lift = TRUE,
          hover_shadow = TRUE,
          argonH1("Current Prices (USD)", display = 4) %>% argonTextColor(color = "white"),
          withSpinner(htmlOutput("render_prices"))
        ) %>% argonMargin(orientation = "t", value = -100),
        argonCard(
          width = 12,
          status = "success",
          border_level = 0,
          hover_shadow = TRUE,
          argonH1("Market Capitalisation Trend", display = 4),
          withSpinner(plotOutput("historicalMarket"))
        ) 
      ) 
    )
  ),
  argonDashHeader(
    gradient = FALSE,
    color = "info",
    top_padding = 8,
    bottom_padding = 8,
    argonRow(
        argonCard(
          width = 7,
          h1("Price Prediction"),
          p(strong("Take note that the prediction will take a while to generate."), style = "font-size:13px;"),
          withSpinner(uiOutput("coinPrice")),
          withSpinner(dygraphOutput("plotPrediction"))
        ),
        argonCard(
          width = 5,
          h1("Forecast Based on Price Predictions"),
          withSpinner(plotOutput("plotForecast"))
        )
    )
  ),
  
  argonDashHeader(
    gradient = FALSE,
    color = "primary",
    top_padding = 8,
    bottom_padding = 8,
    argonRow(
      argonCard(
        width = 7,
        h1("Portfolio Optimization"),
        p(strong("Take note that the optimization will take a while to generate."), style = "font-size:13px;"),
        p(strong("The weights here represent the percentage of your overall portfolio that should contain this asset, based on the optimization."), style = "font-size:13px;"),
        withSpinner(uiOutput("render_optimization")),
        br(),
        actionButton("optimizeButton", "Optimize Portfolio!"),
      ),
      argonCard(
        width = 5,
        h1("Efficient Frontier"),
        withSpinner(uiOutput("efficient_frontier"))
      )
    )
  ),
  argonDashHeader(
    gradient = FALSE,
    color = "secondary",
    top_padding = 8,
    bottom_padding = 8,
    mask = FALSE,
    background_img = "https://images.unsplash.com/photo-1624996379671-b4d0837e45cb?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=870&q=80",
    opacity = 6
  )
)
