about_us_tab <- argonTabItem(
  tabName = "about-us",
  argonDashHeader(
    gradient = TRUE,
    color = "warning",
    separator = TRUE,
    separator_color = "info",
    top_padding = 8,
    bottom_padding = 8,
    argonRow(
      width = 12,
      argonCard(
        width = 7,
        status = "success",
        border_level = 0,
        hover_shadow = TRUE,
        argonH1("About the App", display = 4),
        p(style="text-align: justify;", strong("The Decrypt app is created by a group of students from the DBA3702 (Descriptive Analytics with R) module in the National University of Singapore.")),
        p(style="text-align: justify;", strong("We designed the app with the intention of providing users potentially interested in Cryptocurrency investments with simple, important and useful information to guide their cryptocurrency purchases.")),
        p(style="text-align: justify;", strong("Besides just allowing users to make more informed choices for their investment decisions, we also designed a simple and clean user interface, to allow for ease of usage.")),
        p(style="text-align: justify;", strong("The app consists of basic sentiment analyses of two extremely popular social media platforms - Twitter and Reddit, as well as a Technical Analysis dashboard that aims to reveal more of financial fundamentals. Taken together, we believe they are relatively formidable in conveying important insight.")),
        p(strong('Enjoy using our App! :)'))
      ),
      argonCard(
        width = 5,
        status = "success",
        border_level = 0,
        hover_shadow = TRUE,
        argonH1("How To Use", display = 4),
        "There are only afew things you will need to take note in order to use the platform efficiently.",
        br(), br(),
        strong("Technical Analysis Dashboards"),
        p(style="text-align: justify;", strong("The default cryptocurrency selected when the page is loaded is 'bitcoin'. The top 20 cryptocurrencies based on market capitalisation at real-time is listed for selection. Feel free to select up to 3 cryptocurrencies, but do take note that the user-input mechanism is reactive. It responds to each change in input and will load the appropriate data accordingly. Hence, try not to make too many changes in a short span of time..")),
        p(style="text-align: justify;", strong("As the data displayed on the platform is entirely loaded from live API pulls, they might take some time to load due to specific limits set by different providers. Furthermore, prediction algorithms run in iterations, and would require additional time to run. We appreciate your patience.")),
        p(style="text-align: justify;", strong("Additional plotting options are available for specific charts, and they can be found where the charts are displayed.")),
        br(),
        strong("Sentiment Analysis Dashboards"),
        p(style="text-align: justify;", strong("Similar to technical analysis, you can select up to 3 cryptocurrencies. Some chart options might only allow for individual cryptocurrency analysis at any point in time (which would then prompt you to choose from radio buttons). The data here is also pulled live from Twitter and Reddit and Reddit data can take a significantly longer time to load.  ")),
        p(style="text-align: justify;", strong('Lastly, do take note that information gleaned from our dashboards should not be used solely to make investment decisions. Rather, it is meant to supplement your decisions, so please invest responsibly!'))
      )
    )  
  )
)
