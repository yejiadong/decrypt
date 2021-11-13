argonHeader <- argonDashHeader(
  gradient = TRUE,
  color = "primary",
  separator = TRUE,
  separator_color = "secondary",
  argonCard(
    argonH1("Analyse Cryptocurrency", display = 4),
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    status = "primary",
    background_color = NULL,
    gradient = FALSE, 
    floating = FALSE,
    p(strong("Select up to 3 cryptocurrencies: (It might take some time for page to refresh)"), style = "font-size:13px;"),
    selectizeInput("cryptocurrency", label = "Choose cryptocurrencies from the dropdown:", multiple = TRUE, selected = "bitcoin", choices = NULL, options = list(maxItems = 3)),
    p(strong("*More options might be available in the respective plots."), style = "font-size:12px;"),
    actionButton("updatecrypto", "Run"),
  ),
)

