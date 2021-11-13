argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "md",
  side = "left",
  id = "my_sidebar",
  brand_logo = "https://i.ibb.co/Y0X4WkD/facebook-cover-photo-1-v1.png",
  dropdownMenus = 
  argonSidebarHeader(title = "Navigation"),
  argonSidebarMenu(
    argonSidebarHeader(title = "Sentiment Analysis"),
    argonSidebarItem(
      tabName = "tabs",
      icon = argonIcon(name = "like-2", color = "warning"),
      "Twitter Sentiment Analysis"
    ),
    argonSidebarItem(
      tabName = "tabs-2",
      icon = argonIcon(name = "app", color = "grey"),
      "Reddit Sentiment Analysis"
    ),
    argonSidebarDivider(),
    argonSidebarHeader(title = "Technical Analysis"),
    argonSidebarItem(
      tabName = "sections",
      icon = argonIcon(name = "sound-wave", color = "grey"),
      "Technical Analysis"
    ),
    argonSidebarDivider(),
    argonSidebarHeader(title = "Other"),
    argonSidebarItem(
      tabName = "about-us",
      icon = argonIcon(name = "circle-08", color = "grey"),
      "About Us"
    ),
    argonSidebarDivider(),
    actionButton("screenshot", "Take a screenshot")
  )
)