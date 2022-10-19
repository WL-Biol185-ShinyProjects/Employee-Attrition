library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Healthcare Attrition", titleWidth = 750),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "Graphs"),
      menuItem("History", tabName = "History")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Graphs"),
      tabItem(tabName = "History")
    )
  )
)
