library(shinydashboard)
library(highcharter)

ui = dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Key performance indicator", tabName = "overview", icon = icon("dashboard")),
      menuItem("Insight by store", tabName = "storeinsight", icon = icon("th")),
      menuItem("Analytics", tabName = "analytics", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
              fluidRow(
                valueBoxOutput('avgweeklysales'),
                valueBoxOutput('yearlygrowth'),
                valueBoxOutput('holidayaug'),
                box(width=3,title="Top performing stores",status="warning",
                    highchartOutput("topperform")),
                box(width=3,title="Least performing stores",status="warning",
                    highchartOutput("leastperform")),
                box(title = "Average weekly sales by month", status = "primary",
                    highchartOutput("weeklysalesplot"))
              )),
      tabItem(tabName="storeinsight",
              fluidRow(
                column(3, wellPanel(
                  selectInput("choosestore",
                            "Choose A Store",
                            paste('Store',seq(1:45))),
                  actionLink("selectall","Select/Unselect All Departments"), 
                  uiOutput("ui"))
                  ),
                column(9,wellPanel(
                  htmlOutput("desc")
                ),
                box())
                )
    )
  )
)
)