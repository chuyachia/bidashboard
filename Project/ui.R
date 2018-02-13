library(shinydashboard)
library(highcharter)

ui = dashboardPage(
  dashboardHeader(title="Retail BI dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Key performance indicators", tabName = "overview", icon = icon("dashboard")),
      menuItem("Store insights", tabName = "storeinsights", icon = icon("th")),
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
      tabItem(tabName="storeinsights",
              fluidRow(
                box(width=12,htmlOutput("desc")),
                box(width=3,
                    selectInput("choosestore",
                              "Choose a store",
                              (function(){ls = as.character(seq(1:45)) 
                              names(ls) =paste('Store',seq(1:45)) 
                              return(ls)})()),
                    actionLink("selectall","Select/Unselect all departments"), 
                    uiOutput("ui"),
                    actionButton("update", "Update view")
                  ),
                box(width=9,title = "Sales breakdown by department",
                    highchartOutput("salesdept")
                ),
                box(width=12,title="Average weekly sales trend by department",
                    highchartOutput("trenddept")    
                )
                )
    )
  )
)
)