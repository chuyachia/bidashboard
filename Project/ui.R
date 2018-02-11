library(shinydashboard)
library(highcharter)

ui = dashboardPage(
  dashboardHeader(),
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
                box(width=3,
                    htmlOutput("desc"),
                    selectInput("choosestore",
                              "Choose A Store",
                              (function(){ls = as.character(seq(1:45)) 
                              names(ls) =paste('Store',seq(1:45)) 
                              return(ls)})()),
                    actionLink("selectall","Select/Unselect All Departments"), 
                    uiOutput("ui"),
                    actionButton("update", "Update View")
                  ),
                box(width=9,title = "Sales break down by department",
                    highchartOutput("salesdept")
                ),
                box(width=9,title="Average weekly sales trend by department",
                    highchartOutput("trenddept")    
                )
                )
    )
  )
)
)