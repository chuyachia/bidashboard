library(shinydashboard)
library(highcharter)

ui = dashboardPage(
  dashboardHeader(title="Retail BI dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Key performance indicators", tabName = "overview", icon = icon("dashboard")),
      menuItem("Store insights", tabName = "storeinsights", icon = icon("th")),
      menuItem("Department trends", tabName = "departtrends", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
              fluidRow(
                column(12,
                valueBoxOutput('avgweeklysales'),
                valueBoxOutput('yearlygrowth'),
                valueBoxOutput('holidayaug')
                ),
                column(12,
                 tabBox(width=6,
                        id = "tabset1", 
                        title="Store sales ranking",
                        tabPanel("Top performing stores", highchartOutput("topperform")),
                        tabPanel("Least performing stores", highchartOutput("leastperform"))
                 ),
                 tabBox(width=6,
                        id = "tabset2",
                        title = "Holiday best sellers", 
                        tabPanel("Super Bowl", highchartOutput("sbsales")),
                        tabPanel("Labor Day", highchartOutput("ldsales")),
                        tabPanel("Thanksgiving", highchartOutput("tgsales")),
                        tabPanel("Christmas", highchartOutput("xmsales"))
                 )
                ),
                column(12,
                 box(width=12,title = "Average weekly sales by month", status = "primary",
                     highchartOutput("weeklysalesplot"))
                 )
              )),
      tabItem(tabName="storeinsights",
              fluidRow(
                column(12,
                box(width=12,htmlOutput("desc"),status = "primary")
                ),
                column(12,
                box(width=3,status = "primary",
                    selectInput("choosestore",
                              "Choose a store",
                              (function(){ls = as.character(seq(1:45)) 
                              names(ls) =paste('Store',seq(1:45)) 
                              return(ls)})()),
                    actionLink("selectall","Select/Unselect all departments"), 
                    uiOutput("ui"),
                    actionButton("update", "Update view")),
                box(width=9,title = "Sales breakdown by department",status = "primary",
                    highchartOutput("salesdept"))
                ),
                column(12,
                box(width=12,title="Weekly sales trends by department",status = "primary",
                    highchartOutput("trenddept"))
                )
                )
              ),
      tabItem(tabName="departtrends",
              fluidRow(
                column(12,box(width=12,title="Average weekly sales time series decomposition",
                              status="primary",
                              uiOutput("ui2"),plotOutput("decompo"))),
                column(12,box(width=12,title="Department sales correlations",
                              status="primary",
                    highchartOutput("cor")))
              ))
  )
)
)