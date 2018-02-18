library(shinydashboard)
library(shinycssloaders)
library(highcharter)
library(ggiraph)

ui = dashboardPage(
  dashboardHeader(title="Retail BI dashboard",
                  tags$li(a(href='#',
                            icon('question-circle'),
                            id='show',
                            title = "About",
                            class="action-button shiny-bound-input"),
                          class="dropdown"),
                  tags$li(a(href = 'https://github.com/chuyachia/bidashboard',
                            target="_blank",
                            icon("code"),
                            title = "Source code"),
                          class = "dropdown")),
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
                        tabPanel("Top performing stores", 
                                 highchartOutput("topperform")%>% withSpinner()),
                        tabPanel("Least performing stores", 
                                 highchartOutput("leastperform")%>% withSpinner())
                 ),
                 tabBox(width=6,
                        id = "tabset2",
                        title = "Holiday best sellers", 
                        tabPanel("Super Bowl", 
                                 highchartOutput("sbsales")%>% withSpinner()),
                        tabPanel("Labor Day", 
                                 highchartOutput("ldsales")%>% withSpinner()),
                        tabPanel("Thanksgiving", 
                                 highchartOutput("tgsales")%>% withSpinner()),
                        tabPanel("Christmas", 
                                 highchartOutput("xmsales")%>% withSpinner())
                 )
                ),
                column(12,
                 box(width=12,title = "Average weekly sales by month", status = "primary",
                     highchartOutput("weeklysalesplot")%>% withSpinner())
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
                    highchartOutput("salesdept")%>% withSpinner())
                ),
                column(12,
                box(width=12,title="Weekly sales trends by department",status = "primary",
                    highchartOutput("trenddept")%>% withSpinner())
                )
                )
              ),
      tabItem(tabName="departtrends",
              fluidRow(
                column(12,box(width=12,title="Average weekly sales time series decomposition",
                              status="primary",
                              uiOutput("ui2"),
                              ggiraphOutput("decompo")%>% withSpinner())),
                column(12,box(width=12,title="Department sales correlations",
                              status="primary",
                    highchartOutput("cor")%>% withSpinner()))
              ))
  )
)
)