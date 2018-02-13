setwd('C:\\Users\\Client\\Desktop\\Dashboard project')

library(shiny)
library(shinydashboard)
library(highcharter)
library(treemap)
source('./Project/loadData.R',local = TRUE)

server <- function(input, output,session) {
  mydb <- connectToDB()
  df <- separateDate(mydb,"train")
  source('./Project/Page/kpi.R',local = TRUE)
  source('./Project/Page/storeInsights.R',local = TRUE)
}