source('loadData.R',local = TRUE)

server <- function(input, output,session) {
  mydb <- connectToDB()
  df <- writeToTable(mydb,"train")
  source('Page/kpi.R',local = TRUE)
  source('Page/storeInsights.R',local = TRUE)
  source('Page/departTrends.R',local = TRUE)
}