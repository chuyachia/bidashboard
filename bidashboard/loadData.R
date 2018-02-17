library(DBI)
library(dplyr)
library(tidyr)

connectToDB <- function(){
  mydb <- dbConnect(RSQLite::SQLite(), "Data/my-db.sqlite")
  return(mydb)
}

avgWeeklySales <- function(mydb){
  rtn <-dbGetQuery(mydb,
                   'SELECT round(avg(Weekly_Sales),2) Avg_Sales
                    FROM 
                    (SELECT Store, Date, sum(Weekly_Sales) Weekly_Sales
                   FROM train
                   GROUP BY Store, Date)')
  return (rtn)
}

holidayAug <- function(mydb){
  tb <- dbGetQuery(mydb,
             'SELECT ISHoliday,avg(Weekly_Sales) Avg_Sales
             FROM train
             GROUP BY IsHoliday')
  rtn <- round((tb$Avg_Sales[tb$IsHoliday==1]-tb$Avg_Sales[tb$IsHoliday==0])/tb$Avg_Sales[tb$IsHoliday==0],4)*100
  return (rtn)
}
writeToTable <- function(mydb,table){
  tb <- dbReadTable(mydb,table)
  return (tb)
}

y2yGrowth <- function(df,prevyear,thisyear){
  #only accounting for months for which data is available for all 3 years
  df <- df %>%filter(!Month %in% c("01","11","12"))%>% 
    group_by(Year,Store)%>% 
    summarise(Weekly_Sales = sum(Weekly_Sales))%>%
    group_by(Year)%>%
    summarise(Avg_Sales= mean(Weekly_Sales))
  rtn <- round((df$Avg_Sales[df$Year==thisyear]-df$Avg_Sales[df$Year==prevyear])/df$Avg_Sales[df$Year==prevyear],4)*100
  return (rtn)
}


storeAvg <- function(mydb){
  rtn <- dbGetQuery(mydb,
             'SELECT A.Store AS Store, Type,avg(Weekly_Sales) AS Avg_Sales 
             FROM (SELECT Store, Date, sum(Weekly_Sales) AS Weekly_Sales
                    FROM train
                    GROUP BY Store, Date) AS A
            JOIN stores
             USING (Store)
             GROUP BY A.Store 
             ORDER BY Avg_Sales DESC')
  return (rtn)
}

departByStore <- function(mydb,storenum) {
  rtn <-   dbGetQuery(mydb,
                      paste('SELECT DISTINCT Dept
                      FROM train
                      WHERE Store==',storenum))
  return (rtn)
}

storeInfo <- function(mydb,storenum) {
  rtn <- dbGetQuery(mydb,
             paste('SELECT *
                    FROM stores
                    WHERE Store==',storenum))
  return (rtn)
}
storeSalesByDept <- function(mydb,storenum) {
  rtn <- dbGetQuery(mydb,
                    paste('SELECT Dept, sum(Weekly_Sales) AS Total_Sales
                           FROM train
                           WHERE Store==',storenum,
                           ' GROUP BY Dept'
                           ))
  return (rtn)
}

deptAvg <- function(mydb,storenum){
  rtn <- dbGetQuery(mydb,
                    paste('SELEC avg(Weekly_Sales) AS Avg_Sales 
                    FROM train
                    GROUP BY Dept
                    WHERE Store==',storenum,
                    'ORDER BY Avg_Sales DESC')
                    )
  return (rtn)
}

deptSales<- function(mydb,storenum){
  rtn <- dbGetQuery(mydb,
                    paste('SELECT Date,Dept,Weekly_Sales
                          FROM train
                          WHERE Store==',storenum)
  )
  return (rtn)
}

