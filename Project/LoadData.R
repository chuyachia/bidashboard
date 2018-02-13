setwd('C:\\Users\\Client\\Desktop\\Dashboard project')
library(DBI)
library(dplyr)
library(tidyr)

loadToDB <- function() {
  stores <- read.csv('Data/stores.csv')
  features <- read.csv('Data/features.csv')
  train <- read.csv('Data/train.csv')
  mydb <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
  dbWriteTable(mydb,'train',train,overwrite=TRUE)
  dbWriteTable(mydb,'features',overwrite=TRUE)
  dbWriteTable(mydb,'stores',overwrite=TRUE)
  return(mydb)
}
connectToDB <- function(){
  mydb <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
  return(mydb)
}

avgWeeklySales <- function(mydb){
  #rtn <-dbGetQuery(mydb,
  #                 'SELECT round(avg(Weekly_Sales),2) Avg_Sales
  #                 FROM train')
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
separateDate <- function(mydb,table){
  tb <- dbReadTable(mydb,table)
  tb <- tb %>% separate(Date,c("Year","Month","Day"),"-")
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

other <- function(mydb) {
## Number of departments by store
dbGetQuery(mydb,
           'SELECT COUNT(DISTINCT Dept)
           FROM train
           GROUP BY Store')
## Departments list by store
  dbGetQuery(mydb,
             'SELECT DISTINCT Store, Dept
             FROM train')
## Total sales by store
# Top 10
dbGetQuery(mydb,
           'SELECT train.Store, Type,sum(Weekly_Sales) AS Total_Sales 
           FROM train JOIN stores
           USING (Store)
           GROUP BY train.Store 
           ORDER BY Total_Sales DESC
           LIMIT 10')
# Last 10
dbGetQuery(mydb,
           'SELECT train.Store, Type,sum(Weekly_Sales) AS Total_Sales 
           FROM train JOIN stores
           USING (Store)
           GROUP BY train.Store 
           ORDER BY Total_Sales
           LIMIT 10')

# Store wide weekly average sales
dbGetQuery(mydb,
           'SELECT Date, avg(Weekly_Sales) AS Avg_Sales
           FROM train
           GROUP BY Date')

# Weekly average sales by store type
dbGetQuery(mydb,
           'SELECT Type, Date, avg(Weekly_Sales) AS Avg_Sales
           FROM train JOIN stores
           USING (Store)
           GROUP BY Type, Date')

# Average size by store type
dbGetQuery(mydb,
           'SELECT Type, avg(Size) AS Avg_Size
           FROM stores
           GROUP BY Type')
## Holiday - non holiday average sales difference
# Average sales by holiday status
dbGetQuery(mydb,
           'SELECT ISHoliday,avg(Weekly_Sales)
           FROM train
           GROUP BY IsHoliday')
# Difference
dbGetQuery(mydb,
           'SELECT abs(holiday.Avg_Sales-nonholiday.Avg_Sales)
            FROM (SELECT avg(Weekly_Sales) Avg_Sales FROM train WHERE IsHoliday ==0) AS holiday, 
            (SELECT avg(Weekly_Sales) Avg_Sales FROM train WHERE ISHoliday ==1) AS nonholiday')

# Difference by type
dbGetQuery(mydb,
           'SELECT *
           FROM (SELECT Type, avg(Weekly_Sales) NonHoliday_Avg_Sales 
                 FROM train JOIN stores USING (Store)
                  WHERE IsHoliday ==0 GROUP BY Type) AS holiday JOIN
           (SELECT Type, avg(Weekly_Sales) Holiday_Avg_Sales 
           FROM train JOIN stores USING (Store) 
           WHERE ISHoliday ==1 GROUP BY Type) AS nonholiday
           USING(Type)') 

}