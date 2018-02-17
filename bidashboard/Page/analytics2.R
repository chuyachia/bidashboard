## which department gain the most from which holiday
library(highcharter)
test <- df
test$Holidy <- "NotHoliday"
test$Holidy[test$Date%in%c("2010-11-26","2011-11-25","2012-11-23")] <- "Thanksgiving"
test$Holidy[test$Date%in%c("2010-09-10","2011-09-09","2012-09-07")] <- "LaborDay"
test$Holidy[test$Date%in%c("2010-02-12","2011-02-11","2012-02-10")] <- "SuperBowl"
test$Holidy[test$Date%in%c("2010-12-31","2011-12-30","2012-12-28")] <- "Christmas"

test2 <-test %>%group_by(Holidy,Dept)%>%
  summarise(Avg_Sales=mean(Weekly_Sales))%>%
  spread(Holidy,Avg_Sales)%>%na.omit()%>%
  mutate(TGgain =(Thanksgiving- NotHoliday)/NotHoliday,
         LDgain = (LaborDay-NotHoliday)/NotHoliday,
         SBgain=(SuperBowl-NotHoliday)/NotHoliday,
         XMgain=(Christmas-NotHoliday)/NotHoliday)

## Peak month by department (different periodic trend by depart see 16,3)
test3 <- df%>%filter(Dept=="5")%>%
  spread(Store,Weekly_Sales)%>%
  select(-Dept,-IsHoliday)
test3 <- as.data.frame(test3)

hc <- highchart() %>% 
  hc_xAxis(categories = test3$Date)

for(i in 1:45){
  hc <- hc_add_series(hc,data = round(test3[,i+1],2), name=paste("Store ",i),showInLegend =FALSE)
}
hc

test3 <- df%>% filter(Dept=="16")%>%
  separate(Date,c("Year","Month","Day"),"-")%>%
  group_by(Year,Month)%>%
  summarise(Monthly_Sales=sum(Weekly_Sales))%>%
  unite(Date,c(Year,Month),sep="-")
myts <- ts(test3, start=c(2010, 2), end=c(2012, 10), frequency=12)
test3 <- as.data.frame(test3)
library(xts)
sales <- xts(test3[,-1], order.by=as.Date(test3[,1], "%Y-%m"))
plot(sales)
sales = ts(sales)
sales = decompose(sales, "additive")
library(fpp)

## Department sales correlation
test4 <- df%>%group_by(Date,Dept)%>%
  summarise(Avg_Sales=mean(Weekly_Sales))%>%
  spread(Dept,Avg_Sales)%>%
  select(which(colSums(is.na(.))==0))

hchart(cor(test4[,-1]))
