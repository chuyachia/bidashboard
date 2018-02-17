### KPI
df$Holidy <- "NotHoliday"
df$Holidy[df$Date%in%c("2010-02-12","2011-02-11","2012-02-10")] <- "SuperBowl"
df$Holidy[df$Date%in%c("2010-09-10","2011-09-09","2012-09-07")] <- "LaborDay"
df$Holidy[df$Date%in%c("2010-11-26","2011-11-25","2012-11-23")] <- "Thanksgiving"
df$Holidy[df$Date%in%c("2010-12-31","2011-12-30","2012-12-28")] <- "Christmas"

output$avgweeklysales <- renderValueBox({
  val <- avgWeeklySales(mydb)
  valueBox(paste0("$",format(val,digits=9,decimal.mark=".",big.mark=",")), "storewide average weekly sales", icon = icon("list"),
           color = "blue"
  )
}) 
output$holidayaug <- renderValueBox({
  val <- holidayAug(mydb)
  valueBox(paste0(val,"%"),"boost in sales during holidays",icon=icon("list"),
           color="purple"         
  )
})
output$yearlygrowth <- renderValueBox({
  tb <-df %>% separate(Date,c("Year","Month","Day"),"-")
  val <- y2yGrowth(tb,"2011","2012")
  valueBox(paste0(val,"%"),"growth in sales from 2011 to 2012",icon=icon("list"),
           color="yellow"         
  )
})
output$weeklysalesplot<-renderHighchart({
  plotdata <- df %>% separate(Date,c("Year","Month","Day"),"-")%>% 
    group_by(Store,Year,Month,Day)%>%
    summarise(Weekly_Sales = sum(Weekly_Sales))%>%
    group_by(Year,Month)%>%
    summarise(Avg_Sales = mean(Weekly_Sales))%>%
    spread(Year, Avg_Sales)
  
  hc <- highchart() %>% 
    hc_xAxis(categories = plotdata$Month)%>%
    hc_add_series(name = "Year 2010",data = round(plotdata$`2010`,2))%>%
    hc_add_series(name = "Year 2011",data = round(plotdata$`2011`,2))%>%
    hc_add_series(name = "Year 2012",data = round(plotdata$`2012`,2))
  hc
})

(function(){
  plotdata <- storeAvg(mydb)
  plotdata$Store <- paste("Store",plotdata$Store)
  output$topperform <- renderHighchart({
    plotdata <- plotdata %>%top_n(10,Avg_Sales)
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = plotdata$Store)%>%
      hc_add_series(data = round(plotdata$Avg_Sales), color = "#2f7ed8",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
  output$leastperform <- renderHighchart({
    plotdata <- plotdata %>%top_n(-10,Avg_Sales)%>%arrange(Avg_Sales)
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = plotdata$Store)%>%
      hc_add_series(data = round(plotdata$Avg_Sales), color = "#2f7ed8",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
})()

(function(){
  holiday_sales <-df %>%group_by(Holidy,Dept)%>%
    summarise(Avg_Sales=mean(Weekly_Sales))%>%
    spread(Holidy,Avg_Sales)
  
  holiday_sales$Dept <- paste("Dept",holiday_sales$Dept)
  
  output$tgsales <- renderHighchart({
    plotdata <- holiday_sales%>%top_n(10,Thanksgiving)%>%
      arrange(desc(Thanksgiving))
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = plotdata$Dept)%>%
      hc_add_series(data = round(plotdata$Thanksgiving,2), color = "#8bbc21",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
  output$sbsales <- renderHighchart({
    plotdata <- holiday_sales%>%top_n(10,SuperBowl)%>%
      arrange(desc(SuperBowl))
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = plotdata$Dept)%>%
      hc_add_series(data = round(plotdata$SuperBowl,2), color = "#8bbc21",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
  
  output$ldsales <- renderHighchart({
    plotdata <- holiday_sales%>%top_n(10,LaborDay)%>%
      arrange(desc(LaborDay))
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = plotdata$Dept)%>%
      hc_add_series(data = round(plotdata$LaborDay,2), color = "#8bbc21",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
  
  output$xmsales <- renderHighchart({
    plotdata <- holiday_sales%>%top_n(10,Christmas)%>%
      arrange(desc(Christmas))
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = plotdata$Dept)%>%
      hc_add_series(data = round(plotdata$Christmas,2), color = "#8bbc21",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
})()