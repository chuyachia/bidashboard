### KPI
output$avgweeklysales <- renderValueBox({
  val <- avgWeeklySales(mydb)
  valueBox(paste0("$",format(val,digits=9,decimal.mark=".",big.mark=",")), "average storewide sales per week", icon = icon("list"),
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
  val <- y2yGrowth(df,"2011","2012")
  valueBox(paste0(val,"%"),"growth in sales from 2011 to 2012",icon=icon("list"),
           color="yellow"         
  )
})
output$weeklysalesplot<-renderHighchart({
  plotdata <- df %>% group_by(Store,Year,Month,Day)%>%
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
      hc_add_series(data = round(plotdata$Avg_Sales), color = "#8bbc21",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
})()