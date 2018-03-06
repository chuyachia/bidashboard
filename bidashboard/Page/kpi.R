### KPI
df$Holidy <- "NotHoliday"
df$Holidy[df$Date%in%c("2010-02-12","2011-02-11","2012-02-10")] <- "SuperBowl"
df$Holidy[df$Date%in%c("2010-09-10","2011-09-09","2012-09-07")] <- "LaborDay"
df$Holidy[df$Date%in%c("2010-11-26","2011-11-25","2012-11-23")] <- "Thanksgiving"
df$Holidy[df$Date%in%c("2010-12-31","2011-12-30","2012-12-28")] <- "Christmas"

observeEvent(input$show, {
  showModal(modalDialog(
    title = "About this dashboard",
    footer=modalButton('Close'),
    tags$p("This business intelligence dashboard demo is built using the Shiny framework in R with anonymized 
           historical sales data for 45 Walmart stores, each composed of a different number of departments. The whole data set can be found on",
           tags$a("Kaggle.",target="_blank",href="https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting")),
    tags$p("The ",tags$b("Key performance indicators"), 
           "part of the dashboard presents some key figures and simple charts that give an overview of the business. This part of the dashboard is meant to give users a quick glimpse of the data without going into to much details."),
    tags$p("Users of the dashboard can really delve into the data in the ",tags$b("Store insights"),
            "part where they can choose a specific store of interest and study the breakdown of the sales into different departments as well as the weekly sales trends of each of them."),
    tags$p("Besides store specific facts, departmental trends that hold true across stores are also valuable informations for business users. In the ",
           tags$b("Department trends")," part, a time series decomposition of the average department sales is presented. This allows users to clearly distinguish between the parts of the sales attributable to seasonal fluctuations and those that represent the underlying trend. 
           In this part, the relation between the sales of different departments is also presented in a correlation heatmap.")
  ))
})

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
      hc_add_series(data = round(plotdata$Avg_Sales,2), color = "#2f7ed8",
                    name = "Average weekly sales",showInLegend = FALSE)
    hc
  })
  output$leastperform <- renderHighchart({
    plotdata <- plotdata %>%top_n(-10,Avg_Sales)%>%arrange(Avg_Sales)
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = plotdata$Store)%>%
      hc_add_series(data = round(plotdata$Avg_Sales,2), color = "#2f7ed8",
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