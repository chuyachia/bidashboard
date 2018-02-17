library(scales)
library(ggplot2)


month_avg_sales <- df%>% separate(Date,c("Year","Month","Day"),"-")%>%
  group_by(Dept, Year,Month)%>%
  summarise(Monthly_Sales=sum(Weekly_Sales))%>%
  unite(Date,c(Year,Month),sep="-")%>%
  filter(n()>12)%>%
  ungroup()

output$ui2 <- renderUI({
  if (is.null(month_avg_sales))
    return()
  selectInput("choosedepartts",
              "Choose a department",
              (function(){ls = unique(month_avg_sales$Dept) 
              names(ls) =paste('Dept',unique(month_avg_sales$Dept)) 
              return(ls)})())
})

output$decompo <- renderPlot({
  if (is.null(input$choosedepartts))
    return()
  filtered <- month_avg_sales%>%filter(Dept==input$choosedepartts)
  startdate <- as.numeric(strsplit(filtered$Date[1],"-")[[1]])
  enddate <- as.numeric(strsplit(filtered$Date[nrow(filtered)],"-")[[1]])
  sales.ts = ts(filtered[,3], frequency=12, start=c(startdate[1],startdate[2]), end=c(enddate[1],enddate[2]))
  decomposed <- stl(sales.ts[,1], s.window="periodic")
  filtered$Date <- as.Date(paste0(filtered$Date,'-01'))
  plotdata<- filtered %>%select(Time=Date,Observed=Monthly_Sales,-Dept)%>%
    mutate(Seasonal= decomposed$time.series[,1],
                     Trend=decomposed$time.series[,2],
                     Random=decomposed$time.series[,3])
  plotdata <- gather(plotdata, component, value, -Time)
  plotdata$component<- factor(plotdata$component,levels=c("Observed","Trend","Seasonal","Random"))
  
  ggplot(plotdata, aes(Time, value)) +
    facet_grid(component ~ ., scales="free_y") +
    geom_line() +
    theme_bw() +
    scale_x_date(labels = date_format("%Y-%m"),date_breaks="1 month")+
    labs(y="Average weekly sales", x="Month") +
    #ggtitle("Average weekly sales time series decomposition") +
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x = element_text(angle = 90))
})



output$cor <- renderHighchart({
  plotdata <- df%>%group_by(Date,Dept)%>%
    summarise(Avg_Sales=mean(Weekly_Sales))%>%
    spread(Dept,Avg_Sales)%>%
    select(which(colSums(is.na(.))==0))
  
  hchart(cor(plotdata[,-1]))
})

