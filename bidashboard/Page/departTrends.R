library(scales)
library(ggplot2)
library(ggiraph)

month_avg_sales <- df%>% separate(Date,c("Year","Month","Day"),"-")%>%
  group_by(Dept, Year,Month)%>%
  summarise(Monthly_Sales=sum(Weekly_Sales,na.rm = TRUE))%>%
  unite(Date,c(Year,Month),sep="-")%>%
  filter(n()==33)%>% ## 33: whole period from 2010/2-2012/10
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

output$decompo <- renderggiraph({
  if (is.null(input$choosedepartts))
    return()
  filtered <- month_avg_sales%>%filter(Dept==input$choosedepartts)
  startdate <- as.numeric(strsplit(filtered$Date[1],"-")[[1]])
  enddate <- as.numeric(strsplit(filtered$Date[nrow(filtered)],"-")[[1]])
  sales.ts = ts(filtered[,3], frequency=12, start=c(startdate[1],startdate[2]), end=c(enddate[1],enddate[2]))
  if (!is.null(dim(sales.ts)[2])) {
    sales.ts <- sales.ts[,1]
  }
  decomposed <- stl(sales.ts, s.window="periodic")
  filtered$Time <- as.Date(paste0(filtered$Date,'-01'))
  plotdata<- filtered %>%select(Date,Time,Observed=Monthly_Sales,-Dept)%>%
    mutate(Seasonal= decomposed$time.series[,1],
                     Trend=decomposed$time.series[,2],
                     Random=decomposed$time.series[,3])
  plotdata <- gather(plotdata, component, value, -Time,-Date)
  plotdata$component<- factor(plotdata$component,levels=c("Observed","Seasonal","Trend","Random"))
  plotdata$tooltip <- paste(plotdata$Date,"<br>Average weekly sales :", dollar_format(largest_with_cents = 1e+08)(plotdata$value))
  
  my_gg<- ggplot(plotdata, aes(x=Time, y=value, group = component)) +
    facet_grid(component ~ ., scales="free_y") +
    geom_line(colour="#8bbc21") +
    geom_point_interactive(aes(tooltip = plotdata$tooltip),colour="#8bbc21") +
    theme_bw() +
    scale_x_date(labels = date_format("%Y-%m"),date_breaks="1 month")+
    labs(y="",x="") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  tooltip_css <- "background-color:rgba(255,255,255,0.9);color:black;border:1px solid #8bbc21;padding:5px"
  ggiraph(code = print(my_gg),selection_type="single",
          tooltip_extra_css = tooltip_css)
})



output$cor <- renderHighchart({
  plotdata <- df%>%group_by(Date,Dept)%>%
    summarise(Avg_Sales=mean(Weekly_Sales))%>%
    spread(Dept,Avg_Sales)%>%
    select(which(colSums(is.na(.))==0))
  
  hchart(cor(plotdata[,-1]))
})

