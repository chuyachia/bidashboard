setwd('C:\\Users\\Client\\Desktop\\Dashboard project')

library(shiny)
library(shinydashboard)
library(highcharter)
library(treemap)
source('./Project/LoadData.R',local = TRUE)

server <- function(input, output,session) {
  mydb <- connectToDB()
  df <- separateDate(mydb,"train")
  ### KPI
  output$avgweeklysales <- renderValueBox({
    val <- avgWeeklySales(mydb)
    valueBox(paste0(val,"$"), "average weekly sales", icon = icon("list"),
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
    plotdata <- df %>% group_by(Year,Month)%>%
      summarise(Avg_Sales = mean(Weekly_Sales))%>% 
      spread(Year, Avg_Sales)

    hc <- highchart() %>% 
      hc_xAxis(categories = plotdata$Month)%>%
      hc_add_series(name = "2010",data = round(plotdata$`2010`,2))%>%
      hc_add_series(name = "2011",data = round(plotdata$`2011`,2))%>%
      hc_add_series(name = "2012",data = round(plotdata$`2012`,2))
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
                      name = "Weekly average sales",showInLegend = FALSE)
      hc
    })
    output$leastperform <- renderHighchart({
      plotdata <- plotdata %>%top_n(-10,Avg_Sales)
      hc <- highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = plotdata$Store)%>%
        hc_add_series(data = round(plotdata$Avg_Sales,2), color = "#8bbc21",
                      name = "Weekly average sales",showInLegend = FALSE)
      hc
    })
  })()

  
  
  ### Store insights
  
  output$ui <- renderUI({
    departlist <- departByStore(mydb,input$choosestore)$Dept
    if (is.null(input$choosestore))
      return()
    
    selectInput("selectdepartments",
                       "Select Departments",
                       choices = departlist,
                       selected = departlist,
                      multiple=TRUE)
  })
  
  
  
  checkAllOrNone <- function(session){
    if(input$selectall == 0) { return(NULL) }
    else if (input$selectall%%2 == 0)
    {
      updateSelectInput(session,"selectdepartments","Select Departments",choices=departlist)
    }
    else
    {
      updateSelectInput(session,"selectdepartments","Select Departments",choices=departlist,selected=departlist)
    }
  }
  observeEvent(input$selectall,checkAllOrNone(session))
  
  output$desc <- renderText({
    if (is.null(input$choosestore))
      return()
    tb <- storeInfo(mydb,input$choosestore)
    str1 <- paste("Type : ", tb$Type)
    str2 <- paste("Size :",tb$Size)
    HTML(paste('<span style="font-size:150%">',icon("id-card"),"Store ",input$choosestore,'</span>',
               '<br/>',str1,'<br/>',str2))
  })
  
  output$salesdept <- renderHighchart({
    plotdata <- storeSalesByDept(mydb,input$choosestore)
    plotdata$Total_Sales[plotdata$Total_Sales<0] <-0
    plotdata <- plotdata%>% filter(Dept%in%input$selectdepartments)
    alldepartsales <- sum(plotdata$Total_Sales)
    if (nrow(plotdata)==0){
      hc <- highchart()
      hc
    } else {
      tm <- treemap(plotdata, index = c("Dept"),
                    vSize = "Total_Sales",
                    type = "comp",draw = FALSE)
      hc <- hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified")%>%
        hc_title(text = paste("Storewide sales 2010-2012: ","<b>",alldepartsales,"$</b>"))%>%
        hc_tooltip(formatter = JS("function(){
                                  var pct =parseFloat((this.point.value/this.point.series.tree.val)*100).toFixed(2);
                                  return ('<b>Department '+this.point.name+'</b><br>'+
                                  'Department Sales:' + this.point.value+'$<br>'+
                                  'Percentage in storewide sales: '+pct+'%')
    }"))
    hc
    }

  })
  
  output$trenddept <- renderHighchart({
    plotdata <- df %>% filter(Store==input$choosestore)
    
    plotdata <- plotdata %>% group_by(Dept,Year, Month)%>%
      summarise(Avg_Sales = mean(Weekly_Sales))	%>%
      unite(Date,Year,Month,sep = "-", remove = TRUE)%>% 
      spread(Dept, Avg_Sales)
    plotdata <- as.data.frame(plotdata)
    hc <- highchart() %>% 
      hc_xAxis(categories = plotdata$Date)
    
    for(i in input$selectdepartments){
      hc <- hc_add_series(hc,data = round(plotdata[,i],2), name=paste("Dept ",i),showInLegend =FALSE)
    }
    hc
  })

}