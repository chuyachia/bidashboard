setwd('C:\\Users\\Client\\Desktop\\Dashboard project')

library(shiny)
library(shinydashboard)
library(highcharter)
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
    plotdata <- monthlyAvgSales(df)
    plotdata <- plotdata%>% spread(Year, Avg_Sales)

    hc <- highchart() %>% 
      hc_xAxis(categories = plotdata$Month)%>%
      hc_add_series(name = "2010",data = round(plotdata$`2010`,2))%>%
      hc_add_series(name = "2011",data = round(plotdata$`2011`,2))%>%
      hc_add_series(name = "2012",data = round(plotdata$`2012`,2))
    hc
  })
  plotdata <- storeAvg(mydb)
  plotdata$Store <- paste("Store",plotdata$Store)
  #plotdata <- plotdata%>%mutate(color=colorize(Type,c('#2f7ed8', '#0d233a', '#8bbc21')))
  
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
  
  
  ### Store insights
  departbystore <- departByStore(mydb)
  output$ui <- renderUI({
    if (is.null(input$choosestore))
      return()
    storenum <- strsplit(input$choosestore," ")[[1]][2]
    departlist <<- departbystore$Dept[departbystore$Store==storenum]
    departlist <<- paste("Dept",departlist)
    
    checkboxGroupInput("selectdepartments",
                       "Select Departments",
                       choices = departlist,
                       selected = "Dept 1")
  })
  
  checkAllOrNone <- function(session){
    if(input$selectall == 0) { return(NULL) }
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"selectdepartments","Select Departments",choices=departlist)
    }
    else
    {
      updateCheckboxGroupInput(session,"selectdepartments","Select Departments",choices=departlist,selected=departlist)
    }
  }
  observeEvent(input$selectall,checkAllOrNone(session))
  output$desc <- renderText({
    if (is.null(input$choosestore))
      return()
    storenum <- strsplit(input$choosestore," ")[[1]][2]
    tb <- getStoreInfo(mydb,storenum)
    str1 <- paste("Type : ", tb$Type)
    str2 <- paste("Size :",tb$Size)
    HTML(paste('<h2>',input$choosestore,'</h2>',str1," ",str1))
  })
  
  
}