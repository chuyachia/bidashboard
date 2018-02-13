### Store insights

output$ui <- renderUI({
  departlist <<- departByStore(mydb,input$choosestore)$Dept
  if (is.null(input$choosestore))
    return()
  
  selectInput("selectdepartments",
              "Select departments",
              choices = departlist,
              selected = departlist,
              multiple=TRUE)
})

chosenstore <- eventReactive(input$update, {
  return(input$choosestore)
}, ignoreNULL = FALSE)

checkAllOrNone <- function(session){
  if(input$selectall == 0) { return(NULL) }
  else if (input$selectall%%2 == 0)
  {
    updateSelectInput(session,"selectdepartments","Select departments",choices=departlist)
  }
  else
  {
    updateSelectInput(session,"selectdepartments","Select departments",choices=departlist,selected=departlist)
  }
}
observeEvent(input$selectall,checkAllOrNone(session))

output$desc <- renderText({
  if (is.null(chosenstore()))
    return()
  tb <- storeInfo(mydb,chosenstore())
  str1 <- paste("Type : ", tb$Type)
  str2 <- paste("Size :",tb$Size)
  HTML(paste('<span style="font-size:150%">',icon("id-card"),"Store ",chosenstore(),'</span>',
             '<span style="float:right">',str1,' ',str2,'</span>'))
})

output$salesdept <- renderHighchart({
  plotdata <- storeSalesByDept(mydb,chosenstore())
  plotdata$Total_Sales[plotdata$Total_Sales<0] <-0
  plotdata <- plotdata%>% filter(Dept%in%isolate(input$selectdepartments))
  alldepartsales <- sum(plotdata$Total_Sales)
  if (nrow(plotdata)==0){
    hc <- highchart()
    hc
  } else {
    tm <- treemap(plotdata, index = c("Dept"),
                  vSize = "Total_Sales",palette= "#8bbc21",
                  type = "comp",draw = FALSE)
    hc <- hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified")%>%
      hc_title(text = paste("Storewide total sales 2010-2012: ","<b>$",format(alldepartsales,digits=12,decimal.mark=".",big.mark=","),"</b>"))%>%
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
  plotdata <- df %>% filter(Store==chosenstore())
  
  plotdata <- plotdata %>% group_by(Dept,Year, Month)%>%
    summarise(Avg_Sales = mean(Weekly_Sales))	%>%
    unite(Date,Year,Month,sep = "-", remove = TRUE)%>% 
    spread(Dept, Avg_Sales)
  plotdata <- as.data.frame(plotdata)
  hc <- highchart() %>% 
    hc_xAxis(categories = plotdata$Date)
  
  for(i in isolate(input$selectdepartments)){
    hc <- hc_add_series(hc,data = round(plotdata[,i],2), name=paste("Dept ",i),showInLegend =FALSE)
  }
  hc
})