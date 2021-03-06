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
  plotdata$Avg_Sales[plotdata$Avg_Sales<0] <-0
  plotdata <- plotdata%>% filter(Dept%in%isolate(input$selectdepartments))
  alldepartsales <- sum(plotdata$Avg_Sales)
  if (nrow(plotdata)==0){
    hc <- highchart()
    hc
  } else {
    tm <- treemap(plotdata, index = c("Dept"),
                  vSize = "Avg_Sales",palette= "#8bbc21",
                  type = "comp",draw = FALSE)
    hc <- hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified")%>%
      hc_title(text = paste("Storewide average weekly sales: ","<b>$",format(alldepartsales,digits=9,decimal.mark=".",big.mark=","),"</b>"))%>%
      hc_tooltip(formatter = JS("function(){
                                var val = parseFloat(this.point.value).toFixed(2);
                                var pct =parseFloat((this.point.value/this.point.series.tree.val)*100).toFixed(2);
                                return ('<b>Department '+this.point.name+'</b><br>'+
                                'Department sales: $' + val+'<br>'+
                                'Percentage in storewide sales: '+pct+'%')
                                }"))
    hc
}
  
})

output$trenddept <- renderHighchart({
  plotdata <- deptSales(mydb,chosenstore())
  plotdata <- plotdata%>%spread(Dept,Weekly_Sales)
  plotdata <- as.data.frame(plotdata)
  hc <- highchart() %>% 
    hc_xAxis(categories = plotdata$Date)
  
  for(i in isolate(input$selectdepartments)){
    hc <- hc_add_series(hc,data = round(plotdata[,i],2), name=paste("Dept ",i),showInLegend =FALSE)
  }
  hc
})