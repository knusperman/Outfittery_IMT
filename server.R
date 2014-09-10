

CGcOnDayOpen <- getCGcOnDay(openPO, "f_arrival")
monthlyCGint <- 0


shinyServer(function(input, output, session) {
  
  output$test <- renderText(as.numeric(input$ScenarioPF)+as.numeric(input$ScenarioPP)+as.numeric(input$ScenarioPN))
  
  
  forecastdata<- reactive(
    if( input$SForecastexecute!=0){
    
    switch(isolate(input$sRadioB),
           #incoming only
           "1" = {  d <-CGcOnDayOpen
                    d <- d[d[,1] >= isolate(input$SForecastrange[1]) & d[,1]<=isolate(input$SForecastrange[2]),]
                    d <- data.frame("expected Arrival" = d[1], "expected POs"=d[2], "expected Quantity" = d[3])
                    colnames(d)<-c("expected Arrival","expected POs", "expected Quantity" )
                    d
           },
           #outgoing country level
           "2" = {shipped.data(as.Date(isolate(input$SForecastrange[1])),as.Date(isolate(input$SForecastrange[2])))
           },
           #outgoing order level
           
           #full
           "3" = {#selecting the right columns
             df <- inventoryForecast()[[1]]
             df <- df[c(1,2,3,20,21,40,41)]
             setnames(df,1:7,c("date","CO shipped", "Articles shipped", "PO recieved", "Articles recieved", "CO returned", "Articles returned"))
             df
           })
  })
  
  inventoryForecast <- reactive(
    if(input$SForecastexecute != 0){inventoryforecast(as.numeric(isolate(input$SForecastrange[2])-maxdate))}
  )
  
  openreturns <- reactive(getreturndata.open(state128_256,FALSE))
  
  summaryrange <- reactive({
    as.numeric(input$SForecastrange[2]-input$SForecastrange[1], unit ="days")
  })
  futureboxes <- reactive({
    getfutureboxes(getfutureoverview(compareoutgoingforecast.data(isolate(range()))))
  })
  
  MFCrange<- reactive({
    as.numeric(input$MFCForecastingrange[2]-input$MFCForecastingrange[1], unit ="days")
  })
  
  Sinventoryondate <- reactive(inventory[inventory$datum==input$Sinventorydate,])
  
  Scenariolist <- reactive({
    if(input$ScenarioExecute != 0 ){

        scenario(length = as.numeric(isolate(input$ScenarioForecastrange[2])-maxdate,unit ="days") ,amplifier = as.numeric(isolate(input$ScenarioOrderAmplifier)) ,includeopenreturns = isolate(input$IncludeOpenReturns),manualreturns = TRUE  ,prob = c(as.numeric(isolate(input$scenarioslider3))/100, as.numeric(isolate(input$scenarioslider1))/100)) 

    }

  })
  
  
  
  output$DashboardOutgoingTable <- renderDataTable({
     data <- shipped.data(maxdate-7,maxdate)
     data
  },options = list(bFilter =FALSE, bPaginate =FALSE))
  
  output$DashboardOpenReturnsTable <- renderDataTable({
    b <- data.table(openreturns())[, list("orders"= length(articles_returned), "articles"=sum(articles_returned)), by =date_returned]
    b <- b[with(b, order(date_returned))]
    setnames(b,1,"date")
    b<- b[b$date >maxdate & b$date < maxdate+7 ,]
    b$articles <- as.integer(b$articles)
    b
  },options = list(bFilter =FALSE, bPaginate =FALSE))
  
  output$DashboardOutgoingPlot <- renderPlot({
    print(shipped.plot(maxdate-7,maxdate))
  })
  
  output$DashboardOpenReturnsPlot <- renderPlot({
    b <- data.table(openreturns())[, list("orders"= length(articles_returned), "articles"=sum(articles_returned)), by =date_returned]
    b <- b[with(b, order(date_returned))]
    setnames(b,1,"date")
    b<- b[b$date >maxdate & b$date < maxdate+7 ,]
    orders <- data.frame(b)[1:2]
    orders$type <- "order"
    setnames(orders, 2, "value")
    articles <- data.frame(b)[c(1,3)]
    articles$type <- "articles"
    setnames(articles, 2, "value")
    b <- rbind(orders,articles)
    print(ggplot(b, aes(x = date, y = value, group = type,color=type ))+geom_line()+facet_grid(type~.,scales="free")+ylab(""))
  })
  
  output$DeliveryWindowGraph <- renderPlot({print(ggplot(supplier_po_measures[!is.na(supplier_po_measures$edd),], aes(x = w_percentage_of_window, y = purchase_order_id, color = order_type, size = articles))+geom_point()+coord_cartesian(xlim=c(-5,5))+scale_x_continuous(breaks=seq(-5,5,0.25))+geom_vline(x=0, linetype="dashed",color = "red")+geom_vline(x=1, linetype="dashed",color = "red")+theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x=element_text(angle=90,hjust=0))+xlab("Percentage of delivery window")+ylab("PO id"))})
  output$DeliveryWindowData <- renderDataTable({
    data <- data.frame(supplier_po_measures[!is.na(supplier_po_measures$edd), ])[c(-8,-9,-10,-11,-12,-15,-18)]
    suppliernames <- join(data[1],suppliers)[2]
    data[1]<-suppliernames
    setnames(data, 1:13, c("Supplier id", "PO id","earliest delivery date","latest delivery date", "season", "order type", "booking days", "articles","window", "w. mean day in window", "w. percentage of window", "articles before window","articles after window"))
  data
    })
  
  output$forecastdata <- renderDataTable({
    forecastdata()  
  })
  
  output$supplieranalysistable <- renderDataTable({
    a <- data.frame(supplier_measures)[c(1,2,3,7,9,10,12,15:18)]
    a$avg_window <- round(a$avg_window,2)
    a$avg_bookingdays <- round(a$avg_bookingdays,2)
    a$avg_w_mean_day_in_window <- round(a$avg_w_mean_day_in_window,2)
    a$sd_w_mean_day_in_window <- round(a$sd_w_mean_day_in_window,2)
    a$ArticleShare <- round(a$ArticleShare,2)
    suppliernames <- join(a[1],suppliers)[2]
    a[1] <- suppliernames
    setnames(a, 1:11, c("Supplier","#PO partitions", "avg. # bookingdays", "avg. window size", "articles", "avg. w. mean day in window", "sd w. mean day in window","Articles too early","Articles too late","#PO not in window", "Share of Articles [%]"))
    a
    
  },options = list(bFilter =FALSE))
  

  output$downloadOpenPOData <- downloadHandler(
    filename = function() { 
      paste("openPO", '.csv', sep='') 
    },
    content = function(file) {
      write.csv({  data<-openPOshaped
                   setnames(data,2,"supplier_id")
                   suppliernames <- join(data[2],suppliers)[2]
                   data[2]<-suppliernames
                   setnames(data,2,"Supplier")
                   data}, file)
    }
  )
 
  output$downloadingincCGData <- downloadHandler(
    filename = function() { 
      paste("incomingCG", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(  {out <- CGcOnDayOpen
                  setnames(out, 1:3, c("expected Arrival", "# PO","# Articles" ))
                  setnames(out, 4:21, lev[1:18])
                  out}, file)
    }
  )
  
  output$downloadingingForecastSummary <- downloadHandler(
    filename = function() { 
      paste("forecastsummary", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(  { forecastdata()}, file)
    }
  )
  
  output$returnsOpenReturnsDownload <- downloadHandler(
    filename = function() { 
      paste("openreturns", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(  {    y <- openreturns()
                       
                       y<- y[c(1,3,7,8,29,46,47,27)]
                       setnames(y,1:8,c("order id","Country","Articles shipped","Date shipped","eArticles returned ","eReturn time", "eDate returned ", "eReturn Class"))
                       y[y$`eDate returned` > maxdate, ]
                   }, file)
    }
  )
  output$Flowdownload <- downloadHandler(
    filename = function() { 
      paste("inventoryflows", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(inventoryForecast7reshape(), file)
    }
  )
  
  output$openPOTable<- renderDataTable({
  data<-openPOshaped
  setnames(data,2,"supplier_id")
  suppliernames <- join(data[2],suppliers)[2]
  data[2]<-suppliernames
  setnames(data,2,"Supplier")
  data
    })
  output$welcome <- renderText({hello()[2]})
  output$language <- renderText(hello()[1])
  hello <- reactive(
    switch(sample(1:19,size=1),
    "1"=  c("French" ,"Bonjour" ),
    "2"= c( "Spanish" , "Hola" ),
    "3"=  c("Italian" , "Bon Giorno" ),
    "4"=  c("German" , "Guten Tag "),
    "5"=  c("Chinese" , "Ni hao "),
    "6"=  c("Irish" ," Dia Duit "),
    "7"=  c( "Hindi" , "Namaste "),
    "8"=  c( "Russian" , "Zdravstvuite "),
    "9"=  c( "Greek" , "Yia sou (Ya-soo) "),
    "10"=  c("Czech" , "Dobry rano "),
    "11"=  c("Japanese" , "Ohayou gozaimasu"),
    "12"=  c( "Hebrew" , "Shalom" ),
    "13"=  c( "Arabic,based languages" , "Marhabah" ),
    "14"= c("Swedish" , "Hej "),
    "15"= c("Dutch" , "Goedendag") ,
    "16"=   c("Swahili" , "Jambo" ),
    "17"=   c("Vietnamese" , "Chao "),
    "18"=   c("Korean" , "Ahn nyeong ha se yo "),
    "19"=   c("Portuguese" , "Bom dia ")))
  

  output$InventoryPlot <- renderPlot({
    meltinventory <- melt(inventory_c,id.vars = "date")
    switch(length(input$CGInventoryHistoryselect),
           "1" = y <- meltinventory[meltinventory$variable==input$CGInventoryHistoryselect[1],],
           "2" = y <- meltinventory[meltinventory$variable==input$CGInventoryHistoryselect[1]|meltinventory$variable==input$CGInventoryHistoryselect[2],],
           "3" = y <- meltinventory[meltinventory$variable==input$CGInventoryHistoryselect[1]|meltinventory$variable==input$CGInventoryHistoryselect[2]|meltinventory$variable==input$CGInventoryHistoryselect[3],],
           "4" = y <- meltinventory[meltinventory$variable==input$CGInventoryHistoryselect[1]|meltinventory$variable==input$CGInventoryHistoryselect[2]|meltinventory$variable==input$CGInventoryHistoryselect[3]|meltinventory$variable==input$CGInventoryHistoryselect[4],],
           "5" = y <- meltinventory[meltinventory$variable==input$CGInventoryHistoryselect[1]|meltinventory$variable==input$CGInventoryHistoryselect[2]|meltinventory$variable==input$CGInventoryHistoryselect[3]|meltinventory$variable==input$CGInventoryHistoryselect[4]|meltinventory$variable==input$CGInventoryHistoryselect[5],],
           "6" = y <- meltinventory[meltinventory$variable==input$CGInventoryHistoryselect[1]|meltinventory$variable==input$CGInventoryHistoryselect[2]|meltinventory$variable==input$CGInventoryHistoryselect[3]|meltinventory$variable==input$CGInventoryHistoryselect[4]|meltinventory$variable==input$CGInventoryHistoryselect[5]|meltinventory$variable==input$CGInventoryHistoryselect[6],]
    )
    
    print(ggplot(y[y$date >= input$inventoryrange[1] & y$date<=input$inventoryrange[2] ,], aes(x=date, y = value, group = variable, color = variable))+geom_line()+facet_grid(variable ~ ., scales="free"))
    
  })
  output$InventoryDataTable <-renderDataTable({
    inventory_c[inventory_c$date>=input$inventoryrange[1]&inventory_c$date<=input$inventoryrange[2],]
  })
  HelperForecast<- reactive({switch(input$sRadioB,
                                       "1"=c(" Daily"," fast"," Linear Regression","Displays a forecast of the incoming Purchase Orders in the specified range. See Incoming tab for other views on the data."),
                                       "2"=c(" Daily, Countrywise" , " fast", "STL, HoltWinters Exponential Smoothing","Displays a forecast of the outgoing Customer Orders in the specified range split by country. See Outgoing tab for other views on the data as well as historic performance."),
                                       "3"=c(" Daily"," slow (~30sec/forecast day)","All","Simulates inventory changes in the specified range. Based on the outgoing forecast, every order sampled out of empirical data. The returns for every order are forecasted and also their return time. Because of this the forecast table displays the last rows without shipping, but with returns. The forecasted incoming POs are included as well. See Return or Inventroy tab for other views on the data.")
  )})
  output$HelperForecastLevel <- renderText({ a <- HelperForecast()
                                             a[1]})
  
  output$HelperForecastCalculationTime <- renderText(HelperForecast()[2])
  
  output$HelperForecastModel <- renderText(HelperForecast()[3])
  
  output$HelperForecastDescription <- renderText(HelperForecast()[4])
   
  output$forecastplot <- renderPlot({
    if( input$SForecastexecute!=0){

      switch(isolate(input$sRadioB),
             #incoming only
             "1" = {  d <- CGcOnDayOpen
                      d <- d[d[,1] >= isolate(input$SForecastrange[1])& d[,1]<=isolate(input$SForecastrange[2]),]
                      d <- data.frame("expected Arrival" = d[1], "expected POs"=d[2], "expected Quantity" = d[3])
                      colnames(d)<-c("expected Arrival","expected POs", "expected Quantity" )
                      print(ggplot(d, aes(x = `expected Arrival`, y = `expected Quantity`))+geom_line(col="red"))
             },
             #outgoing country level
             "2" = {print(shipped.plot(as.Date(isolate(input$SForecastrange[1])), as.Date(isolate(input$SForecastrange[2]))))
             },
             #full
             "3" = {#selecting the right columns
               df <- inventoryForecast()[[1]]
               df <- df[c(1,3,21,41)]
               length <- inventoryForecast()[[8]]
               meltdf <- melt(df, id.vars=1)
               meltdf <- meltdf[meltdf$date <=maxdate+length, ]
               grid <- expand.grid("date" = seq(maxdate, maxdate+length, by ="days"))
              meltdf <- join(grid, meltdf)
              meltdf[is.na(meltdf$value)]$value<-0
              print(ggplot(meltdf, aes(x=date, y = value))+geom_line()+facet_grid(variable~.,scales="free"))
             })
    }
    
    
  })

  

  output$MFCdata <- renderDataTable(shipped.data(input$MFCForecastingrange[1],input$MFCForecastingrange[2]))

  output$outgoingforecastplotcomparison <- renderPlot({
    dfForecast <- compareoutgoingforecast.data(MFCrange())
    stlsub <-dfForecast[c(1,11,12)]
    holtsub <- dfForecast[c(6,11,12)]
    setnames(holtsub, 1, "value")
    setnames(stlsub, 1 , "value")
    dfForecastsub <- rbind(stlsub, holtsub)
    #ADJUST Country Number?
    dfForecastsub$algorithm <-c( rep("STL", dim(dfForecast)[1]), rep("HoltWinters", dim(dfForecast)[1] ))
    dfForecastsub$algorithm <- factor(dfForecastsub$algorithm)
    colnames(dfForecastsub) <- c("value", "day", "country", "algorithm")
    plot <- ggplot(dfForecastsub, aes(x = day, y = value, Group = algorithm, col = algorithm )) + geom_line()+ facet_grid( country~ ., scales="free")
    print(plot)
  })
  
  output$thelastxdays <- renderPlot({
    series <- switch(input$countryinput,
          #ADJUST
                    "1"=DEtimeseries,
                    "2"=ATtimeseries,
                    "3"=CHtimeseries,
                    "4"=NLtimeseries,
                    "5"=DKtimeseries,
                    "6"=LUtimeseries,
                    "7"=SEtimeseries,
                    "8"=Ttimeseries)
    country <- switch(input$countryinput,
                      "1" ="DE",
                      "2" ="AT",
                      "3" ="CH",
                      "4" ="NL",
                      "5" ="DK",
                      "6" ="LU",
                      "7" ="SE",
                      "8" ="DE") # total time series. do not change DE here
          #/ADJUST
    h <- as.numeric(input$thelastxdate[2]-input$thelastxdate[1], units ="days")
    plot <- decomp2(series, frequency = 7, country = country,h )
    print(plot)
  })

output$poCGdata <- renderDataTable({
  out <- CGcOnDayOpen
  setnames(out, 1:3, c("expected Arrival", "# PO","# Articles" ))
  setnames(out, 4:21, lev[1:18])
  out
})
output$thelastxdaysdata <- renderDataTable({
  #ADJUST
  series <- switch(input$countryinput,
                   "1"=DEtimeseries,
                   "2"=ATtimeseries,
                   "3"=CHtimeseries,
                   "4"=NLtimeseries,
                   "5"=DKtimeseries,
                   "6"=LUtimeseries,
                   "7"=SEtimeseries,
                   "8"=Ttimeseries)
  country <- switch(input$countryinput,
                    "1" ="DE",
                    "2" ="AT",
                    "3" ="CH",
                    "4" ="NL",
                    "5" ="DK",
                    "6" ="LU",
                    "7" ="SE",
                    "8" ="DE") 
  
  #/ADJUST
  h <- as.numeric(input$thelastxdate[2]-input$thelastxdate[1], units ="days")
  decomp2.data(series, frequency=7, country =country, h)
  
})
output$returndistributionsplitbycountry <- renderPlot(
  print(ggplot(outgoing_basketsubset[outgoing_basketsubset$isreturned==TRUE,], aes(x=datediff_shipped_returned, fill = shipping_country))+geom_density(alpha=0.3) + scale_x_continuous(breaks=seq(0,40,5)) +coord_cartesian(xlim = c(0,50), ylim = c(0,0.15)) + facet_grid( shipping_country~ quarter) + scale_y_continuous(breaks=seq(0,0.15,0.03))+xlab("Difference in days between shipment and return")
))
output$returndistributionsplitbyweekday <- renderPlot(
  print(ggplot(outgoing_basketsubset[outgoing_basketsubset$isreturned==TRUE&outgoing_basketsubset$weekday!="Saturday"&outgoing_basketsubset$weekday!="Sunday",], aes(x=datediff_shipped_returned))+geom_histogram(aes(y=..density..),fill="cornsilk", col="grey60", binwidth=1)+coord_cartesian(xlim = c(0,34))+facet_grid(weekday~.)+scale_x_discrete(breaks=seq(0,30,1))+xlab("Difference in days between shipment and return")
))
output$customeragedistribution <- renderPlot(
  print(ggplot(outgoingdata, aes(x=customer_age, fill = shipping_country)) + geom_density(alpha=0.3) + scale_x_continuous(breaks=seq(0,70,5)) +coord_cartesian(xlim = c(0,70), ylim = c(0,0.1)) + facet_grid( quarter ~ shipping_country) + scale_y_continuous(breaks=seq(0,0.1,0.02))
))

output$scenarioslider3 <- renderUI({
  sliderInput("scenarioslider3", "Probability for a not returned box", min = 0,  max = 100, value = 100-input$scenarioslider1-input$scenarioslider2)  
})

output$returnprobs <- renderPlot({
  rr <- returnratio
  rr$class <- rr$Var2
  levels(rr$class)<- c("Full return", "No return", "Partial return")
  print(ggplot(rr, aes(x=Var1, y=Freq, fill = class, group = class, label = round(Freq,2)))+geom_area(alpha =0.6)+scale_y_continuous(breaks=seq(0,1,0.1))+xlab("time")+ylab("probability"))
})

output$probalarm <- renderText(if(as.numeric(input$scenarioslider1)+as.numeric(input$scenarioslider2)+as.numeric(input$scenarioslider3)>100){"STOP! SUM MUST BE 100!"})

  output$ScenarioOverviewTable <- renderDataTable({
      x <- Scenariolist()[[1]]
      if(is.data.frame(x)){
    
      x <- x[c(1,2,3,20,21)]
      setnames(x,1:5,c("date","CO shipped", "Articles shipped", "CO returned", "Articles returned"))
      x}else{
        data.frame("warning"="No scenario calculated")
      }
  })
output$ScenarioSidebar <- renderDataTable({
  x <- Scenariolist()
  x <- x[[2]]
  if(is.data.frame(x)){
  setnames(x, 1:3, c("country", "date","shipped"))
  x}}, options = list( iDisplayLength =10, aoColumns = list(list(bSearchable = TRUE), list(bSearchable =FALSE), list(bSearchable=FALSE)))
)


  
output$saleschanneldistributionbasketsize <- renderPlot({
  table_bsize_sc$`Sales Channel` =factor(table_bsize_sc$Var2, levels=rev(levels(table_bsize_sc$Var2)))
  print(ggplot(table_bsize_sc, aes(x = Var1, y=Freq, fill = `Sales Channel`))+geom_area(alpha=0.6)+geom_line(aes(x=Var1, y=cumsum))+scale_y_continuous(breaks = seq(0,1,0.1), name = "propability")+scale_x_continuous(breaks=1:15, name="basket size")
  )}
  )

output$saleschanneldistributioncountry <- renderPlot({
  
})
output$CGmonthchanges <- renderPlot({
  if(monthlyCGint == 0){
    #lazy
    monthlyCGint <- 1
    monthlyCG <- data.table(outgoingdata)[, 
                                          list(c=length(articles_shipped),
                                               articles_s_m=mean(articles_shipped),
                                               articles_s_sd=sd(articles_shipped),
                                               articles_R_m=mean(articles_returned),
                                               articles_R_sd = sd(articles_returned),
                                               c_Schuhe_m=mean(c_Schuhe),
                                               c_Schuhe_sd=sd(c_Schuhe),
                                               c_Outdoor_m = mean(c_Outdoor),
                                               c_Outdoor_sd = sd(c_Outdoor),
                                               c_Kleinaccessoires_m = mean(c_Kleinaccessoires) ,
                                               c_Kleinaccessoires_sd = sd(c_Kleinaccessoires),
                                               c_schlafbade_m = mean(c_schlafbade),
                                               c_schlafbade_sd = sd(c_Kleinaccessoires),
                                               c_Konfektion_m = mean(c_Konfektion),
                                               c_Konfektion_sd = sd(c_Konfektion),
                                               c_Schuhpflege_m = mean(c_Schuhpflege),
                                               c_Schuhpflege_sd = sd(c_Schuhpflege),
                                               c_tuecherschals_m = mean(c_tuecherschals),
                                               c_tuecherschals_sd = sd(c_tuecherschals),
                                               c_Guertel_m = mean(c_Guertel),
                                               c_Guertel_sd = sd(c_Guertel), 
                                               c_Stiefel_m = mean(c_Stiefel),
                                               c_Stiefel_sd = sd(c_Stiefel), 
                                               c_KrawattenFliegen_m = mean(c_KrawattenFliegen),
                                               c_KrawattenFliegen_sd = sd(c_KrawattenFliegen),
                                               c_Formalwearaccessories_m = mean(c_Formalwearaccessories),
                                               c_Formalwearaccessories_sd = sd(c_Formalwearaccessories),
                                               c_cooperations_m = mean(c_cooperations),
                                               c_cooperations_sd = sd(c_cooperations),  
                                               c_Oberteile_m = mean(c_Oberteile),
                                               c_Oberteile_sd = sd(c_Oberteile), 
                                               c_Handschuhe_m = mean(c_Handschuhe),
                                               c_Handschuhe_sd = sd(c_Handschuhe),
                                               c_Electroniccases_m = mean(c_Electroniccases),
                                               c_Electroniccases_sd = sd(c_Electroniccases),
                                               c_Kopfaccessoires_m = mean(c_Kopfaccessoires),
                                               c_Kopfaccessoires_sd = sd(c_Kopfaccessoires),
                                               c_Hosen_m = mean(c_Hosen),
                                               c_Hosen_sd = sd(c_Hosen),
                                               c_Taschen_m = mean(c_Taschen),
                                               c_Taschen_sd = sd(c_Taschen), 
                                               c_Unterbekleidung_m = mean(c_Unterbekleidung),
                                               c_Unterbekleidung_sd = sd(c_Unterbekleidung),
                                               c_N_m = mean(c_N),
                                               c_N_sd = sd(c_N)
                                          ), by=outgoingdata$date_shipped_month] 
    
    monthlyCG_means <- data.frame(monthlyCG[,c(1, seq(7,46,2)), with=FALSE])
    setnames(monthlyCG_means,2:21,lev)
    setnames(monthlyCG_means,1,"month")
    
    x <- melt(monthlyCG_means,id.vars = "month")

  }
  #if einbauen
  switch(length(input$CGselect),
    "1" = y <- x[x$variable==input$CGselect[1],],
    "2" = y <- x[x$variable==input$CGselect[1]|x$variable==input$CGselect[2],],
    "3" = y <- x[x$variable==input$CGselect[1]|x$variable==input$CGselect[2]|x$variable==input$CGselect[3],],
    "4" = y <- x[x$variable==input$CGselect[1]|x$variable==input$CGselect[2]|x$variable==input$CGselect[3]|x$variable==input$CGselect[4],],
    "5" = y <- x[x$variable==input$CGselect[1]|x$variable==input$CGselect[2]|x$variable==input$CGselect[3]|x$variable==input$CGselect[4]|x$variable==input$CGselect[5],],
    "6" = y <- x[x$variable==input$CGselect[1]|x$variable==input$CGselect[2]|x$variable==input$CGselect[3]|x$variable==input$CGselect[4]|x$variable==input$CGselect[5]|x$variable==input$CGselect[6],]
)
  print(ggplot(y, aes(x=month, y=value, group=variable))+geom_line()+facet_grid(variable ~ ., scales="free")+ylab("Mean article count in CO"))
})
output$returnTable <- renderDataTable({
  #agg on day
  i <- inventoryForecast()

  i<- i[[1]] 
  if(is.data.frame(i)){
    i <- i[c(1,40:57)]
    setnames(i,4:19, lev2)
    i
  }else{
    data.frame("warning" =" Make a forecast first")
  }

})
output$returnOpenReturns <- renderDataTable({
  y <- openreturns()
  
  y<- y[c(1,3,7,8,29,46,47,27)]
  setnames(y,1:8,c("order id","Country","Articles shipped","Date shipped","eArticles returned ","eReturn time", "eDate returned ", "eReturn Class"))
  y[y$`eDate returned` > maxdate, ]
})

inventoryForecast7reshape <- reactive({
  i <- inventoryForecast()[[7]]
  if(is.data.frame(i)){
    lastweekofinventory <- inventory[inventory$datum>=(maxdate-7)&inventory$datum!=maxdate,1:21]
    setnames(lastweekofinventory,2:21,lev)
    setnames(lastweekofinventory,1,"date")
    setnames(i,2:21,lev)
    setnames(i,1,"date")
    i <- rbind(lastweekofinventory, i)
    length <-  inventoryForecast()[[8]]
    i[i$date <= maxdate+length,]
  }else{
    data.frame("warning"="Make a forecast first")
  } 
})
output$InventoryFlowTable <- renderDataTable({
inventoryForecast7reshape()
})
output$Flowplot <- renderPlot({
  x <- inventoryForecast7reshape()
  if(dim(x)[1]>1){
   
x <- melt(x, id.vars=1)
  switch(length(input$CGInventoryFlowselect),
         "1" = y <- x[x$variable==input$CGInventoryFlowselect[1],],
         "2" = y <- x[x$variable==input$CGInventoryFlowselect[1]|x$variable==input$CGInventoryFlowselect[2],],
         "3" = y <- x[x$variable==input$CGInventoryFlowselect[1]|x$variable==input$CGInventoryFlowselect[2]|x$variable==input$CGInventoryFlowselect[3],],
         "4" = y <- x[x$variable==input$CGInventoryFlowselect[1]|x$variable==input$CGInventoryFlowselect[2]|x$variable==input$CGInventoryFlowselect[3]|x$variable==input$CGInventoryFlowselect[4],],
         "5" = y <- x[x$variable==input$CGInventoryFlowselect[1]|x$variable==input$CGInventoryFlowselect[2]|x$variable==input$CGInventoryFlowselect[3]|x$variable==input$CGInventoryFlowselect[4]|x$variable==input$CGInventoryFlowselect[5],],
         "6" = y <- x[x$variable==input$CGInventoryFlowselect[1]|x$variable==input$CGInventoryFlowselect[2]|x$variable==input$CGInventoryFlowselect[3]|x$variable==input$CGInventoryFlowselect[4]|x$variable==input$CGInventoryFlowselect[5]|x$variable==input$CGInventoryFlowselect[6],]
  )
print(ggplot(y, aes(x = date, y = value, color = variable))+geom_line()+facet_grid(variable ~., scales="free")+geom_vline(x=as.numeric(maxdate), linetype="dashed"))}
else{print(qplot(y=1)+geom_text(label="                              No data available",color = "red"))
  
}
})


output$cg1c <- renderText({Sinventoryondate()[,2]})
output$cg1v <- renderText({Sinventoryondate()[,22]})
output$cg2c <- renderText({Sinventoryondate()[,3]})
output$cg2v <- renderText({Sinventoryondate()[,23]})
output$cg3c <- renderText({Sinventoryondate()[,4]})
output$cg3v <- renderText({Sinventoryondate()[,24]})
output$cg4c <- renderText({Sinventoryondate()[,5]})
output$cg4v <- renderText({Sinventoryondate()[,25]})
output$cg5c <- renderText({Sinventoryondate()[,6]})
output$cg5v <- renderText({Sinventoryondate()[,26]})
output$cg6c <- renderText({Sinventoryondate()[,7]})
output$cg6v <- renderText({Sinventoryondate()[,27]})
output$cg7c <- renderText({Sinventoryondate()[,8]})
output$cg7v <- renderText({Sinventoryondate()[,28]})
output$cg8c <- renderText({Sinventoryondate()[,9]})
output$cg8v <- renderText({Sinventoryondate()[,29]})
output$cg9c <- renderText({Sinventoryondate()[,10]})
output$cg9v <- renderText({Sinventoryondate()[,30]})
output$cg10c <- renderText({Sinventoryondate()[,11]})
output$cg10v <- renderText({Sinventoryondate()[,31]})
output$cg11c <- renderText({Sinventoryondate()[,12]})
output$cg11v <- renderText({Sinventoryondate()[,32]})
output$cg12c <- renderText({Sinventoryondate()[,13]})
output$cg12v <- renderText({Sinventoryondate()[,33]})
output$cg13c <- renderText({Sinventoryondate()[,14]})
output$cg13v <- renderText({Sinventoryondate()[,34]})
output$cg14c <- renderText({Sinventoryondate()[,15]})
output$cg14v <- renderText({Sinventoryondate()[,35]})
output$cg15c <- renderText({Sinventoryondate()[,16]})
output$cg15v <- renderText({Sinventoryondate()[,36]})
output$cg16c <- renderText({Sinventoryondate()[,17]})
output$cg16v <- renderText({Sinventoryondate()[,37]})
output$cg17c <- renderText({Sinventoryondate()[,18]})
output$cg17v <- renderText({Sinventoryondate()[,38]})
output$cg18c <- renderText({Sinventoryondate()[,19]})
output$cg18v <- renderText({Sinventoryondate()[,39]})
output$cg19c <- renderText({Sinventoryondate()[,20]})
output$cg19v <- renderText({Sinventoryondate()[,40]})
output$cg20c <- renderText({Sinventoryondate()[,21]})
output$cg20v <- renderText({Sinventoryondate()[,41]})
})