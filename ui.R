
shinyUI(navbarPage("Inventory Management Tool",
                   
                   tabPanel( 
                     img(src="outfitteryshrinked.png", height=55, width = 110),
                     #Landing 
                     #Dashboard
                     tabsetPanel(
                      tabPanel("Dashboard",
                               
                               fluidRow(column(6,wellPanel(h3(textOutput("welcome")),
                                                           br(),textOutput("language"),br(),
                                                            paste("Today is the", Sys.Date()), br(),br(),
                                                            paste("The data loaded is from the", maxdate), br(),
                                                            "The data is ", code(Sys.Date()-maxdate), "days old"
                                                            )),
                                        column(6,wellPanel(h4("Incoming"),br(),
                                                           "There are currently ", code(length(unique(openPO$purchase_order_id))), "open Purchase Orders in the data", br(),
                                                           code(length(unique(openPO[openPO$earliest_delivery_date<=Sys.Date() & openPO$latest_delivery_date>=maxdate,]$purchase_order_id))), "delivery windows are open for the ", maxdate,br(),
                                                           code(sum(ifelse(openPO[!is.na(openPO$pastwindow),]$pastwindow>0,1,0))), "delivery windows are closed, but we are still missing articles", br(),
                                                           "These missing articles sum up to",code(sum(openPOshaped[openPOshaped$`Latest DD` < maxdate, ]$`Open quantity`)),
                                                           "and are added to the forecast of the next seven days."
                                                           
                                                           ))
                               ),
                               fluidRow(
                                 
                               
                                 
                                 column(6,wellPanel(h4("Outgoing"),
                                                    "The shipments of the last days:",
                                    tabsetPanel(
                                     tabPanel("Plot", plotOutput("DashboardOutgoingPlot")
                                    
                                     ),
                                   
                                     tabPanel( "Data",
                                               dataTableOutput("DashboardOutgoingTable")
                                     )
                                    )
                                                    
                                                    
                                                    )),
                                        
                                 
                                 column(6,wellPanel(h4("Returns"),
                                       "The expected returns for the next days (state 128 or 256): ",
                                          tabsetPanel(
                                            tabPanel("Plot",
                                                     plotOutput("DashboardOpenReturnsPlot")
                                            ),
                                          tabPanel("Data",
                                                   dataTableOutput("DashboardOpenReturnsTable")
                                                   ))
                                      
                                        )
                                 )
                                 
                                
                                   
                               ),
                               fluidRow(
                                 column(12,wellPanel(h4("Inventory History"),br(),
                                                     dateInput("Sinventorydate",label = "Date",value = max(inventory$datum),max =max(inventory$datum), min = min(inventory$datum)),
                                                     maxdate," on stock:",br(),
                                                     
                                                    fluidRow(column(3,wellPanel("Shoes",br(),(textOutput("cg1c")))),
                                                            column(3,wellPanel("Outdoor",br(),(textOutput("cg2c")))),
                                                            column(3,wellPanel("Accessories",br(),(textOutput("cg3c")))),
                                                            column(3,wellPanel("Sleep-/Swimwear",br(),(textOutput("cg4c"))))),
                                                    fluidRow(column(3,wellPanel("Blazers",br(),(textOutput("cg5c")))),
                                                             column(3,wellPanel("Shoe care",br(),(textOutput("cg6c")))),
                                                             column(3,wellPanel("Neckwear",br(),(textOutput("cg7c")))),
                                                             column(3,wellPanel("Belts",br(),(textOutput("cg8c"))))),
                                                    fluidRow(column(3,wellPanel("Boots",br(),(textOutput("cg9c")))),
                                                             column(3,wellPanel("Ties",br(),(textOutput("cg10c")))),
                                                             column(3,wellPanel("Formalwearaccessories",br(),(textOutput("cg11c")))),
                                                             column(3,wellPanel("Cooperations ",br(),(textOutput("cg12c"))))),
                                                    fluidRow(column(3,wellPanel("Tops",br(),(textOutput("cg13c")))),
                                                             column(3,wellPanel("Gloves",br(),(textOutput("cg14c")))),
                                                             column(3,wellPanel("Electroniccases",br(),(textOutput("cg15c")))),
                                                             column(3,wellPanel("Headaccessories",br(),(textOutput("cg16c"))))),
                                                    fluidRow(column(3,wellPanel("Pants",br(),(textOutput("cg17c")))),
                                                             column(3,wellPanel("Bags",br(),(textOutput("cg18c")))),
                                                             column(3,wellPanel("Underwear",br(),(textOutput("cg19c")))),
                                                             column(3,wellPanel("NA",br(),(textOutput("cg20c")))))
                                                     
                                                     ))
                                 )
                               
                             
                      ),
                      tabPanel("Forecast",
                              sidebarLayout(
                                mainPanel(
                                  #plot stuff
                                  
                                  plotOutput("forecastplot"),
                                    dataTableOutput("forecastdata")
                                  ),
                                sidebarPanel("Settings",
                                  #settings
                                  #type of output
                                  #forecast horizon
                                  "The 2nd and 3rd option is computationally expensive. Every day takes more than 30 sec. to calculate.",
                                  dateRangeInput("SForecastrange", label = "Range", start = maxdate, min =maxdate,end= maxdate+1),
                                  radioButtons("sRadioB", label = "Select Level", choices = c("Forecast on Purchase Orders (Incoming only)" = "1", "Forecast on Country level (Outgoing only)" = "2", "Forecast Inventory (Inbound, Outgoing and Returns)" ="3" ), "1"),
                                  actionButton("SForecastexecute", label = "Start"),
                                  br(),
                                  strong("Level:"), textOutput("HelperForecastLevel"), br(), 
                                  strong("Calculation time:"), textOutput("HelperForecastCalculationTime"),br(),
                                  strong("Model:"), textOutput("HelperForecastModel"),br(),
                                  strong("Description:"),textOutput("HelperForecastDescription"),br(),
                                  downloadButton("downloadingingForecastSummary")
                                  
                                  
                             
                                )
                                  )
                              
                              
                     
                      ),
                      tabPanel("Scenario",
                               sidebarLayout(
                                 mainPanel(
                                   dataTableOutput("ScenarioOverviewTable")
                                   
                                   
                                 ),
                                 sidebarPanel("Settings",
                                            
                                              "Choose the type of forecast you want to make. Other views on the forecasted data are available through the navigation bar",br(),
                                              dateRangeInput("ScenarioForecastrange", label = "Range", start = maxdate, min =maxdate,end= maxdate+1),
                                              
                                              sliderInput("ScenarioOrderAmplifier", label = "CO in relation to forecast", min = 0, max = 200,value = 100,step=1,format = "0 %"),
                                              checkboxInput("IncludeOpenReturns", label ="Include returns from recently shipped CO",value = FALSE),
                                             # textInput("ScenarioPF","Probability for a completely returned Box", value = round(returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "F", ]$Freq/ sum(returnratio[returnratio$Var1 ==paste(year(today()),quarters(today()) ,sep = " ") & returnratio$Var2 != "N", ]$Freq),2)),
                                            #  textInput("ScenarioPP","Probability for a partially returned Box", value = round(1-returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "F", ]$Freq /sum(returnratio[returnratio$Var1 ==paste(year(today()),quarters(today()) ,sep = " ") & returnratio$Var2 != "N", ]$Freq)-returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "N", ]$Freq,2)),
                                            #  textInput("ScenarioPN","Probability for a not returned Box", value = round(returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "N", ]$Freq,2)),
                                              sliderInput("scenarioslider1", "Probability for a completely returned box ",min = 0,max=100,value = 100*round(returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "F", ]$Freq/ sum(returnratio[returnratio$Var1 ==paste(year(today()),quarters(today()) ,sep = " ") & returnratio$Var2 != "N", ]$Freq),2)),
                                              sliderInput("scenarioslider2", "Probability for a partially returned box", min=0,max=100, value = round(100-100*returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "F", ]$Freq /sum(returnratio[returnratio$Var1 ==paste(year(today()),quarters(today()) ,sep = " ") & returnratio$Var2 != "N", ]$Freq)-returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "N", ]$Freq-100*round(returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "N", ]$Freq,2),2)),
                                              uiOutput("scenarioslider3"),
                                              strong(textOutput("probalarm")),
                                              
                                              actionButton("ScenarioExecute","Start"),
                                              dataTableOutput("ScenarioSidebar")
                                              
                                 ))
                      
                     )
                     )
                   
                       ),
                   navbarMenu("Incoming",
                              tabPanel("PO Overview (Order level)"  ,downloadButton('downloadOpenPOData', 'Download'),br(),
                                       "This table shows the open Purchase Orders that are expected to be delivered in the next days. ", em("e"), " indicated expected.",br(),
                                       dataTableOutput("openPOTable")
                                       #plots, key measures
                              ),
                              tabPanel("CG Overview (Daily level)"   , downloadButton("downloadingincCGData","Download"), br(),
                                       "Here we see the expected incoming quantities for every CG in the next days.",br(),
                                       "alskdfosefeösf",br(),
                                       dataTableOutput("poCGdata")
                              ),
                              tabPanel("Supplier Analysis",br(),
                                       "This table aggregates the data of all POs with a delivery window, to show supplier performance. ", em("sd"), " means standard deviation,", em("w"), " means that the average calculation is made with weights of delivered articles on the day.",br(),
                                       dataTableOutput(outputId = "supplieranalysistable")
                              ),
                              tabPanel("Delivery Window Analysis",
                                       plotOutput("DeliveryWindowGraph"),br(),
                                       "This visualization shows the (weighted average) delivery day of a PO partition as percentage of the delivery window. The dashed lines indicate the range of the window.",br(),
                                       "balbalblab",br(),
                                       dataTableOutput("DeliveryWindowData")
                            ),
                            tabPanel("About the model",wellPanel(
                                     "The forecast is made with a linear regression model. ",br(), "The covariates are: ", em("cluster of suppliers"), " + ", em("weekday of delivery window opening"), " + ", em("number of articles"), br(),
                                     "Clustering was necessary to avoid overfitting. The number of observations / supplier are too small, to use every supplier as a feature in the model. 10 clusters are made, based on the dispersionand and center of the distribution of deliveries per supplier.  ",br(),
                                     "This model has performed better than a regression tree. However, the forecasts are not completely accurate due to low data quality. The interquartile range of the residuals is around 7, meaning that 50% of the predications are in an interval of [-3.5,3.5] days around the real delivery date.",br(),
                                     "The regression is created at every launch of the app, meaning that it learns from past deliveries. For more information, read Chapter 2."
                                     ))
                   ),
                   navbarMenu("Outgoing",
                              tabPanel("History",
                                       #Landing 
                                       mainPanel(h3("History"),
                                                "Country Split",  
                                                textOutput("date"),
                                                plotOutput("thelastxdays"),
                                                dataTableOutput("thelastxdaysdata")
                                        
                                       ), 
                                       sidebarPanel(
                                         h3("Forecasting Options"),
                                         #textOutput("test"),
                                          dateRangeInput("thelastxdate", "Select date",start= maxdate-15,  end = maxdate ,max = maxdate ),
                                         #ADJUST
                                         selectInput("countryinput", "Country", list("DE"=1,"AT"=2,"CH"=3,"NL"=4,"DK"=5,"LU"=6,"SE"=7, "Total" = 8),1 ),br(),
                                         #/ADJUST
                                         "Here we can analyze time series. The historic data can be decomposed mathematically into a seasonal and trend component. ",br(),
                                         "Everything that is left over is put into the remainder, meaning that we can sum up the components to get the real value", br(), 
                                         "Notice that the scales are different for every facet. To forecast go to ",em("Model Forecast Comparison")
                              )),
                              tabPanel( "Forecast Model Comparison",
                                       mainPanel(h3("Forecast Model Comparison"),
                                        "Summary",  
                                           plotOutput("outgoingforecastplotcomparison"),
                                           dataTableOutput("MFCdata")
                                          ),
                                          sidebarPanel(
                                            h4("Forecasting Options"),
                                            dateRangeInput("MFCForecastingrange", "Select date",start= maxdate,  end = maxdate+7 ,max = maxdate+7*8 ),
                                            "To predict future shippings, we compare two prediction algorithms. They are blended 50:50 for the forecast.", br(),
                                            "The plot shows you how their numbers differ. "
                                            
                                            
                                          )
                              ), 
                             
                              tabPanel("Monthly changes in CO composition",
                                       "The articles in a box are aggregated on Commodity Group level in this tool. Commodity Groups tend to vary with the shipping month. We can visualize this effect with the plot. "
                                       ,checkboxGroupInput("CGselect",label = "Select Commodity Groups",choices = as.list(lev),selected = "Shoes", inline = TRUE),
                                       plotOutput("CGmonthchanges")
                                       
                                       #plotout
                              ),
                              tabPanel("About", wellPanel(
                                "This forecast is made on two levels. ",br(),
                                "First, the daily CO shippings are forecasted. This does not need much calculation time. The tab ", em("Forecast Model Comparison"), " helps to understand the prediction and the ",em("History"), "tab decomposes the time series into its parts.", br(),
                                "Now it gets complicated. The tool models every forecasted order in a unique way. This means that we sample attributes for every forecasted order and based on the attributes, we estimate the composition of CGs.", br(),
                                "To do so, we use empiricial data to sample :",br(), strong("customer age"), " based on the shipping country, ",br(), strong("sales channel"), " based on shipping country and customer age", br(),
                                strong("basket size"), "based on sales channel.",br(),br(),
                                "We now try to find a past CO that fits the shipping month (because the CGs vary through the months, see ",em("Monthly changes in CO composition"), ") the sampled basket size and sales channel", br(),
                                "We randomly pick one of the orders that has these attributes and copy its Commodity Group composition to our forecasted order. This step is repeated ", strong("for every order in the forecast"), "and is the reason for the long calculation time.",br(),
                                "For more information about the picking of the attributes and their dependencies, check out the thesis."
                                
                            
                                
                                
                                ))
                   ),
                   navbarMenu("Returns",
                              tabPanel("Forecast Returns",
                                       dataTableOutput("returnTable")
                              ),
                              tabPanel("Open Returns",
                                       downloadButton("returnsOpenReturnsDownload","Download"),br(),
                                       "Open Returns are defined as returns from Customer Orders that were shipped before the data was extracted and are in state 128 or 256. We do not know if the customer will return them, which is why we include them in the model.",br(),
                                       "In contrast to our forecasted Customer Orders, we do know that we have shipped them with this exact attributes. The probability of a return decreases as time goes by, meaning that it is unlikely that we get a return from an order that has been shipped 30 days ago.",br(), "See the return time distribution for details.",
                                       dataTableOutput("returnOpenReturns")
                              ),

                              tabPanel("Return Time Distribution",
                                       tabsetPanel(
                                         tabPanel("Country View",
                                                  "Countries influence the return time, because the shipping time to the customer, as well as the return time to our warehouse increases with distance.",
                                       plotOutput("returndistributionsplitbycountry")),
                                          tabPanel("Weekday View",
                                                   "The weekday of a shipment has an effect on the return time, because we do not recieve returns on weekends. The gaps in the histogramms show this. This effect is used in the return time estimation. ",
                                            plotOutput("returndistributionsplitbyweekday")))
                              ),
                        
                              tabPanel("About",wellPanel(
                                "The returns are split into two sections. ", em("Open Returns"), " shows orders that are in state 128 or 256, which means they have been shipped in the last days and we do not know if they get returned. ", br(),
                                "The returns from the forecast are in the ",em("Forecast Returns"), " tab. These are the returns of the orders we think we ship in future.",br(),br(),
                                "The return model works with two generalized linear models that do binary classification (called logisitic regression). These do the prediction in two steps. ",br(),
                                "Covariates for these prediction are the customer age, the shipping country and the articles in the box.",br(),br(),
                                "Model1 estimates whether an order HAS ANY returned articles(return class P or F) or not (N).",br(),
                                "Model2 estimates for the as returned marked Customer Orders from the previous model if they are a full return (F) or a partial return (P)",br(),
                                "The thresholds for splitting between one class or the other is based on the return ratio of the last quarter. This is a safety measure, so that the models are not allowed to classify returns in an unusal way. See ", em("Return probabilites"), " in Misc for details",
                                br(), "After this we know the class of the return, but we still need to find out how many articles in a class P return will be returned.",br(),
                                "The amount of returned articles (if we just look at partial returns) can be described with a lognormal distribution. See the thesis for more details. We sample lognormal random variables to determine the amount of returned articles and perform drawing without replacement on the shipped articles to get the returned ones.",
                                br(),br(),
                                "The return time is sampled based on empiricial data and depends on the shipping country and the weekday of shipment."
                              ))
                   ),
                   navbarMenu("Inventory",
                              tabPanel("History",
                                       sidebarLayout(
                                         mainPanel(
                                          wellPanel( checkboxGroupInput("CGInventoryHistoryselect",label = "Select Commodity Groups",choices = as.list(lev),selected = "Shoes",inline = TRUE)),
                                           plotOutput("InventoryPlot"),
                                           dataTableOutput("InventoryDataTable")
                                           
                                           
                                         ),
                                         sidebarPanel(
                                           
                                       dateRangeInput("inventoryrange", "Select date",start= maxdate-15,  end = maxdate ,max = maxdate ),br(),
                                       "Put in a range in the past and select the Commodity Groups you want to observe. Notice that the scales are independant in the facets.", br(),
                                       "Decreases are related to shippings, increases to PO deliveries or returns. To see this for the predicted inventory, go to ", em("Forecast")
                              ))),
                              tabPanel("Forecast",
                                "Displays the inventory of the last days and in the forecasted period.",
                                 tabsetPanel(
                                   
                                   tabPanel("Data",downloadButton("Flowdownload","Download"),
                                       
                                            dataTableOutput("InventoryFlowTable")),
                                   tabPanel("Plot",
                                            mainPanel(
                                            plotOutput("Flowplot")),
                                            sidebarPanel(checkboxGroupInput("CGInventoryFlowselect",label = "Select Commodity Groups",choices = as.list(lev),selected = "Shoes"))
                                   )
                                  
                                  
                                )
                               ),
                                
                              
                              tabPanel("About",
                                       wellPanel("The inventory is split into Commodity Groups. We can browse through past data in the ", em("History"), " tab. ", br(),
                                                 "If we want to analyze our forecast for the inventory, we do this in the ",em("Forecast"), " tab in this section of the navigation bar.", br(),
                                                 "Because there are ", em("dead"), "CGs and the sampling of orders does not do an inventory check in background, these are excluded in the sampling process of the outgoing forecast.", br(),
                                                 "However, their returns based on current orders or POs is included.",
                                                  "The following CGs are excluded in the sampling process: ", br(),strong("electronic cases"), br(), strong("cooperations"), br(), strong("formal wear accessories"), br()
                                                 
                                                 ))
                   ),
                   navbarMenu("Misc",
                              tabPanel("Customer Age Distribution",
                                       plotOutput("customeragedistribution")
                              ),
                              tabPanel("Sales Channel Distribution",
                                       tabsetPanel(
                                         tabPanel("on basket size",
                                                  plotOutput("saleschanneldistributionbasketsize")),
                                         tabPanel("on country",
                                                  plotOutput("saleschanneldistributioncountry"))
                                        )
                                       
                                       ),
                              tabPanel("Return probabilities",
                                       plotOutput("returnprobs")
                                       ),
                              tabPanel("Basket Size Distribution","tbd"
                                       )
                              
                   )
))