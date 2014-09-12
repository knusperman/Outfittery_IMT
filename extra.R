
#-------------------------------------------------------------
#inbound
supplier_po_measures  <- data.table(incomingdata)[, list(
		c=length(booking_date), 
		mean = mean(datediff_booking_PO), 
		w_mean = sum(datediff_booking_PO*percent_delivery),
		sd = sd(datediff_booking_PO, na.rm=TRUE), 
		mad = mad(datediff_booking_PO), 
		median = median(datediff_booking_PO), 
		articles = sum(daily_bookings),
		window = mean(as.numeric(latest_delivery_date- earliest_delivery_date, units="days")),
		mean_day_in_window = mean(as.numeric(booking_date - earliest_delivery_date, units = "days")),
		w_mean_day_in_window = sum(as.numeric(booking_date - earliest_delivery_date, units = "days")*percent_delivery),
		w_percentage_of_window = sum(as.numeric(booking_date - earliest_delivery_date, units = "days")*percent_delivery) / mean(ifelse(as.numeric(latest_delivery_date- earliest_delivery_date, units="days")==0, 1,as.numeric(latest_delivery_date- earliest_delivery_date, units="days"))),
		sd_day_in_window = sd(as.numeric(booking_date - earliest_delivery_date, units = "days", na.rm =TRUE)),
		articles_before_window = as.numeric(ifelse(!is.na(earliest_delivery_date),ifelse(booking_date<earliest_delivery_date,daily_bookings,0),0)),
		articles_after_window = as.numeric(ifelse(!is.na(earliest_delivery_date),ifelse(booking_date>latest_delivery_date,daily_bookings,0),0))),
		by = list(supplier_id, purchase_order_id, earliest_delivery_date, latest_delivery_date, season, order_type)]
		setnames(supplier_po_measures,1:6,c("supplier_id","purchase_order_id","edd","ldd","season","order_type"))
		supplier_po_measures$window <- ifelse(supplier_po_measures$window > 150, 100, supplier_po_measures$window)
wW_supplier_po_measures <- subset(supplier_po_measures, !is.na(window))
wW_supplier_po_measures$windowweekday <- factor(weekdays(wW_supplier_po_measures$edd))
clusterdata <- clustering(data.frame(wW_supplier_po_measures$supplier_id, wW_supplier_po_measures$mean_day_in_window, wW_supplier_po_measures$sd_day_in_window))
c <- kmeans(clusterdata[2:3],10)
clusterdata <- data.frame("supplier_id"=clusterdata[1], "cluster" =c$cluster)
setnames(clusterdata,1,"supplier_id")
wW_supplier_po_measures <- join(wW_supplier_po_measures, clusterdata)
wW_supplier_po_measures$cluster <- factor(wW_supplier_po_measures$cluster)
subwW_supplier_po_measures <- subset(wW_supplier_po_measures, order_type != "Vororder")

openPO.forecast <- data.frame(
	"purchase_order_id" = openPO$purchase_order_id,
	"supplier_id" = openPO$supplier_id,
	"articles" = openPO$q_sum-openPO$fq_sum,
	"edd" = openPO$earliest_delivery_date,
	"window"= as.numeric(openPO$latest_delivery_date - openPO$earliest_delivery_date, units ="days"),
	"windowweekday" = factor(weekdays(openPO$earliest_delivery_date))
	)
openPO.forecast <- join(openPO.forecast, clusterdata)
openPO.forecast$cluster <- factor(openPO.forecast$cluster)
t <- table(openPO.forecast$cluster)
openPO.forecast$cluster <- ifelse(is.na(openPO.forecast$cluster), names(t)[t == max(t)], openPO.forecast$cluster)

#create models

#new models
rlm <- lmrob(formula = w_mean_day_in_window ~ cluster + windowweekday + window+articles, data = wW_supplier_po_measures)
if(rlm$converged==FALSE){rlm <- lm(formula = w_mean_day_in_window ~ cluster + windowweekday + window+articles, data = wW_supplier_po_measures)}
#lm4 <- lm(formula = w_mean_day_in_window ~cluster + windowweekday + sqrt(articles), data = wW_supplier_po_measures[wW_supplier_po_measures$avg_w_mean_day_in_window<40&wW_supplier_po_measures$avg_w_mean_day_in_window>-30,])
       
openPO.forecast$lmpredict <- predict(rlm,data.frame("cluster"=openPO.forecast$cluster, "windowweekday"=openPO.forecast$windowweekday,"window"=openPO.forecast$window, "articles"=openPO.forecast$articles))

openPO$f_arrival <- openPO$earliest_delivery_date+round(openPO.forecast$lmpredict,0)
openPO$pastwindow <- ifelse(openPO$f_arrival<maxdate,as.numeric(maxdate-openPO$latest_delivery_date),NA)

#negative numbers in pastwindow mean we are still in the window, but the rf-forecast was too early
#positive numbers in pastwindow mean we are past the window. 
samples <- sample(1:7, size = dim(openPO)[1], replace = TRUE)
openPO$f_arrival <- ifelse(!is.na(openPO$pastwindow), 
							samples+maxdate
						,  openPO$f_arrival)
openPO$f_arrival <- as.Date(openPO$f_arrival)

	openPO$weekday <- weekdays(openPO$f_arrival)
	openPO[openPO$weekday == "Sunday",]$f_arrival <- openPO[openPO$weekday == "Sunday",]$f_arrival+1
	openPO[openPO$weekday == "Saturday",]$f_arrival <- openPO[openPO$weekday == "Saturday",]$f_arrival-1
	openPO$weekday <- NULL

    openPOshaped <- data.frame(openPO)[c(1,2,3,4,5,6,8,9,50)]
    openPOshaped$openq <- openPOshaped$q_sum-openPOshaped$fq_sum
    maxcols <- apply(openPO[,10:29],1,FUN = which.max)
    openPOshaped$maxCG <- join(data.frame("id"=maxcols), data.frame("id"=1:19, lev[1:19]))$lev
    setnames(openPOshaped,1:11, c("PO","Supplier","Earliest DD", "Latest DD", "Season","Order Type", "Quantity","Fulfilled quantity", "Estimated arrival", "Open quantity", "max articles in CG"))
    openPOshaped<-openPOshaped[c(1:4,9,7,8,10,11,6,5)]
    openPOshaped <- openPOshaped[order(openPOshaped[,5]),]


supplier_measures <- data.table(supplier_po_measures)[, list(
	c=as.numeric(sum(ifelse(!is.na(edd),1,0 ))),
	avg_bookingdays = mean(c),
	w_mean=mean(w_mean),
	sd = sd(w_mean),
	mean_sd = mean(sd),
	avg_window = mean(window, na.rm = TRUE),
	sd_window = sd(window, na.rm =TRUE),
	sum_articles = as.numeric(sum(ifelse(!is.na(edd),articles,0))),
	avg_w_mean_day_in_window = mean(w_mean_day_in_window, na.rm =TRUE),
	avg_w_percentage_of_window = mean(w_percentage_of_window, na.rm =TRUE),
	sd_w_mean_day_in_window = sd(w_mean_day_in_window, na.rm =TRUE),
	sd_w_percentage_of_window = sd(w_percentage_of_window, na.rm = TRUE),
	mean_sd_day_in_window = mean(sd_day_in_window, na.rm = TRUE),
	articles_before_window = sum(articles_before_window),
	articles_after_window = sum(articles_after_window),
	po_not_in_window = sum(ifelse(articles_after_window>0 | articles_before_window>0,1,0))),
	by = supplier_po_measures$supplier_id]

setnames(supplier_measures,1,"supplier_id")
supplier_measures$ArticleShare  <- supplier_measures$sum_articles/sum(supplier_measures$sum_articles)*100
supplier_measures <- supplier_measures[with(supplier_measures, order(-ArticleShare))]

#order_type_measures <- data.table(supplier_po_measures)[, list(
#	c = length(purchase_order_id),
#	avg_bookingdays = mean(c),
#	w_mean=mean(w_mean),
#	sd = sd(w_mean),
#	mean_sd = mean(sd),
#	avg_window = mean(window, na.rm = TRUE),
#	sd_window = sd(window, na.rm =TRUE),
#	avg_w_mean_day_in_window = mean(w_mean_day_in_window, na.rm = TRUE),
#	avg_w_percentage_of_window = mean(w_percentage_of_window, na.rm =TRUE),
#	sd_w_mean_day_in_window = sd(w_mean_day_in_window, na.rm =TRUE),
#	sd_w_percentage_of_window = sd(w_percentage_of_window, na.rm = TRUE),
#	mean_sd_day_in_window = mean(sd_day_in_window, na.rm = TRUE),
#	avg_articles = mean(articles),
#	median= median(w_mean)),
#	by = supplier_po_measures$order_type]
#setnames(order_type_measures, 1, "order_type")

#season_measures <- data.table(supplier_po_measures)[, list(
#	c = length(purchase_order_id),
#	avg_bookingdays = mean(c),
#	w_mean=mean(w_mean),
#	sd = sd(w_mean),
#	mean_sd = mean(sd),
#	mean = mean(mean),
#	avg_window = mean(window, na.rm = TRUE),
#	sd_window = sd(window, na.rm =TRUE),
#	avg_w_mean_day_in_window = mean(w_mean_day_in_window, na.rm = TRUE),
#	avg_w_percentage_of_window = mean(w_percentage_of_window, na.rm = TRUE),
#	sd_w_mean_day_in_window = sd(w_mean_day_in_window, na.rm =TRUE),
#	sd_w_percentage_of_window = sd(w_percentage_of_window, na.rm = TRUE),
#	mean_sd_day_in_window = mean(sd_day_in_window, na.rm = TRUE),
#	avg_articles = mean(articles),
#	median = median(w_mean)),	
#	by = supplier_po_measures$season]
#setnames(season_measures, 1, "season")

# supplier_order_type_measures <- data.table(supplier_po_measures)[, list(
#	c = length(purchase_order_id),
#	avg_bookingdays = mean(c),
#	w_mean=mean(w_mean),
#	sd = sd(w_mean),
#	mean_sd = mean(sd),
#	avg_window = mean(window, na.rm = TRUE),
#	sd_window = sd(window, na.rm =TRUE),
#	avg_w_mean_day_in_window = mean(w_mean_day_in_window, na.rm = TRUE),
#	avg_w_percentage_of_window = mean(w_percentage_of_window, na.rm =TRUE),
#	sd_w_mean_day_in_window = sd(w_mean_day_in_window, na.rm =TRUE),
#	sd_w_percentage_of_window = sd(w_percentage_of_window, na.rm = TRUE),
#	mean_sd_day_in_window = mean(sd_day_in_window, na.rm = TRUE),
#	sum_articles = sum(articles),
#	avg_articles = mean(articles),
#	median = median(w_mean)),
#	by = list(supplier_po_measures$supplier_id,supplier_po_measures$order_type)]
# setnames(supplier_order_type_measures, 1:2, c("supplier_id","order_type"))
# supplier_order_type_measures  <- supplier_order_type_measures[with(supplier_order_type_measures, order(supplier_id)), ]


#-------------------------------------------------------------
#outgoing


#monthlybasketCG<-monthlybasketCG[with(monthlybasketCG, order(month, basketsize))]
#monthlybasketCG<-monthlybasketCG[monthlybasketCG$basketsize<21,]

#dailyshipped <- data.table(outgoingdata)[,  list(
#c = length(id),
#sales_channel = table(sales_channel)/length(id),
#avg_purchase_gross = mean(total_amount_basket_purchase_gross),
#avg_billed_gross = mean(total_amount_billed_purchase_gross),
#percent_articles_kept = mean(articles_returned/articles_shipped),
#percent_box_return = sum(isreturned)/length(id),
#mean_age = mean(customer_age),
#sd_age = sd(customer_age)
#
#	), by = list(date_shipped, shipping_country)]




shipping_table <- data.frame(table(outgoingdata$date_shipped, outgoingdata$shipping_country))
colnames(shipping_table)<-c("days", "shipping_country", "Freq")
shipping_table$days <- as.Date(shipping_table$days)
fulldatetable <- expand.grid(days = seq(min(as.Date(shipping_table$days)), max(as.Date(shipping_table$days)), by="days"),shipped = 0,  shipping_country = levels(outgoingdata$shipping_country))
c_shipping_table <- join(fulldatetable, shipping_table)
c_shipping_table$shipped <- ifelse(is.na(c_shipping_table$Freq), 0, c_shipping_table$Freq)
c_shipping_table$Freq <- NULL
c_shipping_table$week <- paste(week(c_shipping_table$days),sep = " ", format(c_shipping_table$days, "%Y"))
weekly_shipped <- data.table(c_shipping_table)[,list(shipped_weekly = sum(shipped)), by = list(shipping_country, week)]

#ADJUST
DEshipped <- subset(c_shipping_table, c_shipping_table$shipping_country == "DE")$shipped
NLshipped <- subset(c_shipping_table, c_shipping_table$shipping_country=="NL")$shipped
ATshipped <- subset(c_shipping_table, c_shipping_table$shipping_country=="AT")$shipped
CHshipped <- subset(c_shipping_table, c_shipping_table$shipping_country=="CH")$shipped
DKshipped <- subset(c_shipping_table, c_shipping_table$shipping_country=="DK")$shipped
LUshipped <- subset(c_shipping_table, c_shipping_table$shipping_country=="LU")$shipped
SEshipped <- subset(c_shipping_table, c_shipping_table$shipping_country=="SE")$shipped
setnames(c_shipping_table, "shipping_country", "Country")


countrystarts <- data.frame("DE"=as.Date("2013-07-01", "%Y-%m-%d"),
							"NL"=as.Date("2014-04-01", "%Y-%m-%d"),
							"CH"=as.Date("2013-09-09", "%Y-%m-%d"),
							"AT"=as.Date("2014-07-01", "%Y-%m-%d"),
							#ADJUST THESE TO BE CORRECT
							"DK"=as.Date("2014-09-01", "%Y-%m-%d"),
							"LU"=as.Date("2014-09-01", "%Y-%m-%d"),
							"SE"=as.Date("2014-09-01", "%Y-%m-%d") )

DEtimeseries <- ts(data=DEshipped, start(1,7), frequency = 7)
ATtimeseries <- ts(ATshipped, start(1,7), frequency = 7)
#since April 2014
NLtimeseries <- ts(NLshipped[-c(1: as.numeric(countrystarts["NL"]-countrystarts["DE"])) ] , start(1,7), frequency = 7)
#since 9th Sep 2013
CHtimeseries <- ts(CHshipped[-c(1: as.numeric(countrystarts["CH"]-countrystarts["DE"])) ], start(1,7), frequency = 7)
#?since 1st Sep 2014?
DKtimeseries <- ts(DKshipped[-c(1: as.numeric(countrystarts["DK"]-countrystarts["DE"])) ], start(1,7), frequency = 7)
LUtimeseries <- ts(LUshipped[-c(1: as.numeric(countrystarts["LU"]-countrystarts["DE"])) ], start(1,7), frequency = 7)
SEtimeseries <- ts(SEshipped[-c(1: as.numeric(countrystarts["SE"]-countrystarts["DE"])) ], start(1,7), frequency = 7)

Ttimeseries <- ts(data=DEshipped+NLshipped+ATshipped+CHshipped+DKshipped+LUshipped+SEshipped, start(1,7),frequency = 7)
#/ADJUST

###age
#countryage
agetable <-data.frame(table(floor(outgoingdata$customer_age), outgoingdata$shipping_country))
colnames(agetable) <- c("age", "country", "Freq")
c_agetable <- join(expand.grid("age" = factor(18:90), "country" = factor(levels(last6weeksoutgoing$shipping_country))), agetable)
c_agetable$Freq <- ifelse(is.na(c_agetable$Freq), 0, c_agetable$Freq)

#countryagesaleschannel
last6countryagechannel <-data.frame(table(last6weeksoutgoing$shipping_country, last6weeksoutgoing$sales_channel, last6weeksoutgoing$customer_age))
colnames(last6countryagechannel) <- c("country", "sales_channel", "age", "Freq")
c_age_sales_channeltable <- join(expand.grid("age"=factor(18:90), "country" =  factor(levels(last6weeksoutgoing$shipping_country)), "sales_channel" = factor(levels(last6weeksoutgoing$sales_channel))), last6countryagechannel)
c_age_sales_channeltable$Freq <- ifelse(is.na(c_age_sales_channeltable$Freq),0, c_age_sales_channeltable$Freq)

#complete countryagesaleschannel if no data available
countryagechannel_complete <-  data.frame(table(outgoingdata$shipping_country, outgoingdata$sales_channel, outgoingdata$customer_age))
colnames(countryagechannel_complete) <- c("country", "sales_channel", "age", "Freq")
c_age_sales_channeltable_complete <- join(expand.grid("age"=factor(18:90), "country" =  factor(levels(outgoingdata$shipping_country)), "sales_channel" = factor(levels(outgoingdata$sales_channel))), countryagechannel_complete)
c_age_sales_channeltable_complete$Freq <- ifelse(is.na(c_age_sales_channeltable_complete$Freq),0, c_age_sales_channeltable_complete$Freq)

#sales channel basket size
table_bsize_sc <- data.frame(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$sales_channel)/rowSums(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$sales_channel)))
table_bsize_sc<- table_bsize_sc[as.numeric(table_bsize_sc$Var1)<16,]
#ggplot(table_bsize_sc, aes(x = Var1, y=Freq, fill = Var2))+geom_area()
articles <- data.frame(table(last6weeksoutgoing$articles_shipped)/sum(table(last6weeksoutgoing$articles_shipped)))
articles$cumsum <- cumsum(articles$Freq)
articles$Freq <- NULL
table_bsize_sc<- join(table_bsize_sc, articles)
table_bsize_sc$Var1 <- as.numeric(table_bsize_sc$Var1)


table_bs_sc <- data.frame(t(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$sales_channel )) /colSums(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$sales_channel)))
table_bs_sc<- table_bs_sc[as.numeric(table_bs_sc$Var2)<21,]
colnames(table_bs_sc)<- c("sales_channel", "basketsize", "Freq")
table_bs_sc$sales_channel <- as.character(table_bs_sc$sales_channel)
table_bs_sc[is.nan(table_bs_sc$Freq),]$Freq <- 0.05

#country basket size
table_bsize_country <- data.frame(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$shipping_country)/rowSums(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$shipping_country)))
table_bsize_country<- table_bsize_country[as.numeric(table_bsize_country$Var1)<16,]
articles <- data.frame(table(last6weeksoutgoing$articles_shipped)/sum(table(last6weeksoutgoing$articles_shipped)))
articles$cumsum <- cumsum(articles$Freq)
articles$Freq <- NULL
table_bsize_country<- join(table_bsize_country, articles)
table_bsize_country$Var1 <- as.numeric(table_bsize_country$Var1)

#age class basket size
table_bsize_age <- data.frame(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$age_class)/rowSums(table(last6weeksoutgoing$articles_shipped, last6weeksoutgoing$age_class)))
table_bsize_age<- table_bsize_age[as.numeric(table_bsize_age$Var1)<16,]
articles <- data.frame(table(last6weeksoutgoing$articles_shipped)/sum(table(last6weeksoutgoing$articles_shipped)))
articles$cumsum <- cumsum(articles$Freq)
articles$Freq <- NULL
table_bsize_age<- join(table_bsize_age, articles)
table_bsize_age$Var1 <- as.numeric(table_bsize_age$Var1)

returnratio <- data.frame(table( outgoing_basketsubset$quarter,outgoing_basketsubset$returnclass)/rowSums(table( outgoing_basketsubset$quarter,outgoing_basketsubset$returnclass)))
	

#return
returnlm3 <- glm(formula = isreturned ~ customer_age + shipping_country+articles_shipped, family = "binomial", data = outgoing_basketsubset)

returnclasslm <- glm(formula = as.factor(returnclass) ~ customer_age + shipping_country +  articles_shipped, family = "binomial", data = outgoing_basketsubset)


