#packages
#data.table
#caret
#forecast
#ggplot2
#lubridate
#mass
#plyr
#proc
#e1071
#stinepack

#incoming
#----------------------------------------------------------------------------------

incomingdata  <- read.csv2(file = "data/incoming.csv", sep = ";" )

 incomingdata$purchase_order_id  <-  as.factor(incomingdata$purchase_order_id)
 incomingdata$supplier_id  <-  as.factor(incomingdata$supplier_id)
 incomingdata$earliest_delivery_date  <- as.Date(incomingdata$earliest_delivery_date)
 incomingdata$latest_delivery_date  <- as.Date(incomingdata$latest_delivery_date)
 incomingdata$po_date_created  <- as.Date(incomingdata$po_date_created)
 incomingdata$booking_date  <-  as.Date(incomingdata$booking_date)
 incomingdata$q_sum <- NULL
 #incomingdata$v_Schuhe  <- as.numeric(gsub(",","", incomingdata$v_Schuhe))
 #incomingdata$v_Outdoor  <- as.numeric(gsub(",","", incomingdata$v_Outdoor))
 #incomingdata$v_Kleinaccessoires  <- as.numeric(gsub(",","", incomingdata$v_Kleinaccessoires))
 #incomingdata$v_schlafbade  <- as.numeric(gsub(",","", incomingdata$v_schlafbade))
 #incomingdata$v_Konfektion  <- as.numeric(gsub(",","", incomingdata$v_Konfektion))
 #incomingdata$v_Schuhpflege  <- as.numeric(gsub(",","", incomingdata$v_Schuhpflege))
 #incomingdata$v_tuecherschals  <- as.numeric(gsub(",","", incomingdata$v_tuecherschals))
 #incomingdata$v_Guertel  <- as.numeric(gsub(",","", incomingdata$v_Guertel))
 #incomingdata$v_Stiefel  <- as.numeric(gsub(",","", incomingdata$v_Stiefel))
 #incomingdata$v_KrawattenFliegen  <- as.numeric(gsub(",","", incomingdata$v_KrawattenFliegen))
 #incomingdata$v_Formalwearaccessories  <- as.numeric(gsub(",","", incomingdata$v_Formalwearaccessories))
 #incomingdata$v_cooperations  <- as.numeric(gsub(",","", incomingdata$v_cooperations))
 #incomingdata$v_Oberteile  <- as.numeric(gsub(",","", incomingdata$v_Oberteile))
 #incomingdata$v_Handschuhe  <- as.numeric(gsub(",","", incomingdata$v_Handschuhe))
 #incomingdata$v_Electroniccases  <- as.numeric(gsub(",","", incomingdata$v_Electroniccases))
 #incomingdata$v_Kopfaccessoires  <- as.numeric(gsub(",","", incomingdata$v_Kopfaccessoires))
 #incomingdata$v_Hosen  <- as.numeric(gsub(",","", incomingdata$v_Hosen))
 #incomingdata$v_Taschen  <- as.numeric(gsub(",","", incomingdata$v_Taschen))
 #incomingdata$v_Unterbekleidung  <- as.numeric(gsub(",","", incomingdata$v_Unterbekleidung))
 #incomingdata$v_unknowncg3  <- as.numeric(gsub(",","", incomingdata$v_unknowncg3))
 incomingdata$datediff_booking_PO <- as.numeric(incomingdata$booking_date - incomingdata$po_date_created, units = "days")
incomingdata  <- incomingdata[with(incomingdata, order(supplier_id, purchase_order_id, earliest_delivery_date, latest_delivery_date)), ]
setnames(incomingdata, old = "order_typ", new = "order_type")
incomingdata  <-  ddply(incomingdata, .(purchase_order_id, earliest_delivery_date, latest_delivery_date), mutate, percent_delivery = daily_bookings/sum(daily_bookings))
#incomingdata$sumforwindow[incomingdata$purchase_order_id == sumbooking[,1,with=FALSE & incomingdata$earliest_delivery_date = sumbooking[,2,with=FALSE] & incomingdata$latest_delivery_date == sumbooking[,3,with=FALSE] ]]
data.tablesumbooking  <- data.table(incomingdata)[, list(sum = sum(daily_bookings)), by =list(incomingdata$purchase_order_id, incomingdata$earliest_delivery_date, incomingdata$latest_delivery_date)]

#---------------------------------------------------------------------------------
#openpo
#change
openPO <- read.csv2(file="data/open_po.csv", sep = ",")
 openPO$purchase_order_id  <-  as.factor(openPO$purchase_order_id)
 openPO$supplier_id  <-  as.factor(openPO$supplier_id)
 openPO$earliest_delivery_date  <- as.Date(openPO$earliest_delivery_date)
 openPO$latest_delivery_date  <- as.Date(openPO$latest_delivery_date)
 openPO$po_date_created  <- as.Date(openPO$po_date_created)

#----------------------------------------------------------------------------------
## outgoing
outgoingdata  <- read.csv2(file = "data/outgoing.csv", sep = ";")
outgoingdata$id  <- as.factor(outgoingdata$id)
outgoingdata$state  <- as.factor(outgoingdata$state)
outgoingdata$payment_method  <- as.factor(outgoingdata$payment_method)
outgoingdata$date_billed  <- as.Date(outgoingdata$date_billed)
outgoingdata$date_payed  <- as.Date(outgoingdata$date_payed)
outgoingdata$date_returned  <- as.Date(outgoingdata$date_returned)
outgoingdata$date_shipped  <- as.Date(outgoingdata$date_shipped)
outgoingdata$total_amount_basket_purchase_gross  <- as.numeric(gsub(",","", outgoingdata$total_amount_basket_purchase_gross))
outgoingdata$total_amount_billed_purchase_gross  <- as.numeric(gsub(",","", outgoingdata$total_amount_billed_purchase_gross))
outgoingdata$order_id  <- NULL
outgoingdata$isreturned <- ifelse(is.na(outgoingdata$date_returned), FALSE,TRUE)
outgoingdata$date_shipped_yearmonth  <- as.factor(format.Date(outgoingdata$date_shipped, "%y %m"))
outgoingdata$date_shipped_month <- as.factor(format.Date(outgoingdata$date_shipped, "%m"))
outgoingdata$datediff_shipped_returned <- as.numeric(outgoingdata$date_returned - outgoingdata$date_shipped,  units = "days")
outgoingdata <- subset(outgoingdata, !(outgoingdata$isreturned == TRUE & outgoingdata$datediff_shipped_returned<0))
outgoingdata <- outgoingdata[!is.na(outgoingdata$date_shipped),]
outgoingdata$quarter <- paste(year(outgoingdata$date_shipped),quarters(outgoingdata$date_shipped) ,sep = " ")
outgoingdata$quarter <- factor(outgoingdata$quarter)
outgoingdata$percent_value_returned <- ifelse(outgoingdata$total_amount_billed_purchase_gross==0, 1 ,1- outgoingdata$total_amount_billed_purchase_gross/outgoingdata$total_amount_basket_purchase_gross)
outgoingdata$percent_articles_returned <- ifelse(outgoingdata$articles_shipped== 0, 0, outgoingdata$articles_returned/outgoingdata$articles_shipped)
outgoingdata$age_class <- ifelse(outgoingdata$customer_age>60, 5, ifelse(outgoingdata$customer_age>40, 4, ifelse(outgoingdata$customer_age>30,3,2)))

outgoingdata[which(outgoingdata$customer_age>100), 12]   <- NA
outgoingdata[which(outgoingdata$customer_age<18), 12]  <- NA
outgoingdata[is.na(outgoingdata$customer_age),]$customer_age  <- mean(outgoingdata$customer_age, na.rm = TRUE)

lev <- c("Shoes","Outdoor", "Accessories","Sleep-/Nightwear","Blazers","Shoe Care","Neckwear", "Belts","Boots", "Ties", "Formal Wear Accessories", "Cooperations", "Tops", "Gloves", "Electronic Cases", "Head Accessories", "Pants", "Bags", "Underwear", "NoMatch")
lev2 <- c("Shoes", "Outdoor", "Accessories", "Sleep-/Nightwear", "Blazers", "Shoe Care", "Neckwear", "Belts","Boots","Ties", "Tops","Gloves","Head Accessories","Pants","Bags","Underwear")
outgoing_basketsubset<- outgoingdata[outgoingdata$articles_shipped == rowSums(outgoingdata[,c(20:29,32,33,35:38)]),]
outgoing_basketsubset <- outgoing_basketsubset[,c(6,7,8,11,12,18,19,20:39,40:59, 62,67)]
outgoing_basketsubset <- outgoing_basketsubset[outgoing_basketsubset$articles_shipped<21,]
outgoing_basketsubset$returnclass <- ifelse(outgoing_basketsubset$articles_shipped==outgoing_basketsubset$articles_returned, "F", ifelse(outgoing_basketsubset$articles_returned==0, "N", "P"))
outgoing_basketsubset$returnclass <- as.factor(outgoing_basketsubset$returnclass)
outgoing_basketsubset$datediff_shipped_returned <- as.numeric(outgoing_basketsubset$date_returned - outgoing_basketsubset$date_shipped,  units = "days")
outgoing_basketsubset$isreturned <- ifelse(is.na(outgoing_basketsubset$date_returned), FALSE,TRUE)
outgoing_basketsubset$age_class_name <- factor(outgoing_basketsubset$age_class)
levels(outgoing_basketsubset$age_class_name) <- c("younger than 30", "between 30 and 40", "between 40 and 60", "older than 60")
outgoing_basketsubset$quarter <- paste(year(outgoing_basketsubset$date_shipped),quarters(outgoing_basketsubset$date_shipped) ,sep = " ")
outgoing_basketsubset$quarter <- factor(outgoing_basketsubset$quarter)
outgoing_basketsubset$weekday <- weekdays(outgoing_basketsubset$date_shipped)
#outgoing_basketsubset$weekday <- factor(outgoing_basketsubset$weekday, levels(outgoing_basketsubset$weekday)[c(7,5,1,4,2,3,6)])
# levels(outgoing_basketsubset$weekday)<- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

maxdate <- max(outgoing_basketsubset$date_shipped)

outgoing_basketsubset_returns <- outgoing_basketsubset[outgoing_basketsubset$isreturned == TRUE & outgoing_basketsubset$datediff_shipped_returned <31& outgoing_basketsubset$articles_returned != 0,]

returntimetable <- data.frame(table(outgoing_basketsubset_returns$weekday, outgoing_basketsubset_returns$datediff_shipped_returned, outgoing_basketsubset_returns$shipping_country))
setnames(returntimetable, 1:3, c("weekday","diff","shipping_country"))
returntimetable <- returntimetable[returntimetable$diff!=0, ]

state128_256 <- outgoingdata[outgoingdata$state == 256 | outgoingdata$state == 128,]
state128_256$orderage <- as.numeric(max(state128_256$date_shipped)-state128_256$date_shipped , units = "days")
state128_256$articles_shipped <- rowSums(state128_256[c(20:29, 32:33,35:38)])


last6weeksoutgoing <- subset(outgoingdata, outgoingdata$date_shipped > max(outgoingdata$date_shipped)-42)

#sample process
#get df with date/  shipping_country / order number per row. STRING AS FACTORS = FALSE

#------------------------------------------
#inventory

inventory <- read.csv2("data/inventory.csv", sep = ",")
inventory$datum <- as.Date(inventory$datum)
inventory$v_Schuhe  <- as.numeric(gsub(",","", inventory$v_Schuhe))
inventory$v_Outdoor  <- as.numeric(gsub(",","", inventory$v_Outdoor))
inventory$v_Kleinaccessoires  <- as.numeric(gsub(",","", inventory$v_Kleinaccessoires))
inventory$v_schlafbade  <- as.numeric(gsub(",","", inventory$v_schlafbade))
inventory$v_Konfektion  <- as.numeric(gsub(",","", inventory$v_Konfektion))
inventory$v_Schuhpflege  <- as.numeric(gsub(",","", inventory$v_Schuhpflege))
inventory$v_tuecherschals  <- as.numeric(gsub(",","", inventory$v_tuecherschals))
inventory$v_Guertel  <- as.numeric(gsub(",","", inventory$v_Guertel))
inventory$v_Stiefel  <- as.numeric(gsub(",","", inventory$v_Stiefel))
inventory$v_KrawattenFliegen  <- as.numeric(gsub(",","", inventory$v_KrawattenFliegen))
inventory$v_Formalwearaccessories  <- as.numeric(gsub(",","", inventory$v_Formalwearaccessories))
inventory$v_cooperations  <- as.numeric(gsub(",","", inventory$v_cooperations))
inventory$v_Oberteile  <- as.numeric(gsub(",","", inventory$v_Oberteile))
inventory$v_Handschuhe  <- as.numeric(gsub(",","", inventory$v_Handschuhe))
inventory$v_Electroniccases  <- as.numeric(gsub(",","", inventory$v_Electroniccases))
inventory$v_Kopfaccessoires  <- as.numeric(gsub(",","", inventory$v_Kopfaccessoires))
inventory$v_Hosen  <- as.numeric(gsub(",","", inventory$v_Hosen))
inventory$v_Taschen  <- as.numeric(gsub(",","", inventory$v_Taschen))
inventory$v_Unterbekleidung  <- as.numeric(gsub(",","", inventory$v_Unterbekleidung))
inventory$v_unknowncg3  <- as.numeric(gsub(",","", inventory$v_unknowncg3))
inventory_c <- inventory[1:21]
setnames(inventory_c,1,"date")
setnames(inventory_c,2:21, lev)
meltinventory <- melt(inventory_c,id.vars = "date")

