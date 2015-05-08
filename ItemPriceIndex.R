library(googleVis)

#This graph takes a price index for a number of items and swings it around in a circle where each item has its own quadrant.
#The result is then plotted on Google Graph Motion Chart to show the item price shift over time in relation to one another.
#Prices are all indexed to one item (Large Big Burger)

#Read CSV
input<- read.csv(file="data/item-prices.csv", header=TRUE)

#Cast date column as date
input$date <- as.Date(input$date)

#Get item quadrant
uniqueItems<-unique(input$iname) #Get unique items
uniqueCount<-length(unique(input$iname)) #count number of unique items
uniqueDates<-unique(input$date)
itemQuadrant<-data.frame(iname=uniqueItems, quad=c(1:uniqueCount)) #Get unique items and quadrant rank

#Join main table
quadTable<-merge(x=input, y = itemQuadrant, by="iname", all.y=TRUE) #Full join input on iname

#Add angle for each item based on quadrant
quadTable$angle<-(360/uniqueCount)*quadTable$quad

#Get (x,y) coordinates
quadTable$x<-sqrt(quadTable$index^2+quadTable$index^2)*sin(pi/180*quadTable$angle)
quadTable$y<-sqrt(quadTable$index^2+quadTable$index^2)*cos(pi/180*quadTable$angle)

colnames(quadTable)

#Get base coordinate 0 and join it together with table
df.origin<-data.frame(iname="Base", iitem=0, date=unique(quadTable$date), index=0
                      , avgprice=0, upt=10, quad=max(quadTable$quad)+1, angle=0,x=0,y=0)

#Get starting base index coordinates based on index=1
#   dfOne<- unique(data.frame(data.frame(paste(quadTable$iname, "Base", sep="-")),0,0
#                             ,1,0,5,dim(quadTable)[2],0,(sin(pi/180*quadTable$angle)),(cos(pi/180*quadTable$angle))))

df.temp<-data.frame(iname=paste(itemQuadrant$iname,"Base",sep="-"), iitem = 0, index=1, avgprice=0, upt=5, quad=itemQuadrant$quad)
df.temp$angle<-(360/uniqueCount)*df.temp$quad
df.temp$quad<-uniqueCount+1
df.temp<-merge(df.temp, uniqueDates, all.y=T)
colnames(df.temp)[ncol(df.temp)]<-"date"
df.temp<-df.temp[,c("iname","iitem","date","index","avgprice","upt","quad","angle")]
df.temp$x<-sqrt(2)*sin(pi/180*df.temp$angle)
df.temp$y<-sqrt(2)*cos(pi/180*df.temp$angle)

quadTable<-rbind(quadTable, df.origin, df.temp)

quadTable<-quadTable[order(quadTable$iitem,quadTable$date),]

myStateSettings <-'
    {"yZoomedDataMin":-2.335724287,"yZoomedDataMax":1.889257463,"dimensions":{"iconDimensions":["dim0"]}
    ,"sizeOption":"5","duration":{"timeUnit":"D","multiplier":1},"time":"2004-12-27","yLambda":1
    ,"iconKeySettings":[{"LabelX":-134,"LabelY":34,"key":{"dim0":"Cheeseburger"}},{"key":{"dim0":"Large Big Burger"}}
    ,{"LabelX":-230,"LabelY":51,"key":{"dim0":"1-pc Chicken & Rice"}},{"key":{"dim0":"Nuggets"}}
    ,{"key":{"dim0":"Triple Cheeseburger"}},{"key":{"dim0":"Spicy Chicken Burger"}},{"LabelX":-265,"LabelY":-92
    ,"key":{"dim0":"Reg Fries & Float"}},{"key":{"dim0":"2-pc Chicken"}},{"LabelX":-350,"LabelY":-40
    ,"key":{"dim0":"Chicken Burger w Tea"}},{"key":{"dim0":"Beefburger & Float"}},{"key":{"dim0":"Double Cheeseburger"}}
    ,{"key":{"dim0":"Chicken Burger"}}]
    ,"orderedByX":false,"showTrails":false,"xZoomedDataMin":-2.875967601,"playDuration":40000
    ,"nonSelectedAlpha":1,"xZoomedIn":false,"xLambda":1,"colorOption":"6","yZoomedIn":false
    ,"xZoomedDataMax":2.455682017,"xAxisOption":"8","iconType":"BUBBLE"
    ,"uniColorForNonSelected":false,"orderedByY":false,"yAxisOption":"9", "showXMetricPicker":false, "showAdvancedPanel":false,
    "showChartButtons":false}'


#Create motion chart
plot(gvisMotionChart(quadTable, idvar="iname", timevar="date", xvar=8, yvar=9, colorvar=6, sizevar=7
                , options=list(height=750, width=1000 
                               , state=myStateSettings)))