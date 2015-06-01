#  Data analysis on webflow data 2010/10/01-2010/10/30
library(plyr)
library(dplyr)
library(ggplot2)
library(xlsx)
library(reshape2)
library(grid)
library(gridExtra)

# set the working directory
setwd("C:/Users/ASUS/ticket-system/webflow")
Sys.setlocale("LC_TIME", "English")
# list.files()
# options(stringAsFactors = FALSE)


# webflowdata
webflowdata<- read.xlsx2( "webflowdata.xlsx", 2, colClass = c( rep( "numeric", 6 ) ), stringsAsFactors = FALSE )    

# ------ preprocessing the webflowdata
# use letters to replace the original column names
# refill the hour and the date
data1        <- webflowdata[(1:720),]
names(data1) <- letters[1:ncol(data1)]
data1$b      <- rep( c(0:23), nrow(data1)/24 )
my.dates     <- seq( as.Date("2010/10/1"), as.Date("2010/10/30"), "1 day") 
data1$a      <- rep( my.dates, each = 24 )
data1$f      <- data1$f *100

# View(data1)
# a = Date , b = Hour , c = PageView, d = UniquePageView, e = Duration time, f = Bounce rate
# combine and date and time column
data1$my_dates <- paste   ( data1$a, data1$b, sep = "" )
data1$my_dates <- strptime( data1$my_dates, format = "%Y-%m-%d%H" )


# ------------------------------------------------------------
# viewdata : pageview and uniquePageView of the whole October
viewdata  <- select( data1, my_dates, c , d )

# See a summary on how pageview and uniquepageview are correlated
summary( viewdata$c/viewdata$d )
cor(viewdata$d,viewdata$c)

# extract the data where the difference between pageview and uniquepageview are abnormal
peak1 <- viewdata[ viewdata$c/viewdata$d > 8,  ]

# create the date character
datestring <- unlist( strsplit( as.character(peak1[4,1]), split = " " ) )
timestring <- paste( as.character( peak1[3,1]$hour ), "~", datestring[2], sep = "" )
timestring <- paste( datestring[1], timestring )

# melt the data into long format to be used by ggplot
viewdata1 <- melt( viewdata, id.vars = "my_dates", value.name = "View", variable.name = "Views" )   

# manually fill in the dates that are coerced in NAs
viewdata1$my_dates <- rep( viewdata[ 1:nrow(viewdata), 1 ], 2 )

# plyr revalue function
viewdata1$Views <- revalue( viewdata1$Views, c( "c" = "PageView", "d" = "UniqueView" ) )

# plot the viewdata1
# annotate the date where pageview are bigger than uniquepageview by 8 times
plot1 <- ggplot( viewdata1, aes( x = my_dates, y = View , color = Views  ) ) + geom_line( size = 1) + 
           ggtitle( "PageView of October" ) + xlab( "Month: October" ) + 
           scale_x_datetime( labels = c( "4", "11", "18", "25", "Nov 1" ) ) + theme(
             plot.title   = element_text( size = 24, face = "bold" ),
             axis.title   = element_text( size = 18 ),
             axis.text    = element_text( size = 12 ),
             legend.position = "none" ) + 
             annotate( "text", x = peak1[1,1], y = peak1[1,2]+4*10^3, label = as.character(peak1[1,1]) ) + 
             annotate( "text", x = peak1[2,1], y = peak1[2,2]+4*10^3, label = as.character(peak1[2,1]) ) +  
             annotate( "text", x = peak1[3,1], y = peak1[3,2]+4*10^3, label = timestring ) + 
             annotate( "text", x = peak1[5,1], y = peak1[5,2]+4*10^3, label = as.character(peak1[5,1]) ) +
             geom_segment( x = as.numeric( peak1[1,1]-12*60*60 ), xend = as.numeric( peak1[1,1]-12*60*60 ), y = peak1[1,2], yend = peak1[1,3], size = 1, color = "blue", 
                           arrow = arrow( ends = "both", angle = 90, length = unit( 0.2, "cm" ) ) ) + 
             annotate( "text", x = peak1[1,1]-30*60*60, y = 10^5, label = ">8", color = "blue", size = 6 )

# ------------------------------
# Basically implements the same procedure as the viewdata above
# insightdata : Bounce Rate and Duration Time of the whole October
insightdata  <- select( data1, my_dates, e , f )
insightdata1 <- melt  ( insightdata, id.vars = "my_dates", value.name = "Freq" )   

# manually fill in the dates that are coerced in NAs
insightdata1$my_dates <- rep( insightdata[ 1:nrow(insightdata), 1 ], 2 )

# plyr revalue function
insightdata1$variable <- revalue( insightdata1$variable, c( "e" = "DurationTime", "f" = "BounceRate" ) )

# plot the insightdata1
plot2 <- ggplot( insightdata1, aes( x = my_dates, y = Freq , color = variable  ) ) + geom_line( size = 1) + 
           ggtitle( "Duration Time & Bounce Rate of October" ) + xlab( "Month: October" ) + 
           scale_x_datetime( labels = c( "4", "11", "18", "25", "Nov 1" ) ) + theme(   
             plot.title   = element_text( size = 20, face = "bold" ),
             axis.title   = element_text( size = 18 ),
             axis.text    = element_text( size = 12 ),
             legend.position = "none" ) + scale_color_manual( values = c( "dodgerblue3", "firebrick3" ) )


# -----------------------------------------------------------
# pageviewdata: pageview and uniqupageview aggregated by hour
pageviewdata  <- aggregate( list( PageView = c, UniqueView = d ), list( hour = b ), FUN = mean )
pageviewdata1 <- melt(pageviewdata, id.vars = "hour", value.name = "View")

plot3 <- ggplot(pageviewdata1, aes( x = hour, y = View, color = variable ) ) + geom_line( size = 2 ) + geom_point( size = 5 ) +
           ggtitle("PageView By Hour") + theme(
             plot.title   = element_text( size = 24, face = "bold" ),
             axis.title   = element_text( size = 18 ),
             axis.text    = element_text( size = 12 ),
             legend.title = element_text( size = 16 ),
             legend.text  = element_text( size = 14 )) 

# -----------------------------------------------------------------
# Basically implements the same procedure as the pageviewdata above
# plotdata: Bounce rate and duration time aggregated by hour 
plotdata  <- aggregate( list( DurationTime = e, BounceRate = f ), list( hour = b ), FUN = mean )
plotdata1 <- melt(plotdata, id.vars = "hour", value.name = "Freq")

plot4 <- ggplot(plotdata1, aes( x = hour, y = Freq, color = variable ) ) + geom_line( size = 2 ) + geom_point( size = 5 ) +
           ggtitle("Duration Time & Bounce Rate By Hour") + theme( 
             plot.title   = element_text( size = 18, face = "bold" ),
             axis.title   = element_text( size = 18 ),
             axis.text    = element_text( size = 12 ),
             legend.title = element_text( size = 16 ),
             legend.text  = element_text( size = 14 )) + scale_color_manual( values = c( "dodgerblue3", "firebrick3" ) )         

# ------------------------------
# Use of gridExtra package
# plot1 = pageview by month ;  plot2 = duration time & bounce rate by month
# plot3 = average pageview by hour ; plot4 = average duration time & bounce rate by hour

grid.arrange( plot1, plot3, ncol = 2 )
grid.arrange( plot2, plot4, ncol = 2 )


