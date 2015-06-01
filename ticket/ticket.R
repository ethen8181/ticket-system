# SNSD(Girl Generation) ticket data
library(RColorBrewer)
library(data.table)
library(gridExtra)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)


setwd("C:/Users/ASUS/ticket-system/ticket")
Sys.setlocale("LC_TIME", "English")

ticketdata1 <- read.csv( "ticketdata.csv", stringsAsFactors = FALSE )

# deal with the date row
ticketdata1 <- separate(ticketdata1, CREATE_DATE, into = c( "date1", "date2", "date3" ), sep = " " ) 

ticketdata1$my_date <- strptime( paste( ticketdata1$date1, ticketdata1$date3, sep = " " ), format = "%Y/%m/%d %H:%M:%S" )  

# Add the requistite number of seconds to the object , add 12 hours to the data that are in the afternoon
ticketdata1$my_date[ ticketdata1$date2 == "p.m." ] <- ticketdata1$my_date[ ticketdata1$date2 == "p.m." ] + 12*60*60

# delete the original three columns that were used to create my_date column
which( names(ticketdata1) == "date1" )
ticketdata2 <- ticketdata1[ -c( 8:10 ) ]
setnames(ticketdata2, c( "T_STANDARD_TICKET_TYPE_NAME", "SEAT_REGION_NAME" ), c("Ticket_Type", "Seat_Type") )
# View(ticketdata2)

# ---------------------------------------------------------------
# See how each section of the tickets were sold out through time

# select the columns needed
ticketdata3 <- select( ticketdata2, my_date, Seat_Type, Ticket_Type )   


# plot of table(ticketdata2$Seat_Type), grouped by ticket_type
# adjust the x axis text using angle and hjust to prevent them from clumping up
# distinct( select( ticketdata2, Seat_Type ) )
# position = "fill", normalize the stacked bar plot
# add order = desc(Ticket_Type)
# legend can be reversed by guides(fill = guide_legend( reverse = TRUE )
seatplot <- ggplot( ticketdata3, aes( x = Seat_Type, fill = Ticket_Type, order = desc(Ticket_Type) ) ) + geom_bar( position = "fill" ) + 
              ggtitle( "Seat_Type Distribution" ) + theme(
                plot.title   = element_text( size = 28, face = "bold" ),
                legend.title = element_text( size = 16 ),
                legend.text  = element_text( size = 12 ),
                axis.title   = element_text( size = 18 ),    
                axis.text    = element_text( size = 10 ),
                axis.text.x  = element_text( angle = 30, hjust = 1 ) ) +
              

# ----------------------------------
# plot the graph showing which how fast each kind of Seat_Types were sold out
# ticketdata2 : tidy data
# ticketdata3 : order the ticketdata by my_date
# dplyr does not support POSIXlt, POSIXt type 
ticketdata3 <- ticketdata3[order(ticketdata3$my_date),]


# -------------------------------
# create two function 
# process : it does the counting of every distinct Seat_Type by time
# ticketsoldplot : plot the data

process <- function( dataset )
{
    tickettype <- distinct( select( dataset, Seat_Type ) )
    
    tmp <- vector( mode = "list", length = nrow(tickettype) )
    
    for( i in 1:nrow(tickettype) )
    {
        data  <- data.frame()
        data  <- subset( dataset, Seat_Type == tickettype[i,] )
        
        data$count <- ( nrow(data)-1 ):0
        tmp[[i]] <- data
    } 
    
    data1 <- do.call( rbind, tmp )
    data1
} 

# function that does the plotting
ticketsoldplot <- function( dataset, ... )
{
    ggplot( dataset, aes( x = my_date, y = count, color = Seat_Type ) ) + geom_line( size = 1 ) + 
        facet_wrap( ~Ticket_Type, ncol = 1, scales = "free" ) + theme(
            strip.text   = element_text( face = "bold", size = 16 ),
            strip.background = element_rect( fill = "lightblue" ),
            axis.title   = element_text( size = 14 ),
            axis.text    = element_text( size = 10 ),
            legend.title = element_text( size = 14 ),
            legend.text  = element_text( size = 12 )) 
    
}    

# ------------------------------------------------------------------------------------
# ticketdata4 : all kinds, total of 27 Seat_Type type can be quite unpleasant to look at
# tickettype_all : every kind of seat in one plot
ticketdata4    <- process(ticketdata3)
cols <- brewer.pal( 12, "Set3" )
pal1 <- colorRampPalette(cols)
color <- pal1( length( unique(ticketdata4$Seat_Type) ) )
tickettype_all <- ticketsoldplot(ticketdata4) + scale_color_manual( values = color)
tickettype_all

# ticketdata6 : combine the Seat_Type type to just 3F, 2F and B1
# tickettype_3group : plot, which combines the Seat_Type type to just 3F, 2F and B1
ticketdata5 <- ticketdata3

ticketdata5$Seat_Type <- gsub( "FloorB1(FloorSection).*", "\\1", ticketdata5$Seat_Type ) 
ticketdata5$Seat_Type <- gsub( "Floor([23])+Section.*", "\\1F" , ticketdata5$Seat_Type ) 

ticketdata6 <- process(ticketdata5)

tickettype_3group <- ticketsoldplot(ticketdata6)   

# extract the 3F, 2F and B1 each into one plot
ticketdata4_2f <- ticketdata4[ grep( "Floor([2])+Section.*", ticketdata4$Seat_Type ), ]
ticketdata4_3f <- ticketdata4[ grep( "Floor([3])+Section.*", ticketdata4$Seat_Type ), ]
ticketdata4_B1 <- ticketdata4[ grep( "FloorB1",  ticketdata4$Seat_Type ), ]

tickettype_2f <- ticketsoldplot(ticketdata4_2f)  
tickettype_3f <- ticketsoldplot(ticketdata4_3f)  
tickettype_B1 <- ticketsoldplot(ticketdata4_B1)  

# Plot
# seatplot : Seat distribution plot
# tickettype_all : every kind of seat in one plot
# tickettype_3group : plot, which combines the Seat_Type type to just 3F, 2F and B1
# tickettype_2f
# tickettype_3f
# tickettype_B1

# how many tickets are there for each ticket price
pricedata <- data.frame( table(ticketdata2$PRICE) )
ggplot( pricedata, aes( Var1, Freq ) ) + geom_bar( stat = "identity", color = "black", fill = "tomato", width = 0.8 ) +               
    ggtitle( "Purchase Amount per Person" ) + xlab( "price" ) + theme( 
        plot.title = element_text( size = 28, face = "bold" ),
        axis.title = element_text( size = 18 ),
        axis.text  = element_text( size = 12 )) + 
          geom_text( aes( label = Freq ), vjust = -0.2 )  

# amount of purchases per person
purchasedata <- aggregate( PRICE ~ IDENTITY, ticketdata2, sum )
head(purchasedata)

purchasehisto <- ggplot( purchasedata, aes( PRICE ) ) + geom_bar( color = "black", fill = "lightskyblue2", width = 0.8 )           
                   ggtitle( "Purchase Amount per Person" ) + xlab( "price" ) + theme( 
                     plot.title = element_text( size = 28, face = "bold" ),
                     axis.title = element_text( size = 18 ),
                     axis.text  = element_text( size = 12 ))  

#  Amount of Purchase by Gender 
genderprice <- ave( ticketdata2[,"PRICE"], ticketdata2[,"IDENTITY"], FUN = sum )
genderdata  <- data.frame( identity = ticketdata2$IDENTITY, price = genderprice, gender = ticketdata2$SEX )
# exclude the same data
genderdata  <- unique(genderdata)

ggplot( genderdata, aes( gender, price, fill = gender ) ) + geom_boxplot() +
    ggtitle( "Amount of Purchase by Gender" ) + theme(
        plot.title   = element_text( size = 24, face = "bold" ),
        axis.title   = element_text( size = 18 ),
        axis.text    = element_text( size = 12 ),
        legend.title = element_text( size = 16 ),
        legend.text  = element_text( size = 14 ))





