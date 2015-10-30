# Run it after running the console once
# See how many tickets are available for each section
# timeline <- "2010-09-18 15:11:25"
# timeline <- "2010-09-20 15:11:25"

#library(EBImage)
library(grid)
library(dplyr)
library(jpeg)

setwd("/Users/ethen/ticket-system/ticket")

# --------------------------------------------
# Steps for downloading the EBImage package
# source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("EBImage")

# --------------------------------------------
# Use image for background in ggplot2
# https://kohske.wordpress.com/2010/12/26/use-image-for-background-in-ggplot2/
# R: Image Analysis using EBImage
# http://www.r-bloggers.com/r-image-analysis-using-ebimage/


ticketdata4 <- read.csv ( "processdata.csv", stringsAsFactors = FALSE )
seatimage   <- readJPEG("seat.jpg")
# can use display(image) to view the picture


# the total number of seats available for each section
totalseatspersection <- ticketdata4[, c( 2, 4 ) ] %>% group_by( Seat_Type ) %>% summarize( max(count) ) 

# /* testing
# mintime <- as.character( min(ticketdata4$my_date) )
# maxtime <- as.character( max(ticketdata4$my_date) )
# outputline <- paste( "Enter a timeline between ", mintime, " ~ ", maxtime," :", sep = "" )

# set a timeline to see how many tickets are left for each section
# timeline <- "2010-09-18 15:11:25"
# timeline  <- character()
# timeline1 <- timeline
# if( length(timeline1) == 0 )
# {
#     timeline <- readline( prompt = outputline )    
# } 
# */

ticketdata4$my_date <- strptime( ticketdata4$my_date, format = "%Y-%m-%d %H:%M:%S")
image <- function( timeline )
{  
    ticketdata4$diff <- strptime( timeline, format = "%Y-%m-%d %H:%M:%S" ) - ticketdata4$my_date 
    
    # exclude the timeline that haven't occured , diff < 0
    ticketdata5 <- ticketdata4[ !(ticketdata4$diff < 0 ), ]
    ticketdata5$diff <- sapply(ticketdata5$diff, as.numeric)
    # get the closest timeline to the specified time
    closesttime <- ticketdata5[, c( 2, 5 ) ] %>% group_by( Seat_Type ) %>% summarize( min(diff) )
    
    # store the current ticket number
    tmp1 <- vector( mode = "list", length = nrow(closesttime) )
    
    for( i in 1:nrow(closesttime) )
    {
        count <- subset( ticketdata5, ( Seat_Type == as.character(closesttime[i,1]) ) & ( diff == as.numeric(closesttime[i,2]) ) )  
        
        tmp1[[i]] <- count[ nrow(count), c( 2, 4 ) ] # seat type column 1, count column four
    }
    
    # combine the count data
    countdata <- do.call( rbind, tmp1 )
    
    # deal with the Seat_Type that have not been included (The specified time is before that
    # ticket is being sold)
    if( nrow(countdata) != n_distinct(ticketdata4$Seat_Type) )
    {
        tmp2 <- vector( mode = "list", length = n_distinct(ticketdata4$Seat_Type)-nrow(countdata) )
        
        uniqueticket <- unique(ticketdata4$Seat_Type)
        
        for(i in 1:n_distinct(ticketdata4$Seat_Type)  )
        {
            if( !(uniqueticket[i] %in% countdata$Seat_Type) )
            {
                tmp2[[i]] <- ticketdata4[ , c( 2, 4 ) ] %>% 
                             filter( Seat_Type == uniqueticket[i] ) %>% 
                             top_n( 1, count )    
            }    
        }
        
        countdata1 <- do.call( rbind, tmp2 )
        countdata  <- rbind( countdata, countdata1 ) %>% arrange(Seat_Type)    
    }    
    
    # View(ticketdata4)
    # View(totalseatspersection)
    
    # -----------------------------------------
    # start doing the plotting 

    grid.newpage()
    
    # viewport gives a rectangular region which is used to orient a plot
    vp <- viewport( x = 0.5, y = 0.5, width = 0.9, height = 0.9 )
    
    # After we construct a viewport, we need to tell R to use it. This is done with the
    # pushViewport() function
    pushViewport(vp)
    
    # a rectangle (with dashed lines) on the border of the viewport:
    # grid.rect( gp = gpar( lty = "dashed" ) )
    
    # place in the image file
    seatpic <- rasterGrob(seatimage)
    grid.draw(seatpic)
    
    # the coordinates of the lower left corner of the viewport are (0,0), and the upper
    # right corner has coordinates (1,1)
    
    # -------
    # legend
    grid.text( "Total Number of Seats: #", x = 0.15, y = 0.9, gp = gpar( col = "blue", fontface = "bold" ) )
    grid.text( "Number of Seats Available: #", x = 0.16, y = 0.87, gp = gpar( col = "red" , fontface = "bold" ) )
    grid.text( timeline, x = 0.14, y = 0.93, gp = gpar( fontface = "bold" ) )
    # -------------------------------
    # Total number of seats available
    # B1 Section
    grid.text( as.character(totalseatspersection[24,2]), x = 0.44, y = 0.85, gp = gpar( col = "blue", fontface = "bold" ) )   
    grid.text( as.character(totalseatspersection[25,2]), x = 0.53, y = 0.85, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[26,2]), x = 0.44, y = 0.68, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[27,2]), x = 0.53, y = 0.68, gp = gpar( col = "blue", fontface = "bold" ) ) 
    # Purple Section
    grid.text( as.character(totalseatspersection[1,2]), x = 0.3, y = 0.8 , gp = gpar( col = "blue", fontface = "bold" ) )   
    grid.text( as.character(totalseatspersection[2,2]), x = 0.3, y = 0.71, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[3,2]), x = 0.3, y = 0.61, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[4,2]), x = 0.3, y = 0.49, gp = gpar( col = "blue", fontface = "bold" ) ) 
    # Red Section
    grid.text( as.character(totalseatspersection[5,2]), x = 0.66, y = 0.8 , gp = gpar( col = "blue", fontface = "bold" ) )   
    grid.text( as.character(totalseatspersection[6,2]), x = 0.66, y = 0.71, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[7,2]), x = 0.66, y = 0.61, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[8,2]), x = 0.66, y = 0.49, gp = gpar( col = "blue", fontface = "bold" ) ) 
    # Yellow Section 2F
    grid.text( as.character(totalseatspersection[9,2]) , x = 0.57, y = 0.47, gp = gpar( col = "blue", fontface = "bold" ) )   
    grid.text( as.character(totalseatspersection[10,2]), x = 0.54, y = 0.43, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[11,2]), x = 0.5 , y = 0.43, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[12,2]), x = 0.45, y = 0.43, gp = gpar( col = "blue", fontface = "bold" ) )
    grid.text( as.character(totalseatspersection[13,2]), x = 0.43, y = 0.47, gp = gpar( col = "blue", fontface = "bold" ) )
    # Yellow Section 3F right
    grid.text( as.character(totalseatspersection[14,2]), x = 0.69, y = 0.33, gp = gpar( col = "blue", fontface = "bold" ) )   
    grid.text( as.character(totalseatspersection[15,2]), x = 0.69, y = 0.25, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[16,2]), x = 0.69, y = 0.12, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[17,2]), x = 0.63, y = 0.05, gp = gpar( col = "blue", fontface = "bold" ) )
    grid.text( as.character(totalseatspersection[18,2]), x = 0.52, y = 0.26, gp = gpar( col = "blue", fontface = "bold" ) )
    # Yellow Section 3F left
    grid.text( as.character(totalseatspersection[19,2]), x = 0.48, y = 0.26, gp = gpar( col = "blue", fontface = "bold" ) )   
    grid.text( as.character(totalseatspersection[20,2]), x = 0.33, y = 0.05, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[21,2]), x = 0.27, y = 0.12, gp = gpar( col = "blue", fontface = "bold" ) ) 
    grid.text( as.character(totalseatspersection[22,2]), x = 0.27, y = 0.25, gp = gpar( col = "blue", fontface = "bold" ) )
    grid.text( as.character(totalseatspersection[23,2]), x = 0.27, y = 0.33, gp = gpar( col = "blue", fontface = "bold" ) )
    
    # ----------------
    # Ticket available
    # B1
    grid.text( countdata[24,2], x = 0.47, y = 0.85, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[25,2], x = 0.56, y = 0.85, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[26,2], x = 0.47, y = 0.68, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[27,2], x = 0.56, y = 0.68, gp = gpar( col = "red", fontface = "bold" ) )
    # Purple
    grid.text( countdata[1,2], x = 0.33, y = 0.8 , gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[2,2], x = 0.33, y = 0.71, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[3,2], x = 0.33, y = 0.61, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[4,2], x = 0.33, y = 0.49, gp = gpar( col = "red", fontface = "bold" ) )
    # Red
    grid.text( countdata[5,2], x = 0.69, y = 0.8 , gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[6,2], x = 0.69, y = 0.71, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[7,2], x = 0.69, y = 0.61, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[8,2], x = 0.69, y = 0.49, gp = gpar( col = "red", fontface = "bold" ) )
    # Yellow 2F
    grid.text( countdata[9,2] , x = 0.57, y = 0.44, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[10,2], x = 0.54, y = 0.4 , gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[11,2], x = 0.5 , y = 0.4 , gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[12,2], x = 0.45, y = 0.4 , gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[13,2], x = 0.43, y = 0.44, gp = gpar( col = "red", fontface = "bold" ) )
    # Yellow 3F right
    grid.text( countdata[14,2], x = 0.72, y = 0.33, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[15,2], x = 0.72, y = 0.25, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[16,2], x = 0.72, y = 0.12, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[17,2], x = 0.66, y = 0.05, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[18,2], x = 0.52, y = 0.24, gp = gpar( col = "red", fontface = "bold" ) )
    # Yellow 3F left
    grid.text( countdata[19,2], x = 0.48, y = 0.24, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[20,2], x = 0.36, y = 0.05, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[21,2], x = 0.3 , y = 0.12, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[22,2], x = 0.3 , y = 0.25, gp = gpar( col = "red", fontface = "bold" ) )
    grid.text( countdata[23,2], x = 0.3 , y = 0.33, gp = gpar( col = "red", fontface = "bold" ) )
}




