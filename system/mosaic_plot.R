# Create a customized mosaic plot
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(dplyr)

mosaic_plot <- function( df )
{
    # Step 1 : calculate the market segment proportion, the x axis value
    sumofeachrow <- apply( df[ , -1 ], 1, sum )
    # represent each product as the proportion of a product market in each region
    df[ , -1 ] <- df[ , -1 ] / sumofeachrow * 100
    # times it by 100 to represent percentage
    segment <- sumofeachrow / sum(sumofeachrow) * 100
    # width of each column, this records the breaks of the x axis of the plot
    df$xmin <- cumsum(segment) - segment
    df$xmax <- cumsum(segment)
    
    # Step 2 : proportion of a product market in each region, the y axis
    # convert the data into long format to calculate the breaks for the y axis
    # remember to arrange the product column(TicketCode)
    data <- melt( df, id.vars = c( "TicketCode", "xmin", "xmax" ) ) %>%
                arrange( TicketCode )
    # caculate the cumulative sum for each product 
    yaxis <- unlist( with( data, tapply( value, TicketCode, FUN = cumsum ) ) )
    # breaks of the y axis of the plot
    data$ymin <- yaxis - data$value
    data$ymax <- yaxis
    
    # Step 3 : add the positioning of the text
    # in the middle of each respective axis
    data$xtext <- with( data, ( xmin + xmax ) / 2 ) 
    data$ytext <- with( data, ( ymin + ymax ) / 2 )
    # for this part, I only wanted to print one label that I want to talk about
    labelrow <- data %>% filter( TicketCode == "0000010440" & variable == "Mid" )
    
    # Create the plot using geom_rect
    # 1 : exclude the black line that appears in the legend when adding color
    # 2 : add the product(TicketCode) on top of the plot
    # 3 : add the labels on the bar, remember to add data =
    # 4 : change the breaks for the x axis, represents each maket's proportion size
    ggplot( data, aes( xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = variable ) ) + 
        geom_rect( color = "black" ) + 
        guides( fill = guide_legend( override.aes = list( colour = NA ) ) ) + # 1
        geom_text( aes( x = xtext, y = 103, label = TicketCode ), size = 4 ) + # 2
        geom_text( data = labelrow, aes( x = xtext, y = ytext, label = paste( round(value), "%", sep = "" ) ) ) + #3
        scale_fill_manual( values = brewer.pal( 4,"Set3" ) ) + 
        scale_x_continuous( breaks = c( 0, round( df$xmax ) ) ) + # 4
        labs( title = "Mosaic Graph ( Mailed Tickets to Each Region for Each Concert )", 
              x = "market size proportion", y = "product proportion", fill = "region" )
}    



