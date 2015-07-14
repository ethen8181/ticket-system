library(data.table)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

Sys.setlocale("LC_TIME", "English")
setwd("C:/Users/ASUS/ticket-system/system")

files <- list.files( "data", full.names = TRUE )
data  <- fread( files, stringsAsFactors = FALSE, header = TRUE, sep = ",", colClasses = "character" )

# ---------------------------------------------------------------------------------
# total price for every kind ticket, top 50, add new column counting the difference
# of the original total price and the sold total price

price <- data[ , .( original = sum( as.numeric(OriginalPrice) ), 
                    sold = sum( as.numeric(SoldPrice) ), count = .N ), by = TicketCode ] %>%
         arrange( desc(sold), desc(original), desc(count) ) %>% 
         top_n( 50, sold ) %>%
         mutate( diff = original - sold )
    
# top 50 sold prices
ggplot( price, aes( original, sold, size = count, color = diff ) ) + 
    geom_point( alpha = .6 ) + 
    scale_size_continuous( range = c( 5, 20 ) ) + 
    scale_color_gradient( low = "lightblue", high = "darkblue" ) + 
    ggtitle("Top 50 Ticket Revenue")

# ---------------------------------------------------------------------------------

# extract the data ticketcode which their sold are larger than 10^7
high <- price$TicketCode[ (price$sold > 10^7) ]
highdata <- data[ TicketCode %in% high, ] %>% filter( TicketSiteCode == 88888 )

# ---------------------------------------------------------------------------------
# analyze mean of sold price by gender

mean1 <- aggregate( as.numeric(SoldPrice) ~ TicketCode + Gender, data = highdata, FUN = mean ) %>%
         arrange( TicketCode )
# rename the third column, it was too lengthy
names(mean1)[3] <- "Price"
# plot of the mean 
ggplot( mean1, aes( as.factor(Gender), Price, color = TicketCode, group = TicketCode ) ) + 
    geom_point( size = 5 ) + geom_line() 

# conduct t-test to see if there actually is a difference in the mean for each ticket
# rejection level of p-value
alpha <- .05
sapply( high, function(x)
{
    # extract only the needed column from the data
    tmp <- highdata[ TicketCode == x, ] %>% select( SoldPrice, Gender )
    # check the equality of variance for the t-test
    boolean <- var.test( as.numeric(SoldPrice) ~ as.factor(Gender), data = tmp, 
               alternative = "two.sided" )$p.value > alpha
    # conduct the t-test, return boolean, true stating that there's a 
    # difference between the two gender regarding the mean of amount of sold tickets 
    t.test( as.numeric(SoldPrice) ~ as.factor(Gender), data = tmp, 
            paired = FALSE, var.equal = boolean )$p.value < alpha    
})

#------------------------------------------------------------------------------------
# analyze the age distribution

highdata[ , age := year(today()) - as.numeric(BirthYear) ]
# female dominate
table(highdata$Gender)
aggregate( as.numeric(SoldPrice) ~ TicketCode + Gender, data = highdata, FUN = sum ) %>%
    arrange( TicketCode )

# extract one of the ticket concert and look at its age distribution 
agedata <- highdata[ TicketCode == high[1], ] %>% select( SoldPrice, Gender, age )
# age distribution histogram by gender
ggplot( agedata, aes( age, fill = Gender ) ) + geom_histogram() + facet_grid( ~ Gender )

# define age breaks
breaks <- with( highdata, c( min(age), seq( 10, 60, 10 ) , max(age) ) )
highdata$cut <- cut( highdata$age, breaks = breaks, include.lowest = TRUE )
table(highdata$cut)

# the sum of sold price for every ticket, gender and age breaks
sum1 <- highdata[ , .( sum = sum( as.numeric(SoldPrice) ) ), 
                by = list( TicketCode, Gender, cut ) ] %>% arrange( TicketCode, cut )
# plot
ggplot( sum1, aes( Gender, cut, color = Gender, size = sum ) ) + 
    geom_point( alpha = .8 ) + facet_grid( ~ TicketCode ) + 
    scale_size_continuous( range = c( 5, 20 ) ) 

# ------------------------------------------------------------------
# ZipCode

head(highdata$ZipCode)
# convery the zipcity to numeric to elevaluate only the ones from 0-9
highdata$zipcity <- substring( highdata$ZipCode, 1, 1 ) %>% as.numeric()

# store the rows that were coerced to NAs, exclude them from highdata
narow <- which( is.na(highdata$zipcity) )
zipcodedata <- highdata[ -narow, ]
# this is the same as the two lines of code above
zipcodedata <- highdata[ complete.cases(highdata), ]
View(zipcodedata)

# contingency table : gender and zipcity
gender_zip <- with( zipcodedata, table( Gender, zipcity ) )
addmargins(gender_zip)
chisq.test(gender_zip)$expected

# contingency table : TicketCode and zipcity
ticket_zip <- with( highdata, table( TicketCode, zipcity ) )
# convert the contingency table to data frame
ticket_zip_df <- data.frame(ticket_zip) %>% spread( zipcity, Freq )
# does the same thing as the code as above 
library(reshape2)
dcast( data.frame(ticket_zip), TicketCode ~ zipcity, value.var = "Freq" )
xtabs( Freq ~ TicketCode + zipcity, data = data.frame(ticket_zip) )

geographic <- list( c( "1","2" ), c( "3","4","5" ), c( "6","7","8" ), c( "9","0" ) )
# combine it by geographic
combine <- lapply( geographic, function(x)
{
    subset( ticket_zip_df, select = x ) %>%
        apply( 1, sum )
})
combined_ticket_zip <- cbind( ticket_zip_df$TicketCode, 
                              data.frame( do.call( cbind, combine ) ) )
names(combined_ticket_zip) <- c( "TicketCode", "North", "Mid", "South", "East" )

# get the actual "numbers", proportion for each region
# convert data frame into long format to get the table
longformat <- gather( combined_ticket_zip, "Region", "Freq", -1 )
longtable  <- xtabs( Freq ~ TicketCode + Region, data = longformat )
addmargins(longtable)
# use prop.table
prop.table( longtable, 1 )

ggplot( longformat, aes( TicketCode, Freq, fill = Region ) ) + 
    geom_bar( stat = "identity", position = "fill" ) 
# mosaic plot
source("mosaic_plot.R")
mosaic_plot(combined_ticket_zip)
# age and gender table for one of the ticket concert
tabledata <- zipcodedata %>% filter( TicketCode == "0000010440" ) %>% select( Gender, cut )
table(tabledata)

# ------------------------------------------------------------------
# analyze TicketSiteCode

topdata <- data[ TicketCode %in% high, ]
site <- topdata[ , .( sum = sum( as.numeric(SoldPrice) ) ), by = TicketSiteCode ] %>% arrange( desc(sum) )
site
sapply( c( .7, .8 ), function(x)
{
    mean( !cumsum( site$sum / sum(site$sum) ) > x ) * 100   
})

# ------------------------------------------------------------------
# time series

# paste the two column together and exclude unneccesary time
string <- gsub( "(.*)\\s.*\\s(.*)\\.[0]{3}", "\\1 \\2", 
                with( topdata, paste( SoldDate, SoldTime, sep = "" ) ) ) 
# convert character to time
topdata$SoldDate <- ymd_hms(string)
# exclude SoldTime column
topdata$SoldTime <- NULL
# order the data by time, exclude the tickets that were given away for free
topdata <- topdata[ order(topdata$SoldDate), ] %>% 
               filter( !topdata$SoldPrice %in% c( 0, 10 ) )

# fill in the count
process <- lapply( unique(topdata$TicketCode), function(x)
{
    # extract each unique data
    boolean <- topdata$TicketCode == x
    # exclude the free given ticket
    subdata <- topdata[ boolean, ] 
    # normalize the data (x-min)/(max-min), times 100 to express it in percentage
    subdata$count <- ( nrow(subdata):1-1 ) / (nrow(subdata)-1) * 100
    return(subdata)
})    
pdata <- do.call( rbind, process )
# use the pdata ( plot data ) to plot the ticket sold out rate
ggplot( pdata, aes( SoldDate, count, color = TicketCode ) ) + geom_line( size = 1 )


# ---------------------------------------------------------
# cumulative sales
pdata$cumsum <- ave( pdata$SoldPrice , pdata$TicketCode, FUN = cumsum )
ggplot( pdata, aes( SoldDate, as.numeric(cumsum), color = TicketCode ) ) + geom_line()






