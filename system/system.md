# Ticket System Data Analysis

**2015.9.12 Ethen Liu**

## Excecute Summary
After performing some exploratory analysis based on the dataset of a ticket selling system, we were able to discover four primary facts.    

1. The major audience segment of the top-saling tickets(concerts) for this ticket system seems to be female customers whose age is about 30 ~ 40.
2. Geographically, personal taste on concerts varies in different parts of Taiwan, with the northern region generating most of the sells.
3. Of all the channels that sells tickets, online channel alone contributed to about 60 percent of the system's total revenue.
4. Discovered three different patterns when looking at the sold out rate for the 7 top-saling concert. 

## I. Data Description
The dataset used for this report is the ticket selling record for a ticket system for January to April. It consists of 221722 observations and 13 variables. The column names should already be quite descriptive of what each variables stands for, so a variable dictionary will not be included. However, there are a couple of things that may need some explaining.

- `TicketCode` If you want to aggregate the data of the same concert you should do it by using this column. Do not use the `TicketName` column, they might be different for the same concert due to marketing reasons ( making the name look cuter promotes sales...? ). 
- `TicketSiteCode` The code number 88888 tells us that the ticket was sold by online platform, while all the other site code each represents different physical channel ( e.g. convenience stores, bookstores etc. ).
- For `Gender` and `BirthYear` column, unless the column `TicketSiteCode` is 88888 for that corresponding row, then the values for these two columns will be meaningless. The number that appears for the data cell is just the default value set by the ticket system. Therefore, if you want to conduct analysis based on the sex and age of the consumers, you should only include the observations where its `TicketSiteCode` is 88888.
- `ZipCode` The address zipcode for which the ticket was sent to. Only tickets that were bought on online platform will have data recorded for this column, the corresponding city name for the zipcode is in the `City` column. 
- To preprocess the correct time for which the ticket was bought requires combining the Year, Month, Date part from the `SoldDate` column and Hour, Minute, Second from the `SoldTime` column. The original database was somehow messed up, and splitted it into two columns.  
- `SoldPrice` The actual sold price of the ticket might be different to the original price due to reasons such as the member being a VIP member of the ticket system or the ticket site was giving out discounts if the tickets were purchase at those specific sites. Another reason is that the tickets were given for out for free ( will be recorded as 0 or 10 in the data cell due to tax ).
- `URL` Adding http://www.ticket.com.tw/ before the url should lead you to the correct website.

## II. Environment Setting 

```r
# load library
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

# prevent encoding problems
Sys.setlocale("LC_ALL", "C")

# set working directory
setwd("/Users/ethen/ticket-system/system")

# read in the files
files <- list.files( "data", full.names = TRUE )
data  <- fread( files, stringsAsFactors = FALSE, header = TRUE, sep = ",", colClasses = "character" )
```

## III. Exploratory Data Analysis

The exploratory data analysis is split into five sub-sections listed in the following. 

1. Total Ticket Revenue
2. Mean of SoldPrice by Gender
3. Age Distribution
4. Analysis on the ZipCode column
5. Analyze TicketSiteCode

#### 1. Total Ticket Revenue


Business always cares about money, don't blaim them, that is what business do, they make money. Slight digression there, anyway, let us trace back how much revenue did each TicketCode ( each concert ) generated.

- `price` Calculate the total amount of the original ticket price and the price that were sold grouped by each TicketCode. Extract the top 50 ordered by total sold price, also add an additional column that states the difference between the total original and sold price.


```r
# extract price data 
price <- data[ , .( original = sum( as.numeric(OriginalPrice) ), 
                 sold = sum( as.numeric(SoldPrice) ), count = .N ), by = TicketCode ] %>%
         arrange( desc(sold), desc(original), desc(count) ) %>% 
         top_n( 50, sold ) %>%
         mutate( diff = original - sold )
head(price)
```

```
##    TicketCode original     sold count    diff
## 1: 0000010605 34026400 33998920 11921   27480
## 2: 0000010440 32098590 27022635  8779 5075955
## 3: 0000010413 16469980 16207980  4190  262000
## 4: 0000010439 15163400 15122910  7504   40490
## 5: 0000010430 11296430 10970440  4417  325990
## 6: 0000010619 10456200 10424210  3747   31990
```

```r
# top 50 plot
ggplot( price, aes( original, sold, size = count, color = diff ) ) + 
geom_point( alpha = .6 ) + 
scale_size_continuous( range = c( 5, 20 ) ) + 
scale_color_gradient( low = "lightblue", high = "darkblue" ) + 
ggtitle("Top 50 Ticket Revenue")
```

![](system_files/figure-html/unnamed-chunk-2-1.png) 

- **Note1:** The labels indicating which TicketCode each point represents is dropped to avoid overfitting the graph. A clear picture is still seen above that some concerts are just more favored by consumers at that time, selling more tickets ( size of the circle ) and generating more ticket revenue.

- **Note2:** Another discovery from the plot that's worth pointing out is the color of one of the concerts is significantly darker than others, this implies that the total amount of the OriginalPrice largely varies with the final SoldPrice. Indicating there might be some issues regarding the original pricing of this concert. 

> The analysis of the following section will only be based on part of the dataset for simplicity. 

- `highdata` For deeper insight, we will extract the TicketCode in which their total sold revenue are larger than 10^7 and that were sold by the TicketSiteCode 88888 (internet). We will be using this subset of the data for part 2-4 of the following exploratory analysis on age, gender and zipcode. Oh, sorry for coming up with this ambiguous name for the subdata, couldn't think of a better one...

```r
# top-saling TicketCode
high <- price$TicketCode[ (price$sold > 10^7) ]
highdata <- data %>% filter( TicketCode %in% high & TicketSiteCode == 88888 )
```

#### 2. Mean of SoldPrice by Gender
The first question we would like to answer for these top-saling tickets is : Do male or female have different behaviorial patterns ?

- `mean1` SoldPrice is the amount of money that the consumer actually spent on purchasing the tickets. Let us start with looking at the average amount of money spent on buying tickets for each top-saling TicketCode and between genders.

```r
mean1 <- aggregate( as.numeric(SoldPrice) ~ TicketCode + Gender, data = highdata, 
                    FUN = mean ) %>% arrange( TicketCode )
# rename the third column, it was too lengthy
names(mean1)[3] <- "Price"
# plot of the mean 
ggplot( mean1, aes( as.factor(Gender), Price, color = TicketCode, group = TicketCode ) ) + 
geom_point( size = 5 ) + geom_line() + xlab("Gender") + 
ggtitle("Average Price Spent on Purchasing Tickets Between Genders")
```

![](system_files/figure-html/unnamed-chunk-4-1.png) 

- **Note:** Based on the plot, it is kind of hard to tell whether there actually is a difference in the mean of the SoldPrice between male and female, therefore we will confirm the notion by conducting a t-test between the two sample for every TicketCode. 


```r
# rejection level of p-value
alpha <- .05
sapply( high, function(x)
{
    # extract only the needed column from the data
    tmp <- highdata %>% filter( TicketCode == x ) %>% select( SoldPrice, Gender )
    # check the equality of variance for the t-test
    boolean <- var.test( as.numeric(SoldPrice) ~ as.factor(Gender), data = tmp, 
               alternative = "two.sided" )$p.value > alpha
    # conduct the t-test, return boolean, true stating that there's a 
    # difference between the two gender regarding the mean of amount of sold tickets 
    t.test( as.numeric(SoldPrice) ~ as.factor(Gender), data = tmp, 
            paired = FALSE, var.equal = boolean )$p.value < alpha    
})
```

```
## 0000010605 0000010440 0000010413 0000010439 0000010430 0000010619 
##       TRUE      FALSE      FALSE      FALSE       TRUE       TRUE 
## 0000010329 
##       TRUE
```
- **Note:** Based on the results of the t-test, it seems that for the 7 TicketCode that attributed to more than 10^7 ticket revenues, about half of them are impartial for the average amount of money spent on purchasing tickets between male and female. But that was the average per person, what about the total amount?


```r
# gender distribution
table(highdata$Gender)
```

```
## 
##     F     M 
## 17982  9772
```

```r
# total amount of money spent of tickets by gender
aggregate( as.numeric(SoldPrice) ~ TicketCode + Gender, data = highdata, FUN = sum ) %>%
arrange( TicketCode )
```

```
##    TicketCode Gender as.numeric(SoldPrice)
## 1  0000010329      F               3672000
## 2  0000010329      M               2479800
## 3  0000010413      F               6481200
## 4  0000010413      M               3896400
## 5  0000010430      F               5642000
## 6  0000010430      M               2555400
## 7  0000010439      F               6997000
## 8  0000010439      M               2590200
## 9  0000010440      F               9325285
## 10 0000010440      M               6489990
## 11 0000010605      F              11686100
## 12 0000010605      M               7557000
## 13 0000010619      F               5903200
## 14 0000010619      M               2442700
```
- **Note:** Wow! Despite the previous analysis told us that on average, there wasn't a clear bias toward which genders spends more on buying tickets, the consumers/users for this ticket system still seems to be slightly dominated by females. That is, tickets that were purchased by female were almost two times higher than that of male ( Inference from the table above ) and they account for more of the total ticket revenue ( Looking at the aggregated data above, the statement is true for the all 7 top-saling tickets ). 

#### 3. Age Distribution

So that was the discrepancy between the two genders, what about the age ? Which age level is the main target audience for these top-saling tickets. A histogram might be a good place to start.


```r
# add the age of the person using the BirthYear column
highdata[ , age := year(today()) - as.numeric(BirthYear) ]
```


```r
# extract one of the ticket concert and look at its age distribution 
agedata <- highdata %>% filter( TicketCode == high[1] ) %>% select( SoldPrice, Gender, age )

# age distribution histogram by gender
ggplot( agedata, aes( age, fill = Gender ) ) + geom_histogram() + facet_grid( ~ Gender ) + 
ggtitle("Number of Tickets Purchased For Different Age Levels and Genders")
```

![](system_files/figure-html/unnamed-chunk-8-1.png) 

- **Note1:** To avoid making the report too lengthy, the age distribution histogram is depicted for only one of the TicketCode, the first one from the `highdata` to be exact. All the other histogram looks quite similar to it, so we will use it to draw some hypotheses.
- **Note2:** From the histogram, we can boldy assume that the main audience segmentation for this ticket system is female consumers with the age of 30 ~ 40. Let's see if we can solidify that notion with the total revenue?  

Define 7 age levels for categorizing the age column, and add a new column cut to store that level in the `highdata`.


```r
# define age levels
breaks <- with( highdata, c( min(age), seq( 10, 60, 10 ) , max(age) ) )
highdata$cut <- cut( highdata$age, breaks = breaks, include.lowest = TRUE )
# age distribution 
table(highdata$cut)
```

```
## 
##  [5,10] (10,20] (20,30] (30,40] (40,50] (50,60] (60,81] 
##     359     543    3178   15567    5647    1936     524
```

```r
# the sum of sold price for every ticket, gender and age breaks
sum1 <- highdata[ , .( sum = sum( as.numeric(SoldPrice) ) ), 
                by = list( TicketCode, Gender, cut ) ] %>% arrange( TicketCode, cut )
# plot
ggplot( sum1, aes( Gender, cut, color = Gender, size = sum ) ) + 
geom_point( alpha = .8 ) + facet_grid( ~ TicketCode ) + 
scale_size_continuous( range = c( 5, 20 ) ) + 
labs( y = "Age Levels", 
      title = "Total Amount Spent on Tickets for each Age Level between Genders" )
```

![](system_files/figure-html/unnamed-chunk-9-1.png) 

- **Note1:** The table of the cut, and the plot confirms the fact the people of age 30 ~ 40 does in fact buy more tickets and lead to more revenues than other age levels, for clarity, the bigger the point in the plot, the higher the total amount of money was spent for that age level, matching our previous assumption.
- **Note2:** The biggest difference for the amount of money spent between male and female is seen for TicketCode 10605. After looking it up in the TicketName column, it was the concert of [Jay Chou](https://en.wikipedia.org/?title=Jay_Chou). 
- **Note3:** Another minor point, it is quite surprising that there were records of people that falls under the age category 5 ~ 10. This probably indicates the proportion of members of the ticket system that provided false age when registering. 


**Section Conclusion On Age and Gender Distribution:**

Although we do not know whether the person that bought the ticket is actually the one that went to the concert, Suggestions can still be made according to these findings. Next time the ticket system is selling tickets that are similar to these top-saling tickets, in other words, concerts held by similar singers or bands, conduct a A/B testing on the website user interface of the ticket system , test that whether changing the user interface to meet the taste of female consumers with the age of 30 ~ 40 will boost its ticket sales. Or the complete opposite strategy is to make it more appealing to males to elevate that bleak sales of theirs. As for which marketing strategies should this ticket system use, it will more likely depend on other market surveys to see the actual reasons and motivations that lead to their different behavorial patterns.

#### 4. Analysis on the ZipCode column
For this section, we will like look at things from a geographic perspective. For those people that have purchased the concert tickets online, where do they live? We assume that the zipcode should reveal some thoughts on that. 

- A quick review in case you've forgotten. `highdata` contains the TicketCode that generated more than 10^7 to the total revenue and only the records where the tickets were sold online (TicketSiteCode = 88888 ). 

- `zipcity` For this section, we will first add a new column to `highdata`. We'll extract the first digit of the [Zipcode](http://www.easytravel.com.tw/postid_search.asp) column, which represents the city and the rest are which district in the city, but here we're just going to look at the zipcode from the city-level. 
- `zipcodedata` Since the ZipCode column has some noise in them for reasons beyond knowledge (e.g. NA, English letters, random punctuation marks ), so for analyzing convenience, we'll simply convert them into numbers and exclude the ones that were coerced to NAs in the data type transformation process.
- The suppressWarnings function below is used just to avoid printing the warning messages in the report, it does not have any affect on the result.


```r
# convert the zipcity to numeric to elevaluate only the ones from 0-9
suppressWarnings( highdata$zipcity <- substring( highdata$ZipCode, 1, 1 ) %>% as.numeric() )

# exclude the NA rows from highdata
zipcodedata <- highdata[ complete.cases(highdata), ]

# have a glimpse at the contingency table bwteen zipcity and gender
gender_zip <- with( zipcodedata, table( Gender, zipcity ) )
addmargins(gender_zip)
```

```
##       zipcity
## Gender     0     1     2     3     4     5     6     7     8     9   Sum
##    F     425  6253  4851  2169  1838   387   230   477   953   175 17758
##    M     126  3158  2561  1378  1156   227   149   275   514   149  9693
##    Sum   551  9411  7412  3547  2994   614   379   752  1467   324 27451
```

- **Note1:** From the contingency table, we can see that the tickets were mainly mailed to address located in Taipei City ( zipcity = 1 ). Accounting for about thirty percent of the tickets that were purchased online.
- **Note2:** The city name that corresponds to each zipcode can be found in this [link](http://www.easytravel.com.tw/postid_search.asp). And in case you're wondering, 0 stands for tickets that were mailed to addresses that are overseas.

The next question after getting this contingency table is whether the frequency counts are distributed identically across these two different populations. In this case, the question can be translated into are there differences between the proportion of where the tickets were sent to between females and males. For that, we'll use the chi-square test of homogeneity.


```r
chisq.test(gender_zip)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  gender_zip
## X-squared = 105.76, df = 9, p-value < 2.2e-16
```

- **Note:** OK, the p-value is below .05, this provides strong evidence to suggest that when looking at the customers' behavior from a geographic viewpoint, men and women have different preferences towards concert tickets.

That was that, but we're still not finished yet. Let's investigate the contingecy table between each concert and zipcode. This time, instead of looking at proportions between cities, we will look at them from regions. We will combine some zipcodes together so that the data will reflect the tickets that were mailed to north, mid, south or east part of Taiwan. The detailed process for this part is listed below

- **Step 1:** `ticket_zip_df` Get the contingency table of the TicketCode and zipcity and convert it into data frame for further manipulation.
- **Step 2:** `geographic` Store the zipcode "cluster" into a list. Zipcode that begins with 1 and 2 are grouped as northern part of Taiwan. 3, 4, 5 as midland Taiwan. 6, 7, 8 as southern Taiwan. 9, 0 as eastern part of Taiwan. Grouping the tickets that were sent to oversea addresses is probably not a really cogent way, but let's just ignore that unfounded idea for now... 
- **Step 3:** `combined_ticket_zip` extract each zipcode "cluster" and add up the numbers.


```r
# Step 1
ticket_zip <- with( highdata, table( TicketCode, zipcity ) )
ticket_zip_df <- data.frame(ticket_zip) %>% spread( zipcity, Freq )

# Step 2
geographic <- list( c( "1","2" ), c( "3","4","5" ), c( "6","7","8" ), c( "9","0" ) )

# Step 3
# combine it by geographic
combine <- lapply( geographic, function(x)
{
    subset( ticket_zip_df, select = x ) %>%
        apply( 1, sum )
})

# also add the TicketCode column back
combined_ticket_zip <- cbind( ticket_zip_df$TicketCode, 
                              data.frame( do.call( cbind, combine ) ) )

# give descriptive names to each column
names(combined_ticket_zip) <- c( "TicketCode", "North", "Mid", "South", "East" )
combined_ticket_zip
```

```
##   TicketCode North  Mid South East
## 1 0000010329  1341  524   663   65
## 2 0000010413  2079  395   124   27
## 3 0000010430  2311  712   180   35
## 4 0000010439  2795 1131   339  220
## 5 0000010440  1915 2566   555   60
## 6 0000010605  4238 1322   518  359
## 7 0000010619  2144  505   219  109
```

- **Note:** Once again, you can do a chi-square test on it, but we don't really need a statstical method to see that there are differences in the proportion of tickets that were mailed to each region between these concerts. 

To get the actual numbers of proportion of tickets that were mailed to each region for each concert, we will use the prop.table function, but you probably have to convert the data frame back to a table to use it, and we will also use a mosaic plot to visualize this table. For those of you who have never heard of it, it is a plot that is widely used in many marketing departments. This chart is used to show proportion of a product market by region, and proportion of region by product.
- The function the created the mosaic plot is sourced in to prevent adding too many unneccessary information to the report. For those that are interested and wish to revise it for personal use, it's here at this [link](https://github.com/ethen8181/ticket-system/blob/master/system/mosaic_plot.R).


```r
# convert data frame into long format to get the table
longformat <- gather( combined_ticket_zip, "Region", "Freq", -1 )
longtable  <- xtabs( Freq ~ TicketCode + Region, data = longformat )

# use prop.table
prop.table( longtable, 1 )
```

```
##             Region
## TicketCode        North        Mid      South       East
##   0000010329 0.51716159 0.20208253 0.25568839 0.02506749
##   0000010413 0.79200000 0.15047619 0.04723810 0.01028571
##   0000010430 0.71371217 0.21988882 0.05558987 0.01080914
##   0000010439 0.62318841 0.25217391 0.07558528 0.04905240
##   0000010440 0.37578493 0.50353218 0.10890895 0.01177394
##   0000010605 0.65838123 0.20537517 0.08047227 0.05577132
##   0000010619 0.72018811 0.16963386 0.07356399 0.03661404
```

```r
# load the R file that contains the function that does the plotting
source("mosaic_plot.R")
mosaic_plot(combined_ticket_zip)
```

![](system_files/figure-html/unnamed-chunk-13-1.png) 

- **Note1:** From the proportion table and the mosaic plot we can see that most of the tickets were sent to northern part of Taiwan ( if you only look at these 7 top-saling concerts ), but with one exception. For TicketCode 0000010440 [Turandot](https://en.wikipedia.org/?title=Turandot), 50 percent of the tickets were sent to midland Taiwan, was this because only this concert was held there or is it really because, geographically, the major audience segmentation for this concert was in midland Taiwan ?
- **Note2:** Charging the people living in the northern region of Taiwan with more money for mailing the tickets will lead to higher revenue!~~~ Well, if they don't start filing customer complaints.


**Section Conclusion on ZipCode:** 

After seeing the mosaic plot from above, it has stricked upon us that people living in different regions of Taiwan probably have distinct preferences towards the types of concerts, and recall from the previous section that the major users of the ticket system website are females with the age between 30 ~ 50. As a digital marketer, you probably have work to do. Let's take the [Turandot](https://en.wikipedia.org/?title=Turandot) concert for example. 
We can reconfirm the major target audience for this concert again by calling out the gender and age table just for this concert. 


```r
tabledata <- zipcodedata %>% filter( TicketCode == "0000010440" ) %>% select( Gender, cut )
table(tabledata)
```

```
##       cut
## Gender [5,10] (10,20] (20,30] (30,40] (40,50] (50,60] (60,81]
##      F     35      48     243    1296     869     423     104
##      M     38      32     100     866     615     282     145
```

So, next time a similar concert is about to be sold ( or selling ), digital marketers should place an higher bid for online ad campaigns that targets customers that lives in the middle region of Taiwan and are aged between 30 ~ 50. As for what types of conert is defined as a similar concert to this one, this dataset probably does not suffice for answering this question.

#### 5. Analyze TicketSiteCode

In the last part of the exploratory analysis, we wish to observe the total revenue generated by each TicketSite, also how many TicketSite contributes to the majority of the revenue.

- `topdata` Top-saling concert data ( total sales larger than 10^7 ), including all the TicketSite.
- `site` Total revenue generated by each ticketsite.

```r
topdata <- data %>% filter( TicketCode %in% high )
site <- topdata[ , .( sum = sum( as.numeric(SoldPrice) ) ), by = TicketSiteCode ] %>% 
        arrange( desc(sum) )
site
```

```
##      TicketSiteCode      sum
##   1:          88888 77718275
##   2:          00715  7217800
##   3:          00589  2631600
##   4:          00248  2077280
##   5:          00206  1901935
##  ---                        
## 350:          00654     1800
## 351:          00682     1200
## 352:          05405     1200
## 353:          00563     1200
## 354:          00039      800
```

```r
# percentage of the TicketSiteCode that generated 70 and 80% of the total revenue.
sapply( c( .7, .8 ), function(x)
{   
    mean( !cumsum( site$sum / sum(site$sum) ) > x ) * 100   
})
```

```
## [1] 0.5649718 4.5197740
```

- **Note1:** TicketSiteCode 88888 ( the internet ) accounts for 62.64 percent of the ticket system's total revenue, well it is after all an online ticket system so no surprise there. As for the TicketSite that had extremely inferior amount of revenues, maybe it is time to shut them down.
- **Note2:** The long tail theory where a small proportion of the products generates large proportion of the revenues also holds for this ticket system. Where 0.5 percent of the ticketsite contributes to 70 percent of the sales and 4.5 percent contributes to 80.

## IV. Time Series Analysis

For this section we will continue using subset data `topdata`, which includes the ticket selling record of the 7 top-saling tickets in all the TicketSite. In the beginning of the report, it was said that the SoldDate and SoldTime column happens to be messy and requires tidying before conducting the analysis. The steps for doing that is described in the following.

- **Data Preprocessing**
- **Step1:** `string` Paste the two column together and use regular expression to exclude unneccesary information. To be exact, we only need the Year, Month, Day from the SoldDate column and Hour, Minute, Second from the SoldTime column. 
- **Step2:** `SoldDate` Convert the tidied string to time and override the original SoldDate column. Also exclude the SoldTime column.
- **Step3:** `topdata` Order it by time, and exclude the SoldPrice that are 0 and 10 ( those are tickets given out for free ) from the data.
- **Step4:** `pdata` Extract each ticket type and assign numbers to record the number of tickets left for that concert ( Assuming that tickets to each concert was sold out ). Then normalize it, so that the tickets sold-out rate can be use to compare with different concerts.


```r
# Step 1
string <- gsub( "(.*)\\s.*\\s(.*)\\.[0]{3}", "\\1 \\2", 
                with( topdata, paste( SoldDate, SoldTime, sep = "" ) ) ) 
# Step 2
topdata$SoldDate <- ymd_hms(string)
topdata$SoldTime <- NULL

# Step 3
topdata <- topdata[ order(topdata$SoldDate), ] %>% 
               filter( !topdata$SoldPrice %in% c( 0, 10 ) )
# Step 4
process <- lapply( unique(topdata$TicketCode), function(x)
{
    # extract each unique data
    boolean <- topdata$TicketCode == x
    # exclude the free given ticket
    subdata <- topdata[ boolean, ] 
    # normalization : (x-min)/(max-min), times 100 to express it in percentage
    subdata$count <- ( nrow(subdata):1-1 ) / ( nrow(subdata)-1 ) * 100
    return(subdata)
})    
pdata <- do.call( rbind, process )

# plot
ggplot( pdata, aes( SoldDate, count, color = TicketCode ) ) + geom_line( size = 1 ) + 
labs( title = "Tickets Sold-Out Rate", y = "Percentage of Tickets Left", x = "Date" )
```

![](system_files/figure-html/unnamed-chunk-16-1.png) 


**Section Conclusion:** Judging from the plot, we can categorize the sold-out rate of the tickets into three major categories. As for clustering them using the "datamining" way, it seems like Dynamic Time Warp and Functional Data Analysis both do not work well for this dataset due to missing values and inconsistency in the timespan. No expert here, so perhaps I'm wrong. And as mentioned before in this report, if you really want to cluster the ticket concert into different types, the sold-out rate shouldn't be the only attribute that you want to look at.

- **Category 1:** There were no steep downturn in the sold-out rate, the tickets were gradually purchased by the consumers. This applies for TicketCode 0000010329, the concert held by [A-mei](https://en.wikipedia.org/wiki/A-mei). 
- **Category 2:** In the beginning, there was a rapid plummet, then the intercept of the slope starts to decrease. TicketCode 0000010413 [Andrea Bocelli](https://en.wikipedia.org/wiki/Andrea_Bocelli), 0000010430 [Harlem Yu](https://en.wikipedia.org/wiki/Harlem_Yu), 0000010439 [Show Luo](https://en.wikipedia.org/wiki/Show_Luo) and 0000010440 [Turandot](https://en.wikipedia.org/?title=Turandot) falls into this category.
- **Category 3:** The tickets were sold out in a short period of time. Such as TicketCode 0000010605 [Jay Chou](https://en.wikipedia.org/?title=Jay_Chou) and 0000010619 [Sammi Cheng](https://en.wikipedia.org/?title=Sammi_Cheng).

## V. Session Information 


```r
sessionInfo()
```

```
## R version 3.2.2 (2015-08-14)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.5 (Yosemite)
## 
## locale:
## [1] C/C/C/C/C/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] reshape2_1.4.1     RColorBrewer_1.1-2 data.table_1.9.6  
## [4] lubridate_1.3.3    ggplot2_1.0.1      dplyr_0.4.3       
## [7] tidyr_0.3.1       
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.2      knitr_1.11       magrittr_1.5     MASS_7.3-43     
##  [5] munsell_0.4.2    colorspace_1.2-6 R6_2.1.1         stringr_1.0.0   
##  [9] plyr_1.8.3       tools_3.2.2      parallel_3.2.2   grid_3.2.2      
## [13] gtable_0.1.2     DBI_0.3.1        htmltools_0.2.6  lazyeval_0.1.10 
## [17] yaml_2.1.13      assertthat_0.1   digest_0.6.8     formatR_1.2.1   
## [21] memoise_0.2.1    evaluate_0.8     rmarkdown_0.8    labeling_0.3    
## [25] stringi_1.0-1    scales_0.3.0     chron_2.3-47     proto_0.3-10
```


