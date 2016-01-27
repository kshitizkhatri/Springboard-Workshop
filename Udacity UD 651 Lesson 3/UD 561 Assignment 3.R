## Udacity Problem Set 3 UD651

## WE will start with loading dataset 'diamonds' in R.

data (diamonds)

## To get all the basic details about the rows and columns of the data set, we will use the str () function

str (diamonds)

help (diamonds)

## The step used above was to interpret all the variables correctly

PriceHistogram <- qplot (x = price, data = subset (diamonds , !is.na (price)), binwidth = 25)

PriceHistogram

## I will save the plot created in this exercise by the following command

ggsave ('PriceHistogram.png')

summary (price)

## Now I am creating three variables to tell the number of the diamonds for a given price band.

lessthan500 <- diamonds$price < 500
sum (lessthan500)

lessthan250 <- diamonds$price < 250
sum (lessthan250)

morethanOrEqual <- diamonds$price >= 15000
sum (morethanOrEqual)

## For exploring the highest peak in the histogram, We can manually check which range in x axis has the highest point on the graph.
## I have roughly chosen a final point for the range on the x axis on which I will narrow down the histogram.
## Below is the code for narrowing down the x axis to observe the peak in the histogram
qplot (x = price, data = subset (diamonds , !is.na (price)), binwidth = 1)+
scale_x_continuous (limits = c (1,1500), breaks = seq (0,1500,10) )

## Now I will split the price histogram into 5 histograms on the same panel as per the cut of the diamonds
PHasPerCut <- PriceHistogram + facet_wrap (~cut, ncol = 2)
PHasPerCut
ggsave ('PHasPerCut.png')

## For checkin out the statistics for price as per the different cuts of diamonds I will use the by () function
by (diamonds$price, diamonds$cut, summary)

## Since the summary function rounds off the values, we can pass max and min as the arguments in the by () function
by (diamonds$price, diamonds$cut, max)
by (diamonds$price, diamonds$cut, min)

## We will now introduce a new argument to the facet_wrap () function to check for the 
## disparity in the graphs and the data got through the by () function.

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales= "free_y")
ggsave ('PriceCutFreeY.png')

## Next exercise is to plot the histogram for price per carat, but to visualise the data in a better way.
## I have introduced a new variable to the data set PricePerCarat so as to make it a little easier
diamonds$PricePerCarat <- diamonds$price/diamonds$carat

## After multiple trials, I have come across the following code for a proper binwidth as well the x scale to get a better visualization for the data
qplot(x = PricePerCarat, data = diamonds, binwidth = .01) + facet_wrap(~cut, scales = "free_y")+
scale_x_log10 ()
ggsave ('PPCforCut.png')

## Below is the analysis of price for different cuts of diamonds through the boxplots and the number summaries
qplot (x = cut, y= price, data = subset (diamonds , !is.na (price)), geom = 'boxplot') + coord_cartesian (ylim = c (0,7500))
ggsave ('PriceCutBoxPlot.png')
by (diamonds$price, diamonds$cut, summary)

## Below is the analysis of price for clarity of diamonds through the boxplots and the number summaries
qplot (x = clarity, y= price, data = subset (diamonds , !is.na (price)), geom = 'boxplot') + coord_cartesian (ylim = c (0,7500))
ggsave ('PriceClarityBoxPlot.png')
by (diamonds$price, diamonds$clarity, summary)

## Below is the analysis of price for different colors of diamonds through the boxplots and the number summaries
qplot (x = color, y= price, data = subset (diamonds , !is.na (price)), geom = 'boxplot') + coord_cartesian (ylim = c (0,7500))
ggsave ('PriceColorBoxPlot.png')
by (diamonds$price, diamonds$color, summary)

## For determining the interquartile ranges of the best and the worst colors respectively
IQR (subset (diamonds, color == "D")$price)

IQR (subset (diamonds, color == "J")$price)

## Price per carat analysis for different colors of diamonds using boxplot
qplot (x = color, y= PricePerCarat, data = subset (diamonds , !is.na (price)), geom = 'boxplot') + coord_cartesian (ylim = c (0,8000))
ggsave ('PriceColorBoxPlot.png')
by (diamonds$PricePerCarat, diamonds$color, summary)

## Now analysing the weight of the diamonds using the frequency polygon
qplot (x = carat, data = subset (diamonds , !is.na (carat)), binwidth = 0.0001, geom = 'freqpoly',color = carat )
  +     coord_cartesian(ylim = c (0,3000))
## In the above code if we reduce the binwidth further, we may realise that there are only two weight categories for count more than 2000, 3 and 3.1.

## Gapminder Data

hiv_indicator <- read.table('HIV Data.csv', sep=',', header=TRUE)
hiv_indicator_clean <- gather (hiv_indicator, "year", "Percentage",2:34)
colnames (hiv_indicator_clean1) [1] <- "Countries"
hiv_indicator_clean1 <- na.omit (hiv_indicator_clean)
## Saving the cleaned data
write.table (hiv_indicator_clean1,"hiv_indicator_clean.txt",sep = ",")

## Making plots for exploration
library (ggplot2)
qplot (x = Percentage, data = subset (hiv_indicator_clean1,binwidth = .01))+facet_wrap (~year)
ggsave ('PercentageCount.png')
qplot (x = year,y= Percentage, data = subset (hiv_indicator_clean1,binwidth = .001))+ facet_wrap (~Countries)
ggsave ('PercentageVsYear.png')
## Above analysis gives a basic comparison among all the countries whether the percentage of HIV cases
## have increased or decreased or kept constant on a relatively basis.

## Final Exercise of this assignment

FriendBirthdays <- read.csv ("c:/Users/Kshitiz Khatri/Desktop/Springboard/birthdaysExample.csv")
install.packages ("lubridate")
library (lubridate)
FormattedDates <- as.Date (FriendBirthdays$dates, format = "%m/%d/%y")
days <- day (FormattedDates)
weekday <- wday (FormattedDates)
Month <- month (FormattedDates)
Birthdays_new <- data.frame (FormattedDates,days, weekday, Month)

## For answering all questions but the last
table (Birthdays_new$days)
table (Birthdays_new$weekday)
table (Birthdays_new$Month)
