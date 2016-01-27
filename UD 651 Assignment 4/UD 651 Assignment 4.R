## Load the diamonds data set and make a scatter plot for price vs x for the diamonds data set

library (ggplot2)
library (dplyr)

data ("diamonds")
plot1 <- ggplot (aes (x = diamonds$x, y = diamonds$price), data = diamonds) + geom_point ()
plot1

## Correlation between price and x, y and z respectively.
x_and_price_cor <- cor.test( diamonds$price, diamonds$x)
y_and_price_cor <- cor.test( diamonds$price, diamonds$y)
z_and_price_cor <- cor.test( diamonds$price, diamonds$z)
x_and_price_cor
y_and_price_cor
z_and_price_cor

## Scatter Plot for Price Vs Depth
PriceVsDepth <- ggplot (aes (x = diamonds$depth, y = diamonds$price), data = diamonds) + geom_point ()
PriceVsDepth
ggsave ('PriceVsDepth.png')

# Editing a code for making the transparency of the code to be 1/100 and mark the x axis every 2 units
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) + scale_x_continuous (breaks = seq (0,80,2))
 
## Correlation between depth and price
cor_depth_and_price <- cor.test (diamonds$depth, diamonds$price)
cor_depth_and_price

## Scatter plot of price vs carat, without top 1 % price and carat values
priceVsCarat_less_top_one_percent <- ggplot(data = diamonds, aes(x = carat, y = price)) + 
  +     xlim (0, quantile (diamonds$carat, 0.99)) + ylim (0, quantile (diamonds$price,0.99)) + geom_point ()
priceVsCarat_less_top_one_percent

## Introducing new variable, volume and plotting price vs volume
diamonds$volume <- diamonds$x*diamonds$y*diamonds$z
PriceVsVolume <- ggplot (aes (x = volume, y = price), data = diamonds) + geom_point ()
PriceVsVolume
ggsave ('PriceVsVolume.png')

## Correlation of diamonds with 0<volume<800
volume_bounded <- subset(diamonds, 0<volume & volume< 800)
cor.test (volume_bounded$volume, volume_bounded$price)

## Price vs volume with the bounds done in the above exercise and fitting a liner model to it
PriceVsVolume_volume_bounded <- ggplot (aes (x = volume, y = price), data = volume_bounded) + geom_point (alpha = 1/100)
PriceVsVolume_volume_bounded
PriceVsVolume_volume_bounded + stat_smooth (formula = y~x, size = 1)

## Data frame diamondsByClarity
grouped_by_clarity <- group_by (diamonds, clarity)
diamondsByClarity <- summarize (grouped_by_clarity, mean_price = mean (price), median_price = median (price), min_price = min (price), max_price = max (price), n = n ())
View (diamondsByClarity)

## Diamonds by color and diamonds by clarity on same plot
mean_price_clarity <- ggplot (aes (x = mean_price), data = diamonds_mp_by_clarity, binwidth = 50) + geom_bar ()
mean_price_color <- ggplot (aes (x = mean_price), data = diamonds_mp_by_color, binwidth = 50) + geom_bar ()
library (gridExtra)
arranged_in_grid <- grid.arrange (mean_price_color, mean_price_clarity)
arranged_in_grid
ggsave ('mean_price_by_clarity_color.png')

## Final exercise of picking up a data and explore further
## I am going to use the data set used at the end of the third assignment.
## Since I have already cleaned the data in the last assignment, I will load the same data for this one too
HIV_data <- read.table ("c:/Users/Kshitiz Khatri/Desktop/hiv_indicator_clean.txt", header = TRUE, sep = ",")
colnames (HIV_data) [1] <- "Countries"
colnames (HIV_data) [3] <- "Percentage"
qplot (x = year,y= Percentage, data = subset (HIV_data,HIV_data$Countries == "Colombia",binwidth = .001))
ggsave ('colombia.png')
qplot (x = year,y= Percentage, data = subset (HIV_data,HIV_data$Countries == "South Africa",binwidth = .001)) + geom_smooth ()
ggsave ('South_africa.png')

## Before running the code mentioned above I have looked the graphs made in the previous assignment
## Basic observation was that most of the countries which has seen rise in the HIV percentage belonged to African continent.
## With single graph it was not possible to check for the scale of y axes for all the countries, so I have manually picked two countries with rise in HIV cases and plotted them.
## Turns out scales for y axis, that is percentage is quite different.
## Now I will group the data by countries and check for the mean_percentage vs countries relation
HIV_data_grouped <- group_by (HIV_data, Countries)
HIV_grouped_summarized <- summarize (HIV_data_grouped, mean_percentage = mean (Percentage), median_percentage = median (Percentage), n = n ())

ggplot (aes(x = Countries, y = mean_percentage), data = HIV_grouped_summarized) + geom_point()
ggsave ('mean_percentageVsCountries.png')

ggplot (aes(x = Countries, y = median_percentage), data = HIV_grouped_summarized) + geom_point()
ggsave ('median_percentageVsCountries.png')


## Most of the above analysis suggests that, most of the countries has their mean HIV count over the years less than 3 %.
## There were many outliers as well, which were quite high. Going upto 20 as well.
## Basic observation shows that most of the countries with higher HIV percentage were from African continents and had increasing trend till mid period of the timeline.