## Problem set 5

library (dplyr)
library (ggplot2)

data (diamonds)

ggplot (aes (x = diamonds$price), data = diamonds) + geom_histogram (aes (color = cut)) + facet_wrap(~color)

diamond_price_facetting <-  ggplot (aes (x = diamonds$price), data = diamonds) + geom_histogram (aes (color = cut)) + facet_wrap(~color) + scale_fill_brewer (type = 'qual')
diamond_price_facetting 

diamond_price_table <-  ggplot (aes (x = diamonds$table,y = diamonds$price), data = diamonds) + geom_point (aes (color = cut)) + scale_fill_brewer (type = 'qual')
diamond_price_table
ggsave ('diamonds_price_table.png')

## Scatter plot of diamond price vs volume with other specifications
diamonds$volume <- diamonds$x*diamonds$y*diamonds$z
PriceVsVolume <- ggplot (aes (x = volume, y = price), data = subset(diamonds, volume<=quantile (diamonds$volume, 0.99))) + scale_y_log10 ()+ geom_point (aes (color = clarity)) 
PriceVsVolume

## Proportion of friends initiated
pf$prop_initiated <- pf$friendships_initiated/pf$friend_count

median_prop_initiatedVsTenure <- ggplot (aes (x = tenure, y = prop_initiated), data = subset(pf,!is.na (prop_initiated))) + geom_line (aes (color = year_joined.bucket), stat ='summary', fun.y = median)
median_prop_initiatedVsTenure

## Smoothing the curve
median_prop_initiatedVsTenure + stat_smooth ()

## Plotting price/carat vs cut for diamonds data set
price_per_carat <- mutate (diamonds, ratio = price/carat)
ggplot (aes (x = ratio), data = price_per_carat) + geom_point (aes (color = color)) + facet_wrap (~clarity)

## I haven't used any techniques further for my previously used HIV_data but I have started plotting the data individually for selective countries.
## I came across some interesting facts like there rise in the percentage of population between 15-45 was mostly seen in the African Countries.
## I observed that clubbing countries geographically would have made more sense.
## Also, had population parameter been there, it would have got lot interesting to compare absolute numbers as well.

## For example, in 2011, USA and UK had more number of HIV affected people in the given age group than many of the African Countries
## South Africa has seen highest rise in the number of HIV affected people. But to think, South Africa is one of the most flourished country of African continent,
## So, it could be a case that, most of the good treatment facilities for this disease might be well established in South Africa, hence a significant rise in the number of HIV affected people (probably the ones who have
## migrated for treatment.
