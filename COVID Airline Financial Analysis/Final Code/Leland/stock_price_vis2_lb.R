# ---
# title: United airlines monthly plot of cancelled arrivals with stock price
# author: Leland Bolleter
# date: 11/11/2022
# description: plot Sum.of.arr_cancelled_div20 as a bar graph and Open.Stock.Price as a trend line
#    data is for United Airlines for 8/2017 to 8/2022. 
# ---

setwd("/Users/leland15/6203_git/Team-13/Data/Leland")
library(dplyr)
library(tidyverse)
library(ggplot2)

# read in UAL data file and create a subset with values for visual(s)
ual_data <- read.csv("united_lbc.csv") 
ual_vis1_data <- select(ual_data, yyyymm, Open.Stock.Price, Sum.of.arr_cancelled)

# scale Sum.of.arr_cancelled with divide by 20 because dates 202003 and 202004 have outsized 
#     cancellations that obstruct viewing of other data
ual_vis1_data_div20 <- ual_vis1_data %>% mutate(Sum.of.arr_cancelled = Sum.of.arr_cancelled/20)

# rename Sum.of.arr_cancelled to Sum.of.arr_cancelled_div20
names(ual_vis1_data_div20)[3] = "Sum.of.arr_cancelled_div20"

# add a factor field for yyyymm for ggplot work and rename the field to date_factor_yyymm
ual_vis1_data_div20 <- ual_vis1_data_div20 %>% mutate(as.factor(ual_vis1_data_div20$yyyymm))
names(ual_vis1_data_div20)[4] = "date_factor_yyyymm"

# remove the original yyyymm column
ual_vis1_data_div20 <- ual_vis1_data_div20[-1]

# plot Sum.of.arr_cancelled_div20 as a bar graph and Open.Stock.Price as a trend line
ggplot(ual_vis1_data_div20) +
  geom_col(aes(x = date_factor_yyyymm, y = Sum.of.arr_cancelled_div20), color = "darkblue", 
           size = 1, fill = "white") +
  geom_line(aes(x = date_factor_yyyymm, y = Open.Stock.Price, color = "red"), size = 1, group = 1) +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  labs(color = "Stock Price")



