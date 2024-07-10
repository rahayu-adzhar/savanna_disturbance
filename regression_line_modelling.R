library(this.path)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Metrics)
library(tidyverse)


rm(list=ls())

setwd(this.path::here())
fw24<- read.csv("tcc_tch_15_23.csv")

fw24$cat <- as.character(fw24$strata)




h_check <- fw24[c('TCH15', 'TCH16', 'TCH17', 'TCH18', 'TCH19', 'TCH20', 'TCH21', 'TCH22', 'TCH23')]

c_check <- fw24[c('TCC15', 'TCC16', 'TCC17', 'TCC18', 'TCC19', 'TCC20', 'TCC21', 'TCC22', 'TCC23')]


## https://www.statology.org/r-check-if-multiple-columns-are-equal/
##checking if we r equal across the board

h_check <- h_check %>%
  rowwise %>%
  mutate(match_h = as.numeric(n_distinct(unlist(cur_data())) == 1)) %>%
  ungroup()

c_check <- c_check %>%
  rowwise %>%
  mutate(match_c = as.numeric(n_distinct(unlist(cur_data())) == 1)) %>%
  ungroup()


h_check$id <- c(1:89)
c_check$id <- c(1:89)



hc_check <- merge(h_check, c_check, by = "id")

unchanged <- hc_check %>% filter(match_h == 0 | match_c == 0)


#we're gonna plot the path of a girlie (phase diagrams)

name <-  c(unchanged$id)
row <- 1
girlies <- list()

for(i in 1:nrow(unchanged)){{
  plot <- unchanged[row,]
  df <- c()
  df$id <- name[[i]]
  df$tcc <- as.integer(plot[12:20])
  df$tch <- as.integer(plot[2:10])
  df$year <- c(15:23)
  df <- as.data.frame(df)
  
  girlies[[i]] <- df}
  row <- row + 1
  }




### aaaaa baby u don't gotta recreate the graph ur recreating the concept of the shape
#what we want to tell the line
#ur y value will be steady until u hit x value (t0)
#where t0 is degradation start point

#eq 2 is the drop between t0 and t1 (t1 being the start of the recovery curve)

##eq 3 is then the recovery curve

##so if t < t0, use eq1
##else if t>t0 but t<t1, use eq2
##else use eq3

##assumption that degradation drop happens in the middle
##assumption that recovery goes back up to initial h levels
# test_girl <- girlies[[3]]
# low <- min(test_girl$tcc)
# t_rise <-(test_girl[test_girl$tcc == low,])$year
# t_drop <- t_rise - 1
# c_start <- test_girl$tcc[[1]]
# c_end <- test_girl$tcc[[9]]
# c_drop <- (test_girl[test_girl$year == t_drop,])$tcc
# c_rise <- (test_girl[test_girl$year == t_rise,])$tcc
# tau <- 2 #testing value when we r running u like crazy this can b adjusted i think



#t_drop (year when the decline happened, assumed to be 1 year before the lowest point)


## ok the model is meant to draw a girlie given a set of parameters (NOT FULL ON VALUES)
##

# for (i in 1:nrow(test_girl)){
#   year <- test_girl$year[[i]]
# if (year <= t_drop) { 
#   out <- c_start
# } else if (year > t_drop && year < t_rise ){
#   a = c(c_rise : c_drop)
#   out = sample(a,1)
# } else{
#   out = (c_end - c_rise)*(1 - (exp(-tau*(year - t_rise)))) + c_rise
# } #i should be t minue t0, represent going from c-drop to c-end 
#   #browser() #runs u thru loop by loop if u keep clicking c so u cn check girlies
#   test_girl$out[[i]] <- out
# }

##she works beautifully now but loops are Not efficient
##so try to make her a function
# > yay=1:10
# > yay <5
# [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
# > yay[yay <5]
# [1] 1 2 3 4
# > yay[yay <5] =8
# > yay
# [1]  8  8  8  8  5  6  7  8  9 10

#if cukup syarat do this, if not don't
test_girl <- girlies[[3]]
# low <- min(test_girl$tcc)
# t_rise <-(test_girl[test_girl$tcc == low,])$year
# t_drop <- t_rise - 1
# c_start <- test_girl$tcc[[1]]
# c_end <- test_girl$tcc[[9]]
# c_drop <- (test_girl[test_girl$year == t_drop,])$tcc
# c_rise <- (test_girl[test_girl$year == t_rise,])$tc



all_curve <- function(time, site){
  #u want to optimise girlies u dont know v well
#to optimise is to find the most likely value
 # low <- min(site$tcc)
  t_rise <- (site[site$tcc == low,])$year #opt
  t_drop <- t_rise - 1 #opt
  c_start <- site$tcc[[1]] #needs to be optimised bc doesn't account for fluctuations etc et  al for the rest of em prior tot he drop
  c_end <- site$tcc[[9]] #opt
  c_drop <- (site[site$year == t_drop,])$tcc #opt
  c_rise <- (site[site$year == t_rise,])$tcc #opt
  #if ur dependent on other guys dont get optimised outside
  drop_m <- ((c_rise - c_drop)/(t_rise - t_drop))
  drop_w <- c_drop - drop_m*t_drop #c of y = mx + c
  tau <- 2 #needs optimising

  out <- rep(c_start, length(time))
  out[time<= t_drop] = c_start
  out[time>t_drop && year<t_rise] =  drop_m * time + drop_w
  out[time == t_rise] = c_rise
  out[time>t_rise] = (c_end - c_rise)*(1 - (exp(-tau*(time[time>t_rise] - t_rise)))) + c_rise
return(out) #makes her global
  }

print(all_curve(time = 15:23, site = test_girl))

##debugging
##options(error=recover)
##she'll show u a list of how she proceeded and where she got stuck
##


##function can take more than one input
##yay = 1
#whoo <- function(input){}
##whoo can call upon anyone outside of her function + whatever u designate as input
##outside of function girlies are called global variables (anyone can call them for anything)
##input is local, u can only call em w/in the function
##for optimisation, parameters need to be made local