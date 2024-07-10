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

changed <- hc_check %>% filter(match_h == 0 | match_c == 0)


#make a list of all the 

name <-  c(changed$id)
row <- 1
site_list <- list()

for(i in 1:nrow(changed)){{
  plot <- changed[row,]
  df <- c()
  df$id <- name[[i]]
  df$tcc <- as.integer(plot[12:20])
  df$tch <- as.integer(plot[2:10])
  df$year <- c(15:23)
  df <- as.data.frame(df)
  
  site_list[[i]] <- df}
  row <- row + 1
  }

#model accounts for every step to describe a disturbance/recovery curve
#w assumption that all iterations of savanna behaviour will fall somewhere on it

#4 potential stages (and they aren't necessarily in this order i.e. don't always start w first listed stage)
# pre-event : degradation has not occurred
# event: sharp decline facilitated by degradation
# event_floor: what it's like at the bottom
# recovery: recovering from the event


test_site <- site_list[[3]]
##test_site is selected bc she does possess all 4 stages

#parameters to be optimised
tau <- 2
t_rise <- (test_site[test_site$tcc == min(test_site$tcc),])$year #opt
t_drop <- t_rise - 1 #opt 
c_start <- test_site$tcc[[1]] #needs to be optimised bc doesn't account for fluctuations etc et  al for the rest of em prior tot he drop
c_end <- test_site$tcc[[9]] #opt
c_drop <- (test_site[test_site$year == t_drop,])$tcc #opt
c_rise <- (test_site[test_site$year == t_rise,])$tcc #opt


all_curve <- function(time, site, tau, t_rise, t_drop, c_start, c_end, c_drop, c_rise){
  #u want to optimise girlies u dont know v well
#to optimise is to find the most likely value

  #if ur dependent on other guys dont get optimised outside
  drop_m <- ((c_rise - c_drop)/(t_rise - t_drop))
  drop_w <- c_drop - drop_m*t_drop #c of y = mx + c


  out <- rep(c_start, length(time))
  out[time<= t_drop] = c_start
  out[time>t_drop && time<t_rise] =  drop_m * time + drop_w
  out[time == t_rise] = c_rise
  out[time>t_rise] = (c_end - c_rise)*(1 - (exp(-tau*(time[time>t_rise] - t_rise)))) + c_rise
return(out) #makes her global
  }

#print(all_curve(time = 15:23, site = test_girl))
france <- test_site$tcc

new_fit <- nls( france ~ all_curve(time = 15:23, site = test_site, tau, t_rise, t_drop, c_start, c_end, c_drop, c_rise), start = list(
  tau = tau, t_rise = t_rise, t_drop = t_drop, c_start = c_start, c_end = c_end, c_drop = c_drop, c_rise = c_rise
))

##debugging
##options(error=recover)
##she'll show u a list of how she proceeded and where she got stuck

##for optimisation, parameters need to be made local




test_curve <- function(time, site, tau, t_rise, t_drop, c_start, c_end, c_drop, c_rise){
  #u want to optimise girlies u dont know v well
  #to optimise is to find the most likely value
  
  #if ur dependent on other guys dont get optimised outside
  drop_m <- ((c_rise - c_drop)/(t_rise - t_drop))
  drop_w <- c_drop - drop_m*t_drop #c of y = mx + c
  
  
  out <- rep(c_start, length(time))
  out[time<= t_drop] = c_start
  out[time>t_drop & time<t_rise] =  drop_m * time[time>t_drop & time<t_rise] + drop_w
  out[time == t_rise] = c_rise
  out[time>t_rise] = (c_end - c_rise)*(1 - (exp(-tau*(time[time>t_rise] - t_rise)))) + c_rise
  return(out) #makes her global
}

t1 <- print(test_curve(time = 15:23, site = test_site, tau = 3, t_rise = 22, t_drop = 19, c_start = 88, c_end = 83, c_drop = 82, c_rise = 30 ))
t2 <- print(test_curve(time = 15:23, site = test_site, tau = 8, t_rise = 20, t_drop = 19, c_start = 78, c_end = 83, c_drop = 82, c_rise = 15 ))
t3 <- print(test_curve(time = 15:23, site = test_site, tau = 0.1, t_rise = 20, t_drop = 17, c_start = 78, c_end = 83, c_drop = 82, c_rise = 15 ))
t3.5 <- print(test_curve(time = 15:23, site = test_site, tau = 0.1, t_rise = 18, t_drop = 17, c_start = 78, c_end = 83, c_drop = 82, c_rise = 15 ))

france <- test_site$tcc

new_fit <- nls( france ~ test_curve(time = 15:23, site = test_site, tau, t_rise, t_drop, c_start, c_end, c_drop, c_rise), start = list(
  tau = tau, t_rise = t_rise, t_drop = t_drop, c_start = c_start, c_end = c_end, c_drop = c_drop, c_rise = c_rise
))

#something cocked up in the event decreasing line equation (fixed)
#during optimisation t drop can happen after t rise and we need to Stop That