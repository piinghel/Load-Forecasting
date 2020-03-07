# clean environment
rm(list=ls())


# Data manipulation libraries
library(plyr) 
library(dplyr)
library(tidyverse)
library(feasts)
library(tsibble)
library(reshape2)


# Data visualization
library(plotly)
library(feasts)
library(gridExtra)
library(wesanderson)
library(forecast)
library(GGally)
library(patchwork)
# other


library(tictoc) # measures the time of the script

# own functions
source("Functions/save_output.R") 
source("Functions/create_features.R")
source("Functions/split_data.R")
source("Functions/save_output.R")




# Global parameters ---------------------------------------

# colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

THEME <- theme_minimal()
LEGEND <- theme(legend.title = element_blank())

# read data
tic("EDA") # start timing

df <- readRDS('Data/Fifthplay/CleanedData/finalTotC.Rda') %>% 
  as_tsibble(index=Date) %>%
  # rename variable
  dplyr::rename(
    total_cs = TotalCs_imp,
    date = Date
  ) %>%
  split_data(df = ., 
             train_perc = .6,
             validation_perc = .2, embargo=0) %>% 
  # create lags
  lapply(.,create_lags, var = "total_cs",
         lags = seq(from = 1, to = 672, by = 1)) %>%
  list2env(envir = .GlobalEnv)


# how to encode time based features
# https://ianlondon.github.io/blog/encoding-cyclical-features-24hour-time/

train_features <- train %>%
  mutate(
    date_simple = lubridate::date(date),
    year = as.factor(lubridate::year(date)),
    month = as.factor(lubridate::month(date, label=FALSE)),
    week = as.factor(lubridate::week(date)),
    week_day = as.factor(lubridate::wday(date, label = TRUE)),
    day_nr = as.factor(lubridate::yday(date)),
    hour = lubridate::hour(date),
    minute = lubridate::minute(date),
    quarter = lubridate::quarter(date),
    day_type = as.factor(ifelse(week_day %in% c("za", "zo"), 
                                "Sat-Sun","Mon-Fri")),
    working_hour = as.factor(ifelse(hour %in% c(7,8,9,10,11,12,13,
                             14,15,16,17,18,19),"7h-19h","20h-6h")),
    hour = as.factor(hour),
    # lente: vrijdag 20 maart tot zaterdag 20 juni
    # zomer: zaterdag 20 juni Tot dinsdag 22 september
    # herfst: dinsdag 22 september Tot: maandag 21 december
    # winter: zondag 22 december Tot: vrijdag 20 maart 2020
    season = ifelse(month %in% c(1,2,3), "Winter",
                    ifelse(month %in% c(4,5,6), "Spring",
                    ifelse(month %in% c(7,8,9), "Summer",
                    ifelse(month %in% c(10,11,12), "Autumn",NA))))
  ) 
# change weekday from dutch to english
train_features$week_day<-mapvalues(train_features$week_day, 
                                   from = c("ma", "di","wo","do","vr","za","zo"), 
                                   to = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat","Sun"))


# start and end date
train_features %>% head()
train_features %>% tail()
summary(train_features$total_cs)
train_features$date %>% diff.Date() %>% as.integer() %>% summary()


# general overview plot
p_overview_line <- ggplot(train_features,aes(x=date,y=total_cs)) +
  geom_line(size=.8,col=cbPalette[4]) + ylab("Total Consumption") + 
  labs(x = "Date") + THEME
p_overview_line %>% ggplotly()

p_overview_hist <- ggplot(train_features,aes(total_cs))+
  geom_histogram(color=cbPalette[7], fill="white", bins = 60) +
  labs(x = "Consumption", y = "Count") + THEME

p_overview_line/p_overview_hist

# profile weekend and non-weekend days
day_type_profile <- train_features[c("hour","day_type","total_cs")] %>%
  dplyr::group_by(day_type,hour) %>%
  dplyr::summarise(mean_total_cs = mean(total_cs),
                   sd_total_cs = sd(total_cs))

p_day_type_profile <- ggplot(data=day_type_profile,
  aes(x=as.integer(hour)-1,y=mean_total_cs,group=day_type)) +
  geom_line(aes(color=day_type,linetype=day_type)) +
  scale_color_brewer(palette="Dark2") +
  geom_line(aes(color=day_type)) +
  geom_point(aes(color=day_type)) +
  labs(x = "Hour", y = "Mean Total Consumption") +
  THEME + LEGEND
p_day_type_profile


# Night and day
p_night <- ggplot(train_features,aes(x=date, y=total_cs,col=working_hour)) +
  geom_line(alpha=.8,size=.8)+
  ylab("")+theme(legend.position="bottom")+
  scale_color_manual(values=c(cbPalette[6],cbPalette[1]))+
  labs(x = "", y = "Total consumption") + THEME + LEGEND 


# Weekend and non-weekend days
p_day_type <- ggplot(data=train_features,aes(x=date,
  y=total_cs,col=day_type))+geom_line(alpha=.8,size=.8) +
  labs(y = "Total Consumption", x = "")+theme(legend.position="bottom") +
  scale_color_brewer(palette="Dark2")+ labs(x = "Date") +
  THEME + LEGEND

#ggplotly(p_night)
#grid.arrange(p_day_type,p_night,nrow = 2)
(p_night/p_day_type)


# maybe add a normal distribution on top
figure2_pres <- ggplot(train_features, aes(x=total_cs))+
  geom_histogram(color=cbPalette[7], fill="white", bins = 50)+
  facet_grid(day_type ~ working_hour, scales = "free") + 
  labs(x = "Total Consumption", y = "Count") +
  THEME 


# hour
p_hour <- ggplot(train_features, aes(x=as.factor(hour), y=total_cs)) + 
  geom_boxplot() + labs(x="Hour",y="") + THEME
p_hour


# reorder factors
# weekday
train_features$week_day<-ordered(train_features$week_day, 
  levels = c("Mon", "Tue", "Wed","Thu","Fri","Sat","Sun"))

p_wday <- ggplot(train_features, aes(x=week_day, y=total_cs)) + 
  geom_boxplot() + labs(x="Day of the week",y="Consumption") +
  THEME + LEGEND
p_wday


weekday_profile <- train_features[c("hour","week_day","total_cs")] %>%
  dplyr::group_by(week_day, hour) %>%
  dplyr::summarise(mean_total_cs = mean(total_cs),
                   sd_total_cs = sd(total_cs))


p_weekday_profile <- ggplot(data=weekday_profile,
                            aes(x=as.numeric(hour) - 1, y=mean_total_cs, color=week_day))+ 
  geom_line(aes(color=week_day,linetype=week_day))+
  scale_color_brewer(palette="Dark2")+
  geom_point(aes(color=week_day))+
  ylab("Mean consumption")+
  xlab("Hour")+
  THEME+LEGEND
p_weekday_profile

# profile plot for and entirew week
df_week <- tibble(mean_total_cs = weekday_profile$mean_total_cs,
                  time = seq(1,168,1),
                  sd_total_cs = weekday_profile$sd_total_cs)


p_week_profile <- ggplot(df_week, aes(x = time,y = mean_total_cs)) + geom_line(size = .75) +
  geom_ribbon(data = df_week, aes(ymin = mean_total_cs - sd_total_cs, 
  ymax = mean_total_cs + sd_total_cs), fill = "firebrick2", alpha = 0.3) +
  geom_vline(xintercept = seq(24,6*24,24), linetype = 2, size = .5) + 
  labs(x = "Time (hourly)", y = "Mean cons. (1 stdev.)") + THEME +
  annotate("Text",x = seq(15,168,23),y = 21, label = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

p_week_profile
p_week_profile %>% ggplotly()



# season
p_season <- ggplot(train_features, aes(x=season, y=total_cs)) + 
  geom_boxplot() + labs(x="Season",y="Total Consumption") +
  labs(x="Season",y="") + THEME
p_season


# profile plot
season_profile <- train_features[c("hour","season","total_cs")] %>%
  dplyr::group_by(season,hour) %>%
  dplyr::summarise(mean_total_cs = mean(total_cs),
                   sd_total_cs = sd(total_cs))

p_season_profile <- ggplot(data=season_profile,
  aes(x=as.integer(hour)-1,y=mean_total_cs, group=season)) +
  geom_line(aes(color=season, linetype=season)) +
  geom_point(aes(color=season)) +
  scale_color_brewer(palette="Dark2")+
  ylab("Mean Consumption")+
  xlab("Hour")+ THEME + LEGEND
p_season_profile


figure3_pres <- ((p_week_profile)/(p_season_profile+p_weekday_profile)) + 
  plot_layout(heights = c(5,4))


# year
p_year <- ggplot(train_features, aes(x=year, y=total_cs)) + 
  geom_boxplot() + labs(x="Year",y="Total Consumption") +
  labs(x="Year",y="Total Consumption") + THEME
p_year



# summarize by  month
seasonal_month <- train_features[c("year","month","total_cs")] %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(monthly_cs = sum(total_cs))


ggplot(seasonal_month,aes(x=month,y=monthly_cs,group=year))+
  geom_line(aes(color=year))+
  geom_point(aes(color=year))+
  scale_color_brewer(palette="Dark2")+
  ylab("Monthly Consumption") + THEME + LEGEND


# summarize by week
seasonal_week <- train_features[c("year","week","total_cs")] %>%
  dplyr::group_by(year,week) %>%
  dplyr::summarise(weekly_cs = sum(total_cs))


ggplot(seasonal_week,aes(x=as.integer(week),y=weekly_cs, group=year))+
  geom_line(aes(color=year))+
  geom_point(aes(color=year))+
  scale_color_brewer(palette="Dark2")+
  ylab("Weekly Consumption")+xlab("Week number") + THEME + LEGEND


# summarize by day


p_acf <- ggAcf(train_features$total_cs, lag=672)+
  ggtitle("") + THEME
p_pacf <- ggPacf(train_features$total_cs, lag=672)+
  ggtitle("") + THEME

(p_acf/p_pacf)



# Extend the regression lines beyond the domain of the data
lag_1 <- ggplot(sample_n(train_features, 5000), 
                aes(x=total_cs_lag001, y=total_cs)) + 
  geom_point(alpha=.5,aes(colour = day_type)) +    scale_color_brewer(palette="Dark2")+
  xlab("Total Consumption lag 1")+ylab("Total Consumption")+ THEME + LEGEND



lag_96 <- ggplot(sample_n(train_features, 5000), aes(x=total_cs_lag096, y=total_cs)) + 
  geom_point(alpha=.5, aes(colour = day_type)) + scale_color_brewer(palette="Dark2") +
  xlab("Total Consumption lag 96") + ylab("") + THEME + LEGEND


lag_672 <- ggplot(sample_n(train_features, 5000), aes(x=total_cs_lag672, y=total_cs)) + 
  geom_point(alpha=.5, aes(colour = day_type)) + scale_color_brewer(palette="Dark2") +
  xlab("Total Consumption lag 672") + ylab("Total Consumption")+ THEME + LEGEND


(((lag_1 + lag_96)/lag_672) + plot_layout(guides = 'collect')) 



pairplot <- train_features %>% select(total_cs, total_cs_lag001,total_cs_lag096,
                                      total_cs_lag672,year) 


pairplot[1:nrow(pairplot),1:5] %>% 
  sample_n(., size =  1000, replace = FALSE) %>% 
  ggpairs(., mapping = aes(colour = year), 
  columns = c("total_cs", "total_cs_lag001", 
  "total_cs_lag096", "total_cs_lag672"), 
  columnLabels = c("Cons","Cons_lag001",
  "Cons_lag096","Cons_lag672"), alpha = .7) + THEME 


toc("EDA") # end timing






