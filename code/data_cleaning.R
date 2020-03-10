rm(list=ls())

library(tsibble)
library(tidyverse)
library(lubridate)
library(plyr)
library(reshape2)
library(zoo) # to impute zero values
library(tibbletime)
library(tictoc) # measures the time of the script
library(Hmisc) # for the correlation function

# visualize
library(plotly)
library(GGally)

# own functions
source("code/functions/read_data.R") 
source("code/functions/visualization.R")
source("code/functions/save_output.R")



# https://st-dev-data-api.azurewebsites.net/apidocs/


tic("Data Cleaning") # start timing

# Global parameters ---------------------------------------

# colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

THEME <- theme_minimal()
LEGEND <- theme(legend.title = element_blank())


#==============================#
# DATA FROM FIFTH
#==============================#

# read in data fifthplay
fifth <- get_data(dir="data/fifthplay/",
 vars=c("DateTimeMeasurement","Value"))

# some summary stats
lapply(fifth,summary)

# NA values?
lapply(fifth,anyNA)

# check whether time interval is subsequently 15 mins constant
fifth_timediff <- lapply(fifth,function(df)diff(df$Date))


# there are periods of jumps sometimes weeks of missing data
# therefore when comparing data with fluvious check if timestamps match
lapply(fifth_timediff,function(df)summary(as.integer(df)))

# % of data where the time interval is 15%
sum(fifth_timediff$consumptionTR1==15)/length(fifth_timediff$consumptionTR1)


subset_data <- function(df,start,end, vars=NULL){
  if (is.null(vars)) {vars=c(colnames(df))}
  df[df$Date >= start & df$Date <= end,vars]
}

fifth$consumptionTR1[2:dim(fifth$consumptionTR1)[1],][
  fifth_timediff$consumptionTR1==35700.00,]

# for example there is almost a month between these two dates 
# this might be probablematic for modelling, espically such long periods
# hopefully we can impute these data based on the fluvius dataset
subset_data(fifth$consumptionTR1,
  start="2018-03-09 13:15:00",
  end="2018-04-03 09:15:00",
  vars=c("Date","Value"))

# Maximum minimum date to start: "2017-05-31 13:00:00 CEST"
lapply(fifth,function(df)head(df$Date,1))
# Minimum Maximum date to end: "2020-02-05 23:45:00 CET"
lapply(fifth,function(df)tail(df$Date,1))


# take the max minimum time and the minimum max time 
# based on both the fluvius and fifth dataset
fifth_subset <- lapply(fifth,subset_data,
 start = "2017-05-31 13:00:00 CEST",
 end = "2020-02-05 23:45:00 CET",
 vars=c("Date","Value"))


# Check if first date and last date are the same of all dataframes
lapply(fifth_subset,head)
lapply(fifth_subset,tail)


#check whether lengths are equal: NOPE
lapply(fifth_subset,dim)


# Total Consumption fifth
fifth_totalCs <- tibble(Date=fifth_subset$consumptionTR1$Date,
  Value=(fifth_subset$consumptionTR1$Value + 
  fifth_subset$consumptionTR2$Value)
                      )

# Total Injection fifth
fifth_totalIj <- tibble(Date=fifth_subset$injectionTR1$Date,
  Value=(fifth_subset$injectionTR1$Value + 
  fifth_subset$injectionTR1$Value))
# pv generation
fifth_pvG<-fifth_subset$pvGeneration



#==============================#
# DATA FROM FLUVIUS
#==============================#

# read ine data from fluvius
fluv<-get_data(dir="data/fluvius/")


fluv_timediff <-lapply(fluv,function(df)diff(df$Date))

# here all data is 15min constant (this is how it should be)
lapply(fluv_timediff,function(df)summary(as.integer(df)))

lapply(fluv, dim)
lapply(fluv,head)
lapply(fluv,tail)


# Maximum minimum date to start: "2016-02-18 00:15:00 CET"
lapply(fluv,function(df)head(df$Date,1))

# Minimum Maximum date to end: "2019-11-15 CET"
lapply(fluv,function(df)tail(df$Date,1))

# take the same start and end date as fifth
fluv_subset<-lapply(fluv,subset_data,
  start = "2017-05-31 13:00:00 CEST",
  end = "2019-11-15 CET",
  vars=c("Date","Active","Capacitive","Inductive"))


# !!! Need to divide by 4 since it's not measured in the same unit as for fifthplay
# Also we only need date and Active
fluv_inj<-select(fluv_subset$injection,c(Date,Active)) %>% 
  mutate(Active=Active/4)
fluv_cs<-select(fluv_subset$consumption,c(Date,Active)) %>% 
  mutate(Active=Active/4)
fluv_pvG<-select(fluv_subset$pv,c(Date,Active)) %>% 
  mutate(Active=Active/4)


#==============================#
# COMPARE DATA
#==============================#

#==============================#
# 1) Consumption
#==============================#


# check both the same ending and starting date
fluv_cs %>% head() ; fifth_totalCs %>% tail()
fifth_totalCs %>% head(); fifth_totalCs %>% tail()

# but still different lengths
fluv_cs %>% dim() ; fifth_totalCs %>% dim()

# left outer join based on same date
# this will result in NA values
cs_fluvFifth <- left_join(fluv_cs,fifth_totalCs,by='Date')

# rename columns
cs_fluvFifth <- plyr::rename(cs_fluvFifth, 
 c("Active"="Fluvius","Value"="Fifthplay"))

# visualize
# compare comsumption between fifth and Fluvius 
comp_cs <- compare_fifthFluv(df=cs_fluvFifth,freq='daily',
 ylab="Consumption")

comp_cs$figure + THEME

# overview plot
ggpairs(comp_cs$dataframe[,1:3]) + THEME

#boxplot
ggplot(melt(comp_cs$dataframe[,2:3]),
 aes(variable,value)) + geom_boxplot() + xlab("") + THEME



#==============================#
# 2) Injection
#==============================#

# check both the same ending and starting date
head(fluv_inj);tail(fluv_inj)
head(fifth_totalIj);tail(fifth_totalIj)

# but still different lengths
dim(fluv_inj);dim(fifth_totalIj)

# left outer join based on same date
# this will result in NA values
inj_fluvFifth<-left_join(fluv_inj,fifth_totalIj,by='Date')
head(inj_fluvFifth)
dim(inj_fluvFifth)


# rename columns
inj_fluvFifth <- plyr::rename(inj_fluvFifth, 
 c("Active"="Fluvius","Value"="Fifthplay"))

# compare comsumption between fifth and Fluvius 
comp_Inj <- compare_fifthFluv(df=inj_fluvFifth,
  freq='daily',ylab="Injection")
comp_Inj$figure + THEME


# overview plot
ggpairs(comp_Inj$dataframe[,1:3]) + THEME

#boxplot
ggplot(melt(comp_Inj$dataframe[,2:3]),
   aes(variable,value)) + geom_boxplot() + xlab("") + THEME




#==============================#
# 3) Pv generation
#==============================#

head(fifth_pvG);tail(fifth_pvG)
head(fluv_pvG);tail(fluv_pvG)


# but still different lengths
dim(fluv_pvG);dim(fifth_pvG)

# inner join based on same date
# left outer join based on same date
# this will result in NA values
pvGen_fluvFifth <- left_join(fluv_pvG,fifth_pvG,by='Date')


# rename columns
pvGen_fluvFifth <- plyr::rename(pvGen_fluvFifth, 
  c("Active"="Fluvius","Value"="Fifthplay"))


# compare comsumption between fifth and Fluvius 
comp_pvGen <- compare_fifthFluv(df=pvGen_fluvFifth,
  freq='daily',ylab="Pv Generation")
comp_pvGen$figure + THEME

# overview plot
ggpairs(comp_pvGen$dataframe[,1:3]) + THEME


#boxplot
ggplot(melt(comp_pvGen$dataframe[,2:3]),
  aes(variable,value)) + geom_boxplot() + xlab("") + THEME

#==============================#
# 4) Total consumption
#==============================#

# From there you can get consumption, PV and injection data. 
# Note that consumption here means the consumption from the grid only; 
# the building also consumes energy coming from the PV installation. 
# So the total consumption can be calculated via the following formula:
# Total consumption = consumption + PV - Injection


# using variable active to compute total consumption
fluvFifth_totC <- tibble(Date=fluv_cs$Date,
  fluv_consAct = cs_fluvFifth$Fluvius,
  fluv_pvAct = pvGen_fluvFifth$Fluvius,
  fluv_injAct = inj_fluvFifth$Fluvius,
  fifth_cons = cs_fluvFifth$Fifthplay,
  fifth_inj = pvGen_fluvFifth$Fifthplay,
  fifth_pvG = inj_fluvFifth$Fifthplay,
  #  total consumption FLUVIUS: 
  fluv_TotalCs=(
  cs_fluvFifth$Fluvius + 
    pvGen_fluvFifth$Fluvius-
    inj_fluvFifth$Fluvius),
  #  total consumption FIFTH:              
  fifth_TotalCs = (cs_fluvFifth$Fifthplay +
    pvGen_fluvFifth$Fifthplay-
    inj_fluvFifth$Fifthplay))

# interval is 15 min
summary(as.integer(diff(fluvFifth_totC$Date)))

# start: 2017-05-31 13:00:00 
# end: 2019-09-09 00:00:00
head(fluvFifth_totC);tail(fluvFifth_totC)


# compare comsumption between fifth and Fluvius 

compare_totalcs <- fluvFifth_totC %>% 
  select(Date,fluv_TotalCs,fifth_TotalCs) %>%
  plyr::rename(.,c("fluv_TotalCs"="Fluvius","fifth_TotalCs"="Fifthplay")) %>%
  compare_fifthFluv(df=.,freq='daily',ylab="Total Consumption")

compare_totalcs$figure + geom_line(alpha = .5)+
  labs(colour="") + scale_color_brewer(palette="Set1") + THEME + 
  theme(legend.position="top")


# scatterplot
ggplot(compare_totalcs$dataframe, 
                         aes(x=Fluvius, y=Fifthplay, text = Date)) + 
                         geom_point(size=1.5) + THEME

# overview plot
ggpairs(compare_totalcs$dataframe[1:3]) + THEME


#boxplot
ggplot(melt(compare_totalcs$dataframe[2:3]),
  aes(variable,value)) + geom_boxplot() + xlab("") + THEME

#===================================================
# Missing values imputation: use values from fluvius
#===================================================


# Impute missing values and only select the columns 
# Date and Imputed column of fifthplay
df_FifthCleaned <- fluvFifth_totC %>%
  mutate(TotalCs_imp = if_else(is.na(fifth_TotalCs), 
  fluv_TotalCs,fifth_TotalCs)) %>%
  select(c(Date,TotalCs_imp))  


# Check for missing values
anyNA(df_FifthCleaned$TotalCs_imp)

# Dates should be unique
df_FifthCleaned
head(df_FifthCleaned[df_FifthCleaned$Date>"2017-10-29",],20)


# interval should be 15 min
diff(df_FifthCleaned$Date) %>% as.integer() %>% summary()

# zero values in the dataset
df_FifthCleaned$TotalCs_imp %>% summary()
df_FifthCleaned %>% filter(TotalCs_imp==0) %>% nrow


# impute zero values
df_FifthCleaned$TotalCs_imp <- na.spline(replace(df_FifthCleaned$TotalCs_imp, 
                             df_FifthCleaned$TotalCs_imp == 0, NA))

# save cleaned data 

# interval should be still 15 min
diff(df_FifthCleaned$Date) %>% as.integer(.) %>% summary(.)

# cvs file
save_output(output_dir="data/fifthplay/cleaned_data", 
            FUN=write.csv,
            x=df_FifthCleaned,
            file="finalTotC.csv")

# save as R object
save_output(output_dir="data/fifthplay/cleaned_data", 
            FUN=saveRDS,
            object=df_FifthCleaned,
            file="finalTotC.Rda")


toc() # end timing













