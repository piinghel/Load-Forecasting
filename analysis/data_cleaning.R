# clean memory
rm(list = ls())

# --------------------------------------------------------------------
# 0) Load libraries
# --------------------------------------------------------------------

# data manipulation
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
source("analysis/functions/read_data.R")
source("analysis/functions/visualization.R")
source("analysis/functions/save_output.R")
source("analysis/functions/split_data.R")


# https://st-dev-data-api.azurewebsites.net/apidocs/


tic("Data Cleaning") # start timing

# --------------------------------------------------------------------
#  1) Global parameters 
# --------------------------------------------------------------------

# colors
cbPalette <- c(
  "#999999",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

THEME <- theme_gray() #theme_minimal()
LEGEND <- theme(legend.title = element_blank())

# --------------------------------------------------------------------
#  2) Load Data 
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# 2.1) data from fifthplay
# --------------------------------------------------------------------

# read in data fifthplay
fifth <- get_data(dir = "data/fifthplay/",
                  vars = c("DateTimeMeasurement", "Value")) 

# some summary stats
lapply(fifth, summary)

# NA values?
lapply(fifth, anyNA)

# check whether time interval is subsequently 15 mins constant
fifth_timediff <- lapply(fifth, function(df)
  diff(df$date))


format_iso_8601

# there are periods of jumps sometimes weeks of missing data
# therefore when comparing data with fluvious check if timestamps match
lapply(fifth_timediff, function(df)
  summary(as.integer(df)))

# % of data where the time interval is 15%
sum(fifth_timediff$consumption_tr1 == 15) / length(fifth_timediff$consumption_tr1)


subset_data <- function(df, start, end, vars = NULL) {
  if (is.null(vars)) {
    vars = c(colnames(df))
  }
  df[df$date >= start & df$date <= end, vars]
}

fifth$consumption_tr1[2:dim(fifth$consumption_tr1)[1], ][fifth_timediff$consumption_tr1 ==
                                                         35700.00, ]

# for example there is almost a month between these two dates
# this might be probablematic for modelling, espically such long periods
# hopefully we can impute these data based on the fluvius dataset
subset_data(
  fifth$consumption_tr1,
  start = "2018-03-09 13:15:00",
  end = "2018-04-03 09:15:00",
  vars = c("date", "value")
)

# Maximum minimum date to start: "2017-05-31 13:00:00 CEST"
lapply(fifth, function(df)
  head(df$date, 1))
# Minimum Maximum date to end: "2020-02-05 23:45:00 CET"
lapply(fifth, function(df)
  tail(df$date, 1))


# take the max minimum time and the minimum max time
# based on both the fluvius and fifth dataset
fifth_subset <- lapply(
  fifth,
  subset_data,
  start = "2017-05-31 13:00:00 CEST",
  end = "2020-02-05 23:45:00 CET",
  vars = c("date", "value")
)


# Check if first date and last date are the same of all dataframes
lapply(fifth_subset, head)
lapply(fifth_subset, tail)


#check whether lengths are equal: NOPE
lapply(fifth_subset, dim)


# Total Consumption fifth
fifth_total_cs <- tibble(
  date = fifth_subset$consumption_tr1$date,
  value = (
    fifth_subset$consumption_tr1$value +
      fifth_subset$consumption_tr2$value
  )
)

# Total Injection fifth
fifth_total_injection <- tibble(
  date = fifth_subset$injection_tr1$date,
  value = (
    fifth_subset$injection_tr1$value +
      fifth_subset$injection_tr1$value
  )
)
# pv generation
fifth_pv <- fifth_subset$pv_generation



# --------------------------------------------------------------------
# 2.2) DATA FROM fluvius
# --------------------------------------------------------------------

# read ine data from fluvius
fluv <- get_data(dir = "data/fluvius/")


fluv_timediff <- lapply(fluv, function(df)
  diff(df$date))

# here all data is 15min constant (this is how it should be)
lapply(fluv_timediff, function(df)
  summary(as.integer(df)))

lapply(fluv, dim)
lapply(fluv, head)
lapply(fluv, tail)


# Maximum minimum date to start: "2016-02-18 00:15:00 CET"
lapply(fluv, function(df)
  head(df$date, 1))

# Minimum Maximum date to end: "2020-02-03 CET"
lapply(fluv, function(df)
  tail(df$date, 1))

# take the same start and end date as fifth
fluv_subset <- lapply(
  fluv,
  subset_data,
  start = "2017-05-31 13:00:00 CEST",
  end = "2020-02-03 CET",
  vars = c("date", "active", "capacitive", "inductive")
)


# !!! need to divide by 4 since it's not measured in the same unit as for fifthplay
# Also we only need date and active !!!
fluv_inj <- select(fluv_subset$injection, c(date, active)) %>%
  mutate(active = active / 4)
fluv_cs <- select(fluv_subset$consumption, c(date, active)) %>%
  mutate(active = active / 4)
fluv_pv <- select(fluv_subset$pv, c(date, active)) %>%
  mutate(active = active / 4)


# --------------------------------------------------------------------
#  3) COMPARE DATA
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# 3.1) Consumption
# --------------------------------------------------------------------


# check both the same ending and starting date
fluv_cs %>% head()
fifth_total_cs %>% tail()
fifth_total_cs %>% head()
fifth_total_cs %>% tail()

# but still different lengths
fluv_cs %>% dim()
fifth_total_cs %>% dim()

# left outer join based on same date
# this will result in NA values
cs_fluv_fifth <- left_join(fluv_cs, fifth_total_cs, by = 'date')

# rename columns
cs_fluv_fifth <- plyr::rename(cs_fluv_fifth,
                             c("active" = "fluvius", "value" = "fifthplay"))

# visualize
# compare comsumption between fifth and fluvius
comp_cs <- compare_fifthFluv(df = cs_fluv_fifth, freq = 'daily',
                             ylab = "Consumption")

comp_cs$figure + THEME

# overview plot
ggpairs(comp_cs$dataframe[, 1:3]) + THEME

#boxplot
ggplot(melt(comp_cs$dataframe[, 2:3]),
       aes(variable, value)) + geom_boxplot() + xlab("") + THEME

# --------------------------------------------------------------------
# 3.2) Injection
# --------------------------------------------------------------------

# check both the same ending and starting date
head(fluv_inj)
tail(fluv_inj)
head(fifth_total_injection)
tail(fifth_total_injection)

# but still different lengths
dim(fluv_inj)
dim(fifth_total_injection)

# left outer join based on same date
# this will result in NA values
inj_fluv_fifth <- left_join(fluv_inj, fifth_total_injection, by = 'date')
head(inj_fluv_fifth)
dim(inj_fluv_fifth)


# rename columns
inj_fluv_fifth <- plyr::rename(inj_fluv_fifth,
                              c("active" = "fluvius", "value" = "fifthplay"))

# compare comsumption between fifth and fluvius
comp_inj <- compare_fifthFluv(df = inj_fluv_fifth,
                              freq = 'daily', ylab = "Injection")
comp_inj$figure + THEME


# overview plot
ggpairs(comp_inj$dataframe[, 1:3]) + THEME

#boxplot
ggplot(melt(comp_inj$dataframe[, 2:3]),
       aes(variable, value)) + geom_boxplot() + xlab("") + THEME


# --------------------------------------------------------------------
# 3.3) Pv generation
# --------------------------------------------------------------------

head(fifth_pv)
tail(fifth_pv)
head(fluv_pv)
tail(fluv_pv)


# but still different lengths
dim(fluv_pv)
dim(fifth_pv)

# inner join based on same date
# left outer join based on same date
# this will result in NA values
pv_gen_fluvFifth <- left_join(fluv_pv, fifth_pv, by = 'date')


# rename columns
pvGen_fluvFifth <- plyr::rename(pv_gen_fluvFifth,
                                c("active" = "fluvius", "value" = "fifthplay"))


# compare comsumption between fifth and fluvius
comp_pv <- compare_fifthFluv(df = pvGen_fluvFifth,
                                freq = 'daily', ylab = "Pv Generation")
comp_pv$figure + THEME

# overview plot
ggpairs(comp_pv$dataframe[, 1:3]) + THEME


#boxplot
ggplot(melt(comp_pv$dataframe[, 2:3]),
       aes(variable, value)) + geom_boxplot() + xlab("") + THEME

# --------------------------------------------------------------------
# 3.4) Total consumption
# --------------------------------------------------------------------

# From there you can get consumption, PV and injection data.
# Note that consumption here means the consumption from the grid only;
# the building also consumes energy coming from the PV installation.
# So the total consumption can be calculated via the following formula:
# Total consumption = consumption + PV - Injection

fluv_cs$date %>% tail

# using variable active to compute total consumption
fluv_fifth_tot_c <- tibble(
  date = fluv_cs$date,
  fluv_consAct = cs_fluv_fifth$fluvius,
  fluv_pvAct = pvGen_fluvFifth$fluvius,
  fluv_injAct = inj_fluv_fifth$fluvius,
  fifth_cons = cs_fluv_fifth$fifthplay,
  fifth_inj = pvGen_fluvFifth$fifthplay,
  fifth_pv = inj_fluv_fifth$fifthplay,
  #  total consumption fluvius:
  fluv_total_cs = (
    cs_fluv_fifth$fluvius +
      pvGen_fluvFifth$fluvius -
      inj_fluv_fifth$fluvius
  ),
  #  total consumption FIFTH:
  fifth_total_cs = (
    cs_fluv_fifth$fifthplay +
      pvGen_fluvFifth$fifthplay -
      inj_fluv_fifth$fifthplay
  )
) %>% as_tsibble(., index = date)

# interval is 15 min
summary(as.integer(diff(fluv_fifth_tot_c$date)))

# start: 2017-05-31 13:00:00
# 2020-02-03 00:00:00
head(fluv_fifth_tot_c)
tail(fluv_fifth_tot_c)


# compare comsumption between fifth and fluvius

compare_totalcs <- fluv_fifth_tot_c %>%
  select(date, fluv_total_cs, fifth_total_cs) %>%
  plyr::rename(., c("fluv_total_cs" = "fluvius", "fifth_total_cs" = "fifthplay")) %>%
  compare_fifthFluv(df = ., freq = '15 min', ylab = "Total Consumption")

compare_totalcs$figure + geom_line(alpha = .5) +
  labs(colour = "") + scale_color_brewer(palette = "Set1") + THEME +
  theme(legend.position = "top") + labs(x="Date", y="Total Building Consumption")

# save figure
save_output(output_dir = "output/figures", 
            FUN=ggsave,filename = "figure1.png", 
            device = "png",
            height = 9,
            width = 12,
            unit = "cm")

# scatterplot
ggplot(compare_totalcs$dataframe,
       aes(x = fluvius, y = fifthplay, text = date)) +
  geom_point(size = 1.5) + THEME

# overview plot
ggpairs(compare_totalcs$dataframe[1:3]) + THEME


#boxplot
ggplot(melt(compare_totalcs$dataframe[2:3]),
       aes(variable, value)) + geom_boxplot() + xlab("") + THEME


# --------------------------------------------------------------------
# 4) Missing values imputation: use values from fluvius
# --------------------------------------------------------------------


# Impute missing values and only select the columns
# date and Imputed column of fifthplay
df_fifth_cleaned <- fluv_fifth_tot_c %>%
  mutate(total_cs = if_else(is.na(fifth_total_cs),
                               fluv_total_cs, fifth_total_cs)) %>%
  select(c(date, total_cs))


# Check for missing values
anyNA(df_fifth_cleaned$total_cs)

# dates should be unique
df_fifth_cleaned
head(df_fifth_cleaned[df_fifth_cleaned$date > "2017-10-29", ], 20)


# interval should be 15 min
diff(df_fifth_cleaned$date) %>% as.integer() %>% summary()

# zero values in the dataset
df_fifth_cleaned$total_cs %>% summary()
df_fifth_cleaned %>% filter(total_cs == 0) %>% nrow


# impute zero values
df_fifth_cleaned$total_cs <-
  na.spline(replace(
    df_fifth_cleaned$total_cs,
    df_fifth_cleaned$total_cs == 0,
    NA
  ))

# interval should be still 15 min
diff(df_fifth_cleaned$date) %>% as.integer %>% summary

df_fifth_cleaned %>% select(date) %>% duplicated %>% sum



# --------------------------------------------------------------------
# 5) save cleaned data
# --------------------------------------------------------------------


# save cleaned data as cvs file
save_output(
  output_dir = "data/cleaned_data",
  FUN = write.csv,
  x = df_fifth_cleaned,
  file = "final_tot_c",
  row.names=FALSE
)


# save cleaned data as Rda object
save_output(
  output_dir = "data/cleaned_data",
  FUN = saveRDS,
  object = df_fifth_cleaned,
  file = "final_tot_c.Rda"
)

# split data in training/validation/training_validation/test set

# 1) original unit
split_data(
  df = df_fifth_cleaned,
  train_perc = .6,
  validation_perc = .2,
  embargo = 0,
  save = TRUE,
  return_split = FALSE,
  output_dir = "data/cleaned_data/split_original"
)


# 2) summarize to hourly units

df_fifth_cleaned_hourly <- df_fifth_cleaned %>% 
  
  tsibble::index_by(
  # there is a difference between floor_date and ceiling_date
  date_h = lubridate::floor_date(date, "1 hour")
  ) %>% 
  dplyr::summarise(total_cs = sum(total_cs)) %>%
  # again rename
  dplyr::rename(
    date = date_h
)

split_data(
  df = df_fifth_cleaned_hourly,
  train_perc = .6,
  validation_perc = .2,
  embargo = 0,
  save = TRUE,
  return_split = FALSE,
  output_dir = "data/cleaned_data/split_hourly"
)







