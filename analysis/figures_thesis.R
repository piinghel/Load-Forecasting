# clean environment
rm(list = ls())


# --------------------------------------------------------------------
# 0) Load libraries
# --------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(tidymodels)
library(plyr)
library(patchwork)
library(scoringutils)


source("analysis/functions/evaluation_metrics.R")
# --------------------------------------------------------------------
#  1) Global parameters
# --------------------------------------------------------------------
set.seed(69)

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

# figure quality
DPI <- 500
DEVICE = "pdf"

# -------------------------------------------------------------------
# 1) CHAPTER 3
# -------------------------------------------------------------------

# 1) reliability diagram
tibble(
  nominal = seq(0.01, 0.99, 0.01),
  empirical = seq(0.01, 0.99, 0.01) + rnorm(99, 0, 0.025),
  ideal = seq(0.01, 0.99, 0.01)
) %>% gather(., type, measurement, empirical:ideal, factor_key = TRUE) %>%
  ggplot(., aes(x = nominal, y = measurement, color = type)) +
  geom_line(aes(linetype = type, color = type), size = 1.2) +
  geom_point(alpha = .5) +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c('black', 'red')) +
  labs(x = "Nominal", y = "Emperical") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave(
  paste0("output/figures/chapter3/figure4.",DEVICE),
  width = 12,
  height = 8,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)


# 2) PIT
tibble(x = seq(0.05, .95, 0.1),
       y = c(103, 105, 96, 105, 91, 100, 93, 105, 102, 96)) %>%
  ggplot +
  geom_col(aes(y = y, x = x),
           color = "black",
           fill = "white",
           orientation = "x") +
  geom_hline(aes(yintercept = 100, linetype = "reference"),
             colour = "red",
             size = 1) +
  labs(x = "PIT samples", y = "Frequency") +
  scale_linetype_manual(
    name = "limit",
    values = 2,
    guide = guide_legend(override.aes = list(color = c("red")))
  ) +
  theme(legend.position = "top", legend.title = element_blank())

ggsave(
  paste0("output/figures/chapter3/figure5.",DEVICE),
  width = 12,
  height = 8,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)


# -------------------------------------------------------------------
# 2) CHAPTER 4
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2.1 DETERMINISTIC
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2.1.1 POINT FORECASTING
# -------------------------------------------------------------------
PERF_METRIC_SET_FH <- metric_set(rsq,
                                 mae,
                                 mape,
                                 mase)

pred_validation <-
  list.files(path = "./output/deterministic/",
             pattern = "*.csv",
             full.names = T) %>%
  map_df(~ read_csv(.))

pred_validation$model <- mapvalues(
  pred_validation$model,
  from = c("elastic net", "random forest", "boosting", "mlp"),
  to = c("Elastic net", "RF", "GBM", "MLP")
)


perf_point <-
  pred_validation %>% mutate(hour = lubridate::hour(date)) %>%
  group_by(model, hour)  %>%
  PERF_METRIC_SET_FH(.,
                     truth = actual,
                     estimate = fitted,
                     m = 7)

perf_point %>% mutate(.metric =  mapvalues(
  perf_point$.metric,
  from = c("mae", "mape", "mase", "rsq"),
  to = c("MAE", "MAPE (%)", "MASE (168)", "R2")
)) %>%
  ggplot(aes(x = hour, y = .estimate, colour = model))  +
  geom_point(aes(shape = model)) + geom_line(aes(linetype = model))  +
  facet_wrap(vars(.metric), scales = "free") +
  labs(x = "Hour" , y = "Point estimate") +
  theme(legend.position = "bottom",
        legend.title = element_blank())


ggsave(
  paste0("output/figures/chapter4/deterministic/figure2.", DEVICE),
  width = 20,
  height = 15,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)

# -------------------------------------------------------------------
# 2.1.2 PROBABLISTIC FORECASTING
# -------------------------------------------------------------------


# 1) PERFORMANCE DIAGNOSTICS
probPerf_enet <-
  read.csv("output/deterministic/elastic net/probPerf_val.csv") %>% mutate(model = "Elastic net")
probPerf_rf <-
  read.csv("output/deterministic/random forest/probPerf_val.csv") %>% mutate(model = "RF")
probPerf_boosting <-
  read.csv("output/deterministic/boosting/probPerf_val.csv") %>% mutate(model = "GBM")
probPerf_mlp <-
  read.csv("output/deterministic/mlp/probPerf_val.csv") %>% mutate(model = "MLP")

probPerf <-
  rbind(probPerf_enet, probPerf_rf, probPerf_boosting, probPerf_mlp)

probPerf <- probPerf %>% mutate(metric = mapvalues(
  probPerf$metric,
  from = c("winkler score", "crps", "pinball loss"),
  to = c("Winkler score", "CRPS", "Pinball loss")
))

# 1) norm uncond
p_normUncond <- probPerf %>% filter(method == "norm uncond.") %>%
  ggplot(., aes(x = hour, y = value, color = model)) + 
  geom_point(aes(shape = model))  +
  geom_line(aes(linetype = model)) +
  facet_wrap(vars(metric), scales = "free") +
  ggtitle("Normal distribution: uncondional") +
  labs(x = "Hour", y = "Point estimate")

# 2) norm cond
p_normCond <- probPerf %>% filter(method == "norm cond.") %>%
  ggplot(., aes(x = hour, y = value, color = model)) + 
  geom_point(aes(shape = model))  +
  geom_line(aes(linetype = model)) +
  facet_wrap(vars(metric), scales = "free") +
  ggtitle("Normal distribution: conditional") +
  labs(x = "Hour", y = "Point estimate")

# 3) boot uncond.
p_bootUncond <- probPerf %>% filter(method == "boot uncond.") %>%
  ggplot(., aes(x = hour, y = value, color = model)) + 
  geom_point(aes(shape = model))  +
  geom_line(aes(linetype = model)) +
  facet_wrap(vars(metric), scales = "free") +
  ggtitle("Bootstrap: uncondional") +
  labs(x = "Hour", y = "Point estimate")

# 4) boot cond.
p_bootCond <- probPerf %>% filter(method == "boot cond.") %>%
  ggplot(., aes(x = hour, y = value, color = model)) + 
  geom_point(aes(shape = model))  +
  geom_line(aes(linetype = model)) +
  facet_wrap(vars(metric), scales = "free") +
  ggtitle("Bootstrap: condional") +
  labs(x = "Hour", y = "Point estimate")

# 5) quantile reg
p_quantR <- probPerf %>% filter(method == "quantile reg.") %>%
  ggplot(., aes(x = hour, y = value, color = model)) + 
  geom_point(aes(shape = model))  +
  geom_line(aes(linetype = model)) +
  facet_wrap(vars(metric), scales = "free") +
  ggtitle("Quantile reg.") + theme(legend.position = "none") +
  scale_colour_manual(values = c("#7CAE00", "#C77CFF")) +
  scale_shape_manual(values = c(17, 3)) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  labs(x = "Hour", y = "Point estimate")

p <-
  ((p_normUncond + theme(legend.position = "none"))  /
     p_normCond / p_bootUncond / p_bootCond / (p_quantR + theme(legend.position = "none"))) + 
  plot_layout(guides = 'collect') 

 

(
  p_quantR  + theme(legend.position = "none")
)/
  (
    p_normUncond + theme(legend.position = "none")
  ) /
  (p_normCond + theme(legend.position = "none")) /
  (p_bootUncond + theme(legend.position = "none")) /
  (p_bootCond & plot_layout(guides = "collect") & 
     theme(legend.position = "bottom", legend.title = element_blank())) &
  plot_annotation(tag_levels = 'A')


# save
ggsave(
  paste0("output/figures/chapter4/deterministic/figure3.", DEVICE),
  width = 30,
  height = 40,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)


# 3) COMPARE FORECASTS

START <- 24 * 7 * 7
END <- 24 * 7 * 9
pred_enet <-
  read_csv("output/deterministic/elastic net/pred_validation.csv") %>% mutate(model = "Elastic net") %>% slice(START:END)
pred_rf <-
  read_csv("output/deterministic/random forest/pred_validation.csv") %>% mutate(model = "RF") %>%
  slice((START - 168 * 2):(END - 168 * 2)) %>% select(-c(lower_q, upper_q, cover_q)) 
pred_boost <-
  read_csv("output/deterministic/boosting/pred_validation.csv") %>% mutate(model = "GBM") %>%
  slice(START:END) %>% select(-c(lower_q, upper_q, cover_q))
pred_mlp <-
  read_csv("output/deterministic/mlp/pred_validation.csv") %>% mutate(model = "MLP") %>% slice(START:END)

compare_pred <- rbind(pred_enet, pred_rf, pred_boost, pred_mlp)



# elastic net
p_pred_enet <-
  pred_enet %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type), size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nC,
                  ymax = upper_nC),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "", y = "Building consumption (kWh)") +
  ggtitle("Elastic net") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# Random forest
p_pred_rf <- pred_rf %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type) , size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nC,
                  ymax = upper_nC),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "", y = "") +
  ggtitle("RF") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
    
  )

# Boosting
p_pred_boost <-
  pred_boost %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type) , size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nC,
                  ymax = upper_nC),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "Date", y = "Building consumption (kWh)") +
  ggtitle("GBM") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
    
  )

# MLP
p_pred_mlp <-
  pred_mlp %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type) , size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nC,
                  ymax = upper_nC),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "Date", y = "") + ggtitle("MLP") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
    
  )

(p_pred_enet + p_pred_rf) / (p_pred_boost + p_pred_mlp) +
  plot_layout(guides = 'collect') &
  plot_annotation(tag_levels = 'A') &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


ggsave(
  paste0("output/figures/chapter4/deterministic/figure4.",DEVICE),
  width = 30,
  height = 20,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)


# -------------------------------------------------------------------
# 2.2 DYNAMIC
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 2.2.1 POINT FORECASTING
# -------------------------------------------------------------------

intra_val_enet <-
  read.csv("output/dynamic/forecast_horizon/ElasticNet.csv") %>%
  as_tibble() %>% mutate(model = "Elastic net")

intra_val_rf <-
  read.csv("output/dynamic/forecast_horizon/RandomForest.csv") %>%
  as_tibble() %>% mutate(model = "RF")

intra_val_gbm <-
  read.csv("output/dynamic/forecast_horizon/GBM.csv") %>%
  as_tibble() %>% mutate(model = "GBM")

intra_val_mlp <-
  read.csv("output/dynamic/forecast_horizon/MLP.csv") %>%
  as_tibble() %>% mutate(model = "MLP")


intra_val <-
  rbind(intra_val_enet, intra_val_rf, intra_val_gbm, intra_val_mlp)



perf_best_model <-
  data.frame(
    best_model = c(12.785, 9.197, 0.573, 0.826),
    .metric = c("MAE", "MAPE (%)", "MASE (168)", "R2")
  )

point_perf_dynam <- intra_val %>%
  group_by(forecast_horizon, model) %>%
  PERF_METRIC_SET_FH(.,
                     truth = actual,
                     estimate = fitted,
                     m = 168)


point_perf_dynam <- mutate(point_perf_dynam,
                           .metric =  mapvalues(
                             point_perf_dynam$.metric,
                             from = c("mae", "mape", "mase", "rsq"),
                             to = c("MAE", "MAPE (%)", "MASE (168)", "R2")
                           ))


p <-  point_perf_dynam %>%
  ggplot(aes(x = forecast_horizon, y = .estimate, color = model)) +
  geom_point(aes(shape = model)) + geom_line(aes(linetype = model)) + scale_x_continuous(breaks = c(seq(1, 24, 5), 24)) +
  facet_wrap(vars(.metric), scales = "free") +
  scale_y_continuous(trans = 'pseudo_log') +
  labs(y = "Point estimate", x = "Forecast Horizon (update horizon)") +
  theme(legend.position = "bottom",
        legend.title = element_blank())


p + geom_hline(aes(yintercept = best_model), perf_best_model, linetype = "dashed")


ggsave(
  paste0("output/figures/chapter4/dynamic/figure1.",DEVICE),
  width = 20,
  height = 15,
  units = "cm",
  device = DEVICE
)


# -------------------------------------------------------------------
# 2.2.2 PROBABLISTIC FORECASTING
# -------------------------------------------------------------------


START <- 24 * 7 * 8
END <- 24 * 7 * 10
pred_enet <-
  read_csv("output/dynamic/elastic net/predProb_val.csv") %>% mutate(model = "Elastic net") %>% slice(START:END)
pred_rf <-
  read_csv("output/dynamic/random forest/predProb_val.csv") %>% mutate(model = "RF") %>%
  slice(START:END) %>% select(-c(lower_q, upper_q, cover_q))
pred_boost <-
  read_csv("output/dynamic/boosting/predProb_val.csv") %>% mutate(model = "GBM") %>%
  slice(START:END) %>% select(-c(lower_q, upper_q, cover_q))
pred_mlp <-
  read_csv("output/dynamic/mlp/predProb_val.csv") %>% mutate(model = "MLP") %>% slice((START -
                                                                                         168):(END - 168))

compare_pred <- rbind(pred_enet, pred_rf, pred_boost, pred_mlp)


# elastic net
p_pred_enet <-
  pred_enet %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type) , size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nU,
                  ymax = upper_nU),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "", y = "Building consumption (kWh)") +
  ggtitle("Elastic net") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# Random forest
p_pred_rf <- pred_rf %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type) , size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nU,
                  ymax = upper_nU),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "", y = "") +
  ggtitle("RF") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
    
  )

# Boosting
p_pred_boost <-
  pred_boost %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type) , size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nU,
                  ymax = upper_nU),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "Date", y = "Building consumption (kWh)") +
  ggtitle("GBM") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
    
  )

# MLP
p_pred_mlp <-
  pred_mlp %>% gather(., type, values, fitted:actual) %>%
  ggplot(aes(x = date, y = values, group = type)) +
  geom_line(aes(color = type, linetype = type) , size = .6, alpha = 0.7) +
  geom_ribbon(aes(ymin = lower_nU,
                  ymax = upper_nU),
              fill = "grey70",
              alpha = .2) +
  scale_linetype_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("dashed", "solid")
  ) +
  scale_colour_manual(
    labels = c("Actual value", "Forecasted value"),
    values = c("red", cbPalette[4])
  ) +
  labs(x = "Date", y = "") + ggtitle("MLP") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

(p_pred_enet + p_pred_rf) / (p_pred_boost + p_pred_mlp) +
  plot_layout(guides = 'collect') &
  plot_annotation(tag_levels = 'A') &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


ggsave(
  paste0("output/figures/chapter4/dynamic/figure3.", DEVICE),
  width = 30,
  height = 20,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)


# -------------------------------------------------------------------
# 3 PERFORMANCE TEST DATA
# -------------------------------------------------------------------

START <- 168 * 2
END <- START + 168
final_fixed <-
  read_csv("output/deterministic/mlp/predProb_test.csv") %>%
  as_tibble() %>% mutate(model = "MLP (fixed)") %>% 
  mutate(step_ahead = 1) %>% slice(START:END) %>% 
  dplyr::rename(
    lower_final = lower_nU,
    upper_final = upper_nU
  ) %>% select(date, fitted, actual, lower_final, upper_final, model, residual)

final_fixed %>% colnames()  
final_dyn <-
  read_csv("output/dynamic/mlp/predProb_test.csv") %>%
  as_tibble() %>% mutate(model = "MLP (dynamic)") %>% slice(START:END) %>%
  dplyr::rename(
    lower_final = lower_nC,
    upper_final = upper_nC
  ) %>% select(date, fitted, actual, lower_final, upper_final, model, residual)
final_dyn %>% colnames()
final_model <- rbind(final_fixed, final_dyn)


p1_final <- final_model %>%
  ggplot(., aes(x = date, y = fitted, color = model)) +
  geom_line(size = .6, aes(linetype = model))  +
  geom_ribbon(aes(ymin = lower_final,
                  ymax = upper_final,
                  fill = model),
              alpha = .2) +
  geom_line(
    aes(date, actual),
    final_dyn,
    color = "black",
    size = 1,
    linetype = "dotted"
  ) +
  scale_linetype_manual(values = c("dashed", "dashed")) +
  scale_colour_manual(values = c("red", cbPalette[3])) +
  labs(x = "", y = "Building consumption (kWh)")  +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
    
  )


p2_final <-
  final_model %>% ggplot(., aes(x = date, y = residual, color = model)) +
  geom_point(aes(color = model), size = .7) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_colour_manual(values = c("red", cbPalette[3])) +
  theme(legend.title = element_blank(),
        legend.position = "none") + labs(x = "Date", y = "Forecast error") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    size = .6
  )



(p1_final) / (p2_final) +
  plot_layout(heights = c(5, 3),
              ncol = 1,
              guides = 'collect') +
  plot_annotation(tag_levels = 'A') &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave(
  paste0("output/figures/chapter4/final model/figure4.",DEVICE),
  width = 20,
  height = 14,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)

# Diagnostic tool point forecast

# 1) fixed forecast horizon
final_fixed <-
  read_csv("output/deterministic/mlp/predProb_test.csv")

p1_point_fixed <- final_fixed %>% filter(actual <300) %>% 
  ggplot(aes(x = actual, y = fitted)) + geom_point(alpha = 0.5) +
  geom_abline(color = "gray", size = 1, linetype = "dashed") + 
  labs(x = "Observed measurement", y = "Fitted value") + 
  labs(x = "Fitted value", y = "Observed measurement") + ggtitle("A) MLP: fixed forecast horizon")

p2_point_fixed <- final_fixed %>% filter(actual <300) %>% ggplot(aes(x = fitted, y = residual)) + 
  geom_point(alpha = 0.5) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "gray", size=1) +
  labs(x = "Fitted value", y = "Residual")
  

p1_point_fixed + p2_point_fixed 


# 2) fixed forecast horizon
final_dyn <-
  read_csv("output/dynamic/mlp/predProb_test.csv")

p1_point_dyn <- final_dyn %>% filter(actual <300) %>% 
  ggplot(aes(x = actual, y = fitted)) + geom_point(alpha = 0.5) +
  geom_abline(color = "gray", size = 1, linetype = "dashed") + 
  labs(x = "Observed measurement", y = "Fitted value") + 
  labs(x = "Fitted value", y = "Observed measurement") + ggtitle("B) MLP: dynamic forecast horizon")

p2_point_dyn <- final_dyn %>% filter(actual <300) %>% ggplot(aes(x = fitted, y = residual)) + 
  geom_point(alpha = 0.5) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "gray", size=1) +
  labs(x = "Fitted value", y = "Residual")


((p1_point_fixed + p2_point_fixed) + plot_annotation(tag_levels = 'A'))  / (
  (p1_point_dyn + p2_point_dyn) + plot_annotation(tag_levels = 'B'))


ggsave(
  paste0("output/figures/appendix/diagnostic_point.","png"),
  width = 20,
  height = 20,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)



# Diagnostic tool probablistic forecasts: Assess reliability

# 1 fixed


# 1.1 PIT
samples_normal <-
  get_samples_normal(
    fitted = final_fixed$fitted,
    sd_resid = final_fixed$residual %>% sd %>% rep(nrow(final_fixed)),
    mu_resid = final_fixed$residual %>% mean %>% rep(nrow(final_fixed)),
    nr_samples = 5000
  )


pit_object <-
  pit(final_fixed$actual,
      samples_normal,
      num_bins = 10,
      full_output = TRUE)
nr_obs <- final_fixed %>% nrow

p <- pit_plot(pit_object = pit_object, nr_obs = nr_obs)
p


# 1.2 reliability diagram
rel_nd <-
  prep_rel_diagram_und(
    fitted = final_fixed$fitted,
    quantiles = seq(0.01, 0.99, 0.01),
    mu_resid = final_fixed$residual %>% sd %>% rep(nrow(final_fixed)),
    sd_resid = final_fixed$residual %>% sd %>% rep(nrow(final_fixed))
  )

reliability_diagram(
  q_hat = rel_nd,
  quantiles = seq(0.01, 0.99, 0.01),
  actuals = final_fixed$actual
) + ggtitle("MLP: dynamic forecast horizon")

# ggsave(
#   paste0("output/figures/chapter4/final model/figure6_Reliability_dyn.", DEVICE),
#   width = 15,
#   height = 10,
#   units = "cm",
#   dpi = DPI,
#   device = DEVICE
# )



# 2) dynamic

# 2.1 PIT
samples_normal <-
  get_samples_normal(
    fitted = final_dyn$fitted,
    sd_resid = final_dyn$residual %>% sd %>% rep(nrow(final_dyn)),
    mu_resid = final_dyn$residual %>% mean %>% rep(nrow(final_dyn)),
    nr_samples = 5000
  )


pit_object <-
  pit(final_dyn$actual,
      samples_normal,
      num_bins = 10,
      full_output = TRUE)
nr_obs <- final_dyn %>% nrow

p <- pit_plot(pit_object = pit_object, nr_obs = nr_obs)
p


# 2.2 reliability diagram

rel_nd <-
  prep_rel_diagram_und(
    fitted = final_dyn$fitted,
    quantiles = seq(0.01, 0.99, 0.01),
    mu_resid = final_dyn$residual %>% sd %>% rep(nrow(final_dyn)),
    sd_resid = final_dyn$residual %>% sd %>% rep(nrow(final_dyn))
  )

reliability_diagram(
  q_hat = rel_nd,
  quantiles = seq(0.01, 0.99, 0.01),
  actuals = final_dyn$actual
)

