# GDP Reconciliation
library(fpp3)
library(tidyverse)
library(readxl)
library(distributional)
library(zoo)

#### Importing & Processing ####

## Import Excel
excel <- read_excel("inputs/QNA.xlsx")

## Cleaning
df <- excel %>%
  select(2:ncol(excel)) %>%
  .[c(6:26),]

# Column names
yq_vec <- df[1,2:ncol(df)] %>%
  slice() %>%
  as.numeric() %>%
  .[!is.na(.)] %>%
  ts(., start = c(2003, 1), frequency = 4) %>%
  time() %>%
  as.yearqtr() %>%
  as.character()

colnames(df) <- c("Variable", yq_vec)

df <- df %>%
  select(c("Variable", all_of(yq_vec))) %>%
  pivot_longer(cols = !"Variable", names_to = "Period", values_to = "Value") %>%
  mutate(Period = yearquarter(Period),
         Value = as.numeric(Value)) %>%
  filter(!is.na(Value)) %>%
  relocate(Period, .before = Variable) %>%
  pivot_wider(id_cols = Period, names_from = Variable, values_from = Value)

colnames(df) <- colnames(df) %>% str_replace_all(., "[:blank:]", "_") %>%
  str_replace_all(., "[\\,\\&]", "") %>% str_to_lower()


## Creating a tsibble object
qdf <- df %>%
  mutate(construction_realestate = construction + real_estate,
         publicadmin_health_educ = public_administration + human_health_and_social_work_activities + education,
         miscellaneous = agriculture + manufacturing + electricity_and_water + professional_scientific_and_technical_activities +
           entertainment_recreation__other_services) %>%
  select(period, taxes_less_subsidies, fisheries, construction_realestate, wholesale_and_retail_trade, tourism,
         transportation_and_communication, financial_services, publicadmin_health_educ, miscellaneous) %>%
  pivot_longer(cols = !"period", names_to = "variable", values_to = "value")

adf <- qdf %>%
  mutate(period = year(period)) %>%
  group_by(period, variable) %>%
  summarise(value = sum(value), .groups = "drop")

# Ensuring that the most recent year is complete. If not complete, remove it from the annual data frame.
latest_year <- adf %>% distinct(period) %>% pull() %>% max()
quarters_in_year <- qdf %>% filter(str_detect(period, as.character(latest_year))) %>% distinct(period) %>%
  pull(period) %>% length()

if (quarters_in_year < 4) {
  
  adf <- adf %>% filter(period != latest_year)
  
}

# Transforming to tsibble
qtsib <- qdf %>% tsibble(index = period, key = variable) %>% aggregate_key(variable, value = sum(value))
atsib <- adf %>% tsibble(index = period, key = variable) %>% aggregate_key(variable, value = sum(value))


#### Reconciled forecasts ####

# Annual
fit_comb <- atsib %>%
  model(ARIMA = ARIMA(log(value)),
        ETS = ETS(log(value))) %>%
  mutate(COMB = (ARIMA + ETS)/2) %>%
  reconcile(
    ARIMA_bu = bottom_up(ARIMA),
    ARIMA_ols = min_trace(ARIMA, "ols"),
    ARIMA_wls = min_trace(ARIMA, "wls_var"),
    ARIMA_mint = min_trace(ARIMA, "mint_shrink"),
    ETS_bu = bottom_up(ETS),
    ETS_ols = min_trace(ETS, "ols"),
    ETS_wls = min_trace(ETS, "wls_var"),
    ETS_mint = min_trace(ETS, "mint_shrink"),
    COMB_bu = bottom_up(COMB),
    COMB_ols = min_trace(COMB, "ols"),
    COMB_wls = min_trace(COMB, "wls_var"),
    COMB_mint = min_trace(COMB, "mint_shrink"),
  )

fc_comb <- fit_comb %>% forecast(h = "2 years")

fc_comb %>%
  filter(is_aggregated(variable)) %>%
  autoplot(level = NULL)

fc_comb %>%
  filter(.model == "ETS_wls") %>%
  arrange(desc(variable), period) %>%
  as_tibble() %>%
  pivot_wider(id_cols = variable, names_from = period, values_from = .mean)

val <- atsib %>% filter(is_aggregated(variable) & period == 2020) %>% pull(value)

fc_comb %>%
  filter(is_aggregated(variable) & period == 2021) %>%
  arrange(.mean) %>%
  as_tibble() %>%
  mutate(growth = (.mean/val-1)*100)

fc_comb %>%
  as_tibble() %>%
  filter(.model == "ETS_mint") %>%
  select(period, variable, .mean) %>%
  rename(value = .mean) %>%
  bind_rows(as_tibble(atsib), .) %>%
  filter(period >= 2016) %>%
  pivot_wider(id_cols = variable, names_from = period, values_from = value)

# Quarterly
fit_comb_q <- qtsib %>%
  model(ARIMA = ARIMA(log(value)),
        ETS = ETS(log(value))) %>%
  mutate(COMB = (ARIMA + ETS)/2) %>%
  reconcile(
    ARIMA_bu = bottom_up(ARIMA),
    ARIMA_ols = min_trace(ARIMA, "ols"),
    ARIMA_wls = min_trace(ARIMA, "wls_var"),
    ARIMA_mint = min_trace(ARIMA, "mint_shrink"),
    ETS_bu = bottom_up(ETS),
    ETS_ols = min_trace(ETS, "ols"),
    ETS_wls = min_trace(ETS, "wls_var"),
    ETS_mint = min_trace(ETS, "mint_shrink"),
    COMB_bu = bottom_up(COMB),
    COMB_ols = min_trace(COMB, "ols"),
    COMB_wls = min_trace(COMB, "wls_var"),
    COMB_mint = min_trace(COMB, "mint_shrink"),
  )

fc_comb_q <- fit_comb_q %>% forecast(h = "5 year")

fc_comb_q %>%
  filter(is_aggregated(variable)) %>%
  autoplot(level = NULL)

fc_comb_q %>%
  filter(.model == "ETS_wls") %>%
  arrange(desc(variable), period)

fc_comb_q %>%
  filter(is_aggregated(variable) & str_detect(period, "2021")) %>%
  as_tibble() %>%
  mutate(period = year(period)) %>%
  group_by(.model, period, variable) %>%
  summarise(.mean = sum(.mean)) %>%
  mutate(growth = (.mean/val-1)*100)

models <- fc_comb_q %>% pull(.model) %>% unique()

fc_list <- list()

for (m in models) {
  
  fc_list[[m]] <- fc_comb_q %>%
    filter(is_aggregated(variable) & str_detect(period, "2021") & .model == m) %>%
    as_tibble() %>%
    select(period, .mean) %>%
    rename(value = .mean) %>%
    bind_rows(qtsib %>% filter(str_detect(period, "2021") & is_aggregated(variable)) %>%
                as_tibble() %>% select(period, value), .) %>%
    mutate(model = m)
  
}

bind_rows(fc_list) %>%
  group_by(model) %>%
  summarise(value = sum(value)) %>%
  arrange(value)


#### Model evaluation ####
# Quarterly
eval_df_qtr <- qtsib %>%
  stretch_tsibble(.init = 64, .step = 1) %>%
  relocate(.id)

fc_qtr <- eval_df_qtr %>%
  model(ARIMA = ARIMA(value),
        ETS = ETS(value),
        ARIMA_log = ARIMA(log(value)),
        ETS_log = ETS(log(value))) %>%
  mutate(COMB = (ARIMA + ETS)/2,
         COMB_log = (ARIMA_log + ETS_log)/2) %>%
  reconcile(
    ARIMA_bu = bottom_up(ARIMA),
    ARIMA_ols = min_trace(ARIMA, "ols"),
    ARIMA_wls = min_trace(ARIMA, "wls_var"),
    ARIMA_mint = min_trace(ARIMA, "mint_shrink"),
    ETS_bu = bottom_up(ETS),
    ETS_ols = min_trace(ETS, "ols"),
    ETS_wls = min_trace(ETS, "wls_var"),
    ETS_mint = min_trace(ETS, "mint_shrink"),
    COMB_bu = bottom_up(COMB),
    COMB_ols = min_trace(COMB, "ols"),
    COMB_wls = min_trace(COMB, "wls_var"),
    COMB_mint = min_trace(COMB, "mint_shrink"),
    ARIMA_log_bu = bottom_up(ARIMA_log),
    ARIMA_log_ols = min_trace(ARIMA_log, "ols"),
    ARIMA_log_wls = min_trace(ARIMA_log, "wls_var"),
    ARIMA_log_mint = min_trace(ARIMA_log, "mint_shrink"),
    ETS_log_bu = bottom_up(ETS_log),
    ETS_log_ols = min_trace(ETS_log, "ols"),
    ETS_log_wls = min_trace(ETS_log, "wls_var"),
    ETS_log_mint = min_trace(ETS_log, "mint_shrink"),
    COMB_log_bu = bottom_up(COMB_log),
    COMB_log_ols = min_trace(COMB_log, "ols"),
    COMB_log_wls = min_trace(COMB_log, "wls_var"),
    COMB_log_mint = min_trace(COMB_log, "mint_shrink"),
  ) %>%
  forecast(h = 1)

fc_df_qtr <- fc_qtr %>%
  group_by(.id, variable, .model) %>%
  mutate(h = row_number()) %>%
  ungroup()

fc_df_qtr %>%
  filter(period > yearquarter("2020 Q3") & period <= yearquarter("2021 Q1")) %>%
  accuracy(qtsib) %>%
  filter(is_aggregated(variable)) %>%
  arrange(RMSE)

full_join(fc_df_qtr,
          qtsib %>% rename(test_val = value),
          by = c("variable", "period")) %>%
  mutate("Error" = (test_val - .mean)^2) %>%
  # filter(!is_aggregated(variable)) %>%
  as_tibble() %>%
  group_by(.model) %>%
  summarise(RMSE = sqrt(mean(Error, na.rm = TRUE))) %>%
  arrange(RMSE)


# Annual
eval_df_ann <- atsib %>%
  stretch_tsibble(.init = 16, .step = 1) %>%
  relocate(.id)

fc_ann <- eval_df_ann %>%
  model(ARIMA = ARIMA(value),
        ETS = ETS(value),
        ARIMA_log = ARIMA(log(value)),
        ETS_log = ETS(log(value))) %>%
  mutate(COMB = (ARIMA + ETS)/2,
         COMB_log = (ARIMA_log + ETS_log)/2) %>%
  reconcile(
    ARIMA_bu = bottom_up(ARIMA),
    ARIMA_ols = min_trace(ARIMA, "ols"),
    ARIMA_wls = min_trace(ARIMA, "wls_var"),
    ARIMA_mint = min_trace(ARIMA, "mint_shrink"),
    ETS_bu = bottom_up(ETS),
    ETS_ols = min_trace(ETS, "ols"),
    ETS_wls = min_trace(ETS, "wls_var"),
    ETS_mint = min_trace(ETS, "mint_shrink"),
    COMB_bu = bottom_up(COMB),
    COMB_ols = min_trace(COMB, "ols"),
    COMB_wls = min_trace(COMB, "wls_var"),
    COMB_mint = min_trace(COMB, "mint_shrink"),
    ARIMA_log_bu = bottom_up(ARIMA_log),
    ARIMA_log_ols = min_trace(ARIMA_log, "ols"),
    ARIMA_log_wls = min_trace(ARIMA_log, "wls_var"),
    ARIMA_log_mint = min_trace(ARIMA_log, "mint_shrink"),
    ETS_log_bu = bottom_up(ETS_log),
    ETS_log_ols = min_trace(ETS_log, "ols"),
    ETS_log_wls = min_trace(ETS_log, "wls_var"),
    ETS_log_mint = min_trace(ETS_log, "mint_shrink"),
    COMB_log_bu = bottom_up(COMB_log),
    COMB_log_ols = min_trace(COMB_log, "ols"),
    COMB_log_wls = min_trace(COMB_log, "wls_var"),
    COMB_log_mint = min_trace(COMB_log, "mint_shrink"),
  ) %>%
  forecast(h = 1)

fc_df_ann <- fc_ann %>%
  group_by(.id, variable, .model) %>%
  mutate(h = row_number()) %>%
  ungroup()

fc_df_ann %>%
  filter(period <= 2020) %>%
  accuracy(atsib) %>%
  filter(is_aggregated(variable)) %>%
  arrange(RMSE)

full_join(fc_df_ann,
          atsib %>% rename(test_val = value),
          by = c("variable", "period")) %>%
  mutate("Error" = (test_val - .mean)^2) %>%
  # filter(!is_aggregated(variable)) %>%
  as_tibble() %>%
  group_by(.model) %>%
  summarise(RMSE = sqrt(mean(Error, na.rm = TRUE))) %>%
  arrange(RMSE)
