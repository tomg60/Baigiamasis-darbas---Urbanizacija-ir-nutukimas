
# 1. Bibliotekos

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forecast)
library(tseries)
library(tibble)


# ============================================================
# 2. Pradiniai nustatymai
# ============================================================

path_nut <- "data/duomenys_nutukimas.xlsx"
path_urb <- "data/urbanizacija_duom.xls"

conts <- c("Europe", "Africa", "Asia", "Americas")

country_map <- tibble(
  iso3 = c("LTU", "ITA", "ZAF", "EGY", "KEN", "JPN", "KOR", "IND", "USA", "MEX"),
  continent = c(
    "Europe", "Europe",
    "Africa", "Africa", "Africa",
    "Asia", "Asia", "Asia",
    "Americas", "Americas"
  )
)

weighted_mean_na <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

adf_safe <- function(x) {
  x <- na.omit(as.numeric(x))
  
  if (length(x) < 10) {
    return(tibble(adf_stat = NA_real_, p_value = NA_real_))
  }
  
  test <- tryCatch(
    suppressWarnings(adf.test(x)),
    error = function(e) NULL
  )
  
  if (is.null(test)) {
    tibble(adf_stat = NA_real_, p_value = NA_real_)
  } else {
    tibble(
      adf_stat = unname(test$statistic),
      p_value = test$p.value
    )
  }
}

adf_p <- function(x) {
  x <- na.omit(as.numeric(x))
  
  if (length(x) < 10) {
    return(NA_real_)
  }
  
  out <- tryCatch(
    suppressWarnings(adf.test(x)),
    error = function(e) NULL
  )
  
  if (is.null(out)) {
    NA_real_
  } else {
    as.numeric(out$p.value)
  }
}

choose_d <- function(p0, p1, p2, alpha = 0.05) {
  if (!is.na(p0) && p0 < alpha) return(0)
  if (!is.na(p1) && p1 < alpha) return(1)
  if (!is.na(p2) && p2 < alpha) return(2)
  return(2)
}

make_trend_table <- function(df, value_col, indicator_name) {
  df %>%
    filter(continent %in% conts, year >= 1990, year <= 2020) %>%
    group_by(continent) %>%
    group_modify(~{
      m <- lm(reformulate("year", response = value_col), data = .x)
      s <- summary(m)
      
      tibble(
        beta = coef(m)[["year"]],
        p_value = s$coefficients["year", "Pr(>|t|)"],
        r2 = s$r.squared
      )
    }) %>%
    ungroup() %>%
    mutate(
      indicator = indicator_name,
      beta = round(beta, 3),
      p_value = format.pval(p_value, digits = 3, eps = 1e-16),
      r2 = round(r2, 3)
    ) %>%
    select(indicator, continent, beta, p_value, r2)
}

make_adf_stat_table <- function(df, value_col, indicator_name) {
  df %>%
    filter(continent %in% conts, year >= 1990, year <= 2020) %>%
    arrange(continent, year) %>%
    group_by(continent) %>%
    group_modify(~{
      ts_x <- ts(.x[[value_col]], start = min(.x$year), frequency = 1)
      
      bind_rows(
        adf_safe(ts_x) %>% mutate(series = "level"),
        adf_safe(diff(ts_x, differences = 1)) %>% mutate(series = "diff1"),
        adf_safe(diff(ts_x, differences = 2)) %>% mutate(series = "diff2")
      )
    }) %>%
    ungroup() %>%
    mutate(
      indicator = indicator_name,
      adf_stat = round(adf_stat, 3),
      p_value = format.pval(p_value, digits = 3, eps = 1e-16)
    ) %>%
    select(indicator, continent, series, adf_stat, p_value)
}

make_adf_summary_table <- function(df, value_col, indicator_name) {
  df %>%
    filter(continent %in% conts, year >= 1990, year <= 2020) %>%
    arrange(continent, year) %>%
    group_by(continent) %>%
    summarise(
      p_level = adf_p(.data[[value_col]]),
      p_diff1 = adf_p(diff(.data[[value_col]], differences = 1)),
      p_diff2 = adf_p(diff(.data[[value_col]], differences = 2)),
      .groups = "drop"
    ) %>%
    mutate(
      d = mapply(choose_d, p_level, p_diff1, p_diff2),
      indicator = indicator_name,
      p_level = format.pval(p_level, digits = 3, eps = 1e-16),
      p_diff1 = format.pval(p_diff1, digits = 3, eps = 1e-16),
      p_diff2 = format.pval(p_diff2, digits = 3, eps = 1e-16)
    ) %>%
    select(indicator, continent, p_level, p_diff1, p_diff2, d)
}

make_ndiffs_table <- function(df, value_col, indicator_name) {
  df %>%
    filter(continent %in% conts, year >= 1990, year <= 2020) %>%
    arrange(continent, year) %>%
    group_by(continent) %>%
    group_modify(~{
      ts_x <- ts(.x[[value_col]], start = min(.x$year), frequency = 1)
      
      tibble(
        ndiffs_adf = ndiffs(ts_x, test = "adf"),
        ndiffs_kpss = ndiffs(ts_x, test = "kpss"),
        ndiffs_pp = ndiffs(ts_x, test = "pp")
      )
    }) %>%
    ungroup() %>%
    mutate(indicator = indicator_name) %>%
    select(indicator, continent, ndiffs_adf, ndiffs_kpss, ndiffs_pp)
}

make_outlier_points <- function(df, value_col, conts_keep) {
  df %>%
    filter(
      continent %in% conts_keep,
      year >= 1990,
      year <= 2020
    ) %>%
    arrange(continent, year) %>%
    group_by(continent) %>%
    group_modify(~{
      ts_x <- ts(.x[[value_col]], start = min(.x$year), frequency = 1)
      out <- tsoutliers(ts_x)
      
      if (length(out$index) > 0) {
        tibble(
          year = .x$year[out$index],
          value = .x[[value_col]][out$index]
        )
      } else {
        tibble(
          year = numeric(0),
          value = numeric(0)
        )
      }
    }) %>%
    ungroup()
}

analyze_continent <- function(df_all, value_col, indicator_name, y_label, cont_name, h = 10) {
  df <- df_all %>%
    filter(continent == cont_name) %>%
    arrange(year)
  
  ts_x <- ts(df[[value_col]], start = min(df$year), frequency = 1)
  
  # Tiesinis trendas
  m_trend <- lm(reformulate("year", response = value_col), data = df)
  s_trend <- summary(m_trend)
  
  trend_stats <- tibble(
    continent = cont_name,
    beta = coef(m_trend)[["year"]],
    p_value = s_trend$coefficients["year", "Pr(>|t|)"],
    r2 = s_trend$r.squared
  )
  
  # Galimi išskirtiniai taškai
  out <- tsoutliers(ts_x)
  
  # ARIMA modelis
  fit <- auto.arima(
    ts_x,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE
  )
  
  # Ljung-Box testas
  resid_fit <- na.omit(residuals(fit))
  max_lag <- length(resid_fit) - 1
  lb_lag <- min(10, max_lag)
  fit_df <- length(coef(fit))
  fit_df_for_test <- ifelse(lb_lag <= fit_df, max(0, lb_lag - 1), fit_df)
  
  lb_p <- tryCatch(
    Box.test(
      resid_fit,
      type = "Ljung-Box",
      lag = lb_lag,
      fitdf = fit_df_for_test
    )$p.value,
    error = function(e) NA_real_
  )
  
  # Prognozė
  fc <- forecast(fit, h = h)
  
  last_year <- max(df$year, na.rm = TRUE)
  last_y <- df %>% slice_tail(n = 1) %>% pull(.data[[value_col]])
  
  fc_df <- tibble(
    year = (last_year + 1):(last_year + h),
    mean = as.numeric(fc$mean),
    lo80 = as.numeric(fc$lower[, "80%"]),
    hi80 = as.numeric(fc$upper[, "80%"]),
    lo95 = as.numeric(fc$lower[, "95%"]),
    hi95 = as.numeric(fc$upper[, "95%"])
  )
  
  fc_line <- bind_rows(
    tibble(year = last_year, mean = last_y),
    fc_df %>% select(year, mean)
  )
  
  p_fc <- ggplot() +
    geom_line(data = df, aes(x = year, y = .data[[value_col]]), linewidth = 1) +
    geom_ribbon(data = fc_df, aes(x = year, ymin = lo95, ymax = hi95), alpha = 0.2) +
    geom_ribbon(data = fc_df, aes(x = year, ymin = lo80, ymax = hi80), alpha = 0.35) +
    geom_line(data = fc_line, aes(x = year, y = mean), linewidth = 1) +
    theme_minimal(base_size = 13) +
    labs(
      title = paste(indicator_name, "prognozė:", cont_name),
      subtitle = paste("Modelis:", as.character(fit)),
      x = "Metai",
      y = y_label
    )
  
  if (length(out$index) > 0) {
    df_out <- tibble(
      year = df$year[out$index],
      value = df[[value_col]][out$index]
    )
  } else {
    df_out <- tibble(
      year = numeric(0),
      value = numeric(0)
    )
  }
  
  p_out <- ggplot(df, aes(x = year, y = .data[[value_col]])) +
    geom_line(linewidth = 1) +
    geom_point(data = df_out, aes(x = year, y = value), size = 3, color = "red") +
    theme_minimal(base_size = 13) +
    labs(
      title = paste("Galimi išskirtiniai taškai:", cont_name),
      x = "Metai",
      y = y_label
    )
  
  list(
    continent = cont_name,
    data = df,
    ts = ts_x,
    trend_model = m_trend,
    trend_stats = trend_stats,
    outliers = out,
    arima = fit,
    ljung_box_p = lb_p,
    forecast = fc,
    plot_forecast = p_fc,
    plot_outliers = p_out
  )
}

make_diag_table <- function(res_list, indicator_name) {
  tibble(
    indicator = indicator_name,
    continent = names(res_list),
    model = sapply(res_list, \(z) as.character(z$arima)),
    ljung_box_p = sapply(res_list, \(z) z$ljung_box_p)
  ) %>%
    mutate(ljung_box_p = round(ljung_box_p, 3))
}

make_ic_table <- function(res_list, indicator_name) {
  tibble(
    indicator = indicator_name,
    continent = names(res_list),
    model = sapply(res_list, \(z) as.character(z$arima)),
    AIC = sapply(res_list, \(z) AIC(z$arima)),
    BIC = sapply(res_list, \(z) BIC(z$arima))
  ) %>%
    mutate(
      AIC = round(AIC, 3),
      BIC = round(BIC, 3)
    )
}

make_forecast_table <- function(res_list, indicator_name) {
  bind_rows(
    lapply(names(res_list), function(reg) {
      fc <- res_list[[reg]]$forecast
      last_year <- max(res_list[[reg]]$data$year)
      
      tibble(
        indicator = indicator_name,
        continent = reg,
        year = (last_year + 1):(last_year + length(fc$mean)),
        forecast = as.numeric(fc$mean),
        lo80 = as.numeric(fc$lower[, "80%"]),
        hi80 = as.numeric(fc$upper[, "80%"]),
        lo95 = as.numeric(fc$lower[, "95%"]),
        hi95 = as.numeric(fc$upper[, "95%"])
      )
    })
  ) %>%
    mutate(
      forecast = round(forecast, 2),
      lo80 = round(lo80, 2),
      hi80 = round(hi80, 2),
      lo95 = round(lo95, 2),
      hi95 = round(hi95, 2)
    )
}


# ============================================================
# 4. Nutukimo duomenų nuskaitymas ir paruošimas
# ============================================================

nut_raw <- read_excel(path_nut, sheet = "lytys", guess_max = 1e5)

nut <- nut_raw %>%
  mutate(
    year = as.integer(year),
    iso3 = as.character(iso3),
    country = as.character(country),
    obesity_pct = as.numeric(obesity_pct)
  ) %>%
  filter(
    year >= 1990,
    year <= 2020,
    iso3 %in% country_map$iso3
  )


if ("pop" %in% names(nut_raw)) {
  pop_df <- nut_raw %>%
    mutate(
      year = as.integer(year),
      iso3 = as.character(iso3),
      pop = as.numeric(pop)
    ) %>%
    filter(
      year >= 1990,
      year <= 2020,
      iso3 %in% country_map$iso3
    ) %>%
    select(iso3, year, pop) %>%
    distinct()
}

nut <- nut %>%
  mutate(pop = as.numeric(pop)) %>%
  select(-any_of("continent")) %>%
  left_join(country_map, by = "iso3") %>%
  filter(!is.na(continent))

missing_pop_nut <- sum(is.na(nut$pop))
print(paste("Trūkstamų pop reikšmių nutukimo duomenyse:", missing_pop_nut))

if (missing_pop_nut > 0) {
  warning("Nutukimo duomenyse yra trūkstamų pop reikšmių. Svertiniai vidurkiai gali būti paveikti.")
}


# ============================================================
# 5. Urbanizacijos duomenų nuskaitymas ir paruošimas
# ============================================================

urban_raw <- read_excel(path_urb, sheet = 1, .name_repair = "minimal")

year_cols <- grep("^\\d{4}$", names(urban_raw), value = TRUE)

urbans <- urban_raw %>%
  rename(
    country = `Country Name`,
    iso3 = `Country Code`
  ) %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "urban_pct"
  ) %>%
  mutate(
    year = as.integer(year),
    iso3 = as.character(iso3),
    urban_pct = as.numeric(urban_pct)
  )

urban <- urbans %>%
  filter(
    year >= 1990,
    year <= 2020,
    iso3 %in% country_map$iso3
  ) %>%
  left_join(pop_df, by = c("iso3", "year")) %>%
  left_join(country_map, by = "iso3") %>%
  filter(!is.na(continent))

missing_pop_urban <- sum(is.na(urban$pop))

# ============================================================
# 6. Grafikai pagal šalis
# ============================================================

p_obesity_countries <- ggplot(nut, aes(year, obesity_pct, color = country, group = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Suaugusiųjų nutukimas (BMI ≥ 30), 1990–2020",
    x = "Metai",
    y = "Nutukę (%)"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  theme_minimal(base_size = 13)

print(p_obesity_countries)

p_urban_countries <- ggplot(urban, aes(year, urban_pct, color = country, group = country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Urban population (% of total), 1990–2020",
    x = "Metai",
    y = "Urbanizacija (% gyventojų miestuose)"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  theme_minimal(base_size = 13)

print(p_urban_countries)


# ============================================================
# 7. Grafikai pagal regionus
# ============================================================

plot_region_obesity <- function(reg) {
  ggplot(
    filter(nut, continent == reg),
    aes(year, obesity_pct, color = country, group = country)
  ) +
    geom_line(linewidth = 1) +
    labs(
      title = paste("Suaugusiųjų nutukimas (BMI ≥ 30) —", reg),
      x = "Metai",
      y = "Nutukę (%)"
    ) +
    scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right")
}

plot_region_urban <- function(reg) {
  ggplot(
    filter(urban, continent == reg),
    aes(year, urban_pct, color = country, group = country)
  ) +
    geom_line(linewidth = 1) +
    labs(
      title = paste("Urban population (% of total) —", reg),
      x = "Metai",
      y = "Urbanizacija (%)"
    ) +
    scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right")
}

obesity_region_plots <- setNames(lapply(conts, plot_region_obesity), conts)
urban_region_plots <- setNames(lapply(conts, plot_region_urban), conts)

for (reg in conts) print(obesity_region_plots[[reg]])
for (reg in conts) print(urban_region_plots[[reg]])


# ============================================================
# 8. Svertiniai regionų vidurkiai
# ============================================================

nut_cont <- nut %>%
  group_by(continent, year) %>%
  summarise(
    obesity_mean = weighted_mean_na(obesity_pct, pop),
    .groups = "drop"
  )

urb_cont <- urban %>%
  group_by(continent, year) %>%
  summarise(
    urban_mean = weighted_mean_na(urban_pct, pop),
    .groups = "drop"
  )

p_obesity_cont <- ggplot(nut_cont, aes(year, obesity_mean, color = continent)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Nutukimas pagal pasirinktų šalių regionines grupes, 1990–2020",
    x = "Metai",
    y = "Nutukę (%)"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  theme_minimal(base_size = 13)

print(p_obesity_cont)

p_urban_cont <- ggplot(urb_cont, aes(year, urban_mean, color = continent)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Urbanizacija pagal pasirinktų šalių regionines grupes, 1990–2020",
    x = "Metai",
    y = "Urbanizacija (%)"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  theme_minimal(base_size = 13)

print(p_urban_cont)


# ============================================================
# 9. Tiesinio trendo įvertinimas
# ============================================================

trend_o_table <- make_trend_table(nut_cont, "obesity_mean", "Nutukimas")
trend_u_table <- make_trend_table(urb_cont, "urban_mean", "Urbanizacija")
trend_table <- bind_rows(trend_u_table, trend_o_table)

trend_o_table
trend_u_table
trend_table

p_o_trend <- ggplot(
  filter(nut_cont, continent %in% conts, year >= 1990, year <= 2020),
  aes(year, obesity_mean)
) +
  geom_line(linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~continent, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Nutukimo tiesinio trendo įvertinimas, 1990–2020",
    x = "Metai",
    y = "Nutukimas (%)"
  )

print(p_o_trend)

p_u_trend <- ggplot(
  filter(urb_cont, continent %in% conts, year >= 1990, year <= 2020),
  aes(year, urban_mean)
) +
  geom_line(linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~continent, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Urbanizacijos tiesinio trendo įvertinimas, 1990–2020",
    x = "Metai",
    y = "Urbanizacija (%)"
  )

print(p_u_trend)


# ============================================================
# 10. ADF stacionarumo testai ir diferencijavimo eilė
# ============================================================

adf_o_table <- make_adf_stat_table(nut_cont, "obesity_mean", "Nutukimas")
adf_u_table <- make_adf_stat_table(urb_cont, "urban_mean", "Urbanizacija")
adf_stat_table <- bind_rows(adf_u_table, adf_o_table)

adf_o_table
adf_u_table
adf_stat_table

ndiffs_o_table <- make_ndiffs_table(nut_cont, "obesity_mean", "Nutukimas")
ndiffs_u_table <- make_ndiffs_table(urb_cont, "urban_mean", "Urbanizacija")
ndiffs_table <- bind_rows(ndiffs_u_table, ndiffs_o_table)

ndiffs_o_table
ndiffs_u_table
ndiffs_table

adf_obesity_table <- make_adf_summary_table(
  df = nut_cont,
  value_col = "obesity_mean",
  indicator_name = "Nutukimas"
)

adf_urban_table <- make_adf_summary_table(
  df = urb_cont,
  value_col = "urban_mean",
  indicator_name = "Urbanizacija"
)

adf_both_table <- bind_rows(adf_urban_table, adf_obesity_table)

adf_obesity_table
adf_urban_table
adf_both_table


# ============================================================
# 11. ARIMA modeliai ir prognozės
# ============================================================

res_o <- setNames(
  lapply(conts, \(reg) analyze_continent(
    df_all = nut_cont,
    value_col = "obesity_mean",
    indicator_name = "Nutukimo",
    y_label = "Nutukimas (%)",
    cont_name = reg,
    h = 10
  )),
  conts
)

res_u <- setNames(
  lapply(conts, \(reg) analyze_continent(
    df_all = urb_cont,
    value_col = "urban_mean",
    indicator_name = "Urbanizacijos",
    y_label = "Urbanizacija (%)",
    cont_name = reg,
    h = 10
  )),
  conts
)

# Diagnostika: ARIMA modelis ir Ljung-Box p reikšmė.
diag_o_table <- make_diag_table(res_o, "Nutukimas")
diag_u_table <- make_diag_table(res_u, "Urbanizacija")
diag_table <- bind_rows(diag_u_table, diag_o_table)

diag_o_table
diag_u_table
diag_table

# AIC ir BIC.
ic_obesity <- make_ic_table(res_o, "Nutukimas")
ic_urban <- make_ic_table(res_u, "Urbanizacija")
ic_table <- bind_rows(ic_urban, ic_obesity)

ic_obesity
ic_urban
ic_table

# Prognozių lentelės.
forecast_o_table <- make_forecast_table(res_o, "Nutukimas")
forecast_u_table <- make_forecast_table(res_u, "Urbanizacija")
forecast_table <- bind_rows(forecast_u_table, forecast_o_table)

forecast_o_table
forecast_u_table
forecast_table

# Prognozių grafikai.
for (reg in conts) print(res_o[[reg]]$plot_forecast)
for (reg in conts) print(res_u[[reg]]$plot_forecast)

# Atskirų regionų išskirtinių taškų grafikai.
for (reg in conts) print(res_o[[reg]]$plot_outliers)
for (reg in conts) print(res_u[[reg]]$plot_outliers)


# ============================================================
# 12. Išskirtiniai taškai viename grafike
# ============================================================

o_out_pts <- make_outlier_points(nut_cont, "obesity_mean", conts)
u_out_pts <- make_outlier_points(urb_cont, "urban_mean", conts)

p_o_out <- ggplot(
  filter(nut_cont, continent %in% conts, year >= 1990, year <= 2020),
  aes(year, obesity_mean)
) +
  geom_line(linewidth = 1) +
  geom_point(data = o_out_pts, aes(year, value), size = 3, color = "red") +
  facet_wrap(~continent, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Nutukimas – galimi išskirtiniai taškai",
    x = "Metai",
    y = "Nutukimas (%)"
  )

print(p_o_out)

p_u_out <- ggplot(
  filter(urb_cont, continent %in% conts, year >= 1990, year <= 2020),
  aes(year, urban_mean)
) +
  geom_line(linewidth = 1) +
  geom_point(data = u_out_pts, aes(year, value), size = 3, color = "red") +
  facet_wrap(~continent, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Urbanizacija – galimi išskirtiniai taškai",
    x = "Metai",
    y = "Urbanizacija (%)"
  )

print(p_u_out)


# ============================================================
# 13. Urbanizacijos ir nutukimo ryšys


corr_df <- inner_join(
  urb_cont %>% filter(continent %in% conts, year >= 1990, year <= 2020),
  nut_cont %>% filter(continent %in% conts, year >= 1990, year <= 2020),
  by = c("continent", "year")
) %>%
  arrange(continent, year)

# 13.1. Pearsono koreliacija rodiklių lygiams.
pearson_levels <- corr_df %>%
  group_by(continent) %>%
  group_modify(~{
    ct <- cor.test(.x$urban_mean, .x$obesity_mean, method = "pearson")
    
    tibble(
      n = nrow(.x),
      r = unname(ct$estimate),
      p_value = ct$p.value
    )
  }) %>%
  ungroup() %>%
  mutate(
    r = round(r, 3),
    p_value = format.pval(p_value, digits = 3, eps = 1e-16)
  )

pearson_levels

# 13.2. Pearsono koreliacija metiniams pokyčiams.
corr_diff_df <- corr_df %>%
  group_by(continent) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    dU = urban_mean - lag(urban_mean),
    dO = obesity_mean - lag(obesity_mean)
  ) %>%
  filter(!is.na(dU), !is.na(dO)) %>%
  ungroup()

pearson_diff <- corr_diff_df %>%
  group_by(continent) %>%
  group_modify(~{
    ct <- cor.test(.x$dU, .x$dO, method = "pearson")
    
    tibble(
      n = nrow(.x),
      r = unname(ct$estimate),
      p_value = ct$p.value
    )
  }) %>%
  ungroup() %>%
  mutate(
    r = round(r, 3),
    p_value = format.pval(p_value, digits = 3, eps = 1e-16)
  )

pearson_diff

# 13.3. Ryšio grafikas rodiklių lygiams.
p_corr_levels <- ggplot(corr_df, aes(x = urban_mean, y = obesity_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~continent, ncol = 2, scales = "free") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Urbanizacijos ir nutukimo ryšys, 1990–2020 (Pearson)",
    x = "Urbanizacija, svertinis vidurkis (%)",
    y = "Nutukimas, svertinis vidurkis (%)"
  )

print(p_corr_levels)

# 13.4. Ryšio grafikas metiniams pokyčiams.
p_corr_diff <- ggplot(corr_diff_df, aes(x = dU, y = dO)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~continent, ncol = 2, scales = "free") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Metinių pokyčių ryšys: Δ urbanizacija ir Δ nutukimas (Pearson)",
    x = "Δ urbanizacija, proc. punktais per metus",
    y = "Δ nutukimas, proc. punktais per metus"
  )

print(p_corr_diff)

relation_lm_levels <- corr_df %>%
  group_by(continent) %>%
  group_modify(~{
    m <- lm(obesity_mean ~ urban_mean, data = .x)
    s <- summary(m)
    
    tibble(
      intercept = coef(m)[["(Intercept)"]],
      beta_urban = coef(m)[["urban_mean"]],
      p_value_beta = s$coefficients["urban_mean", "Pr(>|t|)"],
      r2 = s$r.squared
    )
  }) %>%
  ungroup() %>%
  mutate(
    intercept = round(intercept, 3),
    beta_urban = round(beta_urban, 3),
    p_value_beta = format.pval(p_value_beta, digits = 3, eps = 1e-16),
    r2 = round(r2, 3)
  )

relation_lm_levels

relation_lm_diff <- corr_diff_df %>%
  group_by(continent) %>%
  group_modify(~{
    m <- lm(dO ~ dU, data = .x)
    s <- summary(m)
    
    tibble(
      intercept = coef(m)[["(Intercept)"]],
      beta_dU = coef(m)[["dU"]],
      p_value_beta = s$coefficients["dU", "Pr(>|t|)"],
      r2 = s$r.squared
    )
  }) %>%
  ungroup() %>%
  mutate(
    intercept = round(intercept, 3),
    beta_dU = round(beta_dU, 3),
    p_value_beta = format.pval(p_value_beta, digits = 3, eps = 1e-16),
    r2 = round(r2, 3)
  )

relation_lm_diff


# ============================================================
# 14. Pagrindinių rezultatų objektai
# ============================================================
#
# trend_table          - urbanizacijos ir nutukimo tiesiniai trendai
# adf_stat_table       - ADF statistikos lentelė
# adf_both_table       - ADF p reikšmės ir pasirinkta diferencijavimo eilė d
# ndiffs_table         - ndiffs pagal ADF, KPSS ir PP
# diag_table           - ARIMA modeliai ir Ljung-Box p reikšmės
# ic_table             - ARIMA modelių AIC ir BIC
# forecast_table       - urbanizacijos ir nutukimo prognozės
# pearson_levels       - Pearsono koreliacija rodiklių lygiams
# pearson_diff         - Pearsono koreliacija metiniams pokyčiams
# relation_lm_levels   - papildoma regresinė ryšio analizė lygiams
# relation_lm_diff     - papildoma regresinė ryšio analizė pokyčiams
# ============================================================

