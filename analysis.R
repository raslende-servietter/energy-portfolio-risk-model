# Packages
library(readxl)
library(readr)
library(dplyr)
library(moments)
library(ggplot2)

# Cleaning
######
# getting the data from the correct worksheet in the xls files and skipping the first two lines
# since they only contain meta data
brent <- read_excel(
  "Brent crude oil.xls",
  sheet = "Data 1",
  skip = 2
)
gas <- read_excel(
  "henry hub natural gas.xls",
  sheet = "Data 1",
  skip = 2
)

# European electricity prices
power <- read_csv("european_wholesale_electricity_price_data_daily.csv")

# selecting only the german prices from the power object
power <- rename(power, price = "Price (EUR/MWhe)")
power_de <- power |>
  filter(Country == "Germany") |>
  transmute(
    date = Date,
    power_price = price
  )

# cleaning the brent and gas objects
brent_clean <- brent |>
  transmute(
    date = as.Date(Date),
    brent_price = `Europe Brent Spot Price FOB (Dollars per Barrel)`
  )

gas_clean <- gas |>
  transmute(
    date = as.Date(Date),
    gas_price = `Henry Hub Natural Gas Spot Price (Dollars per Million Btu)`
  )


# restricting the objects to have the same start and end date
start_date <- as.Date("2019-01-01")
end_date   <- as.Date("2025-12-31")

power_clean <- power_de |>
  filter(date >= start_date,
         date <= end_date)

gas_clean <- gas_clean |>
  filter(date >= start_date,
         date <= end_date)

brent_clean <- brent_clean |>
  filter(date >= start_date,
         date <= end_date)

# joinging the objects into one
energy <- power_clean |>
  inner_join(gas_clean, by = "date") |>
  inner_join(brent_clean, by = "date") |>
  arrange(date)
#####
# EDA
######
# inspecting summaru statisics.
summary(energy[, c("power_price", "gas_price", "brent_price")])
sapply(
  energy[, c("power_price", "gas_price", "brent_price")],
  function(x) c(
    min = min(x),
    max = max(x),
    mean = mean(x),
    sd = sd(x)
  )
)
sum(energy$power_price <= 0)

# plotting all three series. 
par(mfrow = c(3, 1))

plot(
  energy$date,
  energy$power_price,
  type = "l",
  main = "German Power Price",
  xlab = "",
  ylab = "EUR/MWh"
)

plot(
  energy$date,
  energy$gas_price,
  type = "l",
  main = "Henry Hub Natural Gas Price",
  xlab = "",
  ylab = "USD/MMBtu"
)

plot(
  energy$date,
  energy$brent_price,
  type = "l",
  main = "Brent Crude Oil Price",
  xlab = "Date",
  ylab = "USD/barrel"
)

par(mfrow = c(1, 1))

# from the plots and the summary statistics we see that the series need to be transformed before
# the analysis.
energy <- energy |>
  mutate(
    power_change = power_price - lag(power_price),
    gas_return = log(gas_price / lag(gas_price)),
    brent_return = log(brent_price / lag(brent_price))
  )

# removing the first observation chis is now use missing because of the tranformation.
energy_returns <- energy |>
  filter(
    !is.na(power_change),
    !is.na(gas_return),
    !is.na(brent_return)
  )


# recomputing the summary statistics for the transformed series. we see that the 
# distributions have very fat tails. 
summary(
  energy_returns[, c(
    "power_change",
    "gas_return",
    "brent_return"
  )]
)

sapply(
  energy_returns[, c(
    "power_change",
    "gas_return",
    "brent_return"
  )],
  sd
)

quantile(
  energy_returns$power_change,
  probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)
)

quantile(
  energy_returns$gas_return,
  probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)
)

quantile(
  energy_returns$brent_return,
  probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1)
)

# calculation the correlationsfor the three series we see that they are quite weak. this gives 
# this gives justification for extending the analysi beyond simple gaussian VaR model. 
risk_factors <- energy_returns |>
  select(
    power_change,
    gas_return,
    brent_return
  )
cor(risk_factors)

# calculating the third and fourth moments for the series
sapply(risk_factors, skewness)
sapply(risk_factors, kurtosis)

# visualization
par(mfrow = c(3, 1))

hist(
  energy_returns$power_change,
  breaks = 60,
  probability = TRUE,
  main = "Daily German Power Price Changes",
  xlab = "EUR/MWh"
)

curve(
  dnorm(
    x,
    mean = mean(energy_returns$power_change),
    sd = sd(energy_returns$power_change)
  ),
  add = TRUE,
  lwd = 2
)


hist(
  energy_returns$gas_return,
  breaks = 60,
  probability = TRUE,
  main = "Henry Hub Daily Log Returns",
  xlab = "Log return"
)

curve(
  dnorm(
    x,
    mean = mean(energy_returns$gas_return),
    sd = sd(energy_returns$gas_return)
  ),
  add = TRUE,
  lwd = 2
)


hist(
  energy_returns$brent_return,
  breaks = 60,
  probability = TRUE,
  main = "Brent Daily Log Returns",
  xlab = "Log return"
)

curve(
  dnorm(
    x,
    mean = mean(energy_returns$brent_return),
    sd = sd(energy_returns$brent_return)
  ),
  add = TRUE,
  lwd = 2
)

par(mfrow = c(1, 1))


# we will redo the transformations since the distributions are as extreme as they are. 
rm(energy_changes
   )
energy_changes <- energy |>
  mutate(
    power_change = power_price - lag(power_price),
    gas_change   = gas_price - lag(gas_price),
    brent_change = brent_price - lag(brent_price)
  ) |>
  filter(
    !is.na(power_change),
    !is.na(gas_change),
    !is.na(brent_change)
  )

# redoing the jey summary statistics with the new transformations
risk_changes <- energy_changes |>
  select(
    power_change,
    gas_change,
    brent_change
  )

cor(risk_changes)

sapply(risk_changes, skewness)

sapply(risk_changes, kurtosis)

# cheking the most extreme observations and they appear to fall at plausible dates, menaning 
# they are likely to real reflections of what the market was like. it would thefore be a mistake
# to exclude them from the analysis. 
energy_changes |>
  arrange(desc(abs(power_change))) |>
  select(date, power_price, power_change) |>
  head(10)

energy_changes |>
  arrange(desc(abs(gas_change))) |>
  select(date, gas_price, gas_change) |>
  head(10)

energy_changes |>
  arrange(desc(abs(brent_change))) |>
  select(date, brent_price, brent_change) |>
  head(10)

#####
# Portfolio construction
######

# Target initial exposures
power_notional <- 4e6
gas_notional   <- 3e6
brent_notional <- 3e6

# Fixed USD -> EUR conversion
usd_eur <- 0.90

# Prices at beginning of modelling sample
power_start <- energy_changes$power_price[1]
gas_start   <- energy_changes$gas_price[1]
brent_start <- energy_changes$brent_price[1]

# Physical quantities giving approximately the target notionals
q_power <- power_notional / power_start

q_gas <- gas_notional / (gas_start * usd_eur)

q_brent <- brent_notional / (brent_start * usd_eur)

# Show portfolio quantities
c(
  q_power = q_power,
  q_gas = q_gas,
  q_brent = q_brent
)

# calculating daily P&L
energy_changes <- energy_changes |>
  mutate(
    power_pnl = q_power * power_change,
    
    gas_pnl = q_gas * gas_change * usd_eur,
    
    brent_pnl = q_brent * brent_change * usd_eur,
    
    portfolio_pnl = power_pnl + gas_pnl + brent_pnl
  )
# cheking the results
summary(energy_changes$portfolio_pnl)

sd(energy_changes$portfolio_pnl)

quantile(
  energy_changes$portfolio_pnl,
  probs = c(0.01, 0.05, 0.50, 0.95, 0.99)
)

#####
# Parametrics VaR and extec shortfall
######
mu_pnl <- mean(energy_changes$portfolio_pnl)
sigma_pnl <- sd(energy_changes$portfolio_pnl)

# 95% VaR
var_95_param <- -(
  mu_pnl +
    sigma_pnl * qnorm(0.05)
)

# 99% VaR
var_99_param <- -(
  mu_pnl +
    sigma_pnl * qnorm(0.01)
)

# Expected Shortfall under normality
es_95_param <- -(
  mu_pnl -
    sigma_pnl *
    dnorm(qnorm(0.05)) / 0.05
)

es_99_param <- -(
  mu_pnl -
    sigma_pnl *
    dnorm(qnorm(0.01)) / 0.01
)
#####
# Historical VaR
######
var_95_hist <- -quantile(
  energy_changes$portfolio_pnl,
  0.05
)

var_99_hist <- -quantile(
  energy_changes$portfolio_pnl,
  0.01
)

es_95_hist <- -mean(
  energy_changes$portfolio_pnl[
    energy_changes$portfolio_pnl <=
      quantile(energy_changes$portfolio_pnl, 0.05)
  ]
)

es_99_hist <- -mean(
  energy_changes$portfolio_pnl[
    energy_changes$portfolio_pnl <=
      quantile(energy_changes$portfolio_pnl, 0.01)
  ]
)

# now we put the resukts into a summary table
risk_summary <- data.frame(
  Model = c(
    "Parametric",
    "Historical"
  ),
  
  VaR_95 = c(
    var_95_param,
    var_95_hist
  ),
  
  ES_95 = c(
    es_95_param,
    es_95_hist
  ),
  
  VaR_99 = c(
    var_99_param,
    var_99_hist
  ),
  
  ES_99 = c(
    es_99_param,
    es_99_hist
  )
)

risk_summary
#####
# Rolling 99% backtest
######
window <- 500
n <- nrow(energy_changes)

# Storage
var99_param_roll <- rep(NA_real_, n)
var99_hist_roll  <- rep(NA_real_, n)

for (i in (window + 1):n) {
  
  # Previous 500 observations only
  pnl_window <- energy_changes$portfolio_pnl[
    (i - window):(i - 1)
  ]
  
  # Parametric VaR
  mu_window <- mean(pnl_window)
  sd_window <- sd(pnl_window)
  
  var99_param_roll[i] <- -(
    mu_window +
      sd_window * qnorm(0.01)
  )
  
  # Historical VaR
  var99_hist_roll[i] <- -unname(
    quantile(pnl_window, 0.01)
  )
}

# identifying breaches
energy_changes <- energy_changes |>
  mutate(
    var99_param = var99_param_roll,
    var99_hist = var99_hist_roll,
    
    breach_param =
      portfolio_pnl < -var99_param,
    
    breach_hist =
      portfolio_pnl < -var99_hist
  )

# calculating performance
backtest <- energy_changes |>
  filter(!is.na(var99_param))

n_backtest <- nrow(backtest)

param_breaches <- sum(backtest$breach_param)
hist_breaches  <- sum(backtest$breach_hist)

param_rate <- mean(backtest$breach_param)
hist_rate  <- mean(backtest$breach_hist)

backtest_summary <- data.frame(
  Model = c("Parametric", "Historical"),
  Observations = n_backtest,
  Breaches = c(param_breaches, hist_breaches),
  Expected_Breaches = 0.01 * n_backtest,
  Breach_Rate = c(param_rate, hist_rate)
)

backtest_summary

# also doing a Kupiec test
kupiec_test <- function(breaches, alpha = 0.01) {
  
  n <- length(breaches)
  x <- sum(breaches)
  
  p_hat <- x / n
  
  # Handle edge cases
  if (x == 0 || x == n) {
    return(c(
      breaches = x,
      breach_rate = p_hat,
      LR = NA,
      p_value = NA
    ))
  }
  
  LR <- -2 * (
    log((1 - alpha)^(n - x) * alpha^x) -
      log((1 - p_hat)^(n - x) * p_hat^x)
  )
  
  p_value <- 1 - pchisq(LR, df = 1)
  
  c(
    breaches = x,
    breach_rate = p_hat,
    LR = LR,
    p_value = p_value
  )
}

# running the test
kupiec_param <- kupiec_test(
  backtest$breach_param,
  alpha = 0.01
)

kupiec_hist <- kupiec_test(
  backtest$breach_hist,
  alpha = 0.01
)

kupiec_param
kupiec_hist

#####
# Plots
######
plot(
  backtest$date,
  backtest$portfolio_pnl,
  type = "l",
  xlab = "Date",
  ylab = "Daily P&L (€)",
  main = "99% VaR Backtest"
)

lines(
  backtest$date,
  -backtest$var99_param,
  lty = 2,
  lwd = 2
)

lines(
  backtest$date,
  -backtest$var99_hist,
  lty = 3,
  lwd = 2
)

legend(
  "bottomleft",
  legend = c(
    "Portfolio P&L",
    "Parametric 99% VaR",
    "Historical 99% VaR"
  ),
  lty = c(1, 2, 3),
  lwd = c(1, 2, 2),
  bty = "n"
)

#####
# stress test
######
# 1. Worst historical portfolio day
worst_day <- energy_changes |>
  arrange(portfolio_pnl) |>
  select(
    date,
    power_change,
    gas_change,
    brent_change,
    power_pnl,
    gas_pnl,
    brent_pnl,
    portfolio_pnl
  ) |>
  slice(1)

worst_day

# 2. Joint adverse 1% scenario

power_stress <- unname(
  quantile(energy_changes$power_change, 0.01)
)

gas_stress <- unname(
  quantile(energy_changes$gas_change, 0.01)
)

brent_stress <- unname(
  quantile(energy_changes$brent_change, 0.01)
)

# Calculate stressed P&L
power_stress_pnl <- q_power * power_stress

gas_stress_pnl <-
  q_gas * gas_stress * usd_eur

brent_stress_pnl <-
  q_brent * brent_stress * usd_eur

joint_stress_pnl <-
  power_stress_pnl +
  gas_stress_pnl +
  brent_stress_pnl

stress_summary <- data.frame(
  Risk_Factor = c(
    "German Power",
    "Henry Hub Gas",
    "Brent Crude",
    "Total Portfolio"
  ),
  
  Shock = c(
    power_stress,
    gas_stress,
    brent_stress,
    NA
  ),
  
  PnL_EUR = c(
    power_stress_pnl,
    gas_stress_pnl,
    brent_stress_pnl,
    joint_stress_pnl
  )
)

stress_summary
#####
# Model Comparison Table and Plots
######
### results table
final_results <- data.frame(
  Model = c("Parametric", "Historical"),
  
  VaR_95 = c(
    var_95_param,
    var_95_hist
  ),
  
  ES_95 = c(
    es_95_param,
    es_95_hist
  ),
  
  VaR_99 = c(
    var_99_param,
    var_99_hist
  ),
  
  ES_99 = c(
    es_99_param,
    es_99_hist
  ),
  
  Breaches_99 = c(
    param_breaches,
    hist_breaches
  ),
  
  Breach_Rate_99 = c(
    param_rate,
    hist_rate
  ),
  
  Kupiec_p_value = c(
    kupiec_param["p_value"],
    kupiec_hist["p_value"]
  )
)

# Remove inherited names
final_results[] <- lapply(final_results, unname)

final_results

# for a cleaner presentation
final_results |>
  mutate(
    VaR_95 = round(VaR_95),
    ES_95 = round(ES_95),
    VaR_99 = round(VaR_99),
    ES_99 = round(ES_99),
    Breach_Rate_99 = round(100 * Breach_Rate_99, 2),
    Kupiec_p_value = signif(Kupiec_p_value, 3)
  )

### PLot
var_plot <- ggplot(
  backtest,
  aes(x = date)
) +
  geom_line(
    aes(y = portfolio_pnl),
    linewidth = 0.3
  ) +
  geom_line(
    aes(y = -var99_param),
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_line(
    aes(y = -var99_hist),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  geom_point(
    data = backtest |> filter(breach_hist),
    aes(y = portfolio_pnl),
    size = 1.5
  ) +
  labs(
    title = "Rolling 99% Value-at-Risk Backtest",
    subtitle = "Realized portfolio P&L versus 500-day rolling VaR estimates",
    x = NULL,
    y = "Daily portfolio P&L (€)",
    caption = paste0(
      "Expected breach rate: 1% | ",
      "Parametric: ", round(100 * param_rate, 2), "% | ",
      "Historical: ", round(100 * hist_rate, 2), "%"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

var_plot

# Saving the plot for later use
ggsave(
  "var_backtest.png",
  var_plot,
  width = 10,
  height = 6,
  dpi = 300
)

### stress test results
stress_results <- data.frame(
  Scenario = c(
    "Joint empirical 1% downside shock",
    "Worst historical portfolio day"
  ),
  
  Portfolio_Loss_EUR = c(
    -joint_stress_pnl,
    -worst_day$portfolio_pnl
  )
)

stress_results

#####
# Saving results
######
dir.create("output", showWarnings = FALSE)

# model comparison tables
write.csv(
  final_results,
  "output/final_results.csv",
  row.names = FALSE
)


# stress results table
write.csv(
  stress_results,
  "output/stress_results.csv",
  row.names = FALSE
)


# detailed stress results
write.csv(
  stress_summary,
  "output/stress_summary_detailed.csv",
  row.names = FALSE
)







#####

