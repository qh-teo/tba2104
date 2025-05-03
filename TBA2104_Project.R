# ── 0.  Load libraries ────────────────────────────────────────────────────────
library(tidyverse)   
library(readxl)      
library(lubridate)   
library(janitor)     
library(forecast)
library(ggplot2)
library(tibble)
library(dplyr)


food_sales   <- read_excel("Food_Main_Sales08042025.xlsx")            %>% clean_names()
daily_sales  <- read_excel("Daily_Main_Sales_Consolidated08042025.xlsx") %>% clean_names()
events_dates <- read_excel("event_dates_consolidated.xlsx") %>% clean_names()


# Data Transformation ───────────────

food_sales   <- food_sales   %>% 
  mutate(date = as_date(date), year = as.character(year))

daily_sales <- daily_sales %>% 
  filter(company == "X") %>%
  mutate(date = as_date(date)) %>%
  mutate(event = str_trim(event), year = as.character(year))

events_dates <- events_dates %>%
  filter(company == "X") %>%
  mutate(date = as_date(date)) %>%
  mutate(event = str_trim(event), year = as.character(year))

daily_enriched <- daily_sales %>%
  filter(company == "X") %>%
  left_join(events_dates, by = c("event", "date", "year", "company"))

daily_enriched <- daily_enriched %>%
  mutate(
    quarter = paste0("Q", quarter(date)),
    year_quarter = paste0(year(date), "-Q", quarter(date))
  ) #to add quarter for holts-winters modeling


food_enriched <- food_sales %>%             # same for food
 left_join(events_dates, by = c("event", "date", "year", "company"))


# ── 5.  Quick sense‑checks (optional but recommended) ────────────────────────
## rows in daily_sales that did NOT find a match in the calendar:
anti_join(daily_sales, events_dates, by = c("event", "date", "year")) %>% 
  count()                               # should be zero rows

## glimpse at the final tables
glimpse(daily_enriched)
#glimpse(food_enriched)
#write.csv(daily_enriched, "daily_enriched.csv", row.names = FALSE)


quarterly_data <- daily_enriched %>%
  filter(!is.na(date)) %>% 
  group_by(year = year(date), quarter = quarter(date)) %>%
  summarise(gross_profit = sum(gross_profit, na.rm = TRUE), .groups = "drop")


print(quarterly_data)


# Convert to time series (ts object) with frequency = 4 for quarterly
ts_data <- ts(quarterly_data$gross_profit, 
              start = c(min(quarterly_data$year), 
                        min(quarterly_data$quarter)), frequency = 4)


hw_model <- HoltWinters(ts_data)
plot(hw_model)
# Ensure correct ordering of quarters
quarterly_data <- daily_enriched %>%
  filter(!is.na(date)) %>%
  mutate(year = year(date),
         quarter = quarter(date)) %>%
  group_by(year, quarter) %>%
  summarise(gross_profit = sum(gross_profit, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, quarter)  # ensure correct time order

# Confirm structure
print(quarterly_data)

# Convert to time series with frequency = 4 (quarterly)
ts_data <- ts(
  data = quarterly_data$gross_profit,
  start = c(min(quarterly_data$year), min(quarterly_data$quarter)),
  frequency = 4
)

# Fit Holt-Winters Model
hw_model <- HoltWinters(ts_data)
plot(hw_model, main = "Holt-Winters Forecast - Quarterly Gross Profit")
n_total <- nrow(quarterly_data)
n_fitted <- nrow(hw_model$fitted)
pad_length <- n_total - n_fitted

# Pad fitted values with NA at the start
fitted_full <- c(rep(NA, pad_length), as.numeric(hw_model$fitted[,1]))

# Combine with original quarterly data
quarterly_data_plot <- quarterly_data %>%
  arrange(year, quarter) %>%
  mutate(
    year_quarter = paste0(year, "-Q", quarter),
    fitted = fitted_full
  )

# Plot using ggplot2
ggplot(quarterly_data_plot, aes(x = year_quarter)) +
  geom_line(aes(y = gross_profit, group = 1, color = "Actual"), size = 1.2) +
  geom_line(aes(y = fitted, group = 1, color = "Fitted"), linetype = "dashed", size = 1.1) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
  labs(
    title = "Holt-Winters Quarterly Gross Profit",
    x = "Quarter",
    y = "Net Sales",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(hw_model)

# Extract fitted values
fitted_vals <- fitted(hw_model)

# Plot actual vs fitted values
autoplot(ts_data, series = "Actual") +
  autolayer(fitted_vals[,1], series = "Fitted", linetype = "dashed") +
  labs(title = "Holt-Winters Quarterly Gross Profit", y = "Values", x = "Time") +
  theme_minimal()

# Calculate error metrics
actual <- ts_data
predicted <- fitted_vals[,1]

# RMSE
rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))

# MAPE
mape <- mean(abs((actual - predicted)/actual), na.rm = TRUE) * 100

# error metrics
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

#  Forecast ahead (e.g., next 4 periods) and plot forecasts
hw_forecast <- forecast(hw_model, h = 4)
autoplot(hw_forecast) +
  labs(title = "Holt-Winters Forecast", y = "Forecasted Value") +
  theme_minimal()

# Check residuals (lvl 1)
checkresiduals(hw_model)


# Compare to seasonal naïve
snaive_fit  <- snaive(ts_data, h = 4)
accuracy(hw_model)[,c("RMSE","MAPE")]
accuracy(snaive_fit)[,c("RMSE","MAPE")]

# Multiplicative alternative
hw_mult <- HoltWinters(ts_data, seasonal = "multiplicative")
accuracy(hw_mult)[,c("RMSE","MAPE")]

# ETS automatic
ets_fit <- ets(ts_data)
accuracy(ets_fit)[,c("RMSE","MAPE")]
