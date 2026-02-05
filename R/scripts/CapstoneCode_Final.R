# === 0. Install / load packages ===

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(forecast)
library(shiny)

# === 1. Load & clean ===
df <- read_excel("DCAMM Project Main Data.xlsx", sheet = 1)

# 1.1 Make names syntactically valid
names(df) <- make.names(names(df))

# 1.2 Parse dates (if present)
date_cols <- intersect(names(df),
                       c("vendor_invoice_date","service_from_date","service_to_date","acceptance_date"))
df[date_cols] <- lapply(df[date_cols], as_date)

# 1.3 Clean & convert numeric fields
df <- df %>%
  mutate(
    Expended         = as.numeric(Expended),
    TPC              = as.numeric(gsub("[^0-9\\.]", "", TPC)),
    TPC.95.          = as.numeric(gsub("[^0-9\\.]", "", TPC.95.)),
    Percent.TPC.95.  = as.numeric(gsub("[^0-9\\.]", "", X.TPC.95.)),
    SQFT             = as.numeric(gsub("[^0-9\\.]", "", SQFT))
  )

# 1.4 Compute project duration
if(all(c("service_from_date","service_to_date") %in% names(df))) {
  df <- df %>%
    mutate(DurationDays = as.numeric(service_to_date - service_from_date))
}

# === 2. High‐level summaries ===
cat("Dimensions: ", nrow(df), "rows ×", ncol(df), "cols\n\n")
cat("Missing‐value % per column:\n")
print(round(colMeans(is.na(df))*100, 1))
cat("\nNumeric summaries for key fields:\n")
print(summary(select(df, Expended, TPC, TPC.95., Percent.TPC.95., SQFT)))

# === 3. Top categories (first 5) ===
for(col in c("PayClass","Construction.Procurement","Construction.Type",
             "Agy.Name","City","County","Capital.Investment.Category")) {
  if(col %in% names(df)) {
    cat("\nTop 5 levels of", col, ":\n")
    print(df %>% count(.data[[col]]) %>% arrange(desc(n)) %>% head(5))
  }
}

# === 4. Exploratory plots ===

# 4a. Distribution of individual Expended amounts
ggplot(df, aes(x = Expended)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Expended", x = "Expended ($)", y = "Count") +
  theme_minimal()

# 4b. Distribution of Total Project Cost (TPC)
ggplot(df, aes(x = TPC)) +
  geom_histogram(bins = 30, fill = "forestgreen", color = "white") +
  labs(title = "Distribution of Total Project Cost (TPC)", x = "TPC ($)", y = "Count") +
  theme_minimal()

# 4c. Distribution of Total Project Cost 95% (TPC)
ggplot(df, aes(x = TPC.95.)) +
  geom_histogram(bins = 30, fill = "slateblue", color = "white") +
  labs(title = "Distribution of 95% Total Project Cost (TPC 95%)", x = "TPC ($)", y = "Count") +
  theme_minimal()

# 4d. Total Spend (“Expended”) vs. Building Size (SQFT)
ggplot(df, aes(x = SQFT, y = Expended)) +
  geom_point(alpha = 0.5) +
  labs(title = "Expended vs. Gross Square Feet",
       x = "Gross Sq. Ft.", y = "Expended ($)") +
  theme_minimal()

# 4e. Bar chart: total Expended by PayClass
df %>%
  group_by(PayClass) %>%
  summarise(Total = sum(Expended, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(PayClass, -Total), y = Total, fill = PayClass)) +
  geom_col(show.legend = FALSE) +
  scale_y_log10() +   # log scale to show small classes
  labs(title = "Total Expended by Pay Class (log scale)",
       x = "Pay Class", y = "Total Expended ($)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4f. Bar chart: count of projects by Procurement Type
if("Construction.Procurement" %in% names(df)) {
  ggplot(df, aes(x = Construction.Procurement)) +
    geom_bar(fill = "coral") +
    labs(title = "Project Count by Procurement Type",
         x = "Procurement Type", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 4g. Histogram of “%TPC 95%” (to see how far into the project most spending falls)
if("Percent.TPC.95." %in% names(df)) {
  ggplot(df, aes(x = Percent.TPC.95.)) +
    geom_histogram(bins = 30, fill = "orchid", color = "white") +
    labs(title = "Distribution of %TPC 95%", x = "% of TPC at 95% Spend", y = "Count") +
    theme_minimal()
}

# 4h. Service duration distribution (Days between service_from_date and service_to_date)
if(all(c("service_from_date","service_to_date") %in% names(df))) {
  df %>%
    mutate(DurationDays = as.numeric(service_to_date - service_from_date)) %>%
    ggplot(aes(DurationDays)) +
    geom_histogram(bins = 30, fill = "orange", color = "white") +
    labs(title = "Distribution of Service Duration (Days)", x = "Days", y = "Count") +
    theme_minimal()
}


# === 5. Clustering Similar Projects ===
# Prepare project-level summary features: Total Project Cost, Size, and Duration
proj_feats <- df %>%
  select(PID, TPC, SQFT, DurationDays) %>%
  drop_na() %>%
  distinct()

# Run k-means clustering on scaled features
set.seed(123)
km <- kmeans(scale(proj_feats %>% select(-PID)), centers = 4, nstart = 25)
proj_feats$Cluster <- factor(km$cluster)

# Merge cluster assignments back to the main dataframe
df <- df %>%
  left_join(proj_feats %>% select(PID, Cluster), by = "PID")

# Print cluster centroids (in scaled space)
cat("Cluster centroids (scaled features):\n")
print(km$centers)

# Plot projects by size vs. cost colored by cluster
ggplot(proj_feats, aes(x = SQFT, y = TPC, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Project Clusters by Size & Total Cost",
    x = "Gross Square Feet",
    y = "Total Project Cost ($)"
  ) +
  theme_minimal()

# 1A. Unscaled centers (in the scaled space)
print(km$centers)

# 1B. Raw‐value cluster summaries
cluster_summary <- proj_feats %>%
  group_by(Cluster) %>%
  summarise(
    N           = n(),
    Mean_TPC    = mean(TPC, na.rm=TRUE),
    Median_TPC  = median(TPC, na.rm=TRUE),
    Mean_SQFT   = mean(SQFT, na.rm=TRUE),
    Median_SQFT = median(SQFT, na.rm=TRUE),
    Mean_Days   = mean(DurationDays, na.rm=TRUE),
    Median_Days = median(DurationDays, na.rm=TRUE)
  ) %>%
  arrange(Cluster)

print(cluster_summary)





# === 6. Forecasting Prototype with Accuracy Metrics ===
library(forecast)

# your horizon
h <- 3

# identify projects with at least h+12 months of data
good_pids <- df %>%
  filter(!is.na(Project_Month), !is.na(Expended)) %>%
  count(PID) %>%
  filter(n >= (h + 12)) %>%
  pull(PID)

# pick a sample PID (first of the good ones)
sample_pid <- good_pids[1]

# build the full series for a sample project
full_vec <- df %>%
  filter(PID == sample_pid, !is.na(Project_Month), !is.na(Expended)) %>%
  arrange(Project_Month) %>%
  pull(Expended)

n <- length(full_vec)
if(n < (h+1)) stop("Not enough data points")

# split into numeric vectors
train_vec <- head(full_vec, n - h)
test_vec  <- tail(full_vec, h)

# cast the training part as a ts
train_ts <- ts(train_vec, frequency = 12)

# fit and forecast
fit_ets <- ets(train_ts)
fc      <- forecast(fit_ets, h = h)

# compute accuracy
acc <- accuracy(fc, test_vec)
rmse <- acc["Test set","RMSE"]
mape <- acc["Test set","MAPE"]
cat(sprintf("3-month RMSE = $%0.0f\n3-month MAPE = %0.1f%%\n", rmse, mape))

# plot actual vs forecast
time_index_train <- time(train_ts)
time_index_fc    <- seq(time_index_train[length(time_index_train)] + 1/12,
                        by = 1/12, length.out = h)

# 6g. Plot train/test/forecast with explicit data frames
# build small data frames for the forecast and test points
df_fc <- data.frame(
  x = time_index_fc,
  y = as.numeric(fc$mean),
  lo = fc$lower[,2],   # 95% lower
  hi = fc$upper[,2]    # 95% upper
)
df_test <- data.frame(
  x = time_index_fc,
  y = test_vec
)

autoplot(train_ts, series = "Train") +
  # forecast ribbon
  geom_ribbon(
    data    = df_fc,
    aes(x = x, ymin = lo, ymax = hi),
    fill    = "lightblue", alpha = 0.3,
    inherit.aes = FALSE
  ) +
  # forecast line
  geom_line(
    data    = df_fc,
    aes(x = x, y = y),
    color   = "blue", size = 1,
    inherit.aes = FALSE
  ) +
  # actual holdout points
  geom_point(
    data    = df_test,
    aes(x = x, y = y),
    color   = "red", size = 2,
    inherit.aes = FALSE
  ) +
  labs(
    title    = paste("ETS Forecast for Project", sample_pid),
    subtitle = sprintf("3-month RMSE=$%0.0f  MAPE=%0.1f%%", rmse, mape),
    x        = "Project Month (ts index)",
    y        = "Expended ($)"
  ) +
  scale_colour_manual(
    name   = "Series",
    values = c(Train = "black", Forecast = "blue", Actual = "red")
  ) +
  theme_minimal()




# End of clustering & forecast prototype code




library(dplyr)
library(forecast)
library(purrr)

h <- 3

# 1. Find all PIDs with at least (h + 12) months of data
good <- df %>%
  filter(!is.na(Project_Month), !is.na(Expended)) %>%
  group_by(PID) %>%
  filter(n() >= (h + 12)) %>%
  pull(PID) %>%
  unique()

# 2. Loop over each PID and produce a tiny forecast table
all_fc <- map_dfr(good, function(pid) {
  sub <- df %>%
    filter(PID == pid, !is.na(Project_Month), !is.na(Expended)) %>%
    arrange(Project_Month)
  
  n <- nrow(sub)
  if (n < (h + 1)) return(NULL)
  
  # split into train/test by row count
  train_df <- sub %>% slice_head(n = n - h)
  test_df  <- sub %>% slice_tail(n = h)
  
  # fit ETS on the train Expended series
  fit   <- ets( ts(train_df$Expended, frequency = 12) )
  fc    <- forecast(fit, h = h)
  
  # derive the next h Project_Month values
  last_month <- max(train_df$Project_Month)
  new_months <- last_month + seq_len(h)
  
  # return a tibble of PID, month, forecast, lower & upper
  tibble(
    PID           = pid,
    Project_Month = new_months,
    Forecast      = as.numeric(fc$mean),
    Lower95       = as.numeric(fc$lower[,2]),
    Upper95       = as.numeric(fc$upper[,2])
  )
})

# 3. Export for Tableau
write.csv(all_fc, "dccamm_forecasts_all.csv", row.names = FALSE)

