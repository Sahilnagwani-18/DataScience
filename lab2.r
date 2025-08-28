packages <- c(
  "dplyr", "ggplot2", "readr", "matrixStats",
  "e1071", "corrplot", "tidyr", "stringr",
  "lubridate", "forcats", "tibble", "purrr"
)

installed <- rownames(installed.packages())
for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p, repos = "https://cloud.r-project.org/")
  }
  library(p, character.only = TRUE)
}



library(tidyverse)
library(e1071)
library(corrplot)

df <- read.csv("C:/DataScience/yellow_tripdata_sample.csv")


print(head(df))

# --- 3. Data Inspection and Cleaning ---

# Get a summary of the data structure (equivalent to df.info())
glimpse(df)
# We can see there are numeric (dbl, int) and character (chr) datatypes,
# indicating both categorical and numerical data in the dataset.

# Check for null values in each column
# colSums(is.na(df)) counts NA values for each column.
cat("\nNull values per column:\n")
print(colSums(is.na(df)))
# We can see that there are no null values in this sample dataset.

# --- 4. Analysis of Numerical Columns ---

# Helper function to get the mode (R doesn't have a built-in mode function)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# --- 4.1 Passenger Count Analysis ---
cat("\n--- Analyzing Passenger Count Statistics ---\n")

# Calculate descriptive statistics for 'passenger_count'
min_passenger_count <- min(df$passenger_count, na.rm = TRUE)
max_passenger_count <- max(df$passenger_count, na.rm = TRUE)
avg_passenger_count <- mean(df$passenger_count, na.rm = TRUE)
sd_passenger_count <- sd(df$passenger_count, na.rm = TRUE)
median_passenger_count <- median(df$passenger_count, na.rm = TRUE)
mode_passenger_count <- get_mode(df$passenger_count)
variance_passenger_count <- var(df$passenger_count, na.rm = TRUE)

# Print the statistics
cat("Minimum passenger count:", min_passenger_count, "\n")
cat("Maximum passenger count:", max_passenger_count, "\n")
cat("Average passenger count:", avg_passenger_count, "\n")
cat("Standard deviation of passenger count:", sd_passenger_count, "\n")
cat("Median passenger count:", median_passenger_count, "\n")
cat("Mode passenger count:", mode_passenger_count, "\n")
cat("Variance passenger count:", variance_passenger_count, "\n")

# Histogram of Passenger Count
ggplot(df, aes(x = passenger_count)) +
  geom_histogram(binwidth = 1, boundary = 0.5, fill = "lightblue", color = "black") +
  labs(
    title = "Histogram of Passenger Count",
    x = "Passenger Count",
    y = "Frequency"
  ) +
  theme_minimal()

# Observations
cat("\nObservations from plots:\n")
cat("- Majorly there are more trips with 1-2 passengers.\n")
cat("- Fewer trips with higher passenger counts.\n")
cat("- The average passenger count is around 1.5.\n")


# Distribution Plot (Histogram with KDE)
ggplot(df, aes(x = passenger_count)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, boundary = 0.5, fill = "lightblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(
    title = "Distribution of Passenger Count",
    x = "Passenger Count",
    y = "Density"
  ) +
  theme_minimal()

# Boxplot of Passenger Count
ggplot(df, aes(y = passenger_count)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of Passenger Count",
    y = "Passenger Count"
  ) +
  coord_flip() + # Makes the boxplot horizontal
  theme_minimal()

# Violin plot for passenger count distribution
ggplot(df, aes(x = factor(1), y = passenger_count)) +
  geom_violin(fill = "skyblue", color = "black") +
  labs(title = "Passenger Count Distribution (Violin Plot)", 
       x = "", 
       y = "Passenger Count") +
  theme_minimal()


# --- 4.2 Trip Distance Analysis ---
cat("\n--- Analyzing Trip Distance Statistics ---\n")
trip_distance <- na.omit(df$trip_distance)
cat("Minimum trip distance:", min(trip_distance), "\n")
cat("Maximum trip distance:", max(trip_distance), "\n")
cat("Mean trip distance:", mean(trip_distance), "\n")
cat("Median trip distance:", median(trip_distance), "\n")
cat("Mode trip distance:", get_mode(trip_distance), "\n")
cat("Standard deviation:", sd(trip_distance), "\n")
cat("Variance:", var(trip_distance), "\n")
cat("Skewness:", skewness(trip_distance), "\n")
cat("Kurtosis:", kurtosis(trip_distance), "\n")
cat("Missing values:", sum(is.na(df$trip_distance)), "out of", nrow(df), "records\n")

# Histogram of Trip Distance (focus on the main range)
ggplot(df, aes(x = trip_distance)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  xlim(0, 20) +
  labs(
    title = "Histogram of Trip Distance",
    x = "Trip Distance (miles)",
    y = "Frequency"
  ) +
  theme_minimal()

# Boxplot of Trip Distance
ggplot(df, aes(x = trip_distance)) +
  geom_boxplot(fill = "lightgreen") +
  xlim(0, 20) +
  labs(
    title = "Box Plot of Trip Distance",
    x = "Trip Distance (miles)"
  ) +
  theme_minimal()


# --- 4.3 Fare Amount Analysis ---
cat("\n--- Analyzing Fare Amount Statistics ---\n")
fare_amount <- na.omit(df$fare_amount)
cat("Minimum fare amount:", min(fare_amount), "\n")
cat("Maximum fare amount:", max(fare_amount), "\n")
cat("Mean fare amount:", mean(fare_amount), "\n")
cat("Median fare amount:", median(fare_amount), "\n")
cat("Mode fare amount:", get_mode(fare_amount), "\n")
cat("Standard deviation:", sd(fare_amount), "\n")
cat("Variance:", var(fare_amount), "\n")
cat("Skewness:", skewness(fare_amount), "\n")
cat("Kurtosis:", kurtosis(fare_amount), "\n")
cat("Missing values:", sum(is.na(df$fare_amount)), "out of", nrow(df), "records\n")

# Histogram of Fare Amount
ggplot(df, aes(x = fare_amount)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  xlim(0, 50) +
  labs(
    title = "Histogram of Fare Amount",
    x = "Fare Amount (USD)",
    y = "Frequency"
  ) +
  theme_minimal()

# Boxplot of Fare Amount
ggplot(df, aes(x = fare_amount)) +
  geom_boxplot(fill = "lightgreen") +
  xlim(0, 50) +
  labs(
    title = "Box Plot of Fare Amount",
    x = "Fare Amount (USD)"
  ) +
  theme_minimal()


# --- 5. Analysis of Categorical Columns ---

# --- 5.1 Payment Type Analysis ---
# Bar Chart for Payment Type
ggplot(df, aes(x = factor(payment_type))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(
    title = "Bar Chart — Payment Type",
    x = "Payment Type (1=Card, 2=Cash, etc.)",
    y = "Count"
  ) +
  theme_minimal()

# Pie Chart for Payment Type
payment_counts <- as.data.frame(table(df$payment_type))
colnames(payment_counts) <- c("payment_type", "count")

ggplot(payment_counts, aes(x = "", y = count, fill = payment_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(count / sum(count) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Payment Type — Proportion",
    fill = "Payment Type"
  ) +
  theme_void()

# --- 5.2 RatecodeID Analysis ---
# Bar Chart for RatecodeID
ggplot(df, aes(x = factor(RatecodeID))) +
  geom_bar(fill = "lightpink", color = "black") +
  labs(
    title = "Bar Chart — RatecodeID",
    x = "RatecodeID",
    y = "Count"
  ) +
  theme_minimal()


# --- 6. Temporal Analysis (Time Series) ---
# Ensure datetime column is in the correct format
df$tpep_pickup_datetime <- as.POSIXct(df$tpep_pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Create hour and weekday columns
df$pickup_hour <- as.numeric(format(df$tpep_pickup_datetime, "%H"))
df$weekday <- weekdays(df$tpep_pickup_datetime)
df$is_weekend <- df$weekday %in% c("Saturday", "Sunday")

# --- 6.1 Trips per Day ---
daily_trips <- df %>%
  mutate(pickup_date = as.Date(tpep_pickup_datetime)) %>%
  group_by(pickup_date) %>%
  summarise(trip_count = n())

ggplot(daily_trips, aes(x = pickup_date, y = trip_count)) +
  geom_line(marker = 'o') +
  geom_point() +
  labs(
    title = "Trip Count per Day",
    x = "Date",
    y = "Trips"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d")

# --- 6.2 Trips per Hour ---
hourly_trips <- df %>%
  group_by(pickup_hour) %>%
  summarise(trip_count = n())

ggplot(hourly_trips, aes(x = pickup_hour, y = trip_count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(
    title = "Trip Count by Hour of Day",
    x = "Hour (0–23)",
    y = "Trips"
  ) +
  theme_minimal()

# --- 6.3 Fare Analysis by Hour ---
fare_by_hour <- df %>%
  group_by(pickup_hour) %>%
  summarise(
    mean_fare = mean(fare_amount, na.rm = TRUE),
    median_fare = median(fare_amount, na.rm = TRUE)
  )

# Mean Fare by Hour
ggplot(fare_by_hour, aes(x = pickup_hour, y = mean_fare)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(
    title = "Average Fare by Hour of Day",
    x = "Hour (0–23)",
    y = "Average Fare ($)"
  ) +
  theme_minimal()

# Median Fare by Hour
ggplot(fare_by_hour, aes(x = pickup_hour, y = median_fare)) +
  geom_line(marker = 'o') +
  geom_point() +
  labs(
    title = "Median Fare by Hour of Day",
    x = "Hour (0–23)",
    y = "Median Fare ($)"
  ) +
  theme_minimal()


# --- 7. Confidence Intervals ---
# Function to calculate 95% confidence interval for the mean
mean_ci_95 <- function(data) {
  # Drop NA values
  data <- na.omit(data)
  n <- length(data)
  if (n < 2) {
    return(c(Mean = NA, LowerCI = NA, UpperCI = NA))
  }
  # t.test() conveniently provides the CI
  test_result <- t.test(data)
  return(c(
    Mean = test_result$estimate,
    LowerCI = test_result$conf.int[1],
    UpperCI = test_result$conf.int[2],
    N = n
  ))
}

# Calculate CIs for target variables
ci_trip_distance <- mean_ci_95(df$trip_distance)
ci_fare_amount <- mean_ci_95(df$fare_amount)
ci_tip_amount <- mean_ci_95(df$tip_amount)

# Print results
cat("\n=== 95% Confidence Intervals (t-based) ===\n")
cat(sprintf("Trip Distance (miles): mean=%.3f, 95%% CI=(%.3f, %.3f)  [n=%d]\n",
            ci_trip_distance['Mean'], ci_trip_distance['LowerCI'], ci_trip_distance['UpperCI'], ci_trip_distance['N']))
cat(sprintf("Fare Amount ($): mean=%.3f, 95%% CI=(%.3f, %.3f)  [n=%d]\n",
            ci_fare_amount['Mean'], ci_fare_amount['LowerCI'], ci_fare_amount['UpperCI'], ci_fare_amount['N']))
cat(sprintf("Tip Amount ($): mean=%.3f, 95%% CI=(%.3f, %.3f)  [n=%d]\n",
            ci_tip_amount['Mean'], ci_tip_amount['LowerCI'], ci_tip_amount['UpperCI'], ci_tip_amount['N']))

# --- 8. Hypothesis Testing ---

# --- 8.1 One-sample t-test ---
# H0: mean tip_amount = 2
sample_tips <- na.omit(df$tip_amount)
ttest_one_sample <- t.test(sample_tips, mu = 2)

cat("\n=== One-sample t-test (Tip Amount vs $2) ===\n")
cat(sprintf("T-statistic: %.4f, P-value: %.4f\n", ttest_one_sample$statistic, ttest_one_sample$p.value))
if (ttest_one_sample$p.value < 0.05) {
  cat("Result: Reject H0 → The average tip amount is significantly different from $2.\n")
} else {
  cat("Result: Fail to reject H0 → The average tip amount is not significantly different from $2.\n")
}

# --- 8.2 Two-sample t-test ---
# H0: Mean fare for Credit Card (1) vs Cash (2) is equal
fare_credit <- df %>% filter(payment_type == 1) %>% pull(fare_amount) %>% na.omit()
fare_cash <- df %>% filter(payment_type == 2) %>% pull(fare_amount) %>% na.omit()

ttest_two_sample <- t.test(fare_credit, fare_cash) # Welch's t-test is default

cat("\n=== Two-sample t-test (Fare: Credit Card vs Cash) ===\n")
cat(sprintf("T-statistic: %.4f, P-value: %.4f\n", ttest_two_sample$statistic, ttest_two_sample$p.value))
if (ttest_two_sample$p.value < 0.05) {
  cat("Result: Reject H0 → The average fare amount differs significantly between payment methods.\n")
} else {
  cat("Result: Fail to reject H0 → No significant difference in average fare amount between payment methods.\n")
}

# --- 8.3 Chi-square Test of Independence ---
# H0: Payment type and RateCodeID are independent
contingency_table <- table(df$payment_type, df$RatecodeID)
chi2_test <- chisq.test(contingency_table)

cat("\n=== Chi-square Test of Independence (Payment Type vs RateCodeID) ===\n")
cat("Contingency Table:\n")
print(contingency_table)
cat(sprintf("Chi-square Statistic: %.4f, P-value: %.4f, Degrees of Freedom: %d\n",
            chi2_test$statistic, chi2_test$p.value, chi2_test$parameter))
if (chi2_test$p.value < 0.05) {
  cat("Result: Reject H0 → Payment type and RateCodeID are dependent.\n")
} else {
  cat("Result: Fail to reject H0 → No significant association between Payment type and RateCodeID.\n")
}

# --- 9. Correlation Analysis ---

# Helper function to describe correlation strength
describe_strength <- function(r) {
  ar <- abs(r)
  if (ar >= 0.8) return("very strong")
  if (ar >= 0.6) return("strong")
  if (ar >= 0.4) return("moderate")
  if (ar >= 0.2) return("weak")
  return("very weak/none")
}

# --- 9.1 Trip Distance vs. Fare Amount ---
# cor.test calculates correlation and p-value
pearson_dist_fare <- cor.test(df$trip_distance, df$fare_amount, method = "pearson")
spearman_dist_fare <- cor.test(df$trip_distance, df$fare_amount, method = "spearman")

cat("\n=== Trip distance vs Fare amount ===\n")
cat(sprintf("Pearson r = %.3f (p = %.3g) → %s linear relationship.\n",
            pearson_dist_fare$estimate, pearson_dist_fare$p.value, describe_strength(pearson_dist_fare$estimate)))
cat(sprintf("Spearman ρ = %.3f (p = %.3g) → %s monotonic relationship.\n",
            spearman_dist_fare$estimate, spearman_dist_fare$p.value, describe_strength(spearman_dist_fare$estimate)))

# --- 9.2 Fare Amount vs. Tip Amount ---
pearson_fare_tip <- cor.test(df$fare_amount, df$tip_amount, method = "pearson")
spearman_fare_tip <- cor.test(df$tip_amount, df$fare_amount, method = "spearman")

cat("\n=== Fare amount vs Tip amount ===\n")
cat(sprintf("Pearson r = %.3f (p = %.3g) → %s linear relationship.\n",
            pearson_fare_tip$estimate, pearson_fare_tip$p.value, describe_strength(pearson_fare_tip$estimate)))
cat(sprintf("Spearman ρ = %.3f (p = %.3g) → %s monotonic relationship.\n",
            spearman_fare_tip$estimate, spearman_fare_tip$p.value, describe_strength(spearman_fare_tip$estimate)))


# --- 9.3 Correlation Matrix and Heatmap ---
numeric_cols <- df %>%
  select(trip_distance, fare_amount, total_amount, tip_amount, extra, passenger_count)

# Compute Pearson correlation matrix
corr_mat <- cor(numeric_cols, method = "pearson", use = "complete.obs")

cat("\n=== Pearson Correlation Matrix (numeric features) ===\n")
print(round(corr_mat, 3))

# Heatmap using the corrplot package
corrplot(corr_mat,
         method = "color",       # Use color to represent correlation
         type = "upper",         # Show upper triangle
         addCoef.col = "black",  # Add correlation coefficients
         tl.col = "black",       # Text label color
         tl.srt = 45,            # Rotate text labels
         title = "Correlation Matrix (Pearson)",
         mar = c(0, 0, 1, 0))    # Adjust margins