

# -----------------------------
# Prepare Phase
# -----------------------------

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# List of file names for the datasets to be read
daily_activity <-read.csv("dailyActivity_merged.csv")
daily_calories <-read.csv("dailyCalories_merged.csv")
daily_intenities <-read.csv("dailyIntensities_merged.csv")
daily_steps <-read.csv("dailySteps_merged.csv")
heartrate_sec <-read.csv("heartrate_seconds_merged.csv")
hourly_calories <-read.csv("hourlyCalories_merged.csv")
hourly_Intensities <-read.csv("hourlyIntensities_merged.csv")
hourly_steps <-read.csv("hourlySteps_merged.csv")
minuteCalorieNarrow <-read.csv("minuteCaloriesNarrow_merged.csv")
minuteCaloriesWide <-read.csv("minuteCaloriesWide_merged.csv")
minuteIntensitiesNarrow <-read.csv("minuteIntensitiesNarrow_merged.csv")
minuteIntensitiesWide <-read.csv("minuteIntensitiesWide_merged.csv")
minuteMETsNarrow <-read.csv("minuteMETsNarrow_merged.csv")
minuteSleep <-read.csv("minuteSleep_merged.csv")
minuteStepsNarrow <-read.csv("minuteStepsNarrow_merged.csv")
minuteStepsWide <-read.csv("minuteStepsWide_merged.csv")
sleepDay <-read.csv("sleepDay_merged.csv")
weightLogInfo <-read.csv("weightLogInfo_merged.csv")

# List of data frames
data_frames <- list(
  daily_activity = daily_activity,
  daily_calories = daily_calories,
  daily_intenities = daily_intenities,
  daily_steps = daily_steps,
  heartrate_sec = heartrate_sec,
  hourly_calories = hourly_calories,
  hourly_Intensities = hourly_Intensities,
  hourly_steps = hourly_steps,
  minuteCalorieNarrow = minuteCalorieNarrow,
  minuteCaloriesWide = minuteCaloriesWide,
  minuteIntensitiesNarrow = minuteIntensitiesNarrow,
  minuteIntensitiesWide = minuteIntensitiesWide,
  minuteMETsNarrow = minuteMETsNarrow,
  minuteSleep = minuteSleep,
  minuteStepsNarrow = minuteStepsNarrow,
  minuteStepsWide = minuteStepsWide,
  sleepDay = sleepDay,
  weightLogInfo = weightLogInfo
)
# Iterate over each data frame in the list
for (df_name in names(data_frames)) {
  cat("Data frame:", df_name, "\n")
  cat("Structure:\n")
  str(data_frames[[df_name]])
  cat("\n\n")
}


# Function to check NA and duplicates
check_na_duplicates <- function(df_name, df) {
  cat("NA values in", df_name, ":", sum(is.na(df)), "\n")
  cat("Duplicates in", df_name, ":", anyDuplicated(df), "\n")
}

# Loop through the list of data frames and apply the function
for (df_name in names(data_frames)) {
  check_na_duplicates(df_name, data_frames[[df_name]])
}

# -----------------------------
# Process Phase
# -----------------------------

#Removing Rows with Missing Values
hourly_steps <- na.omit(hourly_steps)
weightLogInfo <- na.omit(weightLogInfo)

#Removing Duplicates from hourly_steps
hourly_steps <- hourly_steps[!duplicated(hourly_steps), ]
#Removing Duplicates from minuteSleep
minuteSleep <- minuteSleep[!duplicated(minuteSleep), ]

#Verifying the Removal
cat("Duplicates in hourly_steps after removal:", anyDuplicated(hourly_steps), "\n")
cat("Duplicates in minuteSleep after removal:", anyDuplicated(minuteSleep), "\n")

# -----------------------------
# Analysis Phase
# -----------------------------

# 1. Data Preprocessing and Summary Statistics

# 1.1 Convert date columns to the Date format for time series analysis
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format="%m/%d/%Y")
sleepDay$SleepDay <- as.Date(sleepDay$SleepDay, format="%m/%d/%Y")

# 1.2 Calculate average daily metrics
average_daily_activity <- daily_activity %>%
  summarise(
    Average_Steps = mean(TotalSteps, na.rm = TRUE),
    Average_Active_Minutes = mean(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes, na.rm = TRUE),
    Average_Calories_Burned = mean(Calories, na.rm = TRUE)
  )
print(average_daily_activity)

# 1.3 Calculate average sleep metrics
average_sleep <- sleepDay %>%
  summarise(
    Average_Sleep_Duration = mean(TotalMinutesAsleep, na.rm = TRUE) / 60, 
    Average_Time_In_Bed = mean(TotalTimeInBed, na.rm = TRUE) / 60 
  )
print(average_sleep)

# 1.4 Calculate average heart rate
average_heart_rate <- heartrate_sec %>%
  summarise(
    Average_Heart_Rate = mean(Value, na.rm = TRUE)
  )
print(average_heart_rate)

# 1.5 Calculate average weight and BMI
average_weight_bmi <- weightLogInfo %>%
  summarise(
    Average_Weight_Kg = mean(WeightKg, na.rm = TRUE),
    Average_BMI = mean(BMI, na.rm = TRUE)
  )
print(average_weight_bmi)


# 2. Correlation Analysis

# 2.1 Calculate correlation between TotalSteps and Calories
correlation <- cor(daily_activity$TotalSteps, daily_activity$Calories, use = "complete.obs")
print(correlation)


# 3. Trend Analysis

# 3.0 Integrating Sleep and Activity Data
sleep_activity <- merge(sleepDay, daily_activity, by = "Id")

# 3.1 Analyzing Sleep Quality vs. Activity Levels
sleep_activity$SleepEfficiency <- sleep_activity$TotalMinutesAsleep / sleep_activity$TotalTimeInBed
activity_sleep_cor <- sleep_activity %>%
  summarise(
    Correlation_VeryActive_SleepEff = cor(VeryActiveMinutes, SleepEfficiency, use = "complete.obs"),
    Correlation_LightlyActive_SleepEff = cor(LightlyActiveMinutes, SleepEfficiency, use = "complete.obs")
  )
print(activity_sleep_cor)

# 3.2 Weekly Trends in Activity and Sleep Patterns
library(lubridate)
daily_activity$DayOfWeek <- wday(daily_activity$ActivityDate, label = TRUE)
weekly_activity_trends <- daily_activity %>%
  group_by(DayOfWeek) %>%
  summarise(
    AverageSteps = mean(TotalSteps, na.rm = TRUE)
  )
print(weekly_activity_trends)

weekly_sleep_trends <- sleepDay %>%
  mutate(DayOfWeek = wday(SleepDay, label = TRUE)) %>%
  group_by(DayOfWeek) %>%
  summarise(
    AverageSleepHours = mean(TotalMinutesAsleep, na.rm = TRUE) / 60
  )
print(weekly_sleep_trends)


# 4. Visualization

# 4.1 Daily Step Count Trend Analysis
ggplot(daily_activity, aes(x = ActivityDate, y = TotalSteps)) +
  geom_line() +
  labs(title = "Total Steps per Day Over Time", x = "Date", y = "Total Steps") +
  theme_minimal()

# 4.2 Sleep Duration Distribution Analysis
ggplot(sleepDay, aes(x = TotalMinutesAsleep)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Minutes Asleep", x = "Total Minutes Asleep", y = "Frequency") +
  theme_minimal()

# 4.3 Weekday vs. Weekend Activity and Calorie Consumption Comparison
daily_activity$DayType <- ifelse(weekdays(daily_activity$ActivityDate) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
activity_summary <- aggregate(cbind(TotalSteps, Calories) ~ DayType, data = daily_activity, FUN = mean)

activity_melted <- melt(daily_activity, id.vars = "DayType", measure.vars = c("TotalSteps", "Calories"))
ggplot(activity_melted, aes(x = DayType, y = value, fill = variable)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Distribution of Steps and Calories by Day Type", x = "Day Type", y = "Value") +
  scale_fill_manual(values = c("TotalSteps" = "lightblue", "Calories" = "salmon")) +
  theme_minimal() +
  theme(legend.position = "bottom")


# 5. Analysis and Modeling

# 5.1 Categorizing Activity Levels
sleep_activity$ActivityLevel <- cut(sleep_activity$TotalSteps, 
                                    breaks = quantile(sleep_activity$TotalSteps, probs = 0:3/3), 
                                    include.lowest = TRUE, labels = c("Low", "Medium", "High"))

# 5.2 Sleep Duration Variation by Activity Level
ggplot(sleep_activity, aes(x = ActivityLevel, y = TotalMinutesAsleep)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  labs(title = "Violin Plot of Sleep Duration by Activity Level", x = "Activity Level", y = "Total Minutes Asleep") +
  theme_minimal()

# 5.3 Hourly Activity Analysis
hourly_steps_summary <- hourly_steps %>%
  mutate(ActivityHour = mdy_hms(ActivityHour),
         Hour = hour(ActivityHour)) %>%
  group_by(Hour) %>%
  summarise(Average_Steps = mean(StepTotal, na.rm = TRUE))
ggplot(hourly_steps_summary, aes(x = Hour, y = Average_Steps)) +
  geom_line() +
  labs(title = "Average Steps by Hour of Day", x = "Hour of Day", y = "Average Steps") +
  theme_minimal()

# 5.4 Visualizing Weekly Activity Patterns
ggplot(weekly_activity_trends, aes(x = DayOfWeek, y = AverageSteps, fill = DayOfWeek)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  labs(title = "Weekly Activity Trends", x = "Day of the Week", y = "Average Steps") +
  theme(legend.title = element_blank())

# 5.5 Analyzing Weekly Sleep Duration Trends
ggplot(weekly_sleep_trends, aes(x = DayOfWeek, y = AverageSleepHours)) +
  geom_line(group = 1, color = "darkgreen") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(title = "Weekly Sleep Trends", x = "Day of the Week", y = "Average Sleep Hours (in hours)")

# 5.6 Trend Analysis Over Time
daily_steps <- aggregate(TotalSteps ~ ActivityDate, data = daily_activity, FUN = mean)
ggplot(daily_steps, aes(x = ActivityDate, y = TotalSteps)) +
  geom_line() +
  labs(title = "Daily Steps Trend Over Time", x = "Date", y = "Average Daily Steps") +
  theme_minimal()

# 5.7 Prepare data and build a model to predict TotalSteps
model_data <- na.omit(sleep_activity[,c("TotalSteps", "VeryActiveMinutes", "LightlyActiveMinutes", "TotalMinutesAsleep")])
model <- lm(TotalSteps ~ VeryActiveMinutes + LightlyActiveMinutes + TotalMinutesAsleep, data = model_data)
summary(model)

