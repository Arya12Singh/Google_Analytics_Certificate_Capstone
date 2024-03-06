# GACapstone

Data Preprocessing and Summary Statistics

1.1 Date Format Conversion

daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format="%m/%d/%Y")
sleepDay$SleepDay <- as.Date(sleepDay$SleepDay, format="%m/%d/%Y")

Purpose: This step is essential for preparing the datasets for time series analysis, enabling us to examine trends over time accurately.

Insight Gained: By converting date columns to the correct format, we established a baseline for temporal analysis, crucial for understanding user activity patterns and sleep cycles.

1.2 Average Daily Metrics Calculation

average_daily_activity <- daily_activity %>%
  summarise(
    Average_Steps = mean(TotalSteps, na.rm = TRUE),
    Average_Active_Minutes = mean(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes, na.rm = TRUE),
    Average_Calories_Burned = mean(Calories, na.rm = TRUE)
  )
print(average_daily_activity)

Purpose: Calculating the average daily steps, active minutes, and calories burned provides a snapshot of user engagement with their fitness devices.

Insight Gained: These averages offer a glimpse into the daily routines of the FitBit user base, highlighting general trends in physical activity and energy expenditure. It sets the stage for identifying areas where Bellabeat's products could encourage more active lifestyles.

1.3 Average Sleep Metrics Calculation

average_sleep <- sleepDay %>%
  summarise(
    Average_Sleep_Duration = mean(TotalMinutesAsleep, na.rm = TRUE) / 60, 
    Average_Time_In_Bed = mean(TotalTimeInBed, na.rm = TRUE) / 60 
  )
print(average_sleep)

Purpose: Understanding sleep patterns is vital for a health-focused company. By calculating the average sleep duration and time in bed, we can assess the quality of rest users are getting.

Insight Gained: This information helps us understand how well users are resting, which is crucial for overall wellness. Insights into sleep patterns can inform product features that promote better sleep hygiene among Bellabeat users.

1.4 Average Heart Rate Calculation

average_heart_rate <- heartrate_sec %>%
  summarise(
    Average_Heart_Rate = mean(Value, na.rm = TRUE)
  )
print(average_heart_rate)

Purpose: The average heart rate is a key indicator of cardiovascular health. Analyzing this metric helps us understand the fitness levels of the user base.

Insight Gained: This data can be used to tailor Bellabeat’s health and wellness advice, encouraging activities that contribute to cardiovascular health.

1.5 Average Weight and BMI Calculation

average_weight_bmi <- weightLogInfo %>%
  summarise(
    Average_Weight_Kg = mean(WeightKg, na.rm = TRUE),
    Average_BMI = mean(BMI, na.rm = TRUE)
  )
print(average_weight_bmi)

Purpose: These metrics provide insight into the general health and fitness levels of the user base, indicating areas where Bellabeat products could offer guidance or encouragement.

Insight Gained: By understanding the average weight and BMI, Bellabeat can tailor its wellness programs to help users achieve their weight management goals more effectively.

Correlation Analysis

Purpose: The correlation analysis aims to investigate the relationship between two key health indicators: the daily total steps and calorie burn. This step is crucial for identifying patterns that can inform product development, user engagement strategies, and personalized health recommendations.

By executing the R code to calculate the Pearson correlation coefficient, we obtained a value of 0.5915681. This result indicates a moderate positive correlation between the two variables, suggesting that as the number of steps taken by an individual increases, there's a corresponding increase in the number of calories burned.

correlation <- cor(daily_activity$TotalSteps, daily_activity$Calories, use = "complete.obs")
print(correlation)

Insight:

Behavioral Insight: It confirms the intuitive understanding that physical activity, as quantified by steps taken, is an effective way to increase energy expenditure. This relationship underscores the importance of promoting active lifestyles among Bellabeat users to enhance their health and wellness.

Product Feature Development: Bellabeat can leverage this insight to develop or enhance app features that motivate users to increase their daily step count. For instance, implementing gamification elements that reward users for reaching step count milestones could be an effective strategy.

Personalized Health Recommendations: The moderate correlation provides a basis for personalized recommendations within the Bellabeat app. Users could receive customized activity goals based on their current step counts to gradually increase their calorie burn, promoting sustainable health improvements.

The correlation analysis not only validates the positive impact of increased physical activity on calorie expenditure but also provides Bellabeat with actionable insights to drive product innovation, marketing, and user engagement strategies.

Trend Analysis

Integrating Sleep and Activity Data and its subsequent analysis offer a holistic view of user behaviors, crucial for Bellabeat's mission to enhance women's health through data-driven insights. This section explores the relationship between physical activity levels and sleep quality, alongside identifying weekly trends in both activity and sleep patterns. Here's a breakdown of the insights derived and their implications:

3.0 Integrating Sleep and Activity Analysis

Objective: Combining sleep and activity data provides a comprehensive overview of user health habits. This integration allows us to explore how daily physical activities influence sleep quality and vice versa, supporting the development of well-rounded health recommendations.

codesleep_activity <- merge(sleepDay, daily_activity, by = "Id")

We merged sleep data with daily activity data on the IDfield, creating a unified dataset that contains information on both sleep and physical activities. This consolidated view is essential for performing cross-analysis between different health metrics.

3.1 Analyzing Sleep Quality vs. Activity Levels

Objective: To examine the correlation between physical activity levels (both very active minutes and lightly active minutes) and sleep efficiency, providing insights into how activity impacts sleep.

sleep_activity$SleepEfficiency <- sleep_activity$TotalMinutesAsleep / sleep_activity$TotalTimeInBed
activity_sleep_cor <- sleep_activity %>%
  summarise(
    Correlation_VeryActive_SleepEff = cor(VeryActiveMinutes, SleepEfficiency, use = "complete.obs"),
    Correlation_LightlyActive_SleepEff = cor(LightlyActiveMinutes, SleepEfficiency, use = "complete.obs")
  )
print(activity_sleep_cor)

Insights: The correlation coefficients indicate a mild positive relationship between activity levels and sleep efficiency. Specifically, very active minutes show a correlation coefficient of 0.0457, and lightly active minutes have a coefficient of 0.0752. These results suggest that higher activity levels might be associated with slightly improved sleep efficiency, albeit the relationships are not strongly pronounced.

3.2 Weekly Trends in Activity and Sleep Patterns

Purpose: To identify patterns in activity levels and sleep durations across different days of the week, aiding in understanding how user behaviors vary within a week.

Weekly Activity Trends: Analysis reveals a fluctuation in average daily steps across the week, with the highest activity levels observed on Saturdays and the lowest on Sundays. This pattern underscores the variability in user activities, potentially influenced by workweek schedules and weekend leisure activities.

    library(lubridate)
    daily_activity$DayOfWeek <- wday(daily_activity$ActivityDate, label = TRUE)
    weekly_activity_trends <- daily_activity %>%
      group_by(DayOfWeek) %>%
      summarise(
        AverageSteps = mean(TotalSteps, na.rm = TRUE)
      )
    print(weekly_activity_trends)

Weekly Sleep Trends: Sleep duration trends also vary, with the longest average sleep hours occurring on Sundays and the shortest on Tuesdays. This variability might reflect a catch-up on sleep during weekends after a busy workweek.

weekly_sleep_trends <- sleepDay %>%
  mutate(DayOfWeek = wday(SleepDay, label = TRUE)) %>%
  group_by(DayOfWeek) %>%
  summarise(
    AverageSleepHours = mean(TotalMinutesAsleep, na.rm = TRUE) / 60
  )
print(weekly_sleep_trends)

Insight:

Product and Feature Development: Insights from integrating sleep and activity data can guide the enhancement of Bellabeat's products, emphasizing features that encourage a balance between daily physical activity and adequate rest.

Personalized Health Recommendations: Understanding the relationship between activity levels and sleep efficiency enables Bellabeat to provide personalized advice to users, promoting habits that contribute to better sleep and overall health.

User Engagement: Identifying weekly trends in activity and sleep allows Bellabeat to tailor engagement strategies, such as setting realistic weekly goals for users or encouraging activity on typically less active days.

Visualization:

The visualization phase in data analysis is critical as it translates complex data into easily digestible visuals that can communicate key insights at a glance.

In the Daily Step Count Trend Analysis, the line graph created with ggplot2 demonstrates the fluctuation in daily step counts over time. This visualization helps in identifying patterns, such as peaks on certain days or trends over weeks, which can be correlated with user behavior or external events.

The Sleep Duration Distribution Analysis uses a histogram to show the distribution of total minutes asleep among users. This kind of plot is essential for understanding the common sleep durations and identifying outliers, which can be indicative of sleep disorders or other health-related issues.

The Weekday vs. Weekend Activity and Calorie Consumption Comparison employs boxplots to compare steps and calories between weekdays and weekends. This visualization can reveal differences in activity levels and energy expenditure, suggesting potential areas for targeted health interventions or marketing strategies for Bellabeat's products.

5. Analysis and Modelling

The Analysis and Modeling phase of the Bellabeat case study presented a comprehensive approach to understanding user behavior and the impact of lifestyle choices on health metrics. Here's a detailed overview of the process and the findings:

5.1 Categorizing Activity Levels:
In this stage, users were segmented into different groups based on total step counts. This segmentation allowed for more tailored recommendations, aiming to motivate users, especially those in the lower activity brackets, to increase their physical activity. It laid the groundwork for personalized goal setting, enhancing user engagement with Bellabeat products.

5.2 Sleep Duration Variation by Activity Level:

A violin plot was utilized to examine the relationship between various activity levels and sleep duration. Interestingly, no significant differences were observed across activity levels, suggesting that factors other than daily activity might influence sleep quality. This insight hinted at opportunities for Bellabeat to develop features focused on improving sleep quality, beyond merely tracking it.

5.3 Hourly Activity Analysis:

The hourly analysis of step counts uncovered distinct patterns of activity, identifying peak times when users were most active. This information is particularly useful for Bellabeat to optimize the timing of interactive prompts and reminders, aiming to boost user activity during typically inactive periods.

5.4 Visualizing Weekly Activity Patterns:

Weekly activity trends were visualized, revealing fluctuations in the number of steps taken throughout the week, with higher averages on some days compared to others. These insights are crucial for Bellabeat's strategy, enabling the company to plan weekly targets and initiatives that correspond with users' active and rest days

5.5 Analyzing Weekly Sleep Duration Trends:

Sleep trends depicted a pattern where users seemed to catch up on sleep over the weekends. This valuable information could steer Bellabeat's content strategy, influencing the advice provided for weekend activities and suggesting ways to improve sleep during the workweek.

5.6 Trend Analysis Over Time:

A trend line for daily steps was plotted to identify behavior changes over time. This long-term view revealed seasonal trends and behavioral shifts, informing Bellabeat's marketing strategies and product updates to align with these patterns.

5.7 Predictive Modeling:

A regression model was developed to predict daily step counts based on users' activity and sleep data. With an R-squared value of 0.6568, the model demonstrated a strong ability to forecast step counts, offering a substantial advantage for setting personalized and attainable daily goals within the Bellabeat ecosystem.

Reflecting on the process, the Analysis and Modeling phase underscored the value of data-driven insights in shaping product development and user engagement. The findings from this phase promise to help Bellabeat in crafting experiences that not only engage but also foster healthier lifestyles among its user base. The phase culminated in actionable outcomes that are expected to propel Bellabeat's mission of empowering women through informed health decisions.

