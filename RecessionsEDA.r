# %% [markdown] {"_kg_hide-input":false}
# # Introduction

# %% [markdown] {"_kg_hide-input":false}
# Economic conditions can have a profound effect on individuals daily lives. A strong economy is often characterized by high employment rates, affordable living costs and high consumer spending. People spend more, businesses sell more as the environment is set for economic growth. Unfortunately, growth cannot continue forever, and these times of prosperity can be met with harsh downturns filled with uncertainty, lack of opportunity and instability. Extended periods of a weakening economy are known as recessions and although these periods may present difficulty in the lives of everyday individuals, having a better understanding of what these periods entail and recognizing the warning signs can help people be better prepared and move forward through these turbulent times.
# 
# This analysis will examine historical recession data and economic data pulled from the FRED website. We will identify what to expect from recessions along with trends and patterns between recessions and economic indicators that can give warning signs to a downturn in the economy. It is important to know that some of these trends using economic indicators have already been identified previously by other analyst so I am not pretending to have discovered them. However learning about past analysis has inspired me to to dive deeper into the relationships and explore new potential connections.

# %% [code] {"_kg_hide-output":true,"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:38:12.830719Z","iopub.execute_input":"2024-04-17T18:38:12.832523Z","iopub.status.idle":"2024-04-17T18:38:12.859317Z"},"jupyter":{"outputs_hidden":false}}
options(warn = -1)
options(repr.plot.width = 25, repr.plot.height = 9)

library(tidyverse)
library(ggplot2)
library(zoo) #yearmonth fxn
library(gridExtra)
library(glue)

# %% [code] {"_kg_hide-output":true,"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:38:13.674022Z","iopub.execute_input":"2024-04-17T18:38:13.675911Z","iopub.status.idle":"2024-04-17T18:38:13.786638Z"},"jupyter":{"outputs_hidden":false}}
myd = read_csv("/kaggle/input/us-economic-data/fredmeta.csv")
str(myd)

# %% [code] {"_kg_hide-input":true,"_kg_hide-output":true,"execution":{"iopub.status.busy":"2024-04-17T18:38:14.525261Z","iopub.execute_input":"2024-04-17T18:38:14.527106Z","iopub.status.idle":"2024-04-17T18:38:14.562355Z"},"jupyter":{"outputs_hidden":false}}
# rename columns
colnames(myd) = c("date","recession","cpi","gdp","unemployment","m2", "fedfunds","tentwo", "residential")

# convert to date format
myd$date = as.Date(myd$date, format = "%m/%d/%Y")
str(myd)

# %% [code] {"_kg_hide-input":true,"_kg_hide-output":true,"execution":{"iopub.status.busy":"2024-04-17T18:38:15.372694Z","iopub.execute_input":"2024-04-17T18:38:15.374594Z","iopub.status.idle":"2024-04-17T18:38:15.402221Z"},"jupyter":{"outputs_hidden":false}}
#check for null values
sum(is.na(myd))
colSums(is.na(myd))

# %% [code] {"_kg_hide-input":true,"_kg_hide-output":true,"execution":{"iopub.status.busy":"2024-04-17T18:38:16.126308Z","iopub.execute_input":"2024-04-17T18:38:16.128473Z","iopub.status.idle":"2024-04-17T18:38:17.919195Z"},"jupyter":{"outputs_hidden":false}}
p1 = ggplot(myd, aes(x = date, y = as.factor(recession))) + geom_point() + labs(y = "recession")
p2 = ggplot(myd, aes(x = date, y = cpi)) + geom_line()
p3 = ggplot(na.omit(myd[!is.na(myd$gdp), ]), aes(x = date, y = gdp)) + geom_line()
p4 = ggplot(myd, aes(x = date, y = unemployment)) + geom_line()
p5 = ggplot(myd, aes(x = date, y = m2)) + geom_line()
p6 = ggplot(myd, aes(x = date, y = fedfunds)) + geom_line()
p7 = ggplot(myd, aes(x = date, y = tentwo)) + geom_line()
p8 = ggplot(na.omit(myd[!is.na(myd$residential), ]), aes(x = date, y = residential)) + geom_line()
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)

# CPI, GDP, M2 mostly steady increase so likely useful to convert to pct change later on for analysis

# %% [markdown] {"_kg_hide-input":false}
# # Recessions Overview
# 
# ### Objectives
# - Frequency of recessions
# - Length of recessions and non-recessionary periods
# - Disparities between inidividual recessions
# - Explore notable trends from initial observations in more depth

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:49:38.044893Z","iopub.execute_input":"2024-04-17T18:49:38.049703Z","iopub.status.idle":"2024-04-17T18:49:38.785970Z"},"jupyter":{"outputs_hidden":false}}
options(repr.plot.width = 20, repr.plot.height = 9)

# Recession plot
ggplot(myd, aes(x = date, y = as.factor(recession))) +
  geom_point(aes(color = as.factor(recession))) + 
  scale_color_manual(values = c("1" = "#0e1bd4", "0" = "#9facb2"), 
                     labels = c("1" = "Recession Period", "0" = "Non-Recession Period"))  +
  labs(title = "Recessions since 1947", x = "Date", y = "Recession", color = "Status") +
  theme(text = element_text(size = 15),  
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold")) 

# Recession Periods
# Tibble to identify individual recession periods and their lengths in months
recession_periods <- myd %>%
  mutate(recession_start = ifelse(lag(recession, default = first(recession)) == 0 & recession == 1, 1, 0),
         recession_end = ifelse(lead(recession, default = last(recession)) == 0 & recession == 1, 1, 0)) %>%
  filter(recession_start == 1 | recession_end == 1) %>%
  group_by(group = cumsum(recession_start)) %>%
  summarise(start_date = min(date), end_date = max(date),
            month_length = round((as.yearmon(end_date) - as.yearmon(start_date)) * 12) + 1) %>%
  ungroup() %>%
  select(-group)

# Non-Recessionary Periods
# Tibble to identify non-recessionary periods and their lengths in months
non_recession_periods <- recession_periods %>%
  mutate(next_recession_start = lead(start_date, order_by = start_date, default = last(start_date))) %>%
  filter(!is.na(next_recession_start)) %>%
  mutate(prev_recession_end = end_date) %>%
  mutate(months_to_next_recession = round((as.yearmon(next_recession_start) - as.yearmon(end_date)) * 12) + 1) %>%
  select(prev_recession_end, next_recession_start, months_to_next_recession)

# % time in recession
recession_pct <- round(sum(myd$recession == 1)/nrow(myd)*100, 2)

# %% [markdown] {"_kg_hide-input":false}
# - Clearly much less time in recessions
# - 12 recessions since 1947
# - Around 13% of time in recession

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:47:20.392823Z","iopub.execute_input":"2024-04-17T18:47:20.394997Z","iopub.status.idle":"2024-04-17T18:47:20.837175Z"},"jupyter":{"outputs_hidden":false}}
options(repr.plot.width = 17, repr.plot.height = 9)

# Recessions Plot
rp_year <- recession_periods %>%
  mutate(start_year = year(start_date),
         end_year = year(end_date),
         # Adding a line break before "Recession"
         period_label = paste(as.factor(start_year), as.factor(end_year), sep="-") %>%
                         paste0("\nRecession"))

recession_plot <- ggplot(rp_year, aes(x = period_label, y = month_length)) +
  geom_bar(stat = "identity", fill = "#34346a") +
  geom_hline(yintercept = mean(rp_year$month_length), color = "#9facb2") +
  geom_text(aes(label = month_length), vjust = -0.5, size = 5) +
  labs(title = "Recession Lengths", x = "Recession Period", y = "Months") +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold")) 

print(recession_plot)

# Average recession length for all periods
avg_length_all <- round(recession_periods %>%
  summarise(avg_recession_length = mean(month_length)) %>%
  pull(avg_recession_length), 2)

# Average recession length excluding 2020 recession
avg_length_excluding_2020 <- round(recession_periods %>%
  filter(end_date < "2020-01-01") %>%
  summarise(avg_recession_length = mean(month_length)) %>%
  pull(avg_recession_length), 2)

# %% [markdown] {"_kg_hide-input":false}
# Now seperating and examining individual recessions 
# Notice 2020 is by far the smallest and is be caused by more of an externak black swan event that typical economic cycle conditions so many may consider it an outlier. As such, some calculations throughout this report will be given including and excluding 2020.
# - Average recession length is 10.33 or just under a year.
# - Average excluding is 2020 is 11.09.
# - The two most recent recessions contained the longest and shortest recession.
# - There seems to be greater variation in the length of recent recession compared to earlier recessions.

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:47:29.747643Z","iopub.execute_input":"2024-04-17T18:47:29.750729Z","iopub.status.idle":"2024-04-17T18:47:30.231719Z"},"jupyter":{"outputs_hidden":false}}
nrp_year = non_recession_periods %>%
  mutate(start_year = year(prev_recession_end),
         end_year = year(next_recession_start),
         period_label = paste("Before", as.factor(end_year), "Recession"))

ggplot(nrp_year, aes(x = period_label, y = months_to_next_recession)) +
  geom_bar(stat = "identity", fill = "#34346a") +
  geom_hline(yintercept = mean(nrp_year$months_to_next_recession), color = "#9facb2") +
  labs(title = "Non-Recessionary Period Lengths", x = "Non-Recession Period", y = "Months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = 15), 
      axis.text = element_text(size = 13),
      plot.title = element_text(size = 20, face = "bold")) 

# Avg length of time from end date of one recession to start date of next date 
avg_length_to_next_recession <- round(non_recession_periods %>%
  summarise(avg_months_to_next_recession = mean(months_to_next_recession)) %>%
  pull(avg_months_to_next_recession), 2)

# not including 2020
avg_length_to_next_recession_2020 <- round(non_recession_periods %>%
  filter(prev_recession_end < "2020-01-01") %>%
  summarise(avg_months_to_next_recession = mean(months_to_next_recession)) %>%
  pull(avg_months_to_next_recession), 2)

# %% [markdown] {"_kg_hide-input":false}
# - Average length of non-recessionary periods: 62.92 months
# - Average length excluding 2020: 68.64 months
# - Recessions are sparsed out on average 5-6 years although definite variaiton with the shortest period being 14 months and the longest being 130 months or  .... years
# 
# Again there appears to be some diparity between earlier (pre 1990) and recent recessions (post 1990). 7 of the 8 non-recessionary periods prior to 1990 have been shorter that the average while 4 of the 5 last non-recession periods since 1990 have been above the average. These diferences between time periods will be explored deeper later on in this analysis. First, now that we have values for the length of non-recessionary periods and their subsequent recession lengths, we will check if there is any correlation between the two.

# %% [markdown] {"_kg_hide-input":false}
# ### Is there any relationship between the length of non-recessionary period and the subsequent recession?

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:47:56.671535Z","iopub.execute_input":"2024-04-17T18:47:56.673403Z","iopub.status.idle":"2024-04-17T18:47:57.083286Z"},"jupyter":{"outputs_hidden":false}}
# Colum added for easier joining
recession_periods <- recession_periods %>%
  mutate(next_recession_start = lead(start_date))

# Join of non_recession_periods with recession_periods
analysis_df <- non_recession_periods %>%
  left_join(recession_periods, by = c("next_recession_start" = "start_date")) %>%
  select(recession_start = next_recession_start,
         prev_nonrecession_length = months_to_next_recession,
         recession_length = month_length) %>%
  slice(1:(n() -1))

# Correlation calculation
correlation_result <- cor(analysis_df$prev_nonrecession_length, analysis_df$recession_length, use = "complete.obs")

# Scatter Plot
ggplot(analysis_df, aes(x = prev_nonrecession_length, y = recession_length)) +
  geom_point() +
  labs(title = "Non-Recession Period Length vs. Subsequent Recession Period Length",
       x = "Length Non-Recessionary period",
       y = "Length of Subsequent Recession") +
  geom_smooth(formula = y ~ x, method = "lm", color = "#0e1bd4", se = FALSE) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold")) 

# Excluding 2020
# Filter out the row with recession_start equal to "2020-03-01"
analysis_filtered <- analysis_df %>%
  filter(recession_start != "2020-03-01")

# Calculate correlation
correlation_result_filtered <- cor(analysis_filtered$prev_nonrecession_length, analysis_filtered$recession_length, use = "complete.obs")

# %% [markdown] {"_kg_hide-input":false}
# - Starting with 1953 recession since this dataset starts at 1947 and dont have suffient non-recessionary period length data for the 1948 recession
# - Including 2020 (-0.514) moderate negative
# - Excluding 2020 (-0.314) low negative correlation
# - Using all the data appears to show a trend between the length of the non-recessionary period and the subsequent recession length, specifially that longer non-recessioanry periods may lead to shorter recession lengths and vice-versa. It is important to note that it is only a moderate correlation of -0.5 and excluding the 2020 recession cuts this correlation result down to -0.314.

# %% [markdown] {"_kg_hide-input":false}
# # Early vs Recent Recessions (*Pre and Post 1985*)
# 
# As stated previously, there appears to be notable differences between recessions in in the first and second half of the data. There may be insight to be gained not only comparing the two groups, but also in analyzing more recent recessions specifically as it may provide better insight in to what we can expect in our next economic downturn. The data will be split just around half using 1985 as the cut-off point.

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:47:59.102633Z","iopub.execute_input":"2024-04-17T18:47:59.104383Z","iopub.status.idle":"2024-04-17T18:47:59.827706Z"},"jupyter":{"outputs_hidden":false}}
options(repr.plot.width = 15, repr.plot.height = 9)

# Grouping recession periods into two groups, before 1985 and after 1985
# Have to make groups factors to control order of bars in bar chart
recession_periods <- recession_periods %>%
  mutate(group = factor(ifelse(start_date < "1985-01-01", "Early", "Recent"),
                        levels = c("Early", "Recent")))

# Bar chart of recession lengths and number of recessions by group

# Number of Recessions by Group
plot1 <- ggplot(recession_periods, aes(x = group, fill = group)) +
  geom_bar() +
  labs(title = "Number of Recessions by Time Period", x = "Time Period", y = "Count", fill = "Time Period") +
  scale_fill_manual(values = c("Early" = "#2d478d", "Recent" = "#47a0c6"),
                    labels = c("Early" = "Before 1985", "Recent" = "After 1985")) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold")) 

# Recession Lengths by Group
plot2 <- ggplot(recession_periods, aes(x = group, y = month_length, fill = group)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Length of Recessions by Time Period", x = "Time Period", y = "Months", fill = "Time Period") +
  scale_fill_manual(values = c("Early" = "#2d478d", "Recent" = "#47a0c6"),
                    labels = c("Early" = "Before 1985", "Recent" = "After 1985")) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold")) 

grid.arrange(plot1, plot2, ncol = 2)

# %% [markdown] {"_kg_hide-input":false}
# Number of Recessions
# - Before 1985: 8
# - After 1985: 4
# 
# Total Months in Recession
# - Before 1985: 88
# - After 1985: 36
# 
# Clear disparity between the two as recent times have had half as much instances of recession along with a shorter total duration. The shorter total time may be expected considering the less frequent instances of recession so next we will look at the ranges and averages for recessions in each period.

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:00.917509Z","iopub.execute_input":"2024-04-17T18:48:00.920022Z","iopub.status.idle":"2024-04-17T18:48:01.451456Z"},"jupyter":{"outputs_hidden":false}}
ggplot(recession_periods, aes(x = group, y = month_length, fill = group)) +
  geom_boxplot() +
  labs(title = "Recession Lengths by Time Period", x = "Time Period", y = "Months", fill = "Time Period") +
  scale_fill_manual(values = c("Early" = "#395294", "Recent" = "#47a0c6"),
                    labels = c("Early" = "Before 1985", "Recent" = "After 1985")) + 
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold")) 

# Avg length older vs recent
avg_length_by_group <- round(recession_periods %>%
  group_by(group) %>%
  summarise(avg_recession_length = mean(month_length)) %>%
  pull(avg_recession_length), 2)

# %% [markdown] {"_kg_hide-input":false}
# - Average recession length before 1985: 11 months
# - Average recession length after 1985: 9 months
# 
# So not only have there been less frequent recessions since 1985 as compared to befroe then but recent instances are shorter on avergae.
# With that being said, the range of time is much larger with recent recession containg both the largest and smallest receession lengths so there may be more unpredicatbility in the severity.

# %% [markdown] {"_kg_hide-input":false}
# # Economic Indicators 
# Now we will look at some economic indicators often linked with recessions and visualize the historical data alongside recession data to look for any trends. Calcualtions will also be performed to find specific quantitative values for those trends. I will be focusing on the three indicators that I found the most useful although the dataset contains other indictors that can furthur be explored.
# The following visualizations include grey shaded regions over the plots to indicate recessionary periods. Not all of the indicators presented had data accessible since 1947 so I used data as far back as I could find and beleive it can still provide useful insights.

# %% [markdown] {"_kg_hide-input":false}
# ## 10 vs 2 Year Treasury Yield

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:05.688604Z","iopub.execute_input":"2024-04-17T18:48:05.690572Z","iopub.status.idle":"2024-04-17T18:48:06.229092Z"},"jupyter":{"outputs_hidden":false}}
options(repr.plot.width = 20, repr.plot.height = 9)

myd_tentwo = myd %>% filter(!is.na(tentwo))

# 10 vs 2 year plot
ggplot(myd_tentwo, aes(x = date, y = tentwo)) +
  labs(title = "10 vs 2 Year Treasury Yield and Recessions (1976-2023)", x = "Time", y = "Percent") +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_rect(data = recession_periods %>% 
            filter(start_date >= min(myd_tentwo$date)), inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "#adbbbd", alpha = 0.5) +
  geom_ribbon(aes(ymin = pmin(tentwo, 0), ymax = 0), fill = "#0e1bd4", alpha = 0.5) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold"))

# %% [markdown] {"_kg_hide-input":false}
# The above visualization shows the two year vs ten year treasury yield since 1976 as that is the earliest data I could find. The dark grey shaded areas represent recession periods and the purple shaded areas highlight the inversion periods of the ten minus two year yield which are commonly associated with recessions.
# 
# - Main observation is that an inversion of the yield curve closely precedeing every recession other than the 2020 recession, alhtough even then, the yield curve was very close to inverting and reached a level as small as 0.05%.
# 
# - Again there were some differences between periods before and after 1985
#     - Avg time is 378 days (just over a year) from end of the first inversion period >1 to start of subsequent recession since 1985
#     - Earlier recessions occured while yield curve was still inverted while in recent times, the yield curve reverted back prior to recession
#     - Earlier periods had steeper inversion periods.
#     - Recent periods have multiple inversion and reversions in between recessionary periods
#         - although always occured within under 2 years of first inversion regardless
#     
# - Yield curve usually reaches a max between 2-3% before begins declining

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:08.310640Z","iopub.execute_input":"2024-04-17T18:48:08.312379Z","iopub.status.idle":"2024-04-17T18:48:08.386133Z"},"jupyter":{"outputs_hidden":false}}
# Tibble for each inversion period and their length in months
inversion_periods <- myd %>%
  mutate(inversion_start = ifelse(lag(tentwo, default = first(tentwo)) >= 0 & tentwo < 0, 1, 0),
         inversion_end = ifelse(lead(tentwo, default = last(tentwo)) >= 0 & tentwo < 0, 1, 0)) %>%
  filter(inversion_start == 1 | inversion_end == 1) %>%
  group_by(group = cumsum(inversion_start)) %>%
  summarise(start_date = min(date), end_date = max(date), 
            month_length = round((as.yearmon(end_date) - as.yearmon(start_date)) * 12) + 1) %>%
  ungroup() %>%
  mutate(end_date = if_else(row_number() == n(), NA_Date_, end_date),
         month_length = if_else(row_number() == n(), NA_integer_, month_length)) %>%
  select(-group)

inversion_periods

# Average length of inversion period
avg_length_inversion <- round(mean(inversion_periods$month_length[!is.na(inversion_periods$month_length)]), 2)

# Averag length of of inversion excluding those lasting only a month
avg_length_inversion_excluding_short <- round(mean(inversion_periods$month_length[!is.na(inversion_periods$month_length) & inversion_periods$month_length > 1]), 2)

# %% [markdown] {"_kg_hide-input":false}
# - Avergae length of inverson period is 6.64 months
# - Average length excluding the inversions that lasted only a month: 8.75 months.
#     - All of the inversion that only lasted a month were accompanied by other inversions within that same non-recessionary-period
# 
# 
# - NOTE: 2022 inversion period still ongoing. As of December 2023 that inversion has lasted 17 months.
# - Have to go back to early 80s to see yield curve inverted for this long

# %% [markdown] {"_kg_hide-input":false}
# ### Is there a relationship between the length of the inversion period and the legnth of the subsequent recession length?

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:11.007988Z","iopub.execute_input":"2024-04-17T18:48:11.009744Z","iopub.status.idle":"2024-04-17T18:48:11.075378Z"},"jupyter":{"outputs_hidden":false}}
# Grouping the multiple inversion periods that occur within same nonrecessionary period
inversion_periods_grouped = inversion_periods %>%
    mutate(group = factor(case_when(
        start_date >= "1985-01-01" & end_date <= "1992-01-01" ~ "1990-08-01",
        start_date >= "1995-01-01" & end_date <= "2002-01-01" ~ "2001-04-01",
        start_date >= "2005-01-01" & end_date <= "2010-01-01" ~ "2008-01-01",
        TRUE ~ NA
    ), levels = c("1990-08-01", "2001-04-01", "2008-01-01")))

inversion_periods_grouped = inversion_periods_grouped %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(total_inversion_length = sum(month_length))

# Convert group column to date format
inversion_periods_grouped$group <- as.Date(inversion_periods_grouped$group)


# Combine with recession periods 
inversion_periods_grouped = inversion_periods_grouped %>%
    left_join(recession_periods, by = c("group" = "start_date")) %>%
    select(group, total_inversion_length, month_length)
inversion_periods_grouped

# Correlation calcualtion
correlation_result = round(cor(inversion_periods_grouped$total_inversion_length, inversion_periods_grouped$month_length, use = "complete.obs"), 2)


output <- glue("Correlation result: {correlation_result}")
cat(output)

# %% [markdown] {"_kg_hide-input":false}
# Grouped multiple inversions belonging to the same non-recessionary period. Only had three cases but still interested in seeing result. Only used recessions after 1985 because as mentioned before, that is when recession began occuring after the reversion.

# %% [markdown] {"_kg_hide-input":false}
# ## Unemployment Rate

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:13.741019Z","iopub.execute_input":"2024-04-17T18:48:13.742729Z","iopub.status.idle":"2024-04-17T18:48:14.235855Z"},"jupyter":{"outputs_hidden":false}}
# Unemployment Rate plot
ggplot(myd, aes(x = date, y = unemployment)) +
  labs(title = "Unemployment Rate and Recessions (1947-2023)", x = "Time", y = "Percent") + 
  geom_line() +
  geom_rect(data = recession_periods, inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "#9facb2", alpha = 0.5) +
  geom_ribbon(aes(ymin = pmin(unemployment, 5), ymax = 5), fill = "#0011ff", alpha = 0.5) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold"))

# %% [markdown] {"_kg_hide-input":false}
# Above is the unemployment rate with recession shading since 1947. Keep in mind, the increase in unemployment rate is often used as the definition of a recession so you will clearly see the unemployment rate ticking up in the grey shaded areas and the maximum unemployment being reaced is very close to the ends of recessions and it continously ticking downwards thereafter. This is nothing new so we will instead look into what precedes this rise in unemployment rate and see if there are any thresholds and trends observed before the recession starts.
# 
# The purple shaded areas shade the threshold in which recessions appear to occur which is in under 5%  unemployment environemnts. Once beow this threshold is reached and the unemployment rate beigns ticking up again consecutive quarters, it may indicate a looming recession.

# %% [markdown] {"_kg_hide-input":false}
# ### Is there any pattern in the amount of drop in unemployment rates from one recession to the next?

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:17.249952Z","iopub.execute_input":"2024-04-17T18:48:17.251644Z","iopub.status.idle":"2024-04-17T18:48:17.311729Z"},"jupyter":{"outputs_hidden":false}}
# Calcualting unemployment rate change prior to each recession
unemployment_change <- recession_periods %>%
  # Create column for the next recession's start date
  mutate(next_recession_start = lead(start_date, default = last(start_date))) %>%
  rowwise() %>%
  mutate(
    unemployment_end = myd$unemployment[which(myd$date == end_date)],
    unemployment_start_next = myd$unemployment[which(myd$date == next_recession_start)],
    unemployment_diff = unemployment_start_next - unemployment_end
  ) %>%
  select(start_date, end_date, next_recession_start, unemployment_diff)

unemployment_change

# %% [markdown] {"_kg_hide-input":false}
# Here I calcualted the drop in unemployment from the end the recession to the start of the next.
# 
# - 75% of recesssions occured post >2% drop in unemployment
# - 75% of recessions post 1985 had >3% drop in unemploymnt 
# 
# I will now see if there is anything of note using the maximum an minimum range during the nonrecessionary period instead of using the difference from the ends and starts of recessions.

# %% [markdown] {"_kg_hide-input":false}
# ### What about patterns in the drop of unemployment from the  maximum to the minimum?

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:19.827608Z","iopub.execute_input":"2024-04-17T18:48:19.829671Z","iopub.status.idle":"2024-04-17T18:48:19.995575Z"},"jupyter":{"outputs_hidden":false}}
recession_unemployment <- recession_periods %>%
  mutate(
    next_recession_start = lead(start_date, order_by = start_date, default = last(start_date))
  ) %>%
  filter(!is.na(next_recession_start)) %>%
  rowwise() %>%
  mutate(
    # Period between the end of this recession and the start of the next
    period_end = end_date,
    period_start_next = next_recession_start,
    # Max unemployment rate after the end of this recession and before the next
    max_unempl = max(myd$unemployment[myd$date > period_end & myd$date < period_start_next], na.rm = TRUE),
    # Min unemployment rate in the same period
    min_unempl = min(myd$unemployment[myd$date > period_end & myd$date < period_start_next], na.rm = TRUE),
    # Difference between max and min unemployment rates in the period
    unempl_diff = max_unempl - min_unempl
  ) %>%
  # Ungroup to avoid rowwise operations moving forward
  ungroup() %>%
  # Select relevant columns
  select(end_date, next_recession_start, max_unempl, min_unempl, unempl_diff)

recession_unemployment

# %% [markdown] {"_kg_hide-input":false}
# Here I calculated the maximum drop from the peak unemlployment rate preceding each recession. There was nothing that really stood out using this difference method compared to the previous.

# %% [markdown] {"_kg_hide-input":false}
# ## Real Residential Property Prices

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:22.216360Z","iopub.execute_input":"2024-04-17T18:48:22.218697Z","iopub.status.idle":"2024-04-17T18:48:22.724924Z"},"jupyter":{"outputs_hidden":false}}
myd_residential = na.omit(myd[!is.na(myd$residential), ])

ggplot(myd_residential, aes(x = date, y = residential)) +
  labs(title = "Real Residneital Price Growth and Recessions (1971-2023)", x = "Time", y = "Percent") + 
  geom_line() +
  geom_rect(data = recession_periods %>% 
              filter(start_date >= min(myd_residential$date)), inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "#adbbbd", alpha = 0.5) +
  geom_ribbon(aes(ymin = pmin(residential, 0), ymax = 0), fill = "#0e1bd4", alpha = 0.5) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20, face = "bold"))

# %% [markdown] {"_kg_hide-input":false}
# - Negative rate often suitable environemnt for recession (similar to Ten vs Two yield curve)
# - Continous decline in general precede recessions
#     - Did not occur in 2000 however

# %% [markdown] {"_kg_hide-input":false}
# ### How large is the drop in the real residential price rate?

# %% [code] {"_kg_hide-input":true,"execution":{"iopub.status.busy":"2024-04-17T18:48:25.392151Z","iopub.execute_input":"2024-04-17T18:48:25.394437Z","iopub.status.idle":"2024-04-17T18:48:25.723830Z"},"jupyter":{"outputs_hidden":false}}
residential_diff <- recession_periods %>%
  mutate(next_recession_start = lead(start_date, default = last(start_date))) %>%
  filter(!is.na(next_recession_start)) %>%
  rowwise() %>%
  mutate(
    period_end = end_date,
    period_start_next = next_recession_start,
    max_residential = max(myd_residential$residential[myd_residential$date > period_end & myd_residential$date < period_start_next], na.rm = TRUE),
    min_residential = min(myd_residential$residential[myd_residential$date > period_end & myd_residential$date < period_start_next], na.rm = TRUE),
    residential_diff = max_residential - min_residential
  ) %>%
  ungroup() %>%
  select(end_date, next_recession_start, max_residential, min_residential, residential_diff)

residential_diff

# %% [markdown] {"_kg_hide-input":false}
# # Conclusion
# Recent times appear to show more favorable economic cycle conditions as we have had a notable drop in the frequency of recessions while maintaing a length slightly lower than average. The disparity in lengths however is much more variable post 1985 which may mitigate some these improvments.
# 
# Among economic indicators, the 10-year minus 2-year Treasury yield spread and the unemployment rate have demonstrated the most consistent trends preceding recessions. Typically, the inversion of the yield curve tends to precede economic downturns, a pattern observed since 1985. Meanwhile, fluctuations in the unemployment rate and residential market also offer insights, particularly when unemployment rate is below 5 percent and house prices show steep declines.
# 
# Currently, both the 10-year minus 2-year yield spread and the unemployment rate have reached thresholds identified as signaling a recession. While the yield curve has yet to revert, it is on the brink of doing so, with the potential to become the longest inversion in history if it persists for another two months. Additionally, the current drop in the unemployment rate is the steepest observed before a recession, underscoring the severity of the economic shift. It will be interesting to see what this next economic dowturn entails but as for now I hope this analysis provided a clearer picture of what to expect from recessions and some useful predictors to look out for.