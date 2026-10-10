# clear environment
rm(list = ls())

# turn off scientific notation
options(scipen = 999)

#set wd
setwd("C:/Users/vinug/OneDrive - University of Reading/mm333/Week 2")

#libraries
library(tidyverse)
library(broom)
library(effectsize)
library(rstatix)
library(readr)
library(car)
library(ggplot2)
library(qqplotr)

# load csv file for campaigns
campaigns <- read_csv("MM333_week2_campaigns.csv") %>% 
  mutate(channel = factor(channel, levels = c("Email", "Social", "In-store")))

# load csv file for productivity
productivity <- read_csv("MM333_week2_productivity_paired.csv", show_col_types = FALSE)


# Activity 2 --------------------------------------------------------------

# inspect the campaigns data
glimpse(campaigns)

# get summary for the campaigns data
summary(campaigns)

# count numbers of observation for each level of channel
count(campaigns, channel)

# get a summary of number or observations, missings, and duplicates
campaigns %>%
  summarise(
    n = n(),
    missing_values = sum(is.na(pick(everything()))),
    duplicated_ids = n() - n_distinct(observation_id)
  )

# inspect the productivity data
glimpse(productivity)

# get summary for the productivity data
summary(productivity)


# Activity 3 --------------------------------------------------------------

# select just the level Email and Social from channel
two_channels <- campaigns %>% 
  filter(channel %in% c("Email", "Social"))

# get mean, sd, and median for sales for Email and Social
two_channels %>%
  group_by(channel) %>%
  summarise(
    n = n(),
    mean_sales = mean(sales_revenue_gbp),
    sd_sales = sd(sales_revenue_gbp),
    median_sales = median(sales_revenue_gbp),
    .groups = "drop"
  )

# create a boxplot
ggplot(two_channels, aes(x = channel, y = sales_revenue_gbp, fill = channel)) +
  geom_boxplot(alpha = 0.6, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.35, show.legend = FALSE) +
  labs(x = "Marketing channel", y = "Sales revenue (£)") +
  theme_minimal()

# shapiro-wilk test by channel
by(two_channels$sales_revenue_gbp, two_channels$channel, shapiro.test)

# Q-Q plot
par(mfrow = c(1, 2))

qqPlot(
  two_channels$sales_revenue_gbp[two_channels$channel == "Email"],
  envelope = 0.95,
  main = "Email",
  xlab = "Theoretical Quantiles",
  ylab = "Sales Revenue (£)"
)

qqPlot(
  two_channels$sales_revenue_gbp[two_channels$channel == "Social"],
  envelope = 0.95,
  main = "Social",
  xlab = "Theoretical Quantiles",
  ylab = "Sales Revenue (£)"
)

par(mfrow = c(1, 1))

# levene's test
leveneTest(
  sales_revenue_gbp ~ channel, 
  data = two_channels)

# independent sample t-test
independent_t <- t.test(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  var.equal = TRUE,
  alternative = "two.sided"
)

independent_t
broom::tidy(independent_t)

# effect size for independent sample t-test
effectsize::cohens_d(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  pooled_sd = TRUE
)

# confidence interval plot
# Plot the mean sales revenue for Email and Social campaigns, with 95% confidence intervals as error bars.
two_channels %>%
  ggplot(aes(x = channel, y = sales_revenue_gbp, fill = channel)) +
  stat_summary(
    fun = mean,
    geom = "col",
    width = 0.6
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2
  ) +
  labs(
    title = "Mean Sales Revenue by Marketing Channel",
    x = "Marketing Channel",
    y = "Mean Sales Revenue (£)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Welch's t-test
welch_t <- t.test(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  var.equal = FALSE,
  alternative = "two.sided"
)

welch_t
broom::tidy(welch_t)

# effect size for Welch's t-test
effectsize::cohens_d(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  pooled_sd = FALSE
)

# confidence interval plot
# Plot the mean sales revenue for Email and Social campaigns, with 95% confidence intervals as error bars.
two_channels %>%
  ggplot(aes(x = channel, y = sales_revenue_gbp, fill = channel)) +
  stat_summary(
    fun = mean,
    geom = "col",
    width = 0.6
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2
  ) +
  labs(
    title = "Mean Sales Revenue by Marketing Channel",
    x = "Marketing Channel",
    y = "Mean Sales Revenue (£)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Mann-Whitney U test
mann_whitney <- wilcox.test(
  sales_revenue_gbp ~ channel,
  data = two_channels,
  alternative = "two.sided",
  exact = FALSE,
  conf.int = TRUE
)
mann_whitney
broom::tidy(mann_whitney)

# effect size for Mann-Whitney U test
effectsize ::rank_biserial(
  sales_revenue_gbp ~ channel,
  data = two_channels
)

# boxplot with individual observations
two_channels %>%
  ggplot(aes(x = channel, y = sales_revenue_gbp, fill = channel)) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.1,
    alpha = 0.5
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    colour = "red"
  ) +
  labs(
    title = "Sales Revenue by Marketing Channel",
    x = "Marketing Channel",
    y = "Sales Revenue (£)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Activity 4 --------------------------------------------------------------

# create a new dataframe, and create a new column called change, which is the difference between after and before
differences <- productivity %>%
  mutate(change = productivity_after - productivity_before)

# get summary for the differences data
differences %>%
  summarise(
    n = n(),
    mean_before = mean(productivity_before),
    mean_after = mean(productivity_after),
    mean_change = mean(change),
    sd_change = sd(change),
    median_change = median(change)
  )

# visually present the data
productivity %>%
  pivot_longer(
    cols = c(productivity_before, productivity_after),
    names_to = "time",
    values_to = "productivity"
  ) %>%
  mutate(
    time = factor(
      time,
      levels = c("productivity_before", "productivity_after"),
      labels = c("Before", "After")
    )
  ) %>%
  ggplot(aes(x = time, y = productivity, group = employee_id)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Employee Productivity Before and After Training",
    x = "Measurement",
    y = "Productivity"
  ) +
  theme_minimal()

# shapiro-wilk test
shapiro.test(differences$change)

# qq plot
qqPlot(
  differences$change,
  envelope = 0.95,
  main = "Q-Q Plot of Productivity Changes",
  xlab = "Theoretical Quantiles",
  ylab = "Change in Productivity"
)

# boxplot
differences %>%
ggplot(aes(y = change)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Productivity Changes",
    y = "Change in Productivity",
    x = NULL
  ) +
  theme_minimal()

# paired sample t-test
paired_t <- t.test(
  productivity$productivity_before,
  productivity$productivity_after,
  paired = TRUE,
  alternative = "two.sided"
)

paired_t
broom::tidy(paired_t)

# effect size for paired sample t-test
effectsize::cohens_d(
  productivity$productivity_after,
  productivity$productivity_before,
  paired = TRUE
)

# confidence interval plot
productivity %>%
  pivot_longer(
    cols = c(productivity_before, productivity_after),
    names_to = "time",
    values_to = "productivity"
  ) %>%
  mutate(
    time = factor(
      time,
      levels = c("productivity_before", "productivity_after"),
      labels = c("Before", "After")
    )
  ) %>%
  ggplot(aes(x = time, y = productivity, fill = time)) +
  stat_summary(
    fun = mean,
    geom = "col",
    width = 0.6
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2
  ) +
  labs(
    title = "Mean Employee Productivity Before and After Training",
    x = "Measurement",
    y = "Mean Productivity"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Wilcoxon signed-rank test
wilcoxon_paired <- wilcox.test(
  productivity$productivity_after,
  productivity$productivity_before,
  paired = TRUE,
  alternative = "two.sided",
  exact = FALSE,
  conf.int = TRUE
)

wilcoxon_paired
broom::tidy(wilcoxon_paired)

# effect size for Wilcoxon signed-rank test
effectsize::rank_biserial(
  productivity$productivity_after,
  productivity$productivity_before,
  paired = TRUE
)

# visualisation of results
productivity %>%
  pivot_longer(
    cols = c(productivity_before, productivity_after),
    names_to = "time",
    values_to = "productivity"
  ) %>%
  mutate(
    time = factor(
      time,
      levels = c("productivity_before", "productivity_after"),
      labels = c("Before", "After")
    )
  ) %>%
  ggplot(aes(x = time, y = productivity, fill = time)) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.1,
    alpha = 0.5
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    colour = "red"
  ) +
  labs(
    title = "Employee Productivity Before and After Training",
    x = "Measurement",
    y = "Productivity"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Activity 5 --------------------------------------------------------------

# summary of campaigns data
campaigns %>%
  group_by(channel) %>%
  summarise(
    n = n(),
    mean = mean(conversion_rate, na.rm = TRUE),
    sd = sd(conversion_rate, na.rm = TRUE),
    median = median(conversion_rate, na.rm = TRUE),
    IQR = IQR(conversion_rate, na.rm = TRUE),
    .groups = "drop"
  )

# shapiro-wilk test for each level of channel
by(campaigns$sales_revenue_gbp, campaigns$channel, shapiro.test)

# qq plot for each level of channel
par(mfrow = c(1, 3))

qqPlot(
  campaigns$conversion_rate[campaigns$channel == "Email"],
  envelope = 0.95,
  main = "Email",
  xlab = "Theoretical Quantiles",
  ylab = "Conversion Rate (%)"
)

qqPlot(
  campaigns$conversion_rate[campaigns$channel == "Social"],
  envelope = 0.95,
  main = "Social",
  xlab = "Theoretical Quantiles",
  ylab = "Conversion Rate (%)"
)

qqPlot(
  campaigns$conversion_rate[campaigns$channel == "In-store"],
  envelope = 0.95,
  main = "In-store",
  xlab = "Theoretical Quantiles",
  ylab = "Conversion Rate (%)"
)

par(mfrow = c(1, 1))

# levene's test
leveneTest(
  conversion_rate ~ channel,
  data = campaigns
)

# boxplot
campaigns %>%
  ggplot(aes(x = channel, y = conversion_rate, fill = channel)) +
  geom_boxplot() +
  labs(
    title = "Conversion Rates by Marketing Channel",
    x = "Marketing Channel",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# one-way ANOVA
anova_model <- aov(conversion_rate ~ channel, data = campaigns)
summary(anova_model)

broom::tidy(anova_model)

# TukeyHSD post hoc analysis
tukey_results <- TukeyHSD(anova_model)
tukey_results

broom::tidy(tukey_results)

# effect size for one-way ANOVA
effectsize::eta_squared(anova_model)

# confidence interval plot
campaigns %>%
  ggplot(aes(x = channel, y = conversion_rate, fill = channel)) +
  stat_summary(
    fun = mean,
    geom = "col",
    width = 0.6
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2
  ) +
  labs(
    title = "Mean Conversion Rate by Marketing Channel",
    x = "Marketing Channel",
    y = "Mean Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Kruskal-Wallis test
kruskal_results <- kruskal.test(
  conversion_rate ~ channel,
  data = campaigns
)

kruskal_results
broom::tidy(kruskal_results)

# Dunn's test post-hoc analysis
library(FSA)
dunn_results <- FSA::dunnTest(
  conversion_rate ~ channel,
  data = campaigns,
  method = "holm"
)
dunn_results

# effect size for Kruskal-Wallis test
effectsize::rank_epsilon_squared(
  conversion_rate ~ channel,
  data = campaigns
)

# visualisation of the results
campaigns %>%
  ggplot(aes(x = channel, y = conversion_rate, fill = channel)) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.1,
    alpha = 0.5
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    colour = "red"
  ) +
  labs(
    title = "Conversion Rates by Marketing Channel",
    x = "Marketing Channel",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Activity 6 --------------------------------------------------------------

# get mean and sd for the two variables
campaigns %>%
  summarise(
    n = n(),
    mean_ad_spend = mean(advertising_spend_gbp, na.rm = TRUE),
    sd_ad_spend = sd(advertising_spend_gbp, na.rm = TRUE),
    mean_revenue = mean(sales_revenue_gbp, na.rm = TRUE),
    sd_revenue = sd(sales_revenue_gbp, na.rm = TRUE)
  )

# visually inspect the relationship between the two variables
campaigns %>%
  ggplot(aes(x = advertising_spend_gbp, y = sales_revenue_gbp)) +
  geom_point() +
  labs(
    title = "Advertising Expenditure and Sales Revenue",
    x = "Advertising Expenditure (£)",
    y = "Sales Revenue (£)"
  ) +
  theme_minimal()

# check normality for the first variable with shapiro-wilk test
shapiro.test(campaigns$advertising_spend_gbp)

# check normality for the second variable with shapiro-wilk test
shapiro.test(campaigns$sales_revenue_gbp)

# qq plots
par(mfrow = c(1, 2))

qqPlot(
  campaigns$advertising_spend_gbp,
  envelope = 0.95,
  main = "Advertising Expenditure",
  xlab = "Theoretical Quantiles",
  ylab = "Advertising Expenditure (£)"
)

qqPlot(
  campaigns$sales_revenue_gbp,
  envelope = 0.95,
  main = "Sales Revenue",
  xlab = "Theoretical Quantiles",
  ylab = "Sales Revenue (£)"
)

par(mfrow = c(1, 1))

# scatterplot with fitted regression line
campaigns %>%
  ggplot(aes(x = advertising_spend_gbp, y = sales_revenue_gbp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Advertising Expenditure and Sales Revenue",
    x = "Advertising Expenditure (£)",
    y = "Sales Revenue (£)"
  ) +
  theme_minimal()

# pearson's correlation
pearson_results <- cor.test(
  campaigns$advertising_spend_gbp,
  campaigns$sales_revenue_gbp,
  method = "pearson",
  alternative = "two.sided"
)

pearson_results
broom::tidy(pearson_results)

# scatterplot with regression line and 95% confidence interval
campaigns %>%
  ggplot(aes(x = advertising_spend_gbp, y = sales_revenue_gbp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Advertising Expenditure and Sales Revenue",
    x = "Advertising Expenditure (£)",
    y = "Sales Revenue (£)"
  ) +
  theme_minimal()

# spearman's rho correlation
spearman_results <- cor.test(
  campaigns$advertising_spend_gbp,
  campaigns$sales_revenue_gbp,
  method = "spearman",
  alternative = "two.sided",
  exact = FALSE
)

spearman_results
broom::tidy(spearman_results)

# scatterplot with trend line and 95% confidence interval
campaigns %>%
  ggplot(aes(x = advertising_spend_gbp, y = sales_revenue_gbp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  labs(
    title = "Advertising Expenditure and Sales Revenue",
    x = "Advertising Expenditure (£)",
    y = "Sales Revenue (£)"
  ) +
  theme_minimal()
