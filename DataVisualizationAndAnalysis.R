###***Business Analyst Technical 1
# Data collected and analyzed by Will Sivolella
# Data collected using the following resources:
# https://www.fccincinnati.com/schedule/matches#competition=all&date=2021-01-01
# https://www.transfermarkt.us/major-league-soccer/spieltag/wettbewerb/MLS1/plus/?saison_id=2020&spieltag=1
# https://www.wvxu.org/local-news/2021-05-16/fc-cincinnati-kicks-off-stadium-opener-with-loss-to-inter-miami-cf
# https://www.oursportscentral.com/services/releases/fc-cincinnati-announce-sellout-for-tonights-matchup-against-philadelphia-union-at-tql-stadium/n-5944107
# https://www.mlssoccer.com/standings/2023/conference#season=2023&live=true

library(ggplot2)

# First step, set working directory to file location of data set

# Reading in data and assigning to variable

all_matches <- read.csv("AttendanceData.csv")

# Split data into 2021, 2022 data (will later be training data) and 2023 data (will later be test data)

attendance <- subset(all_matches, is.na(Attendance) == FALSE)
proj_attendance <- subset(all_matches, is.na(Attendance) == TRUE)

str(attendance)

# Displays names and indexes of all the variables
names(attendance)

# Each data point is a home game with the following variables:
# "Attendance": quantitative attendance of the match
# "Promotion.Night": Binary ('1' if match is promotion night, '0' if match is not promotion night)
# "Weekend": Binary ('1' if match is on a weekend, '0' if match is not the weekend)
# "Day.of.the.Week": (Monday, Tuesday, or...)
# "FCC.Rank": The position of FC Cincinnati in the Eastern Conference before the matchday (Used to evaluate team strength) 
# "Opp.Ranks": The position of the opponent in their conference before the matchday (Used to evaluate team strength) 
# "Date": The date of the match

# Display of Summary statistics for the attendance
summary(attendance$Attendance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 19365   21212   22557   22705   24188   25701 

# Finding interquartile range to see if there are outliers in the data
iqr <- 24188 - 21212 # = 2976
min <- 21212-1.5*iqr # = 16748
max <- 24188 + 1.5*iqr # = 28652

# No outliers since no attendances at any MLS league match are less than 16748 (min) or above 28652 (max)

hist(attendance$Attendance, main="Attendance Distribution", xlab="Attendance")
# Relationship resembles that of a normal distribution but with a peak in the last bucket

# Now, perform hypothesis test for significance of promotion night.

#Creating subsets of data based on promotion night versus non-promotion night on weekends
promo <- subset(attendance, Promotion.Night == 1)
nopromo <- subset(attendance, Promotion.Night == 0)

# Histogram visualizations of subsets
par(mfrow=c(1,2))
hist(promo$Attendance, main=" Promotion Night Attendance", xlab="Attendance")
# Relatively flat distribution
hist(nopromo$Attendance, main="Non-promotion Night Attendance", xlab="Attendance")
# Distribution has peaks at middle and end bucket

#Calculating and storing sample means
xbar_promo <- mean(promo$Attendance, na.rm = TRUE)
xbar_nopromo <- mean(nopromo$Attendance, na.rm = TRUE)

xbar_promo # = 22590.47
xbar_nopromo # = 22887.17

#Calculating and storing sample standard deviations
sd_promo <- sd(promo$Attendance, na.rm = TRUE)
sd_nopromo <- sd(nopromo$Attendance, na.rm = TRUE)

sd_promo # = 1819.916
sd_nopromo # = 1745.884

#Calculating and storing sample sizes of each subset
n_promo <- nrow(promo)
n_nopromo <- nrow(nopromo)

n_promo # = 19
n_nopromo # = 12

# Two-sample t test to determine if promotion nights have a significant impact on attendance
# Significance level : 5%
# Hypothesis: H_0: mu_promotion_night <= mu_no_promotion_night, H_a: mu_promotion_night > mu_no_promotion_night

#Calculating and storing degrees of freedom for two-sample t test

df_promo <- min(n_promo - 1, n_nopromo - 1)
df_promo # = 11

#Calculating and storing test statistic

t_ts_promo <- (xbar_promo - xbar_nopromo)/(sqrt(((sd_promo^2)/n_promo) + ((sd_nopromo^2)/n_nopromo)))
t_ts_promo # = -0.4533337

#Calculating and storing p-value
pval_promo <- pt(t_ts_promo, df_promo, lower.tail=FALSE)
pval_promo # = 0.6704397

# Conclusion: If the true average attendances of FC Cincinnati MLS home matches 
# on promotion nights are less than or are equal to the true average attendance of FC 
# Cincinnati MLS home matches on non-promotion nights, then there is a 67.04% chance that 
# we would have obtained the samples (sample means) we obtained or samples that are more
# extreme. Since the calculated p-value (0.6704) is greater than the significance level 
# (0.05), there is not sufficient evidence to reject the null hypothesis that states the 
# average attendances of FC Cincinnati MLS home matches on promotion nights is less than or 
# equal to the average attendance of home matches that not a promotion night.

# 95% two-sample t confidence interval for difference in mean attendance of matches on 
# a promotion night versus matches on a night where there is a not a promotion

#Calculating and storing difference in sample means

promo_mean_diff <- xbar_promo - xbar_nopromo
promo_mean_diff # = -296.693

# Calculating and storing t*

t_star_promo <- qt(0.025, df = df_promo)
t_star_promo # = -2.200985

# Calculating and storing margin of error

m_of_e_promo <- t_star_promo*(sqrt(((sd_promo^2)/n_promo) + ((sd_nopromo^2)/n_nopromo)))
m_of_e_promo # = -1440.477

promo_min <- promo_mean_diff + m_of_e_promo
promo_min # = -1737.17

promo_max <- promo_mean_diff - m_of_e_promo
promo_max # = 1143.784

# 95% Confidence interval (CI): (-1737.17, 1143.78)

# Interpretation of (CI): We are 95% confident that the difference in true mean
# attendances for weekend matches on promotion nights and matches not on promotion nights
# is between -1737.17 and 1143.78.

#Creating subsets of data based on weekend versus weekday on promotion nights
wknd <- subset(promo, Weekend == 1)
wkday <- subset(promo, Weekend == 0)

# Histogram visualizations of subsets
par(mfrow=c(1,2))
hist(wknd$Attendance, main="Weekend Attendance", xlab="Attendance")
# Bimodal distribution with peaks at the middle and end buckets
hist(wkday$Attendance, main="Weekday Attendance", xlab="Attendance")
# Unimodal distribution with peak at the middle bucket

#Calculating and storing sample means
xbar_wknd <- mean(wknd$Attendance, na.rm = TRUE)
xbar_wkday <- mean(wkday$Attendance, na.rm = TRUE)

xbar_wknd # = 22772.56
xbar_wkday # = 21619.33

#Calculating and storing sample standard deviations
sd_wknd <- sd(wknd$Attendance, na.rm = TRUE)
sd_wkday <- sd(wkday$Attendance, na.rm = TRUE)

sd_wknd # = 1871.64
sd_wkday # = 1362.408

#Calculating and storing sample sizes of each subset
n_wknd <- nrow(wknd)
n_wkday <- nrow(wkday)

n_wknd # = 16
n_wkday # = 3

# Two-sample t test to determine if matches on weekends versus weekdays have a 
# significant impact on promotion night attendance
# Significance level : 5%
# Hypothesis: H_0: mu_weekend = mu_weekday, H_a: mu_weekend > mu_weekday

#Calculating and storing degrees of freedom for two-sample t test

df_wknd <- min(n_wknd - 1, n_wkday - 1)
df_wknd # = 2

#Calculating and storing test statistic

t_ts_wknd <- (xbar_wknd - xbar_wkday)/(sqrt(((sd_wknd^2)/n_wknd) + ((sd_wkday^2)/n_wkday)))
t_ts_wknd # = 1.260034

#Calculating and storing p-value
pval_wknd <- pt(t_ts_wknd,df_wknd,lower.tail=FALSE)
pval_wknd # = 0.1673825

# Conclusion: If the true average attendances of FC Cincinnati MLS home matches 
# on weekend promotion nights and weeknight promotion nights are equal, then there is a 16.74% chance that we would have 
# obtained the samples (sample means) that we obtained or samples that are more extreme. 
# Since the calculated p-value (0.1674) is greater than the significance level (0.05), 
# there is not sufficient evidence to reject the null hypothesis

# 95% two-sample t confidence interval for difference in mean attendance of matches on 
# weekends versus weekdays:

#Calculating and storing difference in sample means

wknd_mean_diff <- xbar_wknd - xbar_wkday
wknd_mean_diff # = 1153.229

# Calculating and storing t*

t_star_wknd <- qt(0.025, df = df_wknd)
t_star_wknd # = -4.302653

# Calculating and storing margin of error

m_of_wknd <- t_star_wknd*(sqrt(((sd_wknd^2)/n_wknd) + ((sd_wkday^2)/n_wkday)))
m_of_wknd # = -1507.826

wknd_min <- wknd_mean_diff + m_of_wknd
wknd_min # = -2784.716

wknd_max <- wknd_mean_diff - m_of_wknd
wknd_max # = 5091.174

# 95% Confidence interval (CI): (-2784.716, 5091.174)

# Interpretation of (CI): We are 95% confident that the difference in mean
# attendances for matches on weekends versus weekdays is  between -2784.716 and 5091.174.

# Perform ANOVA test for determining significance of team strength

# First, create four subgroups from the promotion night data (high FFC position and high opponent position, 
# high FFC position and low opponent position, low FFC position and high opponent position, 
# low FFC position and low opponent position).  A high position is if the team is in 7th place or higher 
# in their conference.  The threshold is determined based on who would qualify for the first round of the 
# playoffs if the season ended before that matchday (wildcard spots not included).

hh <- subset(wknd, FCC.Rank <= 7 & Opp.Rank <=7)
hl <- subset(wknd, FCC.Rank <= 7 & Opp.Rank > 7)
lh <- subset(wknd, FCC.Rank > 7 & Opp.Rank <= 7)
ll <- subset(wknd, FCC.Rank > 7 & Opp.Rank > 7)


# Histogram visualizations of subsets
par(mfrow=c(2,2))
hist(hh$Attendance, main="HH Attendance", xlab="Attendance")
hist(hl$Attendance, main="HL Attendance", xlab="Attendance")
hist(lh$Attendance, main="LH Attendance", xlab="Attendance")
hist(ll$Attendance, main="LL Attendance", xlab="Attendance")

# Setup data for ANOVA test

Strength <- c(rep("hh", 3), rep("hl", 2), rep("lh", 5), rep("ll", 6))
Attendance <- c(hh[,2], hl[,2], lh[,2], ll[,2])

df <- data.frame(Strength, Attendance)
df

# Boxplot Visualization of Data 
ggplot(df, aes(x = Strength, 
               y = Attendance, 
               fill = Strength)) + 
  geom_boxplot()

# ANOVA test: compares means between all four groups and determines whether any of 
# these means are statistically different from each other.
# H0: mu_hh = mu_hl = mu_lh = mu_ll, Ha: At least one mean is significantly different 
# Significance level = 0.05

attendance.aov <- aov(Attendance ~ Strength, data = df)
summary(attendance.aov)

#             Df Sum Sq Mean Sq F value Pr(>F)
# Strength     3  8395850 2798617   0.761  0.537
# Residuals   12 44149722 3679144   

# P-val = 0.537, which is greater than our significance level of 0.05. Thus, we do not
# sufficient evidence to reject the null hypotheses, which states that the means of each 
# group are equal. According to the ANOVA test, team performance (both FCC & opponent) 
# does not have a statistically significant impact on attendance.
# Note: small sample sizes may have an impact on results.

# Now, test if just FCC performance has a statistically significant impact on promotion night
# attendance. Again, divide data into subsets by "high" and "low" FCC performance based on if they are 
# in a playoff spot at the time. 

fcc_high <- subset(wknd, FCC.Rank <= 7)
fcc_low <- subset(wknd, FCC.Rank > 7)

# Visualize data with histograms
# Histogram visualizations of subsets
par(mfrow=c(1,2))
hist(fcc_high$Attendance, main="Attendance When FCC is Performing Well", xlab="Attendance")
# Relatively flat distribution
hist(fcc_low$Attendance, main="Attendance When FCC is Performing Poorly", xlab="Attendance")
# Distribution is has two peaks in middle bicket and end bucket

#Calculating and storing sample means
xbar_fcc_high <- mean(fcc_high$Attendance, na.rm = TRUE)
xbar_fcc_low <- mean(fcc_low$Attendance, na.rm = TRUE)

xbar_fcc_high # = 23732.6
xbar_fcc_low # = 22336.18

#Calculating and storing sample standard deviations
sd_fcc_high <- sd(fcc_high$Attendance, na.rm = TRUE)
sd_fcc_low <- sd(nopromo$Attendance, na.rm = TRUE)

sd_fcc_high # = 1582.527
sd_fcc_low # = 1745.884

#Calculating and storing sample sizes of each subset
n_fcc_high <- nrow(fcc_high)
n_fcc_low  <- nrow(fcc_low)

n_fcc_high # = 5
n_fcc_low # = 11

# Two-sample t test to determine if FCC team performance has a significant impact on promotion night attendance
# Significance level : 5%
# Hypothesis: H_0: mu_fcc_high = mu_fcc_low, H_a: mu_fcc_high > mu_fcc_low

#Calculating and storing degrees of freedom for two-sample t test

df_fcc <- min(n_fcc_high - 1, n_fcc_low - 1)
df_fcc # = 4

#Calculating and storing test statistic

t_ts_fcc <- (xbar_fcc_high - xbar_fcc_low)/(sqrt(((sd_fcc_high^2)/n_fcc_high) + ((sd_fcc_low^2)/n_fcc_low)))
t_ts_fcc # = 1.583185

#Calculating and storing p-value
pval_fcc <- pt(t_ts_fcc, df_fcc, lower.tail=FALSE)
pval_fcc # = 0.09427421

# Conclusion: If the true average attendances of FC Cincinnati MLS home weekend matches 
# when they are doing well (in a playoff spot) is equal to the true average 
# attendance of FC Cincinnati MLS home matches when they are doing poorly (not in a playoff spot), 
# then there is a 9.4% chance that we would have obtained the samples (sample means) we obtained 
# or samples that are more extreme. Since the calculated p-value (0.094) is greater than the 
# significance level (0.05), there is not sufficient evidence to reject the null hypothesis.

# If we were working with a more generous significance level, then would have had sufficient
# evidence to reject the null hypotheses.

# 95% two-sample t confidence interval for difference in mean attendance of matches when FCC
# is doing well (in a playoff spot) versus doing poorly (not in a playoff spot)

#Calculating and storing difference in sample means

fcc_mean_diff <- xbar_fcc_high - xbar_fcc_low
fcc_mean_diff # = 1396.418

# Calculating and storing t*

t_star_fcc <- qt(0.025, df = df_fcc)
t_star_fcc # = -2.776445

# Calculating and storing margin of error

m_of_e_fcc <- t_star_fcc*(sqrt(((sd_fcc_high^2)/n_fcc_high) + ((sd_fcc_low^2)/n_fcc_low)))
m_of_e_fcc # = -2448.911

fcc_min <- fcc_mean_diff + m_of_e_fcc
fcc_min # = -1052.493

fcc_max <- fcc_mean_diff - m_of_e_fcc
fcc_max # = 3845.329

# 95% Confidence interval (CI): (-1052.493, 3845.329)

# Interpretation of (CI): We are 95% confident that the difference in true mean
# attendances for promotion night matches when FCC is doing well (in a playoff spot) and 
# matches when they are doing poorly (not in a playoff spot) is between -1052.493 
# and 3845.329.

# Creating match-by-match attendance forecast model for 2023 season
# Linear Regression:

attendance # is training data 
proj_attendance # projected attendance data for forecast model

# Decided to exclude Promotion.Night variable because it will only assume
# that promotion nights hurt attendance based on findings from significance test

regression <- lm(Attendance ~ Weekend + FCC.Rank + Opp.Rank, 
                   data = attendance)
reg_sum <- summary(regression)
reg_sum

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3458.2  -539.5  -357.6   862.9  2808.2 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 22480.41    1296.94  17.333  3.7e-16 ***
#  Weekend      1995.23     740.86   2.693    0.012 *  
#  FCC.Rank      -98.15      92.85  -1.057    0.300    
#Opp.Rank      -55.91      74.41  -0.751    0.459    
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1625 on 27 degrees of freedom
#Multiple R-squared:  0.2396,	Adjusted R-squared:  0.1551 
#F-statistic: 2.835 on 3 and 27 DF,  p-value: 0.05689
  
# If predicting based on weekend, +1995.23 to intercept (when Weekend = 1)
# If predicting based on if FCC table position, -98.15*position to intercept 
# If predicting based on if Opp table position,  -55.91*position to intercept 
# "Weekend" is the only variable that is statistically significant 

#Create Forecast Model:

#forecast <- predict(regression, proj_attendance)
predict(regression, proj_attendance)

forecast <- predict(regression, proj_attendance)
forecast

Date_of_Match <- proj_attendance[,8]

Projected_Attendance <- forecast

df_forecast <- data.frame(Date_of_Match, Projected_Attendance = forecast)

ggplot(df_forecast, aes(x = df_forecast$Date_of_Match, 
                    y = df_forecast$Projected_Attendance)) + 
     geom_point() + 
     labs(title = "Attendance Forecast Model for 2023 Season", xlab = "Date of Match", ylab = "Projected Attendance")

# Export df_forecast for forecast model
# Then import the data to tableau 
