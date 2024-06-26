library(readxl)
library(plm)
library(dplyr)
library(car)
library(lmtest)
library(sandwich)
library(stargazer)

# Setting the Working Directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()

################################################################################

# Importing Dataset and Feature Engineering/Selection

df <- read_excel("final_dataset_2.xlsx", na = "..")

# Standardize net_migration
df$net_migration <- scale(df$net_migration)

# Convert 'year' column to Date if it's not already in Date format
#df$time <- as.Date(paste0(df$time, "-01-01"), format = "%Y-%m-%d")
# Filter out years from 1980 to 2000
#df <- df %>%
#  filter(time < as.Date("1980-01-01") | time > as.Date("1999-12-31"))

# Show the filtered data frame
str(df)



#df <- df[, !(names(df) %in% "export_index")]
df <- df[, !(names(df) %in% "med_high_exports")]
df <- df[, !(names(df) %in% "fuel_exports")]
df <- df[, !(names(df) %in% "goodsservices_exports")]
#df <- df[, !(names(df) %in% "demography")]
df <- df[, !(names(df) %in% "edu_expend")]
df <- df[, !(names(df) %in% "lit_rate")]
#df <- df[, !(names(df) %in% "pop_growth")]
#df <- df[, !(names(df) %in% "net_migration")]
df <- df[, !(names(df) %in% "comp_edu")]
#df <- df[, !(names(df) %in% "gross_fixed_capital_formation")]
#df <- df[, !(names(df) %in% "gdp_growth")]
df <- df[, !(names(df) %in% "gross_fixed_growth")]
#df <- df[, !(names(df) %in% "rural_pop_growth")]
#df <- df[, !(names(df) %in% "gdp_capita")]
#df <- df[, !(names(df) %in% "urban_pop_growth")]
#df <- df[, !(names(df) %in% "crop_production_index")]
#df <- df[, !(names(df) %in% "inflation_deflator")]
#df <- df[, !(names(df) %in% "oil_rents")]
#df <- df[, !(names(df) %in% "broad_money")]
#df <- df[, !(names(df) %in% "broad_money_growth")]
#df <- df[, !(names(df) %in% "labor_participation")]

df <- na.omit(df)

str(df)


#numerical_cols <- sapply(df, is.numeric)
#numerical_data <- df[, numerical_cols]
#standardized_data <- as.data.frame(scale(numerical_data))
#non_numerical_data <- df[, !numerical_cols]
#df <- cbind(non_numerical_data, standardized_data)
df <- df[order(df$country, df$time),]
df$time <- as.factor(df$time)

df$region <- as.factor(df$region)
df$income_group <- as.factor(df$income_group)
df$country <- as.factor(df$country)

df$region <- relevel(df$region, ref = "Europe & Central Asia")
df$income_group <- relevel(df$income_group, ref = "Lower middle income")



df_high <- df[df$income_group == "High income", ]
df_up <- df[df$income_group == "Upper middle income", ]
df_lower <- df[df$income_group == "Lower middle income", ]
df_low <- df[df$income_group == "Low income", ]
combined_df <- rbind(df_up, df_lower)

# Setting Base Countries
df$country <- relevel(df$country, ref = "Australia")
df_high$country <- relevel(df_high$country, ref = "United States")
df_up$country <- relevel(df_up$country, ref = "Brazil")
df_lower$country <- relevel(df_lower$country, ref = "India")
df_low$country <- relevel(df_low$country, ref = "Haiti")
################################################################################

# Model Creation

# Pooled models
pooled_df <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                 + urban_pop_growth 
                 + rural_pop_growth + net_migration + crop_production_index
                 + oil_rents + broad_money + labor_participation
                 + export_index + demography,
                 data = df, index = c("country", "time"), model = "pooling")

pooled_high <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                   + urban_pop_growth 
                   + rural_pop_growth + net_migration + crop_production_index
                   + oil_rents + broad_money + labor_participation
                   + export_index + demography,
                   data = df_high, index = c("country", "time"), model = "pooling")

pooled_up <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                 + urban_pop_growth 
                 + rural_pop_growth + net_migration + crop_production_index
                 + oil_rents + broad_money + labor_participation
                 + export_index + demography,
                   data = df_up, index = c("country", "time"), model = "pooling")

pooled_lower <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                    + urban_pop_growth 
                    + rural_pop_growth + net_migration + crop_production_index
                    + oil_rents + broad_money + labor_participation
                    + export_index + demography,
                   data = df_lower, index = c("country", "time"), model = "pooling")

pooled_low<- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                 + urban_pop_growth 
                 + rural_pop_growth + net_migration + crop_production_index
                 + oil_rents + broad_money + labor_participation
                 + export_index + demography,
                   data = df_low, index = c("country", "time"), model = "pooling")
df$region <- as.factor(df$region)
levels(df$region)
# Summaries
summary(pooled_df)
vif(pooled_df)
summary(pooled_high)
vif(pooled_high)
summary(pooled_up)
vif(pooled_up)
summary(pooled_lower)
vif(pooled_lower)
summary(pooled_low)
vif(pooled_low)

# Calculating Robust Standard Errors

robust_se_df <- coeftest(pooled_df, vcov = vcovHC(pooled_df, type = 'HC0'))
robust_se_high <- coeftest(pooled_high, vcov = vcovHC(pooled_high, type = 'HC0'))
robust_se_up <- coeftest(pooled_up, vcov = vcovHC(pooled_up, type = 'HC0'))
robust_se_lower <- coeftest(pooled_lower, vcov = vcovHC(pooled_lower, type = 'HC0'))
robust_se_low <- coeftest(pooled_low, vcov = vcovHC(pooled_low, type = 'HC0'))

#Two Way LSDV

lsdv_df <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
              + urban_pop_growth 
              + rural_pop_growth + net_migration + crop_production_index
              + oil_rents + broad_money + labor_participation
              + export_index + demography + country + time, data = df
              , index = c("country", "time"), model = "pooling")

lsdv_high <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                + urban_pop_growth 
                + rural_pop_growth + net_migration + crop_production_index
                + oil_rents + broad_money + labor_participation
                + export_index + demography + country + time, data = df_high
                , index = c("country", "time"), model = "pooling")

lsdv_up <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
              + urban_pop_growth 
              + rural_pop_growth + net_migration + crop_production_index
              + oil_rents + broad_money + labor_participation
              + export_index + demography + country + time,
               data = df_up, index = c("country", "time"), model = "pooling")

lsdv_lower <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                 + urban_pop_growth 
                 + rural_pop_growth + net_migration + crop_production_index
                 + oil_rents + broad_money + labor_participation
                 + export_index + demography + country + time, data = df_lower
                 , index = c("country", "time"), model = "pooling")


lsdv_low <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
               + urban_pop_growth 
               + rural_pop_growth + net_migration + crop_production_index
               + oil_rents + broad_money + labor_participation
               + export_index + demography + country + time,
                data = df_low, index = c("country", "time"), model = "pooling")

# Summaries
summary(lsdv_df)
vif(lsdv_df)
summary(lsdv_high)
vif(lsdv_high)
summary(lsdv_up)
vif(lsdv_up)
summary(lsdv_lower)
vif(lsdv_lower)
summary(lsdv_low)
vif(lsdv_low)

# Calculating Robust Standard Errors for all models

robust_lsdv_df <- coeftest(lsdv_df, vcov = vcovHC(lsdv_df, type = 'HC0'))
robust_lsdv_high <- coeftest(lsdv_high, vcov = vcovHC(lsdv_high, type = 'HC0'))
robust_lsdv_up <- coeftest(lsdv_up, vcov = vcovHC(lsdv_up, type = 'HC0'))
robust_lsdv_lower <- coeftest(lsdv_lower, vcov = vcovHC(lsdv_lower, type = 'HC0'))
robust_lsdv_low <- coeftest(lsdv_low, vcov = vcovHC(lsdv_low, type = 'HC0'))


#Within Group Estimator Model
fe_df <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
             + urban_pop_growth 
             + rural_pop_growth + net_migration + crop_production_index
             + oil_rents + broad_money + labor_participation
             + export_index + demography,
                data = df,index = c("country", "time"), 
                model = "within", effect= "twoway")

fe_high <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
               + urban_pop_growth 
               + rural_pop_growth + net_migration + crop_production_index
               + oil_rents + broad_money + labor_participation
               + export_index + demography,
             data = df_high,index = c("country", "time"), 
             model = "within", effect= "twoway")

fe_up <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
             + urban_pop_growth 
             + rural_pop_growth + net_migration + crop_production_index
             + oil_rents + broad_money + labor_participation
             + export_index + demography,
             data = df_up,index = c("country", "time"), 
             model = "within", effect= "twoway")

fe_lower <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                + urban_pop_growth 
                + rural_pop_growth + net_migration + crop_production_index
                + oil_rents + broad_money + labor_participation
                + export_index + demography,
             data = df_lower,index = c("country", "time"), 
             model = "within", effect= "twoway")

fe_low <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
              + urban_pop_growth 
              + rural_pop_growth + net_migration + crop_production_index
              + oil_rents + broad_money + labor_participation
              + export_index + demography,
             data = df_low,index = c("country", "time"), 
             model = "within", effect= "twoway")

fe_combined <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
              + urban_pop_growth 
              + rural_pop_growth + net_migration + crop_production_index
              + oil_rents + broad_money + labor_participation
              + export_index + demography,
              data = combined_df, index = c("country", "time"), 
              model = "within", effect= "twoway")

# Summaries
summary(fe_df)
summary(fe_high)
summary(fe_up)
summary(fe_lower)
summary(fe_low)
summary(fe_combined)
# Random Effects Model
re_df <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
             + urban_pop_growth 
             + rural_pop_growth + net_migration + crop_production_index
             + oil_rents + labor_participation
             + export_index + demography + region,
                data = df,index = c("country", "time"), 
              model = "random", effect= "individual")

re_high <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
               + urban_pop_growth 
               + rural_pop_growth + net_migration + crop_production_index
               + oil_rents + broad_money + labor_participation
               + export_index + demography + region,
             data = df_high,index = c("country", "time"), 
             model = "random", effect= "individual")

re_up <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
             + urban_pop_growth 
             + rural_pop_growth + net_migration + crop_production_index
             + oil_rents + broad_money + labor_participation
             + export_index + demography + region,
             data = df_up,index = c("country", "time"), 
             model = "random", effect= "individual")

re_lower <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                + urban_pop_growth 
                + rural_pop_growth + net_migration + crop_production_index
                + oil_rents + broad_money + labor_participation
                + export_index + demography + region,
             data = df_lower,index = c("country", "time"), 
             model = "random", effect= "individual")

re_low <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
              + urban_pop_growth 
              + rural_pop_growth + net_migration + crop_production_index
              + oil_rents + broad_money + labor_participation
              + export_index + demography + region,
             data = df_low,index = c("country", "time"), 
             model = "random", effect= "individual")

re_combined <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
             + urban_pop_growth 
             + rural_pop_growth + net_migration + crop_production_index
             + oil_rents + broad_money + labor_participation
             + export_index + demography + region,
             data = combined_df,index = c("country", "time"), 
             model = "random", effect= "individual")

summary(re_df)
vif(re_df)
summary(re_high)
vif(re_high)
summary(re_up)
vif(re_up)
summary(re_lower)
vif(re_lower)
summary(re_low)
vif(re_low)
summary(re_combined)


#First difference
fd_df <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
             + urban_pop_growth 
             + rural_pop_growth + net_migration + crop_production_index
             + oil_rents + broad_money + labor_participation
             + export_index + demography,
                data = df,index = c("country", "time"), 
                model = "fd")

fd_high <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
               + urban_pop_growth 
               + rural_pop_growth + net_migration + crop_production_index
               + oil_rents + broad_money + labor_participation
               + export_index + demography,
             data = df_high,index = c("country", "time"), 
             model = "fd")

fd_up <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
             + urban_pop_growth 
             + rural_pop_growth + net_migration + crop_production_index
             + oil_rents + broad_money + labor_participation
             + export_index + demography,
             data = df_up,index = c("country", "time"), 
             model = "fd")

fd_lower <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
                + urban_pop_growth 
                + rural_pop_growth + net_migration + crop_production_index
                + oil_rents + broad_money + labor_participation
                + export_index + demography,
             data = df_lower,index = c("country", "time"), 
             model = "fd")

fd_low <- plm(unemployment ~ gross_fixed_capital_formation + gdp_growth+ inflation_deflator
              + urban_pop_growth 
              + rural_pop_growth + net_migration + crop_production_index
              + oil_rents + broad_money + labor_participation
              + export_index + demography,
             data = df_low,index = c("country", "time"), 
             model = "fd")


# Summaries
summary(fd_df)
vif(fd_df)
summary(fd_high)
vif(fd_high)
summary(fd_up)
vif(fd_up)
summary(fd_lower)
vif(fd_lower)
summary(fd_low)
vif(fd_low)

################################################################################

#Hausman Test
phtest(lsdv_df,re_df)
phtest(lsdv_high,re_high)
phtest(lsdv_up,re_up)
phtest(lsdv_lower,re_lower)
phtest(lsdv_low,re_low)


#F-Test
plmtest(pooled_df, effect = 'time', type= 'honda')
plmtest(pooled_df, effect = 'individual', type= 'honda')
plmtest(pooled_high, effect = 'time', type= 'honda')
plmtest(pooled_high, effect = 'individual', type= 'honda')
plmtest(pooled_up, effect = 'time', type= 'honda')
plmtest(pooled_up, effect = 'individual', type= 'honda')
plmtest(pooled_lower, effect = 'time', type= 'honda')
plmtest(pooled_lower, effect = 'individual', type= 'honda')
plmtest(pooled_low, effect = 'time', type= 'honda')
plmtest(pooled_low, effect = 'individual', type= 'honda')
  
#Hettest
bptest(pooled_df)
bptest(pooled_high)
bptest(pooled_up)
bptest(pooled_lower)
bptest(pooled_low)

################################################################################

# POOLED OLS
#Normal SE
stargazer(pooled_df, pooled_high, pooled_up, pooled_lower, pooled_low, digits = 2, 
          title = 'POOLED OLS Results', 
          dep.var.labels=c("Y = Unemployment"),
          column.labels=c("Global","High Income","UpperMiddle Income","LowerMiddle Income","Low Income"),
          ci = FALSE, font.size = 'small', intercept.top = TRUE, intercept.bottom = FALSE,
          style = 'qje', out = 'pooledtest2.html')

# Robust SE
stargazer(
  pooled_df, pooled_high, pooled_up, pooled_lower, pooled_low, 
  digits = 2, 
  title = 'POOLED OLS Results', 
  dep.var.labels = c("Y = Unemployment"),
  column.labels = c("Global", "High Income", "UpperMiddle Income", "LowerMiddle Income", "Low Income"),
  ci = FALSE, 
  font.size = 'small', 
  intercept.top = TRUE, 
  intercept.bottom = FALSE,
  style = 'qje', 
  out = 'pooledtestfinal.html',
  se = list(robust_se_df[, "Std. Error"], 
            robust_se_high[, "Std. Error"], 
            robust_se_up[, "Std. Error"], 
            robust_se_lower[, "Std. Error"], 
            robust_se_low[, "Std. Error"])
)

# Two-Way LSDV
# Normal SE
stargazer(lsdv_df, lsdv_high, lsdv_up, lsdv_lower, lsdv_low, digits = 2, 
          title = 'Two-Way LSDV Results', 
          dep.var.labels=c("Y = Unemployment"),
          column.labels=c("Global","High Income","UpperMiddle Income","LowerMiddle Income","Low Income"),
          ci = FALSE, font.size = 'small', intercept.top = TRUE, intercept.bottom = FALSE,
          style = 'qje', out = 'lsdvtest2.html')

# Robust SE
stargazer(
  lsdv_df, lsdv_high, lsdv_up, lsdv_lower, lsdv_low, 
  digits = 2, 
  title = 'Two-Way LSDV Results', 
  dep.var.labels = c("Y = Unemployment"),
  column.labels = c("Global", "High Income", "UpperMiddle Income", "LowerMiddle Income", "Low Income"),
  ci = FALSE, 
  font.size = 'small', 
  intercept.top = TRUE, 
  intercept.bottom = FALSE,
  style = 'qje', 
  out = 'lsdvtestfinal.html',
  se = list(robust_lsdv_df[, "Std. Error"], 
            robust_lsdv_high[, "Std. Error"], 
            robust_lsdv_up[, "Std. Error"], 
            robust_lsdv_lower[, "Std. Error"], 
            robust_lsdv_low[, "Std. Error"])
)

stargazer(fe_df, fe_high, fe_up, fe_lower, fe_low, digits = 2, 
          title = 'Within Group Estimator Results', 
          dep.var.labels=c("Y = Unemployment"),
          column.labels=c("Global","High Income","UpperMiddle Income","LowerMiddle Income","Low Income"),
          ci = FALSE, font.size = 'small', intercept.top = TRUE, intercept.bottom = FALSE,
          style = 'qje', out = 'fetest2.html')

stargazer(re_df, re_high, re_up, re_lower, re_low, digits = 2, 
          title = 'Random Effects Resultss', 
          dep.var.labels=c("Y = Unemployment"),
          column.labels=c("Global","High Income","UpperMiddle Income","LowerMiddle Income","Low Income"),
          ci = FALSE, font.size = 'small', intercept.top = TRUE, intercept.bottom = FALSE,
          style = 'qje', out = 'retest2.html')

stargazer(fd_df, fd_high, fd_up, fd_lower, fd_low, digits = 2, 
          title = 'First Differencing Results', 
          dep.var.labels=c("Y = Unemployment"),
          column.labels=c("Global","High Income","UpperMiddle Income","LowerMiddle Income","Low Income"),
          ci = FALSE, font.size = 'small', intercept.top = TRUE, intercept.bottom = FALSE,
          style = 'qje', out = 'fdtest2.html')

# Load necessary libraries
library(ggplot2)
library(dplyr)

levels(df$income_group)
# Assuming your data frame is named 'panel_data' with columns 'country', 'year', 'unemployment', and 'income_group'

library(ggplot2)

# Custom color palette for income groups
income_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

# Group by income group and calculate average unemployment rate for each year
average_unemployment <- df %>%
  group_by(time, income_group) %>%
  summarise(avg_unemployment = mean(unemployment))http://127.0.0.1:44165/graphics/plot_zoom_png?width=1920&height=997

# Convert 'year' to a factor to ensure it's treated as categorical
average_unemployment$time <- as.factor(average_unemployment$time)

# Create line plot
ggplot(average_unemployment, aes(x = time, y = avg_unemployment, group = income_group, color = income_group)) +
  geom_line(size = 1) +  # Increase line thickness
  labs(title = "Average Unemployment Rate by Income Group",
       x = "Year",
       y = "Unemployment Rate",
       color = "Income Group") +  # Update legend title
  scale_color_manual(values = income_colors) +  # Apply custom colors
  theme_minimal() +  # Use minimal theme
  theme(legend.position = "bottom",  # Move legend to bottom
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),  # Customize major y grid lines
        plot.title = element_text(hjust = 0.5),  # Center plot title
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
levels(df$region)

library(ggplot2)

# Custom color palette for regions
region_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2")

# Group by region and calculate average unemployment rate for each year
average_unemployment <- df %>%
  group_by(time, region) %>%
  summarise(avg_unemployment = mean(unemployment))

# Convert 'year' to a factor to ensure it's treated as categorical
average_unemployment$time <- as.factor(average_unemployment$time)

# Create line plot
ggplot(average_unemployment, aes(x = time, y = avg_unemployment, group = region, color = region)) +
  geom_line(size = 1) +  # Increase line thickness
  labs(title = "Average Unemployment Rate by Region",
       x = "Year",
       y = "Unemployment Rate",
       color = "Region") +  # Update legend title
  scale_color_manual(values = region_colors) +  # Apply custom colors
  theme_minimal() +  # Use minimal theme
  theme(legend.position = "bottom",  # Move legend to bottom
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),  # Customize major y grid lines
        plot.title = element_text(hjust = 0.5),  # Center plot title
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Create boxplot
ggplot(df, aes(x = income_group, y = gdp_capita)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Unemployment Rate by Income Group",
       x = "Income Group",
       y = "Unemployment Rate") +
  theme_minimal()


ggplot(df, aes(x = gdp_capita, y = unemployment, color = income_group)) +
  geom_point() +
  labs(title = "Unemployment Rate vs. GDP per Capita",
       x = "GDP per Capita",
       y = "Unemployment Rate",
       color = "Region") +
  theme_minimal()

stargazer(df, out = 'df.html')
