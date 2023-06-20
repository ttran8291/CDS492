# packages I needed for my visualizations
library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)
library(modelr)

# I renamed the dataset into something shorter and understandable
college <- read.csv('Recent_College_Data.csv') 

# filtered and selected out the columns I need for my analysis
college_reduced <- college %>% 
  select(INSTNM, CITY, STABBR, REGION, ADM_RATE, C100_4)

# removes any rows with missing values
college_reduced[is.na(college_reduced) | college_reduced == "NULL"] = NA
college_reduced <- na.omit(college_reduced)


# renamed the columns so it's more understandable
college_reduced <- college_reduced %>% 
  rename(admission_rate_overall = 'ADM_RATE',
         completion_rate_4yr_100nt = 'C100_4',
         institution_name = 'INSTNM',
         city = 'CITY', 
         state = 'STABBR',
         region = 'REGION')

# grouped regions by number so that it will be easier to visualize later
college_reduced <- college_reduced %>% 
  mutate(
    regions_grouped = recode(region,
        `1` = 'New England (CT, ME, MA, NH, RI, VT)',
        `2` = 'Mid East (DE, DC, MD, NJ, NY, PA)',
        `3` = 'Great Lakes (IL, IN, MI, OH, WI)',
        `4` = 'Plains (IA, KS, MN, MO, NE, ND, SD)',
        `5` = 'Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)',
        `6` = 'Southwest (AZ, NM, OK, TX)',
        `7` = 'Far West (AK, CA, HI, NV, OR, WA)',
        `8` = 'Outlying Areas (AS, FM, GU, MH, MP, PR, PW, VI)'
      )
  )

# This creates my first visualization, which is a scatterplot of all the regions
# for the completion rate and admission rate variables to see how these variables 
# correlate to one another
college_reduced %>% 
  ggplot() +
  geom_point(mapping = aes(y = completion_rate_4yr_100nt,
                           x = admission_rate_overall)
             ) +
  facet_wrap(~regions_grouped, scales ='free_x') +
  labs(
    title = 'Scatterplots of College Admission Rate vs Completion Rates
              in Different Regions',
    x = 'Overally Admission Rate',
    y = 'Completion Rate (within 4 years)'
  )


# this creates a linear regression model
college_model <- lm(completion_rate_4yr_100nt ~ admission_rate_overall, 
                       data = college_reduced)
college_model %>%
  tidy()

#finds the r squared value for the regression line which was found to be 0.877
college_model %>%
  glance() %>%
  select(r.squared)

# adds a column for residual and prediction values and tells us if the model is
# appropriate for linear regression. In this case it is since points are scattered
# evenly above and below the residual line
college_reduced$residuals <- residuals(college_model)
college_reduced$predictions <- predict(college_model)

# creates a predicted and residuals plot
college_reduced %>%
  ggplot() +
  geom_point(mapping = aes(x = predictions, y = residuals)) +
  geom_hline(aes(yintercept = 0)) + 
  labs(title = "Residual vs Predicted Plot
                of College Model Data",
       x = 'Predicted',
       y = "Residuals")

# creates a linear regression plot
ggplot(data = college_reduced, aes(x = admission_rate_overall, y = completion_rate_4yr_100nt)) +
  geom_point() +
  geom_abline(intercept = coef(college_model)[1], slope = coef(college_model)[2], color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Admission Rate", y = "Completion Rate (within 4 years)", title = "Linear Regression Plot")