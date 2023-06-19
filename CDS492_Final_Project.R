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
college_reduced[is.na(college_reduced) | college_reduced == "Inf"] = NA
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



college_model <- lm(completion_rate_4yr_100nt ~ admission_rate_overall, 
                       data = college_reduced)
college_model %>%
  tidy()

college_model %>%
  glance() %>%
  select(r.squared)

college_df <- college_reduced %>%
  add_predictions(college_model) %>%
  add_residuals(college_model)

ggplot(college_df) +
  geom_point(mapping = aes(x = completion_rate_4yr_100nt, y = admission_rate_overall)) +
  geom_line(
    mapping = aes(x = completion_rate_4yr_100nt, y = pred),
    color = "indianred3",
    size = 1
  ) +
  geom_linerange(
    mapping = aes(x = completion_rate_4yr_100nt, ymin = pred, ymax = admission_rate_overall),
    linetype = "dashed"
  ) +
  labs(
    title = 'Plot of Residuals of Completion Rate and Admission Rate',
    x = 'Completion Rate (within 4 years)',
    y = 'Admission Rate'
  )
