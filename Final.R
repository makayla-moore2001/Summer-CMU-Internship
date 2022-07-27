# Correlation of different predictors and teen births

# Load data ----

library(tidyverse)
library(vip)
county_rankings <- read.csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/optum_projects/county_rankings.csv")
teen_births <- read_csv("https://data.cdc.gov/api/views/y268-sna3/rows.csv?accessType=DOWNLOAD")
overdoses <- read.csv("~/Summer 2022 CMU Final Project/Drug alcohol Overdose.csv")
alcohol_use <- read.csv("C:\\Users\\makay\\Documents\\Summer 2022 CMU Final Project\\Alcohol Use.csv")

#Renaming drugs in overdose data ----
overdoses <- overdoses %>%
  mutate(ICD.Code.Description = fct_recode(ICD.Code.Description, 
                                           "Unknown Substance" = "X64  Intentional self-poisoning by/exposure to other and unspecified drugs, medicaments, and biological substances",
                                           "Gases/Vapors" = "X67  Intentional self-poisoning (suicide) by and exposure to other gases and vapors",
                                           "Pyschotic" = "X61  Inten. self-poisoning by/exposure to antiepileptic,sedative-hypnotic,antiparkinsonism,&psychotropic drugs,NEC",
                                           "Narcotics" = "X62  Intentional self-poisoning (suicide) by and exposure to narcotics and psychodysleptics [hallucinogens],NEC",
                                           "Anti-Inflammatory" = "X60  Intentional self-poisoning (suicide) by and exposure to nonopioid analgesics, antipyretics, and antirheumatics",
                                           "Noxious Subsatnces" = "X69  Intentional self-poisoning (suicide) by and exposure to other and unspecified chemicals and noxious substances",
                                           "Neuropathic " = "X63  Intentional self-poisoning (suicide) by and exposure to other drugs acting on the autonomic nervous system",
                                           "Alcohol" = "X65  Intentional self-poisoning (suicide) by and exposure to alcohol",
                                           "Hydrocarbons" = "X66  Intentional self-poisoning by and exposure to organic solvents and halogenated hydrocarbons and their vapors",
                                           "Pesticides " = "X68  Intentional self-poisoning (suicide) by and exposure to pesticides"))

overdoses

# Selecting columns needed ---- 

county_rankings <- county_rankings %>%
  filter(!County.FIPS.Code %in% 0) %>%
  select(Name, Teen.births.raw.value, Teen.births..Asian.Pacific.Islander.,
         Teen.births..Black., Teen.births..Hispanic., Teen.births..White., Excessive.drinking.raw.value,
         Unemployment.raw.value, Children.in.poverty.raw.value,
         Children.in.single.parent.households.raw.value, Poor.mental.health.days.raw.value,
         Adult.smoking.raw.value)
county_rankings[is.na(county_rankings)] = 0

# Adding a total column to add up the teen births percentage by race 

county_rankings <- county_rankings %>%
  mutate(Total = Teen.births..Black. + Teen.births..White. + Teen.births..Hispanic. + 
           Teen.births..Asian.Pacific.Islander.)



# Scatter plot of teen births and excessive drinking ----

county_rankings %>%
  filter(!Teen.births.raw.value %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births.raw.value, y = Excessive.drinking.raw.value)) +
  theme_bw()


# Creating scatter plots with correlations of each predictor with teen births with the race of Black ----

# Scatter plot of teen births (Black) and excessive drinking 

county_rankings %>%
  filter(!Teen.births..Black. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Black., y = Excessive.drinking.raw.value)) +
  theme_bw()

# Scatter plot of teen births (Black) and unemployment 

county_rankings %>%
  filter(!Teen.births..Black. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Black., y = Unemployment.raw.value)) +
  theme_bw()

#Scatter plot of teen births (Black) and children in poverty 

county_rankings %>%
  filter(!Teen.births..Black. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Black., y = Children.in.poverty.raw.value)) +
  theme_bw()

# Scatter plot of teen births (Black) and children in single parent households 

county_rankings %>%
  filter(!Teen.births..Black. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Black., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births (Black) and poor mental health days 

county_rankings %>%
  filter(!Teen.births..Black. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Black., y = Poor.mental.health.days.raw.value)) +
  theme_bw()

# Scatter plot of teen births (Black) and adult smoking 

county_rankings %>%
  filter(!Teen.births..Black. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Black., y = Adult.smoking.raw.value)) +
  theme_bw()

# Creating scatter plots with correlations of each predictor with teen births with the race of White ----

# Scatter plot of teen births and excessive drinking

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Excessive.drinking.raw.value)) +
  theme_bw()

# Scatter plot of teen births and unemployment 

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Excessive.drinking.raw.value)) +
  theme_bw()

# Scatter plot of teen births and children in poverty 

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Children.in.poverty.raw.value)) +
  theme_bw()

# scatter plot of teen births and children in single parent households 

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births and poor mental health days 

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Poor.mental.health.days.raw.value)) +
  theme_bw()

# Scatter plot of teen births and adult smoking 

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Adult.smoking.raw.value))+
  theme_bw()

# Creating scatter plots with correlations of each predictor with teen births with the race of Hispanic ----

# Scatter plot of teen births and excessive drinking 

county_rankings %>%
  filter(!Teen.births..Hispanic. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Hispanic., y = Excessive.drinking.raw.value)) +
  theme_bw()

#Scatter plot of teen births and unemployment

county_rankings %>%
  filter(!Teen.births..Hispanic. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Hispanic., y = Unemployment.raw.value)) +
  theme_bw()

# Scatter plot of teen births and children in poverty 

county_rankings %>%
  filter(!Teen.births..Hispanic. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Hispanic., y = Children.in.poverty.raw.value)) +
  theme_bw()

# Scatter plot of teen births and children in single parent households

county_rankings %>%
  filter(!Teen.births..Hispanic. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Hispanic., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births and poor mental health days

county_rankings %>%
  filter(!Teen.births..Hispanic. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Hispanic., y = Poor.mental.health.days.raw.value)) +
  theme_bw()

# Scatter plot of teen births and adult smoking

county_rankings %>%
  filter(!Teen.births..Hispanic. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Hispanic., y = Adult.smoking.raw.value)) +
  theme_bw()

# Creating scatter plots with correlations of each predictor with teen births with the race of Asian Pacific Islander ----

# Scatter plot of teen births and excessive drinking

county_rankings %>%
  filter(!Teen.births..Asian.Pacific.Islander. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Asian.Pacific.Islander., y = Excessive.drinking.raw.value)) +
  theme_bw()

# Scatter plot of teen births and unemployment

county_rankings %>%
  filter(!Teen.births..Asian.Pacific.Islander. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Asian.Pacific.Islander., y = Unemployment.raw.value)) +
  theme_bw()

# scatter plot of teen births and children in poverty

county_rankings %>%
  filter(!Teen.births..Asian.Pacific.Islander. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Asian.Pacific.Islander., y = Children.in.poverty.raw.value)) +
  theme_bw()

#Scatter plot of teen births and children in single parent households

county_rankings %>%
  filter(!Teen.births..Asian.Pacific.Islander. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Asian.Pacific.Islander., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births and poor mental health days

county_rankings %>%
  filter(!Teen.births..Asian.Pacific.Islander. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Asian.Pacific.Islander., y = Poor.mental.health.days.raw.value)) +
  theme_bw()

# Scatter plot of teen births and adult smoking

county_rankings %>%
  filter(!Teen.births..Asian.Pacific.Islander. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Asian.Pacific.Islander., y = Adult.smoking.raw.value)) +
  theme_bw()


# Creating scatter plot of unemployment and excessive drinking of adults ----

county_rankings %>%
  ggplot() +
  geom_point(aes(x = Unemployment.raw.value, y = Excessive.drinking.raw.value)) +
  theme_bw()

# Creating scatter plot of unemployment and adult smoking ---

county_rankings %>%
  ggplot(aes(x = Unemployment.raw.value, y = Adult.smoking.raw.value)) +
  geom_point(alpha = 0.5) +
  theme_bw()



# Create clustering of different races of teen births----

library(dslabs)
county_rankings <- as_tibble(county_rankings)
head(county_rankings)

clean_county_rankings <- county_rankings 
clean_county_rankings
  

init_kmeans <- 
  kmeans(dplyr::select(clean_county_rankings,
                       Teen.births.raw.value, Excessive.drinking.raw.value),
         algorithm = "Lloyd", centers = 4,
         nstart = 1)
clean_county_rankings %>%
  mutate(race_clusters = 
           as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = Teen.births.raw.value, y = Excessive.drinking.raw.value,
             color = race_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# Simple linear regression model of all predictors ----

init_lm <-lm(Teen.births.raw.value ~ Excessive.drinking.raw.value + Adult.smoking.raw.value + Unemployment.raw.value + 
               Children.in.poverty.raw.value + Children.in.single.parent.households.raw.value +
               Poor.mental.health.days.raw.value,
             data = clean_county_rankings)

summary(init_lm)

# computing correlation of excessive drinking

with(clean_county_rankings, cor(Excessive.drinking.raw.value + Adult.smoking.raw.value +
                                  Unemployment.raw.value +
                                  Children.in.poverty.raw.value +
                                  Children.in.single.parent.households.raw.value +
                                  Poor.mental.health.days.raw.value, Teen.births.raw.value))

with(clean_county_rankings, cor(Excessive.drinking.raw.value + Adult.smoking.raw.value+
                                  Unemployment.raw.value +
                                  Children.in.poverty.raw.value +
                                  Children.in.single.parent.households.raw.value+
                                  Poor.mental.health.days.raw.value, Teen.births.raw.value)) ^2

var(predict(init_lm)) / var(clean_county_rankings$Teen.births.raw.value)

# Fitted values of linear regression

train_preds <- predict(init_lm)
head(train_preds)

head(init_lm$fitted.values)

# Teen births against excessive drinking and adult smoking ----

clean_county_rankings %>%
  mutate(pred_vals = predict(init_lm)) %>%
  filter(!Teen.births.raw.value %in% 0) %>%
  ggplot(aes(x = pred_vals,
             y = Teen.births.raw.value)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "red",
              size = 2) +
  theme_bw()

# looking at outliers ----

clean_county_rankings$pred_vals <- predict(init_lm)
ix <- which(clean_county_rankings$Teen.births.raw.value > 55 & clean_county_rankings$pred_vals < 40)
outliers <- clean_county_rankings[ix,]
outliers



# Performing Lasso and Ridge Regression ----


# Ridge Regression Model

library(glmnet)
model_x <- county_rankings %>%
  dplyr::select(-Teen.births..Black., -Teen.births..White., -Teen.births..Hispanic., -Teen.births..Asian.Pacific.Islander.) %>%
  as.matrix()
model_y <- county_rankings$Teen.births.raw.value

init_ridge_fit <- glmnet(model_x, model_y, alpha = 0)
plot(init_ridge_fit, xvar = "lambda")

fit_ridge_cv <- cv.glmnet(model_x, model_y, alpha = 0)
plot(fit_ridge_cv)


# Lasso Regression Model

fit_lasso_cv <- cv.glmnet(model_x, model_y, 
                          alpha = 1)
tidy_lasso_coef <- tidy(fit_lasso_cv$glmnet.fit)
tidy_lasso_coef %>%
  ggplot(aes(x = lambda, y = estimate, 
             group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = 
               fit_lasso_cv$lambda.min) +
  geom_vline(xintercept = 
               fit_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  theme_bw()







































