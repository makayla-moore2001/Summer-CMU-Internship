# Correlation of different predictors and teen births

# Load data ----

library(tidyverse)
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

# Scatter plot of teen births (black) and children in a single parent household

county_rankings %>%
  filter(!Teen.births..Black. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Black., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births (white) and children in a single parent household

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births (Hispanic) and children in a single parent household

county_rankings %>%
  filter(!Teen.births..Hispanic. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Hispanic., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births (Asian Pacific Islander)

county_rankings %>%
  filter(!Teen.births..Asian.Pacific.Islander. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..Asian.Pacific.Islander., y = Children.in.single.parent.households.raw.value)) +
  theme_bw()

# Scatter plot of teen births and excessive drinking

county_rankings %>%
  filter(!Teen.births.raw.value %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births.raw.value, y = Excessive.drinking.raw.value)) +
  theme_bw()

# Scatter plot of teen births and children in poverty

county_rankings %>%
  filter(!Teen.births.raw.value %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births.raw.value, y = Children.in.poverty.raw.value)) +
  theme_bw()

# Scatter plot of teen births (White) and children in poverty

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Children.in.poverty.raw.value)) +
  theme_bw()

# Scatter plot of teen births (White) and poor mental health days in the past 30 days 

county_rankings %>%
  filter(!Teen.births..White. %in% 0) %>%
  ggplot() +
  geom_point(aes(x = Teen.births..White., y = Poor.mental.health.days.raw.value)) +
  theme_bw()

# 

library(ggbeeswarm)
county_rankings %>%
  ggplot(aes(y = Excessive.drinking.raw.value )) +
  geom_beeswarm(aes(x = ""), cex = 3) +
  theme_bw() +
  coord_flip()


# Attempt clustering of different races of teen births----

# Create a simple linear model----


# Create a PCA ----

