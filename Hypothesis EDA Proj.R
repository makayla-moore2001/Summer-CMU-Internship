
# Loading the data ----

library(tidyverse)
maternal_healthcare <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/eda_projects/maternal_health_care_disparities.csv")

# Storing data locally ----

write.csv(maternal_healthcare, file = "maternity.csv")

maternity_df <- read.csv("maternity.csv")

head(maternity_df)

# Hypotheses ----

# Tobacco use reduces the child birth weight among prior birth deaths
# The older the average mothers age is the more the prenatal visits increase
# Tobacco use is more prevalent among mothers with at least 1 prior birth deceased



# Hypothesis 1 tobacco use reduces child birth weight among prior birth deaths ----

h1_df = filter(maternity_df, TobaccoUse %in% c("Yes", "No"), PrePregnancyDiabetes == "No", PrePregnancyHypertension == "No",
               PriorBirthsNowDeceased == "0")
h1_df %>%
  group_by(TobaccoUse) %>%
  summarise(true_count = sum(Births), 
            mean_AverageBirthWeight = mean(AverageBirthWeight)) %>%
  ggplot(aes(x = TobaccoUse, y = mean_AverageBirthWeight)) +
  geom_col(col = "black", fill = "steelblue") +
  geom_text(aes(label = round(mean_AverageBirthWeight, 0)), vjust = -0.25) + #text label
  theme_bw() + 
  labs(title = "", x = "Tobacco Use", y = "Average Birthweight (Grams)") #Tobacco Use is Associated with Decreased Birthweights

maternity_df %>%
  ggplot(aes(x = TobaccoUse)) +
  geom_bar() +
  theme_bw()

# Hypothesis 2 older the average mothers age the more prenatal visits increase

maternity_df %>%
  ggplot(aes(x = AverageMotherAge, y = AverageNumberPrenatalVisits)) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = F, method = "lm") +
  theme_bw() +
  labs(x = "Mother Age (Year)", y = "Number of Prenatal Visits", title = "") #Older Mothers Have More Prenatal Visits"


maternity_df %>%
  ggplot(aes(x = AverageMotherAge, y = AverageNumberPrenatalVisits)) +
  geom_point(color = "darkblue") +
  theme_bw()

# Hypothesis 3 

positions = c("None", "At least One")
filter(maternity_df, PriorBirthsNowDeceased != "Unknown", TobaccoUse != "Unknown") %>%
  mutate(new_dead_baby = ifelse(PriorBirthsNowDeceased == "0", "None", "At least One")) %>%
  group_by(new_dead_baby, TobaccoUse) %>%
  summarise(count = sum(Births)) %>%
  mutate(total = sum(count),
         prop = round(count/total, 5)) %>%
  ggplot(aes(x = new_dead_baby, y = prop, fill = TobaccoUse)) +
  geom_col(col = "black") + 
  scale_x_discrete(limits = positions) + #set positions of bars
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme_bw() +
  labs(title = "", x = "Number of Deceased Births", y = "Proportion of Births", fill = "Tobacco Use")


cluster_df = dplyr::select(maternity_df, PriorBirthsNowDeceased, AveragePrePregnancyBMI, AverageBirthWeight) %>%  drop_na()
#std cont var
clean_cluster_df <- cluster_df %>%
  mutate(std_BMI = as.numeric(scale(AveragePrePregnancyBMI, center = TRUE, scale = TRUE)), #<<
         std_weight = as.numeric(scale(AverageBirthWeight, center = TRUE, scale = TRUE)),
         update_deceased = ifelse(PriorBirthsNowDeceased == "0", "None", "At Least One")) #<<


library(flexclust)
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(clean_cluster_df, #<<
                     std_BMI, std_weight), k = 2, #<<
       control = list(initcent = "kmeanspp")) #<<
cluster_plot = clean_cluster_df %>%
  mutate(country_clusters = 
           as.factor(init_kmeanspp@cluster)) %>% #<< #@ symbol bc init_kmeanspp is an s4 object
  ggplot(aes(x = AveragePrePregnancyBMI, y = AverageBirthWeight,
             color = country_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(color = "Cluster", x = "Mother's BMI", y = "Birth Weight (Grams)")
real_plot = clean_cluster_df %>%
  ggplot(aes(x = AveragePrePregnancyBMI, y = AverageBirthWeight,
             color = update_deceased)) +
  geom_point() +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(color = "Deceased Births #", x = "Mother's BMI", y = "Birth Weight (Grams)")






new_maternitydata <- (dplyr::select(maternity_df,
                                    AveragePrePregnancyBMI, AverageBirthWeight) %>%
                        drop_na())
init_kmeans <- 
  kmeans(new_maternitydata,
         algorithm = "Lloyd", center = 3,
         nstart = 1)


new_maternitydata %>%
  mutate(country_clusters =
           as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = AveragePrePregnancyBMI, y = AverageBirthWeight, 
             color = country_clusters)) +
  geom_point() +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")


#
new_maternitydata <- as_tibble(new_maternitydata)
  
init_kmeanspp <- 
  kcca(dplyr::select(new_maternitydata,
                     AveragePrePregnancyBMI, AverageBirthWeight), k = 4,
       control = list(initcent = "kmeanspp"))
new_maternitydata %>%
  mutate(country_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = AveragePrePregnancyBMI, y = AveragePrePregnancyBMI,
             color = country_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")


# Making presentation slides 



  