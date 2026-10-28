library(MASS)
library(readxl)
library(skimr)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(gridExtra)
library(mgcv)
library(forcats)
library(COUNT)
library(purrr)

#Reading the dataset

fish_2025_data <- read_excel("fish_2025_data.xlsx")


#Cleaning the dataset

fish <- fish_2025_data %>%
  rename(lake_area.ha = lake_data_Lake.area..ha.,
         lake_altitude = lake_data_Lake.altitude..m.,
         lake_mean_depth.m = lake_data_Mean.Depth..m.,
         lake_volume.m3 = lake_data_Volume..m3.,         
         lake_perimeter.m = lake_data_Perimeter..km., 
         lake_fetch.km = lake_data_Fetch..km.,
         lake_straightdistance_tosea.km = lake_data_Distance.to.sea...simple..km.,
         lake_geotype = lake_data_UK_GEOL_TYPE_,
         lake_connectivitytype = lake_data_Connectivity.Type,
         lake_riverbasindistrict = lake_data_WFD_River_Basin_District,
         catch_mean_altitude.m = catchment_Mean.altitude..m.,
         mean_slopeoflake_catch = catchment_Mean.slope..degrees.,
         catch_lakearea.p = catchment_Lake.area..,
         catch_pondarea.p = catchment_Pond.area..,
         catch_rivers_length_total = catchment_Rivers...length..m._per_ha,
         catch_canals_length_total = catchment_Canals...length..m._per_ha,
         catch_lakes_total_perim = catchment_Lakes...Perimeter..m._per_ha,
         catch_ponds_total_perim = catchment_Ponds...Perimeter..m._per_ha,
         catch_lakes_count = catchment_Lakes...Count_per_ha,
         catch_pond_count = catchment_Ponds...Count_per_ha,
         catch_visits_fish = catchment_Visits...Fishing,
         catch_visits_watersport = catchment_Visits...Watersports,
         catch_visits_all = catchment_Visits...All,
         catch_obstacles_count = catchment_Obstacles...Count_per_ha,
         catch_agricover = catchment_LCM2007...Agricultural..,
         catch_urbancover = catchment_LCM2007...Urban..,
         catch_sea_accessible = catchment_Salmon_Accessible,
         catch_mean_temp = catchment_MEAN_mean_temp_2000_2016_pt.tif,
         catch_mean_annualrainfall = catchment_MEAN_mean_annual_rain_range_2000_2016_pt.tif)


#Exploratory Analysis

skim(fish)

# Splitting the variables into relevant groups


vars_stressor     <- c("catch_agricover", "catch_urbancover")

vars_connectivity <- c("catch_lakearea.p", "catch_pondarea.p", "catch_rivers_length_total",
                       "catch_canals_length_total", "catch_lakes_total_perim",
                       "catch_ponds_total_perim", "catch_lakes_count", "catch_pond_count",
                       "catch_visits_fish", "catch_visits_watersport", "catch_visits_all",
                       "catch_obstacles_count", "catch_sea_accessible")

vars_typology_cat <- c("lake_geotype", "lake_connectivitytype", "lake_riverbasindistrict")

vars_typology_num <- c("lake_area.ha", "lake_altitude", "lake_mean_depth.m", "lake_volume.m3",
                       "lake_perimeter.m", "lake_fetch.km", "lake_straightdistance_tosea.km",
                       "catch_mean_altitude.m", "mean_slopeoflake_catch", "catch_mean_temp",
                       "catch_mean_annualrainfall", "lon", "lat")


fish_connectivity <- fish %>% select(all_of(vars_connectivity), richness)
fish_typology <- fish %>% select(all_of(vars_typology_num),all_of(vars_typology_cat), richness)
fish_stressor <- fish %>% select(all_of(vars_stressor), richness)

fish_connectivity_typology <- fish %>% select(all_of(vars_connectivity),all_of(vars_typology_cat),all_of(vars_typology_num),richness)
fish_typology_num <- fish %>% select(all_of(vars_typology_num), richness)


# Relevant Histograms of the Data 

ggplot(fish, aes(x=richness)) +
  geom_histogram(col = "steelblue", bins = 30) +
  labs(y = "Counts", x = "Richness", title = "Counts of Fish Richness")


ggplot(fish, aes(x=sqrt(catch_urbancover))) +
  geom_histogram(col = "steelblue")       

ggplot(fish, aes(x=catch_agricover)) +
  geom_histogram(col = "steelblue")   

#Scatterplots of the stressor variables against the response variable

ggplot(fish, aes(x = (catch_agricover), y = richness)) + 
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.8) +
  labs(x = "Agricultural Land Cover Percentage")


ggplot(fish, aes(x = sqrt(catch_urbancover), y = richness)) + 
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.8) +
  labs(x = "Urban Land Cover Percentage")


#Boxplots

ggplot(fish, aes(y = richness, x = lake_riverbasindistrict)) +
  geom_boxplot(fill = "steelblue", col = "black") + 
  labs(x = "River Basin District", y = "Richness", title = "Boxplot of Fish Species Richness in Different River Basin Districts") +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1))

#Preparing the data

fish_long <- fish %>%
  pivot_longer(
    cols = c(
      lake_altitude, lake_area.ha, lake_mean_depth.m, lake_straightdistance_tosea.km,
      catch_mean_temp, catch_urbancover, catch_agricover, richness
    ),
    names_to = "variable",
    values_to = "value"
  )

# Boxplots using facet_wrap

ggplot(fish_long, aes(x = lake_geotype, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  # scales = "free_y" lets each plot scale its y-axis independently
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(x = "Lake Geotype", y = "Value")

ggplot(fish_long, aes(x = lake_riverbasindistrict, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  #scales = "free_y" lets each plot scale its y-axis independently
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(x = "Lake River Basin District", y = "Value")


# Correlation matrix

#Connectivity variables

correlation_connectivity <- cor(fish_connectivity)

corrplot(correlation_connectivity, method="number",number.cex = 0.62, tl.cex = 0.65)

#Stress Variables

correlation_stressor <- cor(fish_stressor)

corrplot(correlation_stressor, method="number")

#Typology Variables

correlation_typology_num <- cor(fish_typology_num)

corrplot(correlation_typology_num, method="number",number.cex = 0.62, tl.cex = 0.4, tl.col = "black")
text(x = -6, y = ncol(correlation_typology_num) + 2, labels = "Correlation Matrix", adj = c(0, 0), cex = 0.8)


#Typology and Connectivity Variables

fish_typology_num_connectivity <- fish %>% select(all_of(vars_typology_num), all_of(vars_connectivity), richness, -lat, -lon)

correlation_typology_connectivity <- cor(fish_typology_num_connectivity)

corrplot(correlation_typology_connectivity, method="number",number.cex = 0.20, tl.cex = 0.6,tl.col = "black")
text(x = -14, y = ncol(correlation_typology_connectivity) + 2, labels = "Correlation Matrix", adj = c(0, 0), cex = 0.8)

# Scatter plots of typology variables

ggplot(fish, aes(x = catch_mean_temp, y = richness)) + 
  geom_point() +
  labs(x = "Catchment Mean Temperature", y = "richness")


ggplot(fish, aes(x = catch_mean_altitude.m, y = richness)) + 
  geom_point() +
  labs(x = "Catchment Mean Altitude", y = "richness")

#To generate the plots pre-transformation use the identity transformation


# Specify the variables, transformations, and data frames
plot_vars_typology_stressor <- list(
  list(data = fish, var = "lake_area.ha", transform = log, label = "log(lake_area.ha)"),
  list(data = fish, var = "lake_altitude", transform = sqrt, label = "sqrt(lake_altitude)"),
  list(data = fish, var = "lake_mean_depth.m", transform = sqrt, label = "sqrt(lake_mean_depth.m)"),
  list(data = fish, var = "lake_volume.m3", transform = log, label = "log(lake_volume.m3)"),
  list(data = fish, var = "lake_perimeter.m", transform = log, label = "log(lake_perimeter.m)"),
  list(data = fish, var = "lake_fetch.km", transform = sqrt, label = "sqrt(lake_fetch.km)"),
  list(data = fish, var = "lake_straightdistance_tosea.km", transform = identity, label = "lake_straightdistance_tosea.km"),
  list(data = fish, var = "catch_mean_altitude.m", transform = sqrt, label = "sqrt(catch_mean_altitude.m)"),
  list(data = fish, var = "mean_slopeoflake_catch", transform = identity, label = "mean_slopeoflake_catch"),
  list(data = fish, var = "catch_mean_temp", transform = identity, label = "catch_mean_temp"),
  list(data = fish, var = "catch_mean_annualrainfall", transform = identity, label = "catch_mean_annualrainfall"),
  list(data = fish, var = "catch_agricover", transform = identity, label = "catch_agricover"),
  list(data = fish, var = "catch_urbancover", transform = sqrt, label = "sqrt(catch_urbancover)")
)

# Generate plots
plots <- map(plot_vars_typology_stressor, function(p) {
  ggplot(
    p$data,
    aes(
      x = p$transform(.data[[p$var]]),
      y = richness
    )
  ) +
    geom_point(colour = "steelblue") +
    labs(
      x = p$label,
      y = "Richness"
    ) +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 6)
    )
})

# Arrange plots in a grid
e_grid <- grid.arrange(
  grobs = plots,
  ncol = 3,
  top = "Scatterplots of Typology and Stressor Variables Against Richness Post-Transformation"
)


# Scatterplots of the connectivity variables

plot_vars_connectivity <- list(
  list(data = fish, var = "catch_lakearea.p", transform = log, label = "log(catch_lakearea.p)"),
  list(data = fish, var = "catch_pondarea.p", transform = log, label = "log(catch_pondarea.p)"),
  list(data = fish, var = "catch_rivers_length_total", transform = sqrt, label = "sqrt(catch_rivers_length_total)"),
  list(data = fish, var = "catch_canals_length_total", transform = log, label = "log(catch_canals_length_total)"),
  list(data = fish, var = "catch_lakes_total_perim", transform = sqrt, label = "sqrt(catch_lakes_total_perim)"),
  list(data = fish, var = "catch_ponds_total_perim", transform = sqrt, label = "sqrt(catch_ponds_total_perim)"),
  list(data = fish, var = "catch_lakes_count", transform = log, label = "log(catch_lakes_count)"),
  list(data = fish, var = "catch_pond_count", transform = log, label = "log(catch_pond_count)"),
  list(data = fish, var = "catch_visits_fish", transform = identity, label = "catch_visits_fish"),
  list(data = fish, var = "catch_visits_watersport", transform = identity, label = "catch_visits_watersport"),
  list(data = fish, var = "catch_visits_all", transform = sqrt, label = "sqrt(catch_visits_all)"),
  list(data = fish, var = "catch_obstacles_count", transform = log, label = "log(catch_obstacles_count)"),
  list(data = fish, var = "catch_sea_accessible", transform = identity, label = "catch_sea_accessible")
)

# Generate plots
plots_connectivity <- map(plot_vars_connectivity, function(p) {
  ggplot(
    p$data,
    aes(
      x = p$transform(.data[[p$var]]),
      y = richness
    )
  ) +
    geom_point(colour = "steelblue") +
    labs(
      x = p$label,
      y = "Richness"
    ) +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 6)
    )
})

# Arrange plots in a grid
grid.arrange(
  grobs = plots_connectivity,
  ncol = 3,
  top = "Scatterplots of the Connectivity Variables Against Richness Post-Transformation"
)


#Modelling using GLMs

#Should check for overdispersion before settling on the poisson distribution 

#GLM using a poisson distribution


mod1 <- glm(richness ~ sqrt(lake_altitude) + log(lake_perimeter.m) + lake_mean_depth.m
            + lake_straightdistance_tosea.km  + catch_agricover
            + sqrt(catch_urbancover) + lake_riverbasindistrict + lat + lon, family = poisson(), data = fish)

summary(mod1)


#Dispersion Tests looking at the connectivity and stressor variables

Dispersion_Test_S <- glm(richness ~ sqrt(catch_mean_altitude.m) + lake_straightdistance_tosea.km + lat + lon + 
                           catch_agricover + sqrt(catch_urbancover) + log(lake_perimeter.m) + lake_riverbasindistrict,
                         data = fish, family = poisson())


Dispersion_Test_C <-  glm(richness ~ sqrt(lake_altitude) + log(lake_perimeter.m) + 
                            lake_fetch.km + lake_straightdistance_tosea.km + lake_riverbasindistrict + 
                            mean_slopeoflake_catch + catch_mean_annualrainfall + lat + 
                            catch_lakes_count + catch_visits_fish + catch_visits_watersport + 
                            catch_sea_accessible, data = fish, family = poisson())

#Using a Negative Binomial Distribution

Dispersion_Test_S_NB <- glm.nb(richness ~ sqrt(catch_mean_altitude.m) + lake_straightdistance_tosea.km + lat + lon + 
                                 catch_agricover + sqrt(catch_urbancover) + log(lake_perimeter.m) + lake_riverbasindistrict,
                               data = fish)


Dispersion_Test_C_NB <-  glm.nb(richness ~ sqrt(lake_altitude) + log(lake_perimeter.m) + 
                                  lake_fetch.km + lake_straightdistance_tosea.km + lake_riverbasindistrict + 
                                  mean_slopeoflake_catch + catch_mean_annualrainfall + lat + 
                                  catch_lakes_count + catch_visits_fish + catch_visits_watersport + 
                                  catch_sea_accessible, data = fish)



summary(Dispersion_Test_S)
summary(Dispersion_Test_S_NB)

summary(Dispersion_Test_C)
summary(Dispersion_Test_C_NB)

#In both cases the negative binomial distribution produces better AIC scores

P__disp(Dispersion_Test_S)
P__disp(Dispersion_Test_S_NB)

P__disp(Dispersion_Test_C)
P__disp(Dispersion_Test_C_NB)

#Negative binomial distribution has a dispersion value much closer to 1

#Z-Score-test, another test for overdispersion 

mu_s <- predict(Dispersion_Test_S, type = "response")
z <- ((fish$richness - mu_s)^2 - fish$richness)/ (mu_s * sqrt(2))

summary(zscore <- lm(z ~ 1))

mu_c <- predict(Dispersion_Test_C, type = "response")

z_c <- ((fish$richness - mu_c)^2 - fish$richness)/ (mu_c * sqrt(2))

summary(zscore <- lm(z_c ~ 1))

#Z-score test also says that our data is overdispersed.

#Building a model using stressor and typology variables

fish_typology_stressor <- fish %>% select(all_of(vars_stressor), all_of(vars_typology_num), all_of(vars_typology_cat), richness)

Mod.full <- glm.nb(richness ~ ., data = fish_typology_stressor)


#Applying transformations to the data


fish_typology_stressor_transformed <- fish_typology_stressor %>%
  mutate(lake_perimeter.m = log(lake_perimeter.m),
         lake_altitude = sqrt(lake_altitude),
         catch_urbancover = sqrt(catch_urbancover),
         lake_volume.m3 = log(lake_volume.m3),
         lake_fetch.km = log(lake_fetch.km),
         lake_mean_depth.m = sqrt(lake_mean_depth.m),
         catch_mean_altitude.m  = sqrt(catch_mean_altitude.m),
         catch_mean_annualrainfall = sqrt(catch_mean_annualrainfall))

Mod.full_transformed <- glm.nb(richness ~ ., data = fish_typology_stressor_transformed)

n <- nrow(fish)

step(Mod.full, direction = "backward")
step(Mod.full, direction = "backward", k = log(n))

step(Mod.full_transformed, direction = "backward")
step(Mod.full_transformed, direction = "backward", k = log(n))


GLM_Model_Stressor_Typology  <- glm.nb(richness ~ lake_altitude + lake_perimeter.m + 
                                         lake_fetch.km + lake_straightdistance_tosea.km + lake_riverbasindistrict + 
                                         mean_slopeoflake_catch + catch_mean_annualrainfall + lat + 
                                         catch_agricover + catch_urbancover, data = fish)

summary(GLM_Model_Stressor_Typology)

BIC_score <- BIC(GLM_Model_Stressor_Typology)

#BIC penalises degrees of freedom that is why a GLM may give a better BIC value than a GAM model at times

#Building a model using connectivity and typology variables


fish_typology_connectivity <- fish %>% select(all_of(vars_connectivity), all_of(vars_typology_num), richness,-lat,-lon)

fish_typology_connectivity_transformed <- fish_typology_connectivity %>%
  mutate(catch_lakearea.p = sqrt(catch_lakearea.p),
         catch_pondarea.p = sqrt(catch_pondarea.p),
         catch_rivers_length_total = sqrt(catch_rivers_length_total),
         catch_canals_length_total = sqrt(catch_canals_length_total),
         catch_lakes_total_perim = sqrt(catch_lakes_total_perim),
         catch_ponds_total_perim = sqrt(catch_ponds_total_perim),
         catch_lakes_count = sqrt(catch_lakes_count),
         catch_pond_count = sqrt(catch_pond_count),
         catch_visits_all = sqrt(catch_visits_all),
         catch_obstacles_count = sqrt(catch_obstacles_count),
         lake_perimeter.m = log(lake_perimeter.m),
         lake_altitude = sqrt(lake_altitude),
         lake_volume.m3 = log(lake_volume.m3),
         lake_fetch.km = log(lake_fetch.km),
         lake_mean_depth.m = sqrt(lake_mean_depth.m),
         catch_mean_altitude.m  = sqrt(catch_mean_altitude.m),
         catch_mean_annualrainfall = sqrt(catch_mean_annualrainfall))

Mod.full_connectivity <- glm.nb(richness ~ ., data = fish_typology_connectivity)
Mod.full_connectivity_transformed <- glm.nb(richness ~ ., data = fish_typology_connectivity_transformed)

step(Mod.full_connectivity, direction = "backward")
step(Mod.full_connectivity, direction = "backward", k = log(n))

step(Mod.full_connectivity_transformed, direction = "backward")
step(Mod.full_connectivity_transformed, direction = "backward", k = log(n))

GLM_Model_Connectivity_Typology <- glm.nb(richness ~ lake_altitude + lake_mean_depth.m + 
                                            lake_fetch.km + lake_straightdistance_tosea.km + catch_mean_altitude.m + 
                                            mean_slopeoflake_catch + catch_mean_temp + catch_mean_annualrainfall + 
                                            catch_lakes_count + catch_visits_watersport + catch_visits_all + 
                                            catch_sea_accessible, data = fish)


summary(GLM_Model_Connectivity_Typology)

BIC_score_connectivity <- BIC(GLM_Model_Connectivity_Typology)



#Basic residual diagnostics

dev_res <- residuals(GLM_Model_Connectivity_Typology, type = "deviance")

#Get linear predictor
linear_pred <- predict(GLM_Model_Connectivity_Typology, type = "link")

#Plot
plot(linear_pred, dev_res,
     xlab = "Linear Predictor (η)",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Linear Predictor")



resids <- residuals(GLM_Model_Connectivity_Typology, type = "dev")

plot(fitted(GLM_Model_Connectivity_Typology), resids)

#Basic residual diagnostics for the stressor model

dev_res <- residuals(GLM_Model_Stressor_Typology, type = "deviance")

#Get linear predictor
linear_pred <- predict(GLM_Model_Stressor_Typology, type = "link")

#Plot
plot(linear_pred, dev_res,
     xlab = "Linear Predictor (η)",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Linear Predictor")



resids <- residuals(GLM_Model_Stressor_Typology, type = "dev")

plot(fitted(GLM_Model_Stressor_Typology), resids)

#Model building with GAMs is going to produce better results, due to the non-linear relationships within the data. 


#The stressor model which produced the best results

Stressor_model <- gam(richness ~ s(lat,lon,k=30, bs = "gp", m = c(1,1)) + s(sqrt(catch_mean_altitude.m), k = 5, bs = "cs") + s((lake_straightdistance_tosea.km), k = 8, bs = "cs") +
                        s(log(lake_perimeter.m), k = 5, bs = "cs") + s(sqrt(catch_urbancover), k = 3) + s(catch_agricover) + lake_riverbasindistrict,
                      data = fish, family = nb(link = "log"))


summary((Stressor_model))
summary(fitted(Stressor_model))

Stressor_model_AIC <- AIC(Stressor_model)
Stressor_model_BIC <- BIC(Stressor_model)


gam.check(Stressor_model)

par(mfrow = c(1, 1))
plot(Stressor_model)



mean(fitted(Stressor_model))
mean(fish$richness)

#Check concurvity

concurvity(Stressor_model, full = TRUE)
concurvity(Stressor_model, full = FALSE)

#Residual Diagnostics

resids_stressor <- residuals(Stressor_model, type = "dev")
plot(fitted(Stressor_model), resids_stressor,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")

plot(fish$lon,residuals(Stressor_model))
plot(fish$lat,residuals(Stressor_model))
plot(sqrt(fish$catch_urbancover),residuals(Stressor_model))
plot(log(fish$lake_perimeter.m), residuals(Stressor_model))
plot(sqrt(fish$catch_mean_altitude.m),residuals(Stressor_model))
plot((fish$lake_straightdistance_tosea.km),residuals(Stressor_model))   


#Connectivity Model

#Some Connectivity variables have a high proportion of zeros which may lead to difficulty when modelling.

start_col <- which(names(fish_connectivity) == "catch_lakearea.p")
end_col <- which(names(fish_connectivity) == "catch_sea_accessible")
cols <- names(fish_connectivity)[start_col:end_col]

for (i in cols) {
  zero_prop <- sum(fish_connectivity[[i]] == 0, na.rm = TRUE) / length(fish_connectivity[[i]])
  print(paste("Proportion of 0s in", i, ":", zero_prop))
}

#The connectivity model which produces the best results

Connectivity_model <- gam(richness ~ s(lat,lon, k= 40, bs = "gp", m = c(1,1)) + s(log(lake_perimeter.m), k = 5, bs = "cs") + 
                            s(sqrt(lake_altitude), k = 5, bs = "cs") + lake_riverbasindistrict + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") +
                            s(sqrt(catch_lakes_total_perim), bs = "cr") + s((catch_visits_watersport), bs = "cr") + catch_sea_accessible, data = fish, family = nb(link = "log"))



summary(Connectivity_model)

summary(fitted(Connectivity_model))

par(mfrow = c(1, 1))
plot(Connectivity_model)

gam.check(Connectivity_model)


Connectivity_model_AIC <- AIC(Connectivity_model)
Connectivity_model_BIC <- BIC(Connectivity_model)


resids_connectivity <- residuals(Connectivity_model, type = "dev")
plot(fitted(Connectivity_model), resids_connectivity,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")


concurvity(Connectivity_model, full = TRUE)
concurvity(Connectivity_model, full = FALSE)

plot(fish$lon,residuals(Connectivity_model))
plot(fish$lat,residuals(Connectivity_model))
plot(log(fish$lake_perimeter.m),residuals(Connectivity_model))
plot(fish$lake_straightdistance_tosea.km,residuals(Connectivity_model))
plot(sqrt(fish$catch_lakes_total_perim),residuals(Connectivity_model))   
plot(fish$catch_visits_watersport,residuals(Connectivity_model))


fish_full <- fish %>% 
  select(-lake_geotype,-lake_connectivitytype,-Grid_Ref)

fish_transformations <- fish_full %>%
  mutate(catch_lakearea.p = sqrt(catch_lakearea.p),
         catch_pondarea.p = sqrt(catch_pondarea.p),
         catch_rivers_length_total = sqrt(catch_rivers_length_total),
         catch_canals_length_total = sqrt(catch_canals_length_total),
         catch_lakes_total_perim = sqrt(catch_lakes_total_perim),
         catch_ponds_total_perim = sqrt(catch_ponds_total_perim),
         catch_lakes_count = sqrt(catch_lakes_count),
         catch_pond_count = sqrt(catch_pond_count),
         catch_visits_all = sqrt(catch_visits_all),
         catch_obstacles_count = sqrt(catch_obstacles_count),
         lake_perimeter.m = log(lake_perimeter.m),
         lake_altitude = sqrt(lake_altitude),
         lake_volume.m3 = log(lake_volume.m3),
         lake_fetch.km = log(lake_fetch.km),
         lake_mean_depth.m = sqrt(lake_mean_depth.m),
         catch_mean_altitude.m  = sqrt(catch_mean_altitude.m),
         catch_mean_annualrainfall = sqrt(catch_mean_annualrainfall))



Interaction_model <- gam(richness ~ s(catch_mean_altitude.m, k = 5, bs = "cs")  + s(lat,lon,k=30, bs = "gp", m = c(1,1)) + s(lake_straightdistance_tosea.km, k = 8, bs = "cs")
                         + s(lake_perimeter.m, k = 5, bs = "cs") + lake_riverbasindistrict + s(catch_agricover) + s(catch_urbancover) +
                           s(catch_lakes_total_perim, bs = "cs")  + catch_sea_accessible +
                           ti(catch_agricover, catch_lakes_total_perim) + ti(catch_urbancover, catch_lakes_total_perim) + ti(catch_agricover, by = catch_sea_accessible),
                         data = fish_transformations, family = nb(link = "log"))


summary(Interaction_model)

summary(fitted(Interaction_model))

plot(Interaction_model)

gam.check(Interaction_model)

Interaction_model_BIC <- BIC(Interaction_model)
Interaction_model_AIC <- AIC(Interaction_model)

vis.gam(Interaction_model, 
        view = c("catch_agricover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 25, phi = 35)


vis.gam(Interaction_model, 
        view = c("catch_urbancover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 45, phi = 50)

