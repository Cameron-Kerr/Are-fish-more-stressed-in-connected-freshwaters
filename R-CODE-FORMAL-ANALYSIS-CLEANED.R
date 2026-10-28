
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