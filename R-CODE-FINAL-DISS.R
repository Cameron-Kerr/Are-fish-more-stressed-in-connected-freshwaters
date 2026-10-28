library(MASS)
library(readxl)
library(skimr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(corrplot)
library(gridExtra)
library(combinat)
library(mgcv)
library(forcats)
library(dplyr)
library(grid)
library(spdep)
library(COUNT)


#Reading the dataset

fish_2025_data <- read_excel("fish_2025_data.xlsx")
Fish <- read_excel("fish_2025_data.xlsx")


#Cleaning the dataset

Fish1 <- fish_2025_data %>%
  rename(lake_area.ha = lake_data_Lake.area..ha.,
         lake_altitude = lake_data_Lake.altitude..m.,
         lake_mean_depth.m = lake_data_Mean.Depth..m.,
         lake_volume.m3 = lake_data_Volume..m3.,         
         lake_perimeter.km = lake_data_Perimeter..km., 
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

Fish2 <- fish_2025_data %>%
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

skim(Fish1)


# Relevant Histograms of the Data 

ggplot(Fish1, aes(x=richness)) +
  geom_histogram(col = "steelblue", bins = 30) +
  labs(y = "Counts", x = "Richness", title = "Counts of Fish Richness")


ggplot(Fish1, aes(x=sqrt(catch_urbancover))) +
  geom_histogram(col = "steelblue")       

ggplot(Fish1, aes(x=catch_agricover)) +
  geom_histogram(col = "steelblue")   

# Splitting the variables into relevant groups


Fish_Group_T <- Fish1 %>% 
  select(lake_area.ha:mean_slopeoflake_catch,catch_mean_temp:lat,Grid_Ref)


Fish_Group_T_S <- Fish1 %>% 
  select(lake_area.ha:mean_slopeoflake_catch,catch_mean_temp:lat,catch_agricover,catch_urbancover,-Grid_Ref)

summary(Fish_Group_T_S)

Fish_Group_T_noncategorical <- Fish_Group_T %>%
  select(-lake_geotype,-lake_connectivitytype,-lake_riverbasindistrict,-Grid_Ref)

Fish_Group_S <- Fish1 %>%
  select(catch_agricover,catch_urbancover,richness)

Fish_Group_C <- Fish1 %>%
  select(catch_lakearea.p:catch_obstacles_count,richness,catch_sea_accessible)

Fish_T_NoGridRef <- Fish_Group_T %>% 
  select(-Grid_Ref)


Fish_T <- Fish_Group_T %>% mutate(row_id = row_number())
Fish_C <- Fish_Group_C %>% mutate(row_id = row_number())

Fish_joined <- inner_join(Fish_T, Fish_C, by = "row_id")

Fish_joined <- Fish_joined %>%
  select(-richness.x) %>%
  rename(richness = richness.y)


Fish_model_for_C_glm <- Fish_joined %>%
  select(-row_id,-lake_geotype,-lake_connectivitytype,-Grid_Ref,-lat,-lon,-lake_riverbasindistrict)

Fish_Group_Model_Full0 <- Fish1 %>%
  select(-lake_geotype,-lake_connectivitytype,-Grid_Ref)

  
#Scatterplots of the stressor variables against the response variable


ggplot(Fish1, aes(x = (catch_agricover), y = richness)) + 
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.8) +
  geom_smooth(method = "gam", se = FALSE) + 
  labs(x = "Agricultural Land Cover Percentage")



ggplot(Fish1, aes(x = sqrt(catch_urbancover), y = richness)) + 
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.8) +
  labs(x = "Urban Land Cover Percentage") + 
  geom_smooth(method = "gam", se = FALSE)


#Boxplots

ggplot(Fish1, aes(y = richness, x = lake_riverbasindistrict)) +
  geom_boxplot(fill = "steelblue", col = "black") + 
  labs(x = "River Basin District", y = "Richness", title = "Boxplot of Fish Species Richness in Different River Basin Districts") +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1))

#Preparing the data

Fish_long <- Fish1 %>%
  pivot_longer(
    cols = c(
      lake_altitude, lake_area.ha, lake_mean_depth.m, lake_straightdistance_tosea.km,
      catch_mean_temp, catch_urbancover, catch_agricover, richness
    ),
    names_to = "variable",
    values_to = "value"
  )

# Boxplots using facet_wrap

ggplot(Fish_long, aes(x = lake_geotype, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  # scales="free_y" lets each plot scale its y-axis independently
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(x = "Lake Connectivity Type", y = "Value")

ggplot(Fish_long, aes(x = lake_riverbasindistrict, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  # scales="free_y" lets each plot scale its y-axis independently
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(x = "Lake River Basin district", y = "Value")


# Correlation matrix

#Connectivity variables

M <- cor(Fish_Group_C)

corrplot(M, method="number",number.cex = 0.62, tl.cex = 0.65)

#Stress Variables

S <- cor(Fish_Group_S)

corrplot(S, method="number")

#Typology Variables

V <- cor(Fish_Group_T_noncategorical)

corrplot(V, method="number",number.cex = 0.62, tl.cex = 0.4, tl.col = "steelblue")
text(x = -4, y = ncol(V) + 2, labels = "Correlation Matrix", adj = c(0, 0), cex = 0.8)


#Typology and Connectivity Variables

Y <- cor(Fish_model_for_C_glm)

corrplot(Y, method="number",number.cex = 0.20, tl.cex = 0.6,tl.col = "black")
text(x = -14, y = ncol(Y) + 2, labels = "Correlation Matrix", adj = c(0, 0), cex = 0.8)

# Scatter plots

ggplot(Fish1, aes(x = catch_mean_temp, y = richness)) + 
  geom_point() +
  labs(x = "Catchment mean temperature", y = "richness")


ggplot(Fish1, aes(x = catch_mean_altitude.m, y = richness)) + 
  geom_point() +
  labs(x = "Catchment mean altidude", y = "richness")


e1 <- ggplot(Fish_Group_T, aes(x = log(lake_area.ha), y = richness)) + 
  geom_point(col = "steelblue") + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e2 <- ggplot(Fish_Group_T, aes(x = sqrt(lake_altitude), y = richness)) + 
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e3 <- ggplot(Fish_Group_T, aes(x = sqrt(lake_mean_depth.m), y = richness)) + 
  geom_point(col = "steelblue")  +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e4 <- ggplot(Fish_Group_T, aes(x = log(lake_volume.m3), y = richness)) + 
  geom_point(col = "steelblue") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 6)
  )

e6 <- ggplot(Fish_Group_T, aes(x = log(lake_perimeter.m) , y = richness)) + 
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e7 <- ggplot(Fish_Group_T, aes(x = sqrt(lake_fetch.km), y = richness)) + 
  geom_point(col = "steelblue")  + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e8 <- ggplot(Fish_Group_T, aes(x = lake_straightdistance_tosea.km, y = richness)) + 
  geom_point(col = "steelblue")  +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e9 <- ggplot(Fish_Group_T, aes(x = sqrt(catch_mean_altitude.m), y = richness)) + 
  geom_point(col = "steelblue")  +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e10 <- ggplot(Fish_Group_T, aes(x = mean_slopeoflake_catch, y = richness)) + 
  geom_point(col = "steelblue")  +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e11 <- ggplot(Fish_Group_T, aes(x = catch_mean_temp, y = richness)) + 
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e12 <- ggplot(Fish_Group_T, aes(x = catch_mean_annualrainfall, y = richness)) + 
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e13 <- ggplot(Fish1, aes(x = catch_agricover, y = richness)) + 
  geom_point(col = "steelblue") + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

e14 <- ggplot(Fish1, aes(x = sqrt(catch_urbancover), y = richness)) + 
  geom_point(col = "steelblue")  + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))



e_grid <- grid.arrange(e1,e2,e3,e4,e6,e7,e8,e9,e10,e11,e12,e13,e14, ncol = 3,
                       top = "Scatterplots of the Typology and Stressor Variables Against Richness Post-Transformation ")

#Pair plots

Fish_Group_C_Rename <- Fish_Group_C %>%
  rename(lare = catch_lakearea.p,
         pare = catch_pondarea.p,
         rlen = catch_rivers_length_total,
         clen = catch_canals_length_total,
         lper = catch_lakes_total_perim,
         pper = catch_ponds_total_perim,
         lcou = catch_lakes_count,
         pcou = catch_pond_count,
         vf = catch_visits_fish,
         vw = catch_visits_watersport,
         va = catch_visits_all,
         ric = richness,
         sea = catch_sea_accessible,
         ob = catch_obstacles_count
  )


ggpairs(Fish_Group_C_Rename,upper=list(continuous=wrap("points", alpha=0.4, color="#d73027")),
        lower="blank", axisLabels="none")

# Scatterplots of the connectivity variables

t1 <- ggplot(Fish_Group_C, aes(log(catch_lakearea.p), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t2 <- ggplot(Fish_Group_C, aes(x= log(catch_pondarea.p), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t3 <- ggplot(Fish_Group_C, aes(x=sqrt(catch_rivers_length_total), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))
  

t4 <- ggplot(Fish_Group_C, aes(x=log(catch_canals_length_total), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t5 <- ggplot(Fish_Group_C, aes(x= sqrt(catch_lakes_total_perim), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))


t6 <- ggplot(Fish_Group_C, aes(x= sqrt(catch_ponds_total_perim), y = richness)) +
  geom_point(col = "steelblue")  +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t7 <- ggplot(Fish_Group_C, aes(x= log(catch_lakes_count), y = richness)) +
  geom_point(col = "steelblue")  +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t8 <- ggplot(Fish_Group_C, aes(x= log(catch_pond_count), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t9 <- ggplot(Fish_Group_C, aes(x=catch_visits_fish ,y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t10 <- ggplot(Fish_Group_C, aes(x=catch_visits_watersport, y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t11 <- ggplot(Fish_Group_C, aes(x= sqrt(catch_visits_all), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t12 <- ggplot(Fish_Group_C, aes(x= log(catch_obstacles_count), y = richness)) +
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

t13 <- ggplot(Fish_Group_C, aes(x = catch_sea_accessible, y = richness)) + 
  geom_point(col = "steelblue") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

grid.arrange(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13, ncol = 3,
             top = "Scatterplots of the Connectivity Variables Against Richness Post-Transformation")


##################################################################################################################################################

#Formal Analysis

###################################################################################################################################################

#Modelling using GLMs

#GLM using a poisson distribution


mod1 <- glm(richness ~ sqrt(lake_altitude) + log(lake_perimeter.m) + lake_mean_depth.m
            + lake_straightdistance_tosea.km  + catch_agricover
            + sqrt(catch_urbancover) + lake_riverbasindistrict + lat + lon, family = poisson(), data = Fish1)

summary(mod1)

#GLM using a negative binomial distribution 

mod2 <- glm.nb(richness ~ sqrt(lake_altitude) + log(lake_perimeter.m) + lake_mean_depth.m
            + lake_straightdistance_tosea.km  + catch_agricover
            + sqrt(catch_urbancover) + lake_riverbasindistrict + lat + lon, data = Fish1)


summary(mod2)

# Dispersion Tests looking at the connectivity and stressor variables

Dispersion_Test_S <- glm(richness ~ sqrt(catch_mean_altitude.m) + lake_straightdistance_tosea.km + lat + lon + 
                     catch_agricover + sqrt(catch_urbancover) + log(lake_perimeter.m) + lake_riverbasindistrict,
                      data = Fish1, family = poisson())


Dispersion_Test_C <-  glm(richness ~ sqrt(lake_altitude) + log(lake_perimeter.m) + 
                         lake_fetch.km + lake_straightdistance_tosea.km + lake_riverbasindistrict + 
                         mean_slopeoflake_catch + catch_mean_annualrainfall + lat + 
                         catch_lakes_count + catch_visits_fish + catch_visits_watersport + 
                         catch_sea_accessible, data = Fish1, family = poisson())

#Using a Negative Binomial Distribution

Dispersion_Test_S_NB <- glm.nb(richness ~ sqrt(catch_mean_altitude.m) + lake_straightdistance_tosea.km + lat + lon + 
                           catch_agricover + sqrt(catch_urbancover) + log(lake_perimeter.m) + lake_riverbasindistrict,
                         data = Fish1)


Dispersion_Test_C_NB <-  glm.nb(richness ~ sqrt(lake_altitude) + log(lake_perimeter.m) + 
                            lake_fetch.km + lake_straightdistance_tosea.km + lake_riverbasindistrict + 
                            mean_slopeoflake_catch + catch_mean_annualrainfall + lat + 
                            catch_lakes_count + catch_visits_fish + catch_visits_watersport + 
                            catch_sea_accessible, data = Fish1)



summary(Dispersion_Test_S)
summary(Dispersion_Test_C)

P__disp(Dispersion_Test_S)
P__disp(Dispersion_Test_C)

#Z-Score-test, another dispersion test 

summary(Dispersion_Test_S)   

mu_s <- predict(Dispersion_Test_S, type = "response")
z <- ((Fish1$richness - mu_s)^2 - Fish1$richness)/ (mu_s * sqrt(2))

summary(zscore <- lm(z ~ 1))

summary(Dispersion_Test_C)

mu_c <- predict(Dispersion_Test_C, type = "response")
z_c <- ((Fish1$richness - mu_c)^2 - Fish1$richness)/ (mu_c * sqrt(2))

summary(zscore <- lm(z_c ~ 1))

# Z-score test also says that our data is over dispersed

Mod.full <- glm.nb(richness ~ ., data = Fish_Group_T_S)


#Applying transformations to the data


Fish_Group_T_S_TRANSFORM <- Fish_Group_T_S %>%
  mutate(lake_perimiter.m = log(lake_perimeter.m),
         lake_altitude = sqrt(lake_altitude),
         catch_urbancover = sqrt(catch_urbancover),
         lake_volume.m3 = log(lake_volume.m3),
         lake_fetch.km = log(lake_fetch.km),
         lake_mean_depth.m = sqrt(lake_mean_depth.m),
         catch_mean_altitude.m  = sqrt(catch_mean_altitude.m),
         catch_mean_annualrainfall = sqrt(catch_mean_annualrainfall),
         catch_mean_altitude.m = sqrt(catch_mean_altitude.m))

Mod.full <- glm.nb(richness ~ ., data = Fish_Group_T_S_TRANSFORM)

# TEST CAN ALSO BE APPLIED WITH APPROPRIATE TRANSFORMATIONS APPLIED

Mod.full <- glm.nb(richness ~ ., data = Fish_Group_T_S)

Best_GLM_Model_AIC  <- glm.nb(richness ~ lake_altitude + lake_perimeter.m + 
                                lake_fetch.km + lake_straightdistance_tosea.km + lake_riverbasindistrict + 
                                mean_slopeoflake_catch + catch_mean_annualrainfall + lat + 
                                catch_agricover + catch_urbancover, data = Fish1)


n <- nrow(Fish1)

step(Mod.full, direction = "backward")
step(Mod.full, direction = "backward", k = log(n))


W <- BIC(Best_GLM_Model_AIC)

# BIC peanalises degrees of freedom that is why the GLM will give better BIC than a GAM model sometimes

# CONNECTIVITY VERSION

Fish_model_for_C_glm1 <- Fish_model_for_C_glm %>%
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

Mod.full_C <- glm.nb(richness ~ ., data = Fish_model_for_C_glm)

n <- nrow(Fish1)

step(Mod.full_C, direction = "backward")
step(Mod.full_C, direction = "backward", k = log(n))

Best_GLM_Model_AIC_C <- glm.nb(richness ~ lake_altitude + lake_mean_depth.m + 
                                 lake_fetch.km + lake_straightdistance_tosea.km + catch_mean_altitude.m + 
                                 mean_slopeoflake_catch + catch_mean_temp + catch_mean_annualrainfall + 
                                 catch_lakes_count + catch_visits_watersport + catch_visits_all + 
                                 catch_sea_accessible, data = Fish1)

#CHECKING FOR THE C GLM

summary(Best_GLM_Model_AIC_C)

S <- BIC(Best_GLM_Model_AIC_C)

anova(Best_GLM_Model_AIC_C)

dev_res <- residuals(Best_GLM_Model_AIC_C, type = "deviance")

# Get linear predictor
linear_pred <- predict(Best_GLM_Model_AIC_C, type = "link")

# Plot
plot(linear_pred, dev_res,
     xlab = "Linear Predictor (η)",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Linear Predictor")



resids <- residuals(Best_GLM_Model_AIC_C, type = "dev")

plot(fitted(Best_GLM_Model_AIC_C), resids)

# BASIC RESIDUAL DIAGNOSTICS CHECKS FOR S MODEL

dev_res <- residuals(Best_GLM_Model_AIC, type = "deviance")

# Get linear predictor
linear_pred <- predict(Best_GLM_Model_AIC, type = "link")

# Plot
plot(linear_pred, dev_res,
     xlab = "Linear Predictor (η)",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Linear Predictor")



resids <- residuals(Best_GLM_Model_AIC, type = "dev")

plot(fitted(Best_GLM_Model_AIC), resids)

#RESULTS COULD BE GREATLY IMPROVED UPON BY USING GAMs


# BEST STRESSOR MODEL

  Stressor_model <- gam(richness ~ s(lat,lon,k=30, bs = "gp", m = c(1,1)) + s(sqrt(catch_mean_altitude.m), k = 5, bs = "cs") + s((lake_straightdistance_tosea.km), k = 8, bs = "cs") +
                          s(log(lake_perimeter.m), k = 5, bs = "cs") + s(sqrt(catch_urbancover), k = 3) + s(catch_agricover) + lake_riverbasindistrict,
                        data = Fish1, family = nb(link = "log"))
                  

# Again this is putting an assumption on our spatial coordinates, Wood book page 359 - gp basis

summary((Stressor_model))
summary(fitted(Stressor_model))

A <- AIC(Stressor_model)
B <- BIC(Stressor_model)


gam.check(Stressor_model)

plot(Stressor_model)

par(mfrow = c(1, 1))
anova(Stressor_model)

mean(fitted(Stressor_model))
mean(Fish1$richness)

concurvity(Stressor_model, full = TRUE)
concurvity(Stressor_model, full = FALSE)

anova(mod1,Stressor_model, test = "Chisq")

# RESIDUAL CHECKING 

resids1 <- residuals(Stressor_model, type = "dev")
plot(fitted(Stressor_model), resids1,
xlab = "Fitted Values",
ylab = "Deviance Residuals",
main = "Deviance Residuals vs Fitted Values")

plot(Fish1$lon,residuals(Stressor_model))
plot(Fish1$lat,residuals(Stressor_model))
plot(sqrt(Fish1$catch_urbancover),residuals(Stressor_model))
plot(log(Fish1$lake_perimeter.m), residuals(Stressor_model))
plot(sqrt(Fish1$lake_altitude),residuals(Stressor_model))
plot((Fish1$lake_straightdistance_tosea.km),residuals(Stressor_model))   # Think about taking the square root
plot(sqrt(Fish1$mean_slopeoflake_catch),residuals(Stressor_model))


#######################################################################################################################################

#Some Connectivity variables have a high proportion of zeros which may lead to difficulty when modelling.

start_col <- which(names(Fish_Group_C) == "catch_lakearea.p")
end_col <- which(names(Fish_Group_C) == "catch_sea_accessible")
cols <- names(Fish_Group_C)[start_col:end_col]




for (i in cols) {
  zero_prop <- sum(Fish_Group_C[[i]] == 0, na.rm = TRUE) / length(Fish_Group_C[[i]])
  print(paste("Proportion of 0s in", i, ":", zero_prop))
}


###################################################################################################################################################################################


Gam_C_2 <- gam(richness ~ s(lat,lon, k = 40, bs = "tp") + s(log(lake_perimeter.m), bs = "cs") + 
                          s(sqrt(lake_altitude), bs = "cs") + lake_riverbasindistrict +
                 s(log(catch_visits_all), k = 5, bs = "cs") + s(log(catch_visits_fish), bs = "cs") + (catch_sea_accessible) +
                 s(log(catch_obstacles_count + 0.0001))
               
                 , data = Fish1, family = nb(link = "log"))

summary(Gam_C_2)

summary(fitted(Gam_C_2))

anova(Gam_C_2)

plot(Gam_C_2)

gam.check(Gam_C_2)

B1 <- AIC(Gam_C_2)
B2 <- BIC(Gam_C_2)


resids2 <- residuals(Gam_C_2, type = "dev")
plot(fitted(Gam_C_2), resids1,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")

############################################################################################################################################################



Gam_C_3 <- gam(richness ~ s(lat,lon, k = 50, bs = "tp") + s(log(lake_perimeter.m), bs = "cs", k = 5) + 
                 s(sqrt(lake_altitude), bs = "cs", k = 5) +  s(log(catch_visits_fish), k = 3, bs = "cs") 
                  + lake_riverbasindistrict + s(sqrt(catch_rivers_length_total), bs = "cs") + s(sqrt(catch_lakes_total_perim))
              + s(sqrt(catch_ponds_total_perim)),
                data = Fish1, family = nb(link = "log"))



summary(Gam_C_3)

summary(fitted(Gam_C_3))

anova(Gam_C_3)

plot(Gam_C_3)

gam.check(Gam_C_3)

C1 <- AIC(Gam_C_3)
C2 <- BIC(Gam_C_3)


resids2 <- residuals(Gam_C_3, type = "dev")
plot(fitted(Gam_C_3), resids1,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")



##################################################################################################################################################################################


Gam_C_4 <- gam(richness ~ lake_riverbasindistrict + s(lat,lon) + catch_sea_accessible +
                 s(log(lake_perimeter.m), bs = "cs") + s((lake_altitude)) +
                 s(sqrt(catch_rivers_length_total)), data = Fish1, family = nb(link = "log"))


summary(Gam_C_4)


O <- BIC(Gam_C_4)


########################################################################################################################################################################################################################


Gam_C_5 <- gam(richness ~ s(lat,lon, k = 50, bs = "tp") + s(log(lake_perimeter.m), bs = "cs", k = 5) + 
                 s(sqrt(lake_altitude), bs = "cs", k = 5) + lake_riverbasindistrict +
                 catch_lakes_total_perim  + catch_ponds_total_perim + s(sqrt(catch_rivers_length_total), bs = "cs") +
                 s(log(catch_visits_fish), bs = "cs", k = 3) + catch_sea_accessible, data = Fish1, family = nb(link = "log"))


summary(Gam_C_5)

summary(fitted(Gam_C_5))

anova(Gam_C_5)

plot(Gam_C_5)

gam.check(Gam_C_5)

G1 <- AIC(Gam_C_5)
G2 <- BIC(Gam_C_5)
?relevel

#################################################################################################################################################################################################################################


Gam_C_6 <- gam(richness ~ s(lat,lon, k= 40, bs = "tp") + s(log(lake_perimeter.m), bs = "cs", k = 5) + 
                 s(sqrt(lake_altitude),bs = "cs", k = 5) + lake_riverbasindistrict + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") +
                 s(mean_slopeoflake_catch) + s(log(catch_lakes_count + 0.000001), k = 5) + s(catch_visits_fish, k = 5) + s(catch_visits_watersport, k = 5) +
                 catch_sea_accessible, data = Fish1, family = nb(link = "log")
               )



summary(Gam_C_6)

summary(fitted(Gam_C_6))

anova(Gam_C_6)

plot(Gam_C_6)

gam.check(Gam_C_6)

H1 <- AIC(Gam_C_6)
H2 <- BIC(Gam_C_6)

resids4 <- residuals(Gam_C_6, type = "dev")
plot(fitted(Gam_C_6), resids3,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")


# Second Version of the model in which we switch out the continuous variables for the categorical ones to try and help with the high proportion of zeros in the covariates

Gam_C_6 <- gam(richness ~ s(lat,lon, k= 40, bs = "tp") + s(log(lake_perimeter.m), bs = "tp", k = 5) + 
                 s(sqrt(lake_altitude),bs = "tp", k = 5) + lake_riverbasindistrict + s(lake_straightdistance_tosea.km, k = 8, bs = "tp") +
                 s(mean_slopeoflake_catch) + catch_lakes_count + s(catch_visits_fish, k = 5) +
                 catch_sea_accessible, data = Fish2, family = nb(link = "identity"))



##########################################################################################################################################################################################################################################################################################################



Gam_C_7 <- gam(richness ~ s(lat,lon, k= 40, bs = "gp", m = c(1,1)) + s(log(lake_perimeter.m), k = 5) + 
                 s(sqrt(lake_altitude), k = 5) + lake_riverbasindistrict + s(lake_straightdistance_tosea.km, k = 8) +
                 s(mean_slopeoflake_catch) + s(log(catch_lakes_count + 0.000001), k = 5) + s(catch_visits_fish, k = 5) + s(catch_visits_watersport, k = 5) +
                 catch_sea_accessible + ti(catch_visits_fish,catch_visits_watersport), data = Fish1, family = nb(link = "log"))




# This model has a change in the way we view how our spatial coordinates work.
# If I go to page 359 chapter 7 of the Wood book the general thinking is explained.


summary(Gam_C_7)

summary(fitted(Gam_C_7))

anova(Gam_C_7)

plot(Gam_C_7)

gam.check(Gam_C_7)

I1 <- AIC(Gam_C_7)
I2 <- BIC(Gam_C_7)

resids5 <- residuals(Gam_C_7, type = "dev")
plot(fitted(Gam_C_7), resids5,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")


concurvity(Gam_C_7, full = TRUE)
concurvity(Gam_C_7, full = FALSE)


# Okay it is finally time to look into spatial autocorrelation in my Gam model, because it might be worth it from what I can tell, who knows I love typing on
# these computers it is very fun I really 


######################################################################################################################################################################################################################



Gam_C_8 <- gam(richness ~ s(lat,lon, k= 40, bs = "gp", m = c(1,1)) + s(log(lake_perimeter.m), k = 5, bs = "cs") + 
                 s(sqrt(lake_altitude), k = 5, bs = "cs") + lake_riverbasindistrict + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") +
                 s(sqrt(catch_lakes_total_perim), bs = "cr") + s((catch_visits_watersport), bs = "cr") + catch_sea_accessible, data = Fish1, family = nb(link = "log"))


summary(Gam_C_8)

summary(fitted(Gam_C_8))

anova(Gam_C_8)

plot(Gam_C_8)

par(mfrow = c(2, 2))

gam.check(Gam_C_8)

par(mfrow = c(1, 1))

J1 <- AIC(Gam_C_8)
J2 <- BIC(Gam_C_8)


resids6 <- residuals(Gam_C_8, type = "dev")
plot(fitted(Gam_C_8), resids6,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")


concurvity(Gam_C_7, full = TRUE)
concurvity(Gam_C_7, full = FALSE)


##################################################################################################################################################################################################

Gam_C_Final <- gam(richness ~ s(lat,lon, k= 40, bs = "gp", m = c(1,1)) + s(log(lake_perimeter.m), k = 5, bs = "cs") + 
                 s(sqrt(lake_altitude), k = 5, bs = "cs") + lake_riverbasindistrict + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") +
                 s(sqrt(catch_lakes_total_perim), bs = "cr") + s((catch_visits_watersport), bs = "cr") + catch_sea_accessible, data = Fish1, family = nb(link = "log"))



summary(Gam_C_Final)

summary(fitted(Gam_C_Final))

anova(Gam_C_Final)

plot(Gam_C_Final)

par(mfrow = c(2, 2))

gam.check(Gam_C_Final)

par(mfrow = c(1, 1))

P1 <- AIC(Gam_C_Final)
P2 <- BIC(Gam_C_Final)


resids7 <- residuals(Gam_C_Final, type = "dev")
plot(fitted(Gam_C_Final), resids7,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")


concurvity(Gam_C_7, full = TRUE)
concurvity(Gam_C_7, full = FALSE)



####################################################################################################################################################################
#Research Question 3 



Fish_Group_Model_Full <- Fish_Group_Model_Full0 %>%
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







###########################################################################################################################################






Gam_C_Final <- gam(richness ~ s(lat,lon, k= 40, bs = "gp", m = c(1,1)) + s(log(lake_perimeter.m), k = 5, bs = "cs") + 
                     s(sqrt(lake_altitude), k = 5, bs = "cs") + lake_riverbasindistrict + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") +
                     s(sqrt(catch_lakes_total_perim), bs = "cr") + s((catch_visits_watersport), bs = "cr") + catch_sea_accessible, data = Fish1, family = nb(link = "log"))


Stressor_model <- gam(richness ~ s(lat,lon,k=30, bs = "gp", m = c(1,1)) + s(sqrt(catch_mean_altitude.m), k = 5, bs = "cs") + s((lake_straightdistance_tosea.km), k = 8, bs = "cs") +
                        s(log(lake_perimeter.m), k = 5, bs = "cs") + s(sqrt(catch_urbancover), k = 3) + s(catch_agricover) + lake_riverbasindistrict,
                      data = Fish1, family = nb(link = "log"))



Interaction_model <- gam(richness ~ s(catch_mean_altitude.m, k = 5, bs = "cs") + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") + s(lat,lon,k=30, bs = "gp", m = c(1,1)) + 
                           + s(lake_perimeter.m, k = 5, bs = "cs") + lake_riverbasindistrict + s(catch_agricover) + s(catch_urbancover) +
                           s(catch_lakes_total_perim, bs = "cr") + s(catch_visits_watersport, bs = "cr") + catch_sea_accessible +
                           ti(catch_agricover, catch_lakes_total_perim) + ti(catch_urbancover, catch_lakes_total_perim),
                         data = Fish_Group_Model_Full, family = nb(link = "log"))


vis.gam(Interaction_model, 
        view = c("catch_agricover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 35, phi = 45)


vis.gam(Interaction_model, 
        view = c("catch_urbancover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 35, phi = 45)

plot.gam(Interaction_model)

summary(Interaction_model)

summary(fitted(Interaction_model))

anova(Interaction_model)

plot(Interaction_model)


gam.check(Interaction_model)

A2 <- AIC(Interaction_model)
A5 <- BIC(Interaction_model)


resids7 <- residuals(Interaction_model, type = "dev")
plot(fitted(Interaction_model), resids6,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")


concurvity(Interaction_model, full = TRUE)
concurvity(Interaction_model, full = FALSE)


# This is good so far, two significant relationships with watersport but it leaves more
# to be thought about.



##########################################################################################################################




Interaction_model2 <- gam(richness ~ s(catch_mean_altitude.m, k = 5, bs = "cs") + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") + s(lat,lon,k=30, bs = "gp", m = c(1,1)) + 
                           + s(lake_perimeter.m, k = 5, bs = "cs") + lake_riverbasindistrict + s(catch_agricover) + s(catch_urbancover) +
                           s(catch_lakes_total_perim, bs = "cr") + s(catch_visits_watersport, bs = "cr") + catch_sea_accessible +
                           ti(catch_agricover, catch_visits_watersport) + ti(catch_urbancover, catch_visits_watersport),
                         data = Fish_Group_Model_Full, family = nb(link = "log"))



vis.gam(Interaction_model2, 
        view = c("catch_agricover", "catch_visits_watersport"),
        plot.type = "persp", 
        theta = 35, phi = 45)


vis.gam(Interaction_model2, 
        view = c("catch_urbancover", "catch_visits_watersport"),
        plot.type = "persp", 
        theta = 35, phi = 45)

plot.gam(Interaction_model2)

summary(Interaction_model2)

summary(fitted(Interaction_model2))

anova(Interaction_model2)

plot(Interaction_model2)


gam.check(Interaction_model2)

B5 <- BIC(Interaction_model2)

##################################################################################################################################################




Interaction_model3 <- gam(richness ~ s(catch_mean_altitude.m, k = 5, bs = "cs") + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") + s(lat,lon,k=30, bs = "gp", m = c(1,1)) + 
                            + s(lake_perimeter.m, k = 5, bs = "cs") + lake_riverbasindistrict + s(catch_agricover) + s(catch_urbancover) +
                            s(catch_lakes_total_perim, bs = "cr") + s(catch_visits_watersport, bs = "cr") + catch_sea_accessible +
                            ti(catch_agricover, by = catch_sea_accessible) + ti(catch_urbancover, by = catch_sea_accessible),
                          data = Fish_Group_Model_Full, family = nb(link = "log"))




vis.gam(Interaction_model3, 
        view = c("catch_agricover", "catch_sea_accessible"),
        plot.type = "persp", 
        theta = 25, phi = 45)

plot(model, select = 1)


vis.gam(Interaction_model3, 
        view = c("catch_urbancover", "catch_sea_accessible"),
        plot.type = "persp", 
        theta = 35, phi = 45)

plot(Interaction_model3)

summary(Interaction_model3)

summary(fitted(Interaction_model3))

anova(Interaction_model3)

plot(Interaction_model3)


gam.check(Interaction_model3)

C5 <- BIC(Interaction_model3)



##############################################################################################################################


Interaction_model4 <- gam(richness ~ s(catch_mean_altitude.m, k = 5, bs = "cs") + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") + s(lat,lon,k=30, bs = "gp", m = c(1,1)) + 
                            + s(lake_perimeter.m, k = 5, bs = "cs") + lake_riverbasindistrict + s(catch_agricover) + s(catch_urbancover) +
                            s(catch_lakes_total_perim, bs = "cr") + s(catch_visits_watersport, bs = "cr") + catch_sea_accessible +
                            ti(catch_agricover, by = catch_sea_accessible) + ti(catch_urbancover, by = catch_sea_accessible) +
                            ti(catch_agricover, catch_visits_watersport) + ti(catch_urbancover, catch_visits_watersport) + 
                            ti(catch_agricover, catch_lakes_total_perim) + ti(catch_urbancover, catch_lakes_total_perim),
        
                          data = Fish_Group_Model_Full, family = nb(link = "log"))





vis.gam(Interaction_model4, 
        view = c("catch_agricover", "catch_sea_accessible"),
        plot.type = "persp", 
        theta = 25, phi = 45)

plot(model, select = 1)


vis.gam(Interaction_model4, 
        view = c("catch_urbancover", "catch_sea_accessible"),
        plot.type = "persp", 
        theta = 35, phi = 45)

plot.gam(Interaction_model4)

summary(Interaction_model4)

summary(fitted(Interaction_model4))

anova(Interaction_model4)

plot(Interaction_model4)


gam.check(Interaction_model4)

D5 <- BIC(Interaction_model4)



#######################################################################################################################################################


Interaction_model5 <- gam(richness ~ s(catch_mean_altitude.m, k = 5, bs = "cs") + s(lake_straightdistance_tosea.km, k = 8, bs = "cs") + s(lat,lon,k=30, bs = "gp", m = c(1,1)) + 
                           + s(lake_perimeter.m, k = 5, bs = "cs") + lake_riverbasindistrict + s(catch_agricover) + s(catch_urbancover) +
                           s(catch_lakes_total_perim, bs = "cr") + s(catch_visits_watersport, bs = "cr") + catch_sea_accessible +
                           ti(catch_agricover, catch_lakes_total_perim) + ti(catch_urbancover, catch_lakes_total_perim) + ti(catch_agricover, by = catch_sea_accessible),
                         data = Fish_Group_Model_Full, family = nb(link = "log"))





vis.gam(Interaction_model5, 
        view = c("catch_agricover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 25, phi = 45)

plot(model, select = 1)


vis.gam(Interaction_model5, 
        view = c("catch_urbancover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 35, phi = 45)

plot.gam(Interaction_model5)

summary(Interaction_model5)

summary(fitted(Interaction_model5))

anova(Interaction_model5)

plot(Interaction_model5)


summary(j)

gam.check(Interaction_model5)

E5 <- BIC(Interaction_model5)



##################################################################################################################################################################





Interaction_model_Final <- gam(richness ~ s(catch_mean_altitude.m, k = 5, bs = "cs")  + s(lat,lon,k=30, bs = "gp", m = c(1,1)) + s(lake_straightdistance_tosea.km, k = 8, bs = "cs")
                            + s(lake_perimeter.m, k = 5, bs = "cs") + lake_riverbasindistrict + s(catch_agricover) + s(catch_urbancover) +
                            s(catch_lakes_total_perim, bs = "cs")  + catch_sea_accessible +
                            ti(catch_agricover, catch_lakes_total_perim) + ti(catch_urbancover, catch_lakes_total_perim) + ti(catch_agricover, by = catch_sea_accessible),
                          data = Fish_Group_Model_Full, family = nb(link = "log"))
    

summary(Interaction_model_Final)

summary(fitted(Interaction_model_Final))

anova(Interaction_model_Final)

plot(Interaction_model_Final)

gam.check(Interaction_model_Final)

F5 <- BIC(Interaction_model_Final)

vis.gam(Interaction_model_Final, 
        view = c("catch_agricover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 25, phi = 35)


vis.gam(Interaction_model_Final, 
        view = c("catch_urbancover", "catch_lakes_total_perim"),
        plot.type = "persp", 
        theta = 45, phi = 50)


plot(Interaction_model_Final, select = 3, 
     pers = TRUE,          # use persp-style 3D plot
     theta = 40,           # rotation
     phi = 30,             # elevation
     r = 2) 

par(mfrow = c(1,2))



##########################################################################################################################

#Spatial Autocorrelation check 

res <- residuals(Stressor_model)


coords <- cbind(Fish1$lon, Fish1$lat)
nb <- knn2nb(knearneigh(coords, k = 4))
lw <- nb2listw(nb)

moran.test(Fish1$richness, lw)
moran.test(res, lw)