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
library(grid)
library(spdep)
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

corrplot(correlation_typology_num, method="number",number.cex = 0.62, tl.cex = 0.4, tl.col = "steelblue")
text(x = -4, y = ncol(correlation_typology_num) + 2, labels = "Correlation Matrix", adj = c(0, 0), cex = 0.8)


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

#To generate the plots pre-transformation use the identity link


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



