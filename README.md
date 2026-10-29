# Are Fish More Stressed In Connected Freshwaters
An investigation into the relationships between fish species richness in UK freshwaters and other types of variables which help determine fish species richness
## Project Overview
How does connectivity, stressors and the interaction between them affect fish species richness
in UK Freshwaters. This dataset consisted of three variable types, connectivity, stressor and typology
variables. Connectivity variables indicate how connected a freshwater is to another freshwater.
There are two stressor variables. Urban land cover percentage and agricultural land cover percentage.
Typology variables describe the attributes of the freshwater and surrounding catchment; 
these are used as control variables. Richness is the response variable and is a count variable.
## Research Questions
The following relationships will be explored:
- The stress put on the fish species richness in fresh waters by the surrounding land cover
of the freshwaters (1).
- The relationship between fish species richness in fresh waters and connectivity to other
nearby freshwaters (2).
- How the relationship between the stressor variables and species richness is affected
depending on how connected the given freshwater is to other nearby freshwaters (3).
## Repository Contents
- README.md – Overview of the project, including selected figures.

- R-CODE-DISSERTATION.R – R code used for data analysis and figure generation.

- Final_Dissertation.pdf – Full university dissertation.

- Figures/ – Selected figures from the analysis.

- .gitignore – Specifies files that should not be tracked by Git.

- LICENSE - MIT License.
### Data Availability 
The original xlsx has not been made public 
## Methods and Analysis 
A Poisson distribution was the first choice for modelling due to the response being a count variable.
Overdispersion led to a negative binomial model with a log link being chosen. Exploratory analysis
showed the relationships within the data were non-linear. A framework for variable selection was used to
select the best fitting models. GLMs were used at first before moving to GAMs to better model the non-linear
relationships in the data. The GAMs used cubic regression splines for single-variable smooths and a
Gaussian process smooth for the spatial effect of longitude and latitude. Estimated smoothing curves were
used to determine the effects a variable had on richness.
## Results Summary 
The main findings of the study are summarised below. For the full results please refer to 'Final_Dissertation.pdf'

- Agricultural land cover percentage showed a statistically significant relationship with richness, it had a positive 
increasing partial effect.

- Modelling connectivity variables with richness was challenging due to the high proportion of zeros within 
the data. Typology variables were included in the model to avoid confounding as in the stressor model.
The connectivity variables included in the model did not have statistically significant relationships with richness,
except the binary variable indicating whether the freshwater in the catchment was accessible from the sea.

- Only one of the interaction effects was statistically significant. The other two were not statistically significant but showed potentially 
interesting patterns in their effects on richness.
## Reproducibility 
The R code is provided in the 'R-CODE-DISSERTATION.R' file. The original dataset is not in the repository.
## Disertation
Refer to 'Final_Dissertation.pdf' for full analysis and results.


























































































































































