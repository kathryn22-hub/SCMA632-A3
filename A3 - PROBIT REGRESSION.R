options(repos = c(CRAN = "https://cran.rstudio.com/"))

setwd('E:\\ASSIGNMENT\\Data')
getwd()



# Load necessary libraries
library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation
library(margins)  # For calculating marginal effects
library(ggplot2)  # For visualization

df = read.csv('NSSO68.csv', header=TRUE)

subset_df <- df %>%
  filter(state_1 == 'Pun') %>%
  select(eggsno_q, fishprawn_q, goatmeat_q, beef_q,chicken_q, pork_q)
print(subset_df)

#Create Target Variable
subset_df$nv <- ifelse(rowSums(subset_df[,c('eggsno_q','fishprawn_q','goatmeat_q','beef_q','chicken_q','pork_q')])>0,1,0)
 head('subset_df',)
summary('subset_df$nv',) 
 
# Fit the probit regression model
probit_model <- glm(nv ~ eggsno_q + fishprawn_q + goatmeat_q + beef_q + chicken_q + pork_q,family = binomial(link = "probit"), data = subset_df) 
# View the summary of the model
 summary(probit_model)
 
 # Calculate marginal effects
 marginal_effects <- margins(probit_model)
 summary(marginal_effects)
 