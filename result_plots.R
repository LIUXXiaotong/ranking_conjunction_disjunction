library(ggplot2)
library(tidyverse)

df_condition1 <- read.csv("condition1_results.csv") #condition 1: a high, b low, condition b|a high
df_condition2 <- read.csv("condition2_results.csv") #condition 2: a high, b high, condition b|a high
df_condition3 <- read.csv("condition3_results.csv") #condition 3: a high b low, condition b|a low
df_condition4 <- read.csv("condition4_results.csv") #condition 4: a high b high, condition b|a low
df_condition5 <- read.csv("condition5_results.csv") #condition 5: a low b low, condition b|a high
df_condition6 <- read.csv("condition6_results.csv") #condition 6: a low, b high, condition b|a high
df_condition7 <- read.csv("condition7_results.csv") #condition 7: a low b low, condition b|a low
df_condition8 <- read.csv("condition8_results.csv") #condition 8: a low b high, condition b|a low

#Question: if the rates at which the conjunction fallacy occurs different across conditions? 

#From Costello & Watt's papers: 
#Prediction 1: the influence of the distance between underlying probabilities 
#original PT+N model predicts that the closest P(A and B) to P(A) the, the greater the chance the conjunction fallacy would occur 
# <-- these values are closet when P(A) is low and both P(B) and P(A|B) are high 
# <-- supporting data: (Carlson & Yates, 1989; Costello, 2009b; Fantino et al., 1997; Gavanski & Roskos-Ewoldsen, 1991)
# --> this prediction can be tested with the current simualtion data 

#Prediction 2: the influence of the distance between underlying probabilities 
#If an event B causes A, then by definition P(A|B) > P(A) (the probability of A, given the causing event B is higher than the probability of A simpliciter)
# --> this prediction cannot be tested with the current data 

#Prediction 3: the rate at which the conjunctions fallacy occurs 
#average *estimated* value for the conjunction P(A and B) never be greater than the average *estimated* value for its constituent *P(A) 
# --> as a consequence, an individual estimate of P(A and B) can randomly fall above an estiamte of P(A) at most 50% of the time 
# <-- data:: there are some pairs of events for which fallacy rates are signiﬁcantly higher than this bound (Tversky and Kahneman’s Linda and Bill being two examples).

#Let us start with prediction 3 to check the rate at which the conjunction fallacy occurs
# Vector of conditions
conditions <- paste0("df_condition", 1:8)

# Loop over each condition and create the summary DataFrame
for (cond in conditions) {
  # Get the current DataFrame
  df <- get(cond)
  
  # Create the summary DataFrame
  summary_df <- df %>% 
    mutate(
      mean_logical_with_ties = mean_R1_with_ties + mean_R2_with_ties,
      mean_singleconjunction_with_ties = mean_R3_with_ties + mean_R4_with_ties,
      mean_doubleconjucntion_with_ties = mean_R5_with_ties + mean_R6_with_ties,
      mean_conjunctions = mean_singleconjunction_with_ties + mean_doubleconjucntion_with_ties
    ) %>% 
    select(sample_size_in_simulaton,
           mean_logical_with_ties,
           mean_singleconjunction_with_ties,
           mean_doubleconjucntion_with_ties,
           mean_conjunctions)
  
  # Assign the summary DataFrame with a new name
  assign(paste0(cond, "_summary"), summary_df)
}

view(df_condition1_summary)
view(df_condition2_summary)
view(df_condition3_summary)
view(df_condition4_summary)
view(df_condition5_summary)
view(df_condition6_summary)
view(df_condition7_summary)
view(df_condition8_summary)



