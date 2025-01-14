
rm(list=ls())
source("simulation_function.R")

################################################ simulate rankings 
# condition 1: a high, b low, condition b|a high
run_simulation(nsim = 1e4,
               alpha_a = 20, beta_a = 5,
               alpha_b = 5, beta_b = 20,
               alpha_ba = 20, beta_ba = 5,
               condition_name = "condition1", seed = 1234)


# condition 2: a high, b high, condition b|a high
run_simulation(nsim = 1e4, 
               alpha_a = 20, beta_a = 5,
               alpha_b = 20, beta_b = 5, 
               alpha_ba = 20, beta_ba = 5,
               condition_name = "condition2", seed = 1234)

# condition 3: a high b low, condition b|a low
run_simulation(nsim = 1e4,
               alpha_a = 20, beta_a = 5, 
               alpha_b = 5, beta_b = 20, 
               alpha_ba = 5, beta_ba = 20, 
               condition_name = "condition3", seed = 1234)

# condition 4: a high b high, condition b|a low
run_simulation(nsim = 1e4,
               alpha_a = 20, beta_a = 5, 
               alpha_b = 20, beta_b = 5, 
               alpha_ba = 5, beta_ba = 20, 
               condition_name = "condition4", seed = 1234)

# condition 5: a low b low, condition b|a high
run_simulation(nsim = 1e4,
               alpha_a = 5, beta_a = 20, 
               alpha_b = 5, beta_b = 20, 
               alpha_ba = 20, beta_ba = 5, 
               condition_name = "condition5", seed = 1234)

# condition 6: a low, b high, condition b|a high
run_simulation(nsim = 1e4,
               alpha_a = 5, beta_a = 20, 
               alpha_b = 20, beta_b = 5, 
               alpha_ba = 20, beta_ba = 5, 
               condition_name = "condition6", seed = 1234)

# condition 7: a low b low, condition b|a low
run_simulation(nsim = 1e4,
               alpha_a = 5, beta_a = 20, 
               alpha_b = 5, beta_b = 20, 
               alpha_ba = 5, beta_ba = 20, 
               condition_name = "condition7", seed = 1234)


# condition 8: a low b high, condition b|a low
run_simulation(nsim = 1e4,
               alpha_a = 5, beta_a = 20, 
               alpha_b = 20, beta_b = 5, 
               alpha_ba = 5, beta_ba = 20, 
               condition_name = "condition8", seed = 1234)


###################################### simulate event sets 
nsim = 1e4
set.seed = 1234

a <- rbeta(nsim, 20, 5)
b <- rbeta(nsim, 5, 20)
b_a = rbeta(nsim, 20, 5)
aandb = a*b_a

event_set_condition8 <- cbind(a, b, aandb) %>% as.matrix()


set.seed = 1234

a <- rbeta(nsim, 5, 20)
b <- rbeta(nsim, 20, 5)
b_a = rbeta(nsim, 5, 20)
aandb = a*b_a

event_set_condition2 <- cbind(a, b, aandb) %>% as.matrix()

