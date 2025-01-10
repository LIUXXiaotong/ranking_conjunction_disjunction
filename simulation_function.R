rm(list=ls())

# Required libraries
library(dplyr)
library(parallel)
library(matrixStats)

#simulation function
run_simulation <- function(nsim, alpha_a, beta_a,
                           alpha_b, beta_b,
                           alpha_ba, beta_ba,
                           condition_name, seed = NULL) {
  
  # Set random seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  
  # Generate random values for a, b, aandb based on specified beta distributions
  a <- rbeta(nsim, alpha_a, beta_a) #marginal event A
  b <- rbeta(nsim, alpha_b, beta_b) #marginal event B
  b_a <- rbeta(nsim, alpha_ba, beta_ba) #P(B|A)
  aandb <- a * b_a #conjunction event
  
  # Create event_set matrix
  event_set <- cbind(a, b, aandb) %>% as.matrix()
  
  
  pr_linear <- function(N, pr_1, pr_2, pr_3){
    
    samples <-  expand.grid(C1 = 2:N, 
                            C2 = 1:(N-1),  
                            C3 = 0:(N-2)
    )%>% 
      filter(C1 > C2 & C2 > C3)
    
    
    samples <- samples %>% 
      mutate(
        ln_pr_C1 = dbinom(C1, N, pr_1, log = TRUE),
        ln_pr_C2 = dbinom(C2, N, pr_2, log = TRUE),
        ln_pr_C3 = dbinom(C3, N, pr_3, log = TRUE),
        LSE_entry = ln_pr_C1 + ln_pr_C2 + ln_pr_C3)
    
    lx <- samples$LSE_entry
    output <- logSumExp(lx)
    output <- exp(output)
    return(output)
  }
  
  pr_one_tie_case_1 <- function(N, pr_1, pr_2, pr_3){
    
    samples <-  expand.grid(C12 = 1:N, 
                            C3 = 0:(N-1) 
    )%>% 
      filter(C12 > C3)
    
    samples <- samples %>% 
      mutate(
        ln_pr_C1 = dbinom(C12, N, pr_1, log = TRUE),
        ln_pr_C2 = dbinom(C12, N, pr_2, log = TRUE),
        ln_pr_C3 = dbinom(C3, N, pr_3, log = TRUE),
        LSE_entry = ln_pr_C1 + ln_pr_C2 + ln_pr_C3)
    
    lx <- samples$LSE_entry
    output <- logSumExp(lx)
    output <- exp(output)
    return(output)
  }
  
  pr_one_tie_case_2 <- function(N, pr_1, pr_2, pr_3){
    
    samples <-  expand.grid(C1 = 1:N, 
                            C23 = 0:(N-1) 
    )%>% 
      filter(C1 > C23)
    
    samples <- samples %>% 
      mutate(
        ln_pr_C1 = dbinom(C1, N, pr_1, log = TRUE),
        ln_pr_C2 = dbinom(C23, N, pr_2, log = TRUE),
        ln_pr_C3 = dbinom(C23, N, pr_3, log = TRUE),
        LSE_entry = ln_pr_C1 + ln_pr_C2 + ln_pr_C3)
    
    lx <- samples$LSE_entry
    output <- logSumExp(lx)
    output <- exp(output)
    return(output)
  }
  
  pr_all_equal <- function(N, pr_1, pr_2, pr_3){
    samples <-  data.frame(C = 0:N) 
    
    samples <- samples %>% 
      mutate(
        ln_pr_C1 = dbinom(C, N, pr_1, log = TRUE),
        ln_pr_C2 = dbinom(C, N, pr_2, log = TRUE),
        ln_pr_C3 = dbinom(C, N, pr_3, log = TRUE),
        LSE_entry = ln_pr_C1 + ln_pr_C2 + ln_pr_C3)
    
    lx <- samples$LSE_entry
    output <- logSumExp(lx)
    output <- exp(output)
    return(output)
  }
  
  get_diff_rates_ranking <- function(N, a, b, aandb) {
    
    R1 <- pr_linear(N, a, b, aandb) #logical
    R2 <- pr_linear(N, b, a, aandb) #logical 
    R3 <- pr_linear(N, a, aandb, b) #single conjunction fallacy
    R4 <- pr_linear(N, b, aandb, a) #single conjunction fallacy
    R5 <- pr_linear(N, aandb, a, b) #double conjunction fallacy
    R6 <- pr_linear(N, aandb, b, a) #double conjunction fallacy
    
    #ranking with ties 
    # A = B > A&B --> R1 or R2 (logical ranking)
    ( T1 <- pr_one_tie_case_1(N, a, b, aandb) )
    # A = (A&B) > B  --> R3 (single conjunction fallacy) or R5 (double conjunction fallacy)
    ( T2 <- pr_one_tie_case_1(N, a, aandb, b) )
    # B= (A&B) > A --> R4 (single conjunction fallacy) or R6 (double conjunction fallacy)
    ( T3 <- pr_one_tie_case_1(N, b, aandb, a) )
    #(A∧B)>A=B --> R5 (double conjunction fallacy) or R6 (double conjunction fallacy)
    ( T4 <- pr_one_tie_case_2(N, aandb, a, b) )
    # A > B = (A&B) --> R1 (logical) or R3 (single conjunction fallacy)
    ( T5 <- pr_one_tie_case_2(N, a, b, aandb) )
    # B > A= (A&B) -->  R2 (logical) or R4 (single conjunction fallacy)
    ( T6 <- pr_one_tie_case_2(N, b, a, aandb) )
    
    #  A=B= (A&B) -->  R1, R2, R3, R4, R5, or R6
    ( T7 <- pr_all_equal(N, aandb, a, b) )
    
    R1_with_ties <- pr_linear(N, a, b, aandb) + 0.5*T1+0.5*T5+1/6*T7 #R1, logical
    R2_with_ties <- pr_linear(N, b, a, aandb) + 0.5*T1+0.5*T6+1/6*T7 #R2, logical
    R3_with_ties <- pr_linear(N, a, aandb, b) + 0.5*T2+0.5*T5+1/6*T7 #R3, single conjunction fallacy
    R4_with_ties <- pr_linear(N, b, aandb, a)  + 0.5*T3+0.5*T6+1/6*T7#R4, single conjunction fallacy
    R5_with_ties <- pr_linear(N, aandb, a, b) + 0.5*T2+0.5*T4+1/6*T7 #R5, double conjunction fallacy
    R6_with_ties <- pr_linear(N, aandb, b, a) + 0.5*T3+0.5*T4+1/6*T7 #R6, double conjunction fallacy
    
    #sum1 <- sum(R1, R2, R3, R4, R5, R6, T1, T2, T3, T4, T5, T6, T7) #for checking the codes
    #sum2 <- sum( R1_with_ties, R2_with_ties, R3_with_ties, R4_with_ties, # for checking the codes
    #R5_with_ties, R6_with_ties)
    
    results <- c(R1, R2, R3, R4, R5, R6, T1, T2, T3, T4, T5, T6, T7,
                 R1_with_ties, R2_with_ties, R3_with_ties, R4_with_ties,
                 R5_with_ties, R6_with_ties)
    
    
    names(results) <- c("R1", "R2", "R3", "R4", "R5", "R6", "T1", "T2", "T3", 
                        "T4", "T5", "T6", "T7","R1_with_ties", 
                        "R2_with_ties", "R3_with_ties", "R4_with_ties",
                        "R5_with_ties", "R6_with_ties")
    return(results)
  }
  
  calcu_rates <- function(no_sample) {
    
    rates <- mapply(get_diff_rates_ranking, 
                    no_sample, event_set[, "a"], event_set[, "b"], event_set[, "aandb"])
    
    return(rates)
  } 
  
  # Define sample sizes for the simulations
  x <- c(1:20, 25, 30, 35, 40, 45, 50)
  
  events_results  <- lapply(x, calcu_rates)
  
  results_list <- list()
  
  # Loop through each sample size
  for (i in c(1:26)) {
    # Transform and calculate the required means and sums
    results_list[[i]] <- t(events_results[[i]]) %>%
      as.data.frame() %>%
      mutate(
        mean_R1 = mean(R1),
        mean_R2 = mean(R2),
        mean_R3 = mean(R3),
        mean_R4 = mean(R4),
        mean_R5 = mean(R5),
        mean_R6 = mean(R6),
        mean_T1 = mean(T1),
        mean_T2 = mean(T2),
        mean_T3 = mean(T3),
        mean_T4 = mean(T4),
        mean_T5 = mean(T5),
        mean_T6 = mean(T6),
        mean_T7 = mean(T7),
        mean_R1_with_ties = mean(R1_with_ties),
        mean_R2_with_ties = mean(R2_with_ties),
        mean_R3_with_ties = mean(R3_with_ties),
        mean_R4_with_ties = mean(R4_with_ties),
        mean_R5_with_ties = mean(R5_with_ties),
        mean_R6_with_ties = mean(R6_with_ties),
        sum_check1 = mean_R1 + mean_R2 + mean_R3 + mean_R4 + mean_R5 + mean_R6 +
          mean_T1 + mean_T2 + mean_T3 + mean_T4 + mean_T5 + mean_T6 + mean_T7,
        sum_check2 = mean_R1_with_ties + mean_R2_with_ties + mean_R3_with_ties +
          mean_R4_with_ties + mean_R5_with_ties + mean_R6_with_ties
      ) %>% unique()
  }
  
  # Combine results for different sample sizes
  results_combined <- do.call(rbind, results_list)
  
  results_different_sample_size <- results_combined %>%
    select(mean_R1:sum_check2) %>%
    unique() %>%
    mutate(sample_size_in_simulaton = x)
  
  # Save results
  write.csv(results_different_sample_size, paste0(condition_name, "_results.csv"))
  write.csv(results_combined, paste0(condition_name, "_results_full.csv"))
  
  cat(paste0("Results for ", condition_name, " saved successfully!\n"))
}

