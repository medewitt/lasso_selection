library(tidyverse)
library(dplyr)
library(glmnet)
theme_set(theme_minimal())
run_simulation <- function(n= 100, 
                           p = 500, 
                           rho = 0, 
                           true_beta= -.2, 
                           error= 1,
                           nreplicants = 100,
                           niter = 500){
  
  out_rep <- data.frame()
  for(j in 1:nreplicants){
    Sigma_star <- matrix(rep(rho, p^2), 
                         nrow = p)
    
    diag(Sigma_star) <- 1
    
    X <- MASS::mvrnorm(n = n, mu = rep(0, p),
                       Sigma = Sigma_star)
    
    
    X[,1] <- rep(1:10, times = n/10) 
    
    pop <- sample(500:2000, n, replace = T)
    
    beta <- c(true_beta, .2, .1, -.2, 1, .8, rep(0, p-6))
    
    eta <- rnorm(n, X %*% beta + log(pop), error)
    
    eta_star <- exp(eta)
    
    y <- rpois(n = n, lambda = eta_star)
    
    # Run Lasso to Select the Variables of Interest
    log_pop <- log(pop)
    
    log_y <- log(y+1)
    
    new_X <- cbind(X, log_pop)

    collect <- list()
    
    for(i in 1:niter){
      fit_lasso <- cv.glmnet(x = new_X,
                             y = log_y,
                             nfolds = 5)
      
      bestlam <- fit_lasso$lambda.min
      
      
      
      lasso_coef <- predict(fit_lasso, type = "coefficients", s = bestlam)
      
      
      keepers <- lasso_coef %>%
        as.matrix() %>%
        as.data.frame() %>%
        mutate(var_name = rownames(.)) %>%
        mutate(keep = ifelse(`1` != 0, TRUE, FALSE)) %>%
        mutate(keep = ifelse(var_name == "X.Intercept.", FALSE, keep)) %>%
        mutate(keep = ifelse(var_name == "log_pop", FALSE, keep)) %>%
        mutate(keep = ifelse(var_name == "treatment" |
                               var_name == "population", TRUE, keep)) %>%
        filter(!var_name %in% c("log_pop", "X.Intercept."))
      
      true_contained <- keepers %>% 
        filter(var_name %in% c("X", "X.1", "X.2", "X.3", "X.4", "X.5")) %>% 
        summarise(keep = mean(keep)) %>% 
        pull(keep)
      
      num_variables <- sum(keepers$keep)
      
      reduced_X <- X[, pull(keepers, keep)]
      
      fit_glm <-
        glm(y ~ reduced_X + offset(log(pop)), family = "poisson")
      
      collect[[i]] <- data.frame(
        n = n,
        internal_number = i,
        rho = rho,
        num_pred = p,
        predictors_retained = ncol(reduced_X),
        true_beta = true_beta,
        true_contain = true_contained,
        error = error,
        estimated_beta = coef(fit_glm)[2]
      )
    }
    step_1 <- do.call(rbind, collect)
    step_1 <- cbind(step_1, replication = j)
    out_rep <- rbind(out_rep, step_1)
  }  
  
  out_rep
  
}

full_frame <- tidyr::crossing(n = 200,
                              p = c(200, 500, 1000),
                              rho = c(0, .25),
                              true_beta = c(-.2, -.1, -0.01),
                              error = c(.1, 1))

set.seed(42)

library(furrr)
plan("multisession")


simulation_results <-future_pmap(.l = list(
  full_frame$n,
  full_frame$p,
  full_frame$rho,
  full_frame$true_beta,
  full_frame$error
), ~run_simulation(n = ..1, p = ..2, rho = ..3, true_beta = ..4, error = ..5), .id = "sim_number")


out <- bind_rows(simulation_results, .id = "model_run")

write_rds(out, "lasso_sim_results.rds")

# Individual Checks
out %>% 
  filter(model_run==4) %>% 
  ggplot(aes(as.factor(replication), estimated_beta))+
  geom_boxplot()+
  geom_hline(yintercept = max(out$true_beta))


out %>% 
  ggplot(aes(as.factor(replication), 
             estimated_beta, 
             color = as.factor(true_beta)))+
  geom_boxplot()+
  facet_wrap(~rho)

coverage_probs <- out %>% 
  group_by(model_run, replication, error, num_pred, n) %>% 
  summarise(true_beta = max(true_beta),
            q_025 = quantile(estimated_beta, probs = .025),
            q_975 = quantile(estimated_beta, probs = .975)) %>% 
  rowwise() %>% 
  mutate(captured = between(true_beta, q_025, q_975)) %>% 
  ungroup() %>% 
  group_by(model_run) %>% 
  summarise(capture_rate = mean(captured),
            true_beta = max(true_beta),
            error = max(error),
            num_pred = max(num_pred),
            n = max(n))

coverage_probs

coverage_probs %>%
  ggplot(aes(true_beta, capture_rate))+
  geom_point(size = 2)

out %>% 
  filter(error == .1, true_beta == -.1) %>% 
  ggplot(aes(as.factor(num_pred),estimated_beta))+
  geom_jitter()+
  facet_wrap(facets = ~rho)+
  geom_hline(yintercept = -.1, color = "red")
