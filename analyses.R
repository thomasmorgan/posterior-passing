do_analyses <- function() {
  print(">>>>>>>> Doing analyses")
  
  if (do_anova == TRUE) {
    print(">>>>>>>>>>>> Doing anovas")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("anova")
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      # do the anova
      model <- this_data_set$response ~ this_data_set$sex * this_data_set$condition
      this_anova <- aov(model)
      
      # save the results of the anova
      b_base_med <<- c(b_base_med, this_anova[[1]][[1]])
      b_sex_med <<- c(b_sex_med, this_anova[[1]][[2]])
      b_cond_med <<- c(b_cond_med, this_anova[[1]][[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, this_anova[[1]][[4]])
      
      ci <- confint(this_anova)
      b_base_lower <<- c(b_base_lower, ci[1])
      b_sex_lower <<- c(b_sex_lower, ci[2])
      b_cond_lower <<- c(b_cond_lower, ci[3])
      b_sex_cond_lower <<- c(b_sex_cond_lower, ci[4])
      
      b_base_upper <<- c(b_base_upper, ci[5])
      b_sex_upper <<- c(b_sex_upper, ci[6])
      b_cond_upper <<- c(b_cond_upper, ci[7])
      b_sex_cond_upper <<- c(b_sex_cond_upper, ci[8])
      
      p_vals <<- drop1(this_anova,~.,test="F")
      b_sex_p_value <<- c(b_sex_p_value, p_vals[[6]][2])
      b_cond_p_value <<- c(b_cond_p_value, p_vals[[6]][3])
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, p_vals[[6]][4])
    }
  }
  
  if (do_glmm == TRUE) {
    print(">>>>>>>>>>>> Doing glmms")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("glmm")
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      
      # do the glmm
      glmm <- glmer(cbind(response*n_trials_per_participant,  (1-response)*n_trials_per_participant) ~ sex + condition + sex*condition + (1|participant_id), family = binomial, data=this_data_set)
      ci <- confint(glmm, method="Wald", parm=c("(Intercept)", "sex", "condition", "sex:condition"))
      fe <- fixef(glmm)
      
      # save the results of the glmm
      b_base_med <<- c(b_base_med, fe[[1]])
      b_sex_med <<- c(b_sex_med, fe[[2]])
      b_cond_med <<- c(b_cond_med, fe[[3]])
      b_sex_cond_med <<- c(b_sex_cond_med, fe[[4]])
      
      b_base_lower <<- c(b_base_lower, ci[1])
      b_sex_lower <<- c(b_sex_lower, ci[2])
      b_cond_lower <<- c(b_cond_lower, ci[3])
      b_sex_cond_lower <<- c(b_sex_cond_lower, ci[4])
      
      b_base_upper <<- c(b_base_upper, ci[5])
      b_sex_upper <<- c(b_sex_upper, ci[6])
      b_cond_upper <<- c(b_cond_upper, ci[7])
      b_sex_cond_upper <<- c(b_sex_cond_upper, ci[8])
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
    } # end of for each experiment loop
  } # end of do_glmm
  
  if (do_bglmm == TRUE) {
    print(">>>>>>>>>>>> Doing b-glmms")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("bglmm")
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      successes <- this_data_set$response*n_trials_per_participant
      np <- rep(n_participants_per_experiment, n_participants_per_experiment)
      nt <- rep(n_trials_per_participant, n_participants_per_experiment)
      this_data_set <- cbind(this_data_set, successes, np, nt)
      
      # do the bglmm
      model <- jags.model("bglmm.R", data=this_data_set, n.chains=3, quiet=TRUE)
      nodes <- c("beta", "tau_participant")
      samples <- coda.samples(model,nodes,2000,1)
      
      # save the results of the glmm
      b_base_samples <- c(samples[,1][[1]], samples[,1][[2]], samples[,1][[3]])
      b_sex_samples <- c(samples[,2][[1]], samples[,2][[2]], samples[,2][[3]])
      b_cond_samples <- c(samples[,3][[1]], samples[,3][[2]], samples[,3][[3]])
      b_sex_cond_samples <- c(samples[,4][[1]], samples[,4][[2]], samples[,4][[3]])
      
      b_base_med <<- c(b_base_med, median(b_base_samples))
      b_sex_med <<- c(b_sex_med, median(b_sex_samples))
      b_cond_med <<- c(b_cond_med, median(b_cond_samples))
      b_sex_cond_med <<- c(b_sex_cond_med, median(b_sex_cond_samples))
      
      b_base_lower <<- c(b_base_lower, quantile(b_base_samples, probs=c(0.025)))
      b_sex_lower <<- c(b_sex_lower, quantile(b_sex_samples, probs=c(0.025)))
      b_cond_lower <<- c(b_cond_lower, quantile(b_cond_samples, probs=c(0.025)))
      b_sex_cond_lower <<- c(b_sex_cond_lower, quantile(b_sex_cond_samples, probs=c(0.025)))
      
      b_base_upper <<- c(b_base_upper, quantile(b_base_samples, probs=c(0.975)))
      b_sex_upper <<- c(b_sex_upper, quantile(b_sex_samples, probs=c(0.975)))
      b_cond_upper <<- c(b_cond_upper, quantile(b_cond_samples, probs=c(0.975)))
      b_sex_cond_upper <<- c(b_sex_cond_upper, quantile(b_sex_cond_samples, probs=c(0.975)))
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
    } # end of for each experiment loop
  }# end of do bglmm
  
  if (do_pp == TRUE) {
    print(">>>>>>>>>>>> Doing posterior passing")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("pp")
    
    #set initial prior for beta[4]
    pp_u <<- rep(0, n_participants_per_experiment)
    pp_prec <<- rep(0.01, n_participants_per_experiment)
    
    for (experiment in 1:n_experiments_per_repeat) {
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment,]
      successes <- this_data_set$response*n_trials_per_participant
      np <- rep(n_participants_per_experiment, n_participants_per_experiment)
      nt <- rep(n_trials_per_participant, n_participants_per_experiment)
      this_data_set <- cbind(this_data_set, successes, np, nt, pp_u, pp_prec)
      
      # do the pp analysis
      model <- jags.model("pp.R", data=this_data_set, n.chains=3, quiet=TRUE)
      nodes <- c("beta", "tau_participant")
      samples <- coda.samples(model,nodes,2000,1)
      
      # save the results of the glmm
      b_base_samples <- c(samples[,1][[1]], samples[,1][[2]], samples[,1][[3]])
      b_sex_samples <- c(samples[,2][[1]], samples[,2][[2]], samples[,2][[3]])
      b_cond_samples <- c(samples[,3][[1]], samples[,3][[2]], samples[,3][[3]])
      b_sex_cond_samples <- c(samples[,4][[1]], samples[,4][[2]], samples[,4][[3]])
      
      b_base_med <<- c(b_base_med, median(b_base_samples))
      b_sex_med <<- c(b_sex_med, median(b_sex_samples))
      b_cond_med <<- c(b_cond_med, median(b_cond_samples))
      b_sex_cond_med <<- c(b_sex_cond_med, median(b_sex_cond_samples))
      
      b_base_lower <<- c(b_base_lower, quantile(b_base_samples, probs=c(0.025)))
      b_sex_lower <<- c(b_sex_lower, quantile(b_sex_samples, probs=c(0.025)))
      b_cond_lower <<- c(b_cond_lower, quantile(b_cond_samples, probs=c(0.025)))
      b_sex_cond_lower <<- c(b_sex_cond_lower, quantile(b_sex_cond_samples, probs=c(0.025)))
      
      b_base_upper <<- c(b_base_upper, quantile(b_base_samples, probs=c(0.975)))
      b_sex_upper <<- c(b_sex_upper, quantile(b_sex_samples, probs=c(0.975)))
      b_cond_upper <<- c(b_cond_upper, quantile(b_cond_samples, probs=c(0.975)))
      b_sex_cond_upper <<- c(b_sex_cond_upper, quantile(b_sex_cond_samples, probs=c(0.975)))
      
      b_sex_p_value <<- c(b_sex_p_value, NaN)
      b_cond_p_value <<- c(b_cond_p_value, NaN)
      b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
      
      # update the priors for the next run
      pp_u[1] <<- median(b_sex_cond_samples)
      pp_prec[1] <<- 1/var(b_sex_cond_samples)
    } # end of for each experiment loop
  }# end of do pp
  
  if (do_mega_bglmm == TRUE) {
    print(">>>>>>>>>>>> Doing mega bglmm")
    # prep columns
    # this function is in util.R
    save_analysis_results_1("mega_bglmm")
    
    this_data_set <- data_sets
    this_data_set$participant_id <- c(1:nrow(this_data_set))
    successes <- this_data_set$response*n_trials_per_participant
    np <- rep(n_participants_per_experiment*n_experiments_per_repeat, n_participants_per_experiment*n_experiments_per_repeat)
    nt <- rep(n_trials_per_participant, n_participants_per_experiment*n_experiments_per_repeat)
    this_data_set <- cbind(this_data_set, successes, np, nt)
      
    # do the mega_bglmm
    model <- jags.model("bglmm.R", data=this_data_set, n.chains=3, quiet=TRUE)
    nodes <- c("beta", "tau_participant")
    samples <- coda.samples(model,nodes,2000,1)
      
    # save the results of the mega glmm
    b_base_samples <- c(samples[,1][[1]], samples[,1][[2]], samples[,1][[3]])
    b_sex_samples <- c(samples[,2][[1]], samples[,2][[2]], samples[,2][[3]])
    b_cond_samples <- c(samples[,3][[1]], samples[,3][[2]], samples[,3][[3]])
    b_sex_cond_samples <- c(samples[,4][[1]], samples[,4][[2]], samples[,4][[3]])
    
    b_base_med <<- c(b_base_med, median(b_base_samples))
    b_sex_med <<- c(b_sex_med, median(b_sex_samples))
    b_cond_med <<- c(b_cond_med, median(b_cond_samples))
    b_sex_cond_med <<- c(b_sex_cond_med, median(b_sex_cond_samples))
    
    b_base_lower <<- c(b_base_lower, quantile(b_base_samples, probs=c(0.025)))
    b_sex_lower <<- c(b_sex_lower, quantile(b_sex_samples, probs=c(0.025)))
    b_cond_lower <<- c(b_cond_lower, quantile(b_cond_samples, probs=c(0.025)))
    b_sex_cond_lower <<- c(b_sex_cond_lower, quantile(b_sex_cond_samples, probs=c(0.025)))
    
    b_base_upper <<- c(b_base_upper, quantile(b_base_samples, probs=c(0.975)))
    b_sex_upper <<- c(b_sex_upper, quantile(b_sex_samples, probs=c(0.975)))
    b_cond_upper <<- c(b_cond_upper, quantile(b_cond_samples, probs=c(0.975)))
    b_sex_cond_upper <<- c(b_sex_cond_upper, quantile(b_sex_cond_samples, probs=c(0.975)))
    
    b_sex_p_value <<- c(b_sex_p_value, NaN)
    b_cond_p_value <<- c(b_cond_p_value, NaN)
    b_sex_cond_p_value <<- c(b_sex_cond_p_value, NaN)
  }# end of do mega bglmm
}