model{
  for (i in 1:np[1]) {
    successes[i] ~ dbin(p[i], nt[i])
    logit(p[i]) <- x[i]
    x[i] <- beta[1] + beta[2]*sex[i] + beta[3]*condition[i] + beta[4]*sex[i]*condition[i] + participant_effect[participant_id[i]]
  }
  
  for (i in 1:3) {
    beta[i] ~ dnorm(0.0, 0.01)
  }
  beta[4] ~ dnorm(pp_u[1], pp_prec[1])
  for (i in 1:np[1]) {
    participant_effect[i] ~ dnorm(0.0, tau_participant)
  }
  tau_participant ~ dgamma(0.01, 0.01)
  
}

