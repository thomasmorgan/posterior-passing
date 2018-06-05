##runnings jags binom??
library(rjags)
library(coda)
library(R2jags)

# Read in the data and construct the jags objects
#source('../Data/loadData_latest.R')

falsePos <- FalsePos$falsePos
trialNo <- FalsePos$trialNo
analysisType <- FalsePos$analysisType
id <- FalsePos$id
nObs <- length(id)

jags.data = list('falsePos','trialNo','analysisType')
jags.params = ('b_aT')
jags.init = function(){
  list('b_aT' =rnorm(1)) 
}


n.chains = 3
n.cluster = n.chains

  
my.model <- jags.parallel(data=jags.data,inits=jags.init,
                            parameters.to.save = jags.params,
                            model.file='jagsBinom.jags',
                            n.chains = n.chains,n.iter=610,n.burnin=10,
                            n.thin=20,n.cluster=n.cluster)
  
  # This function returns coda compatible objects, for use with coda functions
  my.samples <- as.mcmc(my.model)
  
  # Lets focus on the relevant parameters (ignore participant and question pars)
  relPars = c('b_aT')
  
  
  # Save summary statistics
  my.summary = summary(my.samples[,relPars])
  sink('jagsBinom.txt')
  my.summary
  sink()
  
  # Save samples
  my.samples.jagsBinom = my.samples
  save('my.samples.jagsBinom',file='jagsBinom.dump')
  


