# imports
library(lme4)
library(rjags)
library(gplots)
source("util.R")
source("simulation.R")
source("analyses.R")
source("contours.R")

###            ###
### Parameters ###
###            ###

### True values
# these set the true values of the population
# putting multiple values in will cause simulations to run
# with different true values.
# The first four give the mean value of the four parameters
# that determine participant behavior. The next four give
# the variance in these parameters.
# Warning! var_sexs, var_conds and var_sex_conds do not currently
# do anything!
b_bases <- c(0)
b_sexs <- c(0)
b_conds <- c(0)
b_sex_conds <- c(0, 0.5, 1, 1.5, 2)
var_bases <- c(0, 0.25, 0.5, 0.75, 1)
var_sexs <- c(0)
var_conds <- c(0)
var_sex_conds <- c(0)

### Size parameters
# These determine the size of the simulations for every set of
# parameter values given in the "True values" setction above.
# Warning! n_participants_per_experiment and n_people need to 
# be divisible by 4!
n_repeats <- 20
n_experiments_per_repeat <- 60
n_participants_per_experiment <- 80
n_trials_per_participant <- 25
n_people <- 1000000

total_simulations <- length(b_bases) * length(b_sexs) * length(b_conds) * length(b_sex_conds) * length(var_bases) * length(var_sexs) * length(var_conds) * length(var_sex_conds) * n_repeats
current_simulation <- 1

### Analysis parameters
# These allow you to choose which analyses do you want
# Warning! thhe bglmm and pp analyses are slow!
do_anova <- TRUE
do_glmm <- TRUE
do_bglmm <- TRUE
do_pp <- TRUE
do_mega_bglmm <- TRUE

### Posterior-passing parameters
# These give you various options wrt posterior passing
# log only the final experiment from each chain:
pp_final_expt_only <- TRUE

### plotting parameters
# basically tell it what plots you want
plot_sex_cond <- TRUE
plot_var_base <- TRUE
disp <- 0.1

### Vectors to store meta-data
# This function is in util.R
# It creates a number of vectors that will be
# filled with data. Each set of simulations for a set of parameters
# creates a results table. At the end of each set of parameter values,
# the meta_results table is filled in with a single row that gives data
# on the results across those simulations.
prepare_meta_vectors()

###                        ###
### SIMULATIONS START HERE ###
###                        ###

### For loops to iterate through parameter values
for (i in 1:length(b_bases)) {
  for (j in 1:length(b_sexs)) {
    for (k in 1:length(b_conds)) {
      for (l in 1:length(b_sex_conds)) {
        for (m in 1:length(var_bases)) {
          for (n in 1:length(var_sexs)) {
            for (o in 1:length(var_conds)) {
              for (p in 1:length(var_sex_conds)) {

### Set up values for the simulation
# this function is in util.R
# also save the values to the meta vectors
prepare_for_simulation()

### Vectors to store data
# this function is in util.R
# These store the results of simulations within each set
# of parameter values
prepare_data_vectors()

###
### Each repeat starts here
###

for (rep in 1:n_repeats) {
  print(paste(">>>> Repeat", rep, "of", n_repeats, sep=" "))
  print(paste(">>>> Simulation", current_simulation, "of", total_simulations, sep=" "))
  current_simulation <- current_simulation + 1

  ###
  ### Create the population ###
  ###
  # This function is in simulation.R
  # The population is simply a data table with 3 columns:
  # id, sex, d_base
  # It is created with the "doppelganger quadrangle" method
  # so there are equal number of men and women and the true
  # mean and true variance of each sub population is the same
  population <- create_population()
  
  ###
  ### Create the datsets for each experiment ###
  ###
  # This function is in simulation.R
  # The datasets are stored in a single table with 5 columns:
  # data_set - the id of the experiment
  # participant_id - the id of the participant within that experiment
  # sex - the sex of the participant
  # condition - the condition the participant was in
  # response - the mean response of that participant, ranging from 0 to 1
  data_sets <- create_datasets()
  
  ###
  ### run analyses ###
  ###
  # this function is in analyses.R
  # Now that we have all the data_sets we perform the desired analyses over them.
  # It is in these methods that we start to create the results table. Each entry
  # in this table corresponds to a single analysis on a single data_set.
  do_analyses()
} # end of for each repeat loop

###
### Create the Meta-results table
###
# this function is in util.R
# now we have all our results so we parse them as a single table and collapse
# each repeat simulation (i.e. each run of experiments) into a single entry
save_results_meta()

              }
            }
          }
        }
      }
    }
  }
} # end of parameter value for loops

meta_results <- compile_meta_results()
#meta_results <- read.delim("meta_results_18_09_16.txt")

tidy_workspace()


###                 ###
### PLOT THE GRAPHS ###
###                 ###

detach(meta_results)
attach(meta_results)

plot_contours()
  
  
dev.off
