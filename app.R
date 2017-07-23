#---------General setup---------------------------------------

require('MASS')
require('ggplot2')
require('ggthemes')
require('reshape2')
require('shiny')
require('plotly')

#---------Load external global variables--------------

source('globals.R')

#---------Load kernel---------------------------------

source('kernel.R')

#---------Set internal global variables---------------

#defaults:
# freq_g            <- 1      ##only natural values are valid
# step_g            <- 1      ##only natural values are valid
# strike_g          <- 2500
# n_sim_g           <- 5000   ##note that it increases the complexity together with freq
# c_p_g             <- 'c'    ##'c' is for call and 'p' is for put
# r                 <- r_0/freq_g
# n_days            <- n_days_0
# S0_wig            <- S0_wig_0
# S0_kghm           <- S0_kghm_0 
# mu_wig            <- mu_wig_0
# mu_kghm           <- mu_kghm_0 
# sigma_wig         <- sigma_wig_0
# sigma_kghm        <- sigma_kghm_0
# rho               <- rho_0
# gamma             <- gamma_0
# #set.seed(1000)   ##on repetitivness, if needless - comment
# paths_wig         <- SimPaths()[[1]]

freq_g            <- 1   ##only natural values are valid
step_g            <- 1   ##only natural values are valid
strike_g          <- 2500
n_sim_g           <- 5000  ##note that it increases the complexity together with freq
c_p_g             <- 'c'   ##1 is for call and 0 is for put
r                 <- r_0/freq_g
n_days            <- n_days_0  
S0_wig            <- S0_wig_0
S0_kghm           <- S0_kghm_0 
mu_wig            <- mu_wig_0
mu_kghm           <- mu_kghm_0 
sigma_wig         <- sigma_wig_0
sigma_kghm        <- sigma_kghm_0
rho               <- rho_0
gamma             <- gamma_0
cost_g            <- 0.004
#set.seed(1000)            ##on repetitivness, if needless - comment
paths_wig         <- SimPaths()[[1]]

#---------Load additional facilities------------------

source('facilities.R')

#---------Run test batch (comment to omit)------------

# source('test_sim.R')
# source('test_bs.R')
# source('test_bin.R')
warnings()
