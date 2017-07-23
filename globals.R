####WARNING!!! don't touch anything here - read only
####for manipulating these values please go to internal global variables in app.R

#------1. External global data and variables---------

setwd(getwd())
close_wig       <- read.csv("Import data/wig20_d.csv")[,'Zamkniecie'] ## data from 1.01.10 to 31.12.11 - 253 trading days
close_kghm      <- read.csv("Import data/kgh_d.csv")[,'Zamkniecie']
wig2011         <- read.csv('Import data/wig20_d2011.csv')[,'Zamkniecie']
n_days_0        <- length(close_wig)
S0_wig_0        <- close_wig[n_days_0]
S0_kghm_0       <- close_kghm[n_days_0]
wig2011         <- c(S0_wig_0,read.csv('Import data/wig20_d2011.csv')[,'Zamkniecie'])[1:184]
WIBOR1Y         <- 0.0452
WIBID1Y         <- 0.0432
r_0             <- log(1 + mean(c(WIBOR1Y, WIBID1Y))) / n_days_0 ## daily!!!
third_fri_sep   <- floor((8 + 3/4)/12 * n_days_0)  ##the order number of trading day on the 3rd Friday of September 
third_fri_dec   <- floor((11+3/4)/12 * n_days_0)

#------2. Parameters estimation------------------------

logreturns_wig  <- log(close_wig[2:n_days_0] / close_wig[1:(n_days_0 - 1)])
logreturns_kghm <- log(close_kghm[2:n_days_0] / close_kghm[1:(n_days_0 - 1)])
mu_wig_0        <- mean(logreturns_wig)
mu_kghm_0       <- mean(logreturns_kghm)
sigma_wig_0     <- sd(logreturns_wig)
vol_wig         <- sigma_wig_0*sqrt(n_days_0)
sigma_kghm_0    <- sd(logreturns_kghm)
sigma_max       <- sqrt((n_days_0 - 1) * sigma_wig_0^2/qchisq(0.99, n_days_0-1, lower.tail=FALSE))
vol_max         <- sigma_max*sqrt(n_days_0)
rho_0           <- cor(logreturns_kghm, logreturns_wig) ##UWAGA!! ta korelacja jest między zmiennymi losowymi o rozkładzie normalnym 
gamma_0         <- cov(logreturns_kghm, logreturns_wig)