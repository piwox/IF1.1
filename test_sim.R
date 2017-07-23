#GENERAL STATUS: so far so good (not applicable for freq adjustment)

#------------1. Simulations, convergence and payoffs (with beta plots)------------

#STATUS: so far so good (not applicable for freq adjustment)

#set.seed(1000) ## on repetitivness, if needless - comment
paths           <- SimPaths(1000, 2, 1)
paths_wig       <- paths[[1]]
paths_kghm      <- paths[[2]]
plot(paths_wig[,1], type = 'l')
paths_wig_mean  <- apply(paths_wig, 1, mean); paths_wig_mean[length(paths_wig_mean)] 
paths_wig_sd    <- mean(apply(log(paths_wig[2:253,]/paths_wig[1:252,]),2,sd))
cor_v           <- rep(0,253)
for (i in 1:253)
   cor_v[i]     <-(cor(paths_wig[,i],paths_kghm[,i]))
mean(cor_v); which(cor_v==max(cor_v)) ##mean(cor_v) should be near 0.7
plot(paths_wig_mean, type='l', ylim=c(1800, 3200)) ##should by sligthly exponent overally and jagged for low number of paths 
lines(close_wig)
lines(paths_wig[,1], col = 'blue')
log(paths_wig_mean[253]/paths_wig_mean[1])/252; log(close_wig[253]/close_wig[1])/252 ##these two shouldbe about the same

#------1.1 Quantiles------------------------

#STATUS: so far so good

paths_wig_2010  <- paths_wig#/S0_wig * close_wig[1] ##rescaling for previous year price
paths_wig_q     <- apply(paths_wig_2010, 1, quantile, 
                    probs = c(0 ,0.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99, 1))
plot(paths_wig_q[1,], type='l', ylim=c(1200, 4500),lwd=1.5)
for (i in 2:9)
  lines(paths_wig_q[i,], type='l', col = (i+(i>5)*(10-2*i)),lwd = 1.5)
lines(wig2011, type='l',col=1, lwd=3)

#------1.2. Histograms - returns--------------

#STATUS: so far so good

#WIG20
returns_sym_wig            <- SimPaths(2)[[1]][,1]
returns_sym_wig            <- returns_sym_wig[2:n_days] / returns_sym_wig[1:(n_days - 1)]
returns_wig                <- close_wig[2:n_days] / close_wig[1:(n_days - 1)]

hist(returns_sym_wig, xlim <- c(0.94, 1.06), ylim = c(0, 40), freq = FALSE, breaks = 14)
curve(dlnorm(x, mu_wig, sigma_wig), add = TRUE)

hist(returns_wig, xlim = c(0.94, 1.06), freq = FALSE, breaks = 14)
curve(dlnorm(x, mu_wig, sigma_wig), add = TRUE)

#KGHM
returns_sym_kghm  <- SimPaths(2,,1)[[2]][,1]
returns_sym_kghm  <- returns_sym_kghm[2:n_days] / returns_sym_kghm[1:(n_days-1)]
returns_kghm      <- close_kghm[2:n_days] / close_kghm[1:(n_days - 1)]

hist(returns_sym_kghm, xlim = c(0.92, 1.08), ylim=c(0, 20), freq = FALSE, breaks = 14)
curve(dlnorm(x, mu_kghm, sigma_kghm), add = TRUE)

hist(returns_kghm , xlim = c(0.92, 1.08), freq = FALSE, breaks = 14)
curve(dlnorm(x, mu_kghm, sigma_kghm), add = TRUE)

#------1.3. Histograms - payoffs--------------

#STATUS: so far so good

sep_wig        <- SimGivenDayValues(third_fri_sep, 1000000)
mean(sep_wig) ##should be around 3014
hist(sep_wig, freq = FALSE)
payoff_c2500   <- Payoff(2500, 'c', paths_wig[253,])
payoff_c1600   <- Payoff(1600, 'c', sep_wig)
payoff_c3200   <- Payoff(3200, 'c', sep_wig)
payoff_p2200   <- Payoff(2200, 'p', sep_wig)
payoff_p1600   <- Payoff(1600, 'p', sep_wig)
payoff_p3200   <- Payoff(3200, 'p', sep_wig)
hist(payoff_c1600, freq = FALSE)
hist(payoff_c2500, freq = FALSE)
hist(payoff_c3200, freq = FALSE)
hist(payoff_p1600, freq = FALSE)
hist(payoff_p2200, freq = FALSE)
hist(payoff_p3200, freq = FALSE)

#-----------1.4. Correlated trajectories----------

#STATUS: so far so good

plot(paths_wig[,101]/S0_wig, type = 'l', col = 2, lwd = 2, ylim=c(0.95, 1.7))
lines(paths_kghm[,101]/S0_kghm, type = 'l', col = 1, lwd = 2)
