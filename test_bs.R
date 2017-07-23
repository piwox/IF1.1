#GENERAL STATUS: So far so good

#---------1. Constructing needed objects---------

#STATUS: so far so good

paths_wig        <- SimPaths()[[1]]
price_c2500      <- PriceMatrix()
delta_c2500_s1   <- DeltaMatrix()
delta_c2500_s7   <- DeltaMatrix(step = 5 * freq_g)    ##weekly
delta_c2500_s30  <- DeltaMatrix(step = 21 * freq_g)   ##monthly
value_c2500_s1   <- PortfolioValueMatrix()
value_c2500_s7   <- PortfolioValueMatrix(step = 5  * freq_g)
value_c2500_s30  <- PortfolioValueMatrix(step = 21 * freq_g)

#---------2. Delta neutral properties check-----------

#STATUS: so far so good

print(value_c2500_s1[1:3,1:3]) ##these three below should be coherent and converge to certain limits
print(delta_c2500_s1[1:3,1:3]) ##
print(price_c2500[1:3,1:3])    ##

hist((value_c2500_s1 [nrow(value_c2500_s1),]),   breaks = 50, freq = FALSE)
hist((value_c2500_s7 [nrow(value_c2500_s7),]),   breaks = 50, freq = FALSE)
hist((value_c2500_s30[nrow(value_c2500_s30),]),  breaks = 50, freq = FALSE)

print(mean(value_c2500_s1 [nrow(value_c2500_s1),]))  ##should converge to 0 or a small value close to 0 (due to 
print(mean(value_c2500_s7 [nrow(value_c2500_s1),]))  ##the second factor of convergence which is step)
print(mean(value_c2500_s30[nrow(value_c2500_s1),]))  ##
print(quantile(value_c2500_s1[nrow(value_c2500_s1),], probs = c(0, 0.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99, 1)))

plot(value_c2500_s1[,1],   type = 'l', ylim = c(-200, 200))
lines(value_c2500_s7[,1],  type = 'l')
lines(value_c2500_s30[,1], type = 'l')
for(i in 1:min(1000,n_sim_g)) 
   lines(value_c2500_s1[,i], col = i%%2+1)
#lines(wig2011_hedging,col=3,lwd=2)
plot(apply(value_c2500_s30, 1, mean), type = 'l')
lines(apply(value_c2500_s7, 1, mean), type = 'l')
lines(apply(value_c2500_s1, 1, mean), type = 'l')

#---------3. Convergence to payoff check-----------

#STATUS: ok

ppp=1800:3000
BSPrice(S_t = S0_wig, t = 252)
for(i in seq(253, 0, by = -1)) ##CRUCIAL should confirm the convergence of BS price to payoff with time
   lines(BSPrice(S_t = ppp, t = i), col = i%%2+1)
plot(Delta(strike = 2500, S_t = ppp, t = 0), type = 'l')
#Delta(1,mu_wig,sigma_wig,ppp,2300,1)
for(i in seq(253 * freq_g, 0, by = -1)) ##CRUCIAL should confirm the convergence of BS delta to indicator with time
   lines(Delta(strike = 2500, S_t = ppp, t = i ),col=i%%2+1)

