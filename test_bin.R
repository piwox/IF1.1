n_days = third_fri_dec
strike_g =2300
freq_g = 1
n_sim_g = 100
paths_wig <- SimPaths()[[1]]
x=Payoff(c_p='p', S = paths_wig[,2])
plot(paths_wig[,2],type='l')
abline(h=2300)
lines((x>0)*paths_wig[,2], col=2,lwd=2)
plot(BSPrice(c_p='bp',S_t = paths_wig[,2],t=third_fri_dec:1)+BSPrice(c_p='bc',S_t = paths_wig[,2],t=third_fri_dec:1),type='l')

plot(Delta(S_t = 1000:4000,c_p='bc'),type='l')
lines(Gamma(S_t= 1000:4000, c_p='c'),type='l')
plot(Gamma(S_t= 1000:4000, c_p='c'),type='l')
plot((Gamma(S_t= 1000:4000, c_p='p')/Gamma(S_t= 1000:4000, c_p='bp'))[0:1000],type='l')
hist(GammaMatrix()[24,], freq = FALSE)
plot(GammaMatrix()[,1][1:252])
plot(GammaNeutralMatrix()[,1][1:(24*183)], type = 'l',ylim=c(-0.002,0.01))
#DeltaMatrix(strike=2300, c_p = paste0('b',c_p_g))[,1]
#DeltaMatrix(strike=2300,c_p=c_p_g)[,1]
lines(PortfolioValueMatrix()[,1])
n_days = third_fri_sep
freq_g = 1
strike_g = 3000
paths_wig <- SimPaths()[[1]]
paths_wig[,1]<- wig2011
plot(GammaNeutralMatrix()[,1][1:58],type='l', ylim=c(-100,100))
for( i in 1:25)
   lines(GammaNeutralMatrix()[,i])
lines(PortfolioValueMatrix()[,1])


##benchmark for the most efficient strike
c_p_g = 'c'
g = Gamma(S_t = S0_wig, t=246)
d = Delta(S_t = S0_wig, t=246)
v = BSPrice(t=246)
gbv = Gamma(S_t = S0_wig, c_p = 'bc', strike=3500, t=246 )
dbv = Delta(S_t = S0_wig, c_p = 'bc', strike=2951:5000, t=246 )
vbv = BSPrice(S_t = S0_wig, c_p = 'bc', strike=2951:5000, t=246 )

plot(gbv, type='l')
plot(g/gbv)
plot(d/dbv)
plot(vbv)
plot(g/gbv * vbv)
plot(g/gbv * dbv)
plot()
(g/gbv)[550]
(g/gbv*dbv)[550]
(g/gbv*vbv)[550]

## propoer hedge

strike_g = 2300
c_p_g = 'c'
n_days = third_fri_dec
step_g = 1
freq_g = 1
n_sim_g = 1000
spread = 0
spread1=900
spread2=1000
paths_wig <- SimPaths()[[1]]
GammaNeutralMatrix2()[,10]
plot(GammaNeutralMatrix2()[,2683], type = 'l')
   for (i in 1:100)
lines(GammaNeutralMatrix2()[,i],type = 'l', col =i%%2+1)
plot(rowMeans(GammaNeutralMatrix2()),type ='l', col=2)
lines(rowMeans(PortfolioValueMatrix()),type = 'l')
rowMeans(GammaNeutralMatrix2())
hist(GammaNeutralMatrix2()[240,],breaks = 1000, xlim=c(-100,100))
median(GammaNeutralMatrix2()[240,])
rowMeans(PortfolioValueMatrix())
plot(rowMeans(PriceMatrix(strike = strike_g + spread, c_p='c')), type = 'l')
best_gamma <- which(g/gbv==-min(-g/gbv))
BSPrice(strike=3001:5000, t=247, c_p = 'bc' )
###idea of gamma hedging with two different binary options

