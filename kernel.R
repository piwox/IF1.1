setwd(getwd())
#------1. Simulation----------------------

SimPaths <- function(n_sim = n_sim_g,
                     freq  = freq_g, 
                     kghm  = FALSE) ##freq means how many times a day price moves
{ 
   n_d      = n_days * freq - (freq - 1)
   m_wig    = mu_wig/freq
   m_kghm   = mu_kghm/freq
   s_wig    = sigma_wig/sqrt(freq)
   s_kghm   = sigma_kghm/sqrt(freq)
   g        = gamma/freq
   
   matrix_wig    <- matrix (0, nrow = n_d, ncol = n_sim)
   matrix_kghm   <- matrix (0, nrow = n_d, ncol = n_sim)
   mu            <- c(m_wig - s_wig^2/2, m_kghm - s_kghm^2/2)
   sigma         <- matrix (c(s_wig^2, g, g, s_kghm^2), nrow = 2, ncol =2)
   if(n_sim > 1)
   {
      for (i in 2:n_d)
      {
         temp              <- mvrnorm(n_sim, mu, sigma, 1e-16)
         matrix_wig[i,]    <- temp[,1]
         matrix_kghm[i,]   <- temp[,2]
      }
   }
   else
   {
      for (i in 2:n_d)
      {
         temp              <- mvrnorm(n_sim, mu, sigma)
         matrix_wig[i,]    <- temp[1]
         matrix_kghm[i,]   <- temp[2]
      }
   }
   matrix_wig  <- S0_wig * exp(apply(matrix_wig, 2, cumsum))
   if(kghm==FALSE)
      return(list(matrix_wig))
   else
   {  
      matrix_kghm <- S0_kghm * exp(apply(matrix_kghm, 2, cumsum))
      return (list(matrix_wig, matrix_kghm))
   }
}

#---------------------------------

SimGivenDayValues <- function(day   = third_fri_sep,
                              n_sim = n_sim_g * 10) ##simulates value of WIG20 index on a given day
{ 
   v_wig   <- rep(0, n_sim)
   mu      <- (mu_wig - sigma_wig^2/2) * day
   sigma   <- sqrt(day) * sigma_wig
   v_wig   <- S0_wig * exp(rnorm(n_sim, mu, sigma)) 
   return (v_wig)
}

#----------2. Payoff - returns payoff values for S matrix (or vector/single value)------

Payoff <- function(strike = strike_g,
                   c_p    = c_p_g,
                   S      = paths_wig) ##c_p is a logical argument, choose 'c' for call or 'p' for put
{
   if(c_p=='c' || c_p == 'p')
   return(((c_p=='c') * (S > strike) + (c_p=='p') * (S < strike)) * abs(strike - S))
   if(c_p=='bc' || c_p == 'bp')
   return((c_p=='bc') * (S > strike) + (c_p=='bp') * (S < strike))
}

#-----------3. Black-Scholes pricing-----------

d1  <- function(strike = strike_g, sigma = sigma_wig, S_t = S0_wig, t = n_days) ##where t is time to expiration date
{
   1/(sigma * sqrt(t)) * (log(S_t/strike) + (r + sigma^2/2) * t)
}
d2  <- function(strike =  strike_g, sigma = sigma_wig, S_t = S0_wig, t = n_days)
{
   d1(strike, sigma, S_t, t) - sigma * sqrt(t)
}  

BSPrice <- function(strike = strike_g, 
                    c_p    = c_p_g,       ##c_p is a logical argument, equals 'c' for call or 'p' for put
                    sigma  = sigma_wig, 
                    S_t    = S0_wig, 
                    t      = n_days - 1, 
                    freq   = freq_g)  
{
   sigma   <- sigma/sqrt(freq)
   t       <- t * freq
   if(t==0)
      return(Payoff(strike, c_p, S_t))
   else
   {
      if(c_p=='c')
         return(S_t * pnorm(d1(strike, sigma, S_t, t))
                -pnorm(d2(strike, sigma, S_t, t)) * strike * exp(-r * t))
      else if(c_p=='p')
         return(-S_t*pnorm(-d1(strike, sigma, S_t, t))
                +pnorm(-d2(strike, sigma, S_t, t)) * strike * exp(-r * t))
      else if(c_p=='bc')
         return(pnorm(d2(strike, sigma, S_t, t)) * exp(-r * t))
      else if(c_p=='bp')
         return(pnorm(-d2(strike, sigma, S_t, t)) * exp(-r * t))
   }
}

Bonus <- function(strike = strike_g, 
                  c_p    = c_p_g,       
                  sigma  = sigma_wig, 
                  S_t    = S0_wig, 
                  t      = n_days - 1, 
                  freq   = freq_g,
                  sigma2 = sigma_max)  
{
  - BSPrice(strike, c_p, sigma, S_t, t, freq) + BSPrice(strike, c_p, sigma2, S_t, t, freq)
}

Delta <- function(strike = strike_g, 
                  c_p    = c_p_g,
                  sigma  = sigma_wig,
                  S_t    = S0_wig,
                  t      = n_days - 1,
                  freq   = freq_g)
{
   if(c_p=='c'||c_p=='p')
   {
      sigma   <- sigma/sqrt(freq)
      t       <- t * freq
      pnorm(d1(strike, sigma, S_t, t)) - (c_p=='p')
   }
   else if(c_p=='bc'||c_p=='bp')
      ((c_p=='bc') * 2 - 1) * exp(-r * t) * dnorm(d2(strike, sigma, S_t, t))/(sigma * S_t * sqrt(t))
}

PriceMatrix <- function(strike     = strike_g,
                        c_p        = c_p_g,
                        sim_matrix = paths_wig,
                        sigma      = sigma_wig,
                        freq       = freq_g)
{
   T_ = nrow(sim_matrix)
   for(t in 0:(T_ - 1)) ## :(((( sad loop user is sad
      sim_matrix[T_ - t,] = BSPrice(strike, c_p, sigma, sim_matrix[T_ - t,], t/freq, freq)
   return(sim_matrix)
}

#-----------4. Delta-hedging---------------
DeltaMatrix <- function(strike     = strike_g,
                        c_p        = c_p_g,
                        sim_matrix = paths_wig,
                        sigma      = sigma_wig,
                        step       = step_g,
                        freq       = freq_g)
{
   maxc      <- ncol(sim_matrix)
   T_ = nrow(sim_matrix)
   for(t in seq(1, (T_ - 1), by = step)) ##terribly sorry for the looks if it :( nevertheless works perfectly fine
      sim_matrix[t:min((t+step-1),T_-1),]=matrix(rep(Delta(strike,c_p,sigma,sim_matrix[t,],(T_-t)/freq,freq),min(step,T_-t)),ncol=maxc,nrow=min(step,T_-t),byrow=TRUE)
   sim_matrix[T_,] = (sim_matrix[T_,] > strike) - (c_p=='p')
   return(sim_matrix)
}

PortfolioValueMatrix <- function(strike     = strike_g, ###hooray!!! 
                                 c_p        = c_p_g,
                                 sim_matrix = paths_wig,
                                 sigma      = sigma_wig,
                                 step       = step_g,
                                 freq       = freq_g) 
{
   price_matrix   <- PriceMatrix(strike, c_p, sim_matrix, sigma, freq)
   delta_matrix   <- DeltaMatrix(strike, c_p, sim_matrix, sigma, step, freq)
   maxr           <- nrow(sim_matrix)
   maxc           <- ncol(sim_matrix)
   acc            <- exp(-r + apply(matrix(r, ncol = maxc, nrow = maxr), 2, cumsum))[maxr:1,]
   delta_delta    <- rbind(rep(0, maxc), delta_matrix[2:maxr,] - delta_matrix[1:(maxr - 1),])
   rehedge_cost   <- delta_delta * sim_matrix * acc
   rehedge_cost   <- apply(rehedge_cost, 2, cumsum)/acc
   return(-price_matrix + delta_matrix * sim_matrix - rehedge_cost 
          +(price_matrix[1,1] - delta_matrix[1,1] * sim_matrix[1,1]) * acc[maxr:1,])
}

PortfolioValueMatrix2 <- function(strike     = strike_g, #without discounting 
                                 c_p        = c_p_g,
                                 sim_matrix = paths_wig,
                                 sigma      = sigma_wig,
                                 step       = step_g,
                                 freq       = freq_g) 
{
   price_matrix   <- PriceMatrix(strike, c_p, sim_matrix, sigma, freq)
   delta_matrix   <- DeltaMatrix(strike, c_p, sim_matrix, sigma, step, freq)
   maxr           <- nrow(sim_matrix)
   maxc           <- ncol(sim_matrix)
   acc            <- exp(-r + apply(matrix(r, ncol = maxc, nrow = maxr), 2, cumsum))[maxr:1,]
   delta_delta    <- rbind(rep(0, maxc), delta_matrix[2:maxr,] - delta_matrix[1:(maxr - 1),])
   rehedge_cost   <- delta_delta * sim_matrix * acc
   rehedge_cost   <- apply(rehedge_cost, 2, cumsum)#/acc
   return(-price_matrix + delta_matrix * sim_matrix - rehedge_cost 
          +(price_matrix[1,1] - delta_matrix[1,1] * sim_matrix[1,1]))# acc[maxr:1,])
}

PortfolioValueMatrixCosts <- function(strike     = strike_g, #including transacting costs
                                      c_p        = c_p_g,
                                      sim_matrix = paths_wig,
                                      sigma      = sigma_wig,
                                      step       = step_g,
                                      freq       = freq_g,
                                      cost       = cost_g) 
{
   price_matrix   <- PriceMatrix(strike, c_p, sim_matrix, sigma, freq)
   delta_matrix   <- DeltaMatrix(strike, c_p, sim_matrix, sigma, step, freq)
   maxr           <- nrow(sim_matrix)
   maxc           <- ncol(sim_matrix)
   acc            <- exp(-r + apply(matrix(r, ncol = maxc, nrow = maxr), 2, cumsum))[maxr:1,]
   delta_delta    <- rbind(rep(0, maxc), delta_matrix[2:maxr,] - delta_matrix[1:(maxr - 1),])
   rehedge_cost   <- (delta_delta * sim_matrix + abs(delta_delta*sim_matrix) * cost) * acc
   rehedge_cost   <- apply(rehedge_cost, 2, cumsum)/acc
   return(-price_matrix + delta_matrix * sim_matrix - rehedge_cost 
          +(price_matrix[1,1] - delta_matrix[1,1] * sim_matrix[1,1]) * acc[maxr:1,])
}

PortfolioValueMatrixCosts2 <- function(strike     = strike_g, #without discounting 
                                      c_p        = c_p_g,
                                      sim_matrix = paths_wig,
                                      sigma      = sigma_wig,
                                      step       = step_g,
                                      freq       = freq_g,
                                      cost       = cost_g) 
{
   price_matrix   <- PriceMatrix(strike, c_p, sim_matrix, sigma, freq)
   delta_matrix   <- DeltaMatrix(strike, c_p, sim_matrix, sigma, step, freq)
   maxr           <- nrow(sim_matrix)
   maxc           <- ncol(sim_matrix)
   acc            <- exp(-r + apply(matrix(r, ncol = maxc, nrow = maxr), 2, cumsum))[maxr:1,]
   delta_delta    <- rbind(rep(0, maxc), delta_matrix[2:maxr,] - delta_matrix[1:(maxr - 1),])
   rehedge_cost   <- (delta_delta * sim_matrix + abs(delta_delta*sim_matrix) * cost) * acc
   rehedge_cost   <- apply(rehedge_cost, 2, cumsum)#/acc
   return(-price_matrix + delta_matrix * sim_matrix - rehedge_cost 
          +(price_matrix[1,1] - delta_matrix[1,1] * sim_matrix[1,1]))# * acc[maxr:1,])
}

PortfolioCompositionMatrix <- function(strike     = strike_g,
                                 c_p        = c_p_g,
                                 sim_matrix = paths_wig,
                                 sigma      = sigma_wig,
                                 step       = step_g,
                                 freq       = freq_g) 
{
   price_matrix   <- PriceMatrix(strike, c_p, sim_matrix, sigma, freq)
   delta_matrix   <- DeltaMatrix(strike, c_p, sim_matrix, sigma, step, freq)
   maxr           <- nrow(sim_matrix)
   maxc           <- ncol(sim_matrix)
   acc            <- exp(-r + apply(matrix(r, ncol = maxc, nrow = maxr), 2, cumsum))[maxr:1,]
   delta_delta    <- rbind(rep(0, maxc), delta_matrix[2:maxr,] - delta_matrix[1:(maxr - 1),])
   rehedge_cost   <- delta_delta * sim_matrix * acc
   rehedge_cost   <- apply(rehedge_cost, 2, cumsum)/acc
   return(list(-price_matrix/price_matrix, delta_matrix, (- rehedge_cost 
          +(price_matrix[1,1] - delta_matrix[1,1] * sim_matrix[1,1]) * acc[maxr:1,])/(sim_matrix[1,1]*delta_matrix[1,1]-price_matrix[1,1])))
}

#-------------5. Gamma hedging-------------

Gamma <- function(strike = strike_g, 
                  c_p    = c_p_g,
                  sigma  = sigma_wig,
                  S_t    = S0_wig,
                  t      = n_days - 1,
                  freq   = freq_g)
{
   if(c_p=='c'||c_p=='p')
   {
      sigma   <- sigma/sqrt(freq)
      t       <- t * freq
      ((c_p=='c') * 2 - 1) * dnorm(d1(strike, sigma, S_t, t))/(S_t * sigma * sqrt(t)) 
   }
   else if(c_p=='bc'||c_p=='bp')
      ((c_p=='bc') * 2 - 1) * exp(-r * t) * d1(strike, sigma, S_t, t) *
      dnorm(d2(strike, sigma, S_t, t)) / (sigma^2 * S_t^2 *t)
}
GammaMatrix <- function(strike     = strike_g,
                        c_p        = c_p_g,
                        sim_matrix = paths_wig,
                        sigma      = sigma_wig,
                        step       = 1,
                        freq       = freq_g)
{
   maxc      <- ncol(sim_matrix)
   T_ = nrow(sim_matrix)
   for(t in seq(1, (T_ - 1), by = step)) ##terribly sorry for the looks if it :( nevertheless works perfectly fine
      sim_matrix[t:min((t+step-1),T_-1),]=matrix(rep(Gamma(strike,c_p,sigma,sim_matrix[t,],(T_-t)/freq,freq),min(step,T_-t)),ncol=maxc,nrow=min(step,T_-t),byrow=TRUE)
   sim_matrix[T_,] = (sim_matrix[T_,] > strike) - (c_p=='p')
   return(sim_matrix)
}

GammaNeutralMatrix <- function(strike     = strike_g, ###I have a dream, that one day... 
                                c_p        = c_p_g,
                                sim_matrix = paths_wig,
                                sigma      = sigma_wig,
                                step       = step_g,
                                freq       = freq_g)
{  maxr           <- nrow(sim_matrix)
   maxc           <- ncol(sim_matrix)
   gamma          <- Gamma(strike, c_p, sigma, S_t = S0_wig, t = maxr-1, freq)/Gamma(strike + spread, c_p = paste0('b',c_p), sigma, S_t = S0_wig, t = maxr -1, freq)
   delta_matrix   <- DeltaMatrix(strike, c_p, sim_matrix, sigma, step, freq) - gamma * DeltaMatrix(strike + spread, c_p = paste0('b',c_p), sim_matrix, sigma, step, freq)
   price_matrix   <- PriceMatrix(strike, c_p, sim_matrix, sigma, freq)
   price_matrix_b <- PriceMatrix(strike + spread, c_p = paste0('b',c_p), sim_matrix, sigma, freq)
   acc            <- exp(-r + apply(matrix(r, ncol = maxc, nrow = maxr), 2, cumsum))[maxr:1,]
   delta_delta    <- rbind(rep(0, maxc), delta_matrix[2:maxr,] - delta_matrix[1:(maxr - 1),])
   print(delta_matrix[,1]);print(delta_delta[,1]);print(gamma)
   rehedge_cost   <- delta_delta * sim_matrix * acc
   rehedge_cost   <- apply(rehedge_cost, 2, cumsum)/acc
   return(-price_matrix + delta_matrix * sim_matrix  + gamma * price_matrix_b
          - rehedge_cost 
          + (price_matrix[1,1] - delta_matrix[1,1] * sim_matrix[1,1] - price_matrix_b[1,1] * gamma) * acc[maxr:1,])
}


GammaNeutralMatrix2 <- function(strike     = strike_g, ###without discounting
                               c_p        = c_p_g,
                               sim_matrix = paths_wig,
                               sigma      = sigma_wig,
                               step       = step_g,
                               freq       = freq_g)
{  
   maxr           <- nrow(sim_matrix)
   maxc           <- ncol(sim_matrix)
   gamma_b1       <- GammaMatrix(1500, c_p = paste0('b',c_p), sim_matrix, sigma, step, freq)
   gamma_b2       <- GammaMatrix(3200, c_p = paste0('b',c_p), sim_matrix, sigma, step, freq)
   delta_b1       <- DeltaMatrix(1500, c_p = paste0('b',c_p), sim_matrix, sigma, step, freq)
   delta_b2       <- DeltaMatrix(3200, c_p = paste0('b',c_p), sim_matrix, sigma, step, freq)
   value_b1       <- PriceMatrix(1500, c_p = paste0('b',c_p), sim_matrix, sigma, freq)
   value_b2       <- PriceMatrix(3200, c_p = paste0('b',c_p), sim_matrix, sigma, freq)
   gamma_v        <- GammaMatrix(strike, c_p, sim_matrix, sigma, step, freq)
   delta_v        <- DeltaMatrix(strike, c_p, sim_matrix, sigma, step, freq)
   value_v        <- PriceMatrix(strike, c_p, sim_matrix, sigma, freq)
   over_tres      <- value_b1 > (-gamma_b1/gamma_b2 * value_b2)
   ksi1           <- gamma_v/gamma_b1 * over_tres
   ksi1[which(ksi1>1000)] <- 1000
   #ksi2           <- gamma_v/gamma_b2 * !over_tres
   ksi2           <- (value_v - ksi1 * gamma_b1)/gamma_b2
   #ksi2[which(ksi2>1000)] <- 1000
   ksi0           <- delta_v - ksi1 * delta_b1 - ksi2 * delta_b2
   acc            <- exp(-r + apply(matrix(r, ncol = maxc, nrow = maxr), 2, cumsum))[maxr:1,]
   delta_ksi1     <- rbind(rep(0, maxc), ksi1[2:maxr,] - ksi1[1:(maxr - 1),])
   delta_ksi2     <- rbind(rep(0, maxc), ksi2[2:maxr,] - ksi2[1:(maxr - 1),])
   delta_ksi0     <- rbind(rep(0, maxc), ksi0[2:maxr,] - ksi0[1:(maxr - 1),])
   print(which(over_tres==FALSE));
   print(sim_matrix[,1])
   print(delta_ksi0[,1]);print(delta_ksi1[,1]);print(delta_ksi2[,1])
   print((-gamma_v+ksi1*gamma_b1+ksi2*gamma_b2)[,1]);
   print((ksi0-delta_v+ksi1*delta_b1+ksi2*delta_b2)[,1])
   rehedge_cost   <- (delta_ksi0 * sim_matrix + delta_ksi1 * value_b1 + delta_ksi2 * value_b2) * acc
   rehedge_cost   <- apply(rehedge_cost, 2, cumsum)/acc
   return(-value_v + ksi0 * sim_matrix  + ksi1 * value_b1 + ksi2 * value_b2
          - rehedge_cost 
          + (value_v[1,1] - ksi0[1,1] * sim_matrix[1,1] - ksi1[1,1] * value_b1[1,1] - ksi2[1,1] * value_b2) * acc[maxr:1,])
}
