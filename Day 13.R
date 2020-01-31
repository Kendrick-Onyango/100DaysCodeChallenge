
#Applied Bayesian Analysis
rm(list = ls(all=TRUE))
cat('\014')

path <- "C:/Users/Mustapha/Desktop/100DaysofCode"
 
setwd(path)
  
# remove.packages('rstan')
# if (file.exists('.RData'))  file.remove('.RData')
# 
# 
# install.packages('rstan', repos = 'https://cloud.r-project.org/', dependencies = TRUE)
# 
# 
# # Checking the C++ Toolchain
# pkgbuild::has_build_tools(debug = TRUE)
# 
# 
# #Configuration of the C++ Toolchain
# dotR <- file.path(Sys.getenv('HOME'), '.R')
# if (!file.exists(dotR)) dir.create(dotR)
# M <- file.path(dotR, ifelse(.Platform$OS.type == 'windows', 'Makevars.win', 'Makevars'))
# if (!file.exists(M)) file.create(M)
# cat('\nCXX14FLAGS=-03 -march=native -mtune=native', 
#     if(grepl('^darwin', R.version$os)) 'CXX14FLAGS += -arch x86_64 -ftemplate-depth-256' else
#     if(.Platform$OS.type == 'windows') 'CXX11FLAGS=-03 -march=corei7 -mtune=corei7' else
#       'CXX14FLAGS += -fPIC',
#     file = M, sep = '\n', append = TRUE)
# 
# 
# library(rstan)
# library(ggplot2)
# library(StanHeaders)
# 
# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)
# Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
# 


#packages
pkgs <- c('rstan', 'bayesplot', 'shinystan', 'coda', 'dplyr')

# Install unistalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

#load all packages to library
lapply(pkgs, library, character.only = TRUE)




len.pi <- 1001L                      ### number of candidate values for pi
pi <- seq(0, 1, length.out = len.pi) ### candidate values for pi
a <- b <- 5                          ### hyperparameters
prior <- dbeta(pi, a, b)             ### prior distribution



## Plot   ### set up empty plot, specify labels
plot(pi, prior,type = 'n',xlab = "Density", 
     ylab = expression(paste("Prior Distribution for ", pi)))


### draw density distribution
polygon(c(rep(0, length(pi)), pi),c(prior, rev(prior)),
        col = adjustcolor('red', alpha.f = .4), border = NA)

### add vertical at pi = 0.5 
abline(v = .5, col = 'white')






#Code: Simulating the experiment

set.seed(20190417)                   ### set seed for replicability
len.pi <- 1001L                      ### number of candidate values for pi
pi <- seq(0, 1, length.out = len.pi) ### candidate values for pi
a <- b <- 5                          ### hyperparameters
n <- 200                             ### num. of coin flips
pi_true <- .8                        ### true parameter
data <- rbinom(n, 1, pi_true)        ### n coin flips
posterior <- matrix(NA, 3L, n)       ### matrix container for posterior

for (i in seq_len(n)) {    
  current.sequence <- data[1:i]      ### sequence up until ith draw
  k <- sum(current.sequence)         ### number of heads in current sequence
  
  ##### Updating
  a.prime <- a + k               
  b.prime <- b + i - k
  
  ### Analytical means and credible intervals
  posterior[1, i] <- a.prime / (a.prime + b.prime)
  posterior[2, i] <- qbeta(0.025, a.prime, b.prime)
  posterior[3, i] <- qbeta(0.975, a.prime, b.prime)
}

## Plot
plot(                                ### set up empty plot with labels
  1:n, 1:n,
  type = 'n',
  xlab = "Number of Coin Flips",
  ylab = expression(
    paste(
      "Posterior Means of ",
      pi, " (with 95% Credible Intervals)",
      sep = " "
    )
  ),
  ylim = c(0, 1),
  xlim = c(1, n)
)
abline(                              ### reference line for the true pi
  h = c(.5, .8),
  col = "gray80"
)
rect(-.5, qbeta(0.025, 5, 5),        ### prior mean + interval at i = 0
     0.5, qbeta(0.975, 5, 5),
     col = adjustcolor('red', .4),
     border = adjustcolor('red', .2))
segments(-.5, .5,
         0.5, .5,
         col = adjustcolor('red', .9),
         lwd = 1.5)
polygon(                             ### posterior means + intervals
  c(seq_len(n), rev(seq_len(n))),
  c(posterior[2, ], rev(posterior[3, ])),
  col = adjustcolor('blue', .4),
  border = adjustcolor('blue', .2)
)
lines(
  seq_len(n),
  posterior[1, ],
  col = adjustcolor('blue', .9),
  lwd = 1.5
)






#Code: Fake data generation

set.seed(20190417)
N.sim <- 10000L                               ### num. observations
K.sim <- 5L                                   ### num. predictors
x.sim <- cbind(                               ### model matrix
  rep(1, N.sim), 
  matrix(rnorm(N.sim * (K.sim - 1)), N.sim, (K.sim - 1))
)
beta.sim <- rnorm(K.sim, 0, 10)               ### coef. vector
sigma.sim <- abs(rcauchy(1, 0, 5))            ### scale parameter
mu.sim <- x.sim %*% beta.sim                  ### linear prediction
y.sim <- rnorm(N.sim, mu.sim, sigma.sim)      ### simulated outcome



## Setup
# library(rstan)
# rstan_options(auto_write = TRUE)             ### avoid recompilation of models
# options(mc.cores = parallel::detectCores())  ### parallelize across all CPUs
# Sys.setenv(LOCAL_CPPFLAGS = '-march=native') ### improve execution time

## Data (see data block) as list
standat.sim <- list(
  N = N.sim,
  K = K.sim,
  x = x.sim,
  y = y.sim
)



#Sys.getenv("R_MAKEVARS_USER")

## C++ Compilation
lm.mod <- stan_model(file = "lm.stan")




lm.sim <- sampling(lm.mod,                            ### compiled model
                   data = standat.sim,                ### data input
                   algorithm = "NUTS",                ### algorithm
                   control = list(                    ### control arguments
                     adapt_delta = .85
                   ),
                   save_warmup = FALSE,               ### discard warmup samples
                   sample_file = NULL,                ### no sample file
                   diagnostic_file = NULL,            ### no diagnostic file
                   pars = c("beta", "sigma"),         ### select parameters
                   iter = 2000L,                      ### iter per chain
                   warmup = 1000L,                    ### warmup period
                   thin = 2L,                         ### thinning factor
                   chains = 4L,                       ### num. chains
                   cores = 4L,                        ### num. cores
                   seed = 20190417)                   ### seed





str(lm.sim)



true.pars <- c(beta.sim, sigma.sim)
names(true.pars) <- c(paste0("beta[", 1:5, "]"), "sigma")
round(true.pars, 2L)



print(lm.sim, pars = c("beta", "sigma"))


## Retrieve and manage data
bw.ajps19 <-
  read.table(
    paste0(
      "https://dataverse.harvard.edu/api/access/datafile/",
      ":persistentId?persistentId=doi:10.7910/DVN/DZ1NFG/LFX4A9"
    ),
    header = TRUE,
    stringsAsFactors = FALSE,
    sep = "\t",
    fill = TRUE
  ) %>% 
  select(wave, fortuyn, polarization) %>% ### select relevant variables
  subset(wave == 1) %>%                   ### subset to pre-election wave
  na.omit()                               ### drop incomplete rows

x <- model.matrix(~ fortuyn, data = bw.ajps19)
y <- bw.ajps19$polarization
N <- nrow(x)
K <- ncol(x)



## data as list
standat <- list(
  N = N,
  K = K,
  x = x,
  y = y)

## inference
lm.inf <- sampling(lm.mod,                            ### compiled model
                   data = standat,                    ### data input
                   algorithm = "NUTS",                ### algorithm
                   control = list(                    ### control arguments
                     adapt_delta = .85
                   ),
                   save_warmup = FALSE,               ### discard warmup samples
                   sample_file = NULL,                ### no sample file
                   diagnostic_file = NULL,            ### no diagnostic file
                   pars = c("beta", "sigma"),         ### select parameters
                   iter = 2000L,                      ### iter per chain
                   warmup = 1000L,                    ### warmup period
                   thin = 2L,                         ### thinning factor
                   chains = 4L,                       ### num. chains
                   cores = 4L,                        ### num. cores
                   seed = 20190417)                   ### seed




print(lm.inf,
      pars = c("beta", "sigma"),
      digits_summary = 3L)



## Extract posterior samples for beta[2]
beta2_posterior <- extract(lm.inf)$beta[, 2]

## Probability that beta[2] is greater than zero
mean(beta2_posterior > 0)




## Stanfit to mcmc.list
lm.mcmc <- As.mcmc.list(lm.inf,
                        pars = c("beta", "sigma", "lp__"))

## Diagnostics
library(coda)
geweke.diag(lm.mcmc, frac1 = .1, frac2 = .5)              ### Geweke



heidel.diag(lm.mcmc, pvalue = .1)                         ### Heidelberger-Welch



raftery.diag(lm.mcmc,                                     ### Raftery-Lewis
             q = 0.025,
             r = 0.005,
             s = 0.95,
             converge.eps = 0.001)




## Extract posterior draws from stanfit object
lm.post.draws <- extract(lm.inf, permuted = FALSE)


library(bayesplot)
## Traceplot
mcmc_trace(lm.post.draws, pars = c("beta[1]", "beta[2]", "sigma"))








