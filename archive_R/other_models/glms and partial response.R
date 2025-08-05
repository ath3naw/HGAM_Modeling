# our data
covs <- terra::rast("data/grids/covariates.tif")
ttemp <- covs$ttemp
tiso <- covs$tiso
tseas <- covs$tseas
twet <- covs$twet
pprec <- covs$pprec
pseas <- covs$pseas
pwarm <- covs$pwarm
#presence <- data_pa_random$presence

# some coefficients/ slopes - play around with these
beta_ttemp <- 0.001
beta_tiso <- 0.005
beta_tseas <- 0.02
beta_twet <- 0.003
beta_pprec <- 0.002
beta_pseas <- 0.01
beta_pwarm <- 0.03
alpha <- -36

# calculate probability on link scale
gamma <- alpha + beta_ttemp*ttemp + beta_tiso*tiso + beta_tseas*tseas + beta_twet*twet + beta_pprec*pprec + 
  beta_pseas*pseas + beta_pwarm*pwarm
# logit link function
logitfun <- function(gamma){
  1/(1+exp(-gamma))
}

# calculate probability of presence
logitfun(gamma)

plot(logitfun(gamma))

# calculate a partial response curve
ttemp_mean <- mean(data_pa_random$ttemp)
tiso_mean <- mean(data_pa_random$tiso)
tseas_mean <- mean(data_pa_random$tseas)
twet_mean <- mean(data_pa_random$twet)
pprec_mean <- mean(data_pa_random$pprec)
pseas_mean <- mean(data_pa_random$pseas)
pwarm_mean <- mean(data_pa_random$pwarm)

pr_ttemp_data <- seq(
  from = 11,
  to = 29,
  by = 0.2
)

# some coefficients/ slopes - play around with these
beta_ttemp <- 0.001
beta_tiso <- 0.005
beta_tseas <- 0.02
beta_twet <- 0.003
beta_pprec <- 0.002
beta_pseas <- 0.01
beta_pwarm <- 0.03
alpha <- -36

# calculate probability on link scale
gamma_pr <- alpha + beta_ttemp*pr_ttemp_data + beta_tiso*tiso_mean + beta_tseas*tseas_mean + beta_twet*twet_mean + beta_pprec*pprec_mean + 
  beta_pseas*pseas_mean + beta_pwarm*pwarm_mean

# logit link function
logitfun <- function(gamma){
  1/(1+exp(-gamma))
}

# calculate probability of presence
prob_pr <- logitfun(gamma_pr)

plot(
  x = pr_ttemp_data,
  y = prob_pr#,
  #ylim = c(0, 1)
)

