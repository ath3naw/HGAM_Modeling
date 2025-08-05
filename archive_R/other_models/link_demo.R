

predictor_values <- as.matrix(covs) %>%
  as_tibble %>%
  filter(!is.nan(ttemp))

predictor_values <- predictor_values[floor(seq(from = 1, to = nrow(predictor_values), length.out = 20)),]

predictor_values


coefs <- model_pa_random_logistic$coefficients %>%
  as.list

coefs$intercept <- coefs$`(Intercept)`

predictor_values %>%
  mutate(
    # model formula presence ~ tseas + tmax + trange
    prediction_link = coefs$intercept +
      ttemp*coefs$ttemp + 
      tiso*coefs$tiso +
      tseas*coefs$tseas +
      twet*coefs$twet +
      pprec*coefs$pprec +
      pseas*coefs$pseas +
      pwarm*coefs$pwarm,
    prediction_response = 1/(1+exp(-prediction_link))
  )

