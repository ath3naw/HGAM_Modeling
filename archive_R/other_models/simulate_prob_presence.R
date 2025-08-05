# why we use max_average_catch_size and relative abundance
mosquitoes_in_1sqkm <- c(1, 50000, 100000)
prop_mosquitoes_caught_in_trap <- 0.001
average_catch_size <- mosquitoes_in_1sqkm * prop_mosquitoes_caught_in_trap
average_catch_size

max_average_catch_size <- max(average_catch_size)
relative_abundance <- mosquitoes_in_1sqkm / max(mosquitoes_in_1sqkm)

# relation between relative_abundance and average_catch_size
average_catch_size <- relative_abundance * max_average_catch_size
average_catch_size # same as before



# using poisson dist to determine catch size
average_catch_size <- 3
catch_size <- rpois(1, average_catch_size)
catch_size





n_catches <- 50000
catch_sizes <- rpois(n_catches, average_catch_size)
hist(catch_sizes, breaks = 100) # the catch sizes, estimated by a poisson dist





n_presences <- sum(catch_sizes > 0)
prob_present <- n_presences / n_catches
prob_present

# an approximation for prob_present
# Poisson dist: lambda^ke^{-lambda}/k! -> 1-prob(0 presence)=1-average_catch_size^0e^{-average_catch_size}/0!=1-exp(-average_catch_size)
1-exp(-average_catch_size)

