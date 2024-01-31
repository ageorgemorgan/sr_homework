data(homeworkch3) # load data
# loads firstborns as birth1 and second-borns as birth2

# PROBLEMS 3H1, 3H2, 3H3
N=100 # define number of grid points for parameter space for problems 3H1 and 3H2

p_grid <- seq(from=0, to=1, length.out=N)

prior <- rep(1,N) # flat prior

male_total <- sum(birth1) + sum(birth2)

births_total <- length(birth1) + length(birth2)

# obtain posterior using Bayes' Theorem
likelihood <- dbinom(male_total, size=births_total, prob=p_grid)

posterior <- likelihood*prior

posterior <- posterior/sum(posterior)

# plot the posterior. Requires xkcdcolors package. 
plot(p_grid, posterior, type="b", xlab = 'Probability of Male Birth', ylab= 'Posterior', col=name2color('barney purple'))

# report mode of the posterior (ie. for MAP), but since it's a bell curve it's pretty harmless. 
MAP <- p_grid[which.max(posterior)]
MAP_trunc <- signif(MAP, digits=3)
paste("MAP estimate of male birth probability =", MAP_trunc)

# Get samples from model distribution, and compute confidence intervals
samples <- sample(p_grid, 1e4, replace=TRUE, prob=posterior)

print("Requested HPDIs:")
HPDI(samples, prob=0.5)
HPDI(samples, prob=0.89)
HPDI(samples, prob=0.97)

# Now check the quality of the model very loosely via simulation
sim_births <- rbinom(1e4, size=births_total, prob=samples)
dens(sim_births)

# you should get something that looks pretty reasonable! 