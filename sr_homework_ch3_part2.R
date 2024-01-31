data(homeworkch3) # load data
# loads firstborns as birth1 and second-borns as birth2

# PROBLEMS 3H4 and 3H5
# (mostly same code from last problem!)
N=50 # define number of grid points for parameter space for problems at hand

p_grid <- seq(from=0, to=1, length.out=N)

prior <- rep(1,N) # flat prior

male_total <- sum(birth1)

births_total <- length(birth1)

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

# Now check the quality of the model very loosely via simulation
sim_births <- rbinom(1e4, size=births_total, prob=samples)
dens(sim_births)

# you should get something that looks pretty reasonable! 

# Now we look at correlations among siblings!

# first, get the number of firstborn girls

num_firstborn_fem <- sum(1-birth1)

# then, extract only those second births where the first child was female
second_births_following_fem <- birth2[birth1==0]

# now simulate 49 births using model trained only on first generation. 
sim_births_gen2 <- rbinom(1e4, size=num_firstborn_fem, prob=samples)
dens(sim_births_gen2, adj=1) # note change to adj (smoothing kernel parameter) to get smoother density

# Let's look at the mean of the simulated births...
mean_sim <- signif(mean(sim_births_gen2), 3)

paste("Mean Number of Males Births in Simulation =", mean_sim)
paste("Actual Number of Male Second Births Following Females = ", sum(second_births_following_fem))

# You should see a strong discrepancy between model predictions and observed data: 
# the number of births in the data is 39, and I found the model predicted a mean of 25! 
# So, our simple model cannot account for correlations between siblings. 