library(rethinking)
data(Howell1)
d<- Howell1
d_4h2<-d[d$age<18,]

wbar = mean(d_4h2$weight)

# Obtain tabular summary of the data
# Strictly speaking, we should also plot the data (height vs weight) to infer a good
# trend, but that's not really the point of this exercise! 
precis(d_4h2)

# Stage 1: Prior Predictive Sampling
#^ the data here helps pick the prior for mean height: 
a.samples = rnorm(1e4, 108.32, 30)

# use priors from adult data for b and sigma
b.samples = rlnorm(1e4, 0,1)
sigma.samples = runif(1e4, 0,50)

# now get rough samples for mu...
mu.link <-function(weight) a.samples+b.samples*(weight-wbar)
weight.seq <- seq(from=0, to=45, by=1) # 45 generated by taking max of child weights
mu.samples<-apply(sapply(weight.seq, mu.link), 1, mean)

# now check how well the prior does
prior<-rnorm(1e4, mu.samples, sigma.samples)
dens(prior)
mean(prior)

# so far things look OK, not great, with a prior predicted mean of about 115 cm, which is a little high
# but not a disaster

# Stage 2: Train posterior using the data
m4h2<- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - wbar ) ,
    a ~ dnorm( 108.32 , 30 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d_4h2 )

# Stage 3 : Interpret the posterior (plot data, fit, and confidence intervals)
plot(height~weight, data=d_4h2, col=col.alpha(rangi2, 0.5))
mu<-link(m4h2, data=data.frame(weight=weight.seq))
mu.mean<-apply(mu,2,mean)
mu.PI<-apply(mu,2,PI, prob=0.89)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
sim.height<-sim(m4h2, data=list(weight=weight.seq))
height.PI <-apply(sim.height, 2, PI, prob=0.89)
shade(height.PI, weight.seq)

# Now, the model seems to give on-the-bad-side of OK predictions for weights
# near the mean, but for very small or very large weights it fails pretty
# badly. A better alternative is easy to guess: we should be modelling weight
# as a quadratic function of height (the variable names are indeed in the
# correct places! We could also model height = log of weight or something like
# that. The point is a simple linear input-output dependence is not enough to
# get a solid description of the data. 