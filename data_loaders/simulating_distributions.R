"
Simulating distributions

TODO: 
- Load my data and fit a regression using glmstan 
- Make an example with replicate
- Check if coefficients are within 1 and 2 standard deviations
- Compute uncertainty interval assuming a t distribution
- Compute uncertainty interval from the fit model
- Reproduce these uncertainty computations for the EM models fast and slow answers E2
"
a <- rep(NA,10)
for (i in c(1:10)) {
    a[i] <- rbinom(1,1,0.5)
}
a
