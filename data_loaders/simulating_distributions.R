"
Simulating distributions

"
a <- rep(NA,10)
for (i in c(1:10)) {
    a[i] <- rbinom(1,1,0.5)
}
a
