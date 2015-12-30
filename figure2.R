# Plot coverage against various X

cov_sim <- function(x_name = "n", # Name of X
                    n = 36, # Range (or value) of sample size
                    mu = .5, # Range (or value) of sample mean
                    sd = 2.5, # Range (or value) of sample sd
                    alpha = .05, # Range (or value) of alpha
                    n_sims = 500 # n of exps at each level of X
                    ) {
    # Simulates one-sample t-tests for each level of X
    # Outputs a dataframe that can be used to plot coverage vs X
    # Choose X by entering a range for one of values entered for pow calc

    cov_dat <- data.frame(
        n = n,
        mu = mu,
        sd = sd,
        alpha = alpha,
        power = NA,
        coverage_sig_a = NA,
        coverage_sig_b = NA)
    
    # Run simulation loop over values of X
    for (i in 1:nrow(cov_dat)) {
        # Save power and current X values
        pow <- power.t.test(
            n = cov_dat[i, "n"], 
            delta = cov_dat[i, "mu"], 
            sd = cov_dat[i, "sd"], 
            sig.level = cov_dat[i, "alpha"], 
            alternative = "two.sided",
            type = "one.sample",
            power = NULL
        )
        cov_dat[i, "power"] <- pow$power
        # Run n_sims experiments
        bt <- replicate(rnorm(n = pow$n, 
                              mean = pow$delta, 
                              sd = pow$sd), n = n_sims)
        means <- apply(bt, 2, mean)  # sample means
        f_se <- function(x) {sd(x) / sqrt(pow$n)}
        ses <- apply(bt, 2, f_se)  # sample standard errors
        # generate bounds for 1-alpha confidence intervals for each sample
        crit <- qt(1 - alpha/2, df = pow$n-1)
        upper <- means + crit * ses
        lower <- means - crit * ses
        # Save CIs from simulated experiments
        d <- data.frame(mean = means,
                        upper = upper,
                        lower = lower)
        # Significant CIs
        sig_cis <- filter(d, lower > 0)
        # Significant CIs that cover mu
        sig_cov_cis <- filter(sig_cis, 
                              lower <= pow$delta, 
                              upper >= pow$delta)
        # significant + cover mu / significant
        coverage_a <- nrow(sig_cov_cis) / nrow(sig_cis)
        cov_dat[i, "coverage_sig_a"] <- coverage_a
        # significant + cover mu / all experiments
        coverage_b <- nrow(sig_cov_cis) / n_sims
        cov_dat[i, "coverage_sig_b"] <- coverage_b
    }
    print(x_name)
    # Build plot
    p1 <- ggplot(cov_dat, aes_string(x=x_name)) +
        geom_hline(yintercept = .95, lty = 2) +
        geom_line(aes(y = power), col="gray50") +
        geom_point(aes(y=coverage_sig_a), col="black", shape=1) +
        geom_point(aes(y=coverage_sig_b), col="red", shape=1) +
        scale_x_continuous(x_name,
                           limits = c(0, 1)) +
        scale_y_continuous("Coverage proportion", 
                           breaks = seq(.1, .9, .2),
                           limits = c(0, 1))
    
    # Return list with data and plot
    return(list(data = cov_dat, plot = p1))
}

s1 <- cov_sim(x_name = "power", n = seq(3, 104, length=100))
s2 <- cov_sim(x_name = "power", mu = seq(.2, 1.5, length=100))
s3 <- cov_sim(x_name = "power", sd = seq(1, 6, length=100))

grid.arrange(s1$plot,
             s2$plot,
             s3$plot,
             nrow=2)
