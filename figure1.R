
# plot of simulated effects --------------------------------------------------

# Plots histograms of simulated effects, 

library(gridExtra)
# requires data.frame d from simulation block of ms.Rmd

d$sig <- ifelse(d$lower > 0, T, F) # indicator for significant samples
col_vals <- c("gray40", "red") # color labels for sig/non-sig samples
# Plot has complex label
plotlab <- bquote("Simulated sampling distribution of effects" ~ 
                  paste(mu == .(ppower$delta), ",") ~
                  "power" == .(round(ppower$power, 2)))
# histograms (top panel)
p1 <- ggplot(d, aes(x = mean), alpha = .5) +
    # histogram & density of all means
    # geom_histogram(aes(y = ..ncount..)) +
    stat_density(col = col_vals[1], aes(y = ..density..), 
                 geom = "line", adjust = 2) +
    # histogram & density of significant means
    # geom_histogram(data = filter(d, sig == T), fill = col_vals[2],
    #                aes(y = ..ncount..)) +
    stat_density(data = filter(d, sig == T), col = col_vals[2],
                 aes(y = ..density..), geom = "line", adjust = 2) +
    # mean of all means
    geom_vline(data = summarise(d, MU = mean(mean)),
               aes(xintercept=MU), col = col_vals[1]) +
    # mean of significant means
    geom_vline(data = filter(d, sig == T) %>%
                   summarise(MU = mean(mean)),
               aes(xintercept=MU), col = col_vals[2]) +
    annotate(geom = "text", label = "All samples", 
             x = -.5, y = 1, col = col_vals[1]) +
    annotate(geom = "text", label = "Significant samples", 
             x = 2.5, y = 1, col = col_vals[2]) +
    coord_cartesian(xlim = c(-1.5, 3.5)) +
    # labs(title = plotlab) +  # use fig.cap instead
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank())
# CIs (lower panel) -- show 100 CIs
subd <- filter(d, n < 50)
p2 <- ggplot(subd, aes(y = n, x = mean, xmin = lower, xmax = upper)) +
    # all CIs
    geom_errorbarh(col = col_vals[1]) +
    # significant CIs
    geom_errorbarh(data = filter(subd, sig == T), col = col_vals[2],
                   aes(y = n, x = mean, xmin = lower, xmax = upper)) +
    # mean of all means
    geom_vline(data = summarise(d, MU = mean(mean)),
               aes(xintercept=MU), col = col_vals[1]) +
    # mean of significant samples
    geom_vline(data = filter(d, sig == T) %>%
                   summarise(MU = mean(mean)),
               aes(xintercept=MU), col = col_vals[2]) +
    coord_cartesian(xlim = c(-1.5, 3.5)) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank())

grid.arrange(p1 + theme(plot.margin = unit(c(1,1,-.5,1), "cm")), 
             p2 + theme(plot.margin = unit(c(0,1,1,1), "cm")), 
                        ncol = 1, heights = c(.3, .7))
