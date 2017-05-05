# This example from http://wiki.cbr.washington.edu/qerm/index.php/R/Interactive_Plots

library(stats)
library(manipulate)

plot.beta.binomial <- function(Heads=5, Tails=4, alpha=1.5,
                               beta=1.5, Bayesian=TRUE){
    ## Calculate the distributions
    alpha <- max(alpha, .001)
    beta <- max(beta, .001)
    num.trials <- Heads+Tails
    p.seq <- seq(0.00001,.99999, len=500)
    likelihood <- dbinom(x=Tails, size=num.trials, prob=p.seq)
    prior <- dbeta(x=p.seq, alpha, beta)
    param1 <- alpha+Tails
    param2 <-  num.trials-Tails+beta
    posterior <- dbeta(x=p.seq, param1,param2)

    ## Scale them for plotting
    scale <- .9
    likelihood <- likelihood*scale/max(likelihood)
    density.max <- max(c(prior, posterior))
    prior <- prior*scale/density.max
    posterior <- posterior*scale/density.max
    p.hat <- Tails/num.trials        # the MLE

    ## Plot them
    plot(p.seq, likelihood, type="l", lwd=2, col=1, ylab=NA,
          axes=FALSE,xlab="Probability of a Tails", 
          xlim=c(0,1), ylim=c(0,1))
    mtext(side=3, text="Beta-Binomial", line=1, cex=2.5)
    mtext(side=2, text="Scaled Likelihood/Density", line=1)
    axis(1); box()
    points(p.hat, scale, cex=1.25, pch=16)
    if(Bayesian){
        lines(p.seq, prior, lwd=2, col=2)
        lines(p.seq, posterior, lwd=2, col=4)
    }
    par(xpd=TRUE)
    legend(x=0, y=1.05, legend=c("Likelihood", 
                        "Prior", "Posterior"),
                        col=c(1,2,4), ncol=3, lty=c(1,1,1),
                        bty="n", lwd=2)
}

plot.beta.binomial()

manipulate(plot.beta.binomial(Heads, Tails, alpha, beta,
                              Bayesian),
                              Heads=slider(0,25),
                              Tails=slider(0,25),
                              alpha=slider(1,15, init=1, step=.5),
                              beta=slider(1,15, init=1, step=.5),
                              Bayesian=checkbox())

