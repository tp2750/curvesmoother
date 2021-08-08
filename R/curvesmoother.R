curvesmoother <- function(dat, ...) {
    stopifnot(all(c("x","y") %in% names(dat)))
    dat <- dat[, c("x", "y")]
    fit1 <- smooth.spline(dat, ...)
    yhat <- predict(fit1, dat$x)$y
    yhatdot <- predict(fit1,dat$x,deriv = 1)$y
    data.frame(dat, yhat, yhatdot, lambda=fit1$lambda, df = fit1$df, spar = fit1$spar)
}


cs_plot <- function(f2) {
    grid.arrange(
    ggplot(f2, aes(x=x,y=y)) + geom_point(color = "black") + geom_line(aes(y=yhat), color = "blue") + ggtitle(sprintf("Smooth Spline df=%.2f, lambda = %.2g", unique(f2$df), unique(f2$lambda))),
    ggplot(f2, aes(x=x)) + geom_line(aes(y=yhatdot), color = "blue") + ggtitle(sprintf("Smooth Spline Slopes df=%.2f, lambda = %.2g", unique(f2$df), unique(f2$lambda))),
    nrow=2
    )
}