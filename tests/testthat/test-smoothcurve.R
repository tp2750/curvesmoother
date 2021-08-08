x <- 1:100
y <- 1- exp(-x/10)
test_that("call smooth", {
    expect_equal(names(curvesmoother(data.frame(x,y))), c("x","y","yhat","yhatdot","lambda","df","spar"))
})
