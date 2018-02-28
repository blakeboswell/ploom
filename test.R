
y <- biglm::biglm(
  mpg ~ cyl + disp + hp + wt,
  data = mtcars
)

x <- ploom::online_lm(
  mtcars,
  formula = mpg ~ cyl + disp + hp + wt
)

x$qr$qr$D

y$qr$D
# all.equal(c(x$qr$D), y$qr$D)
# all.equal(c(x$qr$rbar), y$qr$rbar)
# all.equal(c(x$qr$thetab), y$qr$thetab)
# all.equal(c(x$qr$sserr), y$qr$ss)
# all.equal(c(x$qr$tol), y$qr$tol)
# 
# 
# 
# y <- biglm::biglm(
#   mpg ~ cyl + disp + hp + wt,
#   sandwich = TRUE,
#   data = mtcars
# )
# 
# x <- ploom::online_lm(
#   mpg ~ cyl + disp + hp + wt,
#   sandwich = FALSE,
#   data = mtcars
# )
# 
# x$qr$qr$D
# 
# class(x)
# class(x$qr)
# 
# all.equal(x$sandwich$xy$D, y$sandwich$xy$D)
# all.equal(x$sandwich$rbar, y$sandwich$rbar)
# all.equal(x$sandwich$thetab, y$sandwich$thetab)
# all.equal(x$sandwich$sserr, y$sandwich$ss)
# all.equal(x$sandwich$tol, y$sandwich$tol)
# all.equal(coef(x), coef(y))
# 
# a <- vcov(x)
# b <- vcov(y)
# 
# all.equal(a, b)
# 
# all.equal(x$sandwich$xy$D, y$sandwich$xy$D)
# all.equal(x$sandwich$rbar, y$sandwich$rbar)
# all.equal(x$sandwich$thetab, y$sandwich$thetab)
# all.equal(x$sandwich$sserr, y$sandwich$ss)
# all.equal(x$sandwich$tol, y$sandwich$tol)
# 
# 
# summary(x)
# summary(y)


# rm(list = ls())
# 
# X <- do.call(
#   rbind,
#   list(
#     c(1,5.1,3.5,1.4),
#     c(1,4.9,3.0,1.4),
#     c(1,7.0,3.2,4.7),
#     c(1,6.4,3.2,4.5),
#     c(1,6.3,3.3,6.0),
#     c(1,5.8,2.7,5.1),
#     c(1,4.6,3.4,1.4),
#     c(1,5.0,3.4,1.5),
#     c(1,4.4,2.9,1.4),
#     c(1,4.9,3.1,1.5)
#   ))
# 
# y <- c(0.2, 0.2, 1.4, 1.5, 2.5, 1.9, 0.3, 0.2, 0.2, 0.1)
# w <- rep(1.0, length(y))
# 
# 
# miller <- miller_lsqr(ncol(X))
# update_miller(X, y, w, miller)
# miller$D
# 
# bound <- new(BoundedQr, ncol(X))
# bound$update(X, y, w)
# bound$D



