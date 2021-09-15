
lm_ <- setClass("lm_")

linreg <- function(){
  group <- factor(c(1,1,2,2) )

  X <- model.matrix(~ group)

  Xt <- t(X) # Transpose of X
  Xt_X <- Xt %*% X # X_transpose * X
  Xt_X_i <- solve(Xt_X) # inverse of (X_transpose * X)

  y <- all.vars(group)
  #Xt_y <- Xt %*% y

  #reg_coefficients <- Xt_X_i %*% Xt_y
  #fitted_Values <- X %*% reg_coefficients

  #resids <- y - fitted_Values # seq_along(y) kanske
  #df <- length(reg_coefficients) # Inkorrekt

  # residual_variance <- (t(resids)%*%resids)/df
  #
  #var_reg_coefficients <- residual_variance %*% Xt_X_i

  #t_vals_reg_coefficients <- reg_coefficients/sqrt(var_reg_coefficients)

  #p_values <- pt()


  # return object of class linreg either as an s3 or RC
}

linreg()

