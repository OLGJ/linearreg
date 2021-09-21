#' @title Linreg
#' @name linreg
#' @param formula Formula for the linear regression.
#' @param data A dataframe with observations.
#' @return This package returns many statistics.
#' \describe{
#'  \item{Regression.Coefficient}{Regressions coefficients}
#'  \item{Fitted.Values}{The fitted values}
#'  \item{Residuals}{The residuals}
#'  \item{df}{The degrees of freedom}
#'  \item{Residual.Variance}{The residual variance}
#'  \item{Variance.B}{The variance of the regression coefficients}
#'  \item{T.Values}{The t-values for each coefficient}
#'  \item{P.Values}{The p-values for each coefficient}
#'  \item{call}{The arguments used to call the function}
#' }
#' @import ggplot2
#' @field Several statistics.
#' @docType package
#' @export linreg
#' @exportClass linreg
NULL

linreg <- setRefClass(
  "linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    regression.coefficients = "matrix",
    fitted.values = "matrix",
    residuals = "matrix",
    df = "numeric",
    residual.variance = "numeric",
    variance.regression.coefficients = "matrix",
    t.vals = "matrix",
    p.vals = "matrix",
    call = "character"

  ),
  methods = list(
    initialize = function(formula, data){
      .self$formula = formula
      .self$data = data

      X <- model.matrix(formula, data)
      y <- all.vars(formula)[1]
      yvals <- data[,y]
      Xt <- t(X) # Transpose of X
      Xt_X <- Xt %*% X # X_transpose * X
      Xt_X_i <- solve(Xt_X) # inverse of (X_transpose * X)
      Xt_y <- Xt %*% yvals # transpose of X * y-values

      .self$regression.coefficients = Xt_X_i %*% Xt_y # Regression coeff
      .self$fitted.values = X %*% .self$regression.coefficients # Fitted values
      .self$residuals = yvals - .self$fitted.values # Residuals
      .self$df = nrow(X)-ncol(X) # Degrees of freedom
      .self$residual.variance = c((t(.self$residuals)%*%.self$residuals)/.self$df) # Residual variance
      .self$variance.regression.coefficients = .self$residual.variance * Xt_X_i #Var(^B)

      sq_coeff <- sqrt(diag(.self$variance.regression.coefficients))

      .self$t.vals = .self$regression.coefficients/sq_coeff # T-values
      .self$p.vals = pt(.self$t.vals, .self$df) # P-values

      .self$call = c(deparse(substitute(formula)), deparse(substitute(data)))

    },
    show = function(){

      # Call
      call_label <- paste0("\n", "Call:\n", "lm(formula",call[1],", data = ",
                           call[2],")", "\n\n")
      cat(call_label)
      # Coefficients
      rnames <- row.names(.self$regression.coefficients)
      coef_vector <- c(.self$regression.coefficients)
      names(coef_vector) <- c(rnames)
      cat("Coefficients:\n"
          , names(coef_vector), "\n", coef_vector)



    },
    resid = function(){
      # Residuals as a vector
      resid_vector <- c(.self$residuals)
      return(resid_vector)
    },
    pred = function(){
      # Returns predicted values of y in matrix form
      return(.self$fitted.values)
    },
    coef = function(){
      # Coefficients as named vector
      rnames <- row.names(.self$regression.coefficients)
      coef_vector <- c(.self$regression.coefficients)
      names(coef_vector) <- c(rnames)
      return(coef_vector)
    },
    summary = function(){

      # Coefficients
      rnames <- row.names(.self$regression.coefficients)
      coef_vector <- c(.self$regression.coefficients)
      names(coef_vector) <- c(rnames)

      sum_dir <- data.frame( coef_vector,
                            c(.self$t.vals),
                            c(.self$p.vals),
                            c(.self$df))

      colnames(sum_dir) = c("Coefficients","t-values", "p-values", "df")

      print(sum_dir)
    },
    plot = function(){
      # Bottom label for both plots
      bot_label <- paste0("lm(",call[1],")")

      # Plot 1
      plot_data = data.frame(Residuals = .self$residuals,
                             Fitted = .self$fitted.values,
                             rows = c(1:length(residuals)),
                             absolute = abs(.self$residuals))

      critical_values = tail(plot_data[order(plot_data$absolute),],3)
      critical_point = critical_values$absolute[1]

      gg1 <- ggplot2::ggplot(data = plot_data, aes(y = Residuals, x = Fitted))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("Residuals vs Fitted") +
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point(shape = 1, colour = "black", fill = "white", size = 2, alpha=.8)+
        scale_color_continuous(low = "grey", high = "black") +
        stat_summary(fun=median, geom="line", color = "red", linetype = "solid")+
        geom_hline(yintercept = 0, linetype = "dotted", color = "grey")+
        geom_label(data= plot_data[plot_data$absolute >= critical_point,],
          aes(label=rows))+
        labs(caption=bot_label) +
        theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

      # Plot 2
      resid_standard_error <- sqrt(.self$residual.variance)
      sqrt_std_resid <- sqrt(abs(.self$residuals / resid_standard_error))
      plot2_data = data.frame(
        sqrt_std_resid = sqrt_std_resid,
        fitted = .self$fitted.values,
        rows = c(1:length(residuals)),
        absolute = abs(.self$residuals))

      gg2 <- ggplot2::ggplot(data = plot2_data, aes(y = sqrt_std_resid, x = fitted))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        geom_point(shape = 1, colour = "black", fill = "white", size = 2)+
        stat_summary(fun=mean, geom="line", color = "red", linetype = "solid")+
        labs(x = "Fitted Values",
             y = expression(sqrt("Standardized residuals")))+
        ggtitle("Scale-Location")+
        theme(plot.title = element_text(hjust = 0.5))+
        labs(caption=bot_label) +
        theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))+
        geom_label(data= plot2_data[plot2_data$absolute >= critical_point,],
                 aes(label=rows))
      print(gg1)
      print(gg2)
    })

  )


