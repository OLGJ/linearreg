#' @title Linreg
#' @description Linear regression package in R.
#' @name linreg
#' @param formula Formula for the linear regression.
#' @param data A dataframe with observations.
#' @import ggplot2
#' @importFrom methods new
#' @field formula formula.
#' @field data data.frame.
#' @field regression.coefficients matrix.
#' @field fitted.values matrix.
#' @field residuals matrix.
#' @field df numeric.
#' @field residual.variance numeric.
#' @field variance.regression.coefficients matrix.
#' @field t.vals matrix.
#' @field p.vals matrix.
#' @field call character.
#' @field sq.coeff numeric.
#'
#' @docType class
#' @export

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
    call = "character",
    sq.coeff = "numeric"

  ),
  methods = list(
    initialize = function(formula, data){
      "Initialization."
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

      .self$sq.coeff <- sqrt(diag(.self$variance.regression.coefficients))

      .self$t.vals = .self$regression.coefficients/.self$sq.coeff # T-values
      .self$p.vals = pt(-abs(.self$t.vals), .self$df) # P-values

      .self$call = c(deparse(substitute(formula)), deparse(substitute(data)))

    },
    print = function(){
      "Prints a short output of the linear regression."
      # Call
      call_label <- paste0("\n", "Call:\n", "linreg(formula = ",call[1],", data = ",
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
      "Returns the residuals as a vector."
      resid_vector <- c(.self$residuals)
      return(resid_vector)
    },
    pred = function(){
      "Returns the predicted values of y in matrix form"
      return(.self$fitted.values)
    },
    coef = function(){
      "Returns the coefficients as named vector"
      rnames <- row.names(.self$regression.coefficients)
      coef_vector <- c(.self$regression.coefficients)
      names(coef_vector) <- c(rnames)
      return(coef_vector)
    },
    summary = function(){
      "Prints a short summary with Coefficients, their std error, t- & p-vals."
      rnames <- row.names(.self$regression.coefficients)
      coef_vector <- c(.self$regression.coefficients)
      names(coef_vector) <- c(rnames)


      sum_dir <- data.frame("Estimate" = round(coef_vector,5),
                            "Std Error" = round(c(.self$sq.coeff), 5),
                            "t value" = round(c(.self$t.vals),3),
                            "p value" = sapply(.self$p.vals,
                                          function(t) if(t < 0.001) {"***"}
                                          else if(t < 0.01) {"**"}
                                          else if(t < 0.05) {"*"}))

      cat("Coefficients:\n")
      print.data.frame(sum_dir)
      resid.std <- sqrt(.self$residual.variance)
      cat(paste("\nResidual standard error:",  round(resid.std, 2), "on", .self$df,
                  "degrees of freedom"))
    },
    plot = function(){
      "Generates plots."
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

