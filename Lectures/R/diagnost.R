if (!require(statmod)) install.packages("statmod")

# mod: the glm model to diagnose
#
# type: the type of residuals to be calculated. This can be
#   #"rqr", for "randomized quantile residuals" (default). These are calculated using statmod::qresiduals() 
#   #"deviance", or "pearson", or any other type of residuals from residuals()
#
# plots: Which plots to show? This can be 
#   #"response", for reponses ~ fitteds 
#   #"fitted", for residuals ~ fitteds 
#   #"qqplot", to make a QQ-plot
#   #"formula", residuals ~ term, for all "terms" (covariates) appearing in the model formula
#   #"data", residuals ~ var, for all numerical covariates appearing in the provided data
#   #FALSE, return the residuals, do not plot
#   #a character vector of the names of the variables (in the provided data) against which the residuals are to be plotted

diagnost <- function(mod, type = "rqr", plots = c("response", "fitted", "qqplot", "formula", "data")) {
  if (type == "rqr") {
    prsd <- statmod::qresiduals(mod) |> pnorm()
    prsd[prsd == 1] <- 1 - 1e-010
    prsd[prsd == 0] <- 1e-010
    rsd <- prsd |> qnorm()
  } else {
    rsd <- residuals(mod, type = type)
  }
  if (length(plots) == 1) {
    if (plots == FALSE) {
      return(rsd)
    }
  }
  if (length(dev.list()) == 0) dev.new()
  opar <- par(no.readonly = TRUE)
  par(mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0), pch = 20)
  if ("formula" %in% plots) plots <- c(plots, attr(terms(mod), "term.labels")) |> unique()
  
  NumVarNames <- colnames(Filter(is.numeric, mod$data))
  resops <- all.vars(formula(mod))[1]
  if (resops %in% NumVarNames) NumVarNames <- NumVarNames[-which(NumVarNames == resops)]
  if ("data" %in% plots) plots <- c(plots, NumVarNames) |> unique()
  
  if (length(plots) > 1) {
    par(mfrow = c(1, 2))
  } else {
    par(mfrow = c(1, 1))
  }
  for (i in 1:length(plots)) {
    if (plots[i] == "response") {
      scatter.smooth(fitted(mod), mod$y, lpars = list(col = gray(0.6)), main = NULL, xlab = "Predicted Values", ylab = "Response")
      abline(a = 0, b = 1, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste("Response vs Fitted"), font = 2)
    } else if (plots[i] == "fitted") {
      scatter.smooth(fitted(mod), rsd, degree = 2, lpars = list(col = gray(0.6)), main = NULL, xlab = "Predicted Values", ylab = "Residuals")
      abline(h = 0, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "Residuals vs Fitted"), font = 2)
    } else if (plots[i] == "qqplot") {
      qqnorm(rsd, main = "")
      qqline(rsd, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "QQ-plot"), font = 2)
    } else if (plots[i] %in% names(mod$model)) {
      scatter.smooth(mod$model[, plots[i]], rsd, degree = 2, lpars = list(col = gray(0.6)), xlab = plots[i], main = NULL, ylab = "Residuals")
      abline(h = 0, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "Residuals vs", plots[i]), font = 2)
    } else if (plots[i] %in% names(mod$data)) {
      scatter.smooth(mod$data[, plots[i]], rsd, degree = 2, xlab = plots[i], main = NULL, ylab = "Residuals")
      abline(h = 0, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "Residuals vs", plots[i]), font = 2)
    }
  }
  opar$mfrow <- c(1, 1)
  on.exit(par(opar))
}
