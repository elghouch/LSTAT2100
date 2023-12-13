if (!require(statmod)) install.packages(statmod)
diagnost <- function(mod, type = "rqr", plots = c("response", "fitted", "qqplot", "FormulaVar", "DataVar")) {
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
  NumVarNames <- colnames(Filter(is.numeric, mod$data))
  if ("FormulaVar" %in% plots) plots <- c(plots, attr(terms(mod), "term.labels")) |> unique()
  if ("DataVar" %in% plots) plots <- c(plots, NumVarNames[-which(NumVarNames == all.vars(formula(mod))[1])]) |> unique()
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
      scatter.smooth(fitted(mod), rsd, lpars = list(col = gray(0.6)), main = NULL, xlab = "Predicted Values", ylab = "Residuals")
      abline(h = 0, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "Residuals vs Fitted"), font = 2)
    } else if (plots[i] == "qqplot") {
      qqnorm(rsd, main = "")
      qqline(rsd, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "QQ-plot"), font = 2)
    } else if (plots[i] %in% names(mod$model)) {
      scatter.smooth(mod$model[, plots[i]], rsd, lpars = list(col = gray(0.6)), xlab = plots[i], main = NULL, ylab = "Residuals")
      abline(h = 0, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "Residuals vs", plots[i]), font = 2)
    } else if (plots[i] %in% names(mod$data)) {
      scatter.smooth(mod$data[, plots[i]], rsd, xlab = plots[i], main = NULL, ylab = "Residuals")
      abline(h = 0, col = "blue")
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, paste(type, ":", "Residuals vs", plots[i]), font = 2)
    }
  }
  opar$mfrow <- c(1, 1)
  on.exit(par(opar))
}
