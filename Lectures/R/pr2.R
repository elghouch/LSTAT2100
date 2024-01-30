# ... : the name of one or more glm models for which Psudo-R2 is to be calculated
pr2 <- function(...) {
  list(...) |> sapply(\(glm) (1 - glm$deviance / glm$null.deviance) * 100)
}
