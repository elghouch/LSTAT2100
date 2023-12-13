pr2 <- function(...) {
  list(...) |> sapply(\(glm) (1 - glm$deviance / glm$null.deviance) * 100)
}
