sam <- function(s, r) {
  acos(sum(s * r) / sqrt(sum(s^2) * sum(r^2)))
}

scm <- function(s, r) {
  1 - ((1 + cor(s, r)) / 2)
}

ecs <- function(s, r) {
  sqrt(sum((cumsum(s) - cumsum(r))^2))
}
