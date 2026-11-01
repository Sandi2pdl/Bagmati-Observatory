raw <- read.csv("./data/raw/bagmati_raw_inventory.csv")

# Mean function create in global environment
mean <- function(x) {
   cal_mean <- sum(x, na.rm = TRUE) / length(x[!is.na(x)])
    return(cal_mean)
}
mean(raw$DBH_cm)

sum_sq_diff <- function(x) {
  sum((x - my_mean(x))^2, na.rm = TRUE)
}

my_sd <- function(x) {
  sqrt(sum_sq_diff(x) / (length(x[!is.na(x)]) - 1))
}

m <- my_mean(raw$DBH_cm)
s <- my_sd(raw$DBH_cm)

perc.cv <- function(x) {
    cv <- (m / s) * 100
    return(cv)
}

perc.cv(raw$DBH_cm)
