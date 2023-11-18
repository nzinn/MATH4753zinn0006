

#' 95% Confidence interval 
#'
#' `myci` creates a 95% confidence interval of the mean from a single sample
#'
#' @param x sample 
#' @export 
myci <- function(x, conf.level = .95) {

    alpha <- 1 - conf.level

    n <- length(x)
    interval <- mean(x) + c(-1, 1) * qt(1 - alpha / 2, n - 1) * sd(x) / sqrt(n)
    return (list(lowerbound = interval[1], upperbound = interval[2]))
}
