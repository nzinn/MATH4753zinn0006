


#' Number of tickets
#'
#' `ntickets` calculates how many tickets should be sold to ovoid overbooking 
#'
#' This function takes the number of total seats, the desired probability of a booking, and the pobability of a "no-show", and provides a discrete and coninuous number of tickets that should be sold to avoid overbooking. It plots the objective function vs n as well, both continuous and discrete.
#'
#' @param N the number of seats
#' @param gamma the probability of an overbooking
#' @param p the probability of a "no-show"
#'
#' @returns A list containing nd and nc, the discrete and continuous number of tickets that should be sold, respectively, and N, gamma, and p.
#'
#' @export
ntickets <- function(N, gamma, p) {

    ## Discrete interval
    searchd = N:(2 * N)
    ## Continuous interval
    searchc = seq(N, 2 * N, 0.0001)
    ## Discrete value
    nd = searchd[which.min(abs(objd(gamma, N, searchd, p)))]
    ## Continuous value
    nc = searchc[which.min(abs(objc(gamma, N, searchc, p)))]

    ## Plot the discrete objective vs n
    plot(searchd, -1 * objd(gamma, N, searchd, p), xlim=c(N, N + N * .1), main=paste("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma = ", gamma, " N =", N, " discrete", sep=""), xlab="n", ylab="Objective")

    graphics::abline(h=0, col="red")
    graphics::abline(v=nd, col="red")

    ## Take care of warning
    x <- NULL 
    ## Plot the continuous
    graphics::curve(-1 * objc(gamma, N, x, p), from = N, to = N + N * .1, main=paste("Objective Vs n to find optimal tickets sold\n(", nc, ") gamma = ", gamma, " N =", N, " continuous", sep=""), xlab = "n", ylab="Objective")

    graphics::abline(h=0, col="red")
    graphics::abline(v=nc, col="red")
    
    ## Return values
    return (list(nc = nc, nd = nd, N = N, p = p, gamma = gamma))
}

## Discrete objective function
objd <- function(gamma, N, n, p) {
    return (stats::pbinom(N, n, p) + gamma  - 1)
}

## Continuous objuective function
objc <- function(gamma, N, n, p) {
    return (stats::pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p))) + gamma - 1)
}


