#'
#' @title Psedurandom Number Generator using the middle square method
#'
#' @description
#' Implements John von Neumann’s *Middle Square Method* to generate a sequence
#' of pseudorandom numbers. In each iteration, the seed is squared, the middle
#' digits are extracted, and the result is normalized by dividing by \eqn{10^{d}},
#' where \eqn{d} is the number of digits in the original seed.
#'
#' @param seed Integer initial value (the seed) that determines the starting
#' point of the sequence. It should have between 2 and 8 digits.
#' @param n Integer. Number of pseudorandom values to generate.
#'
#' @details
#' The Middle Square Method is one of the earliest pseudorandom number
#' generation algorithms. Although simple, it often produces short or cyclic
#' sequences and should mainly be used for educational or demonstration purposes.
#'
#' At each step, the algorithm:
#' 1. Squares the current seed.
#' 2. Extracts the middle digits (same number of digits as the original seed).
#' 3. Normalizes the result by dividing by \eqn{10^{d}}.
#' 4. Uses this value as the new seed for the next iteration.
#'
#' @return
#' A numeric vector of length \code{n} containing the generated pseudorandom values.
#'
#' @examples
#' # Generate 4 pseudorandom numbers from seed 4097
#' Squaremean(4097, 4)
#'
#' # Try with other seeds
#' Squaremean(1324, 4)
#' Squaremean(6500, 4)
#'
#' @export
Squaremean <- function(seed, n) {
  dig <- nchar(seed)
  replicate(n, {
    square <- seed^2
    xi <- format(square, scientific = FALSE)
    xi <- paste0(strrep("0", 2 * dig - nchar(xi)), xi)
    start <- floor(dig / 2) + 1
    end <- start + dig - 1
    middle <- substr(xi, start, end)
    ri <- as.numeric(middle) / 10^dig
    seed <<- as.numeric(middle)
    ri
  })
}
