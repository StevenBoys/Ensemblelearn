#' Function that combines results from weak models in parallel
#'
#' @param multi_est - list containing multiple estimates from weak models
#' @param weights - nonegative vector that combines the multiple estimates
#'
#' @return outputs Comb_parallel(multi_est, weights)
#' @export
#'
#' @examples
#' multi_est <- list(1, 2, 3)
#' weights <- c(0.2, 0.4, 0.4)
#' Comb_parallel(multi_est, weights)
Comb_parallel <- function(multi_est, weights){
  # Normalize the weights
  weights <- weights/sum(weights)
  # Calculate the weighted sums of the multiple estimates from weak models
  comb_out <- multi_est[[1]] * weights[1]
  for(i in 2:length(multi_est)){
    comb_out <-  comb_out + multi_est[[i]] * weights[i]
  }
  # Return the parallel combined result
  return(comb_out)
}
