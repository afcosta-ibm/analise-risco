#'  Simulates a game of heads or tails.
#'  
#'  @name throw.coin 
#' 
#'  @aliases throw_coin throwCoin
#'  
#'  @param coins Number of coins
#'  @param pitches Number of coin launches
#'   
#'  @description Heads or tails is the practice of throwing a coin in the air to choose between two alternatives. It is a form of sortition which inherently has only two possible and equally likely outcomes. 
#'  @description This function allows you to simulate the launching of a virtual coin. This function use an algorithm pseudo-random for generate the numbers 0 or 1. The number 0 represent the tails and the number 1 represent the heads.
#'  
#'  @export
#'  
#'  @return The frequency of occurrences of heads  
#'  
#'  @keywords 
#'
#'  @family risk
#'
#'  @examples
#'
#' x <- throw.coin(1, 10)
#' x <- throw_coin(1, 100)
#' x <- throwCoin(1, 1000)

throw.coin <- throw_coin <- throwCoin <- function(coins, pitches) {
  sum(sample(c(0,1), pitches, replace=TRUE, prob=rep(0.5, times=2)))/pitches
}