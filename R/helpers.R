#' Get first word from a string
#'
#' @param x a string with spaces between words
#' @return first word from string of words
#' @keywords internal

get_first_word <- function(x)
{
  x.split <- strsplit(x, split = " ")
  x1.list <- lapply(x.split,`[`, 1) # returns NA if no name
  x1 <- unlist(x1.list)
  return(x1)
}

#' Clean matching keys
#'
#' @param key a string that acts as a key that may have punctuation. Note that it retains underscores, which are used in the keys.
#' @return a string with no punctuation
#' @keywords internal


## function to clean keys
clean_key <- function(key){
  return(gsub(" +|(?!_)[[:punct:]]","", key, perl = T))
}
