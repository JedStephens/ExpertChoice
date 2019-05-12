#' Convert from choice_sets to a question dataugmented_full_factorialrame
#'
#' @param choice_sets The choice sets list generated from one of the methods. (See Step 6 of the tutorial)
#' @param augmented_full_factorial The augmented full factorial object.
#' @param m The number of alternatives in each choice set
#' @param randomise_choice_sets A binary variable indicating if the order of the choice sets should be randomised. Some methods create choice sets which have a systematic order. Randomising the order of the choice sets does not change the alternatives within the choice sets. It simply rearranges the choice_set object in a random manner.
#'
#' @return a data.frame object
#' @export
#'
#' @examples
#' See Step 8 of tutorial
construct_question_frame <- function(augmented_full_factorial, choice_sets, m, randomise_choice_sets = TRUE){
  number_of_questions <- length(choice_sets)
  if(randomise_choice_sets){
    choice_sets <- rlist::list.sample(choice_sets, number_of_questions)
  }
  tidy_questions <- dplyr::tibble(question = rep(1:number_of_questions, each = m),
                                  choice = rep(1:m, number_of_questions),
                                  levels = unlist(choice_sets))
  tidy_questions <- dplyr::left_join(tidy_questions, augmented_full_factorial, by = "levels")
  return(tidy_questions)
}
