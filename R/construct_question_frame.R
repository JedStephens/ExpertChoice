#' Convert from choice_sets to a question data
#'
#' @param choice_sets The choice sets list generated from one of the methods. (See Step 6 of the tutorial)
#' @param augmented_full_factorial The augmented full factorial object.
#' @param randomise_choice_sets A binary variable indicating if the order of the choice sets should be randomised. Some methods create choice sets which have a systematic order. Randomising the order of the choice sets does not change the alternatives within the choice sets. It simply rearranges the choice_set object in a random manner.
#'
#' @return a data.frame object
#' @export
#'
#' @examples
#' #See Step 9 of Practical Introduction to ExpertChoice vignette.
#'
#' # Step 1
#' attrshort  = list(condition = c("0", "1", "2"),
#' technical =c("0", "1", "2"),
#' provenance = c("0", "1"))
#'
#' #Step 2
#' # ff stands for "full fatorial"
#'  ff  <-  full_factorial(attrshort)
#'  af  <-  augment_levels(ff)
#' # af stands for "augmented factorial"
#'
#' # Step 3
#' # Choose a design type: Federov or Orthogonal. Here an Orthogonal one is used.
#' nlevels <- unlist(purrr::map(ff, function(x){length(levels(x))}))
#' fractional_factorial <- DoE.base::oa.design(nlevels = nlevels, columns = "min34")
#'
#' # Step 4 & 5
#' # The functional draws out the rows from the original augmented full factorial design.
#' colnames(fractional_factorial) <- colnames(ff)
#' fractional <- search_design(ff, fractional_factorial)
#' # Step 5 (skipped, but important, see vignette)
#'
#' # Step 6
#' # Two modulators c(1,1,1) and c(0,1,1) are specified.
#' dce_modulo <- modulo_method(
#' fractional,
#' list(c(1,1,1),c(0,1,1))
#' )
#'
#' # Step 7 and Step 8 are very important for the design, but skipped here.
#'
#' # Step 9! -- Construct a question frame to use with your study.
#' # Note the use of af here.
#' questions <- construct_question_frame(af, dce_modulo)
#' levels(questions$condition) <- c("bad", "good", "excellent")
#' levels(questions$technical) <- c("poor", "fair", "skilled")
#' levels(questions$provenance) <- c("none", "present")
#' questions
construct_question_frame <- function(augmented_full_factorial, choice_sets, randomise_choice_sets = TRUE){
  m <- unique(unlist(purrr::map(choice_sets, function(x){length(x)})))
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
