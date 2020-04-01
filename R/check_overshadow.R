#' Check Overshadow - Pareto Dominate Solutions
#'
#' @param choice_sets An object of the choiceset class made by one of the DCE methods.
#'
#' @return A matrix of logicals indicating which if any card for a given row is Pareto dominate.
#' @export
#'
#' @examples #See Step 7 of the Practical Introduction to ExpertChoice Vignette.
#' # Step 1
#' attrshort  = list(condition = c("0", "1", "2"),
#' technical =c("0", "1", "2"),
#' provenance = c("0", "1"))
#'
#' #Step 2
#' #' # ff stands for "full fatorial"
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
#' # Step 5 - Skipped, but important, see vignette.
#'
#' # Step 6
#' # Two modulators c(1,1,1) and c(0,1,1) are specified.
#' dce_modulo <- modulo_method(
#' fractional,
#' list(c(1,1,1),c(0,1,1))
#' )
#'
#' # Step 7! -- Check for Pareto dominate solutions
#'check_overshadow(dce_modulo)
check_overshadow <- function(choice_sets) {
  if(!("choice_sets" %in% class(choice_sets))){
    # Should the input object not have the choice_sets class then stop.
    stop("Input is not of class choice_sets. Generate the input using one of the methods.")
  }
  pareto_matrix <- matrix(logical(), nrow = length(choice_sets), ncol = 2)
  for (c in 1:ncol(pareto_matrix)) {
    pareto <- logical(length(choice_sets))
    for (l in 1:length(choice_sets)) {
      sublist_questions <- strsplit(choice_sets[[l]], "")
      sublist_questions_main <- as.numeric(sublist_questions[[c]])

      sub_pareto <- logical(length(sublist_questions))
      for (sl in (c + 1):length(sublist_questions)) {
        underorequal <- (sublist_questions_main <= as.numeric(sublist_questions[[sl]]))
        overoreuqal <- (sublist_questions_main >= as.numeric(sublist_questions[[sl]]))
        # print(underorequal)
        # print(overoreuqal)
        sub_pareto[sl] <- all(underorequal) | all(overoreuqal)
        # print(sub_pareto)
      }
      pareto[l] <- any(sub_pareto)
      # print(pareto[l])
    }
    pareto_matrix[, c] <- pareto
  }
  # return(apply(pareto_matrix, 1, all))
  return(pareto_matrix)
}
