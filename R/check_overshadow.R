#' Check Overshadow
#'
#' @param choice_sets An object of the choiceset class made by one of the DCE methods.
#'
#' @return A matrix of logicals indicating which if any card for a given row is Pareto optimal.
#' @export
#'
#' @examples See step 7 of the practical vingette
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
