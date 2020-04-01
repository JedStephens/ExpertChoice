#' Modulo Method - Described by Street et al.
#'
#' @param fractional_fatorial The usual.
#' @param generators a list of generators
#'
#' @return a choiceset list.
#' @export
#'
#' @examples
#' # See step 6 of the Practical Introduction to ExpertChoice vignette.
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
#' # Step 4
#' # The functional draws out the rows from the original augmented full factorial design.
#' colnames(fractional_factorial) <- colnames(ff)
#' fractional <- search_design(ff, fractional_factorial)
#' # Step 5 - Skipped, but important, see vignette.
#'
#' # Step 6! -- The modulo_method function
#' # Two modulators c(1,1,1) and c(0,1,1) are specified.
#' dce_modulo <- modulo_method(
#' fractional,
#' list(c(1,1,1),c(0,1,1))
#' )
#' dce_modulo
modulo_method <- function(fractional_fatorial, generators){
 # This is an implementation of the method described by Street et. al.
  attributes_list <- attributes(fractional_fatorial)$attributes_list
  maxs <- as.numeric(unlist(lapply(attributes_list, max)))
  mins <- as.numeric(unlist(lapply(attributes_list, min)))

  if(!is.list(generators)){
    stop(simpleError("The generators must be a list, even if it is a list with only one element"))
  }
  number_of_generators <- length(generators)
  choice_sets <- vector("list", 1 + number_of_generators)
  choice_sets[[1]] <- apply(fractional_fatorial, 1, paste0, collapse = "")

  for (g in 1:number_of_generators) {
    if(length(generators[[g]]) != length(maxs)){
      stop(simpleError("Ill defined generators"))
    }
    adjusted_fractional_fatorial <- fractional_fatorial
    # Will perform this column wise...
    for (j in 1:ncol(fractional_fatorial)) {
      adjusted_fractional_fatorial[,j]  <- .factor_modulo(as.numeric(fractional_fatorial[[j]]) + (generators[[g]])[j], maxs[j], mins[j])
    }
    choice_sets[[g + 1]] <- apply(adjusted_fractional_fatorial, 1, paste0, collapse = "")
  }
  # Create a matrix by binding together list elements.

  choice_sets_bound <- rlist::list.cbind(choice_sets)
  #return(choice_sets_bound)

  # Convert into choice sets list.
  choice_sets <- vector("list", length = nrow(fractional_fatorial))
  for (s in 1:length(choice_sets)) {
    choice_sets[[s]] <- choice_sets_bound[s,]
  }
  class(choice_sets) <- c(class(choice_sets), "choice_sets")
  m <- unique(unlist(purrr::map(choice_sets, function(x){length(x)})))
  attr(choice_sets, "m") <- m
  return(choice_sets)
}

.factor_modulo <- function(vector, max_level, min_level){
  if(min_level == 0){
    min_0 <- TRUE
  }else{
    min_0 <- FALSE
    vector    <- vector - min_level
    max_level <- max_level - min_level
  }
  # Note that the comparison should always be on max_level + 1.
  result <- vector %% (max_level + 1)

  if(!min_0){
    result <- result + min_level
  }
  return(result)
}

#https://stat.ethz.ch/pipermail/r-help/2003-May/033921.html
#modulo_method(fractional_f4521_64, list(c(1,1,1,1,1,1), c(1,2,2,1,3,4))) -> learning
