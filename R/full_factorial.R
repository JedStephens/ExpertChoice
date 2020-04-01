#' Full Factorial Design
#'
#'
#' @param attributes_list A named list: giving the variable name and the levels as characters. The levels should start from the base of either "0" or "1" and go up in integer values.
#' @description Generates the full factorial design with all the factors coded using standardised orthogonal contrast coding.
#' @references Kuhfeld, W. F. Marketing Research Methods in SAS Experimental Design, Choice, Conjoint, and Graphical Techniques 2010.
#'
#' Jörg Suckut (https://stats.stackexchange.com/users/237455/j%c3%b6rg-suckut), How to calculate (standardized) orthogonal contrast coding in R?, URL (version: 2019-02-12): https://stats.stackexchange.com/q/392173
#'
#' @return a `data.frame` with the full factorial design and factors coded using standardised orthogonal contrast coding.
#'
#' @export
#'
#' @examples
#' # See step 1 of the Practical Introduction to ExpertChoice vignette.
#' attrshort  = list(condition = c("0", "1", "2"),
#' technical =c("0", "1", "2"),
#' provenance = c("0", "1"))
#' full_factorial(attrshort)

full_factorial <- function(attributes_list){
  base_level <- unique(unlist(purrr::map(attributes_list, function(x){min(as.integer(x))})))
  if(length(base_level) > 1){
    stop("There is an inconsistent choice of base level.\nStart all factors on either 0 or 1.")
  }
  n_levels <- purrr::map(attributes_list, function(x){length(x)})

  check_sequencing <- function(){
    pass <- TRUE
    for (l in 1:length(attributes_list)) {
      #print(base_level:{n_levels[[l]] - 1})
     pass <- pass & all(attributes_list[[l]] == base_level:{n_levels[[l]] - ifelse(base_level == 0, 1, 0)})
    }
    return(pass)
  }
  if(! check_sequencing()){
    stop("The levels do not increase in integer values.")
  }


  .as_orthogonal_factor <- function(x){
    x_factor <- as.factor(x)
    nlevels_fct  <- nlevels(x_factor)
    # orthogonal coding for x-level factors, see Kuhfeld 2010 (pg. 62, 73)
    EC <-cbind(c(rep(1, nlevels_fct)), stats::contr.sum(x_factor))
    stats::contrasts(x_factor) <- (far::orthonormalization(EC)*nlevels_fct^(1/2))[,(2:nlevels_fct)]
    return(x_factor)
  }

  #Create the full factorial design.
  full_factorial <-expand.grid(lapply(attributes_list, .as_orthogonal_factor))
  # Add on additional matrixes.

  # Add X_full
  attr(full_factorial, "X_full") <- stats::model.matrix(stats::as.formula(
    paste("", paste(attributes(full_factorial)$names, collapse=" + "), sep=" ~ ")), full_factorial)

  # Add attributes_list
  attr(full_factorial, "attributes_list") <- attributes_list

  # Add marker for standardised orthogonal contrast coding
  attr(full_factorial, "snd_orth_constrast") <- TRUE

  attr(full_factorial, "factor_base_level") <- base_level

  return(full_factorial)
}
