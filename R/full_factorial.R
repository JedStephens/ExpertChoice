#' Full Factorial Design
#'
#' @param attributes_list A named list: giving the variable name and the levels as characters. The levels should start from the base ("1") and go up in integer values.
#' @description Generates the full factorial design with all the factors coded using standardised orthogonal contrast coding.
#' @references Kuhfeld, W. F. Marketing Research Methods in SAS Experimental Design, Choice, Conjoint, and Graphical Techniques 2010.
#' Jörg Suckut (https://stats.stackexchange.com/users/237455/j%c3%b6rg-suckut), How to calculate (standardized) orthogonal contrast coding in R?, URL (version: 2019-02-12): https://stats.stackexchange.com/q/392173
#'
#' @return a `data.frame` with the full factorial design and factors coded using standardised orthogonal contrast coding.
#' @export
#'
#' @examples
#' attrshort  = list(condition = c("1", "2", "3"),
#' technical =c("1", "2", "3"),
#' provenance = c("1", "2"))
#' full_factorial(attrshort)

full_factorial <- function(attributes_list){
  as_orthogonal_factor <- function(x){
    x_factor <- as.factor(x)
    nlevels_fct  <- nlevels(x_factor)
    # orthogonal coding for x-level factors, see Kuhfeld 2010 (pg. 62, 73)
    EC <-cbind(c(rep(1, nlevels_fct)), contr.sum(x_factor))
    contrasts(x_factor) <- (far::orthonormalization(EC)*nlevels_fct^(1/2))[,(2:nlevels_fct)]
    return(x_factor)
  }
  #Create the full factorial design.
  full_factorial <-expand.grid(lapply(attributes_list, as_orthogonal_factor))
  # Add on additional matrixes.

  # Add X_full
  attr(full_factorial, "X_full") <- model.matrix(as.formula(
    paste("", paste(attributes(full_factorial)$names, collapse=" + "), sep=" ~ ")), full_factorial)

  # Add attributes_list
  attr(full_factorial, "attributes_list") <- attributes_list

  # Add marker for standardised orthogonal contrast coding
  attr(full_factorial, "snd_orth_constrast") <- TRUE

  return(full_factorial)
}
