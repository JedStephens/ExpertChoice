#' Augment levels and B-matrix to Full Factorial Design.
#'
#' @param full_factorial a `data.table` generated from the `full_factorial` function.
#'
#' @description Augments the full factorial design with a column summarising the levels of that design. Importantly, it also adds the B-matrix as an attribute.
#' @references Street, D. J.; Burgess, L. & Louviere, J. J. Quick and easy choice sets: Constructing optimal and nearly optimal stated choice experiments International Journal of Research in Marketing, 2005 , 22 , 459 - 470
#' @return a `data.frame` with an additional column identifying the level and the B-matrix attribute.
#' @export
#'
#' @examples
#' # See Practical Introduction to ExpertChoice Vignette. Step 2.
#'
#' #Step 1
#' attrshort  = list(condition = c("0", "1", "2"),
#' technical =c("0", "1", "2"),
#' provenance = c("0", "1"))
#'
#' #Step 2! - the augment_levels function
#' #' # ff stands for "full fatorial"
#'  ff  <-  full_factorial(attrshort)
#'  af  <-  augment_levels(ff)
#' # af stands for "augmented factorial"
#' af
#' # Compare ff and af. - do not confuse them. They serve different purposes.

augment_levels <- function(full_factorial){
  if(attributes(full_factorial)$snd_orth_constrast != TRUE){
    stop(simpleError("The full factorial design must be generated using the `full_factorial` function"))
  }

  level_summary <- (apply(full_factorial, 1, paste0, collapse = ""))
  full_factorial$levels <- level_summary

  DWin <- function(x){diff(range(x)) < .Machine$double.eps ^ 0.5}
  # This function checks for equality among all elements of a single vector
  #https://stackoverflow.com/q/4752275/7837538

  # Add B matrix
  B <- t(attributes(full_factorial)$X_full)[-1,]
  colnames(B) <- level_summary
  # The theory is that B %*% t(B) should equal I. (Street et al., 2005, pg. 463)
  almostI <- tcrossprod(B, B)

  if(DWin(diag(almostI))){
    message("Applying B mat")
    prefactor <- diag(almostI)[1]

    attr(full_factorial, "B_mat") <- B * 1/(sqrt(prefactor))
    #print(B %*% t(B))
  }else{
    simpleWarning("The B %*% t(B) should equal I, but does not. Create GitHub issue!")
  }
  return(full_factorial)
}
