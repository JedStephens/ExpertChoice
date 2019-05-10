#' Fractional Factorial Design Efficiency
#'
#' @param formula A specification, in formula form, of the desired effects sought to be estimated.
#' @param searched_fractional_factorial a fractional factorial generated as the result of a `search_design`.
#' @references Kuhfeld, W. F. Marketing Research Methods in SAS Experimental Design, Choice, Conjoint, and Graphical Techniques 2010.
#'
#' @return
#' @export
#'
#' @examples
#' See tutorial for worked examples.
fractional_factorial_efficiency <- function(formula, searched_fractional_factorial){
  if(attributes(searched_fractional_factorial)$searched != TRUE){
    stop(simpleError("The input must be the result of a search from the full factorial"))
  }
  # Calculations (most follow Kuhfeld 2010, pg. 62, 63, 73, 74)
  # Assumption of standardise orthogonal constrast coding is ensured by this being a searched_fractional_factorial
  Nd  <- nrow(searched_fractional_factorial)
  X   <- model.matrix(formula, searched_fractional_factorial)
  tXX <- crossprod(X, X) # t(X) %*% X. (Thats the equivalent)
  p   <- nrow(tXX)
  invtXX  <- solve(tXX)
  invdiag <- (diag(invtXX))
  Lambda  <- eigen(invtXX, only.values = TRUE)$values
  # Therom: Let Q be an n × n matrix. The product of the n eigenvalues of Q is the same as the determinant of A.
  detLambda <- prod(Lambda)

  # Efficiency Ratings
  # Kuhfeld 2010, pg. 63
  A_eff  <- 1/(sum(Nd * invdiag / p)) * 100
  D_eff  <- 1/(detLambda^(1/p) * Nd)  * 100

  # Some user feedback.
  cat(paste("Your fractional factorial design has an A-efficiency of", A_eff,"\n",
            "Your fractional factorial design has a D-efficiency of", D_eff, "\n")
      )
  # Function output.
  list(X=X, information_mat = round(tXX,3), inv_cov_mat= round(invtXX,3),
       lamda_mat = Lambda, inv_diag = invdiag, A_eff = A_eff, D_eff = D_eff )

}
