#' Fractional Factorial Design Efficiency
#'
#' @param formula A specification, in formula form, of the desired effects sought to be estimated.
#' @param searched_fractional_factorial a fractional factorial generated as the result of a `search_design`.
#' @references Kuhfeld, W. F. Marketing Research Methods in SAS Experimental Design, Choice, Conjoint, and Graphical Techniques 2010.
#'
#' @return a list with the following objects:
#' 1. X - This is the formula expanded version of the fractional factorial which was passed to the function.
#' 2. information_mat - This is the information matrix described by the associated note. Note: it is rounded to three decimal places to ease reading.
#' 3. inv_information_mat - This is the inverse of the information matrix. Note: it is rounded to three decimal places to ease reading.
#' 4. lamda_mat - This is the diagonal elements of the Lamda Matrix described by Kuhfeld (pg. 62). The elements are the eigen values of the inv_information_mat.
#' 5. inv_diag - This is the diagonal elements of the inv_information_mat. (May be of use to some researchers...)
#' 6. GWLP - This is the generalised world lengths for the searched design. (Note: this would not change depending on what is in the formula expansion.)
#' 7. A_eff - This is the A-efficiency of the design given the particular formula expansion.
#' 8. D_eff - This is the D-efficiency of the design given the particular formula expansion.
#'
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

  # Attempts at inversing tXX.
  # First perform the QR decomposition on X.
  # Then take only the R section of the QR decomposition
  # Finally use the Inverse from QR/Choleski Decomposition.
  # See: https://stat.ethz.ch/pipermail/r-help/2002-June/022561.html
  # Also see ?chol2inv() and ?qr() and ?qr.R()

  invtXX  <- chol2inv(qr.R(qr(X))) # This generates the equilvant of (X'X)^(-1).
  #invtXX  <- solve(tXX)

  invdiag <- (diag(invtXX))
  Lambda  <- eigen(invtXX, only.values = TRUE)$values
  # Therom: Let Q be an n × n matrix. The product of the n eigenvalues of Q is the same as the determinant of A.
  detLambda <- prod(Lambda)

  # Efficiency Ratings
  # Kuhfeld 2010, pg. 63
  A_eff  <- 1/(sum(Nd * invdiag / p)) * 100
  D_eff  <- 1/(detLambda^(1/p) * Nd)  * 100
  # Also add the generalised word lengths for good measure.
  gwlp   <- DoE.base::GWLP(searched_fractional_factorial)

  # Some user feedback.
  cat("Your fractional factorial design has an A-efficiency of", round(A_eff,3),"%\n",
      "Your fractional factorial design has a D-efficiency of", round(D_eff,3), "%\n"
      )
  # Function output.
  list(X=X, information_mat = round(tXX,3), inv_information_mat= round(invtXX,3),
       lamda_mat = Lambda, inv_diag = invdiag, GWLP = gwlp, A_eff = A_eff, D_eff = D_eff )

}
