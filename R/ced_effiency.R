

# STEP SEVEN.
# Diagnositics about the DCE design.

Lambda_matrix <- function(augmented_factorial, set_list, m){
  # Street et al., 2005, pg. 462
  profiles <- augmented_factorial$levels
  lamda <- base::matrix(data = integer(), nrow = length(profiles), ncol = length(profiles))
  colnames(lamda) <- profiles
  rownames(lamda) <- profiles

  check_profiles <- function(x, p){
    pair1 <- profiles %in% x
    pair2 <- pair1[p]
    pair_present <- pair1 * pair2
    return(pair_present)
  }
  for (r in 1:nrow(lamda)) {
    set_presence <- lapply(set_list, check_profiles, p = r)
    #return(set_presence)
    lamda_row_presence <- colSums(matrix(unlist(set_presence), byrow = TRUE,
                                         nrow = length(set_list), ncol = length(profiles)))
    #return(lamda_row_presence)
    lamda[r,] <- lamda_row_presence
  }
  diag(lamda) <- 0
  lamda <- -lamda
  diag(lamda) <- -rowSums(lamda)

  return(list(mat = lamda, prefactor = 1/((m^2)*(length(set_list)))))
}

C_matrix <- function(lamda_mat, augmented_factorial){
  #attributes(augmented_factorial)$B_mat %*% (lamda_mat$prefactor * lamda_mat$mat) %*% t(attributes(augmented_factorial)$B_mat)
  tcrossprod(attributes(augmented_factorial)$B_mat %*% (lamda_mat$prefactor * lamda_mat$mat), attributes(augmented_factorial)$B_mat)
}

.is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


detC_optimal <- function(levels_vector, m){
  # <- attributes(augmented_full_factorial)$out.attrs$dim
  k <- length(levels_vector)
  # A function.
  determine_s <- function(q,m,l){
    #return
    if((((l == 2) & (m %% 2 != 0)))){
      #CASE 1
      print("Case 1")
      return((m^2 -1)/4)

    }else if(((l == 2) & (m %% 2 == 0))){
      #CASE 2
      print("Case 2")
      return((m^2) / 4)

    }else if(((l > 2) & (l <= m))){
      #CASE 3
      print("Case 3")
      # First determine y and then x.
      for (y in 0:(l-1)) {
        x <- (m - y)/l # See page 463 of Street, Burgess, Louviere.
        # If x is a whole number then we have a winner!
        if(.is.wholenumber(x)){
          print(paste("The implied x is", x, "and y is", y))
          return((m^2 - (l*x^2 + 2*x*y + y))/2)
        }
      }
    }else if((l >= m)){
      # Case 4
      print("Case 4")
      return((m*(m-1))/2)
    }
  }

  detC_theroy <- 1
  for (q in 1:k) {
    l <- levels_vector[q]
    cat(paste("q is", q, "\n"))
    print(paste("L is", l))
    s <- determine_s(q, m, l)
    print(paste("s is", s))

    notQ <- (1:k)[-q]
    bracket <- (2*s) / (m^2 * (l - 1) * prod(levels_vector[notQ]))
    detC_theroy <- detC_theroy*((bracket)^(l-1))
  }
  names(detC_theroy) <- "upper_bound_detC"
  return(detC_theroy)
}


ced_effiency <- function(C, C_optimal, full_factorial){
  p <- ncol(attributes(full_factorial)$X_full)-1
  #cat(paste("p is", p))
  ced <- ((det(C))/C_optimal)^(1/p) * 100
  names(ced) <- "ced_effiency"
  return(ced)
}
