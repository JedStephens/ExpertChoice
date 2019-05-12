#' Stephens Pairing
#'
#' @param fractional_factorial A fractional factorial design.
#' @description Currently only returns a discrete choice experiment with two alternatives.
#' Does not allow one alternative to overshadow the other.
#' In the optimised object, each row is a choice set. (These are marked by the row names).
#' In the p1 column is the row from the fractional_factorial object which is matched with the p2 row from the fractional factorial_factorial.
#' (Hence they are "pairs".)
#'
#' @return Returns a list which includes the choice_set object.
#' @export
#'
#' @examples
#'

stephens_pairing <- function(fractional_factorial){
  # The first part of this method creates all the objects which are required to run the linear programme.
  # The .build_mat function is a helper function calculates for each row how if that row where matched with another row the value of overlaps, levels, etc.
  # See that that function for more detials.
 built <- .build_mat(fractional_factorial)
  # The output needs to be rearranged into a format that is standard for any linear programme.
  # Matrix of constraints given by con.
 con   <- rbind(matrix(c(built$out$exact, built$out$overshadow), byrow = TRUE, nrow = 2), t(built$con))
  # The sings relating the constraights to the RHS given by constraint_sign_vector
 constraint_sign_vector  <- c("==", "==", rep("==", ncol(built$con)))
  # Finally the constraint_rhs
 constraint_rhs          <- c(0,0, rep(1, ncol(built$con)))

 # Now the linear programme can be solved.
 LP <- Rglpk::Rglpk_solve_LP(obj = built$out$obj2_value, types = rep("B", nrow(built$out)), max = TRUE,
                            mat = con, dir = constraint_sign_vector, rhs = constraint_rhs)

 optimal_value <- LP$optimum
 optimised <- built$out[as.logical(LP$solution),]
 # The row.names are the rows of the built$out matrix which were selected.
 # It is more informative to the reader to have the row numbers relablled as the different choice sets.
 row.names(optimised) <- paste0("CS_", 1:nrow(optimised))
 #View(optimised)

 # Importantly, the partial factoiral design still has the rownames of its design row.
 # These need to be superceeded witht he the row numbers that relate to the P1 and P2 outputs.
 # That can be done by surplanting the row.names.
 # All depreciated since moving to tiblle...
 #row.names(fractional_factorial) <- c(1:nrow(fractional_factorial))
 #Break into the two groups.
 fractional_factorial[optimised$pp1,]
 fractional_factorial[optimised$pp2,]
 View(fractional_factorial[optimised$pp1,])

 # What these are are "column groups". The ROWS of each column group corrospond with one another.
 # We want then convernt these column groups into ROW groups i.e. the CHOICE SETS.
 # The function l2 obliges.
 choice_sets <- .l2(fractional_factorial, optimised$pp1, optimised$pp2)

 # Finally create a return.
 return(list(choice_sets = choice_sets, optimised_solution = list(optimised = optimised, optimal_value = optimal_value)))
}

# Helper Functions.
.build_mat <- function(design){
  design_rows <- nrow(design)
  obj_value   <- numeric(length = design_rows^2)
  levels_diff <- numeric(length = design_rows^2)
  exact       <- logical(length = design_rows^2)
  overshadow  <- logical(length = design_rows^2)
  pair_place1 <- integer(length = design_rows^2)
  pair_place2 <- integer(length = design_rows^2)

  for (R in 1:design_rows) {
    for (r in 1:design_rows) {
      base_row <- as.numeric(design[R,])
      comparative_row <- as.numeric(design[r,])
      comparison <- base_row - comparative_row
      p <- (R-1)*design_rows + r

      # Vectors
      pair_place1[p] <- R
      pair_place2[p] <- r
      exact[p]       <- all(comparison == 0)
      #Commented out is the original.
      # This would not exclude the base level.
      overshadow[p]  <- (sum(comparison > 0) == ncol(design)) | (sum(comparison < 0) == ncol(design))
      # IT looks like TYPE B does not work in some instances...
      # Type B design.
      #overshadow[p]  <- (sum(comparison >= 0) == (ncol(design) -1)) | (sum(comparison <= 0) == (ncol(design) -1))
      obj_value[p]   <- sum(comparison != 0)
      levels_diff[p] <- sum(abs(comparison))
    }
  }
  out <-(data.frame(pp1=pair_place1, pp2=pair_place2,
                    obj_value = obj_value, levels_diff = levels_diff,
                    obj2_value = obj_value - levels_diff,
                    exact = exact, overshadow = overshadow))

  con_mat <- matrix(FALSE, nrow = nrow(out), ncol = design_rows)
  for (i in 1:design_rows) {
    con_mat[,i]  <- (out$pp1 == i) | (out$pp2 == i)
  }

  return(list(out=out, con = con_mat))
}

.l2<- function(array, p1, p2){
  choice_sets <- vector("list", length = length(p1))
  for (s in 1:length(p1)) {

    choice_sets[[s]] <- c(paste0(((array[p1,])[s,]), collapse = ""),
                          paste0(((array[p2,])[s,]), collapse = ""))
  }
  return(choice_sets)
}
