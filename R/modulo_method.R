modulo_method <- function(fractional_fatorial, generators){
 # This is an implementation of the method described by Street et. al.
  maxs <- as.numeric(apply(fractional_fatorial, 2, max))

  if(length(generators) != length(maxs)){
    stop(simpleError("Ill defined generators"))
  }

  fractional_fatorial + generators


}
#https://stat.ethz.ch/pipermail/r-help/2003-May/033921.html
modulo_method(fractional_f4521_64, c(1,1,1,1,1,1))
