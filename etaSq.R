#### Function for Computing Eta Square ####
#' Compute eta-square from the output of ezANOVA
#' This function only works for the output from ezANOVA
#' 
#' @param output_ezANOVA The ANOVA results from ezANOVA
#' @param nFactor_within Number of within-subject factors
#' @param nFactor_between Number of between-subject factors
#' @return Eta-square of each main effect and interaction effect
#' 
etaSq_ez <- function(output_ezANOVA,nFactor_within,nFactor_between){
  # Input:
  #     output_ezANOVA
  
  # Number of effects in the ezANOVA table (including intercept)
  nEffect_raw <- dim(output_ezANOVA$ANOVA)[1] 
  
  # Between-subject factors
  if (nFactor_between!=0) {
    nInteract_between <- 0
    for (k in 2:nFactor_between){
      nInteract_between <- nInteract_between + choose(nFactor_between,k) # number of interaction effects of between-subject factors
    }
    nEffect_between <- nFactor_between + nInteract_between # number of main and interaction effects of between-subject factors
    rowNames_between <- paste(c(2:(1+nEffect_between))) # rowNames of between-subject effects in the ezANOVA table
    rowIndex_between <- match(rowNames_between,rownames(output_ezANOVA$ANOVA)) # row index of between-subject effects in the ezANOVA table
    ANOVAtable_between <- output_ezANOVA$ANOVA[rowIndex_between,] # extract between-subject effects only
    ssEffect_between <- sum(ANOVAtable_between$SSn[1:nEffect_between]) # sum of Sum of Square (SS) of all between variable
    ssError_between <- ANOVAtable_between$SSd[1] # SS of the error term for between variable
    ssTotal_between <- ssEffect_between+ssError_between # sum of total SS of between varialbe
  } else {
    nEffect_between <- 0
  }
  
  # Within-subject factors
  if (nFactor_within!=0) {
    rowNames_within <- paste( c((2+nEffect_between):nEffect_raw) ) # rowNames of within-subject effects in the ezANOVA table
    rowIndex_within <- match(rowNames_within,rownames(output_ezANOVA$ANOVA)) # row index of within-subject effects in the ezANOVA table
    ANOVAtable_within <- output_ezANOVA$ANOVA[rowIndex_within,] # extract within-subject effects only
    nEffect_within <- dim(ANOVAtable_within)[1] # number of within-subject effects
    ssEffect_within <- sum(ANOVAtable_within$SSn[1:nEffect_within]) # sum of Sum of Square (SS) of all within variables
    ssError_within <- ANOVAtable_within$SSd[1] # SS of the error term for within variable
    ssTotal_within <- ssEffect_within+ssError_within # sum of total SS of within variable
  } else {
    nEffect_within <- 0
  }
  
  # Create an array for filling eta-square values
  nEffect_total <- nEffect_between + nEffect_within  # number of all factors
  etaSq_array <- array(NA, dim = c(nEffect_total,1), 
                       dimnames = list(output_ezANOVA$ANOVA$Effect[2:nEffect_raw],
                                       "etaSquare")
  )
  # fill in between-subject varialbe eta-square
  if (nFactor_between!=0) {
    for (i in 1:nEffect_between){
      etaSq_array[rowIndex_between[i]-1,1] <- ANOVAtable_between$SSn[i]/ssTotal_between
    } 
  }
  # fill in within-subject varialbe eta-square
  if (nFactor_within!=0) {
    for (i in 1:nEffect_within){
      etaSq_array[rowIndex_within[i]-1,1] <- ANOVAtable_within$SSn[i]/ssTotal_within
    } 
  }
  
  return(etaSq_array)
  
}
  