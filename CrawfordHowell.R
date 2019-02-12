# Crawford-Howell t-test for single case studies
#' 
#' @param case Data of a single case subject 
#' @param control Data of a group of subjects for comparison 
#' 
CrawfordHowell <- function(case, control){
  tval <- (case - mean(control)) / (sd(control)*sqrt((length(control)+1) / length(control)))
  degfree <- length(control)-1
  pval <- 2*(1-pt(abs(tval), df=degfree)) #two-tailed p-value
  result <- data.frame(t = tval, df = degfree, p=pval)
  return(result)
}
