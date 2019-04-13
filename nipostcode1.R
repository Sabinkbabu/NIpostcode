add_percent <- function(x, multiplier, no_digits)
{
  percent <- round(x * multiplier, digits = no_digits)
  result <- paste(percent, "%", sep = "")
  return(result)
}

sample_vector <- c(0.458, 1.66653, .083112)
add_percent(sample_vector, multiplier = 10, no_digits = 5)


my_stats <- function(values, parametric = TRUE, allow_print = FALSE){
  if (parametric) {
    central_tendancy <- mean(values) 
    spread <- sd(values)
  }
  else{
    central_tendancy <- median(values)
    spread <- mad(values)
  }
  if (allow_print & parametric){
    cat("Mean = ", central_tendancy, "\n", "SD = ", spread, "\n")
  }
  else if (allow_print & !parametric)
}