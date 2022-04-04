format_number <- function(
  x, scale = NULL, suffix = "scale",
  dollar_sign = FALSE, accuracy = 0.1, negative_parens = TRUE) {
  
  stopifnot("x must be numeric" = is.numeric(x))
  
  dollar_sign <- ifelse(dollar_sign, "$", "")
  
  if (!is.null(scale)) {
    stopifnot("If specified, scale must be 'auto', 'k', 'm', or 'b'" =
                scale %in% c("auto", "k", "m", "b")
    )
    
    if (identical(scale, "auto")) {
      scale <- case_when(x >= 1000000000 ~ "b",
                         x >= 1000000 ~ "m",
                         TRUE ~ "k")
    }
    
    if (identical(suffix, "word")) {
      suffix <- switch(scale, m = " million", b = " billion", k = " thousand")
    } else if (identical(suffix, "none")) {
      suffix <- ""
    } else {
      suffix <- toupper(scale)
    }
    
    scale <- sapply(scale, switch, m = 1e-6, b = 1e-9, k = 1e-3)
    
    dollar(
      x, scale = scale, accuracy = accuracy, negative_parens = negative_parens,
      prefix = dollar_sign, suffix = suffix, big.mark = ",", sep = "")
    
  } else {
    
    dollar(
      x, accuracy = accuracy, negative_parens = negative_parens,
      prefix = dollar_sign, big.mark = ",", sep = "")
    
  }
}