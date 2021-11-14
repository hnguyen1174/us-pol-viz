#' Fill discrete gradient
#'
#' @inheritParams ggplot2::continuous_scale
#'
#' @return
#' @export
scale_fill_discrete_gradient <- function(..., 
                                         colours, 
                                         bins = 5, 
                                         na.value = 'grey50', 
                                         guide = 'colourbar', 
                                         aesthetics = 'fill', 
                                         colors)  {
  
  if (missing(colours)) {
    colours <- colors
  }
  
  ggplot2::continuous_scale(
    aesthetics,
    'discrete_gradient',
    discrete_gradient_pal(colours, bins),
    na.value = na.value,
    guide = guide,
    ...
    )
  } 

#' Get number of decimal places
#'
#' @param x a number
#'
#' @return the number of decimal places
#' @export
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    0
  }
}

#' sect_x
#'
#' @param x a numeric vector
#'
#' @return
#' @export
sect_x <- function(x) {
  
  diff_x <- diff(x)[!is.na(diff(x))]
  diff_x <- round(diff_x, 2)
  max_dec <-  max(sapply(diff_x, decimalplaces))
  m_int <- 10^(-1*max_dec)
  x_div <- as.integer(round(diff_x / m_int, 0))
  x_div <- x_div[x_div != 0]
  max_divisor <- max(Reduce(intersect, lapply(x_div, numbers::divisors)))
  fac_div <- m_int * max_divisor
  fac_div
}

#' Get discrete gradient palette
#'
#' @param colours colors
#' @param bins bin to group numeric values into
#'
#' @return discrete gradient palette
#' @export 
discrete_gradient_pal <- function(colours, bins = 5) {
  ramp <- scales::colour_ramp(colours)
  
  function(x) {
    if (length(x) == 0) return(character())
    
    i <- floor(x * bins)
    i <- ifelse(i > bins-1, bins-1, i)
    ramp(i/(bins-1))
  }
}