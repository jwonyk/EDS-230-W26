#' Computation of annual average solar radiation
#' Following is the function using photovoltaic system to measure the annual average solar radiation using the following equation:
#' 
#' Energy = A x r x H x PR
#' 
#' @param A Numeric. Solar panel area is square meters (m^2).
#' @param r Numeric. Panel yield (from 0–1) representing manufacturer efficiency.
#' @param H Numeric. Annual average solar radiation (kWh m^2).
#' @param PR Numeric. Performance ratio (from 0-1) accounting for site-specific losses. Typically ~0.75.
#'
#' @returns Numeric. Annual energy solar radiation in kilowatt-hours (kWh).
#'
#' @examples
#' PV system in regular house (using author's house)
#' ps_energy(A = 25, 
#'           r = 0.20, 
#'           H = 2500, 
#'           PR = 0.75)
#' 
#' @export
ps_energy <- function(A, r, H, PR) {
  
  # Basic computation validations
  if(any(A <= 0)) stop("A is an area that only takes positive input. Try again.")
  if(any(r <= 0 | r > 1)) stop("r is a panel yield and must a number between 0 - 1. Try again.")
  if(any(PR <= 0 | PR > 1)) stop("PR is a performance ratio and must a number between 0 - 1. Try again.")
  if(any()) stop("H is an annual average solar radiation that only takes positive input. Try again.")
  
  # Energy produced by solar panel system
  energy <- A * r * H * PR
  
  # Return the computation
  return(energy)
  
}