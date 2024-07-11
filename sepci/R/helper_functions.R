
#' Obtain function of volume above a given height
#'
#' Obtain function of volume above a given height from a
#' profile function locally parametrized for a tree and this tree height.
#'
#' @param profile_equation a one-dimensional function that returns the radius of
#' the tree for a given height
#' @param tree_height Tree height
#'
#' @return a function that returns the volume of the tree crown
#' above a certain height
#'
#' @import purrr
#' @importFrom stats integrate
#'
#' @export
#'
#' @examples
#' profile_function <- function(x) {
#' if(x<3){return(0)}
#' if(x>5){return(0)}
#' return(1)
#' }
#' volume_function <- above_height_volume(profile_function, 5)
#' volume_function(0) # should be 2*pi
#' volume_function(4) # should be pi
above_height_volume <- function(profile_equation, tree_height = 100) {
  pe <- function(x) {
    unlist(map(x, function(x) profile_equation(x)))
  }

  return(function(x) {
    integrate(function(x) 2 * pi * x * pe(x),
              lower = x, upper = tree_height)$value
  })
}

#' Obtain function of crown projection area at a given height
#'
#' @param profile_equation a one-dimensional function that returns the radius of
#' the tree for a given height
#' @param hlcr Height of the largest crown radius
#'
#' @return a function that returns the crown projection area of the tree at a
#' given height
#'
#'
#' @export
#'
#' @examples
#' profile_function <- function(x) {
#' if(x<3){return(0)}
#' if(x>5){return(0)}
#' return(1)
#' }
#' area_function <- cparea_at_height(profile_function, hlcr = 3)
#' area_function(0) # should be 0
#' area_function(4) # should be pi
cparea_at_height <- function(profile_equation, hlcr) {

  result_function <- function(x) {
    if (x < hlcr) {
      x <- hlcr
    }
    return(pi * profile_equation(x) ^ 2)
  }

  return(result_function)

}

#' Obtain function of surface above a given height
#'
#' Obtain function of surface above a given height from a
#' profile function locally parametrized for a tree and this tree height.
#'
#' @param profile_equation a one-dimensional function that returns the radius of
#' the tree for a given height
#' @param hbc height at the base of the crown
#' @param tree_height Tree height
#'
#' @return a function that returns the surface of the tree crown
#' above a certain height
#'
#' @import numDeriv purrr
#' @importFrom stats integrate
#'
#' @export
#'
#' @examples
#' profile_function <- function(x) {
#' if(x<3){return(0)}
#' if(x>5){return(0)}
#' return(1)
#' }
#' surface_function <- above_height_surface(profile_function, 3, 5)
#' surface_function(0)
#' surface_function(4)
above_height_surface <- function(profile_equation, hbc, tree_height = 100) {
  pe <- function(x) {
    unlist(map(x, function(x) profile_equation(x)))
  }
  dpe <- function(x) grad(pe, x)
  return(function(x) {
    result <- 0
    if (x < hbc) {
      x <- hbc
    }
    result <- result +
      integrate(function(x) 2 * pi * pe(x) * sqrt(1 + dpe(x) ^ 2), lower = x,
                upper = tree_height)$value
    return(result)
  })
}
