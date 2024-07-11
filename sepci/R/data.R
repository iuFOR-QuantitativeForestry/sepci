#' Valdepoza marteloscope point process stand
#'
#' An adaptation of marteloscope data of Valdepoza (Palencia). The maximum crown
#' radius has being computed following
#' \insertCite{CONDES2005203;textual}{sepci}.
#'
#' @format ## `valdepoza`
#' An object of class "ppp" representing the point pattern of tree locations in
#' a simulated stand of 100 x 100 meters. Each tree is marked with:
#'
#' \describe{
#'   \item{dbh}{Diameter at breast height in meters}
#'   \item{height}{Tree height in meters}
#'   \item{heightStartCrown}{The height were the crown of the tree starts in
#'    meters}
#'   \item{heightLargestCrownRadius}{The height were the crown of the tree
#'   reach's its maximum radius in meters}
#'   \item{largestCrownRadius}{The maximum crown radius of the tree in meters}
#' }
#' @source Original marteloscope data can be found at
#' https://uvadoc.uva.es/handle/10324/46949.
"valdepoza"
