#' ZickeZacke example data
#'
#' An example data set with artificial preference test data.
#' The 'HoiHoiHoi' item contains 1 tie to have variance for
#' a simulation. Also, the 'HoiHoiHoi' item lacks two measurements for the
#' subject 'drei'.
#'
#' @format A data frame with 30 rows and 5 variables:
#' \describe{
#'   \item{subjectID}{animal or subject ID}
#'   \item{optionA}{First item presented to the subject}
#'   \item{optionB}{Second item presented to the subject}
#'   \item{quantityA}{amount of A; this can also be binary (0,1)}
#'   \item{quantityB}{amount of B; this can also be binary (0,1)}
#' }
#' @source \url{https://talbotsr.com/simsalRbim/}
"ZickeZacke"
