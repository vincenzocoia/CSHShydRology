#' Gumbel-transformed axes
#'
#' Transforms return period or annual exceedance probability to be spaced
#' according to a reduced Gumbel variate. Can be added as a scale layer to
#' a ggplot2 graphic.
#'
#' @param ... Arguments to pass to \code{scale_x_continuous} or
#' \code{scale_y_continuous} from \pkg{ggplot2}.
#' @return The same output as \code{scale_x_continuous} or
#' \code{scale_y_continuous}, but with the appropriate Gumbel spacing.
#' @rdname gumbel_spacing
#' @importFrom scales trans_new breaks_log
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   y = sort(stats::rexp(100), decreasing = TRUE),
#'   aep = 1:100 / 101
#' )
#'
#' ggplot(df, aes(aep, y)) +
#'   geom_point() +
#'   scale_x_gumbelAEP()
#'
#' ggplot(df, aes(1 / aep, y)) +
#'   geom_point() +
#'   scale_x_gumbelRP("Return Period")
#' @export
scale_x_gumbelRP <- function(...) {
  ggplot2::scale_x_continuous(..., trans = gumbelRP_trans())
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbelRP <- function(...) {
  ggplot2::scale_y_continuous(..., trans = gumbelRP_trans())
}

#' @rdname gumbel_spacing
#' @export
scale_x_gumbelAEP <- function(...) {
  ggplot2::scale_x_continuous(..., trans = gumbelAEP_trans())
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbelAEP <- function(...) {
  ggplot2::scale_y_continuous(..., trans = gumbelAEP_trans())
}

#' Gumbel transformations used for ggplot2 scales
#'
#' Build the transformation objects underlying the \code{scale_*_gumbel*()}
#' functions. \code{gumbelRP_trans()} works on return periods (values greater
#' than 1); \code{gumbelAEP_trans()} works on annual exceedance probabilities
#' (values between 0 and 1). Both map their input onto the reduced Gumbel
#' variate \eqn{-\log(-\log(1 - p))}.
#'
#' These are functions rather than stored objects so that the transformation is
#' built when it is used, against the installed version of \pkg{scales}, rather
#' than being fixed when the package is built.
#'
#' @return A \code{transform} object, as produced by
#' \code{\link[scales]{trans_new}}.
#' @rdname gumbel_trans
#' @examples
#' tr <- gumbelRP_trans()
#' tr$transform(c(2, 10, 100))
#'
#' tr_aep <- gumbelAEP_trans()
#' tr_aep$transform(c(0.5, 0.1, 0.01))
#' @export
gumbelRP_trans <- function() {
  scales::trans_new(
    "gumbelRP",
    transform = function(x) -log(-log(1 - 1 / x)),
    inverse = function(x) 1 / (1 - exp(-exp(-x))),
    breaks = scales::breaks_log(),
    domain = c(1, Inf)
  )
}

#' @rdname gumbel_trans
#' @export
gumbelAEP_trans <- function() {
  scales::trans_new(
    "gumbelAEP",
    transform = function(x) -log(-log(1 - x)),
    inverse = function(x) 1 - exp(-exp(-x)),
    breaks = scales::breaks_log(),
    domain = c(1e-100, 1)
  )
}
