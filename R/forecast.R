#' @export
forecast <- function(model , x , h = 1 , interval = FALSE, level =0.95 , nSim = 500) UseMethod("forecast")

#' @export
forecast.default <- function(model , x , h = 1 , interval = FALSE, level =0.95 , nSim = 500) {
  if (inherits(model, "ardlDlm")) {
    frc <- forecast.ardlDlm(model = model , x = x, h = h , interval = interval, level = level , nSim = nSim)
  } else if (inherits(model, "dlm")) {
    frc <- forecast.dlm(model = model , x = x, h = h , interval = interval, level = level , nSim = nSim)
  } else if (inherits(model, "koyckDlm")) {
    frc <- forecast.koyckDlm(model = model , x = x, h = h , interval = interval, level = level , nSim = nSim)
  } else if (inherits(model, "polyDlm")) {
    frc <- forecast.polyDlm(model = model , x = x, h = h , interval = interval, level = level , nSim = nSim)
  } else {
    stop("Unknown model class")
  }
}