# performance::binned_residuals

function (model, term = NULL, n_bins = NULL, show_dots = NULL, 
          ci = 0.95, ci_type = c("exact", "gaussian", "boot"),
          residuals = c("deviance", "pearson", "response"),
          iterations = 1000, verbose = TRUE, 
          ...) 
{
  # Checks
  ci_type <- match.arg(ci_type)
  residuals <- match.arg(residuals)
  if (isFALSE(insight::model_info(model)$is_bernoulli)) {
    ci_type <- "gaussian"
    if (verbose) {
      insight::format_alert("Using `ci_type = \"gaussian\"` because model is not bernoulli.")
    }
  }
  # Predictions
  fitted_values <- stats::fitted(model)
  mf <- insight::get_data(model, verbose = FALSE)
  if (is.null(term)) {
    pred <- fitted_values
  }
  else {
    pred <- mf[[term]]
  }
  if (is.null(show_dots)) {
    n <- .safe(insight::n_obs(model))
    show_dots <- is.null(n) || n <= 1e+05
  }
  y0 <- .recode_to_zero(insight::get_response(model, verbose = FALSE)) # observations
  y <- switch(residuals,
              response = y0 - fitted_values,
              pearson = .safe((y0 - fitted_values)/sqrt(fitted_values * (1 - fitted_values))), 
              deviance = .safe(stats::residuals(model, type = "deviance")))
  if (is.null(y)) {
    insight::format_error("Could not calculate residuals. Try using `residuals = \"response\"`.")
  }
  
  # Bins number
  if (is.null(n_bins)) 
    n_bins <- round(sqrt(length(pred)))
  breaks.index <- floor(length(pred) * (1:(n_bins - 1))/n_bins)
  breaks <- unique(c(-Inf, sort(pred)[breaks.index], Inf))
  model.binned <- as.numeric(cut(pred, breaks))
  d <- suppressWarnings(lapply(1:n_bins, function(.x) {
    items <- (seq_along(pred))[model.binned == .x]
    model.range <- range(pred[items], na.rm = TRUE)
    xbar <- mean(pred[items], na.rm = TRUE)
    ybar <- mean(y[items], na.rm = TRUE)
    n <- length(items)
    sdev <- stats::sd(y[items], na.rm = TRUE)
    if (n == 0) {
      conf_int <- stats::setNames(c(NA, NA), c("CI_low", 
                                               "CI_high"))
    }
    else {
      conf_int <- switch(ci_type, gaussian = stats::qnorm(c((1 - 
                                                               ci)/2, (1 + ci)/2), mean = ybar, sd = sdev/sqrt(n)), 
                         exact = {
                           out <- stats::binom.test(sum(y0[items]), n)$conf.int
                           out <- out - (min(out) - ybar) - (diff(out)/2)
                           out
                         }, boot = .boot_binned_ci(y[items], ci, iterations))
      names(conf_int) <- c("CI_low", "CI_high")
    }
    d0 <- data.frame(xbar = xbar, ybar = ybar, n = n, x.lo = model.range[1], 
                     x.hi = model.range[2], se = stats::qnorm((1 + ci)/2) * 
                       sdev/sqrt(n))
    cbind(d0, rbind(conf_int))
  }))
  d <- do.call(rbind, d)
  d <- d[stats::complete.cases(d), ]
  gr <- abs(d$ybar) > abs(d$se)
  d$group <- "yes"
  d$group[gr] <- "no"
  resid_ok <- sum(d$group == "yes")/length(d$group)
  class(d) <- c("binned_residuals", "see_binned_residuals", 
                class(d))
  attr(d, "resid_ok") <- resid_ok
  attr(d, "resp_var") <- insight::find_response(model)
  attr(d, "term") <- term
  attr(d, "show_dots") <- show_dots
  d
}
