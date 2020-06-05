fitfun <- function(x, max.train.year=2010, fun="gam", response.var="wild", covariate="flow"){
  subx <- subset(x, year<max.train.year)
  subx$resp <- subx[[response.var]]
  subx$cov <- subx[[covariate]]
  if(fun=="gam") fit <- mgcv::gam(resp ~ s(cov), data=subx)
  if(fun=="lm") fit <- stats::lm(resp ~ cov, data=subx)
  if(fun=="auto.arima") fit <- forecast::auto.arima(subx$resp)
  return(fit)
}