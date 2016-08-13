validate <- function( fit, valid = "validate", model = "" ) {  
  
  fcast <- predict(fit, valid)[,2]
  
  predOb <- prediction(fcast, valid$target)
  auc <- performance(predOb, measure = "auc")
  
  return(auc)
}