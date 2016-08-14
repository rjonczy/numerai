submission <- function( pred, name ) {
  
  subm_dir <- "./submissions/"
  
  prediction <- sample
  prediction$probability <- pred
  
  write.table(prediction, file = paste0(subm_dir, paste0('subm-', name)), sep = ",", row.names = FALSE, col.names = TRUE)
}