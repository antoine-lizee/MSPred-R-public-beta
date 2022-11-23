## Logging method for R scripts
# This uses calling handlers for warnings, and exiting handlers for the finally.
#
# Copyright Antoine Lizee 02/2015 antoine.lizee@gmail.com

#' @family Helpers
executeWithLog <- function(logFileName, expr) {
  sink(file = logFileName,  split = T)
  tryCatch({ # needed for the finally exiting handler
    withCallingHandlers(expr, 
                        error = function(e) {
                          cat("### ERROR:\n", e$message, "\n####\n")
                          stop(e)
                        }, 
                        warning = function(w) {
                          cat("### WARNING:\n", w$message, "\n####\n")
                        })},
    finally = {
      cat("### CLOSING SINK... ###\n")
      sink()
    })
}


# test --------------------------------------------------------------------

if (test.B <- F) {
  executeWithLog({
    print(2+2)
    warning("YO")
    print(5+5)
    stop("DAMN!")
    4+4
  })
}