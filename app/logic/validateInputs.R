box::use(
  
)
#' @export
# Function to validate input formats
validate_inputs <- function(run, bds) {
  run_valid <- grepl("^\\d{6}_[A-Za-z0-9]+$", run)
  bds_valid <- grepl("^BDS-\\d{10}-\\d{2}$", bds)
  return(run_valid & bds_valid)
}

