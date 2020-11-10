# NOTE: no bonus

# compute means of all numeric columns in df
# output: a data.frame
col_means <- function(df, na.rm = FALSE) {
  
  checkmate::assert(
    checkmate::check_data_frame(df),
    checkmate::check_numeric(df, min.len = 1),
    checkmate::check_matrix(df),
    checkmate::check_list(df, min.len = 1) 
  )
  checkmate::assert_flag(na.rm) # 'na.rm' should be a logical flag
   
  if (test_atomic_vector(df))
    return(mean(df, na.rm = na.rm))
  else
    warning("Input has been converted into a data.frame")
    df <- as.data.frame(df)
  
  
  if (nrow(df) == 0) {
    warning("Input has 0 rows -> return empty data.frame.")
    return(data.frame())
  }
  if (ncol(df) == 0) {
    warning("Input has 0 columns -> return empty data.frame.")
    return(data.frame())
  }
  
  numeric <- vapply(df, is.numeric, logical(1))
    
  if (sum(numeric) == 0){
    warning("Any numeric columns in data.frame -> return empty data.frame.")
    return(data.frame())
  }
    
  # drop = FALSE -> data.frame even if one column
  numeric_cols <- df[, numeric, drop = FALSE] 
  
  data.frame(lapply(numeric_cols, mean, na.rm = na.rm))
}

testthat::test_file("test-defensive-colmeans.R")
