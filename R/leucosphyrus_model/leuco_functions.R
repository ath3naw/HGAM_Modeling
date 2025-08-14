# fix missing values/cleaning data for correct coords
fix_missing <- function(x) {
  x <- ifelse(!grepl("°", x) & !is.na(x), paste0("0°", x), x)
  x <- ifelse(!grepl("'", x) & !grepl("\"", x) & !is.na(x), sub("°", "°0'0\"", x), x)
  x <- ifelse(!grepl("'", x) & grepl("\"", x) & !is.na(x), sub("°", "°0'", x), x)
  x <- ifelse(!grepl("\"", x) & grepl("'", x) & !is.na(x), sub("'", "'0\"", x), x)
  x <- trimws(x)
  x
}

# take the average of coordinates with a range
average_coords <- function(x) {
  if(is.na(x)) return(NA_real_)
  if(grepl("-", x)){
    parts <- strsplit(x, "-")[[1]]
    part1 <- fix_missing(parts[1])
    part2 <- fix_missing(parts[2])
    part1 <- as.numeric(char2dms(part1, chd = chd, chm = chm, chs = chs))
    part2 <- as.numeric(char2dms(part2, chd = chd, chm = chm, chs = chs))
    return(mean(c(part1, part2)))
  }else{
    x <- fix_missing(x)
    return(as.numeric(char2dms(x, chd = chd, chm = chm, chs = chs)))
  }
}

# cleaning the coordinates for average within a range
clean_coords <- function(x){
  if(grepl("/", x)){
    parts <- strsplit(x, "/")[[1]]
    part1 <- parts[1]
    part2 <- parts[2]
    part1 <- as.numeric(part1)
    part2 <- as.numeric(part2)
    return(mean(c(part1, part2)))
  }else{
    return(as.numeric(x))
  }
}

# predict function
sdm_predict <- function(
    model,
    covariates,
    type = NULL,
    layer_name = "predicted_distribution"
){
  
  if(is.null(type)){
    if (inherits(model, "maxnet")) {
      type <- "logistic"
    } else {
      type <- "response"
    }
  }
  
  prediction <- predict(covariates, model, na.rm = TRUE, type = type)
  names(prediction) <- layer_name
  
  return(prediction)
}

# splits and cleans Collection methods
split_collection_methods <- function(sample_methods) {
  # Replace ' and ' with ',' for consistent splitting
  cleaned <- gsub(" and ", ",", sample_methods)
  # Split by ',')
  split_methods <- strsplit(cleaned, ",")
  # Trim whitespace
  lapply(split_methods, function(x) trimws(x))
}

# creates data frame with method1, method2, etc. headings to document sampling methods
get_methods_df <- function(sample_methods) {
  methods_list <- split_collection_methods(sample_methods)
  max_methods <- max(sapply(methods_list, length))
  # Pad each vector to max_methods with NA
  padded <- lapply(methods_list, function(x) { length(x) <- max_methods; x })
  methods_df <- as.data.frame(do.call(rbind, padded))
  colnames(methods_df) <- paste0("method", seq_len(max_methods))
  return(methods_df)
}