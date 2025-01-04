#' Upload Jaw Data
#'
#' This function uploads all csv's within a given file directory and format into a named list.
#'
#' @details
#' WARNING: This function assumes all data is Jaw Motion Data within the extracted utterance format using the MATLAB Functions "Extract_Jaw_Interval_Data" and "All_Jaw_Utterance_To_CSV". Do not use other data types.
#'
#' @param dir a directory/file path containing only csvs of jaw motion data within the given file format; if no directory is given, the function will use the current working directory
#' @param list a list of jaw instance data frames to add to
#'
#' @return a named list of jaw instance data frames with added data from files in the directory
#' @export
#'
#' @examples
#' \dontrun{Upload_Jaw_Data()}
#' \dontrun{Upload_Jaw_Data("C://Users//JohnDoe//
#' OneDrive//Jaw Motion Research//Data//Jaw-Motion-CSV-ALL")}
Upload_Jaw_Data = function(dir, list) {

  # Create dynamic name for all files within given directory
  path = paste0(dir, "//*.csv")
  files <- Sys.glob(path) # create a list of files within the given path

  # Individually modify and upload each file
  for (i in 1:length(files)) {
    file = files[i]
    filename = sub(".*//", "", file)
    name = sub("\\.csv$", "", filename)  # Remove the '.csv' extension

    # Check if file exists
    if (file.exists(file)) {
      data = tryCatch({
        read.csv(file)
      }, error = function(e) {
        message(paste("Error reading file:", filename))
        NULL
      })

      # Skip if reading failed
      if (is.null(data)) next

      # Relabel Columns
      colnames(data) <- c("X", "Y", "Z", "4", "5", "6")

      # Add the data frame to the list with the name being the filename
      list[[name]] <- data
    }
  }

  return(list)  # Return the list of data frames
}
