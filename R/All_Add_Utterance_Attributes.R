#' All Add Utterance Attributes
#'
#' This function uploads all csv's within a given file directory and format into the parent environment. It then adds a time column and other trial attributes to each trial.
#'
#' @importFrom utils read.csv
#'
#' @details
#' WARNING: This function assumes all data is Jaw Motion Data within the extracted utterance format using the MATLAB Functions "Extract_Jaw_Interval_Data" and "All_Jaw_Utterance_To_CSV". Do not use other data types.
#'
#' \itemize{
#'   \item \code{Time}: A new column added to represent the time since onset of each row, counted in milliseconds.
#'   \item \code{Subject}: The subject number based on the file name.
#'   \item \code{Trial}: The trial number based on the file name.
#'   \item \code{Rate}: A variable that designates the rate of speaking (Slow, Fast, or Other) based on the file name.
#'     \itemize{
#'       \item 'S' -> "Slow"
#'       \item 'F' -> "Fast"
#'       \item Other -> "Other"
#'     }
#'   \item \code{Stress}: A variable that designates the vowel stress condition (Stressed, Unstressed, or Other) based on the file name.
#'   \itemize{
#'       \item 'S' -> "Stressed"
#'       \item 'U' -> "Unstressed"
#'     }
#'   \item \code{Vowel}: A variable of the vowel spoken based on the file name.
#'   \item \code{Consonant}: A variable of the consonant spoken based on the file name.
#'   \item \code{Repetition}: The repetition number from the file name.
#'   \item \code{Hearing}: A variable that designates the hearing type based on the file name.
#'   \item \code{Age}: A variable that identifies the age group (Adult or Child) based on the file name.
#'    \itemize{
#'       \item 'A' -> "Adult"
#'       \item 'C' -> "Child"
#'     }
#'   \item \code{Fix}: Currently Unknown to be fixed
#' }
#'
#' A warning will be issued if a file cannot be read or if a column is missing within the data.
#'
#' @return This function does not return any value. Instead, it uploads all jaw data csv files in the given format into the parent environment as data frames.
#'
#' @export
#'
#' @examples
#' \dontrun{All_Add_Utterance_Attributes()}
All_Add_Utterance_Attributes = function() {

  # Selects all files within given directory
  files <- Sys.glob("*.csv")

  # Individually modify and upload each file
  for (i in 1:length(files)) {
    filename = files[i]

    # Check if file exists
    if (file.exists(filename)) {
      data = tryCatch({
        read.csv(filename)
      }, error = function(e) {
        message(paste("Error reading file:", filename))
        NULL
      })

      # Skip if reading failed
      if (is.null(data)) next

      # Relabel Columns
      colnames(data) <- c("X", "Y", "Z", "4", "5", "6")

      # Add Time Column
      Time <- c(0:(length(data$Z) - 1))
      data$Time <- Time

      # Add attributes

      # Subject Number
      Subject = as.numeric(sub("^T(\\d+).*", "\\1", filename))
      data$Subject = Subject

      # Trial Number
      Trial = as.numeric(sub("^T\\d+_T(\\d+).*", "\\1", filename))
      data$Trial = Trial

      # Rate (Slow, Fast, etc.)
      Rate.abrv = as.character(sub("^([^_]*_){2}([^_]).*", "\\2", filename))
      if (Rate.abrv == 'S') {
        Rate = "Slow"
      } else if (Rate.abrv == 'F') {
        Rate = "Fast"
      } else {
        Rate = "Other"
      }
      data$Rate = Rate

      # Stress (Stressed, Unstressed)
      Stress.abrv = as.character(sub("^T\\d+_T\\d+_[A-Z]([A-Z]).*", "\\1", filename))
      if (Stress.abrv == 'S') {
        Stress = "Stressed"
      } else if (Stress.abrv == 'U') {
        Stress = "Unstressed"
      } else {
        Stress = "Other"
      }
      data$Stress = Stress

      # Vowel Said
      Vowel = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z]([a-z]).*", "\\1", filename))
      data$Vowel = Vowel

      # Consonant Said
      Consonant = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z]([a-z]).*", "\\1", filename))
      data$Consonant = Consonant

      # Repetition Number
      Repetition = as.numeric(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_(\\d+).*", "\\1", filename))
      data$Repetition = Repetition

      # Hearing Type (Normal, ...)
      Hearing = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_([A-Z][A-Z]).*", "\\1", filename))
      data$Hearing = Hearing

      # Age Group (Adult, Child)
      Age.abrv = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_[A-Z][A-Z]_([A-Z]).*", "\\1", filename))
      if (Age.abrv == 'A') {
        Age = "Adult"
      } else if (Age.abrv == 'C') {
        Age = "Child"
      }
      data$Age = Age

      # Check with Masapollo for Indicator
      Fix = as.numeric(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_[A-Z][A-Z]_[A-Z]_(\\d+).*", "\\1", filename))
      data$Fix = Fix

      # Upload Modified Data to the Global Environment
      label = sub("\\.csv$", "", filename)  # Label for the data
      assign(label, data, envir = parent.frame())
    }
  }
}
