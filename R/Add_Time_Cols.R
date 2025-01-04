#' Add Time Columns
#'
#' @param instances a list of jaw utterance instance data frames
#'
#' @return a list of jaw utterance instance data frames with a time column "Time" of time since utterance onset
#' and a recentered time column "CenteredTime" of time since the lowest point of jaw vertical displacement,
#' both in milliseconds
#' @export
#'
#' @examples
#' \dontrun{Add_Time_Cols(instances = instance_directory)}
Add_Time_Cols = function(instances) {

  # Create a time column of time since offset
  add_time = function(df) {
    df$Time = 0:(nrow(df) - 1)
    return(df)
  }

  # Create a time column centered around the lowest point of vertical jaw displacement (milliseconds)
  add_centered_time = function(df) {

    lowest = which.min(df$Z) # Find the lowest point of jaw displacement
    Centered <- rep(NA, length = nrow(df)) # Create a new list

    neg = -1 # Assign time value by counting backwards from the lowest point, which is 0
    for(j in (lowest - 1):1) {
      Centered[j] <- neg
      neg = neg - 1
    }

    pos = 1 # Count forwards from the lowest point
    k = lowest + 1
    while(k <= nrow(df)) {
      Centered[k] <- pos
      pos = pos + 1
      k = k + 1
    }

    Centered[lowest] <- 0 # Set the time of the lowest point to 0
    df$CenteredTime <- Centered # Reassign the list to a new Centered Time column
    return(df)
  }

  # Apply both functions to the list of data frames
  instances <- lapply(instances, add_time)
  instances <- lapply(instances, add_centered_time)

  return(instances)
}
