#' Add Z Columns
#'
#' @param instances a list of jaw utterance instance data frames
#'
#' @return a list of jaw utterance instance data frames with a recentered vertical position column "RecenteredZ"
#' and a rescaled column "RescaledZ" of [-1, 1] for layered visualization
#' @export
#'
#' @examples
#' \dontrun{Add_Z_Cols(instances)}
Add_Z_Cols = function(instances) {

  # Create a position column centered around the mean vertical displacement
  add_centered_z = function(df) {
    mean_z <- mean(df$Z)
    df$RecenteredZ <- df$Z - mean_z # Calculate the deviation of each point
    return(df)
  }

  # Create a rescaled, centered position column [-1, 1]
  add_rescaled_z = function(df) {
    range_z <- range(df$Z) # find the minimum and maximum values
    min_z <- range_z[1]
    max_z <- range_z[2]
    df$RescaledZ <- 2 * (df$Z - min_z) / (max_z - min_z) - 1
    return(df)
  }

  # Add each column to all of the data frames in the list
  instances <- lapply(instances, function(df) {
    df <- add_centered_z(df)
    df <- add_rescaled_z(df)
    return(df)
  })

  return(instances)
}

