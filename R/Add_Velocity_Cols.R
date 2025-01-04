#' Add Velocity Columns
#'
#' @param instances a list of jaw utterance instance data frames
#'
#' @return a list of jaw utterance instance data frames with a velocity column "Velocity"
#' and a rescaled column "RescaledVelocity" of [-1, 1] for layered visualization
#' @export
#'
#' @examples
#' \dontrun{Add_Velocity_Cols(instance_directory)}
Add_Velocity_Cols = function(instances) {

  # Create and add the velocity column to each data frame
  add_velocity = function(df) {

    # Calculate the differences for Z and Time
    delta_Z <- diff(df$Z)
    delta_Time <- diff(df$Time)

    # Calculate the velocity
    velocity <- delta_Z / delta_Time

    # Initialize the Velocity column with NA and assign the calculated velocities
    df$Velocity <- NA
    df$Velocity[-1] <- velocity

    return(df)
  }

  # Create and add the rescaled column to each data frame
  add_velocity_rescaled = function(df) {

    # Rescale to [-1, 1]
    velMin = (min(df$Velocity, na.rm = TRUE))
    velMax = (max(df$Velocity, na.rm = TRUE))
    df$VelocityRescaled = 2 * (df$Velocity - velMin) / (velMax-velMin) - 1

    return(df)
  }

  # Add columns to all df frames within the list
  instances = lapply(instances, add_velocity)
  instances = lapply(instances, add_velocity_rescaled)

  return(instances)

}
