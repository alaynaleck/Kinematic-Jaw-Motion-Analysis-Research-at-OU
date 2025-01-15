#' Create_Instance_Plot
#'
#' @param X a vector of instance data to display
#' @param Y a vector of instance data to display
#' @param Subject an integer representing an existing subject number: "18" or "19"
#' @param Rate a string of the desired utterance rate: "Fast" or "Slow"
#' @param Stress a string of the desired vowel stress type: "Stressed" or "Unstressed"
#' @param Vowel a 1-character string of the desired vowel spoken: 'a' or 'e'
#' @param Consonant a 1-character string of the desired consonant spoken: 'd' or 't'
#' @param Hearing an all capital string of the desired hearing type: "NH"
#' @param Age a string of the desired age category: "Adult" or "Child"
#' @param Instances a named list of jaw instance data frames titled with the OU Speech Motor Lab nomenclature
#' @param IDs a parallel named list of jaw instance IDs titled with the OU Speech Motor Lab nomenclature
#'
#' @return a plot of the selected instance data
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{Create_Instance_Plot(
#' X = "Time",
#' Y = "Z",
#' Subject = "18",
#' Rate = "Fast",
#' Stress = "Stressed",
#' Vowel = 'a',
#' Consonant = 'd',
#' Hearing = "NH",
#' Age = "Adult",
#' Instances = instance_directory,
#' IDs = id_directory)}
Create_Instance_Plot <- function(X, Y, Subject, Rate, Stress, Vowel, Consonant, Hearing, Age, Instances, IDs) {
  # Create the instance identifier
  id <- JawMotionOU::Create_Instance_Identifier(Subject = Subject, Rate = Rate, Stress = Stress, Vowel = Vowel, Consonant = Consonant, Hearing = Hearing, Age = Age)

  # Find the correct IDs and repetitions
  id_idx <- which(IDs == id)

  # Get the final subset of data frames
  final <- Instances[id_idx]

  # Create a base ggplot object
  p <- ggplot(mapping = aes(.data[[X]], .data[[Y]]))

  # Rename X and Y for better viewing and add captions
  xaxis <- switch(X,
                  "RescaledVelocity" = { p <- p + labs(caption = "Each utterance has been normalized from [-1, 1]"); "Velocity" },
                  "RecenteredTime" = { p <- p + labs(caption = "Each utterance has been centered around its time of maximum vertical displacement"); "Time" },
                  "RecenteredZ" = { p <- p + labs(caption = "Each utterance has been centered around its mean vertical displacement"); "Vertical Displacement" },
                  "RescaledZ" = { p <- p + labs(caption = "Each utterance has been normalized from [-1, 1]"); "Z" },
                  "Z" = {"Vertical Displacement"},
                  X)

  yaxis <- switch(Y,
                  "RescaledVelocity" = { p <- p + labs(caption = "Each utterance has been normalized from [-1, 1]"); "Velocity" },
                  "RecenteredTime" = { p <- p + labs(caption = "Each utterance has been centered around its time of maximum vertical displacement"); "Time" },
                  "RecenteredZ" = { p <- p + labs(caption = "Each utterance has been centered around its mean vertical displacement"); "Vertical Displacement" },
                  "RescaledZ" = { p <- p + labs(caption = "Each utterance has been normalized from [-1, 1]"); "Vertical Displacement" },
                  "Z" = {"Vertical Displacement"},
                  Y)

  # Add each geom_path layer to the plot
  for (i in 1:length(final)) {
    graph <- final[[i]]
    if (X == "Velocity" || X == "RescaledVelocity" || Y == "Velocity" || Y == "RescaledVelocity") {
      p <- p + geom_path(data = graph[-1, ])
    } else {
      p <- p + geom_path(data = graph)
    }
  }

  # Add title and subtitle
  p <- p + labs(
    title = paste0("Subject ", Subject, " ",
                   substr(Rate, 1, 1),
                   substr(Stress, 1, 1),
                   Vowel,
                   Consonant, " ",
                   Hearing, " ", Age),
    subtitle = paste0(yaxis, " vs ", xaxis)
  )

  # Add axis names
  p <- p + xlab(xaxis) + ylab(yaxis)

  # Return the final plot
  return(p)
}
