#' Add Instance Attributes
#'
#' @param instances a named list of jaw instance data frames titled with the OU Speech Motor Lab nomenclature
#'
#' @details A more detailed but supersceded version of Add_Instance_Identifiers. Creates a list of attributes/trial conditions based on the list nomenclature.
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
#' @importFrom stats setNames
#'
#' @return a parallel named list of attributes based on the nomenclature
#' @export
#'
#'
#' @examples
#' \dontrun{Add_Instance_Attributes(instance_directory)}
Add_Instance_Attributes = function(instances) {

  # Initialize the list with the same names as the instances
  instance_attributes <- setNames(vector("list", length(names(instances))), names(instances))

  add_attributes = function(label) {
    attributes <- list()

    # Subject Number
    Subject =  as.numeric(sub("^T(\\d+).*", "\\1", label))
    attributes$Subject = Subject

    # Trial Number
    Trial = as.numeric(sub("^T\\d+_T(\\d+).*", "\\1", label))
    attributes$Trial = Trial

    # Rate (Slow, Fast, etc.)
    Rate.abrv = as.character(sub("^([^_]*_){2}([^_]).*", "\\2", label))
    if (Rate.abrv == 'S') {
      attributes$Rate = "Slow"
    } else if (Rate.abrv == 'F') {
      attributes$Rate = "Fast"
    } else {
      attributes$Rate = "Other"
    }

    # Stress (Stressed, Unstressed)
    Stress.abrv = as.character(sub("^T\\d+_T\\d+_[A-Z]([A-Z]).*", "\\1", label))
    if (Stress.abrv == 'S') {
      attributes$Stress = "Stressed"
    } else if (Stress.abrv == 'U') {
      attributes$Stress = "Unstressed"
    } else {
      attributes$Stress = "Other"
    }

    # Vowel Said
    Vowel = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z]([a-z]).*", "\\1", label))
    attributes$Vowel = Vowel

    # Consonant Said
    Consonant = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z]([a-z]).*", "\\1", label))
    attributes$Consonant = Consonant

    # Repetition Number
    Repetition = as.numeric(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_(\\d+).*", "\\1", label))
    attributes$Repetition = Repetition

    # Hearing Type (Normal, ...)
    Hearing = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_([A-Z][A-Z]).*", "\\1", label))
    attributes$Hearing = Hearing

    # Age Group (Adult, Child)
    Age.abrv = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_[A-Z][A-Z]_([A-Z]).*", "\\1", label))
    if (Age.abrv == 'A') {
      attributes$Age = "Adult"
    } else if (Age.abrv == 'C') {
      attributes$Age = "Child"
    }

    # Check with Masapollo for Indicator
    attributes$Fix = as.numeric(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_[A-Z][A-Z]_[A-Z]_(\\d+).*", "\\1", label))

    # Code
    attributes$Code = paste0(Rate.abrv, Stress.abrv, attributes$Vowel, attributes$Consonant)

    # Identifier
    attributes$Identifier =
      paste0(Subject,
             "_",
             Rate.abrv,
             Stress.abrv,
             Vowel,
             Consonant,
             "_",
             Hearing,
             "_",
             Age.abrv)

    return(attributes)
  }

  instance_attributes <- mapply(add_attributes, names(instances), SIMPLIFY = FALSE)

  return(instance_attributes)
}
