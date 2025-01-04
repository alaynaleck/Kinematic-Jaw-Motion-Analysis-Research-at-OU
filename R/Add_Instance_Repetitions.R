#' Add Instance Repetitions
#'
#' @param instances a named list of jaw instance data frames titled with the OU Speech Motor Lab nomenclature
#'
#' @return a parallel named list of repetition number based on the nomenclature
#' @export
#'
#' @examples
#' \dontrun{Add_Instance_Repetitions(instance_directory)}
Add_Instance_Repetitions = function(instances) {

  # Document the repetition based on the nomenclature of the label
  add_repetition = function(label) {
    Repetition = as.numeric(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_(\\d+).*", "\\1", label))
    return(Repetition)
  }

  # Add the repetition to a list of repetitions
  instance_reps <- mapply(add_repetition, names(instances), SIMPLIFY = FALSE)
  return(instance_reps)
}
