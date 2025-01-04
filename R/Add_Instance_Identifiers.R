#' Add Instance Identifiers
#'
#' @param instances a named list of jaw instance data frames titled with the OU Speech Motor Lab nomenclature
#'
#' @return a parallel named list of identifiers based on the nomenclature
#' @export
#'
#' @examples
#' \dontrun{Add_Instance_Identifiers(instance_directory)}
Add_Instance_Identifiers = function(instances) {

  add_attributes = function(label) {

    # Subject Number
    Subject = as.numeric(sub("^T(\\d+).*", "\\1", label))

    # Trial Number
    Trial = as.numeric(sub("^T\\d+_T(\\d+).*", "\\1", label))

    # Rate (Slow, Fast, etc.)
    Rate.abrv = as.character(sub("^([^_]*_){2}([^_]).*", "\\2", label))

    # Stress (Stressed, Unstressed)
    Stress.abrv = as.character(sub("^T\\d+_T\\d+_[A-Z]([A-Z]).*", "\\1", label))

    # Vowel Said
    Vowel = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z]([a-z]).*", "\\1", label))

    # Consonant Said
    Consonant = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z]([a-z]).*", "\\1", label))

    # Hearing Type (Normal, ...)
    Hearing = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_([A-Z][A-Z]).*", "\\1", label))

    # Age Group (Adult, Child)
    Age.abrv = as.character(sub("^T\\d+_T\\d+_[A-Z][A-Z][a-z][a-z]_\\d+_[A-Z][A-Z]_([A-Z]).*", "\\1", label))

    # Identifier
    Identifier = paste0(Subject, "_", Rate.abrv, Stress.abrv, Vowel, Consonant, "_", Hearing, "_", Age.abrv)

    return(Identifier)
  }

  instance_identifier <- mapply(add_attributes, names(instances), SIMPLIFY = FALSE)

  return(instance_identifier)
}
