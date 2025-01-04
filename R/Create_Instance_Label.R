#' Create Instance Indentifier
#'
#' @param Subject an integer representing an existing subject number: "18" or "19"
#' @param Rate a string of the desired utterance rate: "Fast" or "Slow"
#' @param Stress a string of the desired vowel stress type: "Stressed" or "Unstressed"
#' @param Vowel a 1-character string of the desired vowel spoken: 'a' or 'e'
#' @param Consonant a 1-character string of the desired consonant spoken: 'd' or 't'
#' @param Hearing an all capital string of the desired hearing type: "NH"
#' @param Age a string of the desired age category: "Adult" or "Child"
#'
#' @return the instance identifier for the desired utterance type
#' @export
#'
#' @examples
#' {Create_Instance_Identifier(Subject = 19,
#' Rate = "Fast", Stress = "Stressed", Vowel = "a",
#' Consonant = "d", Hearing = "NH", Age = "Adult")}
Create_Instance_Identifier = function(Subject, Rate, Stress, Vowel, Consonant, Hearing, Age) {

  # Convert strings into abbreviations
  Rate.abrv = toupper(substr(Rate, 1, 1))
  Stress.abrv = toupper(substr(Stress, 1, 1))
  Age.abrv = toupper(substr(Age, 1, 1))

  # Combine each variable into a label based on utterance type
  paste0(Subject,
         "_",
         Rate.abrv,
         Stress.abrv,
         tolower(Vowel),
         tolower(Consonant),
         "_",
         toupper(Hearing),
         "_",
         Age.abrv)
}
