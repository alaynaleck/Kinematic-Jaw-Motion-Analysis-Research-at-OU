% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Add_Instance_Attributes.R
\name{Add_Instance_Attributes}
\alias{Add_Instance_Attributes}
\title{Add Instance Attributes}
\usage{
Add_Instance_Attributes(instances)
}
\arguments{
\item{instances}{a named list of jaw instance data frames titled with the OU Speech Motor Lab nomenclature}
}
\value{
a parallel named list of attributes based on the nomenclature
}
\description{
Add Instance Attributes
}
\details{
A more detailed but supersceded version of Add_Instance_Identifiers. Creates a list of attributes/trial conditions based on the list nomenclature.

\itemize{
  \item \code{Time}: A new column added to represent the time since onset of each row, counted in milliseconds.
  \item \code{Subject}: The subject number based on the file name.
  \item \code{Trial}: The trial number based on the file name.
  \item \code{Rate}: A variable that designates the rate of speaking (Slow, Fast, or Other) based on the file name.
    \itemize{
      \item 'S' -> "Slow"
      \item 'F' -> "Fast"
      \item Other -> "Other"
    }
  \item \code{Stress}: A variable that designates the vowel stress condition (Stressed, Unstressed, or Other) based on the file name.
  \itemize{
      \item 'S' -> "Stressed"
      \item 'U' -> "Unstressed"
    }
  \item \code{Vowel}: A variable of the vowel spoken based on the file name.
  \item \code{Consonant}: A variable of the consonant spoken based on the file name.
  \item \code{Repetition}: The repetition number from the file name.
  \item \code{Hearing}: A variable that designates the hearing type based on the file name.
  \item \code{Age}: A variable that identifies the age group (Adult or Child) based on the file name.
   \itemize{
      \item 'A' -> "Adult"
      \item 'C' -> "Child"
    }
  \item \code{Fix}: Currently Unknown to be fixed
}
}
\examples{
\dontrun{Add_Instance_Attributes(instance_directory)}
}
