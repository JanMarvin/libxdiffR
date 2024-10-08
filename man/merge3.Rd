% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge.R
\name{merge3}
\alias{merge3}
\title{Compare Files Using Three-Way Merges}
\usage{
merge3(base, merge1, merge2, level = "minimal", favor = NULL, style = NULL)
}
\arguments{
\item{base}{The common ancestor file or text input. This is the original version from which the two modified versions (\code{merge1} and \code{merge2}) were derived.}

\item{merge1, merge2}{The first and second modified files or text input to be merged.}

\item{level}{A string specifying the merging aggressiveness level. Options include:
\itemize{
\item \code{"minimal"}: Only essential changes are merged, minimizing potential conflicts.
\item \code{"eager"}: Merges more eagerly, potentially leading to more conflicts but also more comprehensive merges.
\item \code{"zealous"}: Aggressively merges changes, resolving conflicts where possible.
\item \code{"zealous_alnum"}: Similar to "zealous", but with additional handling for alphanumeric characters.
}}

\item{favor}{favor: A string specifying which changes to favor in case of conflicts. Options are:
\itemize{
\item \code{"ours"}: Favor the changes from merge1.
\item \code{"theirs"}: Favor the changes from merge2.
\item \code{"union"}: Combine the changes from both merge1 and merge2 when possible.
}}

\item{style}{A string specifying the merge style. Options include:
\itemize{
\item \code{"diff3"}: Standard diff3-style merge output.
\item \code{"zdiff3"}: A zealous variant of the diff3-style merge, offering more comprehensive conflict resolution.
}}
}
\value{
A character string containing the three way merge.
}
\description{
\code{merge3()} is a function designed to perform three-way merges between files or text inputs. Three-way merging is commonly used in version control systems to reconcile changes made by multiple contributors, allowing for the integration of modifications from different branches or versions with a shared common ancestor.
}
\details{
The \code{merge3()} function takes three inputs—an ancestor version and two modified versions—and attempts to reconcile the differences between them, producing a single merged output. This process is crucial in collaborative development environments where multiple team members work on the same files.
The function supports different levels of merging aggressiveness, allowing users to tailor the merge process according to their needs. Additionally, users can specify which version's changes should be favored in case of conflicts, and choose between standard and zealous diff3-style outputs.
The function also supports inputs from URLs, making it versatile for remote file merging scenarios.
}
\examples{
base <- "function(x) print(x) \n"
merge1 <- "function(x) print(x) \n message(x)\n"
merge2 <- "function(x) print(x) \n cat(x)\n"

cat(merge3(base, merge1, merge2))
}
