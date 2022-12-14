% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document.R
\name{copy_bo_document}
\alias{copy_bo_document}
\title{Copy a Webi document}
\usage{
copy_bo_document(conn, document, parent_folder, destination_document_name)
}
\arguments{
\item{conn}{Connection reference}

\item{document}{Numeric id or tibble of properties of Webi document}

\item{parent_folder}{Numeric id of parent folder for copy}

\item{destination_document_name}{Name of destination document}
}
\value{
Response content
}
\description{
Copy a Webi document
}
