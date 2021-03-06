% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxr_search.R
\name{box_search}
\alias{box_search}
\alias{box_search_files}
\alias{box_search_folders}
\alias{box_search_trash}
\title{Search Box files}
\usage{
box_search(query = "", content_types = c("name", "description",
  "file_content", "comments", "tags"), type = NULL,
  file_extensions = NULL, ancestor_folder_ids = NULL,
  created_at_range = NULL, updated_at_range = NULL,
  size_range = NULL, trash = FALSE, owner_user_ids = NULL,
  max = 200)

box_search_files(query, ...)

box_search_folders(query, ...)

box_search_trash(query, ...)
}
\arguments{
\item{query}{The search term that you'd like to use}

\item{content_types}{The content types you'd like to search. More than one
can be supplied with a vector}

\item{type}{The type of object you'd like returned. The default of
\code{NULL} return all possible types. Other acceptable values are "file",
"folder", or "weblink"}

\item{file_extensions}{A vector of strings containing the file extensions
(without dots) by which to narrow your search.}

\item{ancestor_folder_ids}{If supplied, results are limited to one or more
parent folders}

\item{created_at_range}{Optional. Limit search to a range of created at
times. A vector of two dates (coercible via \code{\link[=as.POSIXct]{as.POSIXct()}}).}

\item{updated_at_range}{Optional. Limit search to a range of updated at
times. A vector of two dates (coercible via \code{\link[=as.POSIXct]{as.POSIXct()}}).}

\item{size_range}{Optional. Limit search to a range of file sizes, in bytes.
A vector of two whole numbers (coerible via (coercible via
\code{\link[=as.numeric]{as.numeric()}} and \code{\link[=round]{round()}})).}

\item{trash}{Should the search be limited to the trash folder?
\code{\link[=logical]{logical()}}.}

\item{owner_user_ids}{Optional. Limit search to a files owned by a set of
users. A vector if IDs, coercible with \code{\link[=as.integer64]{as.integer64()}}.}

\item{max}{\code{numeric}. Upper limit on the number of search results
returned (protective measure for users with large numbers of files).}

\item{...}{Parameters passed to \code{box_search}}
}
\value{
An object of class \code{boxr_object_list}. See
\code{\link[=boxr_S3_classes]{boxr_S3_classes()}} for details.
}
\description{
Search Box files
}
\details{
The box.com API supports a maximum of 200 results per request. If
\code{max > 200}, then multiple requests will be sent to retrieve and
combine 'paginated' results for you, behind the scenes.

See the \href{https://community.box.com/t5/Managing-Your-Content/Search-Overview-and-FAQs/ta-p/354}{box.com search description}
for details of the features of the service.
Some notable details:
\itemize{
\item Full Text Searching -
Is available for many source code file types, though not including R at
the time of writing.
\item Boolean Operators Are Supported -
Such as \code{and}, \code{or}, and \code{not} (upper or lower case)
\item Phrases Can Be Searched -
By putting them in "quotation marks".
\item Search Availability -
It takes around 10 minutes for a newly uploaded file to enter the
search index.
}
}
\author{
Brendan Rocks \email{foss@brendanrocks.com}
}
