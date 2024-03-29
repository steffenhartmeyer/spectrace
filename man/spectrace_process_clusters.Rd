% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectrace_process_clusters.R
\name{spectrace_process_clusters}
\alias{spectrace_process_clusters}
\title{Process spectral clusters}
\usage{
spectrace_process_clusters(
  lightData,
  datetimeVar = datetime,
  clusterVar = cluster_id,
  regularise = TRUE
)
}
\arguments{
\item{lightData}{The light data}

\item{datetimeVar}{The name of the datetime variable. Defaults to "datetime".}

\item{clusterVar}{The name of the variable that defines the cluster to process.}

\item{regularise}{Logical. Regularise the data?}
}
\description{
Process spectral clusters
}
