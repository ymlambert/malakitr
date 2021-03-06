% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_instances.R
\name{aggregateInstances}
\alias{aggregateInstances}
\title{Aggregate decrypted instances into a dataframe}
\usage{
aggregateInstances(instanceRegistry, formInfo, oldData = NULL)
}
\arguments{
\item{instanceRegistry}{Instance registry dataframe generated by decryptInstances}

\item{formInfo}{Form info file generated by setupFormDirectory}

\item{oldData}{Optional, dataframe with existing data to be merged with new instances}
}
\value{
Dataframe of aggregated, decrypted instances
}
\description{
Aggregate decrypted instances into a dataframe
}
