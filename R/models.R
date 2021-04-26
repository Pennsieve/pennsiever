#' Class BFClient.
#'
#' This is the main interface class for the Blackfynn R
#' client. It handles the session and should be passed to
#' all method calls.
#'
#' @slot scope The scope in the configuration file
#' @slot org The organization for the current session
#'
#' @export
setClass("BFClient",
         slots = c(scope = "character",
                   org = "character",
                   org.id = "character",
                   session.token = "character",
                   headers = "character",
                   api.host = "character"))

#' Class Dataset
#'
#' This is the Dataset class.
#'
#' @slot name The name of the dataset
#' @slot description The description of the dataset
#' @slot id The dataset ID
#' @slot size The size of the dataset in MB.
#'
#' @export
setClass("BFDataset",
         slots = c(name = "character",
                   description = "character",
                   id = "character",
                   size = "numeric"))

#' Class DataPackage
#'
#' This is the DataPackage Class.
#'
#' @slot id The ID of the package
#' @slot name The name of the package
#' @slot type The type of the package
#' @export
setClass("BFDataPackage",
         slots = c(id = "character",
                   name = "character",
                   type = "character"))


#' Class Timeseries
#'
#' This is the Timeseries Package Class.
#'
#' @slot start Starttime of timeseries
#' @slot end Endtime of timeseries
#' @slot channels List of channels in timeseries package
#' @export
setClass("BFTimeSeries", contains="BFDataPackage",
         slots = c(start = "numeric",
                   end = "numeric",
                   channels = "list"))

#' Class Timeseries-Channel
#'
#' This is the Timeseries Channels Class.
#'
#' @slot id ID of the channel
#' @slot name Name of the channel
#' @slot start Starttime of timeseries
#' @slot end Endtime of timeseries
#' @export
setClass("BFTimeSeries",
         slots = c(id = "character",
                   name = "character",
                   start = "numeric",
                   end = "numeric",
                   channels = "list"))

