#' @include models.R
#' @include comms.R
NULL

#' bf_datasets
#'
#' @param client BFCLient object
setGeneric("bf.datasets", function(client) {NULL})

#' contents
#'
#' @param client BFCLient object
#' @param target Object for which contents should be returned
setGeneric("contents", function(client, target) {NULL})

#' contents
#'
#' @param client BFCLient object
#' @param id String with the ID of a data package
setGeneric("contents.id", function(client, id) {NULL})


#' Wrapper function BFClient
#'
#' @name Blackfynn
#' @rdname Blackfynn
#' @param scope String indicating the API key scope
#' @return A Blackfynn Client object which represents an
#' active session with the Blackfynn platform.
#'
#' @export
Blackfynn <- function(...) new("BFClient", ...)

#' @describeIn BFClient Initializes the BFClient object. It opens
#' a Blackfynn session and stores the API token for the
#' session.
#'
#' @return BFClient object
setMethod("initialize", "BFClient",
          function(.Object, ...) {
            .Object <- callNextMethod()

            # Load Ini file and add api_key/secret
            ini <- read.ini("~/.blackfynn/config.ini")
            if (identical(.Object@scope, character(0))) {
              .Object@scope <- ini$global$default_profile
            }
            api.key <- ini[[.Object@scope]]$api_token
            api.secret <- ini[[.Object@scope]]$api_secret
            if (identical(api.key, NULL)) {
              stop(paste("Scope", .Object@scope, "unknown."))
            }

            api.host = ini[[.Object@scope]]$api_host
            if (identical(api.host, NULL)) {
              .Object@api.host <- "https://api.blackfynn.io"
            } else {
              .Object@api.host <- api.host
            }

            # Get Session token
            query <- list("tokenId" = api.key, "secret" = api.secret)
            url <- paste(.Object@api.host,"/account/api/session",sep="")

            response <- POST(url, body = query, encode = "json",
                             content_type_json(), accept_json()) %>% content()


            .Object@org.id <- response$organization
            .Object@session.token <- response$session_token
            .Object@headers <- c("X-ORGANIZATION-ID" = response$organization,
                                 "X-SESSION-ID" = response$session_token,
                                 "Authorization" = paste("Bearer",
                                                         response$session_token))
            # Get Organization
            url <- paste(.Object@api.host,"/organizations/",
                         response$organization, sep = "")

            .Object@org <- GET(url, add_headers(.headers = .Object@headers)) %>%
              content() %$% organization %$% name

            cat('Profile:',.Object@scope,
                '\nHost:', .Object@api.host,
                '\nOrganization:', .Object@org)
            .Object
          })

#'BFClient
#'     Shows a representation of a Blackfynn Client object.
#'
#' @param object BFClient object
#' @export
setMethod("show", "BFClient",
          function(object) {
            cat("---Blackfynn Client---\n")
            cat("Organization: ", object@org,"\n")
            cat("----------------------\n")
          }
)

#' @describeIn BFDataset
#'     Shows a representation of a Blackfynn Dataset object.
#'
#' @param object BFClient object
#' @export
setMethod("show", "BFDataset",
          function(object) {
            cat("---Blackfynn Dataset---\n")
            cat("id:   ", object@id,"\n")
            cat("name: ", object@name,"\n")
            cat("description: ", object@description,"\n")
            cat("size: ", object@size,"MB\n")
            cat("----------------------\n")
          }
)

#' @describeIn BFClient
#'     Returns a list of Datasets for the organization.
#'     This is mroe inof
#'
#' @param client BFClient object
#'
#' @export
setMethod("bf.datasets", "BFClient",
          function(client) {
            response <- bf.get(client, "/datasets/", list())

            ds <- vector("list", length(response))
            i <- 1
            for (item in response) {
              content <- item$content
              ds[[i]] <- new("BFDataset",
                             name = content$name,
                             description = "desc",
                             id = content$id,
                             size = item$storage/1000)
              i <- i + 1
            }

            ds
          })

#' @describeIn BFDataPackage
#'     Shows a representation of a Blackfynn DataPackage object
#'
#' @param object BFClient object
#' @export
setMethod("show", "BFDataPackage",
          function(object) {
            cat("---Blackfynn Data Package---\n")
            cat("id:   ", object@id,"\n")
            cat("name: ", object@name,"\n")
            cat("type: ", object@type,"\n")
            cat("----------------------------\n")
          }
)

#' @describeIn BFTimeSeries
#'     Shows a representation of a Blackfynn Timeseries object
#'
#' @param object BFClient object
#' @export
setMethod("show", "BFTimeSeries",
          function(object) {
            cat("---Blackfynn Time Series---\n")
            cat("id:   ", object@id,"\n")
            cat("name: ", object@name,"\n")
            cat("start: ", object@start,"\n")
            cat("end: ", object@end,"\n")
            cat("channels: ", object@channels,"\n")
            cat("---------------------------\n")
          }
)

#' @describeIn BFClient
#'     Lists the content of a dataset
#'
#' @param client BFClient client
#' @param target "BFDataset" Id of a Dataset, or Collection
#' @export
setMethod("contents", signature("BFClient", "BFDataset"),
          function(client, target) {
            url <- paste("/datasets/", target@id, sep = "")
            response <- bf.get(client,url,list())

            children <- response$children
            packages <- vector("list", length(children))
            i <- 1
            for (item in children) {
              content = item$content
              packages[[i]] <- new("BFDataPackage",
                                   id = content$id,
                                   name = content$name
                                   )
              i <- i + 1
            }
            packages

          })

#' @describeIn BFDataPackage
#'     Lists the content of a package/collection
#'
#' @param client BFClient client
#' @param target "BFDataPackage" Id of a Dataset, or Collection
#' @export
setMethod("contents", signature("BFClient", "BFDataPackage"),
          function(client, target) {
            out <- contents(client, target@id)
          })


#' @describeIn BFClient
#'     Get the contents of any object on Blackfynn
#'
#'     @param client BFClient client
#'     @param id "Character" ID of an object in a dataset
#'     @export
setMethod("contents.id", signature("BFClient", "character"),
          function(client, id) {
            url <- paste("/packages/", id, sep = "")
            resp <- bf.get(client, url, list(include = "view",
                                                 includeAncestors = TRUE))

            p.type <- resp$content$packageType
            switch (p.type,
                    "TimeSeries" = {
                      cnt <- bf.create.ts(resp)
                    },
                    "Collection" = {
                      cnt <- new("BFDataPackage",
                                           id = resp$content$id,
                                           name = resp$content$name
                      )
                    },
                    {
                      cnt <- new("BFDataPackage",
                                           id = resp$content$id,
                                           name = resp$content$name
                      )
                    })
            cnt
          })



