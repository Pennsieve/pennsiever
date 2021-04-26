#' @include models.R
NULL

#' bf.get
#'
#' @param client BFCLient object
#' @param path "character"
#' @param params "list"
#' @export
setGeneric("bf.get", function(client, path, params) standardGeneric("bf.get"))

#' bf.post
#'
#' @param client BFCLient object
#' @param path "character"
#' @param params "list"
#' @export
setGeneric("bf.post", function(client, path, params) standardGeneric("bf.post"))

#'dev bf.delete
#'
#' @param client BFCLient object
#' @param path "character"
#' @param params "list"
#' @export
setGeneric("bf.delete", function(client, path, params) standardGeneric("bf.delete"))

#' bf.put
#'
#' @param client BFCLient object
#' @param path "character"
#' @param params "list"
#' @export
setGeneric("bf.put", function(client, path, params) standardGeneric("bf.put"))


#' BFClient
#'
#' @param client BFClient
#' @param path "character"
#' @param params "list"
#' @export
setMethod("bf.get", signature("BFClient",
                    "character",
                    "list"),
          function(client, path, params) {
            url <- paste(client@api.host, path, sep = "")

            cat(url)

            resp <- GET(url, add_headers(.headers = client@headers), query = params)
            tryCatch(stop_for_status(resp),
                     http_404 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("404: That URL does not exist", call. = FALSE)
                     },
                     http_403 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("403: Authentication error", call. = FALSE)
                       },
                     http_400 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("400: error", call. = FALSE)
                     },
                     http_500 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("500: Server error", call. = FALSE)
                     }
                    )
            out <- content(resp, "parsed")

          })

#' BFClient
#'
#' @param client BFClient
#' @param path ""
#' @param params "list"
#' @export
setMethod("bf.post", signature("BFClient",
                              "character",
                              "list"),
          function(client, path, params) {
            url <- paste(client@api.host, path, sep = "")

            resp <- POST(url, add_headers(.headers = client@headers), query = params)

            tryCatch(stop_for_status(resp),
                     http_404 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("404: That URL does not exist", call. = FALSE)
                     },
                     http_403 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("403: Authentication error", call. = FALSE)
                     },
                     http_400 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("400: error", call. = FALSE)
                     },
                     http_500 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("500: Server error", call. = FALSE)
                     }
            )
            out <- content(resp, "parsed")
          })

#' BFClient
#'
#' @param client BFClient
#' @param path ""
#' @param params "list"
#' @export
setMethod("bf.delete", signature("BFClient",
                               "character",
                               "list"),
          function(client, path, params) {
            url <- paste(client@api.host, path, sep = "")
            resp <- DELETE(url, add_headers(.headers = client@headers), query = params)
            tryCatch(stop_for_status(resp),
                     http_404 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("404: That URL does not exist", call. = FALSE)
                     },
                     http_403 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("403: Authentication error", call. = FALSE)
                     },
                     http_400 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("400: error", call. = FALSE)
                     },
                     http_500 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("500: Server error", call. = FALSE)
                     }
            )
            out <- content(resp, "parsed")
          })

#' BFClient
#'
#' @param client BFClient
#' @param path ""
#' @param params ""
#' @export
setMethod("bf.put", signature("BFClient",
                                 "character",
                                 "character"),
          function(client, path, params) {
            url <- paste(client@api.host, path, sep = "")
            resp <- PUT(url, add_headers(.headers = client@headers), query = params)

            tryCatch(stop_for_status(resp),
                     http_404 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("404: That URL does not exist", call. = FALSE)
                     },
                     http_403 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("403: Authentication error", call. = FALSE)
                     },
                     http_400 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("400: error", call. = FALSE)
                     },
                     http_500 = function(c) {
                       cat(content(resp, as = "text"))
                       stop("500: Server error", call. = FALSE)
                     }
            )
            out <- content(resp, "parsed")
          })

