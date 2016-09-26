

#' Geospatial Datatype designer
#'
#' @description See official AllegroGraph documentation for information on 2D and nD Geospatial database design.
#' See examples below to get a general idea on how to use these functions.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param stripWidth Determines the granularity of the strip.
#' @param xMin Helps determine size of cartesian plane
#' @param xMax Helps determine size of cartesian plane
#' @param yMin Helps determine size of cartesian plane
#' @param yMax Helps determine size of cartesian plane
#' @param latMin Limit the size of the region modelled by appropriate type
#' @param latMax Limit the size of the region modelled by appropriate type
#' @param longMin Limit the size of the region modelled by appropriate type
#' @param longMax Limit the size of the region modelled by appropriate type
#' @param x Specifies x-coordinate of circle
#' @param y Specifies y-coordinate of circle
#' @param unit Defaults to 'degree'. The unit in which stripWidth is specified. It can be degree, km, radian or mile.
#' @param type Specifies a geospatial sub-type, as created by getCartesianGeoType or getSphericalGeoType
#' @param predicate Specifies a predicate for the query
#' @param lat Specifies the latitude of the center of the circle
#' @param long Specifies the longitude of the center of the circle
#' @param radius Specifies the radius of the circle
#' @param limit Specifies a maximum number of triples to return
#' @param offset Specifies how many triples should be skipped over
#' @param radiusUnit Defaults to km. Spefies the unit in which radius is given when using Haversine
#' @param useContext When true (default false), use the context (graph) field of the triple instead of its object
#' @param resource Name of the polygon to add
#' @param points A list of points represented as AllegroGraph geospatial object (created through createCartesianGeoLiteral)
#'
#' @name geospatial
#'
#' @examples
#'
listGeoTypes = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"geo/types")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' @rdname geospatial
#' @export
getCartesianGeoType = function(repository,stripWidth = NULL,xMin = NULL,xMax=NULL,yMin = NULL,yMax = NULL){
  queryargs = list(stripWidth = stripWidth,xmin = xMin,xmax = xMax,ymin = yMin,ymax = yMax)
  body = NULL
  url = paste0(repository$url,"geo/types/cartesian?")
  return(ag_post(service = repository,url = url,queryargs = queryargs,body = body))
}

#' @rdname geospatial
#' @export
getSphericalGeoType = function(repository,stripWidth = NULL, unit = 'degree',latMin = NULL,latMax=NULL,longMin = NULL,longMax = NULL){
  queryargs = list(stripWidth = stripWidth,unit = unit, latmin = latMin,latmax = latMax,longmin = longMin,longmax = longMax)
  body = NULL
  url = paste0(repository$url,"geo/types/spherical")
  return(ag_post(service = repository,url = url,queryargs = queryargs,body = body))
}

#' @rdname geospatial
#' @export
getStatementsInsideBox = function(repository,type,predicate,xMin,xMax,yMin,yMax,limit = NULL){
  queryargs = list(type = type, predicate = predicate, xmin = xMin,xmax = xMax,ymin = yMin,
                   ymax = yMax,limit = limit)
  body = NULL
  url = paste0(repository$url,"geo/box")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' @rdname geospatial
#' @export
getStatementInsideCircle = function(repository,type, predicate, x, y, radius, limit = NULL, offset = NULL){
  queryargs = list(type = type, predicate = predicate, x = x,y = y, radius= radius, limit = limit, offset = offset)
  body = NULL
  url = paste0(repository$url,"geo/circle")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' @rdname geospatial
#' @export
getStatementsHaversine = function(repository,type, predicate,lat,long,radius,radiusUnit = "km",limit = NULL,offset = NULL){
  queryargs = list(type = type, predicate = predicate, lat = lat,long = long, radius= radius, unit = radiusUnit,
                   limit = limit, offset = offset)
  body = NULL
  url = paste0(repository$url,"geo/haversine")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' @rdname geospatial
#' @export
createPolygon = function(repository, resource, points){
  queryargs = expandUrlArgs(list(resource = resource,point = points))
  body = NULL
  url = paste0(repository$url,"geo/polygon")
  invisible(ag_put(service = repository,url = url,queryargs = queryargs,body = body))
}

#' @rdname geospatial
#' @export
getStatementsInsidePolygon = function(repository,type,predicate,polygon,limit = NULL,offset = NULL){
  queryargs = list(type = type, predicate = predicate, polygon = polygon,limit = limit, offset = offset)
  body = NULL
  url = paste0(repository$url,"geo/polygon")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}
