

#' List all mappings in repository
#' @description Return the datatype and predicate mappings for the repository.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @return A matrix containing the datatype and predicate mappings for the repository.
#' @export
listMappings = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"mapping")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete user-defined mappings in repository
#' @description Remove any user-defined datatype and predicate mappings.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @export
deleteMappings = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"mapping")
  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete all mappings in repository
#' @description Remove all datatype and predicate mappings (included the default ones).
#' @param repository Object of type repository specifying server details and repository to work on.
#' @export
deleteAllMappings = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"mapping/all")
  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Add new predicate mapping
#' @description Add a new predicate mapping between predicate and encoding.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param predicate String
#' @param encoding String
#' @export
addPredicateMapping = function(repository,predicate,encoding){
  queryargs = list(predicate = predicate,encoding = encoding)
  body = NULL
  url = paste0(repository$url,"mapping/predicate")
  invisible(ag_post(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Add new mapped type
#' @description Add a new datatype mapping between type and encoding.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param type String
#' @param encoding String
#' @export
addMappedType = function(repository,type,encoding){
  queryargs = list(type = type,encoding = encoding)
  body = NULL
  url = paste0(repository$url,"mapping/type")
  invisible(ag_post(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete a mapped type
#' @description Remove the datatype mapping between type and encoding.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param type String
#' @export
deleteMappedType = function(repository,type){
  queryargs = list(type = type)
  body = NULL
  url = paste0(repository$url,"mapping/type")
  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}





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






# service = service("http://localhost:10059",user = "test",password = "xyzzy",TRUE)
# cat = catalog(service,"root")
# createRepository(cat,repo = "testTemp",override = TRUE)
# rep = repository(cat,"testTemp")
# addMappedType(rep,"<time>","<http://www.w3.org/2001/XMLSchema#dateTime>")
# allegRo::addStatement(rep, "<x>", "<happened>", "\"2009-09-28T17:41:39\"^^<time>")
# allegRo::addStatement(rep, "<y>", "<happened>", "\"2009-09-28T18:22:00\"^^<time>")
# allegRo::addStatement(rep, "<z>", "<happened>", "\"2009-09-28T17:02:41\"^^<time>")
# s = getStatements(rep,obj=list("\"2009-09-28T17:00:00\"^^<time>", "\"2009-09-28T18:00:00\"^^<time>"),cleanUp = FALSE)
# getStatements(rep,pred = "<happened>",cleanUp = TRUE)$return



