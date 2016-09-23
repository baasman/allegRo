context("testing geo functions")

test_that("specific geo-functions"){
  service = service("http://localhost:10059/",user = "test",password = "xyzzy",TRUE)
  cat = catalog(service,"root")
  createRepository(cat,repo = "testRepo",override = TRUE)
  rep = repository(cat,"testRepo")

  type = getCartesianGeoType(rep,1,-100,100,-100,100)$return
  type2 = getSphericalGeoType(rep,5)$return
  pt = function(x,y) return(createCartesianGeoLiteral(type,x,y))
  pp = function(lat,lon) return(createSphericalGeoLiteral(type2,lat,lon))

  addStatement(rep,'"foo"','<http:at>',pt(1,1))
  addStatement(rep,'"bar"','<http:at>',pt(-2.5,3.4))
  addStatement(rep,'"baz"','<http:at>',pt(-1,1))
  addStatement(rep,'"bug"','<http:at>',pt(10,-2.4))

  expect_equal(2,nrow(getStatementsInsideBox(rep,type = type,predicate = "<http:at>",-10,0,0,10)$return))
  expect_equal(2,nrow(getStatementInsideCircle(rep,type = type,predicate = "<http:at>",0,0,2)$return))

  createPolygon(rep,resource = '"right"',points = list(pt(0, -100),pt(0, 100), pt(100, 100), pt(100, -100)))
  expect_equal(2,nrow(getStatementsInsidePolygon(rep,type,"<http:at>",'"right"')$return))

  addStatement(rep,'"Amsterdam"','<http:loc>', pp(52.366665, 4.883333))
  addStatement(rep,'"london"','<http:loc>',pp(51.533333, 0.08333333))
  addStatement(rep,'"San Francisco"','<http:loc>',pp(37.783333, -122.433334))
  addStatement(rep,'"Salvador"','<http:loc>',pp(-13.083333, -38.45))
  expect_equal(2,nrow(getStatementsHaversine(rep,type2,'<http:loc>',50,0,1000)$return))
  getStatementsInsideBox(rep,type2,'<http:loc>',.08,.09,51,52)


  deleteRepository(cat,"testRepo")
}
