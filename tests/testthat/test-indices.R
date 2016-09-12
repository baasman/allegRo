context("testing indexing fuctions")

test_that("testing free text indices functions",{
  service = service("http://localhost:10059/",user = "test",password = "xyzzy",TRUE)
  cat = catalog(service,"root")
  createRepository(cat,repo = "testRepo",override = TRUE)
  rep = repository(cat,"testRepo")
  addStatement(rep,"<x>", "<p>", "\"foo bar quux rhubarb\"")
  addStatement(rep, "<y>", "<q>", "\"foo bar quux rhubarb\"")
  addStatement(rep, "<z>", "<p>", "\"foo bar quux rhubarb\"^^<type1>")
  createFreeTextIndex(rep,indexName = "index",predicate = "<p>",minimumWordSize = 4)

  expect_equal(listFreeTextIndices(rep)$return,"index")
  expect_equal(length(evalFreeTextSearch(rep,pattern = "foo")$return),0)
  expect_equal(nrow(evalFreeTextSearch(rep,pattern = "quux")$return),2)

  modifyFreeTextIndex(rep,indexName = "index",indexLiterals = "<type1>")

  expect_equal(nrow(evalFreeTextSearch(rep,index = "index",pattern = "rhubarb")$return),1)

  deleteFreeTextIndex(rep,indexName = "index")

  expect_equal(length(listFreeTextIndices(rep)$return),0)

  deleteRepository(cat,"testRepo")
})
