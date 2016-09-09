context("testing indexing fuctions")

test_that("testing free text indices functions",{
  service = createService("http://localhost:10059",user = "test",password = "xyzzy",TRUE)
  createRepository(service,repo = "testRepo",override = TRUE)
  addStatement(service,repo = "testRepo",subj = "<x>",pred = "<p>",obj = "\"foo bar quux rhubarb\"")
  addStatement(service,repo = "testRepo",subj = "<y>",pred = "<q>",obj = "\"foo bar quux rhubarb\"")
  addStatement(service,repo = "testRepo",subj = "<z>",pred = "<p>",obj = "\"foo bar quux rhubarb\"^^<type1>")
  createFreeTextIndex(service,repo = "testRepo",indexName = "index",predicate = "<p>",minimumWordSize = 4)

  expect_equal(listFreeTextIndices(service,repo = "testRepo")$return,"index")
  expect_equal(length(evalFreeTextSearch(service,repo = "testRepo",pattern = "foo")$return),0)
  expect_equal(nrow(evalFreeTextSearch(service,repo = "testRepo",pattern = "quux")$return),2)

  modifyFreeTextIndex(service,repo = "testRepo",indexName = "index",indexLiterals = "<type1>")

  expect_equal(nrow(evalFreeTextSearch(service,repo = "testRepo",index = "index",pattern = "rhubarb")$return),1)

  deleteFreeTextIndex(service,repo = "testRepo",indexName = "index")

  expect_equal(length(listFreeTextIndices(service,repo = "testRepo")$return),0)

  deleteRepository(service,repo = "testRepo")
})
