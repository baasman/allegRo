context("testing repository functions")

test_that("testing various functions that operate on service object",{
  service = createService("http://localhost:10059",user = "test",password = "xyzzy",TRUE)
  createRepository(service,repo = "testRepo",override = TRUE)
  expect_true("testRepo" %in%  listRepositories(service)$return$id)
  deleteRepository(service,repo = "testRepo")
  expect_false("testRepo" %in%  listRepositories(service)$return$id)
})
