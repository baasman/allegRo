context("testing repository functions")

test_that("testing various functions that operate on service object",{
  service = service("http://localhost:10059/",user = "test",password = "xyzzy",TRUE)
  cat = catalog(service,"root")
  createRepository(cat,repo = "testRepo",override = TRUE)
  expect_true("testRepo" %in%  listRepositories(cat,all = FALSE)$return$id)
  deleteRepository(cat,"testRepo")
  expect_false("testRepo" %in%  listRepositories(cat)$return$id)
})
