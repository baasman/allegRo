context("testing session functions")

test_that("Testing session functions",{
  service = service("http://localhost:10059/",user = "test",password = "xyzzy",TRUE)
  cat = catalog(service,"root")
  createRepository(cat,repo = "testSession",override = TRUE)
  rep = repository(cat,"testSession")

  expect_equal(getSize(rep)$return,0)

  session = startSession(cat,repo = "testSession")
  addStatement(session,"<a>", "<p>", '"a"', "<c1>")

  expect_equal(getSize(session)$return,1)
  expect_equal(getSize(rep)$return,0)

  commit(session)

  expect_equal(getSize(rep)$return,1)

  deleteRepository(cat,repo = "testSession")
})
