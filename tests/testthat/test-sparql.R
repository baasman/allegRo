context("testing sparql queries")


test_that("select, describe, and ask queries",{
  service = service("http://localhost:10059",user = "test",password = "xyzzy",TRUE)
  cat = catalog(service,"root")
  createRepository(cat,repo = "testRepo",override = TRUE)
  rep = repository(cat,"testRepo")

  addStatement(rep,"<http:a>", "<http:p>", '"a"', "<http:c1>")
  addStatement(rep,"<http:b>", "<http:p>", '"b"', "<http:c2>")

  #select
  lits = evalQuery(rep,"select ?literals {?s <http:p> ?literals }")$return
  expect_equal(c('"b"','"a"'),as.vector(unlist(lits)))
  expect_equal("literals",names(lits))

  litsFromC = evalQuery(rep,"select ?literals {?s <http:p> ?literals }",context = "<http:c1>")$return
  expect_equal('"a"',as.vector(unlist(litsFromC)))
  expect_equal("literals",names(litsFromC))

  #describe
  desc = evalQuery(rep,"describe ?s {?s ?p ?o}")$return
  expect_equal(2,nrow(desc))

  addStatement(rep,"<http:b>", "<http:p>", '"b"')
  desc = evalQuery(rep,"describe ?s {?s ?p ?o}")$return

  expect_equal(class(desc),"list")

  #ask
  expect_true(evalQuery(rep,"ask {?a ?b ?c}"))
  expect_false(evalQuery(rep,"ask {<http:d> ?b ?c }"))

  deleteRepository(cat,"testRepo")
})
