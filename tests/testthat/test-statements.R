context("testing statement functions")


test_that("testing various functions that operate on service object",{
  service = service("http://localhost:10059",user = "test",password = "xyzzy",TRUE)
  cat = catalog(service,"root")
  createRepository(cat,repo = "testRepo",override = TRUE)
  rep = repository(cat,"testRepo")
  addStatement(rep,"<a>", "<p>", '"a"', "<c1>")
  addStatement(rep,"<b>", "<p>", '"b"', "<c2>")

  expect_equal(nrow(listContexts(rep)$return),2)
  expect_equal(nrow(getStatements(rep,context = "<c1>")$return),1)
  expect_equal(nrow(getStatements(rep,subj = list("<a>","<b>"))$return),2)
  expect_equal(nrow(getStatements(rep,subj = list("<x>","<y>"))$return),0)
  deleteStatements(rep,obj = '"a"')
  expect_equal(getSize(rep)$return,1)
  deleteRepository(cat,repo = "testRepo")
})

