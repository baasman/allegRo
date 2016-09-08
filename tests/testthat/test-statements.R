context("testing statement functions")


test_that("testing various functions that operate on service object",{
  service = createService("http://localhost:10059",user = "test",password = "xyzzy",TRUE)
  createRepository(service,repo = "testRepo",override = TRUE)
  addStatement(service,repo = "testRepo",subj = "<a>",pred = "<p>",obj = '"a"',context = "<c1>")
  addStatement(service,repo = "testRepo",subj = "<b>",pred = "<p>",obj = '"b"',context = "<c2>")

  expect_equal(nrow(listContexts(service,repo = "testRepo")$return),2)
  expect_equal(nrow(getStatements(service,repo = "testRepo",context = "<c1>")$return),1)
  expect_equal(nrow(getStatements(service,repo = "testRepo",subj = list("<a>","<b>"))$return),2)
  expect_equal(nrow(getStatements(service,repo = "testRepo",subj = list("<x>","<y>"))$return),0)
  deleteStatements(service,repo = "testRepo",obj = '"a"')
  expect_equal(getSize(service,repo = "testRepo")$return,1)
  deleteRepository(service,repo = "testRepo")
})
