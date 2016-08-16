context("checking createService")

test_that(desc = "testing class of return object from createService",
          expect_equal(
            class(createService("http://localhost:10035/","baasman","password",testConnection = FALSE))[1],
            "service"))
