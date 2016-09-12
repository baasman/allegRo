context("checking createService")

test_that(desc = "testing class of return object from createService",
          expect_equal(class(service(url = "http://localhost:10059/",
          user = "test", password = "xyzzy", testConnection = TRUE))[1], "service"))


