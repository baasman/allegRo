# allegRo
An API for the semantic graph database, AllegroGraph.

To use this package, you'll need to install your own AllegroGraph server. To install a free version, follow this [link](http://franz.com/agraph/downloads/)
and follow the installation instructions.

###Installation

After you've set up a working version of AllegroGraph, download the package as follows. As of right now it is not available on CRAN.

To install the latest version of this package

```R
if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
devtools::install_github("baasman/allegRo")
```

### Getting started

We'll start off with creating a connection to the server, and specifying our credentials. For example, my test server is located on "localhost:10059", so that is where I'll be pointing it too. By specifying the testConnection = TRUE we can perform a simple get command to ensure we a receiving a response from the server.

```r
url = "http://localhost:10059/"
user = "baasman"
password = "mypassword"

service = createService(url,user,password,testConnection = TRUE)

#to view the specifications
print(service)
```

If you receive the message saying you can safely move on, we can proceed with the actual work.

Lets take a look at the different catalogs and repositories on this server. 

```r
listCatalogs(service)

listRepositories(service,catalogid = "root")
```

We could easily create a new repository and add some triples to it. First, we'll create a new repository under root called "repoTest"

```{r}
catalogid = "root"
repositoryid = "testRepo"
expectedSize = 100

#to get a full list of arguments, look at the help documentation
createRepository(service = service,catalogid = catalogid, repositoryid = repositoryid, expectedSize = expectedSize)
```

Secondly, suppose that want to add some triples to this store. There are multiple ways we could go about this, but for now, the easiest methods are either 
adding straight from a flat text file, or using a loop and dynamically changing the subject, predicate, or object.

```{r}

subject = "<http://test.com/tmp#person>"
predicate = "<http://test.com/tmp#hasItem>"
objects = c("<http://test.com/tmp#sword>","<http://test.com/tmp#shield>")

for(i in 1:2){
  addStatement(service,catalogid = "root",repositoryid = "testRepo",subj = subject,pred = predicate,
               obj = objects[i],context = "<testing>")
}

```

To add them from a text file, we can use the following function:

```{r}

addStatementsFromFile(service = service,catalogid = "root",repositoryid = "testRepo",
                      filepath = paste0(path.package("allegRo"),"/inst/extdata/testtrips.nq"),
                      context = "<testing>")

```

Finally, we can evaluate sparql queries on this test store. I already forgot what items our person has!

```{r}
returnType = "data.table" # can also be dataframe, matrix, list or plain vector
query = "select ?items {<http://test.com/tmp#person> <http://test.com/tmp#hasItem> ?items }"
cleanUp = TRUE #will remove all XMLSchema, convert appropriate types to R types.

evalQuery(service,catalogid = "root",repositoryid = "testRepo",returnType = returnType,
          query = query,limit = 10,cleanUp = cleanUp)
          

```

Another efficient way to return these triples is to match part of the triple pattern. Since all added triples are under the
"<testing>" context, we can return all statements in which the context == "<testing>"
```{r}
getStatements(service,repo = "testRepo",context = "<testing>", returnType = returnType, limit = 10, cleanUp = cleanUp)
```

As of right now, you could return triples by SPARQL queries, statement matching, and text indices.

