# allegRo
An API for the semantic graph database, AllegroGraph. As of right now, the package is very limited in terms of functionality, but I'm hoping to periodically
add as much functionality to it as time allows me to.

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

We'll start off with creating a connection to the server, and specifying our credentials. My server is located on "localhost:10059", so that is where
I'll be pointing it too. By specifying the testConnection = TRUE we can perform a simple get command to ensure we a receiving a response from the server.

```r
url = "http://localhost:10059/"
user = "baasman"
password = "mypassword"

service = createService(url,user,password,testConnection = TRUE)

#to view the specifications, look at the summary
summary(servive)
```

If you receive the message saying you can safely move on, we can proceed with the actual work.

Lets take a look at the different catalogs and repositories on this server. 

```r
listCatalogs(service)

listRepositories(service,catalogid = "root")
```

We could easily create a new repository and add some triples to it.

```{r}
catalogid = "root"
repositoryid = "repoTest"
expectedSize = 100

#to get a full list of arguments, look at the help documentation
createRepository(service = service,catalogid = catalogid, repositoryid = repositoryid, expectedSize = expectedSize)

```

