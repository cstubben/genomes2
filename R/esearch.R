esearch <-function(term, db="pubmed", usehistory="y", parse=TRUE, verbose=TRUE, showURL=FALSE, ...)
{
    # TO DO drop verbose and use message() instead of print()  and then suppressMessages(esearch)
   if(is.vector(term)) term <- paste(term, collapse=",")   # collapse=" OR " ?
   term  <- gsub(" ", "+", term)  # replace spaces
   email <- Sys.getenv("email")  # will be empty string if not set
   if(email == ""){print("WARNING: please set your email using Sys.setenv(email='name@email.com')" ) }
   opts <- c(email=email, tool="esearch.R", db=db, term=term, usehistory=usehistory, ...)
   if( any(duplicated(names(opts)))){ stop("Duplicated keys are not allowed in url strings")}
   opts <- paste( paste(names(opts), opts, sep="="), collapse="&")

   search <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
   search <- paste(search, opts, sep = "?")
   
   if(showURL) print(search)

   gp <- xmlParse( readLines(search) )
   # parse the output
   if(parse){
      # xvalue in genomes package 
      if( !is.na( xvalue(gp, "//ERROR") )){
         gp <- xvalue(gp, "//ERROR")
      }else{
         count <- as.numeric(xvalue(gp, "//Count"))
         if(count == 0){  
            if(verbose) print( "No results found.")
            gp <- NULL
            # xvalue(gp, "//OutputMessage")  #OR
         }else{
            # return history object for esummary or efetch
            if(usehistory == "y"){
               if(verbose){ 
                  if(count == 1){ print("1 result found") 
                  }else{         print(paste( count, "results found")) }
               }
               query <- xvalue(gp, "//QueryKey")
               web   <- xvalue(gp, "//WebEnv")
               gp <- data.frame( db= db, results=count,  query_key = query, WebEnv = web, stringsAsFactors=FALSE)
               class(gp) <- c("EntrezHistory", "data.frame")
            # or id list
            }else{
               retmax <- xvalue(gp, "//RetMax")
               if(verbose){
                  if(retmax == count){
                     if(count >1 ) print(paste( retmax, db, "ids returned"))
                     }else{         print(paste( retmax, " ", db, " ids returned (", count, " total ids)", sep="")) }
                  }
               # paste(xpathSApply(gp, "//Id", xmlValue), collapse=",")  # comma-separated?
               gp <- as.numeric( xpathSApply(gp, "//Id", xmlValue) )
            } 
         }
      }
   }
   gp 
}
