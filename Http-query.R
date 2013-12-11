install.packages("RCurl")
install.packages("XML")
install.packages("plyr")
install.packages("kulife")

require("RCurl")
require("XML")
require("plyr")
require("kulife")

#######################
# Download PubMed XML #
#######################

# Search and fetch XML from PubMed
searchPubmed <- function(query.term) {
  # change spaces to + in query
  query.gsub <- gsub(" ", "+", query.term)
  # change single-quotes to URL-friendly %22
  query.gsub <- gsub("'","%22", query.gsub)
  # Perform search and save history, this will save PMIDS in history
  pub.esearch <- getURL(paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=",
                              query.gsub, "&usehistory=y", sep = ""))
  # Parse esearch XML
  pub.esearch <- xmlTreeParse(pub.esearch, asText = TRUE)
  # Count number of hits (super assign)
  pub.count <<- as.numeric(xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["Count"]]))
  # Save WebEnv-string, it contains "links" to all articles in my search
  pub.esearch <- xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["WebEnv"]])
  # Show how many articles that's being downloaded
  cat("Searching (downloading", pub.count, "articles)\n")
  
  ## We need to batch download, since efetch will cap at 10k articles ##
  # Start at 0
  RetStart <- 0
  # End at 10k
  RetMax <- 10000
  # Calculate how many itterations will be needed
  Runs <- (pub.count %/% 10000) + 1
  # Create empty object
  pub.efetch <- list(NULL)
  # Loop to batch download
  for (i in 1:Runs) {
    # Download XML based on hits saved in pub.esearch (WebEnv)
    tmp <- getURL(paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&WebEnv=",
                        pub.esearch,"&query_key=1&retmode=xml&retstart=", RetStart, "&retmax=", RetMax, sep = ""))
    pub.efetch[i] <- tmp
    RetStart <- RetStart + 10000
    RetMax <- RetMax + 10000
  }
  
  # Print that download is completed
  cat("Completed download from PubMed.\n")
  # Return XML
  return(pub.efetch)
}

# Function to extract journal name from individual article
extractLastNames <- function(query.term = query) {
  tmp <- lapply(1:length(pub.efetch), function(i) {
    # Parse XML into XML Tree
    xml.data <- xmlTreeParse(pub.efetch[[i]], useInternalNodes = TRUE)
    # Use xpathSApply to extract Journal name
    xpathSApply(xml.data, "//PubmedArticle/MedlineCitation/Article/AuthorList/Author/LastName", xmlValue)
  })
  LastNames <- unlist(tmp)
  # Show how many journals that were extracted
  cat("Extracted ", length(LastNames), " hits (",(length(LastNames)/pub.count)*100," %) from a total of ",
      pub.count," hits. For query named: ", query.term,"\n", sep="")
  # Create data frame with journal counts
  journal <- data.frame(count(LastNames))
  # Calculcate percent
    return(LastNames)
}

extractFirstNames <- function(query.term = query) {
  tmp <- lapply(1:length(pub.efetch), function(i) {
    # Parse XML into XML Tree
    xml.data <- xmlTreeParse(pub.efetch[[i]], useInternalNodes = TRUE)
    # Use xpathSApply to extract Journal name
    xpathSApply(xml.data, "//PubmedArticle/MedlineCitation/Article/AuthorList/Author/ForeName", xmlValue)
  })
  FirstNames <- unlist(tmp)
  # Show how many journals that were extracted
  cat("Extracted ", length(FirstNames), " hits (",(length(FirstNames)/pub.count)*100," %) from a total of ",
      pub.count," hits. For query named: ", query.term,"\n", sep="")
  # Create data frame with journal counts
  journal <- data.frame(count(FirstNames))
  # Calculcate percent
  return(FirstNames)
}

query <- c("cbt" = "'madhu mazumdar' AND 2012[DP]") 
pub.efetch <- searchPubmed(query) 
View(pub.efetch)

#creating dataframes of particular branches/items
LastNames <- extractLastNames() 
FirstNames <- extractFirstNames() 
cbind(FirstNames, LastNames)

#Understanding tree structure
xml=xmlTreeParse(pub.efetch[[1]], useInternalNodes = TRUE)
top=xmlRoot(xml) 
root=names(top) #name of root
print(root)[1:2]
childNodes=names(top[[1]]) #names of child nodes
names(childNodes[[1]]) #child nodes of root's child nodes 

#Turning entire xml into a dataframe
dataframe=ldply(xmlToList(xml), data.frame)#works 19 results are duplicated for some reason
dataframe2=xmlToDataFrame(xml)#works but only uses 2 columns for all the data

#Exporting xml 
write.xml(dataframe, file="Mazumdar2012(2).xml") #doesn't work b/c begins with invalid character, ".id" root node? 
write.xml(dataframe2, file="Mazumdar2012.xml") #Produces proper xml, but with the wrong attribute labels
