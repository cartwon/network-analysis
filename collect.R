# MS&E382 Winter 2017
# Lab 1

######################################################################################
#
# Import the necessary libraries
#
######################################################################################
# install.packages('magrittr', repos = "http://cran.us.r-project.org")
# install.packages('igraph', repos = "http://cran.us.r-project.org")
# install.packages('httr', repos = "http://cran.us.r-project.org")
# install.packages('data.table', repos = "http://cran.us.r-project.org")
library(magrittr)
library(httr)
library(data.table)
library(igraph)
# Set your directory for the project
# setwd("/Users/dir")

######################################################################################
#
# Part I: Import data and create graph objects
#
######################################################################################

# Import your word list
name_of_file <- "ListOfWords.txt"
word_list <- read.table(name_of_file, stringsAsFactors = F) %>% unlist %>% as.vector
num_words <- length(word_list)

# Initialize results tables
total_table <- data.table()
network_table <- data.table()

# Outer for loop searches NYT for each word in the list
for (i in 1:(length(word_list))) {
    a <- word_list[i]
    url <- paste("http://query.nytimes.com/svc/cse/v2/sitesearch.json?query=%22",a,"%22&spotlight=true",sep="")
    # The number of results
    data <- content(GET(url))$results$meta$results_estimated_total %>% as.numeric
    # Put results in table
    if(length(data)==0){
        total_table <- rbindlist(l = list(total_table,list(a,0)))
    }else{
        total_table <- rbindlist(l = list(total_table,list(a,data)))
    }
    # Inner for loop searches NYT for each combination of words in list
    # This builds a valued network of co-occurrence in articles
    # Need to change this i depending on the length of your word list
    if (i < num_words) {
        for (j in (i+1):length(word_list)) {
            b <- word_list[j]
            print(c(i,j))
            print(c(a,b))
            url2 <- paste("http://query.nytimes.com/svc/cse/v2/sitesearch.json?query=%22",a,"%22%20%22",b,"%22&spotlight=true",sep="")
            # The number of results
            data2 <- content(GET(url2))$results$meta$results_estimated_total %>% as.numeric
            # Put results in table
            if(length(data2)==0){
                network_table <- rbindlist(l = list(network_table,list(a,b,0)))
            }else{
                network_table <- rbindlist(l = list(network_table,list(a,b,data2)))
            }
        }
    }
}

setnames(total_table,c(1,2),c("SearchTerm","ResultsTotal"))
setnames(network_table,c(1,2,3),c("SearchTerm1","SearchTerm2","CoOccurrences"))
network_table <- network_table[!CoOccurrences==0]
save(df, file='network.RData')
