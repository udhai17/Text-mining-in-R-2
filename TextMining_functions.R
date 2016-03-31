# New York Times article search

library(bitops)
library(RCurl)
library(RJSONIO)
library(rlist)
library(stringr)
library(dplyr)
library(magrittr)
library(RTextTools)
library(tm)
library(ngram)

options(dplyr.print_min=100)

## Define functions
#

# Create a single row data frame from a list
dfrow.from.list = function(aList) { 
  data.frame(rbind(unlist(aList)),
             stringsAsFactors=FALSE)
}

# Retrieve a single page (with 10 articles) from the New York Times
get.nyt.page = function(page=0,          # page number (default: 0)
                        query.string="", # single word only (REQUIRED)
                        begin.date="",   # yyyymmdd (REQUIRED)
                        end.date=""      # yyyymmdd (REQUIRED)
) { # page=0; query.string="sports"; begin.date="20010101"; end.date="20151231"
  str_c(# create query string to send to NYT
        "http://api.nytimes.com", 
        "/svc/search/v2/articlesearch.json",
        "?api-key=",    articlesearch.key,
        "&q=",          str_replace_all(query.string," ","%20"),
        "&begin_date=", begin.date,
        "&end_date=",   end.date,
        "&page=",       page
  ) %>%
  {Sys.sleep(0.1); .} %>%    # wait 0.1s (rate limit 10/s)
  getURL() %>%             # retreive data from NYT
  fromJSON() %>%           # convert from JSON to an R list
  { .$response$docs } %>%  # retrieve only the documents
  list.select(             # keep only these four fields
      headline=as.character(headline["main"]), 
      snippet, 
      lead_paragraph, 
      abstract,
      pub_date) %>% 
  lapply(dfrow.from.list) %>% # convert each list item to a dataframe
  bind_rows                   # create a single dataframe
}

get.nyt.articles = function(pages=0, # vector of page numbers
                            query.string="", # single word only (REQUIRED)
                            begin.date="",   # yyyymmdd (REQUIRED)
                            end.date=""      # yyyymmdd (REQUIRED)
) { 
  lapply(pages, 
         get.nyt.page, 
         query.string=query.string,
         begin.date=begin.date,
         end.date=end.date
         ) %>% 
    bind_rows()
}

# Clean documents
clean.documents = function (document.vector) {
  document.vector %>% # document.vector = docs[93:94]
    tolower() %>%                           # change to lower case
    str_replace_all("'s","")            %>% # remove "'s"
    str_replace_all("’s","")            %>% # remove "’s"
    str_replace_all("\\.","")           %>% # remove periods
    str_replace_all("[[:digit:]]+"," ") %>% # change numbers to a space
    str_replace_all("[[:punct:]]"," ")  %>% # change punctuation to a space
    str_replace_all("[[:blank:]]+"," ") %>% # change white space to a space
    str_trim(side = "both")                 # remove spaces at the ends
}

# Create strings of n-grams
modify.words = function(document.vector, stem.words=FALSE, ngram.vector=1, stop.words=c()) {
  document.vector %>% # document.vector = docs.clean
    str_split("[[:space:]]") %>%            
    lapply(function(x) setdiff(x,stop.words)) %>%
    { if(stem.words) lapply(., wordStem) 
      else . 
    } %>% 
    lapply(function(x) { 
      ngrams(x,ngram.vector) %>%
        lapply( function(x) paste(x,collapse=".")) %>% 
        paste(collapse=" ") 
    })
}

# create.ngrams.remove.stopwords = function(document.vector, ngram.vector, stop.words) {
#   document.vector %>% # document.vector = docs.clean
#     str_split("[[:space:]]") %>%            # string to vector of words
#     lapply(wordStem) %>%                    # stem words (OPTION)
#     lapply(function(word.vector) {          # for each word.vector
#       word.vector %>%
#         match(stop.words,nomatch=FALSE) %>% # remove stop words
#         `!` %>%                             # remove stop words
#         { word.vector[.] } %>%              # remove stop words
#         ngrams(ngram.vector) %>%            # create n-grams
#         sapply(function(word.vec) {         # create n-gram words
#           paste(word.vec,collapse=".") 
#         }) %>%
#         paste(collapse=" ")                 # create string of n-grams
#     }) %>%
#     as.character() 
# }

reduce.dtm = function (dtm, freq.threshold) {
  word.counts=colSums(dtm)
  new.columns = names(word.counts)[freq.threshold<=word.counts]
  dtm[,new.columns]
}

# List the ten most common words in cluster i
TopWords = function (dtm, clusters, i) { # clusters=res$cluster; i=1
  dtm_names = colnames(dtm)
  row_count = sum(clusters==i)
  dtm_csums =
    apply(matrix(dtm[clusters==i,], nrow=row_count),
          2,
          mean)
  names(dtm_csums) = dtm_names
  dtm_ndx = order(dtm_csums, decreasing=TRUE)[1:10]
  bind_rows(
    data.frame(word=paste(c("[cluster ",formatC(i, format="f", digits=0),"]"), collapse=""),avg=NA),
    data.frame(word=paste(c("[",formatC(row_count, format="f", digits=0)," records]"), collapse=""),avg=NA),
    data.frame(word=dtm_names[dtm_ndx], avg=dtm_csums[dtm_ndx])
  )
}

check.clusters = function(cluster, count.min) { # cluster=res$cluster; count.min=3
  cluster.counts = table(cluster)
  as.numeric(names(cluster.counts)[cluster.counts >= count.min]) %>%
    lapply(function(clnum) { # clnum=1
      TopWords(dtm,cluster,clnum) 
    }) %>%
    bind_cols()
} 

view.dtm = function(cluster.number) {
  docs[res$cluster==cluster.number]
}
view.cluster = function(cluster.number) {
  docs[cluster==cluster.number]
}

#
# Define functions (END)