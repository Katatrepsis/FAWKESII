# Eventually it would be nice to have some way to automatically extract data from
# the PDF versions of the management plans. This can be done using the xPDF suite of
# packages, but is more straightforward in Windows (which I successfully made work) 
# than Mac (which I did not). Much of this comes from an excellent guide to PDF text 
# extraction by Clay Ford at Virginia:
# http://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

# Text mining Natura2000 plans ####

# Install and load packages
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)

# Find location of PDFs - note that at the moment I am using the papers as an example
# because somebody removed all of the Natura 2000 management plans from Dropbox!
setwd("/Users/christopherhassall/Dropbox/FAWKES II/Literature")
setwd("C:\\Users\\fbscha\\Dropbox\\FAWKES II\\Literature")

# Retrieve a list of PDFs from that folder
files <- list.files(pattern = "pdf$")

# These functions extract text from the PDFs (which can be provided as a list) into a
# "corpus" object. Corpus objects require special functions to read them, but a few
# of these can be seen below. In this case, I select the 2nd, 3rd and 4th PDFs in the 
# list, because the first file (Anderson_2009.pdf) did not have searchable text.
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(files[c(2:4)]), 
                   readerControl = list(reader = Rpdf))

# The corpus object can then be "crunched" to find the frequency of words, after some
# pre-processing of the text. In this example, punctuation, stop words, case, word
# endings (leaving just "stems", hence "stemming") and numbers are removed. The result
# is a list of terms with their frequency in the text, which is known as a "term document
# matrix" (TDM).
opinions.tdm <- TermDocumentMatrix(opinions, control = list(removePunctuation = TRUE,
                                                            stopwords = TRUE,
                                                            tolower = TRUE,
                                                            stemming = TRUE,
                                                            removeNumbers = TRUE))

# The TDM can be inspected to view subsets of the table. Note that there are some terms
# which should be excluded, as they are garbled OCR coding (e.g. â€“).
inspect(opinions.tdm[1:10,]) 

# It can also be searched to find the most frequent terms, by specifying a range of
# frequencies by which to subset the TDM
findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)

# You can then find the frequency of those terms by using the frequent terms object
# "ft" to subset the TDM
ft <- findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)
inspect(opinions.tdm[ft,]) 

# Finally, it is possible to find the total frequency across a range of documents:
ft.tdm <- inspect(opinions.tdm[ft,])
apply(ft.tdm, 1, sum)

