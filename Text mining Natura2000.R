# Eventually it would be nice to have some way to automatically extract data from
# the PDF versions of the management plans. This can be done using the xPDF suite of
# packages, but is more straightforward in Windows than Mac. Much of this comes from
# http://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

# Text mining Natura2000 plans ####
install.packages("tm")
library(tm)
setwd("/Users/christopherhassall/Dropbox/FAWKES II/Resources/Mgt plans (sample UK)")
files <- list.files(pattern = "pdf$")
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(files[1]), 
                   readerControl = list(reader = Rpdf))
content(pdf)[1:13]

lapply(files, function(i) system(paste("/Applications/xpdfbin-mac-3.04/pdftotext", paste0('"', i, '"')), wait = FALSE) )

