library(plyr)
library(mice)
library(VIM)
library(foreign)
library(survey)
library(dataMeta)

##see https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-178A
##for the database info

##load files
#2015
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h178assp.zip", temp <- tempfile())
unzipped_file = unzip(temp)
MEPS15 = read.xport(unzipped_file)
unlink(temp)

##Grab relevant variables (ID, Pharmacy Preferences, Therapeutic Class)
##Grab DUPERSID, PHARTP*, and all TC* variables
PHE15 <- MEPS15[,c(3,5,20:27,38:53)]

##replace -1 and -9 to NA
PHE15[PHE15==-1] <- NA
PHE15[PHE15==-9] <- NA
PHE15[PHE15==-8] <- NA
PH15[PH15==-7] <- NA

##ID which variables to drop
missing <- md.pattern(PHE15)
#use 80% rule (remove if >80% of entries are missing); not sure if this is the best value though
#dropping all variables except DUPERSID, RXRECIDX, TC1, TC1S1, TC1S1_1, PHARTP1, PHARTP2
PH15 <- PHE15[,c(1:4,11)]

#View missing patterns
aggr_plot15 <- aggr(PH15, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(PH15), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

##view correlations
cor(PH15[,c(3:5)],use="complete.obs")

##consolidate variables yay
# create new variable consolidating the variables 

library(data.table)
#PHARTT= combined total preference for brick/mortar vs indirect
#PHARTC = consolidate into brick/mortar vs indirect
PH15$PHARTT <- NA
PH15$PHARTT <- ifelse((PH15$PHARTP1 == 1 | PH15$PHARTP1 == 5), 1, PH15$PHARTT)
PH15$PHARTT <- ifelse((PH15$PHARTP1 == 2 | PH15$PHARTP1 == 3 | PH15$PHARTP1 == 4), 2, PH15$PHARTT)
PH15$PHARTT <- ifelse((PH15$PHARTP1 == 1 | PH15$PHARTP1 == 5) & (PH15$PHARTP2 == 2 | PH15$PHARTP2 == 3 | PH15$PHARTP2 == 4), 3, ifelse((PH15$PHARTP1 == 1 | PH15$PHARTP1 == 5), 1, PH15$PHARTT))

PH15$PHARTT_CAT<-ifelse(((PH15$PHARTP1 == 1 | PH15$PHARTP1 == 5) & (PH15$PHARTP2 == 1 | PH15$PHARTP2 == 5|is.na(PH15$PHARTP2))), 1, ifelse((PH15$PHARTP1<=4 & PH15$PHARTP1>=2) & ((PH15$PHARTP1<=4 & PH15$PHARTP1>=2) |is.na(PH15$PHARTP2)),2,3))

### chi square test for categorical
