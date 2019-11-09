setwd("C:/Users/Khalid/Desktop/Data Science")
bankruptcy <- read.csv(file = 'C:/Users/Khalid/Desktop/Data Science/bankruptcy.csv')
head(bankruptcy)
### loading data
bankruptcy <- read.csv(file = 'C:/Users/Khalid/Desktop/Data Science/bankruptcy.csv')
head(bankruptcy)
bankruptcy <- na.omit(bankruptcy)
# remove colomn
bankruptcy$Company <- NULL
is.na(bankruptcy)
# remove na
newbankruptcy <- na.omit(bankruptcy)
# add new column SUM.TA
newbankruptcy$SUM.TA <- newbankruptcy$WC.TA + newbankruptcy$RE.TA
# add new column PRO.TA
newbankruptcy$PRO.TA <- newbankruptcy$EBIT.TA * newbankruptcy$S.TA
head(newbankruptcy)
# merge 2 dataframe
totalbankruptcy <- merge(bankruptcy, newbankruptcy)
head(totalbankruptcy)
# create new dataframe with 10 to 20 rows only
bankruptcy1 <- totalbankruptcy[10:20,]
# create new dataframe with 1 to 4 columns only
bankruptcy2 <- totalbankruptcy[,1:4]
# create new dataframe with WC.TA and EBIT.TA columns only
bankruptcy3 <- totalbankruptcy[,c("WC.TA", "EBIT.TA")]
# create new dataframe with RE.TA < -20 only
bankruptcy4 <- totalbankruptcy[which(totalbankruptcy$RE.TA < -20),]
# create new dataframe with RE.TA < -20 and bankrupt = 0 only
bankruptcy5 <- totalbankruptcy[which(totalbankruptcy$RE.TA < -20 & totalbankruptcy$Bankrupt == 0),]
# create new dataframe with RE.TA < -20 and bankrupt = 0 only and only columns with WC.TA and EBIT.TA
bankruptcy6 <- totalbankruptcy[which(totalbankruptcy$RE.TA < -20 & totalbankruptcy$Bankrupt == 0), c("WC.TA", "EBIT.TA")]
