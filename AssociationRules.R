###############################################################################
#book data set

book <- read.csv(file.choose())

head(book)
dim(book)

library(arules)

colnames(book)
str(book)

install.packages("arulesViz")
library(arulesViz)

### converting to transact for density value #####

book1<-read.transactions(book,format="basket")

inspect(book1[1:10])
class(book1)
summary(book1)

#transactions as item Matrix in sparse format with
#2001 rows (elements/itemsets/transactions) and
#321 columns (items) and a density of 0.003115265 

itemFrequencyPlot(book1,topN=20)


rules <- apriori(as.matrix(book))
arules::inspect(rules)
rules.sorted <- sort(rules,by="lift")
arules::inspect(rules.sorted)

book_rules <- apriori(as.matrix(book),parameter = list(support = 0.0002,confidence = 0.05))
summary(book_rules)

inspect(book_rules[1:5])
windows()
plot(book_rules,method = "scatterplot")
plot(book_rules,method = "grouped")
plot(book_rules,method = "graph")
plot(book_rules,method = "mosaic")

rules <- sort(book_rules,by="lift")

inspect(rules[1:4])


############################################################################


book_rules1 <- apriori(as.matrix(book),parameter = list(support = 0.002,confidence = 0.05))
summary(book_rules1)

inspect(book_rules1[1:5])
windows()
plot(book_rules1,method = "scatterplot")
plot(book_rules1,method = "grouped")
plot(book_rules1,method = "graph")
plot(book_rules,method = "mosaic")

rules <- sort(book_rules1,by="lift")

inspect(rules[1:4])

################################################################################### 

book_rules2 <- apriori(as.matrix(book),parameter = list(support = 0.007,confidence = 0.09))
summary(book_rules2)

inspect(book_rules2[1:5])
windows()
plot(book_rules2,method = "scatterplot")
plot(book_rules2,method = "grouped")
plot(book_rules2,method = "graph")
plot(book_rules2,method = "mosaic")

rules <- sort(book_rules2,by="lift")

inspect(rules[1:4])


################################################################################### 

book_rules3 <- apriori(as.matrix(book),parameter = list(minlen=2,support = 0.00005,confidence = 0.08))
summary(book_rules3)

inspect(book_rules3[1:5])
windows()
plot(book_rules3,method = "scatterplot")
plot(book_rules3,method = "grouped")
plot(book_rules3,method = "graph")
plot(book_rules3,method = "mosaic")

rules <- sort(book_rules3,by="lift")

inspect(rules[1:4])

# With change in minlen=2 , we are asking for atleast one combination on both lhs and rhs


###########################Movies Data###################################

moviesdata <- read.csv(file.choose())

head(moviesdata)

library(arules)

names(moviesdata)

rules <- apriori(moviesdata[,1:5])
arules::inspect(rules)
rules.sorted <- sort(rules,by="lift")
arules::inspect(rules.sorted)

###############################################################################
movies_rules1 <- apriori(moviesdata[,1:5],parameter = list(support=0.1,confidence=1))
summary(movies_rules1)

rules1 <- sort(movies_rules1,by="lift")
inspect(rules1[1:10])

################################################################################

movies_rules2 <- apriori(moviesdata[,1:5],parameter = list(support=0.6,confidence=1))
summary(movies_rules2)

rules2 <- sort(movies_rules2,by="lift")
inspect(rules2[1:10])
