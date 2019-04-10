#Roberts, Stewarts, Tingley, stm: R Package for Structural Topic Models
library(stm)
library(Rtsne)
library(geometry)
library(Hmisc)
#library(wordcloud)
setwd("C:/Users/marlastuart/Dropbox/Journal Articles Analysis/Data")
data <- read.csv("FINAL data for topic model.csv")
names(data)
head(data)
names(data)[3] <- "documents"

# process the data
processed <- textProcessor(data$documents, 
                           lowercase=FALSE,
                           removestopwords=FALSE,
                           removenumbers=FALSE,
                           removepunctuation=FALSE,
                           stem=FALSE,
                           verbose=FALSE,
                           metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, verbose=FALSE)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

##Estimate best number of topics using evaluation statistics
set.seed(02138)
compare <- searchK(out$documents, out$vocab, K = c(5, 6, 7),
                   max.em.its = 100,
                   data = out$meta,
                   emtol = .001,
                   init.type = "Spectral")
set.seed(02138)
compare2 <- searchK(out$documents, out$vocab, K = c(8, 9, 10),
                   max.em.its = 100,
                   data = out$meta,
                   init.type = "Spectral",
                   emtol = .001)
set.seed(02138)
compare3 <- searchK(out$documents, out$vocab, K = c(3, 4, 15, 20),
                    max.em.its = 100,
                    data = out$meta,
                    init.type = "Spectral",
                    emtol = .001)

results <- rbind(compare3$results, compare2$results, compare$results)
results <- results[order(results$K) , ]

Fit3 <- stm(out$documents, out$vocab, K=3,
            max.em.its = 50,
            data = out$meta,
            init.type = "Spectral",
            ngroups = 3,
            emtol = .001)  #Spectral for large data sets

Fit4 <- stm(out$documents, out$vocab, K=4, 
            max.em.its = 50,
            data = out$meta,
            init.type = "Spectral",
            ngroups = 2,
            emtol = .001)  #Spectral for large data sets

Fit5 <- stm(out$documents, out$vocab, K=5,
            max.em.its = 50,
            data = out$meta,
            init.type = "Spectral",
            ngroups = 3,
            emtol = .001)  #Spectral for large data sets

Fit6 <- stm(out$documents, out$vocab, K=6, #this also crashes R every time -- why?
            max.em.its = 50,
            data = out$meta,
            ngroups = 4,
            init.type = "Spectral",
            emtol = .001)  #Spectral for large data sets

Fit7 <- stm(out$documents, out$vocab, K=7,
            max.em.its = 50,
            data = out$meta,
            init.type = "Spectral",
            ngroups = 4,
            emtol = .001)  #Spectral for large data sets

Fit8 <- stm(out$documents, out$vocab, K=8,
            max.em.its = 50,
            data = out$meta,
            init.type = "Spectral",
            ngroups = 4,
            emtol = .001)  #Spectral for large data sets

Fit9 <- stm(out$documents, out$vocab, K=9,
            max.em.its = 50,
            data = out$meta,
            init.type = "Spectral",
            ngroups = 3,
            emtol = .001)  #Spectral for large data sets

Fit10 <- stm(out$documents, out$vocab, K=10,
             max.em.its = 50,
             data = out$meta,
             init.type = "Spectral",
             ngroups = 3,
             emtol = .001)  #Spectral for large data sets

Fit15 <- stm(out$documents, out$vocab, K=15,
             max.em.its = 50,
             data = out$meta,
             init.type = "Spectral",
             ngroups = 3,
             emtol = .001)  #Spectral for large data sets

Fit20 <- stm(out$documents, out$vocab, K=20,
             max.em.its = 50,
             data = out$meta,
             init.type = "Spectral",
             ngroups = 3,
             emtol = .001)  #Spectral for large data sets

corrs3 <- topicCorr(Fit3, method = "simple", cutoff = 0.01)
corrs4 <- topicCorr(Fit4, method = "simple", cutoff = 0.01)
corrs5 <- topicCorr(Fit5, method = "simple", cutoff = 0.01)
corrs6 <- topicCorr(Fit6, method = "simple", cutoff = 0.01)
corrs7 <- topicCorr(Fit7, method = "simple", cutoff = 0.01)
corrs8 <- topicCorr(Fit8, method = "simple", cutoff = 0.01)
corrs9 <- topicCorr(Fit9, method = "simple", cutoff = 0.01)
corrs10 <- topicCorr(Fit10, method = "simple", cutoff = 0.01)
corrs15 <- topicCorr(Fit15, method = "simple", cutoff = 0.01)
corrs20 <- topicCorr(Fit20, method = "simple", cutoff = 0.01)

corrs3$cor[corrs3$cor == 1] <- NA
corrs4$cor[corrs4$cor == 1] <- NA
corrs5$cor[corrs5$cor == 1] <- NA
corrs6$cor[corrs6$cor == 1] <- NA
corrs7$cor[corrs7$cor == 1] <- NA
corrs8$cor[corrs8$cor == 1] <- NA
corrs9$cor[corrs9$cor == 1] <- NA
corrs10$cor[corrs10$cor == 1] <- NA
corrs15$cor[corrs15$cor == 1] <- NA
corrs20$cor[corrs20$cor == 1] <- NA

mean3 <- mean(corrs3$cor, na.rm = TRUE)
mean4 <- mean(corrs4$cor, na.rm = TRUE)
mean5 <- mean(corrs5$cor, na.rm = TRUE)
mean6 <- mean(corrs6$cor, na.rm = TRUE)
mean7 <- mean(corrs7$cor, na.rm = TRUE)
mean8 <- mean(corrs8$cor, na.rm = TRUE)
mean9 <- mean(corrs9$cor, na.rm = TRUE)
mean10 <- mean(corrs10$cor, na.rm = TRUE)
mean15 <- mean(corrs15$cor, na.rm = TRUE)
mean20 <- mean(corrs20$cor, na.rm = TRUE)

meanCorr <- rbind(mean3, mean4, mean5, mean6, mean7, mean8, mean9, mean10, mean15, mean20)
results <- cbind(results, meanCorr)

dev.off()
par(mfrow=c(3,3))
plot(corrs4)#, main = "4 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs5)#, main = "5 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs6)#, main = "6 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs7)#, main = "7 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs8)#, main = "8 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs9)#, main = "9 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs10)#, main = "10 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs15)#, main = "15 Topics", vlabels = "", vertex.color = "black") #this is good
plot(corrs20)#, main = "20 Topics", vlabels = "", vertex.color = "black") #this is good

dev.off()
par(mfrow=c(3,3))

graphics.off()
par("mar")
par(mar=c(1,1,1,1))
plot(results$K, results$heldout, xlab = "Number of Topics", ylab = "Heldout Likelihood", main = "Heldout (better = lower)")
ticks = c(3, 4, 5, 6, 7, 8, 9, 10, 15, 20)
axis(side = 1, at = ticks)

plot(results$K, results$residual, xlab = "Number of Topics", ylab = "Residuals", main = "Residuals (better = lower)")
ticks = c(3, 4, 5, 6, 7, 8, 9, 10, 15, 20)
axis(side = 1, at = ticks)

plot(results$K, results$exclus, xlab = "Number of Topics", ylab = "Exlusivity", main = "Exclusivity (better = higher)")
ticks = c(3, 4, 5, 6, 7, 8, 9, 10, 15, 20)
axis(side = 1, at = ticks)

plot(results$K, results$semcoh, xlab = "Number of Topics", ylab = "Coherence", main = "Coherence (better = lower)")
ticks = c(3, 4, 5, 6, 7, 8, 9, 10, 15, 20)
axis(side = 1, at = ticks)

plot(results$K, results$meanCorr, xlab = "Number of Topics", ylab = "Intra-Topic Correlation", main = "Intra-Topic Correlation (better = 0)")
ticks = c(3, 4, 5, 6, 7, 8, 9, 10, 15, 20)
axis(side = 1, at = ticks)
             
topics <- labelTopics(Fit4, n=20)
topics8 <- labelTopics(Fit8, n=88)
topics20 <- labelTopics(Fit20, n=88)


#Highest probablity, words with highest prbablity in the model
#FREX = frequent and exclusive, words that distinguish the topic
#lift, another way to select highest scoring works
#score, matrix of best words by score
#they all give the 7 top words for the category

# find highly illustrative examples of model -- returns the top ranked documents
thoughts1 <- findThoughts(Fit4, n=10)
plotQuote(thoughts1) #doesnt' give the quote but does give the abstract IDs
 
plot(Fit4) #top 3 words associated with each topic
plot(Fit4, labeltype = "frex") #top 3 unique words associated with each topic
plot.STM(Fit4, type = "hist") 
plot.STM(Fit4, type = "summary", labeltype = "frex")
plot.STM(Fit4, type = "hist", labeltype = "frex")

#Find words that load onto a single topic
checkBeta(Fit3, tolerance = 0.01) #change stm object
checkBeta(Fit4, tolerance = 0.01) #change stm object
checkBeta(Fit5, tolerance = 0.01) #no words load on a single topic
checkBeta(Fit6, tolerance = 0.01) #change stm object, 
checkBeta(Fit7, tolerance = 0.01) #change stm object, 
checkBeta(Fit8, tolerance = 0.01) #change stm object, 
checkBeta(Fit9, tolerance = 0.01) #change stm object, 
checkBeta(Fit10, tolerance = 0.01) #change stm object, 
checkBeta(Fit15, tolerance = 0.01) #change stm object, 
checkBeta(Fit20, tolerance = 0.01) #change stm object, 
# for 3-20 topics there are none that have words that load onto a single topic

## add data to dataset and do some other stuff
topic.probs.20 <- data.frame(Fit20$theta)
topic.probs$max <- apply(topic.probs.20, 1, max)
topic.probs$min <- apply(topic.probs.20, 1, min)
topic.probs$diff <- topic.probs$max - topic.probs$min
#topic.probs$max <- do.call(pmax, topic.probs[1:5]) another way to get max

dataFit20 <- cbind(data, topic.probs.20)


####Export data
write.csv(topics20, file = "Words 20 topics 88 words") #top 20 words for each topic
write.csv(dataFit20, file = "dataFit") #topic proportions for each document



