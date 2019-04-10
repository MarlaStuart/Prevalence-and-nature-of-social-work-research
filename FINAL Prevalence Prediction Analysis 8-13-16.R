###################################################################################################
###################################################################################################
# Clear enviornment
rm(list=ls())

### Unsupervised machine learning to determine if an abstract is a research article
###http://will-stanton.com/machine-learning-with-r-an-irresponsibly-fast-tutorial/
#install.packages("caret", dependencies = TRUE) # Classification And REgression Training
#install.packages("randomForest")
library(caret)
library(randomForest)
library(binda)
library(crossval)

##################################################################
# predict using top ranked Tokens picked through cross validation THIS IS WHAT I ACTUALLY DID.
######## ALL THE OTHER STUFF I WAS TRYING
setwd("C:/Users/marlastuart/Dropbox/Journal Articles Analysis/Data/Final Set of Journal Data 3-8-16")
Xtrain <- read.table("TrainFull.csv", sep = ",", header = TRUE)
Ytrain <- factor(Xtrain$MarlaStudy, levels = c(0, 1), labels = c("Not a Study", "Study"))
myvars <- names(Xtrain) %in% c("ID", "MarlaStudy") # delete the ID and MarlaStudy variables
Xtrain <- Xtrain[!myvars]
Xtrain <- as.matrix(Xtrain)

# Identify most discriminating variables
br = binda.ranking(Xtrain, Ytrain)
pdf(file="fig2-ranking.pdf")
plot(br, top=59, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main = "59 Most Discriminating Tokens")
dev.off()

## crossvalidation analysis

# predict using specified Tokens
predfun1 = function(Xtrain, Ytrain, Xtest, Ytest, selTokens)
{
  binda.out = binda(Xtrain[, selTokens, drop=FALSE], Ytrain, verbose=FALSE)
  ynew = predict.binda(binda.out, Xtest[, selTokens, drop=FALSE], verbose=FALSE)$class 
  
  cm = confusionMatrix(Ytest, ynew, negative="Not a Study") 
  
  return(cm)  
}

# predict using top ranked Tokens
predfun2 = function(Xtrain, Ytrain, Xtest, Ytest, numTokens)
{
  bir = binda.ranking(Xtrain, Ytrain, verbose=FALSE)
  selTokens = bir[,"idx"][1:numTokens]
  
  cm = predfun1(Xtrain, Ytrain, Xtest, Ytest, selTokens)
  
  return(cm)  
}

#' We use 5-fold cross-validation (i.e. divide into 5 folds of 4 samples each )
#' and repeat 20 times

K=5
B=20


#' Find optimal number of Tokens (we compute errors for various numbers
#' of top ranked genes)

set.seed(12345)
numTokens = c(1:744)
simFun = function(i)
{
  cat("Number of Tokens:", i, "/n")
  cvp = crossval(predfun2, Xtrain, Ytrain, K=K, B=B, numTokens = i, verbose=FALSE)
  return( diagnosticErrors(cvp$stat) )
}

#' this may take some time:
cvsim = lapply(numTokens, simFun)
cvsim = do.call(rbind, cvsim)
binda.sim = cbind(numTokens,cvsim)

#' This shows accuracy, sensitivity, specificity, positive predictive value,
#' negative predictive value, and log-odds ratio 
#' (for definitions see help page ?diagnosticErrors)
binda.sim

save(binda.sim, file="binda.sim.rda")
write.table(binda.sim, file = "binda.sim.csv", col.names = TRUE, row.names = FALSE, sep = ",")
write.table(br, file = "br.csv", col.names = TRUE, row.names = FALSE, sep = ",")

#The diagnostic errors are computed as follows:
#  acc = (TP+TN)/(FP+TN+TP+FN)
# sens = TP/(TP+FN)
# spec = TN/(FP+TN)
# ppv = TP/(FP+TP)
# npv = TN/(TN+FN)
# lor = log(TP*TN/(FN*FP))


####################################
###Predict full datasets
library(binda)
setwd("C:/Users/marlastuart/Dropbox/Journal Articles Analysis/Data/Final Set of Journal Data 3-8-16")
fullData <- read.table("FINAL All used unique abstracts.csv", sep = ",", header = TRUE)
names(fullData)[1] <- "ID" #Fix first variable name
myvars59 <- c("study", "results", "this.study", "findings", "sample", "abstract", "participants", "data", "higher", "associated", "survey", "significantly", "method", "analyses", "analysis", "data.from", "regression", "critical", "lower", "compared", "longitudinal", "the.study", "effects", "these.findings", "interviews", "measures", "text", "modeling", "the.findings", "scale", "predicted", "logistic", "researcher", "outcomes", "significant", "subject", "measured", "interview", "current.study", "logistic.regression", "the.current.study", "historical", "database", "trial", "more.likely", "controlling.for", "factor.analysis", "predict", "framework", "randomized", "predictors", "present.study", "measure", "questionnaire", "correlated", "ratio", "less.likely", "statistically.significant", "rate")
fullDataMatrix59 <- fullData[myvars59]
fullDataMatrix59 <- as.matrix(fullDataMatrix59)

Xtrain <- read.table("TrainFull.csv", sep = ",", header = TRUE)
Ytrain <- factor(Xtrain$MarlaStudy, levels = c(0, 1), labels = c("Not a Study", "Study"))
myvars59 <- c("study", "results", "this.study", "findings", "sample", "abstract", "participants", "data", "higher", "associated", "survey", "significantly", "method", "analyses", "analysis", "data.from", "regression", "critical", "lower", "compared", "longitudinal", "the.study", "effects", "these.findings", "interviews", "measures", "text", "modeling", "the.findings", "scale", "predicted", "logistic", "researcher", "outcomes", "significant", "subject", "measured", "interview", "current.study", "logistic.regression", "the.current.study", "historical", "database", "trial", "more.likely", "controlling.for", "factor.analysis", "predict", "framework", "randomized", "predictors", "present.study", "measure", "questionnaire", "correlated", "ratio", "less.likely", "statistically.significant", "rate")
Xtrain <- Xtrain[myvars59]
Xtrain <- as.matrix(Xtrain)

set.seed(12345)
model59 <- binda(Xtrain, Ytrain)
predicted59 = predict.binda(model59, fullDataMatrix59)

summary(predicted59$class)
fullDataPredicted <- cbind(fullData$ID, predicted59$class)
write.table(fullDataPredicted, file = "fullDataPredicted.csv", col.names = TRUE, row.names = FALSE, sep = ",")

#######################
setwd("C:/Users/Marla Stuart/Dropbox/Qualifying Papers and Bibs/Intervention/Journal Articles Analysis/Data/Final Set of Journal Data 3-8-16")
trainSet <- read.table("Validation Training Data 8-9-16.csv", sep = ",", header = TRUE)
testSet <- read.table("Validation Test Data 8-9-16.csv", sep = ",", header = TRUE)

# Convert MarlaStudy to Factor 0=no, 1=yes
trainSet$Study <- factor(trainSet$MarlaStudy, levels = c(0, 1), labels = c("Not a Study", "Study"))
testSet$Study <- factor(testSet$MarlaStudy, levels = c(0, 1), labels = c("Not a Study", "Study"))


# Convert predictor variables to a matrix, required for binda classification
# Problem is that there are terms in the dictionary that do not show up in the valudation set. 
## Do they have high frequencies? Unlikely.
## Validation set and full set frequencies are virtually the same. There are no tokens with 0 frequency in validation
## set that have a frequency of even 1 in the full set. There are 46 variables with difference in frequency from .01 to .04
## and they are all in the validation set.

#Algorithms using individual tokens
myvars1 <- c("articlesToKeep")
myvars2 <- c("Study_Concepts")
myvars3 <- c("Design_Concepts", "Data_Concepts", "Analysis_Concepts")
myvars4 <- c("study", "results")
myvars5 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study")
myvars6 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study", "data.from", "regression", "lower", "these.findings", "higher", "interview", "survey", "predictors")
myvars7 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study", "data.from", "regression", "lower", "these.findings", "higher", "interview", "survey", "predictors",
             "questionnaire", "significantly", "scale", "sample", "analyses", "participants", "results", 
             "controlled", "the.analysis", "the.findings")
myvars8 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study", "data.from", "regression", "lower", "these.findings", "higher", "interview", "survey", "predictors",
             "questionnaire", "significantly", "scale", "sample", "analyses", "participants", "results", 
             "controlled", "the.analysis", "the.findings", "trial", "moderate", "psychometric", "probability", "compared", "interviews", "rate", "samples", "study")
myvars9 <- c("study")
myvars10 <- c("study", "results")
myvars11 <- c("study", "results", "findings")
myvars12 <- c("study", "results", "findings", "research")
myvars13 <- c("study", "results", "findings", "research", "data")
myvars14 <- c("study", "results", "findings", "research", "data", "document")
myvars15 <- c("study", "results", "findings", "research", "data", "document", "sample")
myvars16 <- c("study", "results", "research", "document", "this.study", "findings", "data", "analysis", "sample", "associated", 
              "significant", "studies", "participants", "effects", "outcomes", "method", "model", "higher", "significantly", "survey", 
              "measures", "the.study", "analyses", "compared", "association", "critical", "assessment", "theory", "data.from", "regression", 
              "interviews", "qualitative", "history", "lower", "control", "models", "evaluation", "the.findings", "longitudinal", 
              "these.findings", "scale", "measure", "empirical", "variables", "more.likely", "framework", "result", "explore", 
              "find", "complex", "theoretical", "range", "randomized", "describes", "modeling", "attitudes", "outcome", "predicted", 
              "validity", "evaluated", "explores", "trial", "evaluate", "logistic", "interview", "rate", "samples", "this.research", 
              "describe", "baseline", "prevalence", "measured", "predictors", "observed", "current.study", "logistic.regression", 
              "the.current.study", "questionnaire", "the.sample", "assessments", "the.research", "exploratory", "interaction", 
              "quantitative", "hypothesis", "tool", "controlling.for", "factor.analysis", "predict", "reliability", "present.study", 
              "explored", "described", "theories", "correlated", "ratio", "exploring", "less.likely", "statistically.significant", 
              "pilot", "cohort", "multivariate", "independent", "evaluating", "analyze", "measurement", "the.data", "controlled", 
              "the.analysis", "scales", "network", "mediated", "random", "structural.equation", "moderate", "psychometric", 
              "systematic.review", "case.study", "text", "multilevel", "probability", "measuring", "surveys", "compare", "subjects", 
              "finding", "instruments", "tools", "confirmatory", "linear", "pilot.study", "reliable", "experiment", "we.conducted", 
              "instrument", "percent", "valid", "interactions", "explain", "structural.equation.modeling", "confirmatory.factor.analysis",
              "quasiexperimental", "ratings", "comparing", "fidelity", "paradigm", "historical", "crosssectional", "interval", "latent", 
              "questionnaires", "dependence", "hypotheses", "spatial", "cause", "studied", "interviewed", "moderated", "variance", 
              "descriptive", "metaanalysis", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", 
              "observations", "narrative", "metaanalyses", "participatory", "databases", "complexity", "mediate", "mediating", 
              "semistructured", "causes", "explaining", "regression.models", "validate", "explained", "rating", "methodology", 
              "subject", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "bivariate", 
              "sampling", "thematic", "describing", "multisite", "resulted", "posttest", "retrospective", "variable", "cohorts", 
              "case.studies", "qualitative.research", "ethnographic", "vignettes", "statistics", "interviewing", "researcher", "coded", 
              "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", 
              "semistructured.interviews", "subscale", "causal", "thematic.analysis", "evaluates", "errors", "visual", "caused", 
              "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", 
              "coding", "correlation", "diary", "intervention.research", "observation", "rated", "census", "experiments", "explains", 
              "map", "code", "distribution", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", 
              "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", 
              "regressions", "response.rate", "webbased.survey", "comparative", "participatory.research", "secondary.data", 
              "secondary.data.analysis", "with.fidelity", "independence", "we.present", "systematic.reviews", "american.community.survey", 
              "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", 
              "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", 
              "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", 
              "this.sample", "vignette", "epistemology", "induction", "confidentiality", "documentation", "abstracts", "policy.analysis", 
              "realism", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", 
              "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", 
              "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", 
              "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "action.research", "categorical", 
              "chi", "epistemological", "mixed.methods", "participatory.action.research", "theorized", "life.history", "rct", 
              "critical.realism", "evaluator", "positivist", "postmodern", "theorize", "acs", "agency.data", "archiving", "associates", 
              "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", 
              "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", 
              "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", 
              "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", 
              "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", 
              "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate",
              "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", 
              "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", 
              "research.design", "abduction", "big.data", "biography", "closedended", "deduction", "ethnography", "external.validity", 
              "frankfurt.school", "grand.theory", "hermeneutic", "hermeneutics", "impact.assessments", "informed.consent", "naturalism", 
              "positivism", "retroduction", "universalism", "fixed.effects", "snowball", "treatment.effects", "confidential")
myvars17 <- c("results", "this.study", "sample", "study", "findings")
myvars18 <- c("this.study")
myvars19 <- c("Study_Concepts", "Design_Concepts", "Data_Concepts", "Analysis_Concepts")
myvars29 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study", "data.from", "regression", "lower", "these.findings", "higher", "interview", "survey", "predictors",
              "questionnaire", "significantly", "scale", "sample", "analyses", "participants", "results", 
              "controlled", "the.analysis", "the.findings", "trial", "moderate", "psychometric", "probability", "compared", "interviews", "rate", "samples", "study", "findings", "baseline", "prevalence", "we.conducted", "the.sample", "randomized" )              
myvars30 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study", "data.from", "regression", "lower", "these.findings", "higher", "interview", "survey", "predictors",
              "questionnaire", "significantly", "scale", "sample", "analyses", "participants", "results", 
              "controlled", "the.analysis", "the.findings", "trial", "moderate", "psychometric", "probability", "compared", "interviews", "rate", "samples", "study", "findings", "baseline", "prevalence", "we.conducted", "the.sample", "randomized", "method", "hypothesis", "reliability", "dependence", "the.study", "more.likely")
myvars31 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study", "data.from", "regression", "lower", "these.findings", "higher", "interview", "survey", "predictors",
              "questionnaire", "significantly", "scale", "sample", "analyses", "participants", "results", 
              "controlled", "the.analysis", "the.findings", "trial", "moderate", "psychometric", "probability", "compared", "interviews", "rate", "samples", "study", "findings", "baseline", "prevalence", "we.conducted", "the.sample", "randomized", "method", "hypothesis", "reliability", "dependence", "the.study", "more.likely", "pilot", "this.research", "independent", "observations", "measure", "attitudes", "observed")

# Algorithms using Key Concept
myvars20 <- c("Longitudinal_methods", "Logistic_regression", "Mediation_A", "Factor_analysis", "Hierarchical_methods", "Structural_equation_models", "Focus_groups", "Latent_variables", "Online_survey", "Eligibility_A", "Propensity_score", "Simulation_A", "Cluster_analysis", "Grounded_theory", "Response_rate", "ANOVA_A", "Archival_A", "Cost_benefit_analysis", "Discourse_analysis", "Person_based")
myvars21 <- c("Longitudinal_methods", "Logistic_regression", "Mediation_A", "Factor_analysis", "Hierarchical_methods", "Structural_equation_models", "Focus_groups", "Latent_variables", "Online_survey", "Eligibility_A", "Propensity_score", "Simulation_A", "Cluster_analysis", "Grounded_theory", "Response_rate", "ANOVA_A", "Archival_A", "Cost_benefit_analysis", "Discourse_analysis", "Person_based", "Regression_A", "Prediction", "Moderation_A")
myvars22 <- c("Longitudinal_methods", "Logistic_regression", "Mediation_A", "Factor_analysis", "Hierarchical_methods", "Structural_equation_models", "Focus_groups", "Latent_variables", "Online_survey", "Eligibility_A", "Propensity_score", "Simulation_A", "Cluster_analysis", "Grounded_theory", "Response_rate", "ANOVA_A", "Archival_A", "Cost_benefit_analysis", "Discourse_analysis", "Person_based", "Regression_A", "Prediction", "Moderation_A", "Sample_A", "Survey_A")
myvars23 <- c("Study_Concepts", "Data_Concepts")
myvars24 <- c("Study_Concepts", "Data_Concepts", "Analysis_Concepts")
myvars25 <- c("Study_Concepts", "Data_Concepts", "Analysis_Concepts", "Design_Concepts")
myvars26 <- c("Study_Concepts", "Data_Concepts", "Analysis_Concepts", "Design_Concepts", "Study_A")
myvars27 <- c("Study_Concepts", "Data_Concepts", "Analysis_Concepts", "Design_Concepts", "Study_A", "Outcomes_A")
myvars28 <- c("Study_Concepts", "Data_Concepts", "Analysis_Concepts", "Design_Concepts", "Study_A", "Outcomes_A", "Findings_A", "Analysis_A", "Sample_A", "Measurement_A", "Comparative_A", "Correlation_A", "Research_A", "Data_A", "Statistical_significance", "Document_review", "Models_A", "Subjects_A", "Survey_A", "Effects_A", "Exploratory_A", "Miscellaneous_analysis_terms", "Evaluation_A", "Interviews_A", "Theory_A", "Scale_A", "Control_A", "Regression_A", "Prediction", "Descriptive_A", "Experimental", "Longitudinal_methods", "Epistemology_A", "Causality_A", "Observational_methods", "Qualitative_A", "Rating_A", "Variable_A", "Validity_A", "Moderation_A", "Historical_methods", "Logistic_regression", "Reliability_A", "Hypotheses_A", "Mediation_A", "Interactions_A", "Factor_analysis", "Hierarchical_methods", "Complexity_A", "Tool_A", "Explanatory_A", "Narrative_approaches", "Pilot_test", "Case_based", "Structural_equation_models", "Focus_groups", "Quantitative_A", "Content_analysis", "Spatial_methods", "Systematic_review", "Coding_A", "Network_analysis", "Latent_variables", "Online_survey", "Meta_analysis", "Secondary_analysis", "Eligibility_A", "Fidelity_A", "Participatory_methods", "Propensity_score", "Simulation_A", "Intervention_research", "Cluster_analysis", "Census_A", "Measurement_Error", "Grounded_theory", "Response_rate", "Chi_square", "Mixed_method", "Informed_consent", "Ethnographic_A", "Text_analysis", "ANOVA_A", "Archival_A", "Visual_methods", "Cost_benefit_analysis", "Discourse_analysis", "Person_based", "Biographical_A", "Policy_analysis", "Inference_A")
myvars32 <- c("study", "results", "research", "document", "this.study", "findings", "data", "analysis", "sample", "associated", 
              "significant", "studies", "participants", "effects", "outcomes", "method", "model", "higher", "significantly", "survey", 
              "measures", "the.study", "analyses", "compared", "association", "critical", "assessment", "theory", "data.from", "regression", 
              "interviews", "qualitative", "history", "lower", "control", "models", "evaluation", "the.findings", "longitudinal", 
              "these.findings", "scale", "measure", "empirical", "variables", "more.likely", "framework", "result", "explore", 
              "find", "complex", "theoretical", "range", "randomized", "describes", "modeling", "attitudes", "outcome", "predicted", 
              "validity", "evaluated", "explores", "trial", "evaluate", "logistic", "interview", "rate", "samples", "this.research", 
              "describe", "baseline", "prevalence", "measured", "predictors", "observed", "current.study", "logistic.regression", 
              "the.current.study", "questionnaire", "the.sample", "assessments", "the.research", "exploratory", "interaction", 
              "quantitative", "hypothesis", "tool", "controlling.for", "factor.analysis", "predict", "reliability", "present.study", 
              "explored", "described", "theories", "correlated", "ratio", "exploring", "less.likely", "statistically.significant", 
              "pilot", "cohort", "multivariate", "independent", "evaluating", "analyze", "measurement", "the.data", "controlled", 
              "the.analysis", "scales", "network", "mediated", "random", "structural.equation", "moderate", "psychometric", 
              "systematic.review", "case.study", "text", "multilevel", "probability", "measuring", "surveys", "compare", "subjects", 
              "finding", "instruments", "tools", "confirmatory", "linear", "pilot.study", "reliable", "experiment", "we.conducted", 
              "instrument", "percent", "valid", "interactions", "explain", "structural.equation.modeling", "confirmatory.factor.analysis",
              "quasiexperimental", "ratings", "comparing", "fidelity", "paradigm", "historical", "crosssectional", "interval", "latent", 
              "questionnaires", "dependence", "hypotheses", "spatial", "cause", "studied", "interviewed", "moderated", "variance", 
              "descriptive", "metaanalysis", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", 
              "observations", "narrative", "metaanalyses", "participatory", "databases", "complexity", "mediate", "mediating", 
              "semistructured", "causes", "explaining", "regression.models", "validate", "explained", "rating", "methodology", 
              "subject", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "bivariate", 
              "sampling", "thematic", "describing", "multisite", "resulted", "posttest", "retrospective", "variable", "cohorts", 
              "case.studies", "qualitative.research", "ethnographic", "vignettes", "statistics", "interviewing", "researcher", "coded", 
              "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", 
              "semistructured.interviews", "subscale", "causal", "thematic.analysis", "evaluates", "errors", "visual", "caused", 
              "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", 
              "coding", "correlation", "diary", "intervention.research", "observation", "rated", "census", "experiments", "explains", 
              "map", "code", "distribution", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", 
              "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", 
              "regressions", "response.rate", "webbased.survey", "comparative", "participatory.research", "secondary.data", 
              "secondary.data.analysis", "with.fidelity", "independence", "we.present", "systematic.reviews", "american.community.survey", 
              "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", 
              "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", 
              "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", 
              "this.sample", "vignette", "epistemology", "induction", "confidentiality", "documentation", "abstracts", "policy.analysis", 
              "realism", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", 
              "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", 
              "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", 
              "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "action.research", "categorical", 
              "chi", "epistemological", "mixed.methods", "participatory.action.research", "theorized", "life.history", "rct", 
              "critical.realism", "evaluator", "positivist", "postmodern", "theorize", "acs", "agency.data", "archiving", "associates", 
              "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", 
              "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", 
              "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", 
              "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", 
              "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", 
              "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate",
              "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", 
              "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", 
              "research.design", "abduction", "big.data", "biography", "closedended", "deduction", "ethnography", "external.validity", 
              "frankfurt.school", "grand.theory", "hermeneutic", "hermeneutics", "impact.assessments", "informed.consent", "naturalism", 
              "positivism", "retroduction", "universalism", "fixed.effects", "snowball", "treatment.effects", "confidential", "Study_Concepts", "Data_Concepts", "Analysis_Concepts", "Design_Concepts", "Study_A", "Outcomes_A", "Findings_A", "Analysis_A", "Sample_A", "Measurement_A", "Comparative_A", "Correlation_A", "Research_A", "Data_A", "Statistical_significance", "Document_review", "Models_A", "Subjects_A", "Survey_A", "Effects_A", "Exploratory_A", "Miscellaneous_analysis_terms", "Evaluation_A", "Interviews_A", "Theory_A", "Scale_A", "Control_A", "Regression_A", "Prediction", "Descriptive_A", "Experimental", "Longitudinal_methods", "Epistemology_A", "Causality_A", "Observational_methods", "Qualitative_A", "Rating_A", "Variable_A", "Validity_A", "Moderation_A", "Historical_methods", "Logistic_regression", "Reliability_A", "Hypotheses_A", "Mediation_A", "Interactions_A", "Factor_analysis", "Hierarchical_methods", "Complexity_A", "Tool_A", "Explanatory_A", "Narrative_approaches", "Pilot_test", "Case_based", "Structural_equation_models", "Focus_groups", "Quantitative_A", "Content_analysis", "Spatial_methods", "Systematic_review", "Coding_A", "Network_analysis", "Latent_variables", "Online_survey", "Meta_analysis", "Secondary_analysis", "Eligibility_A", "Fidelity_A", "Participatory_methods", "Propensity_score", "Simulation_A", "Intervention_research", "Cluster_analysis", "Census_A", "Measurement_Error", "Grounded_theory", "Response_rate", "Chi_square", "Mixed_method", "Informed_consent", "Ethnographic_A", "Text_analysis", "ANOVA_A", "Archival_A", "Visual_methods", "Cost_benefit_analysis", "Discourse_analysis", "Person_based", "Biographical_A", "Policy_analysis", "Inference_A")

trainSetMatrix1 <- trainSet[myvars1]
trainSetMatrix2 <- trainSet[myvars2]
trainSetMatrix3 <- trainSet[myvars3]
trainSetMatrix4 <- trainSet[myvars4]
trainSetMatrix5 <- trainSet[myvars5]
trainSetMatrix6 <- trainSet[myvars6]
trainSetMatrix7 <- trainSet[myvars7]
trainSetMatrix8 <- trainSet[myvars8]
trainSetMatrix9 <- trainSet[myvars9]
trainSetMatrix10 <- trainSet[myvars10]
trainSetMatrix11 <- trainSet[myvars11]
trainSetMatrix12 <- trainSet[myvars12]
trainSetMatrix13 <- trainSet[myvars13]
trainSetMatrix14 <- trainSet[myvars14]
trainSetMatrix15 <- trainSet[myvars15]
trainSetMatrix16 <- trainSet[myvars16]
trainSetMatrix17 <- trainSet[myvars17]
trainSetMatrix18 <- trainSet[myvars18]
trainSetMatrix19 <- trainSet[myvars19]
trainSetMatrix20 <- trainSet[myvars20]
trainSetMatrix21 <- trainSet[myvars21]
trainSetMatrix22 <- trainSet[myvars22]
trainSetMatrix23 <- trainSet[myvars23]
trainSetMatrix24 <- trainSet[myvars24]
trainSetMatrix25 <- trainSet[myvars25]
trainSetMatrix26 <- trainSet[myvars26]
trainSetMatrix27 <- trainSet[myvars27]
trainSetMatrix28 <- trainSet[myvars28]
trainSetMatrix29 <- trainSet[myvars29]
trainSetMatrix30 <- trainSet[myvars30]
trainSetMatrix31 <- trainSet[myvars31]
trainSetMatrix32 <- trainSet[myvars32]

testSetMatrix1 <- testSet[myvars1]
testSetMatrix2 <- testSet[myvars2]
testSetMatrix3 <- testSet[myvars3]
testSetMatrix4 <- testSet[myvars4]
testSetMatrix5 <- testSet[myvars5]
testSetMatrix6 <- testSet[myvars6]
testSetMatrix7 <- testSet[myvars7]
testSetMatrix8 <- testSet[myvars8]
testSetMatrix9 <- testSet[myvars9]
testSetMatrix10 <- testSet[myvars10]
testSetMatrix11 <- testSet[myvars11]
testSetMatrix12 <- testSet[myvars12]
testSetMatrix13 <- testSet[myvars13]
testSetMatrix14 <- testSet[myvars14]
testSetMatrix15 <- testSet[myvars15]
testSetMatrix16 <- testSet[myvars16]
testSetMatrix17 <- testSet[myvars17]
testSetMatrix18 <- testSet[myvars18]
testSetMatrix19 <- testSet[myvars19]
testSetMatrix20 <- testSet[myvars20]
testSetMatrix21 <- testSet[myvars21]
testSetMatrix22 <- testSet[myvars22]
testSetMatrix23 <- testSet[myvars23]
testSetMatrix24 <- testSet[myvars24]
testSetMatrix25 <- testSet[myvars25]
testSetMatrix26 <- testSet[myvars26]
testSetMatrix27 <- testSet[myvars27]
testSetMatrix28 <- testSet[myvars28]
testSetMatrix29 <- testSet[myvars29]
testSetMatrix30 <- testSet[myvars30]
testSetMatrix31 <- testSet[myvars31]
testSetMatrix32 <- testSet[myvars32]

trainSetMatrix1 <- as.matrix(trainSetMatrix1)
trainSetMatrix2 <- as.matrix(trainSetMatrix2)
trainSetMatrix3 <- as.matrix(trainSetMatrix3)
trainSetMatrix4 <- as.matrix(trainSetMatrix4)
trainSetMatrix5 <- as.matrix(trainSetMatrix5)
trainSetMatrix6 <- as.matrix(trainSetMatrix6)
trainSetMatrix7 <- as.matrix(trainSetMatrix7)
trainSetMatrix8 <- as.matrix(trainSetMatrix8)
trainSetMatrix9 <- as.matrix(trainSetMatrix9)
trainSetMatrix10 <- as.matrix(trainSetMatrix10)
trainSetMatrix11 <- as.matrix(trainSetMatrix11)
trainSetMatrix12 <- as.matrix(trainSetMatrix12)
trainSetMatrix13 <- as.matrix(trainSetMatrix13)
trainSetMatrix14 <- as.matrix(trainSetMatrix14)
trainSetMatrix15 <- as.matrix(trainSetMatrix15)
trainSetMatrix16 <- as.matrix(trainSetMatrix16)
trainSetMatrix17 <- as.matrix(trainSetMatrix17)
trainSetMatrix18 <- as.matrix(trainSetMatrix18)
trainSetMatrix19 <- as.matrix(trainSetMatrix19)
trainSetMatrix20 <- as.matrix(trainSetMatrix20)
trainSetMatrix21 <- as.matrix(trainSetMatrix21)
trainSetMatrix22 <- as.matrix(trainSetMatrix22)
trainSetMatrix23 <- as.matrix(trainSetMatrix23)
trainSetMatrix24 <- as.matrix(trainSetMatrix24)
trainSetMatrix25 <- as.matrix(trainSetMatrix25)
trainSetMatrix26 <- as.matrix(trainSetMatrix26)
trainSetMatrix27 <- as.matrix(trainSetMatrix27)
trainSetMatrix28 <- as.matrix(trainSetMatrix28)
trainSetMatrix29 <- as.matrix(trainSetMatrix29)
trainSetMatrix30 <- as.matrix(trainSetMatrix30)
trainSetMatrix31 <- as.matrix(trainSetMatrix31)
trainSetMatrix32 <- as.matrix(trainSetMatrix32)

testSetMatrix1 <- as.matrix(testSetMatrix1)
testSetMatrix2 <- as.matrix(testSetMatrix2)
testSetMatrix3 <- as.matrix(testSetMatrix3)
testSetMatrix4 <- as.matrix(testSetMatrix4)
testSetMatrix5 <- as.matrix(testSetMatrix5)
testSetMatrix6 <- as.matrix(testSetMatrix6)
testSetMatrix7 <- as.matrix(testSetMatrix7)
testSetMatrix8 <- as.matrix(testSetMatrix8)
testSetMatrix9 <- as.matrix(testSetMatrix9)
testSetMatrix10 <- as.matrix(testSetMatrix10)
testSetMatrix11 <- as.matrix(testSetMatrix11)
testSetMatrix12 <- as.matrix(testSetMatrix12)
testSetMatrix13 <- as.matrix(testSetMatrix13)
testSetMatrix14 <- as.matrix(testSetMatrix14)
testSetMatrix15 <- as.matrix(testSetMatrix15)
testSetMatrix16 <- as.matrix(testSetMatrix16)
testSetMatrix17 <- as.matrix(testSetMatrix17)
testSetMatrix18 <- as.matrix(testSetMatrix18)
testSetMatrix19 <- as.matrix(testSetMatrix19)
testSetMatrix20 <- as.matrix(testSetMatrix20)
testSetMatrix21 <- as.matrix(testSetMatrix21)
testSetMatrix22 <- as.matrix(testSetMatrix22)
testSetMatrix23 <- as.matrix(testSetMatrix23)
testSetMatrix24 <- as.matrix(testSetMatrix24)
testSetMatrix25 <- as.matrix(testSetMatrix25)
testSetMatrix26 <- as.matrix(testSetMatrix26)
testSetMatrix27 <- as.matrix(testSetMatrix27)
testSetMatrix28 <- as.matrix(testSetMatrix28)
testSetMatrix29 <- as.matrix(testSetMatrix29)
testSetMatrix30 <- as.matrix(testSetMatrix30)
testSetMatrix31 <- as.matrix(testSetMatrix31)
testSetMatrix32 <- as.matrix(testSetMatrix32)

# Set a random seed (so I always get the same results)
set.seed(42)

# Train the model using a "binda" algorithm -- appropriate for binary predictor variables
model1 <- binda(trainSetMatrix1, trainSet$MarlaStudy)
model2 <- binda(trainSetMatrix2, trainSet$MarlaStudy)
model3 <- binda(trainSetMatrix3, trainSet$MarlaStudy)
model4 <- binda(trainSetMatrix4, trainSet$MarlaStudy)
model5 <- binda(trainSetMatrix5, trainSet$MarlaStudy)
model6 <- binda(trainSetMatrix6, trainSet$MarlaStudy)
model7 <- binda(trainSetMatrix7, trainSet$MarlaStudy)
model8 <- binda(trainSetMatrix8, trainSet$MarlaStudy)
model8.1 <- binda(testSetMatrix8, testSet$MarlaStudy)

model9 <- binda(trainSetMatrix9, trainSet$MarlaStudy)
model10 <- binda(trainSetMatrix10, trainSet$MarlaStudy)
model11 <- binda(trainSetMatrix11, trainSet$MarlaStudy)
model12 <- binda(trainSetMatrix12, trainSet$MarlaStudy)
model13 <- binda(trainSetMatrix13, trainSet$MarlaStudy)
model14 <- binda(trainSetMatrix14, trainSet$MarlaStudy)
model15 <- binda(trainSetMatrix15, trainSet$MarlaStudy)
model16 <- binda(trainSetMatrix16, trainSet$MarlaStudy)
model17 <- binda(trainSetMatrix17, trainSet$MarlaStudy)
model18 <- binda(trainSetMatrix18, trainSet$MarlaStudy)
model19 <- binda(trainSetMatrix19, trainSet$MarlaStudy)
model20 <- binda(trainSetMatrix20, trainSet$MarlaStudy)
model21 <- binda(trainSetMatrix21, trainSet$MarlaStudy)
model22 <- binda(trainSetMatrix22, trainSet$MarlaStudy)
model23 <- binda(trainSetMatrix23, trainSet$MarlaStudy)
model24 <- binda(trainSetMatrix24, trainSet$MarlaStudy)
model25 <- binda(trainSetMatrix25, trainSet$MarlaStudy)
model26 <- binda(trainSetMatrix26, trainSet$MarlaStudy)
model27 <- binda(trainSetMatrix27, trainSet$MarlaStudy)
model28 <- binda(trainSetMatrix28, trainSet$MarlaStudy)
model29 <- binda(trainSetMatrix29, trainSet$MarlaStudy)
model30 <- binda(trainSetMatrix30, trainSet$MarlaStudy)
model31 <- binda(trainSetMatrix31, trainSet$MarlaStudy)
model32 <- binda(trainSetMatrix32, trainSet$MarlaStudy)

model.ranking1 <- binda.ranking(trainSetMatrix1, trainSet$MarlaStudy)
model.ranking2 <- binda.ranking(trainSetMatrix2, trainSet$MarlaStudy)
model.ranking3 <- binda.ranking(trainSetMatrix3, trainSet$MarlaStudy)
model.ranking4 <- binda.ranking(trainSetMatrix4, trainSet$MarlaStudy)
model.ranking5 <- binda.ranking(trainSetMatrix5, trainSet$MarlaStudy)
model.ranking6 <- binda.ranking(trainSetMatrix6, trainSet$MarlaStudy)
model.ranking7 <- binda.ranking(trainSetMatrix7, trainSet$MarlaStudy)
model.ranking8 <- binda.ranking(trainSetMatrix8, trainSet$MarlaStudy)
model.ranking8.1 <- binda.ranking(testSetMatrix8, testSet$MarlaStudy)

model.ranking9 <- binda.ranking(trainSetMatrix9, trainSet$MarlaStudy)
model.ranking10 <- binda.ranking(trainSetMatrix10, trainSet$MarlaStudy)
model.ranking11 <- binda.ranking(trainSetMatrix11, trainSet$MarlaStudy)
model.ranking12 <- binda.ranking(trainSetMatrix12, trainSet$MarlaStudy)
model.ranking13 <- binda.ranking(trainSetMatrix13, trainSet$MarlaStudy)
model.ranking14 <- binda.ranking(trainSetMatrix14, trainSet$MarlaStudy)
model.ranking15 <- binda.ranking(trainSetMatrix15, trainSet$MarlaStudy)
model.ranking16 <- binda.ranking(trainSetMatrix16, trainSet$MarlaStudy)
model.ranking17 <- binda.ranking(trainSetMatrix17, trainSet$MarlaStudy)
model.ranking18 <- binda.ranking(trainSetMatrix18, trainSet$MarlaStudy)
model.ranking19 <- binda.ranking(trainSetMatrix19, trainSet$MarlaStudy)
model.ranking20 <- binda.ranking(trainSetMatrix20, trainSet$MarlaStudy)
model.ranking21 <- binda.ranking(trainSetMatrix21, trainSet$MarlaStudy)
model.ranking22 <- binda.ranking(trainSetMatrix22, trainSet$MarlaStudy)
model.ranking23 <- binda.ranking(trainSetMatrix23, trainSet$MarlaStudy)
model.ranking24 <- binda.ranking(trainSetMatrix24, trainSet$MarlaStudy)
model.ranking25 <- binda.ranking(trainSetMatrix25, trainSet$MarlaStudy)
model.ranking26 <- binda.ranking(trainSetMatrix26, trainSet$MarlaStudy)
model.ranking27 <- binda.ranking(trainSetMatrix27, trainSet$MarlaStudy)
model.ranking28 <- binda.ranking(trainSetMatrix28, trainSet$MarlaStudy)
model.ranking29 <- binda.ranking(trainSetMatrix29, trainSet$MarlaStudy)
model.ranking30 <- binda.ranking(trainSetMatrix30, trainSet$MarlaStudy)
model.ranking31 <- binda.ranking(trainSetMatrix31, trainSet$MarlaStudy)
model.ranking32 <- binda.ranking(trainSetMatrix32, trainSet$MarlaStudy)

plot(model.ranking1)
plot(model.ranking2)
plot(model.ranking3)
plot(model.ranking4)
plot(model.ranking5)
plot(model.ranking6)
plot(model.ranking7)

plot(model.ranking8, top=20, ylab = "", main = "Top 20 Predictor Variables")
plot(model.ranking8.1, top=20, ylab = "", main = "Top 20 Predictor Variables")

plot(model.ranking9)
plot(model.ranking10)
plot(model.ranking11)
plot(model.ranking12)
plot(model.ranking13)
plot(model.ranking14)
plot(model.ranking15)
plot(model.ranking16)
plot(model.ranking17)
plot(model.ranking18)
plot(model.ranking19)
plot(model.ranking20)
plot(model.ranking21)
plot(model.ranking22)
plot(model.ranking23)
plot(model.ranking24)
plot(model.ranking25)
plot(model.ranking26)
plot(model.ranking27)
plot(model.ranking28)
plot(model.ranking29)
plot(model.ranking30)
plot(model.ranking31)
plot(model.ranking32)

predicted1 <- predict(model1, testSetMatrix1)
predicted2 <- predict(model2, testSetMatrix2)
predicted3 <- predict(model3, testSetMatrix3)
predicted4 <- predict(model4, testSetMatrix4)
predicted5 <- predict(model5, testSetMatrix5)
predicted6 <- predict(model6, testSetMatrix6)
predicted7 <- predict(model7, testSetMatrix7)
predicted8 <- predict(model8, testSetMatrix8)
predicted8.1 <- predict(model8.1, trainSetMatrix8)


predicted9 <- predict(model9, testSetMatrix9)
predicted10 <- predict(model10, testSetMatrix10)
predicted11 <- predict(model11, testSetMatrix11)
predicted12 <- predict(model12, testSetMatrix12)
predicted13 <- predict(model13, testSetMatrix13)
predicted14 <- predict(model14, testSetMatrix14)
predicted15 <- predict(model15, testSetMatrix15)
predicted16 <- predict(model16, testSetMatrix16)
predicted17 <- predict(model17, testSetMatrix17)
predicted18 <- predict(model18, testSetMatrix18)
predicted19 <- predict(model19, testSetMatrix19)
predicted20 <- predict(model20, testSetMatrix20)
predicted21 <- predict(model21, testSetMatrix21)
predicted22 <- predict(model22, testSetMatrix22)
predicted23 <- predict(model23, testSetMatrix23)
predicted24 <- predict(model24, testSetMatrix24)
predicted25 <- predict(model25, testSetMatrix25)
predicted26 <- predict(model26, testSetMatrix26)
predicted27 <- predict(model27, testSetMatrix27)
predicted28 <- predict(model28, testSetMatrix28)
predicted29 <- predict(model29, testSetMatrix29)
predicted30 <- predict(model30, testSetMatrix30)
predicted31 <- predict(model31, testSetMatrix31)
predicted32 <- predict(model32, testSetMatrix32)

######################################### Trying a different method
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# a metric called "complexity" limits the splits to a reasonable number -- can be overriden with rpart.control
# http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
# https://www.r-bloggers.com/classification-tree-models/
# https://www.med.emory.edu/EMAC/curriculum/diagnosis/sensand.htm
## some papers that use binda: http://strimmerlab.org/software/binda/
# https://www.r-bloggers.com/binary-classification-a-comparison-of-titanic-proportions-between-logistic-regression-random-forests-and-conditional-trees/
# https://en.wikipedia.org/wiki/Sensitivity_and_specificity
model13a <- rpart(MarlaStudy ~ study + results + findings + research + data,
                  data = trainSet,
                  method = "class")
plot(model13a)
text(model13a)fancyRpartPlot(model13a)
predicted13a <- predict(model13a, testSet, type = "class")
summary(predicted13a)
#same findings as binda

model14a <- rpart(MarlaStudy ~ study + results + findings + research + data + document,
                  data = trainSet,
                  method = "class")
plot(model14a)
text(model14a)
fancyRpartPlot(model14a)
predicted14a <- predict(model14a, testSet, type = "class")
summary(predicted14a)

model15a <- rpart(MarlaStudy ~ study + results + findings + research + data + document + sample,
                  data = trainSet,
                  method = "class")
plot(model15a)
text(model15a)
fancyRpartPlot(model15a)
predicted15a <- predict(model15a, testSet, type = "class")
summary(predicted15a)

model17a <- rpart(MarlaStudy ~ results + this.study + sample + study + findings,
                  data = trainSet,
                  method = "class")
plot(model17a)
text(model17a)
fancyRpartPlot(model17a)
predicted17a <- predict(model17a, testSet, type = "class")
summary(predicted17a)

model19a <- rpart(MarlaStudy ~ Design_Concepts + Data_Concepts + Analysis_Concepts,
                  data = trainSet,
                  method = "class")
plot(model19a)
text(model19a)
fancyRpartPlot(model19a)
predicted19a <- predict(model19a, testSet, type = "class")
summary(predicted19a)

model16a <- rpart(MarlaStudy ~ the.current.study + questionnaire + the.sample + assessments + the.research + exploratory + interaction +
                    quantitative + hypothesis + tool + controlling.for + factor.analysis + predict + reliability + present.study +
                    explored + described + theories + correlated + ratio + exploring + less.likely + statistically.significant +
                    pilot + cohort + multivariate + independent + evaluating + analyze + measurement + the.data + controlled +
                    the.analysis + scales + network + mediated + random + structural.equation + moderate + psychometric +
                    systematic.review + case.study + text + multilevel + probability + measuring + surveys + compare + subjects +
                    finding + instruments + tools + confirmatory + linear + pilot.study + reliable + experiment + we.conducted +
                    instrument + percent + valid + interactions + explain + structural.equation.modeling + confirmatory.factor.analysis +
                    quasiexperimental + ratings + comparing + fidelity + paradigm + historical + crosssectional + interval + latent +
                    questionnaires + dependence + hypotheses + spatial + cause + studied + interviewed + moderated + variance +
                    descriptive + metaanalysis + exploratory.factor.analysis + focus.groups + hierarchical + moderating + subscales +
                    observations + narrative + metaanalyses + participatory + databases + complexity + mediate + mediating +
                    semistructured + causes + explaining + regression.models + validate + explained + rating + methodology +
                    subject + convenience.sample + eligible + participant + posttreatment + secondary.analysis + surveyed + bivariate +
                    sampling + thematic + describing + multisite + resulted + posttest + retrospective + variable + cohorts +
                    case.studies + qualitative.research + ethnographic + vignettes + statistics + interviewing + researcher + coded +
                    content.analysis + focus.group + online.survey + predicting + propensity + regression.model +
                    semistructured.interviews + subscale + causal + thematic.analysis + evaluates + errors + visual + caused +
                    cluster + moderates + propensity.score + random.effects + standard.deviation + stratified + this.analysis +
                    coding + correlation + diary + intervention.research + observation + rated + census + experiments + explains +
                    map + code + distribution + abstracted + cfa + control.for + grounded.theory + hierarchical.linear.modeling +
                    interpretive + mediates + modeled + moderation + multiple.regression + nonrandomized + pretreatment +
                    regressions + response.rate + webbased.survey + comparative + participatory.research + secondary.data + 
                    secondary.data.analysis + with.fidelity + independence + we.present + systematic.reviews + american.community.survey +
                    analysis.of.variance + chisquare + document.analysis + eligibility + eligibility.criteria + factor.analyses +
                    inductive + latent.class + latent.growth.curve + latent.variable + least.squares + longitudinal.research +
                    mapping + pretest + reliabilities + simulated + simulation + structural.equation.model + structured.interview +
                    this.sample + vignette + epistemology + induction + confidentiality + documentation + abstracts + policy.analysis +
                    realism + anonymity + archival + cluster.analysis + coefficient + confirmatory.factor.analyses + correlational +
                    differenceindifference + error + explanatory + hlm + intervention.study + longitudinal.design + mediation +
                    mixed.method + multilevel.model + multistage + observational + observe + ordinal + quartile +
                    statistical.significance + theme + treatment.effect + we.investigate + wilcoxon + action.research + categorical +
                    chi + epistemological + mixed.methods + participatory.action.research + theorized + life.history + rct +
                    critical.realism + evaluator + positivist + postmodern + theorize + acs + agency.data + archiving + associates +
                    biographical + causality + cba + cluster.sample + cluster.sampling + comparative.analysis + content.analyses +
                    convenience.samples + conversation.analysis + correlate + correlating + cost.benefit + critical.theory +
                    current.analysis + current.sample + deductive + fidelity.to.the.model + focused.group + hierarchical.linear.model +
                    interquartile.range + interviewer + item.response + log + maps + marginal.effects + maximum.likelihood.estimation +
                    modernity + narrative.approach + openended + ordinary.least.squares + paradigms + personcentered + piloted +
                    present.analysis + randomized.control.trial + randomized.control.trials + sem + semistructured.interview + simulate +
                    simulating + statistic + statistical.interactions + structural.equation.models + surveying + triangulation + ttest +
                    twostage + validates + we.studied + web.surveys + mapped + multimethod + pearson + present.sample +
                    research.design + abduction + big.data + biography + closedended + deduction + ethnography + external.validity +
                    frankfurt.school + grand.theory + hermeneutic + hermeneutics + impact.assessments + informed.consent + naturalism +
                    positivism + retroduction + universalism + fixed.effects + snowball + treatment.effects + confidential + 
                    study + results + research + document + this.study + findings + data + analysis + sample + associated +
                    significant + studies + participants + effects + outcomes + method + model + higher + significantly + survey +
                    measures + the.study + analyses + compared + association + critical + assessment + theory + data.from + regression +
                    interviews + qualitative + history + lower + control + models + evaluation + the.findings + longitudinal +
                    these.findings + scale + measure + empirical + variables + more.likely + framework + result + explore +
                    find + complex + theoretical + range + randomized + describes + modeling + attitudes + outcome + predicted +
                    validity + evaluated + explores + trial + evaluate + logistic + interview + rate + samples + this.research +
                    describe + baseline + prevalence + measured + predictors + observed + current.study + logistic.regression,
                  data = trainSet,
                  method = "class")
fancyRpartPlot(model16a)
predicted16a <- predict(model16a, testSet, type = "class")
summary(predicted16a)

model16b <- rpart(MarlaStudy ~ this.study + Control_A,
                  data = trainSet,
                  method = "class")
fancyRpartPlot(model16b)
predicted16b <- predict(model16b, testSet, type = "class")
summary(predicted16b)

model8a <- rpart(MarlaStudy ~ confidential + fixed.effects + snowball + treatment.effects + longitudinal + modeling + predicted + 
                   logistic + measured + current.study + logistic.regression + the.current.study + controlling.for + factor.analysis + 
                   predict + present.study + correlated + ratio + less.likely + statistically.significant + cohort + multivariate + 
                   mediated + random + structural.equation + multilevel + confirmatory + linear + pilot.study + experiment + 
                   structural.equation.modeling + confirmatory.factor.analysis + crosssectional + interval + latent + questionnaires + 
                   interviewed + moderated + variance + exploratory.factor.analysis + focus.groups + hierarchical + moderating + 
                   subscales + mediate + mediating + semistructured + convenience.sample + eligible + participant + posttreatment + 
                   secondary.analysis + surveyed + posttest + retrospective + variable + coded + content.analysis + focus.group + 
                   online.survey + predicting + propensity + regression.model + semistructured.interviews + subscale + caused + 
                   cluster + moderates + propensity.score + random.effects + standard.deviation + stratified + this.analysis + 
                   abstracted + cfa + control.for + grounded.theory + hierarchical.linear.modeling + interpretive + mediates +
                   modeled + moderation + multiple.regression + nonrandomized + pretreatment + regressions + response.rate + 
                   webbased.survey + american.community.survey + analysis.of.variance + chisquare + document.analysis + eligibility + 
                   eligibility.criteria + factor.analyses + inductive + latent.class + latent.growth.curve + latent.variable + 
                   least.squares + longitudinal.research + mapping + pretest + reliabilities + simulated + simulation + 
                   structural.equation.model + structured.interview + this.sample + vignette + anonymity + archival + 
                   cluster.analysis + coefficient + confirmatory.factor.analyses + correlational + differenceindifference + 
                   error + explanatory + hlm + intervention.study + longitudinal.design + mediation + mixed.method + 
                   multilevel.model + multistage + observational + observe + ordinal + quartile + statistical.significance + 
                   theme + treatment.effect + we.investigate + wilcoxon + acs + agency.data + archiving + associates + 
                   biographical + causality + cba + cluster.sample + cluster.sampling + comparative.analysis + content.analyses + 
                   convenience.samples + conversation.analysis + correlate + correlating + cost.benefit + critical.theory + 
                   current.analysis + current.sample + deductive + fidelity.to.the.model + focused.group + 
                   hierarchical.linear.model + interquartile.range + interviewer + item.response + log + maps + marginal.effects + 
                   maximum.likelihood.estimation + modernity + narrative.approach + openended + ordinary.least.squares + 
                   paradigms + personcentered + piloted + present.analysis + randomized.control.trial + randomized.control.trials + 
                   sem + semistructured.interview + simulate + simulating + statistic + statistical.interactions + 
                   structural.equation.models + surveying + triangulation + ttest + twostage + validates + we.studied + 
                   web.surveys + mapped + multimethod + pearson + present.sample + research.design + this.study + data.from + 
                   regression + lower + these.findings + higher + interview + survey + predictors + 
                   questionnaire + significantly + scale + sample + analyses + participants + results + 
                   controlled + the.analysis + the.findings + trial + moderate + psychometric + probability + 
                   compared + interviews + rate + samples + study,
                 data = trainSet,
                 method = "class")
fancyRpartPlot(model8a)
predicted8a <- predict(model8a, testSet, type = "class")
summary(predicted8a)
write.table(predicted8a, file = "predicted8a.csv", col.names = TRUE, row.names = FALSE, sep = ",")

model18a <- rpart(MarlaStudy ~ this.study,
                  data = trainSet,
                  method = "class")
fancyRpartPlot(model18a)
predicted18a <- predict(model8a, testSet, type = "class")
summary(predicted18a)
write.table(predicted18a, file = "predicted18a.csv", col.names = TRUE, row.names = FALSE, sep = ",")            

model1a <- rpart(MarlaStudy ~ articlesToKeep,
                 data = trainSet,
                 method = "class")
fancyRpartPlot(model1a)
predicted1a <- predict(model1a, testSet, type = "class")
summary(predicted1a)

## Model 6 and 8 are the best -- try with different methods
set.seed(42)
myvars6 <- c("confidential", "fixed.effects", "snowball", "treatment.effects", "longitudinal", "modeling", "predicted", "logistic", "measured", "current.study", "logistic.regression", "the.current.study", "controlling.for", "factor.analysis", "predict", "present.study", "correlated", "ratio", "less.likely", "statistically.significant", "cohort", "multivariate", "mediated", "random", "structural.equation", "multilevel", "confirmatory", "linear", "pilot.study", "experiment", "structural.equation.modeling", "confirmatory.factor.analysis", "crosssectional", "interval", "latent", "questionnaires", "interviewed", "moderated", "variance", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", "mediate", "mediating", "semistructured", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "posttest", "retrospective", "variable", "coded", "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", "semistructured.interviews", "subscale", "caused", "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", "regressions", "response.rate", "webbased.survey", "american.community.survey", "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", "this.sample", "vignette", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "acs", "agency.data", "archiving", "associates", "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate", "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", "research.design", "this.study", "data.from", "regression", "lower", "these.findings", "higher", "interview", "survey", "predictors")

## rpart
model6a.1 <- rpart(MarlaStudy ~ confidential + fixed.effects + snowball + treatment.effects + longitudinal + modeling + predicted + logistic + measured + current.study + logistic.regression + the.current.study + controlling.for + factor.analysis + predict + present.study + correlated + ratio + less.likely + statistically.significant + cohort + multivariate + mediated + random + structural.equation + multilevel + confirmatory + linear + pilot.study + experiment + structural.equation.modeling + confirmatory.factor.analysis + crosssectional + interval + latent + questionnaires + interviewed + moderated + variance + exploratory.factor.analysis + focus.groups + hierarchical + moderating + subscales + mediate + mediating + semistructured + convenience.sample + eligible + participant + posttreatment + secondary.analysis + surveyed + posttest + retrospective + variable + coded + content.analysis + focus.group + online.survey + predicting + propensity + regression.model + semistructured.interviews + subscale + caused + cluster + moderates + propensity.score + random.effects + standard.deviation + stratified + this.analysis + abstracted + cfa + control.for + grounded.theory + hierarchical.linear.modeling + interpretive + mediates + modeled + moderation + multiple.regression + nonrandomized + pretreatment + regressions + response.rate + webbased.survey + american.community.survey + analysis.of.variance + chisquare + document.analysis + eligibility + eligibility.criteria + factor.analyses + inductive + latent.class + latent.growth.curve + latent.variable + least.squares + longitudinal.research + mapping + pretest + reliabilities + simulated + simulation + structural.equation.model + structured.interview + this.sample + vignette + anonymity + archival + cluster.analysis + coefficient + confirmatory.factor.analyses + correlational + differenceindifference + error + explanatory + hlm + intervention.study + longitudinal.design + mediation + mixed.method + multilevel.model + multistage + observational + observe + ordinal + quartile + statistical.significance + theme + treatment.effect + we.investigate + wilcoxon + acs + agency.data + archiving + associates + biographical + causality + cba + cluster.sample + cluster.sampling + comparative.analysis + content.analyses + convenience.samples + conversation.analysis + correlate + correlating + cost.benefit + critical.theory + current.analysis + current.sample + deductive + fidelity.to.the.model + focused.group + hierarchical.linear.model + interquartile.range + interviewer + item.response + log + maps + marginal.effects + maximum.likelihood.estimation + modernity + narrative.approach + openended + ordinary.least.squares + paradigms + personcentered + piloted + present.analysis + randomized.control.trial + randomized.control.trials + sem + semistructured.interview + simulate + simulating + statistic + statistical.interactions + structural.equation.models + surveying + triangulation + ttest + twostage + validates + we.studied + web.surveys + mapped + multimethod + pearson + present.sample + research.design + this.study + data.from + regression + lower + these.findings + higher + interview + survey + predictors,
                   data = trainSet,
                   method = "class")
fancyRpartPlot(model6a.1)
predicted6a.1 <- predict(model6a.1, testSet, type = "class")
summary(predicted6a.1)

model6a.2 <- rpart(MarlaStudy ~ confidential + fixed.effects + snowball + treatment.effects + longitudinal + modeling + predicted + logistic + measured + current.study + logistic.regression + the.current.study + controlling.for + factor.analysis + predict + present.study + correlated + ratio + less.likely + statistically.significant + cohort + multivariate + mediated + random + structural.equation + multilevel + confirmatory + linear + pilot.study + experiment + structural.equation.modeling + confirmatory.factor.analysis + crosssectional + interval + latent + questionnaires + interviewed + moderated + variance + exploratory.factor.analysis + focus.groups + hierarchical + moderating + subscales + mediate + mediating + semistructured + convenience.sample + eligible + participant + posttreatment + secondary.analysis + surveyed + posttest + retrospective + variable + coded + content.analysis + focus.group + online.survey + predicting + propensity + regression.model + semistructured.interviews + subscale + caused + cluster + moderates + propensity.score + random.effects + standard.deviation + stratified + this.analysis + abstracted + cfa + control.for + grounded.theory + hierarchical.linear.modeling + interpretive + mediates + modeled + moderation + multiple.regression + nonrandomized + pretreatment + regressions + response.rate + webbased.survey + american.community.survey + analysis.of.variance + chisquare + document.analysis + eligibility + eligibility.criteria + factor.analyses + inductive + latent.class + latent.growth.curve + latent.variable + least.squares + longitudinal.research + mapping + pretest + reliabilities + simulated + simulation + structural.equation.model + structured.interview + this.sample + vignette + anonymity + archival + cluster.analysis + coefficient + confirmatory.factor.analyses + correlational + differenceindifference + error + explanatory + hlm + intervention.study + longitudinal.design + mediation + mixed.method + multilevel.model + multistage + observational + observe + ordinal + quartile + statistical.significance + theme + treatment.effect + we.investigate + wilcoxon + acs + agency.data + archiving + associates + biographical + causality + cba + cluster.sample + cluster.sampling + comparative.analysis + content.analyses + convenience.samples + conversation.analysis + correlate + correlating + cost.benefit + critical.theory + current.analysis + current.sample + deductive + fidelity.to.the.model + focused.group + hierarchical.linear.model + interquartile.range + interviewer + item.response + log + maps + marginal.effects + maximum.likelihood.estimation + modernity + narrative.approach + openended + ordinary.least.squares + paradigms + personcentered + piloted + present.analysis + randomized.control.trial + randomized.control.trials + sem + semistructured.interview + simulate + simulating + statistic + statistical.interactions + structural.equation.models + surveying + triangulation + ttest + twostage + validates + we.studied + web.surveys + mapped + multimethod + pearson + present.sample + research.design + this.study + data.from + regression + lower + these.findings + higher + interview + survey + predictors,
                   data = testSet,
                   method = "class")
fancyRpartPlot(model6a.2)
predicted6a.2 <- predict(model6a.2, trainSet, type = "class")
summary(predicted6a.2)

## ctree
library(party)
model6b.1 <- ctree(MarlaStudy ~ confidential + fixed.effects + snowball + treatment.effects + longitudinal + modeling + predicted + logistic + measured + current.study + logistic.regression + the.current.study + controlling.for + factor.analysis + predict + present.study + correlated + ratio + less.likely + statistically.significant + cohort + multivariate + mediated + random + structural.equation + multilevel + confirmatory + linear + pilot.study + experiment + structural.equation.modeling + confirmatory.factor.analysis + crosssectional + interval + latent + questionnaires + interviewed + moderated + variance + exploratory.factor.analysis + focus.groups + hierarchical + moderating + subscales + mediate + mediating + semistructured + convenience.sample + eligible + participant + posttreatment + secondary.analysis + surveyed + posttest + retrospective + variable + coded + content.analysis + focus.group + online.survey + predicting + propensity + regression.model + semistructured.interviews + subscale + caused + cluster + moderates + propensity.score + random.effects + standard.deviation + stratified + this.analysis + abstracted + cfa + control.for + grounded.theory + hierarchical.linear.modeling + interpretive + mediates + modeled + moderation + multiple.regression + nonrandomized + pretreatment + regressions + response.rate + webbased.survey + american.community.survey + analysis.of.variance + chisquare + document.analysis + eligibility + eligibility.criteria + factor.analyses + inductive + latent.class + latent.growth.curve + latent.variable + least.squares + longitudinal.research + mapping + pretest + reliabilities + simulated + simulation + structural.equation.model + structured.interview + this.sample + vignette + anonymity + archival + cluster.analysis + coefficient + confirmatory.factor.analyses + correlational + differenceindifference + error + explanatory + hlm + intervention.study + longitudinal.design + mediation + mixed.method + multilevel.model + multistage + observational + observe + ordinal + quartile + statistical.significance + theme + treatment.effect + we.investigate + wilcoxon + acs + agency.data + archiving + associates + biographical + causality + cba + cluster.sample + cluster.sampling + comparative.analysis + content.analyses + convenience.samples + conversation.analysis + correlate + correlating + cost.benefit + critical.theory + current.analysis + current.sample + deductive + fidelity.to.the.model + focused.group + hierarchical.linear.model + interquartile.range + interviewer + item.response + log + maps + marginal.effects + maximum.likelihood.estimation + modernity + narrative.approach + openended + ordinary.least.squares + paradigms + personcentered + piloted + present.analysis + randomized.control.trial + randomized.control.trials + sem + semistructured.interview + simulate + simulating + statistic + statistical.interactions + structural.equation.models + surveying + triangulation + ttest + twostage + validates + we.studied + web.surveys + mapped + multimethod + pearson + present.sample + research.design + this.study + data.from + regression + lower + these.findings + higher + interview + survey + predictors,
                   data = trainSet)
plot(model6b.1)
predicted6b.1 <- predict(model6b.1, testSet, type = "node")

model6b.2 <- ctree(MarlaStudy ~ confidential + fixed.effects + snowball + treatment.effects + longitudinal + modeling + predicted + logistic + measured + current.study + logistic.regression + the.current.study + controlling.for + factor.analysis + predict + present.study + correlated + ratio + less.likely + statistically.significant + cohort + multivariate + mediated + random + structural.equation + multilevel + confirmatory + linear + pilot.study + experiment + structural.equation.modeling + confirmatory.factor.analysis + crosssectional + interval + latent + questionnaires + interviewed + moderated + variance + exploratory.factor.analysis + focus.groups + hierarchical + moderating + subscales + mediate + mediating + semistructured + convenience.sample + eligible + participant + posttreatment + secondary.analysis + surveyed + posttest + retrospective + variable + coded + content.analysis + focus.group + online.survey + predicting + propensity + regression.model + semistructured.interviews + subscale + caused + cluster + moderates + propensity.score + random.effects + standard.deviation + stratified + this.analysis + abstracted + cfa + control.for + grounded.theory + hierarchical.linear.modeling + interpretive + mediates + modeled + moderation + multiple.regression + nonrandomized + pretreatment + regressions + response.rate + webbased.survey + american.community.survey + analysis.of.variance + chisquare + document.analysis + eligibility + eligibility.criteria + factor.analyses + inductive + latent.class + latent.growth.curve + latent.variable + least.squares + longitudinal.research + mapping + pretest + reliabilities + simulated + simulation + structural.equation.model + structured.interview + this.sample + vignette + anonymity + archival + cluster.analysis + coefficient + confirmatory.factor.analyses + correlational + differenceindifference + error + explanatory + hlm + intervention.study + longitudinal.design + mediation + mixed.method + multilevel.model + multistage + observational + observe + ordinal + quartile + statistical.significance + theme + treatment.effect + we.investigate + wilcoxon + acs + agency.data + archiving + associates + biographical + causality + cba + cluster.sample + cluster.sampling + comparative.analysis + content.analyses + convenience.samples + conversation.analysis + correlate + correlating + cost.benefit + critical.theory + current.analysis + current.sample + deductive + fidelity.to.the.model + focused.group + hierarchical.linear.model + interquartile.range + interviewer + item.response + log + maps + marginal.effects + maximum.likelihood.estimation + modernity + narrative.approach + openended + ordinary.least.squares + paradigms + personcentered + piloted + present.analysis + randomized.control.trial + randomized.control.trials + sem + semistructured.interview + simulate + simulating + statistic + statistical.interactions + structural.equation.models + surveying + triangulation + ttest + twostage + validates + we.studied + web.surveys + mapped + multimethod + pearson + present.sample + research.design + this.study + data.from + regression + lower + these.findings + higher + interview + survey + predictors,
                   data = testSet)
plot(model6b.2)
predicted6b.2 <- predict(model6b.2, trainSet, type = "node")

## binda
trainSetMatrix6 <- trainSet[myvars6]
trainSetMatrix6 <- as.matrix(trainSetMatrix6)
testSetMatrix6 <- testSet[myvars6]
testSetMatrix6 <- as.matrix(testSetMatrix6)
model6.1 <- binda(trainSetMatrix6, trainSet$MarlaStudy)
model6.2 <- binda(testSetMatrix6, testSet$MarlaStudy)

model8.1 <- binda(testSetMatrix8, testSet$MarlaStudy)
predicted8.1 <- predict(model8.1, trainSetMatrix8)
model.ranking8.1 <- binda.ranking(testSetMatrix8.1, testSet$MarlaStudy)

model.ranking6.1 <- binda.ranking(trainSetMatrix6, trainSet$MarlaStudy)
model.ranking6.2 <- binda.ranking(testSetMatrix6, testSet$MarlaStudy)
plot(model.ranking6.1)
plot(model.ranking6.2)
predicted6.1 <- predict(model6.1, testSetMatrix6)
predicted6.2 <- predict(model6.2, trainSetMatrix6)
summary(predicted6.1$class)
summary(predicted6.2$class)


# Add predicted values for each model to the test file
testSetModels <- cbind(testSet, 
                       predicted1$class, 
                       predicted2$class, 
                       predicted3$class, 
                       predicted4$class, 
                       predicted5$class, 
                       predicted6$class, 
                       predicted7$class, 
                       predicted8$class, 
                       predicted9$class, 
                       predicted10$class, 
                       predicted11$class, 
                       predicted12$class, 
                       predicted13$class, 
                       predicted14$class, 
                       predicted15$class, 
                       predicted16$class, 
                       predicted17$class, 
                       predicted18$class, 
                       predicted19$class, 
                       predicted20$class, 
                       predicted21$class, 
                       predicted22$class, 
                       predicted23$class, 
                       predicted24$class, 
                       predicted25$class, 
                       predicted26$class, 
                       predicted27$class, 
                       predicted28$class,
                       predicted29$class,
                       predicted30$class,
                       predicted31$class,
                       predicted32$class,
                       predicted13a,
                       predicted14a,
                       predicted15a,
                       predicted17a,
                       predicted19a,
                       predicted16a,
                       predicted16b,
                       predicted8a,
                       predicted18a,
                       predicted1a,
                       predicted6a.1,
                       predicted6b.1,
                       predicted6.1$class)

trainSetModels <- cbind(trainSet, 
                        predicted6a.2, 
                        predicted6b.2, 
                        predicted6.2$class)

# Write to file for more analysis in SPSS
write.table(testSetModels, file = "testSetModels.csv", col.names = TRUE, row.names = FALSE, sep = ",")
write.table(trainSetModels, file = "trainSetModels.csv", col.names = TRUE, row.names = FALSE, sep = ",")

######################copying Fiedler
##Create models
trainSet$Study <- factor(trainSet$MarlaStudy, levels = c(0, 1), labels = c("Not a Study", "Study"))
testSet$Study <- factor(testSet$MarlaStudy, levels = c(0, 1), labels = c("Not a Study", "Study"))

myvars16 <- c("study", "results", "research", "document", "this.study", "findings", "data", "analysis", "sample", "associated", 
              "significant", "studies", "participants", "effects", "outcomes", "method", "model", "higher", "significantly", "survey", 
              "measures", "the.study", "analyses", "compared", "association", "critical", "assessment", "theory", "data.from", "regression", 
              "interviews", "qualitative", "history", "lower", "control", "models", "evaluation", "the.findings", "longitudinal", 
              "these.findings", "scale", "measure", "empirical", "variables", "more.likely", "framework", "result", "explore", 
              "find", "complex", "theoretical", "range", "randomized", "describes", "modeling", "attitudes", "outcome", "predicted", 
              "validity", "evaluated", "explores", "trial", "evaluate", "logistic", "interview", "rate", "samples", "this.research", 
              "describe", "baseline", "prevalence", "measured", "predictors", "observed", "current.study", "logistic.regression", 
              "the.current.study", "questionnaire", "the.sample", "assessments", "the.research", "exploratory", "interaction", 
              "quantitative", "hypothesis", "tool", "controlling.for", "factor.analysis", "predict", "reliability", "present.study", 
              "explored", "described", "theories", "correlated", "ratio", "exploring", "less.likely", "statistically.significant", 
              "pilot", "cohort", "multivariate", "independent", "evaluating", "analyze", "measurement", "the.data", "controlled", 
              "the.analysis", "scales", "network", "mediated", "random", "structural.equation", "moderate", "psychometric", 
              "systematic.review", "case.study", "text", "multilevel", "probability", "measuring", "surveys", "compare", "subjects", 
              "finding", "instruments", "tools", "confirmatory", "linear", "pilot.study", "reliable", "experiment", "we.conducted", 
              "instrument", "percent", "valid", "interactions", "explain", "structural.equation.modeling", "confirmatory.factor.analysis",
              "quasiexperimental", "ratings", "comparing", "fidelity", "paradigm", "historical", "crosssectional", "interval", "latent", 
              "questionnaires", "dependence", "hypotheses", "spatial", "cause", "studied", "interviewed", "moderated", "variance", 
              "descriptive", "metaanalysis", "exploratory.factor.analysis", "focus.groups", "hierarchical", "moderating", "subscales", 
              "observations", "narrative", "metaanalyses", "participatory", "databases", "complexity", "mediate", "mediating", 
              "semistructured", "causes", "explaining", "regression.models", "validate", "explained", "rating", "methodology", 
              "subject", "convenience.sample", "eligible", "participant", "posttreatment", "secondary.analysis", "surveyed", "bivariate", 
              "sampling", "thematic", "describing", "multisite", "resulted", "posttest", "retrospective", "variable", "cohorts", 
              "case.studies", "qualitative.research", "ethnographic", "vignettes", "statistics", "interviewing", "researcher", "coded", 
              "content.analysis", "focus.group", "online.survey", "predicting", "propensity", "regression.model", 
              "semistructured.interviews", "subscale", "causal", "thematic.analysis", "evaluates", "errors", "visual", "caused", 
              "cluster", "moderates", "propensity.score", "random.effects", "standard.deviation", "stratified", "this.analysis", 
              "coding", "correlation", "diary", "intervention.research", "observation", "rated", "census", "experiments", "explains", 
              "map", "code", "distribution", "abstracted", "cfa", "control.for", "grounded.theory", "hierarchical.linear.modeling", 
              "interpretive", "mediates", "modeled", "moderation", "multiple.regression", "nonrandomized", "pretreatment", 
              "regressions", "response.rate", "webbased.survey", "comparative", "participatory.research", "secondary.data", 
              "secondary.data.analysis", "with.fidelity", "independence", "we.present", "systematic.reviews", "american.community.survey", 
              "analysis.of.variance", "chisquare", "document.analysis", "eligibility", "eligibility.criteria", "factor.analyses", 
              "inductive", "latent.class", "latent.growth.curve", "latent.variable", "least.squares", "longitudinal.research", 
              "mapping", "pretest", "reliabilities", "simulated", "simulation", "structural.equation.model", "structured.interview", 
              "this.sample", "vignette", "epistemology", "induction", "confidentiality", "documentation", "abstracts", "policy.analysis", 
              "realism", "anonymity", "archival", "cluster.analysis", "coefficient", "confirmatory.factor.analyses", "correlational", 
              "differenceindifference", "error", "explanatory", "hlm", "intervention.study", "longitudinal.design", "mediation", 
              "mixed.method", "multilevel.model", "multistage", "observational", "observe", "ordinal", "quartile", 
              "statistical.significance", "theme", "treatment.effect", "we.investigate", "wilcoxon", "action.research", "categorical", 
              "chi", "epistemological", "mixed.methods", "participatory.action.research", "theorized", "life.history", "rct", 
              "critical.realism", "evaluator", "positivist", "postmodern", "theorize", "acs", "agency.data", "archiving", "associates", 
              "biographical", "causality", "cba", "cluster.sample", "cluster.sampling", "comparative.analysis", "content.analyses", 
              "convenience.samples", "conversation.analysis", "correlate", "correlating", "cost.benefit", "critical.theory", 
              "current.analysis", "current.sample", "deductive", "fidelity.to.the.model", "focused.group", "hierarchical.linear.model", 
              "interquartile.range", "interviewer", "item.response", "log", "maps", "marginal.effects", "maximum.likelihood.estimation", 
              "modernity", "narrative.approach", "openended", "ordinary.least.squares", "paradigms", "personcentered", "piloted", 
              "present.analysis", "randomized.control.trial", "randomized.control.trials", "sem", "semistructured.interview", "simulate",
              "simulating", "statistic", "statistical.interactions", "structural.equation.models", "surveying", "triangulation", "ttest", 
              "twostage", "validates", "we.studied", "web.surveys", "mapped", "multimethod", "pearson", "present.sample", 
              "research.design", "abduction", "big.data", "biography", "closedended", "deduction", "ethnography", "external.validity", 
              "frankfurt.school", "grand.theory", "hermeneutic", "hermeneutics", "impact.assessments", "informed.consent", "naturalism", 
              "positivism", "retroduction", "universalism", "fixed.effects", "snowball", "treatment.effects", "confidential")

trainSetMatrix16 <- trainSet[myvars16]
testSetMatrix16 <- testSet[myvars16]
trainSetMatrix16 <- as.matrix(trainSetMatrix16)
testSetMatrix16 <- as.matrix(testSetMatrix16)

set.seed(42)
model16.1 <- binda(trainSetMatrix16, trainSet$MarlaStudy)
model16.2 <- binda(testSetMatrix16, testSet$MarlaStudy)
predicted16.1 <- predict(model16.1, testSetMatrix16)
predicted16.2 <- predict(model16.2, trainSetMatrix16)

##Select most differentiating tokens
model.ranking16.1 <- binda.ranking(trainSetMatrix16, trainSet$MarlaStudy)
model.ranking16.2 <- binda.ranking(testSetMatrix16, testSet$MarlaStudy)

pdf(file="Ranking16.1.pdf")
plot(model.ranking16.1, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="30 Most Differentiating Tokens")
dev.off()
pdf(file="Ranking16.2.pdf")
plot(model.ranking16.2, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="30 Most Differentiating Tokens")
dev.off()

### Now use just the top 55 tokens from each model (top 40 of each model)
myvars16.55 <- c("analyses", "analysis", "associated", "attitudes", "compared", "controlling.for", "correlated", "critical", "current.study", "data", "data.from", "effects", "findings", "framework", "higher", "historical", "instruments", "interview", "interviewing", "interviews", "logistic", "logistic.regression", "longitudinal", "lower", "measured", "measures", "method", "modeling", "more.likely", "outcomes", "participants", "predict", "predicted", "predictors", "prevalence", "questionnaire", "rate", "ratio", "regression", "researcher", "results", "sample", "scale", "significant", "significantly", "statistics", "study", "subject", "survey", "text", "the.current.study", "the.findings", "the.study", "these.findings", "this.study")

trainSetMatrix16.55 <- trainSet[myvars16.55]
testSetMatrix16.55 <- testSet[myvars16.55]
trainSetMatrix16.55 <- as.matrix(trainSetMatrix16.55)
testSetMatrix16.55 <- as.matrix(testSetMatrix16.55)

set.seed(42)
model16.55.1 <- binda(trainSetMatrix16.55, trainSet$MarlaStudy)
model16.55.2 <- binda(testSetMatrix16.55, testSet$MarlaStudy)
predicted16.55.1 <- predict(model16.55.1, testSetMatrix16.55)
predicted16.55.2 <- predict(model16.55.1, trainSetMatrix16.55)


### Now use just the top 39 tokens from each model (top 30 of each model)
myvars16.39 <- c("analyses", "associated", "analysis", "compared", "critical", "current.study", "data", "data.from", "effects", "findings", "higher", "interview", "interviews", "logistic", "longitudinal", "lower", "measured", "measures", "method", "modeling", "outcomes", "participants", "predict", "predicted", "regression", "researcher", "results", "sample", "scale", "significant", "significantly", "study", "subject", "survey", "text", "the.findings", "the.study", "these.findings", "this.study")

trainSetMatrix16.39 <- trainSet[myvars16.39]
testSetMatrix16.39 <- testSet[myvars16.39]
trainSetMatrix16.39 <- as.matrix(trainSetMatrix16.39)
testSetMatrix16.39 <- as.matrix(testSetMatrix16.39)

set.seed(42)
model16.39.1 <- binda(trainSetMatrix16.39, trainSet$MarlaStudy)
model16.39.2 <- binda(testSetMatrix16.39, testSet$MarlaStudy)
predicted16.39.1 <- predict(model16.39.1, testSetMatrix16.39)
predicted16.39.2 <- predict(model16.39.1, trainSetMatrix16.39)

##Select most differentiating tokens
model.ranking16.39.1 <- binda.ranking(trainSetMatrix16.39.1, trainSet$MarlaStudy)
model.ranking16.39.2 <- binda.ranking(testSetMatrix16.39.2, testSet$MarlaStudy)

pdf(file="Ranking16.39.1.pdf")
plot(model.ranking16.39.1, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="30 Most Differentiating Tokens (from 39)")
dev.off()
pdf(file="Ranking16.39.2.pdf")
plot(model.ranking16.39.2, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="30 Most Differentiating Tokens (from 39)")
dev.off()

### Now just use top 24 tokens (top 20 of each model)
myvars16.24 <- c("analyses", "analysis", "associated", "compared", "critical", "data", "data.from", "effects", "findings", "higher", "longitudinal", "lower", "method", "participants", "regression", "results", "sample", "significantly", "study", "survey", "text", "the.findings", "the.study", "this.study")

trainSetMatrix16.24 <- trainSet[myvars16.24]
testSetMatrix16.24 <- testSet[myvars16.24]
trainSetMatrix16.24 <- as.matrix(trainSetMatrix16.24)
testSetMatrix16.24 <- as.matrix(testSetMatrix16.24)

set.seed(42)
model16.24.1 <- binda(trainSetMatrix16.24, trainSet$MarlaStudy)
model16.24.2 <- binda(testSetMatrix16.24, testSet$MarlaStudy)
predicted16.24.1 <- predict(model16.24.1, testSetMatrix16.24)
predicted16.24.2 <- predict(model16.24.2, trainSetMatrix16.24)


##Select most differentiating tokens
model.ranking16.24.1 <- binda.ranking(trainSetMatrix16.24.1, trainSet$MarlaStudy)
model.ranking16.24.2 <- binda.ranking(testSetMatrix16.24.2, testSet$MarlaStudy)


pdf(file="Ranking16.24.1.pdf")
plot(model.ranking16.24.1, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="24 Most Differentiating Tokens (from 24)")
dev.off()
pdf(file="Ranking16.24.2.pdf")
plot(model.ranking16.24.2, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="24 Most Differentiating Tokens (from 24)")
dev.off()

### Just the top 13 (top 10 from each model
myvars16.13 <- c("analysis", "associated", "critical", "data", "findings", "higher", "participants", "results", "sample", "significantly", "study", "survey", "this.study")

trainSetMatrix16.13 <- trainSet[myvars16.13]
testSetMatrix16.13 <- testSet[myvars16.13]
trainSetMatrix16.13 <- as.matrix(trainSetMatrix16.13)
testSetMatrix16.13 <- as.matrix(testSetMatrix16.13)

set.seed(42)
model16.13.1 <- binda(trainSetMatrix16.13, trainSet$MarlaStudy)
model16.13.2 <- binda(testSetMatrix16.13, testSet$MarlaStudy)
predicted16.13.1 <- predict(model16.13.1, testSetMatrix16.13)
predicted16.13.2 <- predict(model16.13.2, trainSetMatrix16.13)


##Select most differentiating tokens
model.ranking16.13.1 <- binda.ranking(testSetMatrix16.13, testSet$MarlaStudy)
model.ranking16.13.2 <- binda.ranking(trainSetMatrix16.13, trainSet$MarlaStudy)

pdf(file="Ranking16.13.1.pdf")
plot(model.ranking16.13.1, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="13 Most Differentiating Tokens")
dev.off()
pdf(file="Ranking16.13.2.pdf")
plot(model.ranking16.13.2, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="13 Most Differentiating Tokens")
dev.off()

### Just the top 6 (top 5 from each model
myvars16.6 <- c("associated", "findings", "results", "sample", "study", "this.study")

trainSetMatrix16.6 <- trainSet[myvars16.6]
testSetMatrix16.6 <- testSet[myvars16.6]
trainSetMatrix16.6 <- as.matrix(trainSetMatrix16.6)
testSetMatrix16.6 <- as.matrix(testSetMatrix16.6)

set.seed(42)
model16.6.1 <- binda(trainSetMatrix16.6, trainSet$MarlaStudy)
model16.6.2 <- binda(testSetMatrix16.6, testSet$MarlaStudy)
predicted16.6.1 <- predict(model16.6.1, testSetMatrix16.6)
predicted16.6.2 <- predict(model16.6.2, trainSetMatrix16.6)

##Select most differentiating tokens
model.ranking16.6.1 <- binda.ranking(trainSetMatrix16.6.1, trainSet$MarlaStudy)
model.ranking16.6.2 <- binda.ranking(testSetMatrix16.6.2, testSet$MarlaStudy)

pdf(file="Ranking16.6.1.pdf")
plot(model.ranking16.6.1, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="6 Most Differentiating Tokens")
dev.off()
pdf(file="Ranking16.6.2.pdf")
plot(model.ranking16.6.2, top=50, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main="6 Most Differentiating Tokens")
dev.off()

# Predict

testSetModels <- cbind(testSet, 
                       predicted16.1$class, 
                       predicted16.39.1$class, 
                       predicted16.24.1$class, 
                       predicted16.13.1$class, 
                       predicted16.6.1$class)

trainSetModels <- cbind(trainSet, 
                       predicted16.2$class, 
                       predicted16.39.2$class, 
                       predicted16.24.2$class, 
                       predicted16.13.2$class, 
                       predicted16.6.2$class)

write.table(testSetModels, file = "testSetModels.csv", col.names = TRUE, row.names = FALSE, sep = ",")
write.table(trainSetModels, file = "trainSetModels.csv", col.names = TRUE, row.names = FALSE, sep = ",")

### Now use just model 2
myvars16.30 <- c("study", "results", "this.study", "associated", "findings", "participants", "sample", "critical", "significantly", "higher", "survey", "the.study", "data", "analyses", "compared", "method", "longitudinal", "regression", "lower", "data.from", "these.findings", "measures", "interviews", "modeling", "subject", "interview", "outcomes", "researcher", "effects", "predict")
myvars16.20 <- c("study", "results", "this.study", "associated", "findings", "participants", "sample", "critical", "significantly", "higher", "survey", "the.study", "data", "analyses", "compared", "method", "longitudinal", "regression", "lower", "data.from")
myvars16.10 <- c("study", "results", "this.study", "associated", "findings", "participants", "sample", "critical", "significantly", "higher")
myvars16.5 <- c("study", "results", "this.study", "associated", "findings")
myvars16.4 <- c("study", "results", "this.study", "associated")
myvars16.3 <- c("study", "results", "this.study")
myvars16.2 <- c("study", "results")
myvars16.1 <- c("study")

trainSetMatrix16.30 <- trainSet[myvars16.30]
testSetMatrix16.30 <- testSet[myvars16.30]
trainSetMatrix16.20 <- trainSet[myvars16.20]
testSetMatrix16.20 <- testSet[myvars16.20]
trainSetMatrix16.10 <- trainSet[myvars16.10]
testSetMatrix16.10 <- testSet[myvars16.10]
trainSetMatrix16.5 <- trainSet[myvars16.5]
testSetMatrix16.5 <- testSet[myvars16.5]
trainSetMatrix16.4 <- trainSet[myvars16.4]
testSetMatrix16.4 <- testSet[myvars16.4]
trainSetMatrix16.3 <- trainSet[myvars16.3]
testSetMatrix16.3 <- testSet[myvars16.3]
trainSetMatrix16.2 <- trainSet[myvars16.2]
testSetMatrix16.2 <- testSet[myvars16.2]
trainSetMatrix16.1 <- trainSet[myvars16.1]
testSetMatrix16.1 <- testSet[myvars16.1]

trainSetMatrix16.30 <- as.matrix(trainSetMatrix16.30)
testSetMatrix16.30 <- as.matrix(testSetMatrix16.30)
trainSetMatrix16.20 <- as.matrix(trainSetMatrix16.20)
testSetMatrix16.20 <- as.matrix(testSetMatrix16.20)
trainSetMatrix16.10 <- as.matrix(trainSetMatrix16.10)
testSetMatrix16.10 <- as.matrix(testSetMatrix16.10)
trainSetMatrix16.5 <- as.matrix(trainSetMatrix16.5)
testSetMatrix16.5 <- as.matrix(testSetMatrix16.5)
trainSetMatrix16.4 <- as.matrix(trainSetMatrix16.4)
testSetMatrix16.4 <- as.matrix(testSetMatrix16.4)
trainSetMatrix16.3 <- as.matrix(trainSetMatrix16.3)
testSetMatrix16.3 <- as.matrix(testSetMatrix16.3)
trainSetMatrix16.2 <- as.matrix(trainSetMatrix16.2)
testSetMatrix16.2 <- as.matrix(testSetMatrix16.2)
trainSetMatrix16.1 <- as.matrix(trainSetMatrix16.1)
testSetMatrix16.1 <- as.matrix(testSetMatrix16.1)

set.seed(42)

model16.30 <- binda(testSetMatrix16.30, testSet$MarlaStudy)
predicted16.30 <- predict(model16.30, trainSetMatrix16.30)
model16.20 <- binda(testSetMatrix16.20, testSet$MarlaStudy)
predicted16.20 <- predict(model16.20, trainSetMatrix16.20)
model16.10 <- binda(testSetMatrix16.10, testSet$MarlaStudy)
predicted16.10 <- predict(model16.10, trainSetMatrix16.10)
model16.5 <- binda(testSetMatrix16.5, testSet$MarlaStudy)
predicted16.5 <- predict(model16.5, trainSetMatrix16.5)
model16.4 <- binda(testSetMatrix16.4, testSet$MarlaStudy)
predicted16.4 <- predict(model16.4, trainSetMatrix16.4)
model16.3 <- binda(testSetMatrix16.3, testSet$MarlaStudy)
predicted16.3 <- predict(model16.3, trainSetMatrix16.3)
model16.2 <- binda(testSetMatrix16.2, testSet$MarlaStudy)
predicted16.2 <- predict(model16.2, trainSetMatrix16.2)
model16.1 <- binda(testSetMatrix16.1, testSet$MarlaStudy)
predicted16.1 <- predict(model16.1, trainSetMatrix16.1)

trainSetModels3 <- cbind(trainSet, 
                        predicted16.30$class, 
                        predicted16.20$class, 
                        predicted16.10$class, 
                        predicted16.5$class,
                        predicted16.4$class,
                        predicted16.3$class,
                        predicted16.2$class,
                        predicted16.1$class)
                        
write.table(trainSetModels3, file = "trainSetModels3.csv", col.names = TRUE, row.names = FALSE, sep = ",")
write.table(predicted8.1$class, file = "predicted8.1.csv", col.names = TRUE, row.names = FALSE, sep = ",")
            
