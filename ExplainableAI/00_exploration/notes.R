# Create some explanations
library(MASS)
iris_test <- iris[1, 1:4]
iris_train <- iris[-1, 1:4]
iris_lab <- iris[[5]][-1]
model <- lda(iris_train, iris_lab)
explanation <- lime(iris_train, model)
explanations <- explain(iris_test, explanation, n_labels = 3, n_features = 2)

# Get an overview with the standard plot
plot_features(explanations)




# Create some explanations
library(MASS)
iris_test <- iris[c(1, 51, 101), 1:4]
iris_train <- iris[-c(1, 51, 101), 1:4]
iris_lab <- iris[[5]][-c(1, 51, 101)]
model <- lda(iris_train, iris_lab)
explanation <- lime(iris_train, model)
explanations <- explain(iris_test, explanation, n_labels = 1, n_features = 2)

# Get an overview with the standard plot
plot_explanations(explanations)


library(wdl)

data("marioKart")
marioKart[, Date := as.Date(Time)]
marioKart[, Weekday := weekdays(Date)]
table(marioKart$Weekday, useNA = "ifany")
marioKart[, .N, by = Weekday]
marioKart[, Weekday := factorizeWeekdays(Weekday)]
table(marioKart$Weekday)
setclasses(marioKart, "character", "factor", useAsFunctions = TRUE)
marioKart[, Cup := rep(seq_len(.N / 4), each = 4)]
marioKart[, .(UniqueN = uniqueN(.SD)), by = Cup, 
          .SDcols = patterns("^Player|^Character")][, all(UniqueN == 1)]
cups <- marioKart[, c(lapply(mget(grep("^Score", colnames(marioKart), 
                                       value = TRUE)), sum), 
                      .(Race1Time = Time[1], EndTime = Time[.N])), 
                  by = c("Cup", grep("^Player|^Character", colnames(marioKart), 
                                     value = TRUE), "Date", "Weekday")]

cups[, GapTime := c(Inf, difftime(Race1Time[-1], EndTime[-.N], units = "mins"))]
plot(ecdf(cups$GapTime), xlim = c(0, 300))
cups[, Session := cumsum(GapTime > 20)]
cups[, .(UniqueN = uniqueN(.SD)), by = Session, 
     .SDcols = patterns("^Player")][, all(UniqueN == 1)]
cups[, .(UniqueN = uniqueN(.SD)), by = Session, 
     .SDcols = patterns("^Character")][, all(UniqueN == 1)]
marioKart <- cups[, .(Cup, Session)][marioKart, on = "Cup"]
mkLong <- melt(marioKart, measure.vars = patterns(Player = "^Player", 
                                                  Character = "^Character", 
                                                  Score = "^Score"), 
               variable.name = "PlayerNo")
mkLong
data <- mkLong[, .(Date, Weekday, Player, Score)]
data <- oneHotEncode(data, cols = c("Weekday", "Player"))
data[, Date := as.integer(Date)]
ind <- sample(nrow(data), 100)
data_train <- data[-ind]
trainLabel <- data_train$Score
data_train[, Score := NULL]
data_test <- data[ind, -"Score"]

trainXGB <- xgb.DMatrix(data = as.matrix(data_train), label = trainLabel)
model<- xgboost(data = trainXGB,
                   nrounds = 20)
explanation <- lime(data_train, model)
explanations <- explain(data_test, explanation, n_labels = 5, n_features = 20)

# Get an overview with the standard plot
plot_explanations(explanations)


library(wdl)

