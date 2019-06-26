options(java.home="/Library/Java/JavaVirtualMachines/jdk-11.0.3.jdk/Contents/Home/jre")

install.packages("rJava",,"http://rforge.net/",type="source")
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")



setnames(resultProbs, colnames(resultProbs), colnames(resultResponse))

meanProbError <- mean(as.vector(abs(resultResponse - resultProbs)))
# now see if the probabilities and the true false values differ 
# 




## set up h2o
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", 
                 repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/5/R")




# next steps:
# xxset up h2o to work
# set up error rate for h20
# rename column names from data in preparation
# add error measurement of the network
# xx3blue one brown deep learning serie anschauen + notizen
# liste machen mit methoden die ausprobiert werden können
# random forest, xgboost, multinomial naive bayes
# recurrent neural net, hierarchical attention neural net
# nach ähnlichen Datensätzen suchen
# markus seine methode im blick behalten