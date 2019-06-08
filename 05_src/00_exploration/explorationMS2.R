## set up h2o
h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll()
h2o.clusterInfo()

args(h2o.deeplearning)
help(h2o.deeplearning)
example(h2o.deeplearning)
#demo(h2o.deeplearning)  #requires user interaction


data = read.fst("03_computedData/04_preparedData/News0.01Subset.fst",
                as.data.table = TRUE)
label = read.fst("03_computedData/04_preparedData/NewsLabel0.01Subset.fst",
                 as.data.table = TRUE)


# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/4/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()



# next steps:
# 3blue one brown deep learning serie anschauen + notizen
# liste machen mit methoden die ausprobiert werden können
# nach ähnlichen Datensätzen suchen