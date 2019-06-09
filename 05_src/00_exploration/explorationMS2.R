## set up h2o
h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll()
h2o.clusterInfo()

args(h2o.deeplearning)
help(h2o.deeplearning)
example(h2o.deeplearning)
demo(h2o.deeplearning)  #requires user interaction


data = read.fst("03_computedData/04_preparedData/NewsWithLabel0.01Subset.fst",
                as.data.table = TRUE)

data.h2o <- as.h2o(data)
model.h2o <- h2o.deeplearning(y = "labelRaw", training_frame = data.h2o)
predict.h2o <- h2o.predict(object = model.h2o, newdata = data.h2o)




# next steps:
# 3blue one brown deep learning serie anschauen + notizen
# liste machen mit methoden die ausprobiert werden können
# nach ähnlichen Datensätzen suchen