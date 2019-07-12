# next steps:

# bei 10pc der Daten: 18000 obs, vocab of 10825, ist fast unmöglich
# eine gute repräsentation zu lernen, vocab reduzen?
# remove stopwords für wordtovec vlt?
# lernrate auschecken (optimizer)
# pretrained word vectors reinnehmen.
# word padding nachschauen wie richtig!
# mit na's auffüllen ausprobieren, MLP als input überlegen
# coden dass alle verfahren die gleiche hardgecodete subsets bekommen
# schau die im convolutional layers an und ob da alles gut gegangen ist
# für NN normalizierung rein
# binary cross entropy > mse? auschecken?
# find similarities to words to see if word2vec works. (write function)
# check out h2o word2vec
# generell: bei verschiedenen auswahlmöglichkeiten schauen was gut dokumentiert ist

# ueberlegen wie explaining aussehen koennte.
# methoden: multinomial naive bayes
# balancing for training?
# tdfidf
# features: number of words, number of words per category, Number of Camel words,
# number of caps
# Number of camel words
# lstm tutorial durchmachen (kerras vignetten)
# recurrent neural net, hierarchical attention neural net
# nach ähnlichen Datensätzen suchen
# markus seine methode im blick behalten
# context2word in python implementieren
# mehrere saetze als input haben eventuell beruecksichtigen
# toxicity datensatz ziehen
# bert auschecken

# xxframework intelligent neu schreiben prepareData, dann als argument
# xxpreparedataW2V nochmal durchgehen und besser selecten
# xx(ueberlegen, was alles raus muss, numbers drin, toLower, sonderzeichen bleiben drin)
# xxkeras auf mac zum laufen zu bringen
# xxwrite array  in word to vec, then try to implement CNN
# xxbenchmarken mit anderen spalten (short description) im datensatz
# xxworld post und the world post zusammenfassen
# xxsubsets einmal wegspeichern und das wars, so samplen dass in der kleinsten
#     klasse erwarte anzahl von beobachtungen xx ist.
# xxbert link öffnen
# xxgrößeren wordvec ausprobieren mit weniger pruning
# xxrandom forest, xxxgboost, 
# xxprepareWord2Vec 
# xxsetup random forest
# xxset up h2o to work
# xgboost: add watchlist
# xxset up error rate for h20
# xxrename column names from data in preparation (nicht notwendig)
# xxadd error measurement of the network
# xx3blue one brown deep learning serie anschauen + notizen


install_keras(method = c("auto", "virtualenv", "conda"),
              conda = "auto", version = "default", tensorflow = "default",
              extra_packages = c("tensorflow-hub"), ...)

library(reticulate)
use_python("//usr/local/bin/python3", required = TRUE)
py_available()
py_config()
py_numpy_available()
library(keras)





