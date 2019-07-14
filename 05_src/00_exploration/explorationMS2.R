# next steps/ to dos:

# den gecleanten text anschauen und checken ob was übersehen wurde.
# Glove nutzen: schauen wieviel übereinstimmende Terme und was machen mit
#   denen, die nicht übereinstimmen, nochmal Glove lernen?
# bei GloVe: keine terme removen die nur 1 mal vorkommen
# bei BOW  ndoc > 2 hinzufuegen
# word2vec problem, entweder die vecs sind schlecht,
#     oder das unsupervised learning ist das problem
#     -> binäres framework schreiben, momentan 0.67 accuracy, jetzt
#         das tunen mit preproc und Glove ausprobieren
#       dann framework schreiben wo alle binären accuracys ausgegeben werden
#       
# mache das sampling zum wegschreiben und das framework noch intelligenter
# lernrate auschecken (optimizer)
# pretrained word vectors reinnehmen.
# schau die im convolutional layers an und ob da alles gut gegangen ist
# für NN normalizierung rein
# binary cross entropy > mse? auschecken?
# find similarities to words to see if word2vec works. (write function)
# generell: bei verschiedenen auswahlmöglichkeiten schauen was gut dokumentiert ist

# ueberlegen wie explaining aussehen koennte.
# methoden: multinomial naive bayes
# balancing for training?
# features: number of words, number of caps
# lstm tutorial durchmachen (kerras vignetten)
# recurrent neural net, hierarchical attention neural net
# markus und wdl datensätze im auge behalten
# context2word in python implementieren
# mehrere saetze als input haben eventuell beruecksichtigen
# toxicity datensatz ziehen
# bert auschecken

# liste für groll:
# bamlls lesen
# naive bayes implementieren
# word2vec verstehen

# xxremove stopwords für wordtovec vlt?
# xxcoden dass alle verfahren die gleiche hardgecodete subsets bekommen
# xxword padding nachschauen wie richtig!
# xximplementiere embedding NN mit
# xxtdfidf implementieren
# xxmit na's auffüllen ausprobieren, result: 0er passen gut
# xxcheck out h2o word2vec (nicht nötig)
# xxnummer 3 auschecken bei word embeddings article
# xxbeobachtung bei 10pc der Daten: 18000 obs, vocab of 10825, 
#     ist fast unmöglich gute wordvecs zu lernen
# xxtoLower checken in word2vec (wurde nicht gemacht)
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





