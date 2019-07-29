# next steps/ to dos:

#
# checken auf wieviel Beobachtungen mehrere saetze sind
# fasttext auschecken
# bag of words vlt in einzelschritten computen
# checken ob die conf matrix richtig berechnet hat. selbe accuracy
# eventuell als kernthema: performances auf verschiedenen Trainingsgrößen
#       evaluieren/einfluss von balancing, ensemble learner/vergleich von LSTM
#       attention layer
# verena fragen was tun wegen 's , bspw. John's
# bei BOW, tdidf  ndoc > 2 hinzufuegen     
# binary cross entropy > mse? auschecken?
# write exploration function for wordvectors
# generell: bei verschiedenen auswahlmöglichkeiten schauen was gut dokumentiert ist
# ueberlegen wie explaining aussehen koennte.
# methoden: multinomial naive bayes
# balancing for training?
# hierarchical attention neural net
# markus und wdl datensätze im auge behalten
# context2word in python implementieren
# bert auschecken
# validation data muss genauso preprocessed sein wie train Data

# liste für groll:
# bamlls lesen
# naive bayes implementieren
# word2vec verstehen

# xxlstm tutorial durchmachen (kerras vignetten)
# xxmehrere saetze als input haben eventuell beruecksichtigen A: (springt den Rahmen)
# xx simple ensemble embedden
# xx confusionmatrix ausgeben + evaluation zwischen sicherheit und Richtig
# xx  für xxcnn, xxxgboost, xxRF umsetzen
# xxlernrate auschecken (optimizer) A: Adam perfomt gut, kleinere lernrate immer
# xx  eine gute methode
# xxpretrained word vectors reinnehmen. (glove genutzt)
# xxschau die im convolutional layers an und ob da alles gut gegangen ist
# xxfeatures: number of words, number of caps A: eher unrelevant
# xxmache das sampling zum wegschreiben und das framework noch intelligenter
# xx    done: jetzt werden die indizes an der richtigen stelle gesampelt
# xxword2vec problem, entweder die vecs sind schlecht, A: die waren schlecht
# xx    oder das unsupervised learning ist das problem
# xx    -> binäres framework schreiben, momentan 0.67 accuracy, jetzt
# xx        das tunen mit preproc und Glove ausprobieren
# xx      dann framework schreiben wo alle binären accuracys ausgegeben werden
# xxGlove nutzen: schauen wieviel übereinstimmende Terme und was machen mit
# xx denen, die nicht übereinstimmen, nochmal Glove lernen? -
# xx  A: gibt genug übereinstimmungen (0.967)
# xxbei GloVe: keine terme removen die nur 1 mal vorkommen
# xxbinary framework reduzieren auf eng beiandanderliegende Kategorien
# xxbow 100% funkt nicht, 46gb vector, zu groß!
# xx den gecleanten text anschauen und checken ob was übersehen wurde.
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





