# next steps/ to dos:

# [vor runter schreiben von methoden]
# methoden: multinomial naive bayes oder logReg (hat besser performed)
# validation data muss genauso preprocessed sein wie train Data
# preparation functions und predictors durchgehen und schauen dass überall same
# accuracy measure auschecken: binary cross entropy > mse?

# [vor finaler modellrechnung]
# - framework muss 100%ig abgeklärt sein


# [exploration]
# word clouds for biggest categorys (parents/parenting)
# (arts and style, worldpost/The worldpost)
# exploration function for word vectors (relate to wordclouds)

# [liste für groll:]
# wie wird die anzahl der epochen/ nrounds gewählt (ist ja schon tuning)
# guetemass wahrscheinlichkeit der richtigen klasse implementieren
# validation/test bei xgboost und RF checken (muss gleich sein) (wird watchlist eigentlich gebraucht)
#     im falle RF wird ja auf vollen 90% trainiert, auf 10% predicted
#     im falle XG wird auf 80% von 90% trainiert, auf 10% predicted
#     watchlist ist nur zum tunen.
# framework update (RF und XGBoost watchlist/outofbag, aber gleiche trainingsmenge?) ist rechenbar
# kategorien zusammenlegen Schema (vom menschen nicht auseinanderhaltbar, gemeinsame words)
# human experiment
# fußnoten? richtlinien für die Arbeit, Alles was im anhang ist wird nicht gebraucht?

# xxdokumentieren welche tokens entfernt wurden. (0 wörter auch entfernen)
#     xxgeht nicht vor prepareData da vom embedding abhängig? 
#     xxwird geprinted in prepare fct's.
# xxnormales MLP mit keras implementieren on bag of words/TFIDF
# xxchecken ob die conf matrix richtig berechnet hat. selbe accuracy
# xxSparse auf tfidf prepare
# xxxgb mit sparse matrix berechnen (auf mac übertragen)
# xxrf sparse einrichten
# xxsourcetree problem lösen
# xx o nach c problem entfernen
# ab neuAnfang

# xx kategorien reduzieren: 40 auf 35
# xxencoding fehler checken!!!! â zb.
# xx's , bspw. John's mit word tagging ersetzen
# xxcolorpalette vom wdl code holen
# xxbag of words vlt in einzelschritten computen (ndoc = 20 minimal) Wolle fragen
# xx      A: 32Gb auf omega verfuegbar
# xxkaggle threads nochmal durchlesen
# xxmergedSD reinnehmen und schauen wie das performt. A: performed nicht besser.
# xxto lower: alle wörter werden eh großgeschrieben
# xx confusionmatrix colnames namen.
# xxbei BOW, tdidf  ndoc > 2 hinzufuegen (checken) A: ist drin
# xx RF/XGboost auf embedded indexes ausprobieren. A: nur 0.22, schlecht
# xx checken auf wieviel Beobachtungen mehrere saetze sind. A: 5%
# xx balancing for training? A: macht accuracies schlechter (2x getestet)
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

# [post masterarbeit]
# attention layer implemtieren
# bert auschecken/context2word in python implementieren
# hierarchical attention neural net
# markus und wdl datensätze im auge behalten
# fasttext auschecken

install_keras(method = c("auto", "virtualenv", "conda"),
              conda = "auto", version = "default", tensorflow = "default",
              extra_packages = c("tensorflow-hub"), ...)

library(reticulate)
use_python("//usr/local/bin/python3", required = TRUE)
py_available()
py_config()
py_numpy_available()
library(keras)

# balancing
data <- read.fst("03_computedData/04_preparedData/Emb-10pc-FALSE.fst", 
                 as.data.table = TRUE)
test <- generalizedSampling(data = data, method = "down", 
                    label = "labelRaw")
table(test$labelRaw)

notFound <- fread("03_computedData/02_cleanedData/notFound.csv")
