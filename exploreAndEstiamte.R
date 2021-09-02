library(TAM)
library(psych)
library(difR)

# questa funzione serve solo per plottare le ICC degli item senza  usare TAM
pIRT = function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}

# Importa i csv per la classe 8 (uno alla volta se no ci mette due giorni)
# non ho messo i csv su github perché sono troppo pesanti
files = list.files(paste0(getwd(), "/8"), pattern = ".csv")

myfiles  = gsub(".csv", "", files)

# dati 2009-2010,  classe 8  -----
math0910.8 = read.csv(paste0(getwd(), "/8/", files[[1]]))

# seleziona solo le variabili per analisi (per tutti gli studenti)

item = math0910.8[, 17:60] 
describe(item)

# 1pl -----
pl1 = tam.mml(item[, c(1:34)])
# fit assoluta del modello in relazione agli item (infit and outfit)
# tutti gli item fittano al modello (non ci sono risposte inattese per livelli
# vivini -infit- o lontani -outfit-)
tam.fit(pl1)
fit.pl1 = tam.modelfit(pl1)
summary(pl1) # gli item tendono ad essere molto molto facili, a parte qualche 
# eccezione
summary(fit.pl1)
tif.1pl = IRT.informationCurves(pl1)
plot(tif.1pl)


p.1pl = pl1$person$EAP # meglio non usare  valori reali di theta per il 
# plot perché ci mette una vita
item.1pl = pl1$item
theta = seq(-7, 7, .001)

colors = seq(1, nrow(item.1pl), 1)

plot(theta, pIRT(theta = theta, a = item.1pl[1, "B.Cat1.Dim1"],   
                 b = item.1pl[1, "xsi.item"]), 
    type = "l")
for (i in 2:nrow(item.1pl)){
  lines(theta, pIRT(theta = theta, a = item.1pl[i, "B.Cat1.Dim1"],   
                   b = item.1pl[i, "xsi.item"]), 
        col = colors[i])
}
abline(h = 0.5)

# 2pl 
pl2 = tam.mml.2pl(item[, c(1:34)])
# fit degli item ---
tam.fit(pl2) # gli item vanno bene, quasi troppo (sembra ci sia overfitting)
# fit del modello 
fit.pl2 = tam.modelfit(pl2)
summary(pl2)
summary(fit.pl2) # gli item sono molto facili, solo pochi hanno una discriminatività
# elevata 

# il 2 PL sembra essere migliore. 
IRT.compareModels(pl1, pl2)

item.2pl = pl2$item

plot(theta, pIRT(theta = theta, 
                 a = item.2pl[1, "B.Cat1.Dim1"],   
                 b = item.2pl[1, "xsi.item"]), 
     type = "l", ylim=c(0,1))
for (i in 2:nrow(item.2pl)){
  lines(theta, pIRT(theta = theta, 
                    a = item.2pl[i, "B.Cat1.Dim1"],   
                    b = item.2pl[i, "xsi.item"]), 
        col = colors[i], ylim=c(0,1))
}
abline(h = 0.5)
# l'item D9_a ha una disciminatività negativa (funziona malissimo, è l'item
# celeste nel grafico che ha valori maggiori di probabilità di risposta corretta 
# per livelli bassi di abilità)

# 3pl ----
# attenzione perché ci mette tanto a stimare (circa mezzora)
pl3 = tam.mml.3pl(item[, c(1:34)], est.guess = 1:34)
tam.fit(pl3) # non riesco a vedere il fit deglin item 

summary(pl3)
fit.pl3 = tam.modelfit(pl3)
summary(it.pl3)
# non vale la pena usare il 3pl: i guessing sono tutti molto bassi, 
# probabilmente c'era davvero la penalizzazione per la risposta errata
IRT.compareModels(pl1, pl2, pl3)

item.3pl = pl3$item

plot(theta, pIRT(theta = theta, 
                 a = item.3pl[1, "B.Cat1.Dim1"],   
                 b = item.3pl[1, "AXsi_.Cat1"], 
                 c= item.3pl[1, "guess"]), 
      type = "l", ylim=c(0,1))
for (i in 2:nrow(item.3pl)){
  lines(theta, pIRT(theta = theta, 
                    a = item.3pl[i, "B.Cat1.Dim1"],   
                    b = item.3pl[i, "AXsi_.Cat1"], 
                    c= item.3pl[i, "guess"]), 
        col = colors[i], ylim=c(0,1))
}
# c'èe sempre l'item con la discriminatività negativa
abline(h = 0.5)


# dati campione ------ 

item.camp = math0910.8[math0910.8$CAMPIONE %in% 1 , 17:60]

# 1pl -----

pl1.camp = tam.mml(item.camp[, c(1:34)])
tam.fit(pl1.camp) # item D9a e D19a hanno circa il 20% di varianza non spiegata 
# (D9_a anche sul'infit)
fit.pl1.camp = tam.modelfit(pl1.camp)
summary(pl1.camp) # gli item sono molto semplici, a parte qualche eccezione (D8b)
summary(fit.pl1.camp)


item.1pl.camp = pl1.camp$item
theta = seq(-7, 7, .001)

colors = seq(1, nrow(item.1pl.camp), 1)

plot(theta, pIRT(theta = theta, a = item.1pl.camp[1, "B.Cat1.Dim1"],   
                 b = item.1pl.camp[1, "xsi.item"]), 
     type = "l")
for (i in 2:nrow(item.1pl.camp)){
  lines(theta, pIRT(theta = theta, a = item.1pl.camp[i, "B.Cat1.Dim1"],   
                    b = item.1pl.camp[i, "xsi.item"]), 
        col = colors[i])
}
abline(h = 0.5)


# 2pl ------
pl2.camp = tam.mml.2pl(item.camp[, c(1:34)])
# fit degli item ---
tam.fit(pl2.camp) # gli item vanno ben
# fit del modello 
fit.pl2.camp = tam.modelfit(pl2.camp)
summary(pl2.camp)
summary(fit.pl2.camp) # gli item sono molto facili, solo pochi hanno una discriminatività
# elevata. Item D9_a ha discriminatività negativa

# il 2 PL è meglio. 
IRT.compareModels(pl1.camp, pl2.camp)

item.2pl.camp = pl2.camp$item

plot(theta, pIRT(theta = theta, 
                 a = item.2pl.camp[1, "B.Cat1.Dim1"],   
                 b = item.2pl.camp[1, "xsi.item"]), 
     type = "l")
for (i in 2:nrow(item.2pl.camp)){
  lines(theta, pIRT(theta = theta, 
                    a = item.2pl.camp[i, "B.Cat1.Dim1"],   
                    b = item.2pl.camp[i, "xsi.item"]), 
        col = colors[i])
}
abline(h = 0.5)


# 3pl -----

pl3.camp = tam.mml.3pl(item.camp[, c(1:34)], est.guess = 1:34)
tam.fit(pl3.camp) # non riesco a vedere il fit deglin item 

summary(pl3.camp) # qui forse il guessing ha senso (siamo nell'ordine del 20%)
fit.pl3.camp = tam.modelfit(pl3.camp)
summary(fit.pl3.camp) # ha brutti indici di fit assoluti
# non vale la pena usare il 3pl 
IRT.compareModels(pl1.camp, pl2.camp, pl3.camp)

item.3pl.camp.camp = pl3.camp$item

plot(theta, pIRT(theta = theta, 
                 a = item.3pl.camp.camp[1, "B.Cat1.Dim1"],   
                 b = item.3pl.camp.camp[1, "AXsi_.Cat1"], 
                 c= item.3pl.camp.camp[1, "guess"]), 
     type = "l", ylim=c(0,1))
for (i in 2:nrow(item.3pl.camp.camp)){
  lines(theta, pIRT(theta = theta, 
                    a = item.3pl.camp.camp[i, "B.Cat1.Dim1"],   
                    b = item.3pl.camp.camp[i, "AXsi_.Cat1"], 
                    c= item.3pl.camp.camp[i, "guess"]), 
        col = colors[i], ylim=c(0,1))
}
# c'è sempre l'item con la discriminatività negativa
abline(h = 0.5)

# math 2011/2012 classe 8 -----
math1112.8 = read.csv(paste0(getwd(), "/8/", files[[2]]))
describe(math1112.8)

item1112.8 = math1112.8[, c(14:59, 2, 78)]
describe(item1112.8)

# 1pl -----
pl1.1112.8 = tam.mml(item1112.8[, c(1:46)])
tam.fit(pl1.1112.8) # due item hanno valori di outfit leggermente maggiori degli altri item 
# (item E1, E6), ma comunque accettabili
fit.pl1.1112.8 = tam.modelfit(pl1.1112.8)
summary(pl1.1112.8) # gli item tendono ad essere estremamente facili, specie il primo.
# item E6 è tra i più difficili. Nota che questi due item sono anche quelli con i valori
# di outfit leggermente più elevati (hanno registrato risposte inattese per livelli
# rispettivamente alti e bassi del tratto)
summary(fit.pl1.1112.8)
tif.pl1.1112.8 = IRT.informationCurves(pl1.1112.8)
plot(tif.pl1.1112.8)

item.pl1.1112.8 = pl1.1112.8$item
theta = seq(-7, 7, .001)

colors = seq(1, nrow(item.pl1.1112.8), 1)
# il primo item è estremamente facile.
plot(theta, pIRT(theta = theta, a = item.pl1.1112.8[1, "B.Cat1.Dim1"],   
                 b = item.pl1.1112.8[1, "xsi.item"]), 
     type = "l")
for (i in 2:nrow(item.pl1.1112.8)){
  lines(theta, pIRT(theta = theta, a = item.pl1.1112.8[i, "B.Cat1.Dim1"],   
                    b = item.pl1.1112.8[i, "xsi.item"]), 
        col = colors[i])
}
abline(h = 0.5)

# 2pl -----
pl2.1112.8 = tam.mml.2pl(item1112.8[, c(1:46)])
# fit degli item ---
tam.fit(pl2.1112.8) # questa volta gli item fittano al modello. Come nel caso dell'anno 
# precedente, c'è pochissima varianza non spiegata e tanti valori vicini (troppo vicini) a 1.
# questo mi fa sopsettare che ci sia overfitting 
fit.pl2.1112.8 = tam.modelfit(pl2.1112.8)
summary(pl2.1112.8) # item molto molto facili, non ci sono discriminatività negative
# tutti i valori di discriminatività sono tendenzialmente bassi
summary(fit.pl2.1112.8) # gli indici di fitperò vanno bene, hanno valori verosimili
# che non mi fanno pensare all'overfitting

# il 2 PL sembra essere migliore. Tuttavia, gli inidici comparativi non sono così benevoli
# verso il 2PL, facendomi di nuovo pensare che forse non sia conveniente andare ad aggiunere
# il parametro di discriminatività 
IRT.compareModels(pl1.1112.8, pl2.1112.8)

item.2pl.1112.8 = pl2.1112.8$item

plot(theta, pIRT(theta = theta, 
                 a = item.2pl.1112.8[1, "B.Cat1.Dim1"],   
                 b = item.2pl.1112.8[1, "xsi.item"]), 
     type = "l", ylim=c(0,1))
for (i in 2:nrow(item.2pl.1112.8)){
  lines(theta, pIRT(theta = theta, 
                    a = item.2pl.1112.8[i, "B.Cat1.Dim1"],   
                    b = item.2pl.1112.8[i, "xsi.item"]), 
        col = colors[i], ylim=c(0,1))
}
abline(h = 0.5)
# si vedono molto bene i pochi item molto discriminativi 

# 3pl ----
# attenzione perché ci mette tanto a stimare (UN'ORA) e non ne vale la pena
# non stimo più i 3PL a meno che il 2PL non sia nettarmente migliore dell'1PL
pl3.1112.8 = tam.mml.3pl(item1112.8[, c(1:46)], est.guess = 1:46)
tam.fit(pl3.1112.8) # non riesco a vedere il fit deglin item 

summary(pl3.1112.8)
fit.pl3.1112.8 = tam.modelfit(pl3.1112.8)
summary(fit.pl3.1112.8)
# non vale la pena usare il 3pl: i guessing sono tutti molto bassi, 
IRT.compareModels(pl1.1112.8, pl2.1112.8, pl3.1112.8)

item.3pl.1112.8 = pl3.1112.8$item

plot(theta, pIRT(theta = theta, 
                 a = item.3pl.1112.8[1, "B.Cat1.Dim1"],   
                 b = item.3pl.1112.8[1, "AXsi_.Cat1"], 
                 c= item.3pl.1112.8[1, "guess"]), 
     type = "l", ylim=c(0,1))
for (i in 2:nrow(item.3pl.1112.8)){
  lines(theta, pIRT(theta = theta, 
                    a = item.3pl.1112.8[i, "B.Cat1.Dim1"],   
                    b = item.3pl.1112.8[i, "AXsi_.Cat1"], 
                    c= item.3pl.1112.8[i, "guess"]), 
        col = colors[i], ylim=c(0,1))
}

abline(h = 0.5)


# campione normativo 2011-2012 math classe 8 -----

item.camp.1112.8 = math1112.8[math1112.8$CAMPIONE %in% 1 , c(14:59, 2, 78)]


# 1pl -----
pl1.1112.8.camp = tam.mml(item.camp.1112.8[, c(1:46)])
tam.fit(pl1.1112.8.camp)# gli item vanno tutti molto bene. 
fit.pl1.1112.8.camp = tam.modelfit(pl1.1112.8.camp)
summary(pl1.1112.8.camp) # Gli item sono facilissimi
summary(fit.pl1.1112.8.camp) # La fit va bene 
tif.pl1.1112.8.camp = IRT.informationCurves(pl1.1112.8.camp)
plot(tif.pl1.1112.8.camp)

item.pl1.1112.8.camp = pl1.1112.8.camp$item
theta = seq(-7, 7, .001)

colors = seq(1, nrow(item.pl1.1112.8.camp), 1)
# il primo item è estremamente facile.
plot(theta, pIRT(theta = theta, 
                 a = item.pl1.1112.8.camp[1, "B.Cat1.Dim1"],   
                 b = item.pl1.1112.8.camp[1, "xsi.item"]), 
     type = "l")

for (i in 2:nrow(item.pl1.1112.8.camp)){
  lines(theta, pIRT(theta = theta, a = item.pl1.1112.8.camp[i, "B.Cat1.Dim1"],   
                    b = item.pl1.1112.8.camp[i, "xsi.item"]), 
        col = colors[i])
}
abline(h = 0.5)

# 2pl -----
pl2.1112.8.camp = tam.mml.2pl(item.camp.1112.8[, c(1:46)])
# fit degli item ---
tam.fit(pl2.1112.8.camp) # Io sospetto che il 2PL sia in overfit. Le statistiche 
# di infit e outfit sono molto (troppo vicine a 1), indicando che il modello spiega 
# quasi perfettamente i dati. Inoltre, ci sono molti valori < 1 --> questo indica
# che i dati hanno meno variabilità di quanto atteso dal modello. Sono tutte cose 
# sospette
fit.pl2.1112.8.camp = tam.modelfit(pl2.1112.8.camp)
summary(pl2.1112.8.camp) # infatti molte discriminatività sono inferiori a 1, tanto che 
# questo parametro mi sembra evitabile
summary(fit.pl2.1112.8.camp) 

# la model comparison favorisce il modello 2pl
IRT.compareModels(pl1.1112.8.camp, pl2.1112.8.camp)

item.2pl.1112.8.camp = pl2.1112.8.camp$item

plot(theta, pIRT(theta = theta, 
                 a = item.2pl.1112.8.camp[1, "B.Cat1.Dim1"],   
                 b = item.2pl.1112.8.camp[1, "xsi.item"]), 
     type = "l", ylim=c(0,1))
for (i in 2:nrow(item.2pl.1112.8.camp)){
  lines(theta, pIRT(theta = theta, 
                    a = item.2pl.1112.8.camp[i, "B.Cat1.Dim1"],   
                    b = item.2pl.1112.8.camp[i, "xsi.item"]), 
        col = colors[i], ylim=c(0,1))
}
abline(h = 0.5)
# si vedono molto bene i pochi item molto discriminativi 

# 3pl ----
# attenzione perché ci mette tanto a stimare 
pl3.1112.8.camp = tam.mml.3pl(item.camp.1112.8[, c(1:46)], 
                              est.guess = 1:46)
summary(pl3.1112.8.camp)# sempre il guessing è prossimo a 0
fit.pl3.1112.8.camp = tam.modelfit(pl3.1112.8.camp)
summary(fit.pl3.1112.8.camp) # la fit assoluta non è male

# favorisce il 3pl di poco
IRT.compareModels(pl1.1112.8.camp, pl2.1112.8.camp, pl3.1112.8.camp)

item.3pl.1112.8.camp = pl3.1112.8.camp$item

plot(theta, pIRT(theta = theta, 
                 a = item.3pl.1112.8.camp[1, "B.Cat1.Dim1"],   
                 b = item.3pl.1112.8.camp[1, "AXsi_.Cat1"], 
                 c= item.3pl.1112.8.camp[1, "guess"]), 
     type = "l", ylim=c(0,1))
for (i in 2:nrow(item.3pl.1112.8.camp)){
  lines(theta, pIRT(theta = theta, 
                    a = item.3pl.1112.8.camp[i, "B.Cat1.Dim1"],   
                    b = item.3pl.1112.8.camp[i, "AXsi_.Cat1"], 
                    c= item.3pl.1112.8.camp[i, "guess"]), 
        col = colors[i], ylim=c(0,1))
}
abline(h = 0.5)

# non penso ne valga la pena ----

# math 2012-2013 classe 8 ------

math1213.8 = read.csv(paste0(getwd(), "/8/", files[[3]]))
describe(math1213.8)

item1213.8 = math1213.8[, c(28:72, 5, 10)]
describe(item1213.8)

# 1pl -----
pl1.1213.8 = tam.mml(item1213.8[, c(1:45)])
tam.fit(pl1.1213.8) # a parte item D17 (comunque dentro il range), gli item tendono ad avere una buona fit al
# modello
fit.pl1.1213.8 = tam.modelfit(pl1.1213.8)
summary(pl1.1213.8) # item mediamente facili
summary(fit.pl1.1213.8) # la fit non è male

item.pl1.1213.8 = pl1.1213.8$item
theta = seq(-7, 7, .001)

colors = seq(1, nrow(item.pl1.1213.8), 1)
# il primo item è estremamente facile.
plot(theta, pIRT(theta = theta, a = item.pl1.1213.8[1, "B.Cat1.Dim1"],   
                 b = item.pl1.1213.8[1, "xsi.item"]), 
     type = "l")
for (i in 2:nrow(item.pl1.1213.8)){
  lines(theta, pIRT(theta = theta, a = item.pl1.1213.8[i, "B.Cat1.Dim1"],   
                    b = item.pl1.1213.8[i, "xsi.item"]), 
        col = colors[i])
}
abline(h = 0.5)

# 2pl -----
pl2.1213.8 = tam.mml.2pl(item1213.8[, c(1:45)]) # non riesce proprio a stimare il
# modello

# tam.fit(pl2.1213.8) 
# fit.pl2.1213.8 = tam.modelfit(pl2.1213.8)
# summary(pl2.1213.8) 
# summary(fit.pl2.1213.8) 
# 
# IRT.compareModels(pl1.1213.8, pl2.1213.8)
# 
# item.2pl.1213.8 = pl2.1213.8$item
# 
# plot(theta, pIRT(theta = theta, 
#                  a = item.2pl.1213.8[1, "B.Cat1.Dim1"],   
#                  b = item.2pl.1213.8[1, "xsi.item"]), 
#      type = "l", ylim=c(0,1))
# for (i in 2:nrow(item.2pl.1213.8)){
#   lines(theta, pIRT(theta = theta, 
#                     a = item.2pl.1213.8[i, "B.Cat1.Dim1"],   
#                     b = item.2pl.1213.8[i, "xsi.item"]), 
#         col = colors[i], ylim=c(0,1))
# }
# abline(h = 0.5)




# campione normativo math 2012-2013 class 8 -----
item.camp.1213.8 = math1213.8[math1213.8$campione %in% 1 , c(28:72, 5, 10)]
describe(item.camp.1213.8)

# 1pl -----
pl1.1213.8.camp = tam.mml(item.camp.1213.8[, c(1:45)])
tam.fit(pl1.1213.8.camp) # tendezialmente buona fit degli item a parte item D10b, 
# D17b D17c i valori sono comunque accettabili, solo sono più alti rispetto a quelli 
# degli altri item
fit.pl1.1213.8.camp = tam.modelfit(pl1.1213.8.camp)
summary(pl1.1213.8.camp) # item mediamente facili tranne 1 (D14c)
summary(fit.pl1.1213.8.camp) # La fit va bene 


item.pl1.1213.8.camp = pl1.1213.8.camp$item
theta = seq(-7, 7, .001)

colors = seq(1, nrow(item.pl1.1213.8.camp), 1)
# il primo item è estremamente facile.
plot(theta, pIRT(theta = theta, 
                 a = item.pl1.1213.8.camp[1, "B.Cat1.Dim1"],   
                 b = item.pl1.1213.8.camp[1, "xsi.item"]), 
     type = "l")

for (i in 2:nrow(item.pl1.1213.8.camp)){
  lines(theta, pIRT(theta = theta, a = item.pl1.1213.8.camp[i, "B.Cat1.Dim1"],   
                    b = item.pl1.1213.8.camp[i, "xsi.item"]), 
        col = colors[i])
}
abline(h = 0.5)

# 2pl -----
pl2.1213.8.camp = tam.mml.2pl(item.camp.1213.8[, c(1:45)])
# fit degli item ---
tam.fit(pl2.1213.8.camp)  # di nuovo io sospetto ci sia overfit 
fit.pl2.1213.8.camp = tam.modelfit(pl2.1213.8.camp)
summary(pl2.1213.8.camp) 
summary(fit.pl2.1213.8.camp)# poche discriminatività sopra l'1 

# la model comparison favorisce il modello 2pl
IRT.compareModels(pl1.1213.8.camp, pl2.1213.8.camp)

# Model comparison mi suggerisce il 2PL
item.2pl.1213.8.camp = pl2.1213.8.camp$item

plot(theta, pIRT(theta = theta, 
                 a = item.2pl.1213.8.camp[1, "B.Cat1.Dim1"],   
                 b = item.2pl.1213.8.camp[1, "xsi.item"]), 
     type = "l", ylim=c(0,1))
for (i in 2:nrow(item.2pl.1213.8.camp)){
  lines(theta, pIRT(theta = theta, 
                    a = item.2pl.1213.8.camp[i, "B.Cat1.Dim1"],   
                    b = item.2pl.1213.8.camp[i, "xsi.item"]), 
        col = colors[i], ylim=c(0,1))
}
abline(h = 0.5)













