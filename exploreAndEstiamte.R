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

files = list.files(paste0(getwd(), "/8"), pattern = ".csv")

myfiles  = gsub(".csv", "", files)


math0910.8 = read.csv(paste0(getwd(), "/8/", files[[1]]))

# seleziona solo le variabili per analisi (per tutti gli studenti)

item = math0910.8[, 17:60] 
describe(item)

# stima 1pl
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
plot(pl1, items=1:10, type="items" , export=FALSE,
     low = -5, high = 5)

p.1pl = pl1$person$EAP # meglio non usare  valori reali di theta per il 
# plot perché ci mette una vita
item.1pl = pl1$item
theta = seq(-7, 7, .001)

colors = seq(1, nrow(item.1pl), 1)

plot(theta, pIRT(theta = theta, a = item.1pl[1, "B.Cat1.Dim1"],   
                 b = item.1pl[1, "xsi.item"]), 
   , type = "l")
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
     , type = "l")
for (i in 2:nrow(item.2pl)){
  lines(theta, pIRT(theta = theta, 
                    a = item.2pl[i, "B.Cat1.Dim1"],   
                    b = item.2pl[i, "xsi.item"]), 
        col = colors[i])
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
summary(fit.pl3)
# non vale la pena usare il 3pl: i guessing sono tutti molto bassi, 
# probabilmente c'era davvero la penalizzazione per la risposta errata
IRT.compareModels(pl1, pl2, pl3)

item.3pl = pl3$item

plot(theta, pIRT(theta = theta, 
                 a = item.3pl[1, "B.Cat1.Dim1"],   
                 b = item.3pl[1, "AXsi_.Cat1"], 
                 c= item.3pl[1, "guess"]), 
     , type = "l")
for (i in 2:nrow(item.3pl)){
  lines(theta, pIRT(theta = theta, 
                    a = item.3pl[i, "B.Cat1.Dim1"],   
                    b = item.3pl[i, "AXsi_.Cat1"], 
                    c= item.3pl[i, "guess"]), 
        col = colors[i])
}
# c'èe sempre l'item con la discriminatività negativa
abline(h = 0.5)


dichoDif(item[, c(1:34)], item$sesso, method = "TID", 
         focal.name = "m")

