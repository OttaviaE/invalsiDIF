library(ltm)
library(difR)

difLord(item.camp[,1:34], item.camp$sesso, focal.name = "f", 
        model = "1PL", engine = "ltm", purify = TRUE, 
        p.adjust.method = "BH")
difRaju(item.camp[,1:34], item.camp$sesso, focal.name = "f", 
        model = "1PL", engine = "ltm", purify = TRUE, 
        p.adjust.method = "BH")


p = difLord(item.camp[,1:34], item.camp$sesso, focal.name = "f", 
        model = "2PL", engine = "ltm", purify = TRUE, 
        p.adjust.method = "BH")
plot(p)

plot(p, plot = "itemCurve", item = 1)

# devo ancora vedere come fare DIF non uniforme. 
# confrontare i risultati di questo pacchetto con quelli di lordif