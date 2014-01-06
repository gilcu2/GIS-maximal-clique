
#  To są luźne notatki z tego co robiłem. Możliwe że działa ale nei pisałem tego jako skrypt więc mogą być problemy
#
#

#Postęp

library(ggplot2)
pgd = read.table('progress-data-20-min-C125', sep=',')
names(pgd)<-c('Graf','Algorytm','w','Czas','Pamiec')
p<-qplot(x=Czas, y=w, color=Algorytm, data=pgd, ylab="w(g)",xlab="Czas w milisekundach, skala logarytmiczna", log="x")
ggsave('doc/img/progress.pdf')
#DIMACS

#library(ggplot2)
t = read.table('dimacs-best-90sec', sep=',')
names(t)<-c('Graf','Algorytm','w','Czas','Pamiec')
maxs <- aggregate(w ~ Graf + Algorytm, data = t, FUN = max)
maxs

readyToPlot <-merge(maxs,t)

withoutMANN = subset(readyToPlot, readyToPlot$Graf != 'MANN_a81.clq.b')
withoutMANN
withoutMANN = subset(withoutMANN, withoutMANN$Graf != 'MANN_a27.clq.b')
withoutMANN
withoutMANN = subset(withoutMANN, withoutMANN$Graf != 'MANN_a45.clq.b')
withoutMANN
ggplot(withoutMANN, aes(Graf, w, fill=Algorytm)) + geom_bar(position='dodge') + coord_flip() + ylab('w(g)')
ggsave('doc/img/dimacs1.pdf')
p <- ggplot(withoutMANN, aes(Graf, Czas, fill=Algorytm, stat="identity")) + geom_bar(position='dodge', stat="identity") + coord_flip() + ylab('Czas w milisekundach')
ggsave('doc/img/dimacs1czas.pdf')

manOnly <- subset(readyToPlot, readyToPlot$Graf %in% c('MANN_a27.clq.b', 'MANN_a45.clq.b','MANN_a81.clq.b'))
ggplot(manOnly, aes(Graf, w, fill=Algorytm)) + geom_bar(position='dodge') + coord_flip() + ylab('w(g)')
ggsave('doc/img/dimacs2.pdf', height=3)

p <- ggplot(manOnly, aes(Graf, Czas, fill=Algorytm, stat="identity")) + geom_bar(position='dodge', stat="identity") + coord_flip() + ylab('Czas w milisekundach')
ggsave('doc/img/dimacs2czas.pdf', height=3)

readyToPlotTime <- rbind(readyToPlot, c('MANN_a81.clq.b','Bron-Kerbosch',0,90000,0))