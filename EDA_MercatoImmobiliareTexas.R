#Analisi esplorativa dei dati  del mercato immobiliare in Texas


#Iniziamo importando in R i pacchetti che utilizzeremo e il dataset "realestate_texas.csv":


library(ggplot2)
library(dplyr)
library(moments)
library(gghalves)
library(stringr)
library(cowplot)
library(RColorBrewer)


dati <- read.csv("realestate_texas.csv",sep=",")


#In questo modo, abbiamo creato un oggetto DataFrame con 240 osservabili di 8 variabili.
#Vediamo le prime 5 righe del dataset:


head(dati, 5)

#Le variabili sono:

#* `city`: città di riferimento. È una variabile di tipo `carattere`, quindi una variabile **qualitativa** nominale;
#* `year`: anno di riferimento. In generale, è una variabile **quantitativa a intervalli**, poiché non ha una chiara definizione di 0. In questo caso, sarà considerata una variabile categorica, con 5 livelli;
#* `month`: mese di riferimento. È una variabile categorica **codificata**, con 12 livelli;
#* `sales`, `listings`: sono variabili  **quantitative** discrete. Entrando nel dettaglio, le variabili contengono le seguenti informazioni:

# * `sales`: numero totale di vendite;
# * `listings`: numero totale di annunci attivi.
#* `volume`, `median_price`, `months_inventory`: sono variabili  **quantitative** continue. Esaminandole più da vicino, ci danno le seguenti informazioni:

# * `volume`: valore totale delle vendite in milioni di dollari;
# * `median_price`: prezzo mediano di vendita in dollari; 
# * `months_inventory`: tempo necessario per vendere tutti gli annunci attivi al tasso di vendita attuale, in mesi.

#Creiamo prima alcune funzioni che utilizzero' dopo:
#crea una funzione per il coefficiente di variazione (non una funzione integrata)
cv=function(x){
  sd(x)/mean(x)*100
}

#crea una funzione per l'indice di Gini (non è una funzione integrata)
gini.index=function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J=length(table(x))
  gini=1-sum(fi2)
  gini.norm=gini*J/(J-1)
  return(gini.norm)
}

#crea una funzione per il range o intervallo di variazione (non una funzione integrata)
range=function(x){
  return(max(x)-min(x))
}

# Calcola il numero di valori mancanti in ogni colonna
missing_count <- dati %>%
  summarise_all(~ sum(is.na(.)))
missing_count

#Per quanto riguarda la variabile `city`, creiamo la tabella di distribuzione di frequenza.
#Prima di farlo, utilizziamo la funzione `attach()` per accedere alle variabili del DataFrame 
#senza selezionarle per indice o nome dal DataFrame stesso:

attach(dati)

#Costruzione della tabella di distribuzione di frequenza per la variabile `city`:

N = dim(dati)[1]
ni = table(city)
fi = ni/N
city_freq_distr <- cbind(ni,fi)
city_freq_distr


#Dalla tabella, ci rendiamo conto che abbiamo a che fare con una distribuzione **quadrimodale**, 
#poiché le frequenze assolute sono le stesse per tutte le categorie. Dato che le osservazioni 
#sono uniformemente distribuite sulle 4 categorie, ci aspettiamo che l'indice di Gini sia pari a 1. In effetti:


gini.index(city)

#Il motivo è che il dataset è costituito da dati relativi alle vendite mensili 
#delle proprietà immobiliari di Beaumont, Bryan-College Station, Tyler e Wichita Falls, dal 2010 al 2014.
#Pertanto, abbiamo $12 volte 5=60$ osservazioni per ogni città. Infatti, se consideriamo, ad esempio,
#le osservazioni associate alla città "Beaumont", troviamo 5 osservazioni per ogni mese e 12 osservazioni per ogni anno:


Beaumont_data = filter(dati, city == "Beaumont")
Beaumont_N = dim(Beaumont_data)[1]
ni = table(Beaumont_data$month)
fi = ni/Beaumont_N
Ni = cumsum(ni)
Fi = Ni/Beaumont_N
month_freq_distr <- cbind(ni,fi,Ni,Fi)
month_freq_distr

ni = table(Beaumont_data$year)
fi = ni/Beaumont_N
Ni = cumsum(ni)
Fi = Ni/Beaumont_N
year_freq_distr <- cbind(ni,fi,Ni,Fi)
year_freq_distr


#Lo stesso vale per le altre 3 città. Pertanto, secondo la interpretazione classica
#della probabilità, se estraiamo a caso un'osservazione dal dataframe:

#- la probabilità che la città sia "Beaumont" è pari a $60/240$, cioè


Beaumont_prob = Beaumont_N/N
Beaumont_prob

#la probabilità che il mese sia luglio, cioè 7, è pari a:


July_data = filter(dati, month == 7)
July_N = dim(July_data)[1]
July_prob = July_N/N
July_prob


#la probabilità che il mese sia dicembre, cioè 12, e l'anno sia 2012 è data da:

dec2012_data = filter(dati, month == 12 & year == 2012)
dec2012_N = dim(dec2012_data)[1]
dec2012_prob = dec2012_N/N
dec2012_prob


#Passiamo ora a calcolare gli indici di posizione, gli indici di variabilità e
#gli indici di forma delle variabili `sales`, `volume`, `median_price`, `listings` e `months_inventory`. 
#A questo scopo, costruiamo una tabella riassuntiva con tutte le misure statistiche che vogliamo calcolare:


dati.colnames <- colnames(dati)
columns = c("min", "1st quartile", "median", "3rd quartile", "max", "range",
            "IQR", "mean", "std.dev", "var.coeff", "skewness", "kurtosis")
summary.df <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(summary.df) = columns
                 


for (variable.name in dati.colnames[4:8]) {
  
  variable <- pull(dati,variable.name)
  
  quartiles = as.numeric(quantile(variable))
  
  df <- dati %>%
    summarise(range=max(variable)-min(variable),
              IQR=IQR(variable),
              mean=mean(variable),
              dev.st=sd(variable),
              var.coeff=cv(variable),
              skewness=skewness(variable),
              kurtosis=kurtosis(variable)-3)
  
  row = c(quartiles,as.numeric(df))
    
  summary.df <- rbind(summary.df, row)

}

summary.df <- cbind(dati.colnames[4:8],summary.df)
colnames(summary.df) = c("variable", columns)
summary.df



#La colonna `curtosi` ci permette di concludere che le variabili `sales`, `median_price`, 
#`listings` e `months_inventory` hanno una curtosi negativa, quindi le loro distribuzioni sono **platicurtica**,
#cioè producono meno outlier estremi rispetto alla distribuzione normale. D'altra parte, 
#la distribuzione della variabile `volume` è detta **leptocurtica**, e quindi produce più outlier 
#rispetto alla distribuzione normale. Possiamo vedere quanto detto osservando i grafici di densità delle variabili:

layout(mat = matrix(c(1,1,2,2,3,3,
                      4,5,5,6,6,7),
                    nrow = 2,
                    byrow = T))

d1 <- density(sales)
d2 <- density(volume)
d3 <- density(median_price)
d4 <- density(listings)
d5 <- density(months_inventory)
plot(d1, main = "Kernel Density di `sales`")
polygon(d1, col = "lightgreen", border = "darkgreen")
plot(d2, main = "Kernel Density di `volume`")
polygon(d2, col = "lightgreen", border = "darkgreen")
plot(d3, main = "Kernel Density di `median_price`")
polygon(d3, col = "lightgreen", border = "darkgreen")
plot.new()
plot(d4, main = "Kernel Density di `listings`")
polygon(d4, col = "lightgreen", border = "darkgreen")
plot(d5, main = "Kernel Density di `months_inventory`")
polygon(d5, col = "lightgreen", border = "darkgreen")
plot.new()


#Se ora osserviamo il coefficiente di Fisher di skewness, apprendiamo che, ad eccezione del prezzo mediano, 
#tutte le variabili sono positivamente skewed, ovvero la loro distribuzione è caratterizzata dalla maggior parte dei valori 
#che si raggruppano intorno alla coda sinistra della distribuzione, mentre la coda destra della distribuzione è più lunga. 
#La distribuzione della variabile `prezzo medio` è invece negativamente skewed, cioè la coda sinistra è più lunga di quella destra 
#e il grosso della distribuzione è concentrato sulla coda destra.

#Se ora ricordiamo che il coefficiente di variazione (CV) è una misura statistica della dispersione *relativa* delle osservazioni 
#in un insieme di dati intorno alla media, scopriamo che la variabile con la maggiore variabilità è il valore totale delle vendite 
#in milioni di dollari, ovvero la variabile `volume`. Quest'ultima è anche la variabile con il più alto grado di skewness, 
#come si può vedere dalla colonna `skewness` della tabella.

#Prima di aggiungere alcune colonne interessanti al dataframe, dividiamo la variabile `sales` in classi e costruiamo 
#la corrispondente tabella di distribuzione di frequenza. Abbiamo scelto di creare 5 classi, per sintetizzare i dati senza perdere troppe informazioni:


dati$sales_cl=cut(sales,breaks=seq(75,425,70))
attach(dati)
ni=table(sales_cl)
fi=ni/N
Ni=cumsum(ni)
Fi=Ni/N
cbind(ni,fi,Ni,Fi)
gini.index(sales_cl)

barplot(ni,
        main="Distribuzione di frequenza per la variabile vendite nelle classi",
        xlab="N° di vendite in classi",
        ylab="Frequenza assoluta",
        col="pink",
        border="red")
barplot(fi,
        main="Distribuzione di frequenza per la variabile vendite nelle classi",
        xlab="N° di vendite in classi",
        ylab="Frequenza relativa",
        col="lightblue",
        border="blue")



#Infine, calcoliamo l'indice di Gini della variabile "sales" divisa in classi:

gini.index(sales_cl)


#Ora notiamo che possiamo calcolare il prezzo medio dalle colonne `sales` e `volume` del dataframe,
#dividendo il valore totale delle vendite per il numero totale di vendite. Per comodità, 
#convertiamo anche il prezzo medio in dollari:

avg_price = volume*10^6/sales
dati_with_avg_price = tibble::add_column(dati,
                                            avg_price,
                                            .after = "volume")
head(dati_with_avg_price,10)


#In base ai dati disponibili, possiamo anche aggiungere un'altra colonna che dia un'idea 
#dell'efficacia delle offerte di vendita. Infatti, possiamo dividere il numero totale 
#di vendite per il numero totale di offerte di vendita attive:


sales_offers_efficiency = (sales/listings)*100/months_inventory
dati_with_avg_and_efficiency = tibble::add_column(dati_with_avg_price,
                                                     sales_offers_efficiency,
                                                     .after = "listings")
head(dati_with_avg_and_efficiency,10)


#Valori maggiori della nuova variabile indicano un maggiore efficacia delle offerte di 
#vendita


#Consideriamo ora il boxplot che illustra la distribuzione del prezzo mediano di vendita
#in funzione della città e dell'anno:

#Usiamo gghalves per mostrare violin plot e dare un'occhiata sia agli indici di
#variabilità che agli indici di forma

colore_pesca <- "#FFDAB9"
ggplot(dati)+
  geom_half_boxplot(aes(x=city,y=median_price),side="l",fill="lightblue")+
  geom_half_violin(aes(x=city,y=median_price),side="r",fill= colore_pesca)+
  labs(title="Boxplot e densità di probabilità del prezzo mediano per citta",
       x="Città",
       y="Prezzo mediano di vendita [$]")

ggplot(dati)+
  geom_boxplot(aes(x=city,y=median_price,fill=factor(year)))+
  labs(title="Boxplot del prezzo mediano per città ogni anno",
       x="Città",
       y="Prezzo mediano di vendita [$]",
       fill="Anni")+
  scale_fill_manual(values = brewer.pal(5,"Set2"))
#Il boxplot mostra che la distribuzione del prezzo mediano di vendita di tutte le città è asimmetrica.
#Inoltre, apprendiamo che in media, la città con il prezzo medio di vendita più elevato è Bryan-College Station,
#seguita rispettivamente da Tyler, Beaumont e Wichita Falls.

#Costruiamo ora un altro boxplot con il valore totale delle vendite per anno, considerando ancora separatamente le 4 città:

ggplot(dati)+
  geom_boxplot(aes(x=city,y=sales,fill=factor(year)))+
  labs(title="Boxplot del n° di vendite per città ogni anno",
       x="Città",
       y="N° di vendite",
       fill="Anni")+
  scale_fill_manual(values = brewer.pal(5,"Set2"))
  

#Questo grafico ci permette di vedere che, indipendentemente dall'anno, la città 
#con la mediana più alta del valore totale delle vendite è Tyler, seguita da Bryan-College Station. 
#Combinando questo dato con il grafico precedente, ci aspettiamo che il numero totale di vendite 
#sia maggiore (in media) a Tyler che a Bryan-College Station, poiché quest'ultima tende ad avere
#un prezzo medio di vendita più elevato rispetto alla prima.<br> Possiamo estrapolare ulteriori
#informazioni utili dal grafico. Ad esempio, si può notare che il valore totale delle vendite per anno:

#- è aumentato costantemente a Tyler, Bryan-College Station e Beaumont;
#- è rimasto pressoché costante a Wichita Falls.

#Osserviamo ora nuovamente il valore totale delle vendite, ma questa volta considerando 
#i dati per anno, mediante un grafico a barre sovrapposte:


ggplot(dati)+ 
  geom_bar(aes(x=city,y=sales, fill=factor(year)), stat="identity", position="stack")+
  labs(title="Bar plot del n° di vendite per anno come funzione della città",
       x="Città",
       y="N° di vendite",
       fill="Anni")+
  scale_fill_manual(values = brewer.pal(5,"YlOrRd"))

#Si nota che c'è stato un aumento del valore totale delle vendite in Texas a partire dal 2011,
#principalmente a causa dell'incremento significativo del valore totale delle vendite a Bryan-College Station.
#Questo diventa ancora più evidente osservando il grafico normalizzato a barre sovrapposte:

ggplot(dati)+ 
  geom_bar(aes(x=city,y=sales, fill=factor(year)), stat="identity", position="fill")+
  labs(title="Bar plot normalizzato del n° di vendite per anno come funzione della città",
       x="Città",
       y="N° di vendite",
       fill="Anni")+
  scale_fill_manual(values = brewer.pal(5,"YlOrRd"))


#Inoltre, il grafico a linee seguente mostra la percentuale del valore totale 
#delle vendite di ciascuna città per anno:


total_per_year <- dati %>%
  group_by(year)%>%
  summarise(total=sum(volume))

i = 1
years = unique(year)

for (yr in years) {
  
  percentage_per_city <- filter(dati, year == yr) %>%
  group_by(city)%>%
  summarise(year = unique(year),
            percentage = sum(volume)/total_per_year$total[i])
  
  if (i==1) {
    
    percentage_per_year_and_city <- percentage_per_city
    
  } else {
    percentage_per_year_and_city <- rbind(percentage_per_year_and_city,
                                          percentage_per_city)
  }
  
  i <- i+1
  
}

swtch <- function(x,i,j) {
  x[c(i,j)] <- x[c(j,i)]
  return(x)
}

#percentage_per_year_and_city

ggplot(percentage_per_year_and_city)+
  geom_line(aes(x=year,
                y=percentage*100,
                group=city,
                col=city),
            lwd=1
            )+
  geom_point(aes(x=year,
                y=percentage*100,
                group=city,
                col=city),
            size=2.5)+
  labs(x = "Anni",
       y = "Percentuale del valore totale delle vendite",
       title = "Percentuale del valore totale delle vendite per anno")+
  geom_text(aes(x=year,
                y=percentage*100-1,
                label=paste(format(round(percentage*100, 2), nsmall = 2),"%")))+
  scale_color_discrete(name = "City",
                       labels = swtch(str_wrap(unique(city), width = 15), 1, 3),
                       breaks = swtch(unique(city), 1, 3))+
  scale_y_continuous(breaks = seq(5,40,5))+
  theme(plot.title = element_text(size= 20,
                                  hjust = 0.5),
        legend.box.background = element_rect(color = "black",
                                             linewidth = 0.75),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.title.align = 0.5,
        panel.spacing = unit(1.5, "lines"))



#Dai due grafici precedenti, possiamo osservare che la percentuale del valore totale delle vendite per anno:

#- è aumentata costantemente per Bryan-College Station;
#- è leggermente diminuita per Beaumont;
#- è costantemente diminuita per Wichita Falls;
#- è rimasta quasi costante per Tyler.

#L'aumento complessivo del valore totale delle vendite a partire dal 2011 è principalmente 
#dovuto a un aumento del numero di vendite piuttosto che a un aumento del prezzo medio di vendita. 
#Per vedere ciò, mostriamo le serie temporali associate alle due variabili sales e median_price:



p1 <- ggplot(data = dati)+
  geom_line(aes(x = factor(month.abb[month], levels = month.abb),
                y=median_price,
                group=city,
                col=city),
            lwd=1
            )+
  facet_wrap(~year, nrow = 1)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_color_discrete(name = "City")+
  labs(x="Mese",
       y="Prezzo mediano di vendita in dollari",
       title = "Prezzo mediano di vendita per mese")+
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 14),
        legend.position = "bottom",
        strip.text = element_text(size = 12,
                                  face = "bold"),
        strip.background = element_rect(color = "black",
                                        fill = "lightblue",
                                        linewidth = 1.2),
        legend.box.background = element_rect(color = "black", linewidth = 1),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.title.align = 0.5)

p2 <- ggplot(data = dati)+
  geom_line(aes(x = factor(month.abb[month], levels = month.abb),
                y=sales,
                group=city,
                col=city),
            lwd=1
            )+
  facet_wrap(~year, nrow = 1)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  labs(x="Mese",
       y="Numero totale di vendite",
       title = "Numero totale di vendite per mese")+
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 14),
        legend.position = "none",
        strip.text = element_text(size = 12,
                                  face = "bold"),
        strip.background = element_rect(color = "black",
                                        fill = "lightblue",
                                        linewidth = 1.2),
        legend.box.background = element_rect(color = "black",
                                             linewidth = 1),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.title.align = 0.5)

plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1.125,1))


#Dai due grafici, possiamo osservare che c'è stato un aumento significativo nel numero
#di vendite a Bryan-College Station e Tyler nel 2013 e 2014, e un lieve aumento 
#a Beaumont a partire dal 2011. Lo stesso non è vero per il prezzo mediano. Infatti,
#è rimasto quasi costante a Beaumont e Wichita Falls, mentre c'è stato solo 
#un lieve aumento nelle città di Bryan-College Station e Tyler.
