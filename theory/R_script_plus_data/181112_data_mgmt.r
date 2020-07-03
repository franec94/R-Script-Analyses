######################################################################
###
###  Data managment
###                      Modelli Statistici 
###
#######################################################################

library(nycflights13)   # un database
library(microbenchmark)


str(airports)
head(airports)
class(airports$lat)
class(airports$name)
nrow(airports)
ncol(airports)

str(planes)

# semplici estrazioni di dati

airports[1:10,"name"]
airports[airports$lat>0,"lat"]
class(airports[airports$lat>0,"lat"])

# diversi modi per calcolare nuove variabili

airports$lat_rad_1 <- airports$lat*pi/180

attach(airports)
airports$lat_rad_2 <- lat*pi/180
detach(airports)

airports <- transform(airports, lat_rad_3 = lat*pi/180) 

# funzioni aggregate

seats_mean_1 <- tapply(planes$seats , planes$manufacturer, mean)
seats_mean_2 <- aggregate(seats ~ manufacturer, planes, mean)

# tempi computazionali
compare <- microbenchmark(tapply(planes$seats , planes$manufacturer, mean), aggregate(seats ~ manufacturer, planes, mean), times = 1000)

########################################################################

# tidy data VS messy data ("long to wide")
# esempio di messy data:

df1 <- data.frame(Paziente=c("P1", "P2", "P3"), TrattA=c(23, 45, 30), TrattB=c(18, 35, 20), TrattC=c(19, 34, 23))
df1


# regole per tidy data:
# una colonna corrisponde a una variabile
# una riga corrisponde a una osservazione 
# ogni gruppo omogeneo di osservazioni corrisponde ad una tabella
# l'estrazione di dati in questo modo è indipendente dal formato
# NB spesso è preferito un formato "messy" in output

df2 <- reshape(df1, varying = c("TrattA", "TrattB", "TrattC"), times=c("TrattA", "TrattB", "TrattC"),
        direction = "long",  timevar="Tratt", v.names = "Val", new.row.names=1:9)
df2

# strumenti appositi per la conversione di dati:
# package tidyr(): gather(), spread(), ...

########################################################################

library(dplyr) # package per manipolazione di data

# operazioni base sui dataframe:
# filter() opera una selezione sulle righe
# select() opera una selezione sulle colonne
# arrange() ordina le righe del df
# mutate() calcola nuove variabili
# distinct() seleziona le righe di df non ripetute
# NB l'output di questi comandi è ancora un dataframe

select(flights, dest)
distinct(select(flights, dest))

filter(flights, dest %in% c("BQN", "MIA"))
filter(flights, dest == "SFO" | dest == "OAK")
filter(flights, month == 1)
filter(flights, hour >= 0 & hour <= 5)
filter(flights, hour >= 0, hour <= 5)
filter(flights, arr_delay > 2 * dep_delay)

# scriviamo una funzione select e vediamo i tempi
SelectM = function(x,y)
{
	return(x[y,])
}
compare <- microbenchmark(filter(flights, month == 1),SelectM(flights, flights$month == 1), times = 1000)

?select
select(flights, dep_time, carrier)
select(flights, dep_time, dep_time + 2)
select(weather, contains("wind"))
select(weather, ends_with("ed"))

arrange(flights, month, day, hour, minute)
arrange(flights, -month, -day, -hour, -minute)

arrange(flights, desc(distance/hour))

flights <- mutate(flights, speed = distance / (air_time / 60))
head(mutate(flights, delta = dep_delay - arr_delay))



ifelse(is.na(dep_delay),0,dep_delay)


# il costrutto ifelse può essere utile per definire variabili in modo condizionale
flights <- mutate(flights, dep_delay=ifelse(is.na(dep_delay),0,dep_delay))
flights <- mutate(flights, max_delay=pmax(dep_delay, arr_delay))
flights <- mutate(flights, dep_delay=ifelse(is.na(dep_delay) & is.na(arr_delay),0,dep_delay))

# le funzioni utilizzate non devono operare sulle righe del df
flights2 <- flights %>% mutate (max_delay = max(arr_delay, dep_delay))
flights2 <- flights %>% mutate (max_delay = pmax(arr_delay, dep_delay))


# variabili aggregate:
# group_by() memorizza il df 
# summarize() 
# si possono applicare funzioni come: n(), mean(), sum(), min()

flights <- mutate(flights, date= as.Date(paste(year, month, day), "%Y %m %d"))

flights_by_date <- group_by(flights, date)

delays <- summarise( flights_by_date,
  mean = mean(dep_delay, na.rm = TRUE),
  median = median(arr_delay, na.rm = TRUE),
  q75 = quantile(arr_delay, 0.75, na.rm = TRUE),
  over_15_prep = sum(arr_delay > 15, na.rm = TRUE),
  count = n(),
  over_15 = mean(arr_delay > 15, na.rm = TRUE),
  max_delay = max(arr_delay, na.rm=TRUE)
)

filter(flights, is.na(dep_delay))

flights_mod <- mutate(group_by(flights, date),
			dep_delay=ifelse(is.na(dep_delay),mean(dep_delay, na.rm=TRUE),dep_delay))

nrow(flights_mod)

filter(flights_mod, is.na(dep_delay))


########################################################################

# per facilitare la scrittura di sequenze di operazioni sui dati
# il package dplyr() importa magrittr()
# utilizzo dell'operatore "pipe": x %>% f(y) corrisponde a f(x,y)
# NB si può andare a capo dopo il pipe

# esempio:
# trovare giorni in cui la temp. media giornaliera all'aereoporto Newark (EWR)
# ha superato gli 85°F (30°C) ed elencarli dal più caldo al meno caldo

temp1 <- group_by( filter(weather, origin=="EWR") , year, month, day, origin )
temp2 <- filter ( summarize( temp1, avg_temp = mean(temp) ), avg_temp > 85 )
arrange(temp2, -avg_temp)




weather %>%
	filter(origin=="EWR") %>%  
	group_by(year, month, day, origin) %>%
	summarize ( avg_temp = mean(temp) ) %>%
	filter (avg_temp > 85) %>%
	arrange (-avg_temp) 

# ricordare di applicare ungroup() alla fine!

# NB l'operatore pipe può essere utilizzato anche fuori dal contesto db
# flights %>% select(carrier) %>% class()


########################################################################

# "join" di tabelle basato sulla corrispondenza di un gruppo di variabili

flights_short <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier)

flights_short %>% left_join(airlines)
flights_short %>% left_join(weather)
flights_short %>% left_join(planes, by = "tailnum") # year?
flights_short %>% left_join(airports, c("origin" = "faa"))

# inner_join(A,B) include solo le osservazioni per i quali si ha il match
# left_join(A,B) include tutte le osservazioni di A e, se esistono, le osservazioni
#               di B per i quali si ha il match
# semi_join(A,B) tiene tutte le osservazioni di A che hanno corrispondenza in B 

flights %>% anti_join(planes, by = "tailnum") %>% nrow
flights %>% semi_join(planes, by = "tailnum") %>% nrow


