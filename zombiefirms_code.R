options(digits = 3)
library(ggplot2)
library(plyr)
library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(topicmodels)
library(stm)
library(tidytext)
library(plotly)
library(lubridate)
library(corpus)
library(lubridate)
library(stringr)
library(ggrepel)
library(tidyr)
library(dplyr)
library(tidytext)
library(igraph)
library(gdata)
library(readtext) 
library(tm)
library(tm.plugin.factiva)
library(RNewsflow)

# set working directory
# upload of files might vary depending on folders allocation or users' settings

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/zombie_firms/II_review/zombiefirms-gh")

# DATA COLLECTION INFORMATION ######
# Check if term included in dfm #####
#textfreq <- textstat_frequency(dfm_it)
textstat_frequency(dfm_de) %>% subset(feature %in% "europäische_zentralbank")

test_term <- quanteda::convert(corpus_de08,to = "data.frame")
unique(stringr::str_extract_all(test_term$text,
   "\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:] deutschen \\-?[:alnum:]*\\-?[:alnum:]*\\b"))

tt <- tx_de %>% filter(str_detect(text,"greensill"))

# bigram and trigram detect #####

bigrams_tx  <- tx_it %>% filter(country == "Italy") %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_tx  %>% dplyr::count(bigram, sort = TRUE)
bigrams_separate  <- bigrams_tx  %>% separate(bigram,c("word1","word2"),sep=" ")
bigrams_filtered  <- bigrams_separate  %>%
  filter(!word1 %in% stopwords_it) %>%
  filter(!word2 %in% stopwords_it)
bigrams_filtered <- bigrams_filtered  %>%  dplyr::count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered  %>% unite(bigram, word1, word2, sep = " ")
# bigrams_united  <- bigrams_united$bigram
bigrams_united <- unique(bigrams_united)

bigrams_united <- bigrams_united %>% filter(! bigram %in% zombiefirm_pattern_it)

write.csv(bigrams_united,"bigrams_it2.csv",row.names= F)

trigrams_tx <- tx_de %>% filter(country == "Germany") %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
trigrams_tx %>% dplyr::count(trigram, sort = TRUE)
trigrams_separate  <- trigrams_tx %>% separate(trigram,c("word1","word2","word3"),sep=" ")
trigrams_filtered <- trigrams_separate  %>%
  filter(!word1 %in% stopwords_de) %>%
  filter(!word2 %in% stopwords_de) %>%
  filter(!word3 %in% stopwords_de)
trigrams_filtered  <- trigrams_filtered %>%  dplyr::count(word1, word2,word3, sort = TRUE)
trigrams_united  <- trigrams_filtered  %>% unite(trigram, word1, word2,word3, sep = " ")
# trigrams_united <- trigrams_united$trigram
trigrams_united <- unique(trigrams_united)
# 
write.csv(trigrams_united,"trigrams_definal.csv",row.names= F)



# Search strategy ####
# German keywords: zombieunternehmen, zombiefirma, zombiefirmen, zombie firm, zombie firms, zombie-firma, zombie-firmen, zombie-unternehmen, zombie company, zombie companies, zombi-firma, zombi-firmen, zombi-unternehmen,industrie zombie ,industrien zombie ,industrie zoombie ,industrien zoombie ,industrie zombi ,industrien zombi ,industrie-zombie , industrien-zombie ,industrie-zoombie ,industrien-zoombie ,industrie-zombi ,industrien-zombi ,industriezombie ,industrienzombie ,industriezoombie ,industrienzoombie ,industriezombi ,industrienzombi, insolvenzantragspflicht
# Italian keywords: aziende zombie, azienda zombie, azienda zombi, aziende zombi, impresa zombie, impresa zombi, imprese zombie, impresa zombi, zombie company, zombie companies, zombi company, zombi companies, società zombie, oranizzazione zombie, organizzazione zombi, organizzazioni zombie, organizzazioni zombi, attività zombie , attività zoombie , attività zombi ,attività-zombie ,attività-zoombie ,attività-zombi ,attivitá zombie ,attivitá zoombie ,attivitá zombi ,attivitá-zombie ,attivitá-zoombie ,attivitá-zombi, industrie decotte, industria decotta, azienda decotta, aziende decotte, compagnia decotta, compagnie decotte, società decotta, organizzazione decotta, organizzazioni decotte
# Source: Factiva database, personal newspapers subscription, newspaper's trade agreement
# Screening of newspaper articles: mentioning zombiefirms terms and covid terms. See cleanout texts for exclusion criteria
# Corpora with saved documents can not be shared for legal reasons. 
# Section "Germany collection raw data" and "Italy collection raw data" are for informative purpose on how raw data were collected
# Data analysis starts from UPLOAD GERMAN COPRUS

# Germany collection raw data (ignore) #####

# html from Factiva download
filenames <- list.files( pattern="*.html", full.names=TRUE)

# source read by Factiva for each document. It creates a nested list
source_list <- lapply(filenames,FactivaSource)

# last Factiva passage for each document
raw_list <- lapply(source_list,Corpus,list(language = NA))

n <- length(raw_list) 

# to check the number of list's element. This is to arrange number of within brakets below

# Vector with all elements of nested list. To avoid writing each element
corpus_vector <- c(raw_list[[	1	]],
                   raw_list[[	2	]],
                   raw_list[[	3	]],
                   raw_list[[	3	]],
                   raw_list[[	4	]],
                   raw_list[[	5	]],
                   raw_list[[	6	]],
                   raw_list[[	7	]],
                   raw_list[[	8	]],
                   raw_list[[	9	]],
                   raw_list[[	10	]],
                   raw_list[[	11	]],
                   raw_list[[	12	]],
                   raw_list[[	13	]],
                   raw_list[[	14	]],
                   raw_list[[	15	]],
                   raw_list[[	16	]],
                   raw_list[[	17	]],
                   raw_list[[	18	]],
                   raw_list[[	19	]],
                   raw_list[[	20	]],
                   raw_list[[	21	]],
                   raw_list[[	22	]],
                   raw_list[[	23	]],
                   raw_list[[	24	]],
                   raw_list[[	25	]],
                   raw_list[[	26	]],
                   raw_list[[	27	]],
                   raw_list[[	28	]],
                   raw_list[[	29	]],
                   raw_list[[	30	]],
                   raw_list[[	31	]]
)



# Here the final CORPUS (comprising corpus of all documents) is done. Second line is to avoid duplicates later
# transformation ortographic punctuation

corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'"," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x," ' "," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'''"," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x," ' "," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'"," ")))

# corpus composition
corpus_de <- corpus(corpus_vector)

# preparation for variables annotation

# variable: type of content
de_content_business <- c("Automobil Industrie Online"	,
                         "boerse-online.de"	,
                         "bondguide online"	,
                         "Börse Online"	,
                         "Börsen-Zeitung"	,
                         "Börsen Radio Network AG"	,
                         "Capital"	,
                         "Citywire"	,
                         "Czerwensky intern"	,
                         "DGAP Finanznachrichten"	,
                         "Die Bank"	,
                         "Dow Jones Newswires German"	,
                         "Euro am Sonntag"	,
                         "Fundamentalanalyse / Research"	,
                         "Gründerszene"	,
                         "manager magazin Online"	,
                         "Neumarkter Nachrichten"	,
                         "news aktuell OTS - Originaltextservice"	,
                         "Platow Brief"	,
                         "Unternehmeredition online"	,
                         "VerkehrsRundschau"	,
                         "Versicherungswirtschaft"	,
                         "WirtschaftsWoche Online"	,
                         "ZfK - Zeitung für kommunale Wirtschaft"	,
                         "ZfK - Zeitung für kommunale Wirtschaft Online"	)

# variable: geographical cover
de_cover_local <- c(
  "Berliner Zeitung"	,
  "Bremer Nachrichten"	,
  "Dresdner Neueste Nachrichten"	,
  "Dresdner Neueste Nachrichten Online"	,
  "Frankenpost"	,
  "Frankfurter Neue Presse Online"	,
  "General Anzeiger"	,
  "Göttinger Tageblatt / Eichsfelder Tageblatt Online"	,
  "Hamburger Abendblatt"	,
  "Hamburger Abendblatt Online"	,
  "Hamburger Morgenpost"	,
  "Hannoversche Allgemeine Zeitung"	,
  "Hannoversche Allgemeine Zeitung Online"	,
  "Leipziger Volkszeitung"	,
  "Leipziger Volkszeitung Online"	,
  "Lübecker Nachrichten Online"	,
  "Main-Spitze"	,
  "Märkische Allgemeine Zeitung Online"	,
  "Münchner Merkur"	,
  "Neue Presse Online"	,
  "Neue Westfälische"	,
  "Nordbayerischer Kurier"	,
  "Ostsee-Zeitung Online"	,
  "Passauer Neue Presse"	,
  "Peiner Allgemeine Zeitung Online"	,
  "Rheinische Post Online"	,
  "Schaumburger Nachrichten Online"	,
  "Schwarzwälder Bote"	,
  "Trierischer Volksfreund"	,
  "Weser Kurier"	,
  "Wolfsburger Allgemeine Online / Aller-Zeitung Online"
)

# variable: political alignment
de_rating_left <- c("DIE ZEIT"	,
                    "Frankfurter Rundschau"	,
                    "Süddeutsche Zeitung"	,
                    "Süddeutsche Zeitung Online"	,
                    "ZEIT online"	)

de_rating_neutral <- c("Handelsblatt"	,
                       "Handelsblatt Online"	,
                       "Spiegel Online"	,
                       "Spiegel Plus"	,
                       "Berliner Zeitung"	,
                       "Bremer Nachrichten"	,
                       "Dresdner Neueste Nachrichten"	,
                       "Dresdner Neueste Nachrichten Online"	,
                       "Frankenpost"	,
                       "Frankfurter Neue Presse Online"	,
                       "General Anzeiger"	,
                       "Göttinger Tageblatt / Eichsfelder Tageblatt Online"	,
                       "Hamburger Abendblatt"	,
                       "Hamburger Abendblatt Online"	,
                       "Hamburger Morgenpost"	,
                       "Hannoversche Allgemeine Zeitung"	,
                       "Hannoversche Allgemeine Zeitung Online"	,
                       "Leipziger Volkszeitung"	,
                       "Leipziger Volkszeitung Online"	,
                       "Lübecker Nachrichten Online"	,
                       "Main-Spitze"	,
                       "Märkische Allgemeine Zeitung Online"	,
                       "Neue Presse Online"	,
                       "Neue Westfälische"	,
                       "Nordbayerischer Kurier"	,
                       "Ostsee-Zeitung Online"	,
                       "Passauer Neue Presse"	,
                       "Peiner Allgemeine Zeitung Online"	,
                       "Rheinische Post Online"	,
                       "Schaumburger Nachrichten Online"	,
                       "Schwarzwälder Bote"	,
                       "Trierischer Volksfreund"	,
                       "Weser Kurier"	,
                       "Wolfsburger Allgemeine Online / Aller-Zeitung Online"	,
                       "manager magazin Online"	,
                       "WirtschaftsWoche Online"	,
                       "Automobil Industrie Online"	,
                       "boerse-online.de"	,
                       "bondguide online"	,
                       "Börse Online"	,
                       "Börsen-Zeitung"	,
                       "Börsen Radio Network AG"	,
                       "Capital"	,
                       "Citywire"	,
                       "Czerwensky intern"	,
                       "DGAP Finanznachrichten"	,
                       "Die Bank"	,
                       "Dow Jones Newswires German"	,
                       "Euro am Sonntag"	,
                       "Fundamentalanalyse / Research"	,
                       "Gründerszene"	,
                       "Neumarkter Nachrichten"	,
                       "news aktuell OTS - Originaltextservice"	,
                       "Platow Brief"	,
                       "Unternehmeredition online"	,
                       "VerkehrsRundschau"	,
                       "Versicherungswirtschaft"	,
                       "ZfK - Zeitung für kommunale Wirtschaft"	,
                       "ZfK - Zeitung für kommunale Wirtschaft Online"	,
                       "Bundesanzeiger Jahresabschluss-Veröffentlichungen"	,
                       "Der Spiegel"	,
                       "Der Tagesspiegel"	,
                       "Der Tagesspiegel Online"	,
                       "Deutsche Welle"	,
                       "Express"	,
                       "Focus"	,
                       "Focus-Money"	,
                       "Focus Online"	,
                       "Reuters - Nachrichten auf Deutsch"	)

de_rating_right <- c("Münchner Merkur"	,
                     "BILD Plus"	,
                     "bild.de"	,
                     "Die Welt"	,
                     "Welt am Sonntag"	,
                     "WELT online"	)

# variable: means of diffusion
de_diffusion_online <- c("Wolfsburger Allgemeine Online / Aller-Zeitung Online"	,
                         "WirtschaftsWoche Online"	,
                         "Handelsblatt Online"	,
                         "Spiegel Online"	,
                         "ZEIT online"	,
                         "Dresdner Neueste Nachrichten Online"	,
                         "Hamburger Abendblatt Online"	,
                         "Hannoversche Allgemeine Zeitung Online"	,
                         "Leipziger Volkszeitung Online"	,
                         "Märkische Allgemeine Zeitung Online"	,
                         "Rheinische Post Online"	,
                         "Schaumburger Nachrichten Online"	,
                         "manager magazin Online"	,
                         "Automobil Industrie Online"	,
                         "boerse-online.de"	,
                         "bondguide online"	,
                         "Börse Online"	,
                         "Börsen-Zeitung"	,
                         "Börsen Radio Network AG"	,
                         "DGAP Finanznachrichten"	,
                         "Dow Jones Newswires German"	,
                         "Fundamentalanalyse / Research"	,
                         "Gründerszene"	,
                         "news aktuell OTS - Originaltextservice"	,
                         "Unternehmeredition online"	,
                         "ZfK - Zeitung für kommunale Wirtschaft Online"	,
                         "Der Spiegel"	,
                         "Der Tagesspiegel Online"	,
                         "Deutsche Welle"	,
                         "Focus Online"	,
                         "BILD Plus"	,
                         "bild.de"	,
                         "WELT online")

# variables annotation
corpus_de$content <- ifelse(docvars(corpus_de,"origin") %in% de_content_business, "business","general")
corpus_de$cover <- ifelse(docvars(corpus_de,"origin") %in% de_cover_local,  "local","national")
corpus_de$diffusion <- ifelse(docvars(corpus_de, "origin") %in%  de_diffusion_online, "online","paper")
corpus_de$rating <- ifelse(docvars(corpus_de,"origin") %in% de_rating_left,"left",
                           ifelse(docvars(corpus_de,"origin") %in% de_rating_right,"right","center"))


corpus_de$origin2 <- ifelse(docvars(corpus_de,"origin") %in% c("BILD Plus","bild.de"),"Bild",
                            ifelse(docvars(corpus_de,"origin") %in% c("Bremer Nachrichten","Weser Kurier"),  "Weser Kurier",
                                   ifelse(docvars(corpus_de,"origin") %in% c("Der Tagesspiegel Online","Der Tagesspiegel"), "Taggespiel" ,           
                                          ifelse(docvars(corpus_de,"origin") %in% c("DIE ZEIT","ZEIT online"), "Die Zeit",      
                                                 ifelse(docvars(corpus_de,"origin") %in% c("Dresdner Neueste Nachrichten Online","Dresdner Neueste Nachrichten"), "Dresdner Neueste",
                                                        ifelse(docvars(corpus_de, "origin") %in% c("Focus Online","Focus-Money","Focus"), "Focus",
                                                               ifelse(docvars(corpus_de,"origin") %in% c("Hamburger Abendblatt Online","Hamburger Abendblatt"), "Hamburger Abendblatt", 
                                                                      ifelse(docvars(corpus_de,"origin") %in% c("Handelsblatt Online","Handelsblatt","WirtschaftsWoche Online"),  "Handelsblatt"    ,
                                                                             ifelse(docvars(corpus_de,"origin") %in% c("Hannoversche Allgemeine Zeitung Online","Hannoversche Allgemeine Zeitung"), "HAZ", 
                                                                                    ifelse(docvars(corpus_de,"origin") %in% c("Leipziger Volkszeitung Online","Leipziger Volkszeitung"), "Leipziger Volkszeitung"    ,           
                                                                                           ifelse(docvars(corpus_de,"origin") %in% c("Spiegel Online","Spiegel Plus"),  "Spiegel",
                                                                                                  ifelse(docvars(corpus_de,"origin") %in% c("Süddeutsche Zeitung Online","Süddeutsche Zeitung"), "Suddeutsche Zeitung",
                                                                                                         ifelse(docvars(corpus_de,"origin") %in% c("Welt am Sonntag","WELT online","Die Welt"), "Die Welt",     
                                                                                                                ifelse(docvars(corpus_de,"origin") %in% c("ZfK - Zeitung für kommunale Wirtschaft Online","ZfK - Zeitung für kommunale Wirtschaft"),  "Zfkw",     
                                                                                                                       corpus_de$origin))))))))))))))                                                                         


# papers from Frankfurter Allgemeine Zeitung to append

faz_source <- readtext(paste0(getwd(), "*.pdf"),
                       docvarsfrom = "filenames", 
                       docvarnames = c("datetimestamp", "origin2"),
                       sep = "_",
                       encoding = "UTF-8")

faz_corpus <- corpus(faz_source)
faz_corpus$rating <- "right"
faz_corpus$diffusion <- "online"
faz_corpus$content <- "general"
faz_corpus$cover <- "national"
faz_corpus$origin <- "FAZ.net"

# combine corpora
corpus_de <- (faz_corpus + corpus_de)
# variable date
corpus_de$datet <- as_date(corpus_de$datetimestamp)

corpus_de <- corpus_de[!duplicated(docvars(corpus_de)),] # delete duplicates

corpus_de08 <- corpus_de
save(corpus_de08,file = "corpus_de08full.Rdata")



# BIGRAM GERMANY



bigrams_tx  <- tx_de %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_tx  %>% dplyr::count(bigram, sort = TRUE)
bigrams_separate  <- bigrams_tx  %>% separate(bigram,c("word1","word2"),sep=" ")
bigrams_filtered  <- bigrams_separate  %>%
  filter(!word1 %in% stopwords_de) %>%
  filter(!word2 %in% stopwords_de)
bigrams_filtered <- bigrams_filtered  %>%  dplyr::count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered  %>% unite(bigram, word1, word2, sep = " ")
# bigrams_united  <- bigrams_united$bigram
bigrams_united <- unique(bigrams_united)

write.csv(bigrams_united,"bigramsdenew.csv",row.names= F)

trigrams_tx <- tx_de %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
trigrams_tx %>% dplyr::count(trigram, sort = TRUE)
trigrams_separate  <- trigrams_tx %>% separate(trigram,c("word1","word2","word3"),sep=" ")
trigrams_filtered <- trigrams_separate  %>%
  filter(!word1 %in% stopwords_de) %>%
  filter(!word2 %in% stopwords_de) %>%
  filter(!word3 %in% stopwords_de)
trigrams_filtered  <- trigrams_filtered %>%  dplyr::count(word1, word2,word3, sort = TRUE)
trigrams_united  <- trigrams_filtered  %>% unite(trigram, word1, word2,word3, sep = " ")
# trigrams_united <- trigrams_united$trigram
trigrams_united <- unique(trigrams_united)
# 
write.csv(trigrams_united,"trigramsnew.csv",row.names= F)




# Italy collection raw data (ignore) #####

# html from Factiva download

# source read by Factiva for each document. It creates a nested list
filenames <- list.files(pattern="*.html", full.names=TRUE)

# source read by Factiva for each document. It creates a nested list
source_list <- lapply(filenames,FactivaSource)

# last Factiva passage for each document
raw_list <- lapply(source_list,Corpus,list(language = NA))

# to check the number of list's element. This is to arrange number of within brakets below
n <- length(raw_list) 

# Vector with all elements of nested list. To avoid writing each element
corpus_vector <- c(raw_list[[	1	]],
                   raw_list[[	2	]],
                   raw_list[[	3	]],
                   raw_list[[	4	]],
                   raw_list[[	5	]],
                   raw_list[[	6	]],
                   raw_list[[	7	]],
                   raw_list[[	8	]],
                   raw_list[[	9	]],
                   raw_list[[	10	]],
                   raw_list[[	11	]],
                   raw_list[[	12	]],
                   raw_list[[	13	]],
                   raw_list[[	14	]],
                   raw_list[[	15	]],
                   raw_list[[	16	]],
                   raw_list[[	17	]],
                   raw_list[[	18	]],
                   raw_list[[	19	]],
                   raw_list[[	20	]],
                   raw_list[[	21	]],
                   raw_list[[	22	]],
                   raw_list[[	23	]],
                   raw_list[[	24	]],
                   raw_list[[	25	]],
                   raw_list[[	26	]],
                   raw_list[[	27	]]	,
                   raw_list[[	28	]]	,
                   raw_list[[	29	]]	,
                   raw_list[[	30	]]	,
                   raw_list[[	31	]]	,
                   raw_list[[	32	]]	,
                   raw_list[[	33	]]	,
                   raw_list[[	34	]]	,
                   raw_list[[	35	]]	,
                   raw_list[[	36	]]	,
                   raw_list[[	37	]]	,
                   raw_list[[	38	]]	,
                   raw_list[[	39	]]	,
                   raw_list[[	40	]]	
)



# Here the final CORPUS (comprising corpus of all documents) is done. Second line is to avoid duplicates later
# transformation ortographic punctuation
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'"," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x," ' "," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'''"," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x," ' "," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'"," ")))

# corpus composition
corpus_it <- corpus(corpus_vector)
corpus_it <- corpus_it[!duplicated(docvars(corpus_it)),] # remove duplicates
# variable date
corpus_it$datet <- as_date(corpus_it$datetimestamp)

# preparation for variables annotation

# variable: type of content
it_content_business <- c("Assinews"	,
                         "Bebeez.it"	,
                         "Bluerating Online"	,
                         "Il Sole 24 Ore-Online"	,
                         "Il Sole 24 Ore Digital Replica Edition of Print Edition"	,
                         "ItaliaOggi"	,
                         "ItaliaOggi7"	,
                         "La Legge Per Tutti"	,
                         "MF - Mercati Finanziari"	,
                         "Milano Finanza"	,
                         "Radiocor Italian Language Newswire"	,
                         "Industria Italiana")

# variable: geographical cover
it_cover_local <- c("Corriere Alto Adige"	,
                    "Corriere del Mezzogiorno"	,
                    "Corriere del Veneto"	,
                    "Corriere delle Alpi"	,
                    "Corriere di Bologna"	,
                    "Corriere Fiorentino"	,
                    "Gazzetta di Modena Online"	,
                    "Il Gazzettino"	,
                    "Il Gazzettino Online"	,
                    "Il Tirreno"	,
                    "L'Arena"	,
                    "La Gazzetta del Mezzogiorno"	,
                    "La Nuova Sardegna"	,
                    "La Provincia Pavese"	,
                    "La Repubblica Firenze"	,
                    "La Repubblica Milano"	,
                    "La Repubblica Torino"	,
                    "La Tribuna di Treviso"	,
                    "Messaggero Veneto"	,
                    "Messaggero Veneto Online")

# variable: political alignment
it_rating_left <- c("La Repubblica Firenze"	,
                    "La Repubblica Milano"	,
                    "La Repubblica Torino"	,
                    "Avvenire"	,
                    "Avvenire Online"	,
                    "Il Fatto Quotidiano"	,
                    "Il Fatto Quotidiano Online"	,
                    "La Repubblica"	,
                    "La Repubblica.it")

it_rating_neutral <- c("Corriere delle Alpi"	,
                       "Gazzetta di Modena Online"	,
                       "Il Gazzettino"	,
                       "Il Gazzettino Online"	,
                       "Il Tirreno"	,
                       "L'Arena"	,
                       "La Gazzetta del Mezzogiorno"	,
                       "La Nuova Sardegna"	,
                       "La Provincia Pavese"	,
                       "La Tribuna di Treviso"	,
                       "Messaggero Veneto"	,
                       "Messaggero Veneto Online"	,
                       "Assinews"	,
                       "Bebeez.it"	,
                       "Bluerating Online"	,
                       "Il Sole 24 Ore-Online"	,
                       "Il Sole 24 Ore Digital Replica Edition of Print Edition"	,
                       "ItaliaOggi"	,
                       "ItaliaOggi7"	,
                       "La Legge Per Tutti"	,
                       "MF - Mercati Finanziari"	,
                       "Milano Finanza"	,
                       "Radiocor Italian Language Newswire"	,
                       "Industria Italiana"	,
                       "24Ovest.it"	,
                       "Adnkronos - General News"	,
                       "Adnkronos - Labor News"	,
                       "Agenparl"	,
                       "Agenzia Giornalistica Italia"	,
                       "ANSA - Economic and Financial Service"	,
                       "ANSA - Political and Economic News Service"	,
                       "ANSA - Regional Service"	,
                       "Askanews"	,
                       "Citynews Italy"	,
                       "HuffPost Italia"	,
                       "Il Piccolo"	,
                       "Il Piccolo Online"	,
                       "Il Resto del Carlino"	,
                       "L'Espresso"	,
                       "La Nazione"	,
                       "La Stampa"	,
                       "Reuters - Notizie in Italiano")

it_rating_right <- c("Corriere Alto Adige"	,
                     "Corriere del Mezzogiorno"	,
                     "Corriere del Veneto"	,
                     "Corriere di Bologna"	,
                     "Corriere Fiorentino"	,
                     "Corriere della Sera"	,
                     "Corriere della Sera Magazines and Supplements"	,
                     "Corriere della Sera Online"	,
                     "Il Giornale"	,
                     "Il Giorno"	,
                     "Il Messaggero"	,
                     "Il Messaggero Online")

# variable: means of diffusion
it_diffusion_online <- c("Adnkronos - General News"	,
                         "Avvenire Online"	,
                         "Il Fatto Quotidiano Online"	,
                         "La Repubblica.it"	,
                         "Il Gazzettino Online"	,
                         "Messaggero Veneto Online"	,
                         "Bebeez.it"	,
                         "Il Sole 24 Ore-Online"	,
                         "La Legge Per Tutti"	,
                         "24Ovest.it"	,
                         "Adnkronos - Labor News"	,
                         "Agenparl"	,
                         "Askanews"	,
                         "HuffPost Italia"	,
                         "Il Piccolo Online"	,
                         "Corriere della Sera Online"	,
                         "Il Messaggero Online")

# variables annotation
corpus_it$content <- ifelse(docvars(corpus_it, "origin") %in% it_content_business,
                            "business", "general")

corpus_it$cover <- ifelse(docvars(corpus_it, "origin") %in% it_cover_local,"local","national")
corpus_it$diffusion <- ifelse(docvars(corpus_it,"origin") %in% it_diffusion_online, "online","paper")
corpus_it$rating <- ifelse(docvars(corpus_it,"origin") %in% it_rating_left,"left",
                           ifelse(docvars(corpus_it,"origin") %in% it_rating_right,"right","neutral") )



corpus_it$origin2 <- ifelse(docvars(corpus_it,"origin") %in% 
                              c("Adnkronos - General News","Adnkronos - Labor News"),"adnkronos",
                            ifelse(docvars(corpus_it,"origin") %in% c("ANSA - Economic and Financial Service",
                                                                      "ANSA - Political and Economic News Service","ANSA - Regional Service"),"Ansa",
                                   ifelse(docvars(corpus_it,"origin") %in% c("Avvenire Online","Avvenire"),"Avvenire",      ifelse(docvars(corpus_it, "origin") %in% c("Corriere Alto Adige","Corriere del Mezzogiorno","Corriere del Veneto","Corriere della Sera Magazines and Supplements","Corriere della Sera Online","Corriere della Sera","Corriere di Bologna","Corriere Fiorentino"), "Corriere Sera",
                                                                                                                                   ifelse(docvars(corpus_it, "origin") %in% c("Il Fatto Quotidiano Online","Il Fatto Quotidiano"), "Fatto Quotidiano",
                                                                                                                                          ifelse(docvars(corpus_it,"origin") %in% c("Il Gazzettino Online","Il Gazzettino"),"Il Gazzettino",
                                                                                                                                                 ifelse(docvars(corpus_it,"origin") %in% c("Il Messaggero Online","Il Messaggero"),"Il Messaggero",
                                                                                                                                                        ifelse(docvars(corpus_it,"origin") %in% c("Il Piccolo Online","Il Piccolo"),"Il Piccolo",
                                                                                                                                                               ifelse(docvars(corpus_it,"origin") %in% c( "Il Sole 24 Ore Digital Replica Edition of Print Edition","Il Sole 24 Ore-Online","Radiocor Italian Language Newswire"), "Sole 24 ore",
                                                                                                                                                                      ifelse(docvars(corpus_it,"origin") %in% c("ItaliaOggi","ItaliaOggi7" ), "ItaliaOggi",                      ifelse(docvars(corpus_it,"origin") %in% c("La Repubblica Firenze","La Repubblica Milano","La Repubblica Torino","La Repubblica.it","La Repubblica"), "La Repubblica",
                                                                                                                                                                                                                                                                                        ifelse(docvars(corpus_it,"origin") %in% c("Messaggero Veneto Online","Messaggero Veneto" ),"Messaggero Veneto" ,
                                                                                                                                                                                                                                                                                               ifelse(docvars(corpus_it,"origin") %in% c("MF - Mercati Finanziari","Milano Finanza" ),"Milano Finanza" ,
                                                                                                                                                                                                                                                                                                      corpus_it$origin)))))))))))))

corpus_it08 <- corpus_it
save(corpus_it08,file = "corpus_it08orig.Rdata")


####

## UPLOAD GERMAN CORPUS to use for all analysis #####
# upload for either keyness corpus-level & co-occurrences networks or keyness sentence-level

load("corpus_de08full.Rdata")
corpus_de08 <- corpus_de08[!duplicated(docvars(corpus_de08)),] # for duplicates




corpus_de08$country <- "Germany"
corpus_de08$covidtp[corpus_de08$datet < "2020-01-01"] <- "before 2020-01-01"
corpus_de08$covidtp[corpus_de08$datet >= "2020-01-01"] <- "after 2020-01-01"

corpus_de08$abbrev[corpus_de08$origin2 == "Handelsblatt"] <- "HB"
corpus_de08$abbrev[corpus_de08$origin2 == "Suddeutsche Zeitung"] <- "SZ"
corpus_de08$abbrev[corpus_de08$origin2 == "Die Welt"] <- "WT"
corpus_de08$abbrev[corpus_de08$origin2 == "Die Zeit"] <- "ZT"
corpus_de08$abbrev[corpus_de08$origin2 == "Spiegel"] <- "SP"
corpus_de08$abbrev[corpus_de08$origin2 == "Bild"] <- "BL"
corpus_de08$abbrev[corpus_de08$origin2 == "FAZ"] <- "FZ"
corpus_de08$rating[corpus_de08$origin2 == "FAZ"] <- "right"
# names to texts
docnames(corpus_de08) <- paste(corpus_de08$abbrev, corpus_de08$id, sep="_")  
# selection of newspapers of interest
corpus_de08 <-  corpus_subset(corpus_de08,
                              origin2 == "Suddeutsche Zeitung" | origin2 == "Die Welt" |
                                origin2 == "Die Zeit" | origin2 == "FAZ") 

# filter time >= 2020-01-01
corpus_de08 <- corpus_subset(corpus_de08,datet >= "2020-01-01")

# clean out texts (outliers, out of context etc., commented)
corpus_de08 <- corpus_subset(corpus_de08, !docnames(corpus_de08) %in% c( 
  
  #doubles similar
  "WT_WELTON0020201129egbt0005v","WT_WELTON0020201202egc20005p","WT_WELTON0020201215egce000mk",
  "WT_DWELT00020210112eh1c0000p","SZ_SDDZ000020210301eh310001w",
  "WT_WELTON0020210412eh4c000h9","ZT_DIEZEI0020220113ec8b002hj","ZT_DIEZEI0020220113ed1q005j1",
  "WT_WELTON0020180712ee7c000pa","WT_WELTON0020190705ef75000p8",
  "WT_WELTON0020190907ef97000bu","WT_DWELT00020200512eg5c0000h","WT_DWELT00020200618eg6i0000u",
  "WT_DWELT00020200824eg8o0000v","SZ_SDDZ000020200908eg980002f", 
  "FZ_SD1202003155958457",
  "FZ_FD1202003195963222",
  "FZ_FD2202008146064354",
  "FZ_SD1202008236066007","FZ_FD2202008266072045","FZ_FDA202009016076044","FZ_FD1202009036077473",
  "FZ_FZ_20200904", "FZ_FD1202009126082842","FZ_FD2202009166086472","FZ_FD1202009176087739","FZ_FZ_20200918",
  "FZ_FD2202010066098334","FZ_FZ_20201019", "FZ_FD1202011096122521", "FZ_SD1202011296136764","FZ_FZ_20201224",
  "FZ_FD1202104276234436","FZ_FD1202108025000526464873", #
  # outlier length # too short
  "WT_WSONNT0020200816eg8g0001g",
  "SZ_SDDZ000020210315eh3f0000c",
  # piece of information
  "WT_DWELT00020201124egbo0000t",
  "WT_WSONNT0020211009eha90000w",
  "WT_DWELT00020200512eg5c00014",
  "ZT_ZEITON0020200810eg8a0002t",
  "ZT_ZEITON0020200919eg9j0002v",
  "FZ_FD2202009046078725", # trend in bankruptcy
  # out of context
  "ZT_DIEZEI0020200827eg8r0000t", # shortening work hours,
  "WT_WELTON0020201214egce000gb", # interview with banker on  general finance state, not limited covid
  "ZT_DIEZEI0020201119egbj0000m", # story-like dossier on receving credit loan
  "FZ_FD1202007116039759", # interview to min. economy Peter Altmaier at time, out of context
  "WT_WELTON0020200427eg4q00005" , # history out of context
  "WT_WELTON0020200712eg7c0005l", # abroad enterprises
  "ZT_DIEZEI0020210805eh850000v", # competition automotive
  "WT_WELTON0020201228egcs000bz" , # meaningless DAX
  "SZ_SDDZ000020170714ed7e0001x" , # menaingless music
  "WT_WELTON0020211119ehbj00088" , # meaningless flies
  "SZ_SDDZ000020190311ef3b0001p" , # meaningless movie
  "ZT_DIEZEI0020130620e96k00029" , # meaningless movie
  "SZ_SUDZEIT020141117eabe0001f" , # meaningless movie
  "SZ_SDDZ000020161121ecbl0001z" , # meaningless movie
  "SZ_SDDZ000020190311ef3b0001p" , # meaningless movie
  "ZT_DIEZEI0020110922e79m0002v" , # meaningless movie
  "SZ_SDDZ000020170120ed1k0002b" , # meaningless movie
  "SZ_SUDZEIT020170713ed7d0002u" , # U2 concerts
  "SZ_SDDZ000020181112eebc0001r", # music album
  "SZ_SDDZ000020140818ea8i000gw", # essay economics
  "SZ_SDDZ000020201231egcv0001k", # personal diary
  "SZ_SDDZ000020200228eg2s0002t", # China
  "WT_WSONNT0020200322eg3m00054" # lifestyle
))

# Germany: corpus preparation for KEYNESS CORPUS-LEVEL AND CO-OCCURRENCES NETWORKS anlysis ####

corpus_de08 <- tolower(corpus_de08)
corpus_de08 <- gsub("'", " ",  corpus_de08)
corpus_de08 <- gsub("’", " ",  corpus_de08)
corpus_de08 <- gsub("‘", " ", corpus_de08)
corpus_de08 <- gsub("«", " ",  corpus_de08)
corpus_de08 <- gsub("»", " ",  corpus_de08)
corpus_de08 <- gsub("„"," ", corpus_de08)
corpus_de08 <- gsub("“"," ",corpus_de08)
corpus_de08 <- gsub("\"", " ",  corpus_de08)
corpus_de08 <- gsub("\n", " ", corpus_de08)
corpus_de08 <- gsub("\t", " ",  corpus_de08)
corpus_de08 <- gsub("\\s+", " ", corpus_de08)

# other file: corpus converted to text
tx_de <- convert(corpus_de08, to = "data.frame")


# bigram and variables annotation
# bg_de <- pull(read.csv("de_bigrams_08.csv"),2) # from previous automation process on whole corpus
bg_de <- read.xls("zombiefirms.xls",sheet = "de_bg", encoding = "latin1")[,1]
bg_denot <- read.xls("zombiefirms.xls",sheet = "de_bgnot", encoding = "latin1")[,1]
bg_de <- bg_de[! bg_de %in% bg_denot]

# lemmatization of compounds
zombiefirm_pattern_de <- read.xls("zombiefirms.xls",sheet = "de_lemma", encoding = "latin1")[,1] # no lemmatized terms 
zombiefirm_pattern_de <- paste0("\\b",zombiefirm_pattern_de,"\\b") # worked to convert
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma", encoding = "latin1")[,2] # lemmatized version
names(zombiefirm_replace_de) <- zombiefirm_pattern_de
tx_de$text <- str_replace_all(tx_de$text, zombiefirm_replace_de )

# removal list

zombiefirm_removepat <- read.xls("zombiefirms.xls",sheet = "de_rem", encoding = "latin1")[,1] # no lemmatized terms to remove
zombiefirm_removepat <- paste0("\\b",zombiefirm_removepat,"\\b") # worked to convert
zombiefirm_removerpl <- read.xls("zombiefirms.xls",sheet = "de_rem", encoding = "latin1")[,2] # lemmatized version to remove
names(zombiefirm_removerpl) <- zombiefirm_removepat
tx_de$text <- str_replace_all(tx_de$text, zombiefirm_removerpl )

rem_de <-  read.xls("zombiefirms.xls",sheet = "de_rem", encoding = "latin1")[,2]  # remove list
rem_de <- unique(rem_de)

corpus_de08 <- corpus(tx_de)

# document-term matrix 
dfm_de <-  tokens( corpus_de08,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
  #  tokens_compound(phrase(c(bg_de)))  %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de") ,rem_de)) %>% # , rem_dekey)) %>% 
  dfm()




## UPLOAD ITALIAN CORPUS to use for all analysis #####
# upload for either keyness corpus-level & co-occurrences networks or keyness sentence-level

load("corpus_it08orig.Rdata")
corpus_it08 <- corpus_it08[!duplicated(docvars(corpus_it08)),] # for duplicates


corpus_it08$country <- "Italy"
corpus_it08$covidtp[corpus_it08$datet < "2020-01-01"] <- "before 2020-01-01"
corpus_it08$covidtp[corpus_it08$datet >= "2020-01-01"] <- "after 2020-01-01"

corpus_it08$abbrev[corpus_it08$origin2 == "La Repubblica"] <- "RP"
corpus_it08$abbrev[corpus_it08$origin2 == "Corriere Sera"] <- "CS"
corpus_it08$abbrev[corpus_it08$origin2 == "Il Giornale"] <- "GN"
corpus_it08$abbrev[corpus_it08$origin2 == "Sole 24 ore"] <- "S24"
corpus_it08$abbrev[corpus_it08$origin2 == "Fatto Quotidiano"] <- "FQ"

# names to texts
docnames(corpus_it08) <- paste(corpus_it08$abbrev, corpus_it08$id, sep="_")   #  1:ndoc(corpus_de08), sep="_")

# selection of newspapers of interest
corpus_it08 <-  corpus_subset(corpus_it08,origin2 == "La Repubblica"|origin2 == "Corriere Sera"|origin2 == "Il Giornale"|origin2 == "Fatto Quotidiano") 

# filter time >= 2020-01-01
corpus_it08 <- corpus_subset(corpus_it08,datet >= "2020-01-01")

# clean out texts (outliers, out of context etc., commented)
corpus_it08 <- corpus_subset(corpus_it08, !docnames( corpus_it08) %in% c( 
  # double similar
   "GN_GIONLE0020130426e94q00082","CS_CORDES0020171218edci0000g","CS_CORSUP0020200706eg760000o", 
   "CS_CORDES0020210201eh210007q",
   "CS_CORSUP0020200406eg4600017","CS_CORONL0020200503eg53000br",
   #
   "CS_CORDES0020200629eg6t00017",
   "CS_CORDES0020201102egb20001h",
   "CS_CORSUP0020210419eh4j0000w",
   # outliers length # too short
   "RP_LAREP00020200720eg7k0003c",  
   "RP_LAREP00020210203eh230002c",
   # only piece of information
   "RP_LAREP00020201217egch0002h",
   "CS_CORDES0020201222egcm0002s",
   "GN_GIONLE0020210216eh2g0001l",
   "CS_CORDES0020210320eh3k0006t",
   "RP_LAREP00020210412eh4c0000i",
  # out of context
  "GN_GIONLE0020211003eha300010", # just mention
 "CS_CORVEN0020210209eh290000k", # summary
 "FQ_FATONL0020210310eh3a000bs", # summary
 "RP_REPONL0020211024ehao0028w", # not content
 "RP_REPONL0020191104efb4000ma" , # date 2019
  "CS_CORVEN0020210313eh3d00002", # about vaccine
  "CS_CORDES0020141219eacj00063", # about actress Gwyne Paltrow
  "FQ_FATONL0020210212eh2c0008w", # about Berlusconi, mention like joke (Berlusconi zombie company)
  "CS_CORDES0020160118ec1i00072", # about enterpreneur Calabrò, just mention
  "FQ_FATQUO0020150918eb9h0000m", # American federal bank, mention to Europe,
  "FQ_FATONL0020200115eg1f000b6", # Camorra case,
  "CS_CORONL0020200131eg1v000xf", # Jacobini family Puglia,
  "CS_CORDES0020200217eg2h0002o", # real madrid 
  "CS_CORONL0020200131eg1v00107",# Jacobini family Puglia,
  "CS_CORDES0020200201eg2100015", # Jacobini family Puglia,
  "RP_REPBAR0020200201eg210000k", # Jacobini family Puglia court,
  "RP_LAREP00020200506eg5600001", # Germany Karlsruhe decision vs state bond selling by EU
  "CS_CORONL0020200507eg570008e", # Germany kurzarbeit explanation
  "FQ_FATQUO0020200508eg570000n", # Personal reply
  "GN_GIONLE0020200514eg5e0003v", # not relevant, personal info
  "RP_LAREP00020200613eg6d00047", # cina
  "CS_CORDES0020200615eg6f0004u", # short info
  "CS_CORSUP0020200615eg6f0000m", # short info
  "RP_LAREP00020200622eg6m0000j", # 70's, not in relation to covid
  "CS_CORONL0020200719eg7j0005l", # Lega specific courtship case
  "CS_CORDES0020200724eg7o0007l", # Corneliani firm, out of covid
  "RP_REPTOR0020200803eg8300008", # courthsip, no covid
  "RP_LAREP00020200913eg9d0001s", # Milan singular court case
  "CS_CORONL0020200922eg9m000by", # mention in Brianza antigen
  "FQ_FATONL0020200923eg9n0008u", # mention in Brianza antigen
  "FQ_FATQUO0020200928eg9r0000h", # no Italy
  "RP_LAREP00020201019egaj0001p", # Cina
  "RP_LAREP00020201204egc400012", # Lega
  "CS_CORDES0020210116eh1g00083", # personal story
  "CS_CORSUP0020200629eg6t0000n", # out of context, general comment industry
  "CS_CORONL0020201031egav0008n", # summary of issue, no content 
  "CS_CORSUP0020201109egb90000y", # EU level
  "GN_GIONLE0020201211egcb00014", # ilva no covid
  "RP_REPNAP0020210117eh1h0000h", # napoli no covid
  "FQ_FATONL0020210129eh1s00008", # usa gamestop no covid
  "FQ_FATQUO0020210130eh1t0000r", # usa gamestop no covid, reflection ita
  "RP_REPTOR0020210218eh2i0000n", # individual court case, no covid
  "RP_REPTOR0020210218eh2i0000s", # individual court case, no covid
  "CS_CORONL0020210418eh4i001bi", # individual story, no covid
  "CS_CORDES0020210618eh6i00075", # small piece info
  "CS_CORDES0020210903eh930001t", # snapshot interview no covid
  "GN_GIONLE0020211016ehag0003i", # small input info, within series snapshots
  "RP_REPGEN0020211208ehc800002", # Ferrero footbal manager case, no covid
  "CS_CORONL0020211208ehc80006r", # Ferrero footbal manager case, no covid
  "RP_REPGEN0020211211ehcb00005", # Ferrero footbal manager case, no covid
  "RP_REPGEN0020211211ehcb0002b", # Ferrero footbal manager case, no covid
  "RP_REPGEN0020211218ehci0000g", # Ferrero footbal manager case, no covid
  "RP_REPGEN0020220113ei1d00004", # court case, no covid
  "CS_CORDES0020210209eh290004j", # piece of news
  "CS_CORSUP0020210713eh7d0000v", # about cooperatives, not politics involvement
  "GN_GIONLE0020210302eh320002j", # out of context,
  "GN_GIONLE0020210501eh5100003", # short, court case
  "CS_CORONL0020211103ehb30015p", # just mentioned, broad comment on Draghi
  "CS_CORONL0020200706eg76000en" # startup Intesa San Paolo
))


# Italy: corpus preparation for KEYNESS CORPUS-LEVEL AND CO-OCCURRENCES NETWORKS anlysis   ####

corpus_it08 <- tolower(corpus_it08)
corpus_it08 <- gsub("'", " ",  corpus_it08)
corpus_it08 <- gsub("’", " ",  corpus_it08)
corpus_it08 <- gsub("‘", " ", corpus_it08)
corpus_it08 <- gsub("«", " ",  corpus_it08)
corpus_it08 <- gsub("»", " ",  corpus_it08)
corpus_it08 <- gsub("„"," ", corpus_it08)
corpus_it08 <- gsub("“"," ",corpus_it08)
corpus_it08 <- gsub("\"", " ",  corpus_it08)
corpus_it08 <- gsub("\n", " ", corpus_it08)
corpus_it08 <- gsub("\t", " ",  corpus_it08)
corpus_it08 <- gsub("\\s+", " ", corpus_it08)

# other file: corpus converted to text
tx_it <- convert(corpus_it08, to = "data.frame")

# bigram and variables annotation
bg_it <- read.xls("zombiefirms.xls",sheet = "it_bg", encoding = "latin1")[,1]
bg_itnot <- read.xls("zombiefirms.xls",sheet = "it_bgnot", encoding = "latin1")[,1]
bg_it <- bg_it[! bg_it %in% bg_itnot]

# lemmatization of compounds
zombiefirm_pattern_it <- read.xls("zombiefirms.xls",sheet = "it_lemma", encoding = "latin1")[,1] # no lemmatized terms
zombiefirm_pattern_it <- paste0("\\b",zombiefirm_pattern_it,"\\b") # worked to convert
zombiefirm_replace_it <- read.xls("zombiefirms.xls",sheet = "it_lemma", encoding = "latin1")[,2] # lemmatized version
names(zombiefirm_replace_it) <- zombiefirm_pattern_it
tx_it$text <- str_replace_all(tx_it$text, zombiefirm_replace_it )

# removal list

zombiefirm_removepat_it <- read.xls("zombiefirms.xls",sheet = "it_rem", encoding = "latin1")[,1]
zombiefirm_removepat_it <- paste0("\\b",zombiefirm_removepat_it,"\\b")
zombiefirm_removerpl_it <- read.xls("zombiefirms.xls",sheet = "it_rem", encoding = "latin1")[,2]
names(zombiefirm_removerpl_it) <- zombiefirm_removepat_it
tx_it$text <- str_replace_all(tx_it$text,zombiefirm_removerpl_it)

rem_it <- read.xls("zombiefirms.xls",sheet = "it_rem", encoding = "latin1")[,2]
rem_it <- unique(rem_it)

corpus_it08 <- corpus(tx_it)

# document-term matrix 
dfm_it <-  tokens( corpus_it08,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
#  tokens_compound(phrase(c(bg_it))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>% 
  dfm()


# KEYNESS CORPUS-LEVEL ####
# here two different corpora where run for Italian and German corpus in order to combine for visualization

# German keyness
kn_de <- textstat_keyness(dfm_group(dfm_subset(dfm_de, datet >= "2020-01-01" ),groups = rating),
                          target = "right")
kn_de$country <- "Germany"

kn_it <- textstat_keyness(dfm_group(dfm_subset(dfm_it, datet >= "2020-01-01" ),groups = rating),
                          target = "right")
kn_it$country <-  "Italy"

# Combining keyness from the two sample

kn_defig <- kn_de[c(1:20,11903:11922),]  %>% mutate(feature = reorder(feature, chi2)) %>%
  mutate(refgrp = ifelse(chi2 < 0, "Left","Right"))
kn_itfig <- kn_it[c(1:20,12144:12164),]  %>% mutate(feature = reorder(feature, chi2)) %>%
  mutate(refgrp = ifelse(chi2 < 0, "Left","Right"))

rbind(kn_defig,kn_itfig) %>%
  ggplot(aes(x = chi2, y = feature, fill = refgrp)) + geom_col() +
  scale_fill_manual(values = c("Left" =  "grey","Right" = "black"),
                    name = "Political Leaning") + 
  # scale_x_continuous(breaks = c(min(kn_sen_de$chi2), -2.96790,0.30648,max(kn_sen_de$chi2))) +
  xlab(expression(chi^2)) + 
  facet_wrap(~ country, scales = "free") +
  theme_bw() +
  theme(axis.title.y = element_blank(), legend.position = "bottom")
ggsave(filename = "images/fig1.jpg", width = 4.5, height = 5) # 6 5


# Germany: CO-OCCURRENCES NETWORKS ####
# Co-occurrences were edited individually with code here, and editing of combined figure with external editor

# Left Germany
fcm_lf <- tokens(corpus_subset(corpus_de08,datet >= "2020-01-01" & rating == "left"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
# tokens_compound(phrase(c(bg_de))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de,
                  "heute","neuen","statt")) %>%
  fcm(context = "window", window = 20, tri = FALSE)  # 20 words window unit

# degree centrality (plotting undirected network), selecting top 30 words
co_occur_network_lf <- graph_from_adjacency_matrix(fcm_lf, mode = "undirected", diag = FALSE) 
nm_occ <- as.data.frame(names(V(co_occur_network_lf)))
dg_occ <- as.data.frame(strength(co_occur_network_lf))
df_occ <- cbind(nm_occ,dg_occ)
df_occ <- df_occ[order(-strength(co_occur_network_lf)),]
df_occ[1:21,]

co_occur_network_lf2 <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = df_occ[1:21,1]),
                                                    mode = "undirected", diag = FALSE)


E(co_occur_network_lf2)$weight <- 1
co_occur_network_lf2 <- simplify(co_occur_network_lf2, edge.attr.comb=list(weight="sum"))
co_occur_network_lf2$ref <- paste0(unique(corpus_de08$country)," ","Left")

tk_lf <- tkplot(co_occur_network_lf2) # manually modify layout
l_lf <- tkplot.getcoords(tk_lf) # to take layout from tkplot geo coordinate 

# plotting final graph, weights for editing
par(mar = c(0, 0,1.3 , 0)) 
plot(co_occur_network_lf2,
     layout = l_lf,
     vertex.size = ( strength(co_occur_network_lf2) / max(strength(co_occur_network_lf2)) * 20),
     vertex.shape = "circle",
     vertex.label =  paste0(V(co_occur_network_lf2)$name),  
     vertex.label.color = "black",
     vertex.label.cex = 2,
     vertex.color = "white", 
    #  edge.color = co_occur_network_lf2$coll,
     # edge.color = 4 * E(co_occur_network_lf2)$weight,
   edge.width = (E(co_occur_network_lf2)$weight / 3),
  #  edge.width = 2,
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network_lf2)$name == "zombie_firms",2,1),
     vertex.label.dist = 1.2
     
)
title(co_occur_network_lf2$ref,cex.main= 2)

# Right Germany
fcm_rt <- tokens(corpus_subset(corpus_de08,datet >= "2020-01-01" & rating == "right"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
# tokens_compound(phrase(c(bg_de))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de )) %>%
  fcm(context = "window", window = 20, tri = FALSE)  # 20 words window unit

# degree centrality (plotting undirected network), selecting top 30 words
co_occur_network_rt <- graph_from_adjacency_matrix(fcm_rt, mode = "undirected", diag = FALSE) # , weighted = T
nm_occrt <- as.data.frame(names(V(co_occur_network_rt)))
dg_occrt <- as.data.frame(strength(co_occur_network_rt))
df_occrt <- cbind(nm_occrt,dg_occrt)
df_occrt <- df_occrt[order(-strength(co_occur_network_rt)),]
df_occrt[1:21,]

co_occur_network_rt2 <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = df_occrt[1:21,1]),
                                                    mode = "undirected", diag = FALSE)

E(co_occur_network_rt2)$weight <- 1
co_occur_network_rt2 <- simplify(co_occur_network_rt2, edge.attr.comb=list(weight="sum"))
co_occur_network_rt2$ref <- paste0(unique(corpus_de08$country)," ","Right")

tk_rt <- tkplot(co_occur_network_rt2) # manually modify layout
l_rt <- tkplot.getcoords(tk_rt) # to take layout from tk_rt geo coordinate

# plotting final graph, weights for editing

par(mar = c(0, 0,1.3 , 0)) 
plot(co_occur_network_rt2,
     layout = l_rt,
     vertex.size = ( strength(co_occur_network_rt2) / max(strength(co_occur_network_rt2)) * 20),
     vertex.shape = "circle",
     vertex.label =  paste0(V(co_occur_network_rt2)$name),  
     vertex.label.color = "black",
     vertex.label.cex = 2,
     vertex.color = "white", 
     #  edge.color = co_occur_network_rt2$coll,
     # edge.color = 4 * E(co_occur_network_rt2)$weight,
     edge.width = (E(co_occur_network_rt2)$weight / 3),
    # edge.width = 2,
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network_rt2)$name == "zombie_firms",2,1),
     vertex.label.dist = 1.2
     
)
title(co_occur_network_rt2$ref,cex.main= 2)

# Italy: CO-OCCURRENCES NETWORKS ####

# Left Italian
fcm_lf <- tokens(corpus_subset(corpus_it08,datet >= "2020-01-01" & rating == "left"),
                remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(bg_it))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>% 
  fcm(context = "window", window = 20, tri = FALSE) # 20 words window unit

# degree centrality (plotting undirected network), selecting top 30 words
co_occur_network_lf <- graph_from_adjacency_matrix(fcm_lf, mode = "undirected", diag = FALSE)
nm_occ <- as.data.frame(names(V(co_occur_network_lf)))
dg_occ <- as.data.frame(strength(co_occur_network_lf))
df_occ <- cbind(nm_occ,dg_occ)
df_occ <- df_occ[order(-strength(co_occur_network_lf)),]
df_occ[1:21,]

co_occur_network_lf2 <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = df_occ[1:21,1]),
                                                    mode = "undirected", diag = FALSE)


E(co_occur_network_lf2)$weight <- 1
co_occur_network_lf2 <- simplify(co_occur_network_lf2, edge.attr.comb=list(weight="sum"))
co_occur_network_lf2$ref <- paste0(unique(corpus_it08$country)," ","Left")

tk_lf <- tkplot(co_occur_network_lf2) # manually modify layout
l_lf <- tkplot.getcoords(tk_lf) # to take layout from tkplot geo coordinate 

# plotting final graph, weights for editing
par(mar = c(0, 0,1.3 , 0)) 
plot(co_occur_network_lf2,
     layout = l_lf,
     vertex.size = ( strength(co_occur_network_lf2) / max(strength(co_occur_network_lf2)) * 20) , 
     vertex.shape = "circle",
     vertex.label =  paste0(V(co_occur_network_lf2)$name), 
     vertex.label.color = "black",
     vertex.label.cex = 2,
     vertex.color = "white",
     edge.width = (E(co_occur_network_lf2)$weight / 3),
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network_lf2)$name == "zombie_firms",2,1),
     vertex.label.dist = 1.2
     
)
title(co_occur_network_lf2$ref,cex.main= 2)

# Right Italian
fcm_rt <- tokens(corpus_subset(corpus_it08,datet >= "2020-01-01" & rating == "right"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(bg_it))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>%
  fcm(context = "window", window = 20, tri = FALSE) # 20 words window unit

# degree centrality, selecting top 30 words
co_occur_network_rt <- graph_from_adjacency_matrix(fcm_rt, mode = "undirected", diag = FALSE) 
nm_occrt <- as.data.frame(names(V(co_occur_network_rt)))
dg_occrt <- as.data.frame(strength(co_occur_network_rt))
df_occrt <- cbind(nm_occrt,dg_occrt)
df_occrt <- df_occrt[order(-strength(co_occur_network_rt)),]
df_occrt[1:21,]

co_occur_network_rt2 <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = df_occrt[1:21,1]),
                                                    mode = "undirected", diag = FALSE)


E(co_occur_network_rt2)$weight <- 1
co_occur_network_rt2 <- simplify(co_occur_network_rt2, edge.attr.comb=list(weight="sum"))
co_occur_network_rt2$ref <- paste0(unique(corpus_it08$country)," ","Right")


tk_rt <- tkplot(co_occur_network_rt2) # manually modify layout
l_rt <- tkplot.getcoords(tk_rt) # to take layout from tkplot geo coordinate

# plotting final graph, weights for editing
par(mar = c(0, 0,1.3 , 0)) 
plot(co_occur_network_rt2,
     layout = l_rt,
     vertex.size = (strength(co_occur_network_rt2) / max(strength(co_occur_network_rt2)) * 20) ,
     vertex.shape = "circle",
     vertex.label =  paste0(V(co_occur_network_rt2)$name),  
     vertex.label.color = "black",
     vertex.label.cex = 2,
     vertex.color = "white", 
     edge.width = (E(co_occur_network_rt2)$weight / 3), 
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network_rt2)$name == "zombie_firms",2,1),
     vertex.label.dist = 1.2
     
)
title(co_occur_network_rt2$ref,cex.main=2)


# Germany: corpus preparation for KEYNESS SENTENCE-LEVEL ####

# German corpus sentence-level
corpus_de08 <- corpus_reshape(corpus_de08, to = "sentences") 

corpus_de08 <- tolower(corpus_de08)
corpus_de08 <- gsub("'", " ",  corpus_de08)
corpus_de08 <- gsub("’", " ",  corpus_de08)
corpus_de08 <- gsub("‘", " ", corpus_de08)
corpus_de08 <- gsub("«", " ",  corpus_de08)
corpus_de08 <- gsub("»", " ",  corpus_de08)
corpus_de08 <- gsub("„"," ", corpus_de08)
corpus_de08 <- gsub("“"," ",corpus_de08)
corpus_de08 <- gsub("\"", " ",  corpus_de08)
corpus_de08 <- gsub("\n", " ", corpus_de08)
corpus_de08 <- gsub("\t", " ",  corpus_de08)
corpus_de08 <- gsub("\\s+", " ", corpus_de08)

# other file: corpus converted to text
tx_de <- convert(corpus_de08, to = "data.frame")


# lemmatization of synonymous
zombiefirm_pattern_de <- read.xls("zombiefirms.xls",sheet = "de_lemma", encoding = "latin1")[,1] # no lemmatized terms 
zombiefirm_pattern_de <- paste0("\\b",zombiefirm_pattern_de,"\\b") # worked to convert
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma", encoding = "latin1")[,2] # lemmatized version 
names(zombiefirm_replace_de) <- zombiefirm_pattern_de
tx_de$text <- str_replace_all(tx_de$text, zombiefirm_replace_de )

# removal list

zombiefirm_removepat <- read.xls("zombiefirms.xls",sheet = "de_rem", encoding = "latin1")[,1] # no lemmatized terms to remove
zombiefirm_removepat <- paste0("\\b",zombiefirm_removepat,"\\b") # worked to convert
zombiefirm_removerpl <- read.xls("zombiefirms.xls",sheet = "de_rem", encoding = "latin1")[,2] # lemmatized version to remove
names(zombiefirm_removerpl) <- zombiefirm_removepat
tx_de$text <- str_replace_all(tx_de$text, zombiefirm_removerpl )

rem_de <-  read.xls("zombiefirms.xls",sheet = "de_rem", encoding = "latin1")[,2]  # remove list
rem_de <- unique(rem_de)


corpus_de08 <- corpus(tx_de)


# selecting sentence-level
 
zombieterms <- unique(read.xls("zombiefirms.xls",sheet = "de_lemma")[,3]) # selecting terms
zombieterms <-  zombieterms[zombieterms != ""]
zombieterms <- c(zombieterms,"zombie")
zombieterms <- paste0("\\b",zombieterms,"\\b",collapse="|") 

tx_de_sen <- tx_de %>% filter(str_detect(text,zombieterms)) %>% filter(datet >= "2020-01-01") # filtering

corpus_de_sen <- corpus(tx_de_sen) # corpus  sentence-level


# Document-term matrix
dfm_de <-  tokens( corpus_de_sen,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
# tokens_compound(phrase(c(bg_de))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de,
                  "zombie_firms","zombification","zombie_unternehmen","zombie_wirtschaft","zombie_firma", # specific to sentence level
                  "unternehmenszombies", "zombie_banken", "zombifizierung","zombie","zombiefirms" # ,
                  # "entstehen","oecd-definition", "spd-politiker"
                  )) %>% 
  dfm()


# Italy: corpus preparation for KEYNESS SENTENCE-LEVEL ####
corpus_it08 <- corpus_reshape(corpus_it08, to = "sentences")

corpus_it08 <- tolower(corpus_it08)
corpus_it08 <- gsub("'", " ",  corpus_it08)
corpus_it08 <- gsub("’", " ",  corpus_it08)
corpus_it08 <- gsub("‘", " ", corpus_it08)
corpus_it08 <- gsub("«", " ",  corpus_it08)
corpus_it08 <- gsub("»", " ",  corpus_it08)
corpus_it08 <- gsub("„"," ", corpus_it08)
corpus_it08 <- gsub("“"," ",corpus_it08)
corpus_it08 <- gsub("\"", " ",  corpus_it08)
corpus_it08 <- gsub("\n", " ", corpus_it08)
corpus_it08 <- gsub("\t", " ",  corpus_it08)
corpus_it08 <- gsub("\\s+", " ", corpus_it08)

# other file: corpus converted to text
tx_it <- convert(corpus_it08, to = "data.frame")

# bigram and variables annotation
# bg_it <- read.xls("zombiefirms.xls",sheet = "it_bg", encoding = "latin1")[,1]
# bg_itnot <- read.xls("zombiefirms.xls",sheet = "it_bgnot", encoding = "latin1")[,1]
# bg_it <- bg_it[! bg_it %in% bg_itnot]

# lemmatization of compounds
zombiefirm_pattern_it <- read.xls("zombiefirms.xls",sheet = "it_lemma", encoding = "latin1")[,1] # no lemmatized terms
zombiefirm_pattern_it <- paste0("\\b",zombiefirm_pattern_it,"\\b") # worked to convert
zombiefirm_replace_it <- read.xls("zombiefirms.xls",sheet = "it_lemma", encoding = "latin1")[,2] # lemmatized version
names(zombiefirm_replace_it) <- zombiefirm_pattern_it
tx_it$text <- str_replace_all(tx_it$text, zombiefirm_replace_it )

# removal list

zombiefirm_removepat_it <- read.xls("zombiefirms.xls",sheet = "it_rem", encoding = "latin1")[,1]
zombiefirm_removepat_it <- paste0("\\b",zombiefirm_removepat_it,"\\b")
zombiefirm_removerpl_it <- read.xls("zombiefirms.xls",sheet = "it_rem", encoding = "latin1")[,2]
names(zombiefirm_removerpl_it) <- zombiefirm_removepat_it
tx_it$text <- str_replace_all(tx_it$text,zombiefirm_removerpl_it)

rem_it <- read.xls("zombiefirms.xls",sheet = "it_rem", encoding = "latin1")[,2]
rem_it <- unique(rem_it)

# corpus_it08 <- corpus(tx_it)

# selecting sentence-level
zombieterms <- unique(read.xls("zombiefirms.xls",sheet = "it_lemma")[,3]) # selecting terms
zombieterms <-  zombieterms[zombieterms != ""]
zombieterms <- c(zombieterms,"zombie")
zombieterms <- paste0("\\b",zombieterms,"\\b",collapse="|")

tx_it_sen <- tx_it %>% filter(str_detect(text,zombieterms)) %>% filter(datet >= "2020-01-01")

corpus_it_sen <- corpus(tx_it_sen)



# document-term matrix sentence-level
dfm_it <-  tokens( corpus_it_sen,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
 # tokens_compound(phrase((bg_it))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it,
                  "zombie_firms","zombiefirms", "zombie",
                    "aziende_zombie", "imprese_zombie", "zombie_company", "società_zombie", "organizzazione_zombie",
                   "zombiefirms","zombie")) %>% 
  dfm()


# KEYNESS SENTENCE-LEVEL analysis ####

# German analysis
kn_sen_de <- textstat_keyness(dfm_group(dfm_subset(dfm_de, datet >= "2020-01-01" ),groups = rating),
                              target = "right")
kn_sen_de$country <- unique(corpus_de_sen$country)


# selecting terms for visualization, hierarchy kept, terms selected have same rank of ones filtered out
kn_sen_de <- kn_sen_de %>% filter(feature %in% c("geschäftsführer","nullzinspolitik","niedrigen_zinsen","kapital", #right
                                      "insolvenzantragspflicht","wirtschaftsauskunftei_creditreform",
                                      "gesamtwirtschaftlichen","produktivität", 
                                      "warnen","nicht_überlebensfähig", "leben_halten","arbeitslosigkeit", # left
                                      "schulden","pleite","bezahlen","kritik")                   
)

# Italian analysis

kn_sen_it <- textstat_keyness(dfm_group(dfm_subset(dfm_it, datet >= "2020-01-01" ),groups = rating),
                              target = "right")
kn_sen_it$country <- unique(corpus_it_sen$country)

# selecting terms for visualization, hierarchy kept, terms selected have same rank of ones filtered out
kn_sen_it <- kn_sen_it %>% filter(feature %in% c("imprese","banca","società","credito",
                                                 "credito_garantito","gestione","debito","crediti", #right
                                                 "alitalia","draghi","ilva","sopravvivere","intervento_pubblico",  #left 
                                                   "politica_industriale","sostegno","danneggiare")
                                    )


rbind(kn_sen_it,kn_sen_de) %>% mutate(feature = reorder(feature, chi2)) %>%
  mutate(refgrp = ifelse(chi2 < 0, "Left","Right"))  %>% 
  ggplot(aes(x = chi2, y = feature, fill = refgrp)) + geom_col() +
  scale_fill_manual(values = c("Left" =  "grey","Right" = "black"),
                    name = "Political Leaning") + 
  # scale_x_continuous(breaks = c(min(kn_sen_de$chi2), -2.96790,0.30648,max(kn_sen_de$chi2))) +
  xlab(expression(chi^2)) + 
  facet_wrap(~ country, scales = "free",dir = "v") +
  theme_bw() +
  theme(axis.title.y = element_blank(), legend.position = "bottom")
ggsave(filename = "images/fig2.jpg", width = 5, height = 6)






textplot_keyness(kn_sen_it, 
                 margin = 0.1,
                 labelsize = 8, color = c("black","grey")) +
  ylab(kn_sen_it$country) +
  xlab(expression(chi^2)) +
  theme_bw()  +
  theme( axis.title.y = element_text(size = 20),
         axis.text.y = element_blank(),axis.ticks.y = element_blank(),
         axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 15),
         plot.title = element_text(hjust = 0.5),legend.position = "bottom",
         legend.text = element_text(size=20))
ggsave(filename = "images/knsenitfin.jpg", width = 15, height = 14) #  width = 22, height = 16)

# Combining figures
ggpubr::ggarrange(kn_sen_de_pl,kn_sen_it_pl,ncol  =1, nrow  = 2, common.legend = T,legend = "bottom")
ggsave(filename = "images/knsentoto.jpg", width = 23, height = 30) #  width = 22, height = 16)


# Qualitative analyses ####
# Same procedure for both corpora. Here generic "corpus"

# Selecting corpus-level
# corpus # text-level
# corpus <- corpus_reshape(corpus, to = "paragraph") # paragraph level
# corpus <- corpus_reshape(corpus, to = "sentences") # sentence level

# to filter out texts, paragraphs or sentences with specific term "xxxx"
# tx <- convert(corpus, to = "data.frame")
# checksen <- tx %>% filter(str_detect(text,"\\bxxxx\\b"))

# Code of newspapers' articles reported are:
# WT_WSONNT0020200816eg8g0001n (Die Welt, August 2020)
# SZ_SUDZEIT020220115ei1e00034 (Süddeutsche Zeitung, January 2021)
# CS_CORDES0020201222egcm00066 (Corriere della Sera, December 2020)
# RP_LAREP00020210221eh2l00038 (La Repubblica, February 2021)
# SZ_SUDZEIT020201123egbm0001i (Süddeutsche Zeitung, November 2020)
# CS_CORDES0020201221egcl000eo (Corriere della Sera, December 2020)
# SZ_SUDZEIT020210129eh1s0001x (Süddeutsche Zeitung, January 2021)











