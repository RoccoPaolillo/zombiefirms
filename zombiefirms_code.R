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
library(dendextend)
library(ggdendro)
library(lubridate)
library(stringr)
library(ggrepel)
library(tidyr)
library(dplyr)
library(tidytext)
library(igraph)
library(gdata)
library(readtext)
library(diffobj)

setwd("")

# Background code (cleaning etc.), not needed for analysis reproduction. Corpora are already prepared ####

## to find regex (root words/combinations) of "zombie firms" terms

df_word <- unique(stringr::str_extract_all(txdf$text,
        regex("\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]*sistema \\-?[:alnum:]*\\-?[:alnum:]*",
                                                 ignore_case = TRUE)))

unique(unlist(df_word))

## check doubles and out of context texts
# 
# dfm_sim <- dfm(tokens(corpus_de08)) # corpus needs to be uploaded
# 
# jaccard similarity computed
# sim <- textstat_simil(dfm_sim,
#                       method="jaccard",margin="documents", min_simil = 0.70)
# 
# # take off diagonal: same item compared to itself
# simlist <- as.list(sim,diag=FALSE)
# 
# check specific text
# a <- textstat_simil(dfm_sim, dfm_sim["xxx",], margin = "documents", method = "jaccard")

# comparing texts
# checktest <- txit %>% filter(str_detect(text,regex("xxx", ignore_case = T)))
# txck <- txde %>% filter(doc_id == "FZ_FZ_20210801" | doc_id == "FZ_FD1202108025000526464873")
# diffObj(txck[1,]$text,txck[2,]$text)

## correspondence analysis to identify texts that distribute too far from others on underlying dimensions,
## signaling to be then check manually

# # correspondence analysis one-dimensional
# ca <- textmodel_ca(dfm_sim)
# tca <- textplot_scale1d(ca, margin = "documents",  groups = docvars(corpus_de08, "origin2"))
# plotly::ggplotly(tca)


## check for length outliers to clean out

# compute ntoken (number of words) for each text

# corpus_df$ntok <- ntoken(corpus_df)
# txdf <- quanteda::convert(corpus_df,to = "data.frame")
# 
# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
# }
# 
# txdf %>% group_by(origin2) %>% mutate(outlier = if_else(is_outlier(ntok), doc_id, NA_character_)) %>% 
#   ggplot(aes(x = origin2, y = ntok, color = origin2))  + geom_boxplot() + # geom_point() +
#   geom_text_repel(aes(label = outlier), na.rm = TRUE) +
#   stat_summary(fun=mean, geom="point",shape = 20,color="red") +
#   ylab("Count tokens") +
#   xlab("Articles") + 
#   labs(color = "Source") +
#   theme_bw() +
#   theme(axis.text.x = element_blank())
# 
# # length total corpus
# 
# t <- txdf %>% mutate(outlier = if_else(is_outlier(ntok), doc_id, NA_character_)) %>% 
#   ggplot(aes( y = ntok))  + geom_boxplot() + 
#   ylab("Number Tokens") +
#   ggtitle("Italy") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# plotly::ggplotly(t)


## to identify covid-containing text
# txit$cvid <- str_detect(txit$text, regex("covid*|corona*|coronavirus|corona-virus|virus|sars-cov-2", ignore_case = TRUE))
# a <- txit %>% filter(datet >= "2020-01-01" & cvid == TRUE)

## Analysis reproduction. For each language corpora need to be uploaded and wrangled. 
## To change in corpus_df to run, where otherwise  ####

## ITALY PROCESSING #####

load("corpus_it08orig.Rdata")
# bigrams, trigrams (not used) and variables
bg <- pull(read.csv("it_bigrams_08.csv"),2)
trg <- pull(read.csv("it_trigrams_08.csv"),2)
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

# selection of journals of interest
corpus_it08 <-  corpus_subset(corpus_it08,origin2 == "La Repubblica"|origin2 == "Corriere Sera"|origin2 == "Il Giornale"|origin2 == "Fatto Quotidiano") 

# clean out texts (outliers, out of context etc., commented)
corpus_it08 <- corpus_subset(corpus_it08, !docnames( corpus_it08) %in% c( 
  "GN_GIONLE0020130426e94q00082","CS_CORDES0020171218edci0000g",  # double similar
  "CS_CORVEN0020210313eh3d00002","RP_REPONL0020191104efb4000ma","RP_REPONL0020211024ehao0028w",  # outliers length
  "CS_CORVEN0020210209eh290000k",
  # out of context
  "CS_CORDES0020141219eacj00063", # pointless (actor Gwyne Paltrow)
  "CS_CORONL0020200706eg76000en", # university start-up
  "FQ_FATONL0020210212eh2c0008w" # about Berlusconi, mention like joke (Berlusconi zombie company)
))




# GERMANY PROCESSING ####

# background: to include FAZ papers acquired apart
# fazbought <- readtext(paste0("FAZ_boughtpapers/*",".pdf"),
#                       docvarsfrom = "filenames",
#                       docvarnames = c("datet", "origin2","id"),
#                       sep = "_")
# 
# corpus_faz <- corpus(fazbought)
# 
# # corpus_faz$abbrev[corpus_faz$origin2 == "FAZ"] <- "FZ"
# 
# # docnames(corpus_faz) <- paste(corpus_faz$abbrev, corpus_faz$id, sep="_") 
# 
# corpus_de08 <- corpus_de08 + corpus_faz
# save(corpus_de08,file="corpus_de08full.Rdata")


load("corpus_de08full.Rdata")
# bigrams, trigrams (not used) and variables
bg <- pull(read.csv("de_bigrams_08.csv"),2)
trg <- pull(read.csv("de_trigrams_08.csv"),2)
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
# selection of journals of interest
corpus_de08 <-  corpus_subset(corpus_de08,
                              origin2 == "Suddeutsche Zeitung" | origin2 == "Die Welt" |
                                origin2 == "Die Zeit" | origin2 == "FAZ") 

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
  
 # #outliers length,
  "ZT_DIEZEI0020201119egbj0000m","WT_WSONNT0020140921ea9l0004y","ZT_DIEZEI0020210805eh850000v",
  "WT_WELTON0020200712eg7c0005l","WT_WELTON0020200427eg4q00005" ,
 "FZ_FD1202007116039759","FZ_SD1202101316179362",
 # meaningless
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
 #
 "SZ_SDDZ000020140818ea8i000gw", # essay economics
 "SZ_SDDZ000020201231egcv0001k", # personal diary
 "SZ_SDDZ000020200228eg2s0002t", # China
 "WT_WSONNT0020200322eg3m00054" # lifestyle
))

# 

# clean corpus ####

corpus_df <- corpus_it08

corpus_df <- tolower(corpus_df)
corpus_df <- gsub("'", " ",  corpus_df)
corpus_df <- gsub("’", " ",  corpus_df)
corpus_df <- gsub("‘", " ", corpus_df)
corpus_df <- gsub("«", " ",  corpus_df)
corpus_df <- gsub("»", " ",  corpus_df)
corpus_df <- gsub("„"," ", corpus_df)
corpus_df <- gsub("“"," ",corpus_df)
corpus_df <- gsub("\"", " ",  corpus_df)
corpus_df <- gsub("\n", " ", corpus_df)
corpus_df <- gsub("\t", " ",  corpus_df)
corpus_df <- gsub("\\s+", " ", corpus_df)

# other file: corpus converted to corpus
txdf <- convert(corpus_df, to = "data.frame")

# lemmatization and back Germany (mainly zombiefirms) #### 
# same for Italian corpus)

zombiefirm_pattern_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,1] # no lemmatized terms 
zombiefirm_pattern_de <- paste0("\\b",zombiefirm_pattern_de,"\\b") # worked to convert
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,2] # lemmatized version
names(zombiefirm_replace_de) <- zombiefirm_pattern_de
txdf$text <- str_replace_all(txdf$text,regex(zombiefirm_replace_de,ignore_case = T))

corpus_df <- corpus(txdf)
# terms to remove, from key-center co-occurrences
rem_dekey <- read.xls("zombiefirms.xls",sheet = "rem_dekey")[,1]
#terms to remove
rem_de <- c("die welt","faz.net","die zeit","suddeutsche zeitung","handelsblatt","spiegel","f.a.z.","f.a.z",
            "faz","f.az","f a z","fa.z","1","2","3","4","5","6","7","welt","darf","schließlich", "000",
            "immer", "unternehmen","firmen","trotzdem" , "nämlich" ,  "nennt","zweiten","besser","frankfurt-hahn",
            "immerhin",
            "schwer","rund","wegen","denen","sz","WELT AM SONNTAG",
            "leben_erhalten_schrieb_michael_hüther_direktor","deutschland","später",
            "dass","zahl","prozent","viele","mehr","schon","sei","gibt","sagt","sagte","dabei","menschen","seien","diesmal",
            "sitzen","darin","geht","seit","sogar","tun","gewaltigen_anzahl_sogenannter_zombieunternehmen_führen_angesichts",
            "komme", "kommst", "kommt", "kommen",
            "muss","musst","muss","müssen","müsst","warum",
            "soll","sollst","sollt","sollen", "sollt", "sollen",
            "laut","jahr","ende","etwa","etwas","moglich", "allerdings","uhr","ezb","ab",
            "kann","kannst","kann", "können", "könnt", "könnte","könnten","könne","fast",
            "gut", "zudem", "eigentlich" , "weitere", "bisher", "weniger","iw","gar","hoch",
            "allerdings",  "möglich","dafür", "wäre" ,"gerade" ,"jahren", "ja", "bereits", "derzeit","blick", "rheinland-pfalz",
            "anteil","daher","viertagewoche","sagen","sagt","anfang","gehen","gab","gab es","hochbahn","benex","tepco", "zugbegleiter",
            "bahn",
            "millionen_unternehmen_entspricht","passiert","lange","erst","macht","wären","hälfte",
            #
            "quelle","sollten","heißt","längst","hatte","stellt","hätten","müssten",
            "teil","sicht","sehen","besteht","sewing","dadurch","wohl","wann","hätte",
            "jedoch","patrik-ludwig","viele_menschen"
)

# compound terms
compound_de <- c("europäische union","zombie firma","zombie unternehmen","zombie firmen",
                 "deutschen banken", "jens ehrhardt",	
                 "carsten dierig","paschal donohoe",	
                 "lucas flöther","alexander herzog","flüchtlingswelle 2015","millionen kurzarbeiter",
                 "ifo geschäftsklimaindex","insolvenzen abgewendet","jörg hofmann","mark schieritz",
                 "stefan bratzel",	"isabel schnabel","jan roth","corona hilfen",		
                 "paul ziemiak","insolvenz tsunami","euro gruppe", "euro zone","billige geld", 	
                 "sieben tage", "erreicht worden","corona-hilfen","corona-hilfe",
                 #
                 "märz 2020","im griff","in den griff")

# Document-term matrix
dfm_df <-  tokens( corpus_df,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de, rem_dekey)) %>% 
  dfm()

# to check frequency
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,2]
freq_stat <- textstat_frequency(dfm_df,groups = rating) %>% 
  subset(feature %in% c(unique(zombiefirm_replace_de)))









# lemmatization and back Italy (mainly zombiefirms) #### 
# see aboove German corpus for the code)

zombiefirm_pattern_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,1]
zombiefirm_pattern_it <- paste0("\\b",zombiefirm_pattern_it,"\\b")
zombiefirm_replace_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,2]
# zombiefirm_replace <- paste0("\\b",zombiefirm_replace,"\\b",collapse="|")
names(zombiefirm_replace_it) <- zombiefirm_pattern_it
txdf$text <- str_replace_all(txdf$text,regex(zombiefirm_replace_it,ignore_case = T))
# txit$text <- stri_replace_all_fixed(txit$text, zombiefirm_pattern,  zombiefirm_replace, vectorize_all=FALSE)

corpus_df <- corpus(txdf)

rem_it <- c("fra","già?","già","oltre","ieri","può","soprattutto","molto","grandi","meno","tutto","tutti",
            "c'é","c'e","state","essere","percentuale","imprese","solo","parte","ne","cosa","fare",
            "c’è","c ’è","c'è","c 'è","d", "avere","milano","torino","bologna","napoli","pisa","quindi",
            "volta","veneto", "termini","fatto quotidiano","la repubblica","corriere della sera", "il giornale",
            "il corrier","corriere","il fatto","https*","fatto_quotidiano","fatto_quotidiano","fatto","poi", "mai","prima",
            "cento","gran_parte","gran* parte", #
            "anni","soltanto","imprese_intervistate","imprese intervistate","d accordo","d_accordo","circa","però",
            "proprio","d_italia","allora","fa","ciò","stata","qui","altra","vero","là","dopo","dare",
            "ottenuto","città","febbraio",
            "attesa", "mondo", "chiusura","ultima","domanda","invece", "senso", "giorni","passato",
            "addirittura", "base","chiaro", "poco", "alto","ora","ancora", "considera","posto","luglio",
            "presenza", "tanto", "prova","tutte","sorta","fronte","causa","mentre","quando",
            "fino","secondo","novembre","terzo", "mettere", "mesi", "gennaio","altri","intervista",
            "agosto", "spiegato", "presentato","tratta","punto","seguito","caso","deciso", #
            "modo","quali","quasi"
            
) 

compound_it <- c( "aziende zombie","imprese zombie","zombie company","società zombie","organizzazione zombie",
                  "cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
                  "unione europea","salva italia","decreto ristori","decreti ristoro",
                  "lotto di vaccini","istituto di credito","instituti di credito",
                  "piano nazionale","sistema produttivo","sistemi produttivi",
                  "rapporto finanziario","made in italy","tassi di interesse", "tasso di interesse",
                  "tassi d'interesse","tasso d'interesse","interessi economici","interesse economico",
                  "ambito finanziario","termini di resa","termini di interesse","ambito lavorativo",
                  "camera di commercio","ministero dello sviluppo economico","rapport eu",
                  "stati finanziari","stati di bilancio","sviluppo economico",
                  "Ferretti Group","Gruppo Ferretti","Ferretti Yachts",
                  "blocco dei licenziamenti","blocco licenziamenti", "blocco_dei_licenziamenti",
                  "blocco_licenziamenti",
                  "sistema economico","sistemi economici","sistema politico","sistemi politici",
                  "terzo trimestre",
                  "sistema politico","sistema sanitario","sistema democratico","sistema finanziario","sistema imprenditoriale",
                  "sistema educativo","sistema paese","sistema fiscale","sistema produttivo","sistema capitalistico",
                  "sistema produttivo","sistema economico","sistema bancario","sistema industriale",
                  "fine pandemia","fine della pandemia", "in piedi", "pioggia di soldi","alto rendimento","alto adige",
                  "posto di lavoro","posto lavoro", "crisi economica","crisi finanziaria","crisi del 2008",
                  "posti di lavoro","presidente del consiglio","a favore")


dfm_df <-  tokens( corpus_df,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_it,bg))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>% 
  dfm()


zombiefirm_replace_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,2]
freq_stat <- textstat_frequency(dfm_df, groups = rating) %>% 
  subset(feature %in% c(zombiefirm_replace_it))



# keyness, repeated for Italy and Germany ####


kn_de <- textstat_keyness(dfm_group(dfm_subset(dfm_df, datet >= "2020-01-01" ),groups = rating),
                 target = "right")

kn_it <- textstat_keyness(dfm_group(dfm_subset(dfm_df, datet >= "2020-01-01" ),groups = rating),
                          target = "right")

kn_deplot <- textplot_keyness(kn_de, n = 20, margin = 0.1,
                 labelsize = 8, color = c("black","grey")) + 
# ggtitle("Italy") +
  ylab("Germany") +
  xlab(expression(chi^2)) +
 theme_bw() +
  theme( axis.title.y = element_text(size = 20),
    axis.text.y = element_blank(),axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.text = element_text(size=20))

kntot <- ggpubr::ggarrange(kn_deplot,kn_itplot,ncol  =1, nrow  = 2, common.legend = T,legend = "bottom")
ggsave(kn_deplot,file="images/kn_deplot.jpg", width = 16, height = 14)


# tk_df (co-occurrence) ITA ####

# co-occurrences feature matrix Germany left

fcm_lf <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "left"),
                remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_it,bg))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>%
  fcm(context = "window", window = 20) 
fctop_lf <- topfeatures(fcm_lf, 20) %>% names()
# co-occurrence plot left
co_occur_network_lf <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = fctop_lf), mode = "undirected", diag = FALSE)
E(co_occur_network_lf)$weight <- count.multiple(co_occur_network_lf)
co_occur_network_lf <- simplify(co_occur_network_lf)
# tkplot for interactive networks (don't close while working on the code!)
tk_lf <- tkplot(co_occur_network_lf)
l_lf <- tkplot.getcoords(tk_lf) # take tk_lf coordination(s)

# Sane procedure for Germany right
fcm_rt <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "right"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_it,bg))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>%
  fcm(context = "window", window = 20) 
fctop_rt <- topfeatures(fcm_rt, 20) %>% names()
co_occur_network_rt <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = fctop_rt), mode = "undirected", diag = FALSE)
E(co_occur_network_rt)$weight <- count.multiple(co_occur_network_rt)
co_occur_network_rt <- simplify(co_occur_network_rt)
tk_rt <- tkplot(co_occur_network_rt)
l_rt <- tkplot.getcoords(tk_rt)

# tk_df (co-occurrence) DEU ####
fcm_lf <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "left"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de)) %>%
  fcm(context = "window", window = 20) 
fctop_lf <- topfeatures(fcm_lf, 20) %>% names()
co_occur_network_lf <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = fctop_lf), mode = "undirected", diag = FALSE)
E(co_occur_network_lf)$weight <- count.multiple(co_occur_network_lf)
co_occur_network_lf <- simplify(co_occur_network_lf)
tk_lf <- tkplot(co_occur_network_lf)
l_lf <- tkplot.getcoords(tk_lf)

fcm_rt <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "right"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de)) %>%
  fcm(context = "window", window = 20) 
fctop_rt <- topfeatures(fcm_rt, 20) %>% names()
co_occur_network_rt <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = fctop_rt), mode = "undirected", diag = FALSE)
E(co_occur_network_rt)$weight <- count.multiple(co_occur_network_rt)
co_occur_network_rt <- simplify(co_occur_network_rt)
# tkplot for interactive network to manipulate. An interactive window will open to work with
# Leave the window open until end of plotting final figures.
tk_rt <- tkplot(co_occur_network_rt) 
# to take layout from tkplot geo coordinate 
l_rt <- tkplot.getcoords(tk_rt)


# CO-occurrences ####


png("images/IT_co-occ.png")

 # left co-occurrences
 par(mar = c(0, 0,1.3 , 0)) 
plot(co_occur_network_lf,
     layout = l_lf,
      vertex.size = eigen_centrality(co_occur_network_lf)$vector * 15,
     vertex.shape = "circle",
     vertex.label = V(co_occur_network_lf)$name,
     vertex.label.color = "black",
     vertex.label.cex = 2,
     vertex.color = "grey",
     edge.width = (E(co_occur_network_lf)$weight / 4),
    
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network_lf)$name == "zombiefirms",2,1),
     vertex.label.dist = 1.8
    
)
title("Germany Left",cex.main=2)

plot(co_occur_network_rt,
     layout = l_rt,
     # main = "Italy Right",
     vertex.size = eigen_centrality(co_occur_network_rt)$vector * 15,
     vertex.shape = "circle",
     vertex.label = V(co_occur_network_rt)$name,
     vertex.label.color = "black",
     vertex.label.cex = 2,
     vertex.color = "grey",
     edge.width = (E(co_occur_network_rt)$weight / 4),
    
     vertex.label.font = ifelse(V(co_occur_network_rt)$name == "zombiefirms",2,1),
     vertex.label.dist = 1
)
title("Germany Right",cex.main=2)


# occurrences to specific term, repeated for each document-term-matrix ####

calculateCoocStatistics <- function(coocTerm, binDTM, measure = "DICE"){
  
  # Ensure Matrix {SparseM} or matrix {base} format
  require(Matrix)
  
  # Ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM[binDTM > 1] <- 1
  }
  
  # calculate cooccurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  
  # retrieve numbers for statistic calculation
  k <- nrow(binDTM)
  ki <- sum(binDTM[, coocTerm])
  kj <- colSums(binDTM)
  names(kj) <- colnames(binDTM)
  kij <- coocCounts[coocTerm, ]
  
  # calculate statistics
  switch(measure, 
         DICE = {
           dicesig <- 2 * kij / (ki + kj)
           dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
           sig <- dicesig
         },
         LOGLIK = {
           logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
                          + (k - ki - kj + kij) * log(k - ki - kj + kij) 
                          + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
                          - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
           logsig <- logsig[order(logsig, decreasing=T)]
           sig <- logsig    
         },
         MI = {
           mutualInformationSig <- log(k * kij / (ki * kj))
           mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
           sig <- mutualInformationSig    
         },
         {
           sig <- sort(kij, decreasing = TRUE)
         }
  )
  sig <- sig[-match(coocTerm, names(sig))]
  return(sig)
}

numberOfCoocs <- 15

dfm_dfcooc <- dfm_subset(dfm_df, datet >= "2020-01-01" & rating == "right")  # subcorpus

coocCounts <- t(dfm_dfcooc) %*% dfm_dfcooc

coocTerm <- "aziende_zombie"            # term to search

k <- nrow(dfm_dfcooc)
ki <- sum(dfm_dfcooc[, coocTerm])
kj <- colSums(dfm_dfcooc)
names(kj) <- colnames(dfm_dfcooc)
kij <- coocCounts[coocTerm, ]



coocs <- calculateCoocStatistics(coocTerm, dfm_dfcooc, measure="LOGLIK")

resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))


tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs] #3

# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs] #3

# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)

# Iteration over the most significant numberOfCoocs co-occurrences of the search term
for (i in 1:numberOfCoocs){

  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, dfm_dfcooc, measure="LOGLIK")

  #print the co-occurrences
  coocs2[1:10]

  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]

  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}

# resultGraph[sample(nrow(resultGraph), 6), ]

graphNetwork <- graph.data.frame(resultGraph, directed = F)

graphVs <- V(graphNetwork)[degree(graphNetwork) < 1] # Here for more or less clusters, giving min vertex node to appear <<<<<
graphNetwork <- delete.vertices(graphNetwork, graphVs)

V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange')

# Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
# E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
E(graphNetwork)$width <- ifelse(E(graphNetwork)$sig > halfMaxSig, 4, 1)



# Disable edges with radius
E(graphNetwork)$curved <- 0
# Size the nodes by their degree of networking
V(graphNetwork)$size <- log(degree(graphNetwork)) * 5


rem_dekey <- read.xls("zombiefirms.xls",sheet = "rem_dekey")[,1]
rem_itkey <- read.xls("zombiefirms.xls",sheet = "rem_itkey")[,1]
connected_key <- resultGraph %>% filter(from == coocTerm )
connected_key2 <- resultGraph %>% filter(duplicated(resultGraph$to))
connected_key <- rbind(connected_key, connected_key2)

keep_it <- read.xls("zombiefirms.xls",sheet = "rem_itkey")[,2]
keep_it <- c(keep_it,unique(connected_key),coocTerm)


  
tk_kc <- tkplot(graphNetwork)
tk_kc_l <- tkplot.getcoords(tk_kc)

par(mar = c(0,0,1,0))
plot(graphNetwork,
      layout = tk_kc_l,
     #  layout = layout.fruchterman.reingold,  # Force Directed Layout
    # main = paste(unique(dfm_dfcooc$country),",", unique(dfm_dfcooc$rating),",", unique(dfm_dfcooc$covidtp),":", coocTerm ),
     vertex.label.family = "sans",
     vertex.label.cex = ifelse(V(graphNetwork)$name == coocTerm, 1.5, 1.4),
     vertex.shape = ifelse(V(graphNetwork)$name == coocTerm, 'square', ifelse(V(graphNetwork)$name %in% connected_key, 'square', 'circle') ),
     vertex.label.dist = 0,           # Labels of the nodes moved slightly
     vertex.frame.color = 'darkolivegreen',
     vertex.label.color = 'black',      # Color of node names
     vertex.label.font = ifelse(V(graphNetwork)$name == coocTerm, 2, 1),         # Font of node names
 # To manipulate labels to hide 
  # vertex.label = ifelse(V(graphNetwork)$size >= 2, V(graphNetwork)$name,""  ),
  # vertex.label =  ifelse(V(graphNetwork)$name %in% toshow, V(graphNetwork)$name,"" ),       # node names
 # vertex.label =  ifelse(V(graphNetwork)$name %in% connected_key, V(graphNetwork)$name,
 #                        ifelse(V(graphNetwork)$name %in% rem_dekey," " ,V(graphNetwork)$name )), # remedy names  remkey
 # vertex.label =  ifelse(V(graphNetwork)$name %in% connected_key, V(graphNetwork)$name,
 #                        ifelse(V(graphNetwork)$name %in% rem_itkey," " ,V(graphNetwork)$name )), # remedy names  remkey
# vertex.label =  ifelse(V(graphNetwork)$name %in% keep_it, V(graphNetwork)$name," "), # remedy names  remkey
 
 
 #  vertex.label =  V(graphNetwork)$name,
    # vertex.label.cex = 1, # font size of node names
     edge.width = ifelse(E(graphNetwork)$sig > halfMaxSig, 4, 1),
     vertex.color = "grey"
)
title("Italy Right",cex.main=1.5)
# dev.off()



