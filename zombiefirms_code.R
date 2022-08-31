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

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/zombie_firms/I_review/test_original/")

# check word

# txde <- quanteda::convert(corpus_de08,to = "data.frame")
# txde$text <- tolower(txde$text)
# df_word <- unique(stringr::str_extract_all(txde$text,
#         regex("\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]*corona-\\-?[:alnum:]*\\-?[:alnum:]*",
#                                                  ignore_case = TRUE)))
# 
# unique(unlist(df_word))
# 
# checktest <- txit %>% filter(str_detect(text,regex("xxx", ignore_case = T)))

# txck <- txde %>% filter(doc_id == "FZ_FZ_20210801" | doc_id == "FZ_FD1202108025000526464873")
# diffObj(txck[1,]$text,txck[2,]$text)


# zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,2]
# zb <- list()
# for (i in unique(zombiefirm_replace_de)){
#   
#   sm <- sum(str_count(txde$text,i))
#   df <- tibble(i,sm)
#   zb[[i]] <- df
# }
# 
# dffin <- bind_rows(zb, .id = "name")

## ITALY PROCESSING #####

load("corpus_it08orig.Rdata")

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

docnames(corpus_it08) <- paste(corpus_it08$abbrev, corpus_it08$id, sep="_")   #  1:ndoc(corpus_de08), sep="_")

corpus_it08 <-  corpus_subset(corpus_it08,origin2 == "La Repubblica"|origin2 == "Corriere Sera"|origin2 == "Il Giornale"|origin2 == "Fatto Quotidiano") 


# Germany processing ####

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

docnames(corpus_de08) <- paste(corpus_de08$abbrev, corpus_de08$id, sep="_")  

corpus_de08 <-  corpus_subset(corpus_de08,|
                              origin2 == "Suddeutsche Zeitung" | origin2 == "Die Welt" |
                                origin2 == "Die Zeit" | origin2 == "FAZ") 


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


corpus_df <- corpus_de08

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

txdf <- convert(corpus_df, to = "data.frame")

# lemmatization de

zombiefirm_pattern_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,1]
zombiefirm_pattern_de <- paste0("\\b",zombiefirm_pattern_de,"\\b")
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,2]
# zombiefirm_replace <- paste0("\\b",zombiefirm_replace,"\\b",collapse="|")
names(zombiefirm_replace_de) <- zombiefirm_pattern_de
txde$text <- str_replace_all(txde$text,regex(zombiefirm_replace_de,ignore_case = T))

corpus_df <- corpus(txde)



















## check doubles and useless
# 
dfm_sim <- dfm(tokens(corpus_de08))
# dfm_sim <- dfm_subset(dfm_sim, origin2 == "FAZ")
dfm_it20 <- dfm(tokens(corpus_subset(corpus_it08,datet >= "2020-01-01")))
corpusit20 <- corpus_subset(corpus_it08,datet >= "2020-01-01")

sim <- textstat_simil(dfm_sim,
                         method="jaccard",margin="documents", min_simil = 0.70)

# take off diagonal: same item compared to itself
simlist <- as.list(sim,diag=FALSE)

a <- textstat_simil(dfm_sim, dfm_sim["SZ_SDDZ000020170714ed7e0001x",], margin = "documents", method = "jaccard")

## correspondence analysis ####
# dfm_it08 <- dfm(tokens(corpus_it08))

ca <- textmodel_ca(dfm_sim)

# one dimension
tca <- textplot_scale1d(ca, margin = "documents",  groups = docvars(corpus_de08, "origin2"))
plotly::ggplotly(tca)
#ggsave(paste0("ca_scale1","_",store_df,".jpg"),path = store_df)

# two dimensions, dataframe and plot

dat_ca <- data.frame(nm = ca$rownames,
                     dim1 = coef(ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(ca, doc_dim = 2)$coef_document,
                     origin2 = corpus_it08$origin2,
                     rating = corpus_it08$rating)

ggplot(dat_ca, aes(x = dim1,y=dim2, color = rating)) + geom_point() +  
  geom_text(label = dat_ca$nm, hjust=0.5, vjust=0,show.legend = FALSE) +
  labs(x = "",y="")+
  ggtitle(dfm_it08$country) +
  scale_color_manual(values = c("left" = "red","right" = "blue","center" = "green")) +
  guides(color=guide_legend(title="Source")) + theme_bw() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 


## length
corpus_de08$ntok <- ntoken(corpus_de08)
txde <- quanteda::convert(corpus_de08,to = "data.frame")

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}




txde %>% group_by(origin2) %>% mutate(outlier = if_else(is_outlier(ntok), doc_id, NA_character_)) %>% 
  ggplot(aes(x = origin2, y = ntok, color = origin2))  + geom_boxplot() + # geom_point() +
  geom_text_repel(aes(label = outlier), na.rm = TRUE) +
  stat_summary(fun=mean, geom="point",shape = 20,color="red") +
  ylab("Count tokens") +
  xlab("Articles") + 
  labs(color = "Source") +
  theme_bw() +
  theme(axis.text.x = element_blank())

# length total corpus


t <- txde %>% mutate(outlier = if_else(is_outlier(ntok), doc_id, NA_character_)) %>% 
  ggplot(aes( y = ntok))  + geom_boxplot() + 
  #  geom_text(aes(label = outlier), na.rm = TRUE) +
  ylab("Number Tokens") +
  ggtitle("Italy") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# ggsave(paste0("IT_","boxpplot length documents.jpg"), width = 12, height = 12)

plotly::ggplotly(t)










# it







corpus_it08 <- corpus_subset(corpus_it08, !docnames( corpus_it08) %in% c( 
  "GN_GIONLE0020130426e94q00082","CS_CORDES0020171218edci0000g",  # double similar
    "CS_CORVEN0020210313eh3d00002","RP_REPONL0020191104efb4000ma","RP_REPONL0020211024ehao0028w",  # outliers length
   "CS_CORVEN0020210209eh290000k",
# out of context
"CS_CORDES0020141219eacj00063", # pointless (actor Gwyne Paltrow)
"CS_CORONL0020200706eg76000en", # university start-up
"FQ_FATONL0020210212eh2c0008w" # about Berlusconi, mention like joke (Berlusconi zombie company)
))

txit <- convert(corpus_it08, to = "data.frame")


txit$cvid <- str_detect(txit$text, regex("covid*|corona*|coronavirus|corona-virus|virus|sars-cov-2", ignore_case = TRUE))

a <- txit %>% filter(datet >= "2020-01-01" & cvid == TRUE)






# a <- data.frame(ntoken(corpus_it08))
# txit <- cbind(txit,a)
# rm(a)
# txit <- rename(txit, ntok = ntoken.corpus_it08.)

zombiefirm_pattern <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,1]
zombiefirm_pattern <- paste0("\\b",zombiefirm_pattern,"\\b")
zombiefirm_replace <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,4]
# zombiefirm_replace <- paste0("\\b",zombiefirm_replace,"\\b",collapse="|")
names(zombiefirm_replace) <- zombiefirm_pattern
 txit$text <- str_replace_all(txit$text,regex(zombiefirm_replace,ignore_case = T))
# txit$text <- stri_replace_all_fixed(txit$text, zombiefirm_pattern,  zombiefirm_replace, vectorize_all=FALSE)

corpus_it <- corpus(txit)

rem_it <- c("fra","già?","oltre","ieri","può","soprattutto","molto","grandi","meno","tutto","tutti",
            "c'é","c'e","state","essere","percentuale","imprese","solo","parte","ne","cosa","fare",
            "c’è","c ’è","c'è","c 'è","d", "avere","milano","torino","bologna","napoli","pisa","quindi",
            "volta","veneto", "termini","fatto quotidiano","la repubblica","corriere della sera", "il giornale",
            "il corrier","corriere","il fatto","https*","fatto_quotidiano","fatto_quotidiano","fatto","poi", "mai","prima",
            "cento","gran_parte","gran* parte",
            "anni","soltanto","imprese_intervistate","imprese intervistate","d accordo","d_accordo","circa","però"
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
                        "sistema economico","sistemi economici","sistema politico","sistemi politici")


rem_de <- c("die welt","faz.net","die zeit","suddeutsche zeitung","handelsblatt","spiegel","f.a.z.","f.a.z",
  "faz","f.az","f a z","fa.z","1","2","3","4","5","6","7","welt","darf","schließlich", "000",
  "immer", "unternehmen","firmen","trotzdem" , "nämlich" ,  "nennt","zweiten","besser","frankfurt-hahn",
  "immerhin",
  "schwer","rund","wegen","denen","sz","WELT AM SONNTAG",
  "leben_erhalten_schrieb_michael_hüther_direktor","deutschland","später",
  "dass","zahl","prozent","viele","mehr","schon","sei","gibt","sagt","sagte","dabei","menschen","seien","diesmal",
  "sitzen","darin","geht","seit","sogar","tun","gewaltigen_anzahl_sogenannter_zombieunternehmen_führen_angesichts",
  "komme", "kommst", "kommt", "kommen",
  "muss","musst","muss","müssen","müsst","Warum",
  "soll","sollst","sollt","sollen", "sollt", "sollen",
  "laut","jahr","ende","etwa","etwas","moglich", "allerdings","uhr","ezb","ab",
  "kann","kannst","kann", "können", "könnt", "könnte","könnten","könne","fast",
  "gut", "zudem", "eigentlich" , "weitere", "bisher", "weniger","iw","gar","hoch",
  "allerdings",  "möglich","dafür", "wäre" ,"gerade" ,"jahren", "ja", "bereits", "derzeit","blick", "rheinland-pfalz",
  "anteil","daher","viertagewoche","sagen","sagt","anfang","gehen","gab","gab es","hochbahn","benex","tepco", "zugbegleiter",
  "bahn",
  "millionen_unternehmen_entspricht","passiert","lange","erst","macht","wären","hälfte",
  "quelle"
)


compound_de <- c("europäische union","zombie firma","zombie unternehmen","zombie firmen",
                        "deutschen banken", "jens ehrhardt",	
                        "carsten dierig","paschal donohoe",	
                        "lucas flöther","alexander herzog","flüchtlingswelle 2015","millionen kurzarbeiter",
                        "ifo geschäftsklimaindex","insolvenzen abgewendet","jörg hofmann","mark schieritz",
                        "stefan bratzel",	"isabel schnabel","jan roth","corona hilfen",		
                        "paul ziemiak","insolvenz tsunami","euro gruppe", "euro zone","billige geld", 	
                        "sieben tage", "erreicht worden","corona-hilfen","corona-hilfe")












# keyness

dfm_keyness <- dfm_subset(dfm_df, datet >= "2020-01-01" )

textplot_keyness(textstat_keyness(dfm_group(dfm_keyness,groups = rating),
                                  target = "right"), n = 20, margin = 0.05,
                 labelsize = 8) + 
  # ggtitle(paste0(unique(dfm_keyness$country),", ",unique(dfm_keyness$covidtp))) +
  ylab("") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.text = element_text(size=15))


# co-occurrence
tk_df <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "right"),
                remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de))

# fc_d <- fcm(dfm_df)

fc_d <- fcm(tk_df, context = "window", window = 20)
fc_dtop <- topfeatures(fc_d, 15)
feat <- names(fc_dtop)
fc_dt <- fcm_select(fc_d, pattern = feat)

co_occur_network <- graph_from_adjacency_matrix(fc_dt, mode = "undirected", diag = FALSE)
E(co_occur_network)$weight <- count.multiple(co_occur_network)

# co_occur_network <- simplify(co_occur_network, edge.attr.comb=list(weight="sum"))
co_occur_network <- simplify(co_occur_network)
# V(co_occur_network)$color <- ifelse(V(co_occur_network)$name %in% zombiefirms, 'cornflowerblue', 'orange')


# png(paste0("new_",unique(tk_df$country), unique(tk_df$rating),".png"))
#par(mar = c(1, 0.7, 1, 0.7)) 
plot(co_occur_network,
     #   main = paste(unique(tk_df$country),",", unique(tk_df$rating),",", unique(tk_df$covidtp),":", "top", "15", "co-occurrence"),
     vertex.size = eigen_centrality(co_occur_network)$vector * 15,
     vertex.shape = "circle",
     vertex.label = V(co_occur_network)$name,
     vertex.label.color = "black",
     vertex.label.cex = 1.5,
     vertex.color = "grey",
     edge.width = (E(co_occur_network)$weight / 4),
     # edge.label = E(co_occur_network)$weight,
     # edge.color = 6 * E(co_occur_network)$weight,
     #  vertex.label.dist = 1,
     # vetex.label.degree = 0,
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network)$name == "zombiefirms",2,1),
     vertex.label.dist = 1.8
     # edge.curved=.2
     #  vertex.label.dist=1
)

### new ####




# a <- data.frame(ntoken(corpus_it08))
# txit <- cbind(txit,a)
# rm(a)
# txit <- rename(txit, ntok = ntoken.corpus_it08.)
# txit$cvid <- str_detect(txit$text, regex("covid*|corona*|coronavirus|corona-virus|virus|sars-cov-2", ignore_case = TRUE))

# zombiefirm_pattern <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,1]
# zombiefirm_replace <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,2]
# 
# zombiefirm_pattern <- c("azienda zombie","aziende zombie",
#                         "azienda-zombie","aziende-zombie",
#                         "impresa zombie","imprese zombie",
#                         "impresa-zombie","imprese-zombie",
#                         "organizzazione zombie","organizzazioni zombie",
#                         "organizzazione-zombie","organizzazioni-zombie",
#                         "società zombie","società-zombie",
#                         "zombie-company","zombi-company",
#                         "azienda zombi","aziende zombi",
#                         "azienda-zombi","aziende-zombi",
#                         "impresa zombi","imprese zombi",
#                         "impresa-zombi","imprese-zombi",
#                         "organizzazione zombi","organizzazioni zombi",
#                         "organizzazione-zombi","organizzazioni-zombi",
#                         "società zombi","società-zombi")
# zombiefirm_replace <- c("zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms", "zombiefirms",
#                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms",
#                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms",
#                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms",
#                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms")
# names(zombiefirm_replace) <- zombiefirm_pattern
# 
# txit$text <- str_replace_all(txit$text,regex(zombiefirm_replace,ignore_case = T))
# 
# corpus_it08 <- corpus(txit)

# corpus_it <- corpus(txit)





# 
# 
# rem_it <- c("fra","già?","oltre","ieri","può","soprattutto","molto","grandi","meno","tutto","tutti",
#             "c'é","c'e","state","essere","percentuale","imprese","solo","parte","ne","cosa","fare",
#             "c’è","c ’è","c'è","c 'è","d", "avere","milano","torino","bologna","napoli","pisa","quindi",
#             "volta","veneto", "termini","fatto quotidiano","la repubblica","corriere della sera", "il giornale",
#             "il corrier","corriere","il fatto","https*","fatto_quotidiano","fatto_quotidiano","fatto","poi", "mai","prima",
#             "cento","gran_parte","gran* parte",
#             "anni","soltanto","imprese_intervistate","imprese intervistate","d accordo","d_accordo","circa","però"
# ) 
# 
# compound_it <- c(zombiefirm_pattern,
#   "cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
#                         "unione europea","salva italia","decreto ristori","decreti ristoro",
#                         "lotto di vaccini","istituto di credito","instituti di credito",
#                         "piano nazionale","sistema produttivo","sistemi produttivi",
#                         "rapporto finanziario","made in italy","tassi di interesse", "tasso di interesse",
#                         "tassi d'interesse","tasso d'interesse","interessi economici","interesse economico",
#                         "ambito finanziario","termini di resa","termini di interesse","ambito lavorativo",
#                         "camera di commercio","ministero dello sviluppo economico","rapport eu",
#                         "stati finanziari","stati di bilancio","sviluppo economico",
#                         "Ferretti Group","Gruppo Ferretti","Ferretti Yachts",
#                         "blocco dei licenziamenti","blocco licenziamenti", "blocco_dei_licenziamenti",
#                         "blocco_licenziamenti",
#                         "sistema economico","sistemi economici","sistema politico","sistemi politici",bg,trg)

# compound it
# itx <- c( "Il decreto ristori porta alle imprese zombie e la Banca d'Italia",
#            "A marzo il sistema economico. Per il bene dell'economia e si salva l'economia. Anche se ancora c’è")

# dfm and dfm grouped

# corpus_it <- corpus(txit)
# corpus_it <- corpus_subset(corpus_it,datet >= "2020-01-01")

dfm_df <-  tokens( corpus_it,
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

zombiefirm_replacestat <- gsub(" ","_",zombiefirm_replace)
freq_stat <- textstat_frequency(dfm_df) %>% 
  subset(feature %in% c(zombiefirm_replace,"zombiefirms","aziende_zombie","imprese_zombie","zombie_company","società_zombie","organizzazione_zombie"))

# Germany


dfm_df <-  tokens( corpus_df,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de)) %>% 
  dfm()

zombiefirm_replacestat <- gsub(" ","_",zombiefirm_replace)
freq_stat <- textstat_frequency(dfm_df) %>% 
  subset(feature %in% c(unique(zombiefirm_replace_de)))




# co-occurrence ####
# 
# tk_df <- tokens(corpus_subset(corpus_it,datet >= "2020-01-01" & rating == "left"),
#                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
#                 remove_numbers = TRUE,remove_url = FALSE) %>%
#   tokens_tolower() %>% 
#   tokens_compound(compound_it) %>%
#   tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it))
# 
# fc_d <- fcm(tk_df, context = "window", window = 20)
# fc_dtop <- topfeatures(fc_d, 15)
# feat <- names(fc_dtop)
# fc_dt <- fcm_select(fc_d, pattern = feat)

# Old
# co_occur_network <- graph_from_adjacency_matrix(fc_dt, mode = "undirected", diag = FALSE)
# eigen_centrality(co_occur_network)$vector
# # E(co_occur_network)$weight <- edge.betweenness(co_occur_network)
# #  <-closeness(co_occur_network)
# V(co_occur_network)$color <- ifelse(V(co_occur_network)$name %in% zombiefirms, 'cornflowerblue', 'orange')
# plot(co_occur_network,
#      main = paste(unique(tk_df$country),",", unique(tk_df$rating),",", unique(tk_df$covidtp),":", "top", "15", "co-occurrence"),
#      vertex.size = eigen_centrality(co_occur_network)$vector * 15,
#      vertex.shape = "square",
#      vertex.label = V(co_occur_network)$name,
#      edge.width = E(co_occur_network)$weight
# )

# new
# co_occur_network <- graph_from_adjacency_matrix(fc_dt, mode = "undirected", diag = FALSE)
# E(co_occur_network)$weight <- count.multiple(co_occur_network)
# 
# # co_occur_network <- simplify(co_occur_network, edge.attr.comb=list(weight="sum"))
# co_occur_network <- simplify(co_occur_network)
# # V(co_occur_network)$color <- ifelse(V(co_occur_network)$name %in% zombiefirms, 'cornflowerblue', 'orange')
# 
# 
# # png(paste0("new_",unique(tk_df$country), unique(tk_df$rating),".png"))
# #par(mar = c(1, 0.7, 1, 0.7)) 
# plot(co_occur_network,
#      #   main = paste(unique(tk_df$country),",", unique(tk_df$rating),",", unique(tk_df$covidtp),":", "top", "15", "co-occurrence"),
#      vertex.size = eigen_centrality(co_occur_network)$vector * 15,
#      vertex.shape = "circle",
#      vertex.label = V(co_occur_network)$name,
#      vertex.label.color = "black",
#      vertex.label.cex = 1.5,
#      vertex.color = "grey",
#      edge.width = (E(co_occur_network)$weight / 4),
#      # edge.label = E(co_occur_network)$weight,
#      # edge.color = 6 * E(co_occur_network)$weight,
#      #  vertex.label.dist = 1,
#      # vetex.label.degree = 0,
#      layout=layout.circle,
#      vertex.label.font = ifelse(V(co_occur_network)$name == "zombiefirms",2,1),
#      vertex.label.dist = 1.8
#      # edge.curved=.2
#      #  vertex.label.dist=1
# )
# dev.off()











# coconut ####

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

dfm_dfcooc <- dfm_subset(dfm_df, datet >= "2020-01-01" & rating == "right")

coocCounts <- t(dfm_dfcooc) %*% dfm_dfcooc

coocTerm <- "zombie_unternehmen"

k <- nrow(dfm_dfcooc)
ki <- sum(dfm_dfcooc[, coocTerm])
kj <- colSums(dfm_dfcooc)
names(kj) <- colnames(dfm_dfcooc)
kij <- coocCounts[coocTerm, ]

# as.matrix(coocCounts[202:205, 202:205])

#  function

coocs <- calculateCoocStatistics(coocTerm, dfm_dfcooc, measure="LOGLIK")
# print(coocs[1:numberOfCoocs])

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
graphVs <- V(graphNetwork)[degree(graphNetwork) < 1] # Here for more or less clusters !!!!
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

# All nodes must be assigned a standard minimum-size
V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 

# E(graphNetwork)$width <- 2

# Define the frame and spacing for the plot
#par(mai=c(0,0,1,0)) 


# test Finalizer
s <- t
# png(paste0("images/",unique(dfm_dfcooc$rating),"_",s,"_",unique(dfm_dfcooc$country), ".png"))
# par(mar = c(1,1,1,1)) 
plot(graphNetwork,              
     layout = layout.fruchterman.reingold,  # Force Directed Layout 
     main = paste(unique(dfm_dfcooc$country),",", unique(dfm_dfcooc$rating),",", unique(dfm_dfcooc$covidtp),":", coocTerm ),
     vertex.label.family = "sans",
     vertex.label.cex = 1.5,
     vertex.shape = ifelse(V(graphNetwork)$name == coocTerm, 'square', 'circle'),
     vertex.label.dist = 0.5,           # Labels of the nodes moved slightly
     vertex.frame.color = 'darkolivegreen',
     vertex.label.color = 'black',      # Color of node names
     vertex.label.font = ifelse(V(graphNetwork)$name == coocTerm, 2, 1),         # Font of node names
     vertex.label = V(graphNetwork)$name,       # node names
     #vertex.label.cex = 1, # font size of node names 
     edge.width = ifelse(E(graphNetwork)$sig > halfMaxSig, 4, 1),
     vertex.color = "grey"
)
# dev.off()





# pl_i("aziende_zombie","left")


#####



library(widyr)
#create context window with length 8
tidy_skipgrams <- txit[txit$rating == "left",] %>%
  filter(!text %in% rem_it ) %>%
  filter(!text %in% stopwords_it) %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 15) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, doc_id, ngramID) %>%
  unnest_tokens(word, ngram)

unigram_probs <- txit[txit$rating == "left",] %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs <- tidy_skipgrams %>%
  filter(!word %in% rem_it ) %>%
  filter(!word %in% stopwords_it) %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

normalized_prob %>% 
  filter(word1 == "zombiefirms") %>%
  arrange(-p_together)



