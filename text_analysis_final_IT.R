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

## ITALY PROCESSING #####
setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/zombie_firms/zombie_firms/upload_codes/")
load("corpus_it08.Rdata")
#load("corpus_it.Rdata")
#corpus_it08 <- corpus_it
#load("corpus_it08.Rdata")
#corpus_it08 <- (corpus_it + corpus_it08)
bg <- pull(read.csv("it_bigrams_08.csv"),2)
trg <- pull(read.csv("it_trigrams_08.csv"),2)
corpus_it08$country <- "Italy"
corpus_it08$covidtp[corpus_it08$datet < "2020-01-01"] <- "before 2020-01-01"
corpus_it08$covidtp[corpus_it08$datet >= "2020-01-01"] <- "after 2020-01-01"
# corpus_de$rating[corpus_de$origin2 == "Handelsblatt"] <- "center"
# corpus_de$rating[corpus_de$origin2 == "Spiegel"] <- "center"
# 
# save(corpus_de,file = "corpus_de.Rdata")


corpus_it08$abbrev[corpus_it08$origin2 == "La Repubblica"] <- "RP"
corpus_it08$abbrev[corpus_it08$origin2 == "Corriere Sera"] <- "CS"
corpus_it08$abbrev[corpus_it08$origin2 == "Il Giornale"] <- "GN"
corpus_it08$abbrev[corpus_it08$origin2 == "Sole 24 ore"] <- "S24"
corpus_it08$abbrev[corpus_it08$origin2 == "Fatto Quotidiano"] <- "FQ"


docnames(corpus_it08) <- paste(corpus_it08$abbrev, corpus_it08$id, sep="_")   #  1:ndoc(corpus_de08), sep="_")

# docnames(corpus_de)


corpus_it08 <-  corpus_subset(corpus_it08, origin2 == "La Repubblica" | origin2 == "Corriere Sera" |
                              origin2 == "Il Giornale" | origin2 == "Fatto Quotidiano") 


corpus_it08 <- corpus_subset(corpus_it08, !docnames( corpus_it08) %in% c( 
  "GN_GIONLE0020130421e94l00012","CS_CORONL0020171217edch00131","CS_CORONL0020200706eg76000en", # double similar
  "CS_CORVEN0020210313eh3d00002","RP_REPONL0020191104efb4000ma","RP_REPONL0020211024ehao0028w",    # outliers
  "CS_CORVEN0020210209eh290000k","CS_CORDES0020210201eh210007q" 
                                                                                  
))


corpus_it08 <- gsub("'", " ",  corpus_it08) 
corpus_it08 <- gsub("’", " ",  corpus_it08)
# save(file="corpus_it08.Rdata")



## Check similarity within texts (double-like)
# textstat_simil(dfm(corpus_subset(corpus_it08,id == txit[txit$doc_id=="XXXX",]$id| id == txit[txit$doc_id=="XXXXX",]$id)),
# method = "cosine", margin = "documents")
# #
# ts <- textstat_simil(dfm(corpus_subset(corpus_it08, origin2 == "FAZ")), method = "cosine", margin = "documents")
# ts <- textstat_simil(dfm(corpus_subset(corpus_it08, origin2 == "Handelsblatt")), method = "cosine", margin = "documents")
# 
# # to check texts with diffobj
# # diffobj::diffPrint(txit[txit$doc_id=="HB_66",]$text,txit[txit$doc_id=="HB_69",]$text )
# 
# ts <- textstat_simil(dfm(tokens(corpus_it08)), method = "cosine", margin = "documents")
# #a <- ts[ts[,"cosine"] >= 0.950,]
# a <- data.frame(ts)
# at <- a[a[,"cosine"] >= 0.950,]
# at <- at[order(at$document1),]
# 
# # diffobj: https://cran.r-project.org/web/packages/diffobj/vignettes/diffobj.html
# 
# t1 <- "CS_CORSUP0020200706eg760000o"
# t2 <- "CS_CORONL0020200706eg76000en"
# fl <- "CS/"
# txit[txit$doc_id==t1,]$text %>% write.csv(file=paste0(fl,as.character(t1),".csv"))
# txit[txit$doc_id==t2,]$text %>% write.csv(file=paste0(fl,as.character(t2),".csv"))
# diffCsv(paste0(fl,as.character(t1),".csv"),paste0(fl,as.character(t2),".csv"))



# Bigrams and trigrams

 # library(dplyr)
 # library(tidytext)

# bigrams_txit <- txit %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
# bigrams_txit %>% dplyr::count(bigram, sort = TRUE)
# bigrams_separate <- bigrams_txit %>% separate(bigram,c("word1","word2"),sep=" ")
# bigrams_filtered <- bigrams_separate %>%
#   filter(!word1 %in% stopwords_it) %>%
#   filter(!word2 %in% stopwords_it)
# bigrams_filtered %>%  dplyr::count(word1, word2, sort = TRUE)
# bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
# bigrams_united <- bigrams_united$bigram
# 
#write.csv(bigrams_united,"it_bigrams_08.csv")
# 
#  trigrams_txit <- txit %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
#  trigrams_txit %>% dplyr::count(trigram, sort = TRUE)
#  trigrams_separate <- trigrams_txit %>% separate(trigram,c("word1","word2","word3"),sep=" ")
#  trigrams_filtered <- trigrams_separate %>%
#    filter(!word1 %in% stopwords_it) %>%
#    filter(!word2 %in% stopwords_it) %>%
#    filter(!word3 %in% stopwords_it)
#  trigrams_filtered %>%  dplyr::count(word1, word2,word3, sort = TRUE)
#  trigrams_united <- trigrams_filtered %>% unite(trigram, word1, word2,word3, sep = " ")
#  trigrams_united <- trigrams_united$trigram
# 
# write.csv(trigrams_united,"it_trigrams_08.csv")

 
 txit <- convert(corpus_it08, to = "data.frame")
 # a <- data.frame(ntoken(corpus_it08))
 # txit <- cbind(txit,a)
 # rm(a)
 # txit <- rename(txit, ntok = ntoken.corpus_it08.)
 # txit$cvid <- str_detect(txit$text, regex("covid*|corona*|coronavirus|corona-virus|virus|sars-cov-2", ignore_case = TRUE))

 
 zombiefirm_pattern <- c("azienda zombie","aziende zombie",
                         "azienda-zombie","aziende-zombie",
                         "impresa zombie","imprese zombie",
                         "impresa-zombie","imprese-zombie",
                         "organizzazione zombie","organizzazioni zombie",
                         "organizzazione-zombie","organizzazioni-zombie",
                         "società zombie","società-zombie",
                         "zombie-company","zombi-company",
                         "azienda zombi","aziende zombi",
                         "azienda-zombi","aziende-zombi",
                         "impresa zombi","imprese zombi",
                         "impresa-zombi","imprese-zombi",
                         "organizzazione zombi","organizzazioni zombi",
                         "organizzazione-zombi","organizzazioni-zombi",
                         "società zombi","società-zombi")
 zombiefirm_replace <- c("zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms", "zombiefirms",
                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms",
                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms",
                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms",
                         "zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms","zombiefirms")
 names(zombiefirm_replace) <- zombiefirm_pattern
 
 txit$text <- str_replace_all(txit$text,regex(zombiefirm_replace,ignore_case = T))
 
 corpus_it08 <- corpus(txit)
 
 df_word <- unique(stringr::str_extract_all(txit$text, 
 regex("\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]*zombi\\-?[:alnum:]*\\-?[:alnum:]*", 
                                                  ignore_case = TRUE)))
 
 a <- unique(unlist(df_word))
 a
 
 
 
 
## Bigrams and trigrams
 # bigrams_txit <- txit %>%
 #   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% count(bigram, sort = TRUE) %>%
 #   separate(bigram,c("word1","word2"),sep=" ")%>%
 #   filter(!word1 %in% stopwords_it) %>%
 #   filter(!word2 %in% stopwords_it)%>%
 #   unite(bigram, word1, word2, sep = " ")
#  
 #write.csv(bigrams_txit,"it_bigrams.csv")
#  
# trigrams_tx <- tx %>%
#    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% count(trigram, sort = TRUE) %>%
#    separate(trigram,c("word1","word2","word3"),sep=" ")%>%
#    filter(!word1 %in% stopwords_it) %>%
#    filter(!word2 %in% stopwords_it)%>%
#    filter(!word3 %in% stopwords_it)%>%
#    unite(trigram, word1, word2,word3, sep = " ")
# write.csv(trigrams_tx,"it_trigrams.csv")
 
# trigram_tx <- tx %>%
#    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% count(trigram, sort = TRUE)
# 
# tgr <- tx %>% unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
#   filter(!word1 %in% stop_words$word,
#          !word2 %in% stop_words$word,
#          !word3 %in% stop_words$word)
 
 

 rem_it <- c("fra","già?","oltre","ieri","può","soprattutto","molto","grandi","meno","tutto","tutti",
             "c'é","c'e","state","essere","percentuale","imprese","solo","parte","ne","cosa","fare",
             "c’è","c ’è","c'è","c 'è","d", "avere","milano","torino","bologna","napoli","pisa","quindi",
             "volta","veneto", "termini","fatto quotidiano","la repubblica","corriere della sera", "il giornale",
              "il corrier","corriere","il fatto","https*","fatto_quotidiano","fatto_quotidiano","fatto","poi", "mai","prima",
             "cento","gran_parte","gran* parte",
             "anni","soltanto","imprese_intervistate","imprese intervistate","d accordo","d_accordo","circa","però"
             ) 
 
 compound_it <- phrase(c("cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
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
                         "sistema economico","sistemi economici","sistema politico","sistemi politici",bg,trg))

# compound it
# itx <- c( "Il decreto ristori porta alle imprese zombie e la Banca d'Italia",
#            "A marzo il sistema economico. Per il bene dell'economia e si salva l'economia. Anche se ancora c’è")

 # dfm and dfm grouped
 
 dfm_df <-  tokens( corpus(corpus_it08),
                    remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                    remove_numbers = TRUE,remove_url = FALSE) %>%
   tokens_tolower() %>% 
   tokens_compound(phrase(c(compound_it))) %>%
   tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>% 
   dfm()

 textstat_frequency(dfm_df) %>% subset(feature %in% "zombiefirms")
 
# to test bigram and trigram
# "Carl ist in Europäische Union aber Christine Lambrecht SPD spricht mit zombie unternehmen 
# und angela merkel und angela und merkel.
#        Aber zombie unternehmen sind tot."



## Descriptives ####

# Length

# boxplot outliers length (number tokens) for each source
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

txit %>% group_by(origin2) %>% mutate(outlier = if_else(is_outlier(ntok), doc_id, NA_character_)) %>% 
  ggplot(aes(x = origin2, y = ntok, color = origin2))  + geom_boxplot() + # geom_point() +
  geom_text_repel(aes(label = outlier), na.rm = TRUE) +
  stat_summary(fun=mean, geom="point",shape = 20,color="red") +
  ylab("Count tokens") +
  xlab("Articles") + 
  labs(color = "Source") +
  theme_bw() +
  theme(axis.text.x = element_blank())

# length total corpus


t <- txit %>% mutate(outlier = if_else(is_outlier(ntok), doc_id, NA_character_)) %>% 
  ggplot(aes(y = ntok))  + geom_boxplot() + 
  ylab("Number Tokens") +
  ggtitle("Italy") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0("IT_","boxpplot length documents.jpg"), width = 12, height = 12)

plotly::ggplotly(t)



# Length count tokens
tk <- txit %>% ggplot(aes(x = doc_id, y = ntok, color = origin2)) + geom_point() +
  ylab("Count tokens") +
  xlab("Articles") + 
  labs(color = "Source") +
  theme_bw() +
  theme(axis.text.x = element_blank())

plotly::ggplotly(tk)

# distribution of sample
ggplot(txit,aes(origin2, fill = rating)) + geom_bar() +
  scale_fill_manual(values = c("left" = "red","right"= "blue")) +
  xlab("Source") +
  geom_text(stat='count', aes(label=..count..), hjust= 1) +
  facet_wrap(~ factor(covidtp, levels = c("before 2020-01-01","after 2020-01-01"))) +
  ggtitle(txit$country) +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# distribution time
txit_long <-  gather(txit, datetlong, outcome, origin2:rating)
txit$ordid <- txit[order(txit$doc_id, txit$datet), ]
txita <- txit %>% dplyr::arrange(doc_id,datet)

p <- txit %>% dplyr::arrange(doc_id,datet) %>% ggplot(aes(x = datet, y = doc_id)) + geom_point(aes(color = cvid)) +
  # facet_wrap(~ origin2) +
  # scale_fill_manual(values = c("center" = "green","left" = "red","right"= "blue")) +
  facet_wrap(~ rating + origin2) + 
  scale_color_manual(values = c("FALSE" = "dark green", "TRUE" = "orange"), name = "Mention Covid") +
#  geom_vline(xintercept = "2020-01-01", color = "red") +
  ggtitle(txit$country) +
  xlab("Year") +
  ylab("Texts") +
  #  geom_text(stat='count') +
 # theme_bw() +
  theme_bw() +
  theme(axis.ticks.y  = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  
s <- load("images/de_time.png")









# Percentage

tz <- data.frame(right = ((nrow(txit[which(txit$rating == "right"),]) / nrow(txit)) * 100),
                 left =  ((nrow(txit[which(txit$rating == "left"),]) / nrow(txit)) * 100))

ggplot(txit,aes(x = "", fill = rating)) + geom_bar() + coord_polar(theta = "y") + 
  theme_void()  

txita <- txit %>% 
  group_by(rating) %>% # Variable to be transformed
  add_count() %>% 
  ungroup()   %>% 
  mutate(perct = n / nrow(txit))  %>%
  arrange(perct) %>%
  mutate(labels = scales::percent(perct)) %>% group_by(rating)


#  arrange(perc) %>%
# mutate(labels = scales::percent(perc)) %>%

# ggplot(txita,aes(x = "", y = perct, fill = rating)) +
# geom_col() +
# scale_fill_manual(values = c("right" = "blue","left" = "red")) +
# geom_text(aes(label = labels),
#           position = position_stack(vjust = 0.5)) +
# guides(fill = guide_legend(title = "Political Orientation")) +
# coord_polar(theta = "y") +
# theme_void()


ggplot(txita, aes(x = "", y = perct, fill = rating)) +
  geom_col() +
  geom_text(aes(label = unique(perct)),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("right" = "blue","left" = "red")) +
  guides(fill = guide_legend(title = "Political Orientation")) +
  coord_polar(theta = "y") + 
  theme_void()  

tz <- data.frame(right = ((nrow(txit[which(txit$rating == "right"),]) / nrow(txit)) * 100),
                 left =  ((nrow(txit[which(txit$rating == "left"),]) / nrow(txit)) * 100)) %>% 
  gather(perct, value, c(right,left), factor_key=TRUE) 

ggplot(tz,aes(x = "",y = value, fill = perct)) + geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") + 
 scale_fill_manual(values = c("left" = "red", "right" = "blue"),
                   labels = c("Left: 50,5%","Right: 49.5%") ,  name = "Rating") +
  theme_void()  

txit20 <- txit %>% filter(datet >= "2020-01-01")
cv <- data.frame(yes = ((nrow(txit20[which( txit20$cvid == TRUE),]) /   nrow(txit20)) * 100),
                 no =  ((nrow(txit20[which( txit20$cvid == FALSE),]) /   nrow(txit20)) * 100)) %>% 
  gather(perct, value, c(yes,no), factor_key=TRUE) 

ggplot(cv,aes(x = "",y = value, fill = perct)) + geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") + 
  scale_fill_manual(values = c("yes" = "orange", "no" = "dark green"),
                    labels = c("Yes: 68%","No: 32%") ,  name = "Citing Covid \nafter 2020") +
  theme_void()  


# 
# # Here reported calendar and source
# ggplot(txit,aes(x = doc_id, y = datetimestamp)) + geom_point() + facet_wrap(~ origin2, scales = "free_y") +
#   labs(x = "Documents", y = "Time") +
#   coord_flip() + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# ggsave(paste0("calendar","_",store_df,".jpg"),path = store_df)
# 
# 
# # Absolute Frequency
# 
# features_dfm <- textstat_frequency(dfm_df, n = 30, groups = corpus_df$origin2)
# features_dfm $feature <- with(features_dfm, reorder(feature, -frequency))
# 
# ggplot(features_dfm, aes(x = feature, y = frequency)) +
#   geom_point() + 
#   ggtitle(dfm_df$country)  + coord_flip() +
#   facet_wrap(~ group, scales = "free") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))

# Relative Frequency
# 
# 
# dfm_rel_freq <- dfm_weight(dfm_subset(dfm_df,datet < "2020-01-01"), scheme = "prop") 
# rel_freq <- textstat_frequency(dfm_rel_freq,  n = 20, groups = dfm_rel_freq$origin2)
# freq <- textstat_frequency(dfm_subset(dfm_df,datet < "2020-01-01"),groups = dfm_subset(dfm_df,datet >= "2020-01-01")$origin2)
# 
# ggplot(rel_freq, aes(x = nrow(rel_freq):1, y = frequency)) +
#   geom_point() +
#   facet_wrap(~ group, scales = "free") +
#   coord_flip() +
#   ggtitle(dfm_df$country) +
#   scale_x_continuous(breaks = nrow(rel_freq):1,
#                      labels = rel_freq$feature) +
#   labs(x = NULL, y = "Relative frequency") +
#   theme_bw()
# ggsave("IT_relfreq.jpg",width = 8,height=8)


## frequency for specific word

# freq$feature <-SnowballC::wordStem(freq$feature, language = 'de')
# freq$feature <- freq$feature %>% str_replace("-", "")

# here to combine into one category words not combined with steem or deleted hifen
# deutsch <- c("deutsche","deutschland")
# freq <- freq %>% mutate(dict = case_when(freq$feature %in% deutsch ~ "deutsch",
#                                          TRUE ~ freq$feature))

# freq_word <- textstat_frequency(dfm_df, groups = freq$origin2)
# freq_word <- subset(freq, feature %in% "zombiefirm")  

# ggplot(freq_word, aes(x = group, y = frequency)) +
#  geom_point() + 
#  scale_y_continuous(limits = c(0, 1.0), breaks = c(seq(0, 1.0, 0.1))) +
#  xlab(NULL) + 
#  ylab("Relative frequency") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Text Analysis ###



### compute tf_idf in tidy ####

tf_idf_df  <- txit %>% unnest_tokens(word,text) %>% dplyr::count(origin2, word, sort = TRUE) %>% ungroup()
tf_idf_df_total <- tf_idf_df %>% group_by(origin2) %>% summarise(total = sum(n))
tf_idf_df <- left_join(tf_idf_df,tf_idf_df_total)
tf_idf_df <- tf_idf_df %>% bind_tf_idf(word, origin2,n)

tf_idf_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(origin2) %>%
  top_n(8) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = TRUE) +
#  scale_fill_manual(values = c("left" = "red","right" = "blue", "center" = "green")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~origin2, ncol = 4, scales = "free") +
  coord_flip() +
  theme(legend.position="bottom",legend.title=element_blank())

ggsave("tf_idf_de.jpg",height = 10, width = 12)



# Correspondence Analysis ####

ca <- textmodel_ca(dfm_df)

# one dimension
textplot_scale1d(ca, margin = "documents",  groups = docvars(corpus_df, "origin2")) 

ggsave(paste0("ca_scale1","_",store_df,".jpg"),path = store_df)

# two dimensions, dataframe and plot

dat_ca <- data.frame(nm = ca$rownames,
                      dim1 = coef(ca, doc_dim = 1)$coef_document, 
                      dim2 = coef(ca, doc_dim = 2)$coef_document,
                     origin2 = corpus_df$origin2,
                     rating = corpus_df$rating)

ggplot(dat_ca, aes(x = dim1,y=dim2, color = rating)) + geom_point() +  
  geom_text(label = dat_ca$nm, hjust=0.5, vjust=0,show.legend = FALSE) +
  labs(x = "",y="")+
  ggtitle(dfm_df$country) +
  scale_color_manual(values = c("left" = "red","right" = "blue","center" = "green")) +
  guides(color=guide_legend(title="Source")) + theme_bw() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 



# wordcloud ####

dfm_df %>% textplot_wordcloud(comparison = TRUE)


# relative frequency for each newspaper ####

# corpus(tk_df) %>% corpus     corpus_subset(corpus(tk_df, datet < "2020-01-01"))

dfm_rel_freq <- dfm_weight(dfm_subset(dfm_df, datet >= "2020-01-01"),scheme = "prop")
rel_freq <- textstat_frequency(dfm_rel_freq,  n = 8, groups = dfm_rel_freq$origin2)
freq <- textstat_frequency(dfm_df,groups = dfm_df$origin2)

ggplot(rel_freq, aes(x = nrow(rel_freq):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  ggtitle(paste0(as.character(dfm_rel_freq$country),", ", as.character(dfm_rel_freq$covidtp))) +
  scale_x_continuous(breaks = nrow(rel_freq):1,
                     labels = rel_freq$feature) +
  labs(x = NULL, y = "Relative frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("images/IT_relfreq_aft20.jpg",width = 7,height=3.5)


# keyness ####
setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/twitter_unsupervised/from_server/post_olpol/")
dfm_keyness <- dfm_subset(dfm_df, datet >= "2020-01-01" )

textplot_keyness(textstat_keyness(dfm_group(dfm_keyness,groups = rating),
                                  target = "right"), n = 10, margin = 0.05,
                 labelsize = 8) + 
 # ggtitle(paste0(unique(dfm_keyness$country),", ",unique(dfm_keyness$covidtp))) +
  ylab("") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.text = element_text(size=15))
ggsave("it_knen.jpg", width = 13, height = 7)


dfm_keyness <- dfm_subset(dfm_df, rating == "left")
textplot_keyness(textstat_keyness(dfm_group(dfm_keyness,groups = covidtp),
                                  target = "after 2020-01-01"), n = 20, margin = 0.1) + 
  theme(legend.position = "bottom") + 
  ggtitle(paste0(dfm_keyness$country, ", within comparison ", dfm_keyness$rating, " side"))
ggsave(paste0("images/key_",unique(dfm_keyness$country),"_",unique(dfm_keyness$rating),".jpg"), width = 9, height = 7)



# co-occurrence ####
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.kateto.net/wp-content/uploads/2016/01/NetSciX_2016_Workshop.pdf
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/http://www.statistik.tu-dortmund.de/useR-2008/slides/Csardi.pdf

tag_vsk <- dfm_select(dfm_df, pattern = "imprese_zombie")
topvsk <- names(topfeatures(tag_vsk, 10))
vsk_fcm <- fcm(dfm_df)
tag_fcm <- fcm(tag_vsk)
topgat_fcm <- fcm_select(tag_fcm, pattern = topvsk)
textplot_network(topgat_fcm)

topwrd <- fcm(dfm_df)
textplot_network(topwrd)


## From fcm ####


tk_df <- tokens(corpus_subset(corpus_it08,datet >= "2020-01-01" & rating == "left"),
                remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(compound_it) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it))

# fc_d <- fcm(dfm_df)

fc_d <- fcm(tk_df, context = "window", window = 20)
fc_dtop <- topfeatures(fc_d, 15)
feat <- names(fc_dtop)
fc_dt <- fcm_select(fc_d, pattern = feat)

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
# dev.off()



### Coconut NEW ####

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

dfm_dfcooc <- dfm_subset(dfm_df, datet >= "2020-01-01" & rating == "left")

coocCounts <- t(dfm_dfcooc) %*% dfm_dfcooc

coocTerm <- "zombiefirms"

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
graphVs <- V(graphNetwork)[degree(graphNetwork) < 3]
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
# png(paste0("cnt_NEW_",unique(dfm_dfcooc$country), unique(dfm_dfcooc$rating),".png"))
#par(mar = c(3,3,3,3)) 
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

#### lexical dispersion #####

docnames(corpus_it08) <- paste(corpus_it08$abbrev,  1:ndoc(corpus_it08), sep="_")

dic_zombis <- dictionary(list(
  zombiefirms = c("impresa zombie","imprese zombie","impresa-zombie","imprese-zombie","organizzazione zombie",
                  "organizzazioni zombie","organizzazione-zombie","organizzazioni-zombie","società zombie","società-zombie"),
  covid = c("corona","covid*","coronavirus","sars-cov-2")
  
)) 

ld <- kwic(tokens(corpus_subset(corpus_it08,datet >= "2020-01-01")), pattern = dic_zombis) %>%  textplot_xray()
ld + aes(color = keyword) + 
  scale_color_manual(values = c("zombiefirms" = "red","covid" = "blue")) +
  theme(legend.position = "none")


# distance between texts, dendrogram to visualize ####

tstat_dist <- textstat_dist(dfm_weight(dfm_df, scheme = "prop"))

pres_cluster <- hclust(as.dist(tstat_dist))
pres_cluster$labels <- lab_df

ggdendrogram(pres_cluster, rotate = TRUE, theme_dendro = FALSE) +
  labs(x = "Documents",y = "dist") 

ggsave(paste0("dendro","_",store_df,".jpg"),path = store_df)

# wordfish, scaling of words and texts ####

# wordfish identifies an "ideology", i.e. a continuum between extreme opposite texts built by the researcher,
# and makes a regression to estimate psi: most frequent words (fixed effect), beta (words belonging to which direction of ideology)
# theta: positioning of the document along the ideology

docnames(corpus_df) # to select the most distant texts detected from the dendrogram to compute wordfish

wf <- textmodel_wordfish(dfm_df, dir = c(2, 9)) # dir is the rownumber of the texts selected (with docnames)

# words scaling: distribution in the space frequency (psi) and scaling on ideology (beta)
textplot_scale1d(wf, margin = "features",      
                 highlighted = c("covid","kurzarbeit","unternehmen"),  # words you want to highlight
                 highlighted_color = "red")

ggsave(paste0("wordfish_words","_",store_df,".jpg"),path = store_df)

# documents scaling (theta)
textplot_scale1d(wf, groups = corpus_df$origin2)
ggsave(paste0("wordfish_texts","_",store_df,".jpg"),path = store_df)

# word-network ####
set.seed(2929)
newspaper <- "Suddeutsche Zeitung" # choose journal to report

df_fcm <- fcm(dfm_subset(dfm_df, origin2 == newspaper))

topfcm <- names(topfeatures(df_fcm, 20))
topfcm_fcm <- fcm_select(df_fcm, pattern = topfcm)
textplot_network(topfcm_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5)

ggsave(paste0(newspaper,"_",store_df,".jpg"),path = store_df)


# co-occurrence

dfc <- convert(fcm(dfm_df)[,"alitalia"], to = "data.frame")
dfc %>% filter(alitalia > 5) %>% ggplot(aes(x=doc_id,y=alitalia)) + geom_point() +
  xlab("") + ylab("Co-occurrence with Alitalia") +
  coord_flip() + theme_bw()

ggsave(paste0("IT_coccurrence.jpg"))

#### Topic Modelling, correct the formula each time ####
# convert to stm 

stm_df <- convert(dfm_df, to = "stm")


## Testing model k number suggested ####

# searchk
k_stm <- searchK(stm_df$documents, stm_df$vocab, K = c(4,5,6,7,8,9,10,11,15,20), prevalence = ~ rating, data = stm_df$meta)
k_stm
#save(k_stm,file="ktm_WTSZ.Rdata")
dev.off()
plot(k_stm)

# exclusivity * semantic coherence
selct_stm <- selectModel(stm_df$documents,stm_df$vocab,K = 5, prevalence = ~ rating, 
                         max.em.its = 75, data = stm_df$meta, runs = 20, seed = 122929)

selct_stm
plotModels(selct_stm,legend.position="bottomright")


# Run topic modelling ####

n_k <- 5 # number topics

stm_m <- stm(stm_df$documents,stm_df$vocab,K = n_k, prevalence = ~ rating ,data = stm_df$meta,
             max.em.its = 75,  init.type = "Spectral")

plot.STM(stm_m,n = 5, type = "summary", main = dfm_df$country)
labelTopics(stm_m)


## Estimate model for each topic ####

est_m <- estimateEffect(formula = 1:n_k  ~  rating * datenum , stm_m, meta = stm_df$meta, uncertainty = "Global")

# it will report summary of model (regression coefficient and p-values) for each topic in the console

for (i in 1:n_k) {
  a <- summary(est_m, topics=i)
  print(a)
  
}

## Plot topic modelling ####

## Words to topic ####

tidy(stm_m) %>%
  group_by(topic) %>% 
  top_n(10) %>%
  ungroup %>%
  mutate(term =  reorder(term, beta))  %>%  
  mutate(topiclab = paste0("Topic ",topic)) %>%
  mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
  ggplot(aes(term,beta, fill = topiclab)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic2, scales = "free") +
  coord_flip() +
  ylab("Probability words belonging to topic") +
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(paste0("topic_beta","_",store_df,".jpg"),path = store_df)


## Find thoughts ####

txit$textshort <- str_sub(txit$text, 1, 200) # shorten to first 200 characters
thought_tp <- 1 

thoughts_df <- findThoughts(stm_m, texts = txit$textshort, n = 2, thresh = 0.5,  topics = thought_tp)$docs[[1]]
plotQuote(thoughts_df, width = 30, main = paste0("Topic ", thought_tp))


## Document to topic ####

# dataframe built to have gamma probability (document belong to topic) and show what source (or other metadata) documents belong to
stm_gamm <- tidy(stm_m, matrix = "gamma", document_names = rownames(dfm_df)) 
stm_gamm <- cbind(stm_gamm,dfm_df$origin2,dfm_df$rating)

stm_gamm %>% 
  mutate(topiclab = paste0("Topic ",topic)) %>%
  mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
  ggplot(aes(gamma, fill = as.factor(`dfm_df$origin2`))) +
  geom_histogram(show.legend = T) +
  facet_wrap(~topic2, ncol = 3)  +
  xlab("Probability documents belonging to topic") +
  ylab("") +
  # ggtitle(store_df) +
  #  scale_fill_manual(values = c("left" = "red","right" = "blue", "center" = "green")) +
  #  labels = lab_df_rt) + 
  guides(fill=guide_legend(title="Source")) +  theme_bw() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) 

ggsave(paste0("topic_gamma","_",store_df,".jpg"),path = store_df)




## Plot Different topic scaling ####

n_t <- 3 # topic selected (or change in code)

# on x-axis value1 appears on right, value 2 on left of reader
value1 <- "SZ"       
value2 <- "Die Welt"

 plot(est_m, covariate = "rating", 
     topics = c(n_t), #topics to report
     model = stm_m, # stm
     method = "difference", # because binary
     cov.value1 = "left", cov.value2 = "right",
     xlab = paste0(value2, "  <----->  ", value1),
     labeltype = "custom", custom.labels = c(paste0("Topic ", n_t)), 
     main = store_df)


## Topical Content ####

 # nk same as stm model, they should be coherent

stm_cont <- stm(stm_df$documents,stm_df$vocab,K = n_k, prevalence = ~ rating*datenum, 
                content = ~ rating, # added for content modelling
                data = stm_df$meta, max.em.its = 75,  init.type = "Spectral")

# summary for each topic
for (i in 1:n_k) {
  a <- summary(stm_cont, topics=i)
  print(a)
  
}

# plot semantic perspectives (topical content)
plot(stm_cont, type = "perspectives", topics = n_t, main = store_df)


## Mediation effects (e.g. time) or else. For interactions regression must be with interaction term *

plot(est_m,"datenum",model = stm_m, method = "continuous",topics = n_t, printlegend = FALSE,
    moderator = "rating",moderator.value = "left",linecol = "red",
    #  ylim = c(-0.4, 0.9), 
    main = paste0(store_df,"Topic ", n_t, ": time * political rating"),
    xaxt ="n",
    xlab ="Time")
plot(est_m,"datenum",model = stm_m, method = "continuous",topics  = n_t,printlegend = FALSE,
     moderator = "rating",moderator.value = "right",linecol = "blue", add = T, 
     xaxt ="n",
     xlab ="Time")
legend(x= "top", y = 0.4, c("left", "right"), lwd =2,  col = c("red", "blue"))






## ITALY PROCESSING ####

load("corpus_it.Rdata") 

it_excl <- c("La Legge Per Tutti")
corpus_it <- corpus_subset(corpus_it, !(corpus_it$origin %in% it_excl))
corpus_it$country <- "Italy"


corpus_it <- corpus_subset(corpus_it, origin2 == "La Repubblica" | origin2 == "Corriere Sera")


# outliers etc

# doubles: 247-167; 36-16
# saved from outliers and kept:  "text12","text254"
corpus_it <- corpus_subset(corpus_it, !docnames(corpus_it) %in% c("text285","text283","text247","text6",
                                                                  "text162","text202","text55","text129","text215",
                                                                  "text70","text80","text183", 
                                                                  "text224","text36",
                                                                  "text249",
                                                                  "text273",
                                                                  "text254",
                                                                  "text233",
                                                                  "text12",
                                                                  "text105",
                                                                  "text214",
                                                                  "text242",
                                                                  "text222",
                                                                  "text160"
)) 


lab_it <- paste0(ifelse(corpus_it$origin2 == "La Repubblica", "R-","C-"), docnames(corpus_it))
lab_it_rt <-  c("left"="La Repubblica","right" = "Corriere Sera")

rem_it <- c("fra","già?","oltre","ieri","può","soprattutto","molto","grandi","meno","tutt*","c'é","c'e",
            "state","essere","percentuale","imprese","solo","parte","ne","cosa","fare") 

compound_it <- phrase(c("cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
                        "unione europea", "impres* zombie", "aziend* zombie", "salva italia","decret* ristor*",
                        "lotto di vaccini","istitut* di credito","piano nazionale","sistem* produttivo","sistem* economic*",
                        "sistem* politic*"))



# dfm and dfm grouped
dfm_it <-  tokens(corpus_it,
                  remove_punct = TRUE, remove_symbols = TRUE, 
                  remove_numbers = TRUE,remove_url = FALSE, remove_separators = TRUE) %>%
  tokens_tolower() %>% 
  tokens_compound(compound_it) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>% 
  dfm()

dfm_gr_it <- dfm_group(dfm_it,groups = origin2)

# pass ITALY to final step for analysis

corpus_df <- corpus_it
dfm_df <- dfm_it
dfm_gr_df <- dfm_gr_it
lab_df <- lab_it
lab_df_rt <- lab_it_rt
store_df <- unique(corpus_it$country)


unlink(store_df, recursive = TRUE)
dir.create(store_df)

txit <- convert(corpus_df, to = "data.frame")

txit[which(txit$doc_id==""),]$text     



















## Descriptives ####

# General


ggplot(txit,aes(origin2, fill = rating)) + geom_bar() +
  scale_fill_manual(values = c("center" = "green", "left" = "red","right"= "blue")) +
  xlab("Source") +
  geom_text(stat='count', aes(label=..count..), hjust= 1) +
  coord_flip() + 
  theme_bw()





# Here reported calendar and source
ggplot(txit,aes(x = doc_id, y = datetimestamp)) + geom_point() + facet_wrap(~ origin2, scales = "free_y") +
  labs(x = "Documents", y = "Time") +
  coord_flip() + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(paste0("calendar","_",store_df,".jpg"),path = store_df)


# Absolute Frequency

features_dfm <- textstat_frequency(dfm_df, n = 30, groups = corpus_df$origin2)
features_dfm $feature <- with(features_dfm, reorder(feature, -frequency))

ggplot(features_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  ggtitle(dfm_df$country)  + coord_flip() +
  facet_wrap(~ group, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5))

# Relative Frequency


dfm_rel_freq <- dfm_weight(dfm_df, scheme = "prop") 
rel_freq <- textstat_frequency(dfm_rel_freq,  n = 20, groups = dfm_rel_freq$origin2)
freq <- textstat_frequency(dfm_df,groups = dfm_df$origin2)

ggplot(rel_freq, aes(x = nrow(rel_freq):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(rel_freq):1,
                     labels = rel_freq$feature) +
  labs(x = NULL, y = "Relative frequency")


## frequency for specific word

# freq$feature <-SnowballC::wordStem(freq$feature, language = 'de')
# freq$feature <- freq$feature %>% str_replace("-", "")

# here to combine into one category words not combined with steem or deleted hifen
# deutsch <- c("deutsche","deutschland")
# freq <- freq %>% mutate(dict = case_when(freq$feature %in% deutsch ~ "deutsch",
#                                          TRUE ~ freq$feature))

# freq_word <- textstat_frequency(dfm_df, groups = freq$origin2)
# freq_word <- subset(freq, feature %in% "zombiefirm")  

# ggplot(freq_word, aes(x = group, y = frequency)) +
#  geom_point() + 
#  scale_y_continuous(limits = c(0, 1.0), breaks = c(seq(0, 1.0, 0.1))) +
#  xlab(NULL) + 
#  ylab("Relative frequency") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Text Analysis ###



### compute tf_idf in tidy ####

tf_idf_df  <- txit %>% unnest_tokens(word,text) %>% dplyr::count(origin2, word, sort = TRUE) %>% ungroup()
tf_idf_df_total <- tf_idf_df %>% group_by(origin2) %>% summarise(total = sum(n))
tf_idf_df <- left_join(tf_idf_df,tf_idf_df_total)
tf_idf_df <- tf_idf_df %>% bind_tf_idf(word, origin2,n)

tf_idf_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(origin2) %>%
  top_n(8) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = TRUE) +
  #  scale_fill_manual(values = c("left" = "red","right" = "blue", "center" = "green")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~origin2, ncol = 4, scales = "free") +
  coord_flip() +
  theme(legend.position="bottom",legend.title=element_blank())

ggsave("tf_idf_de.jpg",height = 10, width = 12)



# Correspondence Analysis ####

ca <- textmodel_ca(dfm_df)

# one dimension
textplot_scale1d(ca, margin = "documents",  groups = docvars(corpus_df, "origin2")) 

ggsave(paste0("ca_scale1","_",store_df,".jpg"),path = store_df)

# two dimensions, dataframe and plot

dat_ca <- data.frame(nm = ca$rownames,
                     dim1 = coef(ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(ca, doc_dim = 2)$coef_document,
                     origin2 = corpus_df$origin2,
                     rating = corpus_df$rating)

V2 <- ggplot(dat_ca, aes(x = dim1,y=dim2, color = rating)) + geom_point() +  
  geom_text(label = dat_ca$nm, hjust=0.5, vjust=0,show.legend = FALSE) +
  labs(x = "",y="")+
  ggtitle(dfm_df$country) +
  scale_color_manual(values = c("left" = "red","right" = "blue","center" = "green")) +
  guides(color=guide_legend(title="Source")) + theme_bw() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 



# wordcloud ####

dfm_df %>% textplot_wordcloud(comparison = TRUE)


# relative frequency for each newspaper ####

dfm_prop <- dfm_df %>% dfm_weight(scheme = "prop")
freq_weight <- textstat_frequency(dfm_prop, n = 20, 
                                  groups = dfm_prop$origin2)

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  ggtitle(dfm_df$country) +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")+ theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave(paste0("rel_freq","_",store_df,".jpg"),path = store_df)


# keyness ####

textplot_keyness(textstat_keyness(dfm_gr_df, target = "Die Welt"), n = 20, margin = 0.3) +  theme(legend.position = "bottom")
ggsave(paste0("keyness","_",store_df,".jpg"),path = store_df)


# distance between texts, dendrogram to visualize ####

tstat_dist <- textstat_dist(dfm_weight(dfm_df, scheme = "prop"))

pres_cluster <- hclust(as.dist(tstat_dist))
pres_cluster$labels <- lab_df

ggdendrogram(pres_cluster, rotate = TRUE, theme_dendro = FALSE) +
  labs(x = "Documents",y = "dist") 

ggsave(paste0("dendro","_",store_df,".jpg"),path = store_df)

# wordfish, scaling of words and texts ####

# wordfish identifies an "ideology", i.e. a continuum between extreme opposite texts built by the researcher,
# and makes a regression to estimate psi: most frequent words (fixed effect), beta (words belonging to which direction of ideology)
# theta: positioning of the document along the ideology

docnames(corpus_df) # to select the most distant texts detected from the dendrogram to compute wordfish

wf <- textmodel_wordfish(dfm_df, dir = c(2, 9)) # dir is the rownumber of the texts selected (with docnames)

# words scaling: distribution in the space frequency (psi) and scaling on ideology (beta)
textplot_scale1d(wf, margin = "features",      
                 highlighted = c("covid","kurzarbeit","unternehmen"),  # words you want to highlight
                 highlighted_color = "red")

ggsave(paste0("wordfish_words","_",store_df,".jpg"),path = store_df)

# documents scaling (theta)
textplot_scale1d(wf, groups = corpus_df$origin2)
ggsave(paste0("wordfish_texts","_",store_df,".jpg"),path = store_df)

# word-network ####
set.seed(2929)
newspaper <- "Suddeutsche Zeitung" # choose journal to report

df_fcm <- fcm(dfm_subset(dfm_df, origin2 == newspaper))

topfcm <- names(topfeatures(df_fcm, 20))
topfcm_fcm <- fcm_select(df_fcm, pattern = topfcm)
textplot_network(topfcm_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5)

ggsave(paste0(newspaper,"_",store_df,".jpg"),path = store_df)


#### Topic Modelling, correct the formula each time ####
# convert to stm 

stm_df <- convert(dfm_df, to = "stm")


## Testing model k number suggested ####

# searchk
k_stm <- searchK(stm_df$documents, stm_df$vocab, K = c(5,7,8, 10,15,20,25), prevalence = ~ rating, data = stm_df$meta)
k_stm
graphics.off()
plot(k_stm)

# exclusivity * semantic coherence
selct_stm <- selectModel(stm_df$documents,stm_df$vocab,K = 15, prevalence = ~ rating, 
                         max.em.its = 75, data = stm_df$meta, runs = 20, seed = 122929)

selct_stm
plotModels(selct_stm,legend.position="bottomright")


# Run topic modelling ####

n_k <- 6 # number topics

stm_m <- stm(stm_df$documents,stm_df$vocab,K = 20, prevalence = ~ rating ,data = stm_df$meta,
             max.em.its = 75,  init.type = "Spectral")

plot.STM(stm_m,n = 5, type = "summary", main = dfm_df$country)
labelTopics(stm_m)


## Estimate model for each topic ####

est_m <- estimateEffect(formula = 1:n_k  ~  rating * datenum , stm_m, meta = stm_df$meta, uncertainty = "Global")

# it will report summary of model (regression coefficient and p-values) for each topic in the console

for (i in 1:n_k) {
  a <- summary(est_m, topics=i)
  print(a)
  
}

## Plot topic modelling ####

## Words to topic ####

tidy(stm_m) %>%
  group_by(topic) %>% 
  top_n(10) %>%
  ungroup %>%
  mutate(term =  reorder(term, beta))  %>%  
  mutate(topiclab = paste0("Topic ",topic)) %>%
  mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
  ggplot(aes(term,beta, fill = topiclab)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic2, scales = "free") +
  coord_flip() +
  ylab("Probability words belonging to topic") +
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(paste0("topic_beta","_",store_df,".jpg"),path = store_df)


## Find thoughts ####

txit$textshort <- str_sub(txit$text, 1, 200) # shorten to first 200 characters
thought_tp <- 1 

thoughts_df <- findThoughts(stm_m, texts = txit$textshort, n = 2, thresh = 0.5,  topics = thought_tp)$docs[[1]]
plotQuote(thoughts_df, width = 30, main = paste0("Topic ", thought_tp))


## Document to topic ####

# dataframe built to have gamma probability (document belong to topic) and show what source (or other metadata) documents belong to
stm_gamm <- tidy(stm_m, matrix = "gamma", document_names = rownames(dfm_df)) 
stm_gamm <- cbind(stm_gamm,dfm_df$origin2,dfm_df$rating)

stm_gamm %>% 
  mutate(topiclab = paste0("Topic ",topic)) %>%
  mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
  ggplot(aes(gamma, fill = as.factor(`dfm_df$origin2`))) +
  geom_histogram(show.legend = T) +
  facet_wrap(~topic2, ncol = 3)  +
  xlab("Probability documents belonging to topic") +
  ylab("") +
  # ggtitle(store_df) +
  #  scale_fill_manual(values = c("left" = "red","right" = "blue", "center" = "green")) +
  #  labels = lab_df_rt) + 
  guides(fill=guide_legend(title="Source")) +  theme_bw() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) 

ggsave(paste0("topic_gamma","_",store_df,".jpg"),path = store_df)




## Plot Different topic scaling ####

n_t <- 3 # topic selected (or change in code)

# on x-axis value1 appears on right, value 2 on left of reader
value1 <- "SZ"       
value2 <- "Die Welt"

plot(est_m, covariate = "rating", 
     topics = c(n_t), #topics to report
     model = stm_m, # stm
     method = "difference", # because binary
     cov.value1 = "left", cov.value2 = "right",
     xlab = paste0(value2, "  <----->  ", value1),
     labeltype = "custom", custom.labels = c(paste0("Topic ", n_t)), 
     main = store_df)


## Topical Content ####

# nk same as stm model, they should be coherent

stm_cont <- stm(stm_df$documents,stm_df$vocab,K = n_k, prevalence = ~ rating*datenum, 
                content = ~ rating, # added for content modelling
                data = stm_df$meta, max.em.its = 75,  init.type = "Spectral")

# summary for each topic
for (i in 1:n_k) {
  a <- summary(stm_cont, topics=i)
  print(a)
  
}

# plot semantic perspectives (topical content)
plot(stm_cont, type = "perspectives", topics = n_t, main = store_df)


## Mediation effects (e.g. time) or else. For interactions regression must be with interaction term *

plot(est_m,"datenum",model = stm_m, method = "continuous",topics = n_t, printlegend = FALSE,
     moderator = "rating",moderator.value = "left",linecol = "red",
     #  ylim = c(-0.4, 0.9), 
     main = paste0(store_df,"Topic ", n_t, ": time * political rating"),
     xaxt ="n",
     xlab ="Time")
plot(est_m,"datenum",model = stm_m, method = "continuous",topics  = n_t,printlegend = FALSE,
     moderator = "rating",moderator.value = "right",linecol = "blue", add = T, 
     xaxt ="n",
     xlab ="Time")
legend(x= "top", y = 0.4, c("left", "right"), lwd =2,  col = c("red", "blue"))



