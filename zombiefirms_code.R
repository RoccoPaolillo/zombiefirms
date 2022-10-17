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

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/zombie_firms/I_review/")

# Background code (cleaning etc.), not needed for analysis reproduction. Corpora are already prepared ####

## to find regex (root words/combinations) of "zombie firms" terms

df_word <- unique(stringr::str_extract_all(txdf$text,
regex("\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]* \\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]* chiusura \\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]* \\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]* \\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]*",
                                                 ignore_case = TRUE)))

unique(unlist(df_word))

checksen <- txdf %>% filter(str_detect(text,"\\beuropean zentral bank\\b"))

de_res <- unique(read.xls("zombiefirms.xls",sheet = "de_result2")[,1])
de_res <- paste0("\\b",de_res,"\\b",collapse="|")

## check doubles and out of context texts ####
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
# load("corpus_it_decotte.Rdata")
# corpus_it_decotte$datet <- corpus_it_decotte$date
#  corpus_it08 <- corpus_it08 + corpus_subset(corpus_it_decotte, !(corpus_it_decotte$id %in% corpus_it08$id))

load("corpus_it08orig.Rdata")
corpus_it08 <- corpus_it08[!duplicated(docvars(corpus_it08)),]

# bigrams, trigrams (not used) and variables
bg <- pull(read.csv("it_bigrams_08.csv"),2)
# trg <- pull(read.csv("it_trigrams_08.csv"),2)
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

# selection >= 2020
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
corpus_de08 <- corpus_de08[!duplicated(docvars(corpus_de08)),]


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

# filter >= 2020-01-01
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



# 

# clean corpus ####

 corpus_df <- corpus_it08 # change corpus_it08 or corpus_de08
# corpus_df <- corpus_de08 # change corpus_it08 or corpus_de08

# corpus_df <- corpus_reshape(corpus_df, to = "sentences")
# corpus_df <- corpus_reshape(corpus_df, to = "paragraph")

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
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,2] # lemmatized version #1
names(zombiefirm_replace_de) <- zombiefirm_pattern_de
txdf$text <- str_replace_all(txdf$text,regex(zombiefirm_replace_de,ignore_case = T))

corpus_df <- corpus(txdf)


# de keyterm zombie
# 
# zombieterms <- unique(read.xls("zombiefirms.xls",sheet = "de_lemma")[,3]) # 1
# zombieterms <-  zombieterms[zombieterms != ""]
# zombieterms <- c(zombieterms,"zombie")
# zombieterms <- paste0("\\b",zombieterms,"\\b",collapse="|")
# 
# tx_sen<- txdf %>% filter(str_detect(text,zombieterms))
# 
# tx_sen <- tx_sen %>% filter(datet >= "2020-01-01")
# 
# corpus_sen <- corpus(tx_sen)

# 

# compound terms
compound_de <- c("europäische union","european zentral bank",
                 "deutschen banken", "jens ehrhardt",
                 "carsten dierig","paschal donohoe",
                 "lucas flöther","alexander herzog","flüchtlingswelle 2015",# "millionen kurzarbeiter",
                 "ifo geschäftsklimaindex","insolvenzen abgewendet","jörg hofmann","mark schieritz",
                 "stefan bratzel",	"isabel schnabel","jan roth","corona hilfen",
                 "paul ziemiak","insolvenz tsunami","euro gruppe", "euro zone","billige geld",
                 "sieben tage", "erreicht worden","corona-hilfen","corona-hilfe",
                 #
                 "märz 2020","im griff","in den griff","große koalition","große entrup",
                 #SEN
                 "nicht überlebensfähig","finanziellen risiko","swiss re","entstehung von zombiefirms",
                 "in höhe von" ,
                 "european zentral bank",  "european zentral bank-präsident"    ,       
                 "european zentral bank-präsidenten", "european zentral bank-präsidentin"        , 
                "european zentral bank-geldpolitik" , "european zentral bank-politik"             ,
                 "european zentral bank-zinspolitik" , "european zentral bank-chefbankenaufseher" , 
                 "european zentral bank-rat" , "european zentral bank-direktorin"          ,
                  "european zentral bank-veranstaltung" ,                  
                "european zentral bank-vizepräsident" ,"european zentral bank-präsidentschaft"   ,  
                "european zentral bank-direktoriumsmitglied","european zentral bank-ökonomen"    ,        
                  "european zentral bank-chefökonom" ,"european zentral bank-maßnahmen"         ,  
               "european zentral bank-chef"   , "european zentral bank-rates"               ,
                  "european zentral bank-geldschwemme" ,"european zentral bank-stresstests"     ,    
                  "european zentral bank-direktor"  , "european zentral bank-bankenaufsicht",
               "finanzielle risiko",
               "im rahmen",
                 # keyness
               "neue normalität" 
                 # "römermann fachanwalt"
                 )

bg <- bg[!(bg %in% c("kommenden jahr","ersten halbjahr","gefahr dass","kommende jahr","vergangenen jahr",
                     # SEN
                     "nicht überlebensfähig seien","überlebensfähig seien","dass firmen","unternehmen geben",
                     "bundesregierung züchtet","sagt hantzsch","warnungen kurzarbeitergeld mäste",
                     "millionen unternehmen entspricht", "bundesbank warnt",
                     #
                     "gewaltigen anzahl sogenannter zombieunternehmen führen angesichts",
                     "leben erhalten schrieb michael hüther direktor",
                     "großen stil angewandte kurzarbeit",
                     "staat via kurzarbeit",
                     "staatslöhnen genannt kurzarbeitergeld",
                     "dank kurzarbeit überleben müsste stattdessen",
                     "kurzarbeit weiterbestehen",
                     "warnungen kurzarbeitergeld mäste"
                     
                     ))]

# terms to remove, from key-center co-occurrences
# rem_dekey <- read.xls("zombiefirms.xls",sheet = "rem_dekey")[,1]
#terms to remove
rem_de <- c("die welt","faz.net","die zeit","suddeutsche zeitung","handelsblatt","spiegel","f.a.z.","f.a.z",
            "faz","f.az","f a z","fa.z","welt","darf","schließlich",
            "immer", "trotzdem" , "nämlich" ,  "nennt","zweiten","besser",
            "immerhin", # "unternehmen","firmen",
            "schwer","rund","wegen","denen","sz","WELT AM SONNTAG",
            "später",
            "dass","zahl","prozent","viele","mehr","schon","sei","gibt","sagt","sagte","dabei","menschen","seien","diesmal",
            "sitzen","darin","geht","seit","sogar","tun",
            "komme", "kommst", "kommt", "kommen",
            "muss","musst","muss","müssen","müsst","warum",
            "soll","sollst","sollt","sollen", "sollt", "sollen",
            "laut","jahr","ende","etwa","etwas","moglich", "allerdings","uhr","ezb","ab",
            "kann","kannst","kann", "können", "könnt", "könnte","könnten","könne","fast",
            "gut", "zudem", "eigentlich" , "weitere", "bisher", "weniger","iw","gar","hoch",
            "allerdings",  "möglich","dafür", "wäre" ,"gerade" ,"jahren", "ja", "bereits", "derzeit","blick", # "rheinland-pfalz",
            "anteil","daher","viertagewoche","sagen","sagt","anfang","gehen","gab","gab es","hochbahn","benex","tepco", # "zugbegleiter",
            "passiert","lange","erst","macht","wären","hälfte","rede",
            #
            "quelle","sollten","heißt","längst","hatte","stellt","hätten","müssten",
            "teil","sicht","sehen","besteht","sewing","dadurch","wohl","wann","hätte",
            "jedoch","patrik-ludwig","viele_menschen",
            #
            "demnach","grund","somit","ersten","halbjahr","kommenden", "vergangenen",
            "allein","dürften",
            #
           "instituts","aufgrund","tatsächlich",
            "gemäß","gebe","halle","begriff","fall","vorjahr","zumindest", #"montag",
            "haeusgen","kritisiert", "walter-borjans", "hervor",
            "vielen","tut", #,
            # CO-occurrence
            "schnell","frage","beispiel","zeit","in_höhe_von", "folge","folgen",  #,  
            # SEN 2
            "zusammenhang","hantzsch","gleich","gleichzeitig","deshalb","sogenannten",
            "zahlen", "beispiel_volker_römermann_fachanwalt" , "lassen", "analyse", # ,
           "macht_daher_schon_länger",
           "im_rahmen",
           "politik_unternehmen","runde",  "sogenannte"  
            # SEN
          #   "zombification","zombie_unternehmen","zombie_wirtschaft","zombie_firma","unternehmenszombies",
          #   "zombie_banken", "zombifizierung","zombie","zombiefirms", "entstehung_von_zombiefirms",
          # "entstehen","oecd-definition", "spd-politiker" # ,"wort","neue"
)




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
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de)) %>% # , rem_dekey)) %>% 
  dfm()

# to check frequency
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,5] #1
freq_stat <- textstat_frequency(dfm_df,groups = rating) %>% 
  subset(feature %in% c(unique(zombiefirm_replace_de)))


# lemmatization and back Italy (mainly zombiefirms) #### 
# see aboove German corpus for the code)

zombiefirm_pattern_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,1]
zombiefirm_pattern_it <- paste0("\\b",zombiefirm_pattern_it,"\\b")
zombiefirm_replace_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,2] #1
# zombiefirm_replace <- paste0("\\b",zombiefirm_replace,"\\b",collapse="|")
names(zombiefirm_replace_it) <- zombiefirm_pattern_it
txdf$text <- str_replace_all(txdf$text,regex(zombiefirm_replace_it,ignore_case = T))

corpus_df <- corpus(txdf)

# it keyterm zombie
zombieterms <- unique(read.xls("zombiefirms.xls",sheet = "ita_lemma")[,3]) #1
zombieterms <-  zombieterms[zombieterms != ""]
zombieterms <- c(zombieterms,"zombie")
zombieterms <- paste0("\\b",zombieterms,"\\b",collapse="|")

tx_sen <- txdf %>% filter(str_detect(text,zombieterms))

tx_sen <- tx_sen %>% filter(datet >= "2020-01-01")

corpus_sen <- corpus(tx_sen)


# txit$text <- stri_replace_all_fixed(txit$text, zombiefirm_pattern,  zombiefirm_replace, vectorize_all=FALSE)

compound_it <- c( # "aziende zombie","imprese zombie","zombie company","società zombie","organizzazione zombie",
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
                  "sistema creditizio",
                  "fine pandemia","fine della pandemia", "in piedi", "pioggia di soldi","alto rendimento","alto adige",
                  "posto di lavoro","posto lavoro", "crisi economica","crisi finanziaria","crisi del 2008",
                  "posti di lavoro","presidente del consiglio","a favore","piccole e medie imprese","piccole imprese",
                  "valore aggiunto", "riproduzione riservata",
                  # SEN,
                  "tenute in vita","emergenza sanitaria","flusso di credito","flusso di sussidi",
                  "banca italia","crisi post-covid","crisi economica","crisi finanziaria","crisi pandemica",
                  "crisi industriali","crisi politica","crisi di liquidità","crisi del lavoro","crisi finanziaria",
                  "crisi del governo","reddito di cittadinanza",
                  "situazione finanziaria","area euro", "miliardi di euro", "milioni di euro", "miliardi euro","milioni euro",
                  "tempo indeterminato","situazione finanziaria","situazione straordinaria","in grado",
                  "debito buono","debito cattivo", "in grado",  "fine della pandemia", "fine anno", "fine delle moratorie",
                  "fine della campagna vaccinale", "tenute in vita",
                  # 2nd round
                  "consiglio di sorveglianza","consiglio di amministrazione","consiglio di stato","consiglio di fabbrica",
                  "presidente del consiglio","presidente del parlamento","ex presidente del consiglio",
                  "associazione bancaria","associazione a delinquere","cassa depositi","cassa ordinaria","cassa covid",
                  "prestiti zombie","posizioni zombie","capitale umano","capitale produttivo","capitale ambientale",
                  "capitale immateriale","stato di emergenza","stato di necessità","fare i conti",
                  "mercato interno","mercato economico","mercato del lavoro", 
                  #3rd round
                  "ministro del lavoro","ministero del lavoro","banche italiane","banca d italia", 
                  "banca centrale","banche europee","economia di mercato","economia italiana","economia circolare",
                  "economia della conoscenza","economia agricola", "attività economiche","attività assicurative",
                  "attività produttive", "beni e servizi","servizi di alloggio", "restano in vita",
                  "intervento pubblico","tenuto in vita","tenute in vita","ristori pubblici","ristori del governo",
                  "banca italia",
                  "film dell orrore", "aiuti di stato","stato di salute","stato di default",
                  #4th round
                  "business intelligence","economia reale","economia finanziaria","economia espansiva",
                  "distruzione creativa","creative destruction","fondo perduto", "gruppo ilva",
                  #5th round
                  "senza *","niente *",
                  "azione di governo","distribuzione dei dividendi","distribuzione della ricchezza","distribuzione del capitale",
                  "distribuzione di dividendi","distribuzione degli utili",
                  "sovranità monetaria","articolo 11","articolo 54","articolo 45","articolo 50",
                  "trasporto locale","trasporto aereo",
                  "segretario del tesoro","ministro del tesoro","segreteria al tesoro","tesoro pubblico",
                  # kn
                  "spese clientelari","gruppo intesa sanpaolo","gruppo cgbi","gruppo dei 30","gruppo dei trenta",
                  "gruppo roma capitale","gruppo bancario","gruppo marriot bonvoy",
                  "compagnia di bandiera","compagnie aeree",
                  "zero ore","interesse a zero","beni intermedi","beni pubblici","beni durevoli","beni di consumo",
                  "giro di carte",
                  "limite di indebitamento",
                  "mancanza di risorse","mancanza di liquidità","mancanza di consumi", "mancanza di interventi",
                  "ragazzi di terza media","ragazzi del quartiere",
                  "presidente del consiglio","consiglio di amministrazione","cosiglio dei ministri"
                  
                  )

bg <- bg[!(bg %in% c("altman evidenzia poi","banca commerciale classis capital società"))]



rem_it <- c("fra","già?","già","oltre","ieri","può","soprattutto","molto","grandi","meno","tutto","tutti",
            "c'é","c'e","state","essere","percentuale", "solo","parte","ne","cosa","fare",
            "c’è","c ’è","c'è","c 'è","d", "avere","milano","torino","bologna","napoli","pisa","quindi",
            "volta","veneto", "termini","fatto quotidiano","la repubblica","corriere della sera", "il giornale",
            "il corrier","corriere","il fatto","https*","fatto_quotidiano","fatto_quotidiano","fatto","poi", "mai","prima",
            "cento","gran_parte","gran* parte", #
            "anni","soltanto","d accordo","d_accordo","circa","però",
            "proprio","allora","fa","ciò","stata","qui","altra","vero","là","dopo","dare", 
           "invece", "senso", "giorni","passato",
            "addirittura", "base",  "poco", "alto","ora","ancora", "considera","posto",
            "presenza", "tanto",  "tutte","sorta","fronte","causa","mentre","quando",
            "fino","secondo","terzo", "mettere", "mesi", "altri","intervista",
             "spiegato", "presentato","tratta","punto","seguito","caso","deciso", #
            "modo","quali","quasi","cioè", "forse","stesso","certo","po","dichiarato","detto","oggi","particolare",
            "altre","certo","almeno","infatti","quarto","pure","può_permettersi","considerata","trova",
            "così", "proviene","stesso_periodo","quest_anno", # "meno_rispetto","così","cosiddette"
            "numero","è_stato", "era_stato", "fosse_stato", "in_stato_di","credo","perciò" ,
           "riproduzione_riservata","©",
            #
           "molte","sostenere","settore","rapporto","chiesto",
           "anno", "fase_attuale","esistono", "informazioni_disponibili", 
           "contano","creando",  "fine","tempo","situazione","momento","dati","grado" , "milioni","miliardi",
           "in_grado","mondo", "dire",
           "nemmeno","grazie",   #
           # 2nd round
            "imprese_intervistate", "ottenuto" , 
           # 3rd round                                      # ,  # "prova","chiaro","imprese","luglio","agosto",# ,"intervento","rischio","terziario", "futuro", "italia"
            "stima","aumentare","roma","articolo","quantità","numeri","entro",
           "richiede","far","percorso",
           "sempre"  
        # TRY TO REMOVE: "aumento", "mondo"
           
         # "d_italia","imprese intervistate","ottenuto","città", "mondo",  "attesa","ultima","domanda","dire","tema","buono", "chiuso",
         # "febbraio","giugno","chiusura","gennaio","aprile","novembre", "linea","mese","orrore", "aumentato",
          # SEN,
        #  "zombiefirms", "zombie",
        #  "aziende_zombie", "imprese_zombie", "zombie_company", "società_zombie", "organizzazione_zombie",
        #  "zombiefirms","zombie",
        # "troppe","tante","troppe","tante","passo","possono","società"
) 



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


zombiefirm_replace_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,3] # +1
freq_stat <- textstat_frequency(dfm_df, groups = rating) %>% 
  subset(feature %in% c(zombiefirm_replace_it, "è stato","è_stato","é stato",
                        "azienda decotta",
                        "imprese decotte",
                        "aziende decotte",
                        "società decotte",
                        "industrie decotte"
  ))

# keyness, repeated for Italy and Germany ####

# here two different corpora where run for Italian and German corpus in order to combine for visualization
kn_it <- textstat_keyness(dfm_group(dfm_subset(dfm_df, datet >= "2020-01-01" ),groups = rating),
                 target = "right")
kn_it$country <-  "Italy"

pl_knit <- textplot_keyness(kn_it, n = 20, margin = 0.1,
                 labelsize = 10, color = c("black","grey")) +
  ylab(kn_it$country) +
  xlab(expression(chi^2)) +
  theme_bw()  +
  theme( axis.title.y = element_text(size = 20),
         axis.text.y = element_blank(),axis.ticks.y = element_blank(),
         axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 15),
         plot.title = element_text(hjust = 0.5),legend.position = "bottom",
         legend.text = element_text(size=20))
ggsave(filename = paste0("images/",unique(kn_it$country),".jpg"), width = 20, height = 30)

kn_de <- textstat_keyness(dfm_group(dfm_subset(dfm_df, datet >= "2020-01-01" ),groups = rating),
                          target = "right")
kn_de$country <- "Germany"

pl_knde <- textplot_keyness(kn_de, n = 20, margin = 0.1,
                 labelsize = 10, color = c("black","grey")) +
  ylab(kn_de$country) +
  xlab(expression(chi^2)) +
 theme_bw()  +
  theme( axis.title.y = element_text(size = 20),
    axis.text.y = element_blank(),axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 15),
    plot.title = element_text(hjust = 0.5),legend.position = "bottom",
        legend.text = element_text(size=20))
ggsave(filename = paste0("images/",unique(kn_de$country),".jpg"), width = 20, height = 30)

# kn_tot <- rbind(kn_it,kn_de)

kntot <- ggpubr::ggarrange(pl_knde,pl_knit,ncol  =1, nrow  = 2, common.legend = T,legend = "bottom")
ggsave(filename = "images/kntot.jpg", width = 20, height = 30)

# ggsave(file=paste0("images/knpl_",unique(dfm_df$country),".jpg"), width = 17, height = 14)


# tk_df (co-occurrence) ITA, to be used in Co-occurrences section ####

# co-occurrences feature matrix Italy left


fcm_lf <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "left"),
                remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_it,bg))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>% 
  fcm(context = "window", window = 20, tri = FALSE) 

co_occur_network_lf <- graph_from_adjacency_matrix(fcm_lf, mode = "undirected", diag = FALSE) # , weighted = T
nm_occ <- as.data.frame(names(V(co_occur_network_lf)))
dg_occ <- as.data.frame(strength(co_occur_network_lf))
df_occ <- cbind(nm_occ,dg_occ)
df_occ <- df_occ[order(-strength(co_occur_network_lf)),]
df_occ[1:30,]

co_occur_network_lf2 <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = df_occ[1:30,1]),
                                                    mode = "undirected", diag = FALSE)


E(co_occur_network_lf2)$weight <- 1
co_occur_network_lf2 <- simplify(co_occur_network_lf2, edge.attr.comb=list(weight="sum"))
co_occur_network_lf2$ref <- paste0(unique(corpus_df$country)," ","Left")

# fctop_lf <- topfeatures(fcm_lf,30) %>% names()
# # co-occurrence plot left
# # co_occur_network_lf <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = fctop_lf), mode = "undirected", diag = FALSE ) # , weighted = T
# co_occur_network_lf <- graph_from_adjacency_matrix(fcm_lf, mode = "undirected", diag = FALSE) # , weighted = T
#   nm_occ <- as.data.frame(names(V(co_occur_network_lf)))
#   dg_occ <- as.data.frame(strength(co_occur_network_lf))
#   df_occ <- cbind(nm_occ,dg_occ)
#   df_occ <- df_occ[order(-strength(co_occur_network_lf)),]
#   df_occ[1:30,]
#   
# E(co_occur_network_lf)$weight <- count.multiple(co_occur_network_lf)
# 
# E(co_occur_network_lf)$weight <- 1
# co_occur_network_lf <- simplify(co_occur_network_lf, edge.attr.comb=list(weight="sum"))
# 
# co_occur_network_lf <- simplify(co_occur_network_lf)
# co_occur_network_lf$ref <- paste0(unique(corpus_df$country)," ","Left")
# tkplot for interactive networks (don't close while working on the code!)
# tk_lf <- tkplot(co_occur_network_lf)
# l_lf <- tkplot.getcoords(tk_lf) # take tk_lf coordination(s)

# Same procedure for Italy right
fcm_rt <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "right"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_it,bg))) %>%
  tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>%
  fcm(context = "window", window = 20, tri = FALSE) 

co_occur_network_rt <- graph_from_adjacency_matrix(fcm_rt, mode = "undirected", diag = FALSE) # , weighted = T
nm_occrt <- as.data.frame(names(V(co_occur_network_rt)))
dg_occrt <- as.data.frame(strength(co_occur_network_rt))
df_occrt <- cbind(nm_occrt,dg_occrt)
df_occrt <- df_occrt[order(-strength(co_occur_network_rt)),]
df_occrt[1:30,]

co_occur_network_rt2 <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = df_occrt[1:30,1]),
                                                    mode = "undirected", diag = FALSE)


E(co_occur_network_rt2)$weight <- 1
co_occur_network_rt2 <- simplify(co_occur_network_rt2, edge.attr.comb=list(weight="sum"))
co_occur_network_rt2$ref <- paste0(unique(corpus_df$country)," ","Right")

# fctop_rt <- topfeatures(fcm_rt, 30) %>% names()
# # co-occurrence plot left
# co_occur_network_rt <- graph_from_adjacency_matrix(fcm_rt, mode = "undirected", diag = FALSE ) # , weighted = T
# # co_occur_network_rt <- graph_from_adjacency_matrix(fcm_rt, mode = "undirected", diag = FALSE ) # , weighted = T
# #  co_occur_network_rt <- graph_from_adjacency_matrix(fcm_rt,  mode = "undirected", diag = FALSE ) # , weighted = T
# nm_occrt <- as.data.frame(names(V(co_occur_network_rt)))
# dg_occrt <- as.data.frame(strength(co_occur_network_rt))
# df_occrt <- cbind(nm_occrt,dg_occrt)
# df_occrt <- df_occrt[order(-strength(co_occur_network_rt)),]
# df_occrt[1:30,]
# 
# E(co_occur_network_rt)$weight <- count.multiple(co_occur_network_rt)
# 
# E(co_occur_network_rt)$weight <- 1
# co_occur_network_rt <- simplify(co_occur_network_rt, edge.attr.comb=list(weight="sum"))
# 
# co_occur_network_rt <- simplify(co_occur_network_rt)
# co_occur_network_rt$ref <- paste0(unique(corpus_df$country)," ","Right")


# tk_rt <- tkplot(co_occur_network_rt)
# l_rt <- tkplot.getcoords(tk_rt)

# tk_df (co-occurrence) DEU, to be used in Co-occurrences section  ####
# left co-occurrence matrix

fcm_lf <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "left"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de)) %>%
  fcm(context = "window", window = 20, tri = FALSE) 

# fctop_lf <- topfeatures(fcm_lf, 30) %>% names()

co_occur_network_lf <- graph_from_adjacency_matrix(fcm_lf, mode = "undirected", diag = FALSE) # , weighted = T
nm_occ <- as.data.frame(names(V(co_occur_network_lf)))
dg_occ <- as.data.frame(strength(co_occur_network_lf))
df_occ <- cbind(nm_occ,dg_occ)
df_occ <- df_occ[order(-strength(co_occur_network_lf)),]
df_occ[1:30,]

co_occur_network_lf2 <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = df_occ[1:30,1]),
                                                    mode = "undirected", diag = FALSE)


E(co_occur_network_lf2)$weight <- 1
co_occur_network_lf2 <- simplify(co_occur_network_lf2, edge.attr.comb=list(weight="sum"))
co_occur_network_lf2$ref <- paste0(unique(corpus_df$country)," ","Left")

# co-occurrence plot left
# co_occur_network_lf <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = fctop_lf), mode = "undirected", diag = FALSE ) # , weighted = T
# E(co_occur_network_lf)$weight <- count.multiple(co_occur_network_lf)
# co_occur_network_lf <- simplify(co_occur_network_lf)

# tkplot for interactive networks (don't close while working on the code!)
# tk_lf <- tkplot(co_occur_network_lf)
# l_lf <- tkplot.getcoords(tk_lf) # take tk_lf coordination(s)

# Same procedure for Italy right
fcm_rt <- tokens(corpus_subset(corpus_df,datet >= "2020-01-01" & rating == "right"),
                 remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                 remove_numbers = TRUE,remove_url = FALSE) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de)) %>%
  fcm(context = "window", window = 20, tri = FALSE) 
# fctop_rt <- topfeatures(fcm_rt, 30) %>% names()

co_occur_network_rt <- graph_from_adjacency_matrix(fcm_rt, mode = "undirected", diag = FALSE) # , weighted = T
nm_occrt <- as.data.frame(names(V(co_occur_network_rt)))
dg_occrt <- as.data.frame(strength(co_occur_network_rt))
df_occrt <- cbind(nm_occrt,dg_occrt)
df_occrt <- df_occrt[order(-strength(co_occur_network_rt)),]
df_occrt[1:30,]

co_occur_network_rt2 <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = df_occrt[1:30,1]),
                                                    mode = "undirected", diag = FALSE)

E(co_occur_network_rt2)$weight <- 1
co_occur_network_rt2 <- simplify(co_occur_network_rt2, edge.attr.comb=list(weight="sum"))
co_occur_network_rt2$ref <- paste0(unique(corpus_df$country)," ","Right")


# co-occurrence plot left
# co_occur_network_rt <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = fctop_rt), mode = "undirected", diag = FALSE ) # , weighted = T
# E(co_occur_network_rt)$weight <- count.multiple(co_occur_network_rt)
# co_occur_network_rt <- simplify(co_occur_network_rt)

# newtest to delete ####

# df_rt <- dfm_subset(dfm_df,datet >= "2020-01-01" & rating == "right")
# tp_df_rt <- names(topfeatures(df_rt, 20))
# df_lf <- dfm_subset(dfm_df,datet >= "2020-01-01" & rating == "left")
# tp_df_lf <- names(topfeatures(df_lf, 20))
# topgat_fcm <- fcm_select(fcm_rt, pattern = tp_df_rt)
# 
# 
# fcm_rt <- fcm(df_rt,context = "window", window = 20) 
# fctop_rt <- topfeatures(fcm_rt, 20) %>% names()
# 
# co_occur_network_rt <- graph_from_adjacency_matrix(fcm_select(fcm_rt, pattern = tp_df_rt), mode = "undirected", diag = FALSE)
# E(co_occur_network_rt)$weight <- count.multiple(co_occur_network_rt)
# co_occur_network_rt <- simplify(co_occur_network_rt)
# co_occur_network_rt$ref <- "Germany Right"
# 
# fcm_lf <- fcm(df_lf,context = "window", window = 20) 
# fctop_lf <- topfeatures(fcm_lf, 20) %>% names()
# 
# 
# co_occur_network_lf <- graph_from_adjacency_matrix(fcm_select(fcm_lf, pattern = tp_df_lf), mode = "undirected", diag = FALSE)
# E(co_occur_network_lf)$weight <- count.multiple(co_occur_network_lf)
# co_occur_network_lf <- simplify(co_occur_network_lf)
# co_occur_network_lf$ref <- "Germany Left"



# CO-occurrences ####
# tkplot for interactive network to manipulate. An interactive window will open to work with
# Leave the window open until end of plotting final figures.

# left-wing co-occurrences
tk_lf <- tkplot(co_occur_network_lf2)
l_lf <- tkplot.getcoords(tk_lf) # to take layout from tkplot geo coordinate 

 par(mar = c(0, 0,1.3 , 0)) 
plot(co_occur_network_lf2,
     layout = l_lf,
        vertex.size = ( strength(co_occur_network_lf2) / max(strength(co_occur_network_lf2)) * 20) , # (strength(co_occur_network_lf2) / max(strength(co_occur_network_lf2)) * 10) ,
     vertex.shape = "circle",
     vertex.label =  paste0(V(co_occur_network_lf2)$name), #, strength(co_occur_network_lf) ),
     vertex.label.color = "black",
     vertex.label.cex = 1.2,
     vertex.color = "grey",
   #  edge.width = (E(co_occur_network_lf)$weight / 4),
   edge.width = (E(co_occur_network_lf2)$weight / 1.5),
  # edge.label =  E(co_occur_network_lf)$weight,
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network_lf2)$name == "zombiefirms",2,1),
     vertex.label.dist = 1.2
    
)
title(co_occur_network_lf2$ref,cex.main= 1.5)


# right-wing co-occurrences

tk_rt <- tkplot(co_occur_network_rt2) 
l_rt <- tkplot.getcoords(tk_rt)
par(mar = c(0, 0,1.3 , 0)) 
plot(co_occur_network_rt2,
     layout = l_rt,
     vertex.size = (strength(co_occur_network_rt2) / max(strength(co_occur_network_rt2)) * 20) ,
     vertex.shape = "circle",
     vertex.label =  paste0(V(co_occur_network_rt2)$name), #, strength(co_occur_network_rt) ),
     vertex.label.color = "black",
     vertex.label.cex = 1.2,
     vertex.color = "grey",
     #  edge.width = (E(co_occur_network_rt)$weight / 4),
     edge.width = (E(co_occur_network_rt2)$weight / 1.5),
    #  edge.label =  E(co_occur_network_rt)$weight,
     layout=layout.circle,
     vertex.label.font = ifelse(V(co_occur_network_rt2)$name == "zombiefirms",2,1),
     vertex.label.dist = 1.2
     
)
title(co_occur_network_rt2$ref,cex.main=1.5)



# Keyness zombie ####


# clean corpus ####

# corpus_df <- corpus_it08 # change corpus_it08 or corpus_de08
corpus_df <- corpus_de08 # change corpus_it08 or corpus_de08

 corpus_df <- corpus_reshape(corpus_df, to = "sentences")
# corpus_df <- corpus_reshape(corpus_df, to = "paragraph")

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
zombiefirm_replace_de <- read.xls("zombiefirms.xls",sheet = "de_lemma")[,2] # lemmatized version #1
names(zombiefirm_replace_de) <- zombiefirm_pattern_de
txdf$text <- str_replace_all(txdf$text,regex(zombiefirm_replace_de,ignore_case = T))

corpus_df <- corpus(txdf)


# de keyterm zombie

zombieterms <- unique(read.xls("zombiefirms.xls",sheet = "de_lemma")[,3]) # 1
zombieterms <-  zombieterms[zombieterms != ""]
zombieterms <- c(zombieterms,"zombie")
zombieterms <- paste0("\\b",zombieterms,"\\b",collapse="|")

tx_sen<- txdf %>% filter(str_detect(text,zombieterms))

tx_sen <- tx_sen %>% filter(datet >= "2020-01-01")

corpus_sen <- corpus(tx_sen)

# 

# compound terms
compound_de <- c("europäische union","european zentral bank",
                 "deutschen banken", "jens ehrhardt",
                 "carsten dierig","paschal donohoe",
                 "lucas flöther","alexander herzog","flüchtlingswelle 2015",# "millionen kurzarbeiter",
                 "ifo geschäftsklimaindex","insolvenzen abgewendet","jörg hofmann","mark schieritz",
                 "stefan bratzel",	"isabel schnabel","jan roth","corona hilfen",
                 "paul ziemiak","insolvenz tsunami","euro gruppe", "euro zone","billige geld",
                 "sieben tage", "erreicht worden","corona-hilfen","corona-hilfe",
                 #
                 "märz 2020","im griff","in den griff","große koalition","große entrup",
                 #SEN
                 "nicht überlebensfähig","finanziellen risiko","swiss re","entstehung von zombiefirms",
                 "in höhe von" ,
                 "european zentral bank",  "european zentral bank-präsident"    ,       
                 "european zentral bank-präsidenten", "european zentral bank-präsidentin"        , 
                 "european zentral bank-geldpolitik" , "european zentral bank-politik"             ,
                 "european zentral bank-zinspolitik" , "european zentral bank-chefbankenaufseher" , 
                 "european zentral bank-rat" , "european zentral bank-direktorin"          ,
                 "european zentral bank-veranstaltung" ,                  
                 "european zentral bank-vizepräsident" ,"european zentral bank-präsidentschaft"   ,  
                 "european zentral bank-direktoriumsmitglied","european zentral bank-ökonomen"    ,        
                 "european zentral bank-chefökonom" ,"european zentral bank-maßnahmen"         ,  
                 "european zentral bank-chef"   , "european zentral bank-rates"               ,
                 "european zentral bank-geldschwemme" ,"european zentral bank-stresstests"     ,    
                 "european zentral bank-direktor"  , "european zentral bank-bankenaufsicht",
                 "finanzielle risiko",
                 "im rahmen",
                 # keyness
                 "neue normalität" 
)

bg <- bg[!(bg %in% c("kommenden jahr","ersten halbjahr","gefahr dass","kommende jahr","vergangenen jahr",
                     # SEN
                     "nicht überlebensfähig seien","überlebensfähig seien","dass firmen","unternehmen geben",
                     "bundesregierung züchtet","sagt hantzsch","warnungen kurzarbeitergeld mäste",
                     "millionen unternehmen entspricht", "bundesbank warnt",
                     #
                     "gewaltigen anzahl sogenannter zombieunternehmen führen angesichts",
                     "leben erhalten schrieb michael hüther direktor",
                     "großen stil angewandte kurzarbeit",
                     "staat via kurzarbeit",
                     "staatslöhnen genannt kurzarbeitergeld",
                     "dank kurzarbeit überleben müsste stattdessen",
                     "kurzarbeit weiterbestehen",
                     "warnungen kurzarbeitergeld mäste"
                     
))]

# terms to remove, from key-center co-occurrences
# rem_dekey <- read.xls("zombiefirms.xls",sheet = "rem_dekey")[,1]
#terms to remove
rem_de <- c("die welt","faz.net","die zeit","suddeutsche zeitung","handelsblatt","spiegel","f.a.z.","f.a.z",
            "faz","f.az","f a z","fa.z","welt","darf","schließlich",
            "immer", "trotzdem" , "nämlich" ,  "nennt","zweiten","besser",
            "immerhin", # "unternehmen","firmen",
            "schwer","rund","wegen","denen","sz","WELT AM SONNTAG",
            "später",
            "dass","zahl","prozent","viele","mehr","schon","sei","gibt","sagt","sagte","dabei","menschen","seien","diesmal",
            "sitzen","darin","geht","seit","sogar","tun",
            "komme", "kommst", "kommt", "kommen",
            "muss","musst","muss","müssen","müsst","warum",
            "soll","sollst","sollt","sollen", "sollt", "sollen",
            "laut","jahr","ende","etwa","etwas","moglich", "allerdings","uhr","ezb","ab",
            "kann","kannst","kann", "können", "könnt", "könnte","könnten","könne","fast",
            "gut", "zudem", "eigentlich" , "weitere", "bisher", "weniger","iw","gar","hoch",
            "allerdings",  "möglich","dafür", "wäre" ,"gerade" ,"jahren", "ja", "bereits", "derzeit","blick", # "rheinland-pfalz",
            "anteil","daher","viertagewoche","sagen","sagt","anfang","gehen","gab","gab es","hochbahn","benex","tepco", # "zugbegleiter",
            "passiert","lange","erst","macht","wären","hälfte","rede",
            #
            "quelle","sollten","heißt","längst","hatte","stellt","hätten","müssten",
            "teil","sicht","sehen","besteht","sewing","dadurch","wohl","wann","hätte",
            "jedoch","patrik-ludwig","viele_menschen",
            #
            "demnach","grund","somit","ersten","halbjahr","kommenden", "vergangenen",
            "allein","dürften",
            #
            "instituts","aufgrund","tatsächlich",
            "gemäß","gebe","halle","begriff","fall","vorjahr","zumindest", #"montag",
            "haeusgen","kritisiert", "walter-borjans", "hervor",
            "vielen","tut", #,
            # CO-occurrence
            "schnell","frage","beispiel","zeit","in_höhe_von", "folge","folgen",  #,  
            # SEN 2
            "zusammenhang","hantzsch","gleich","gleichzeitig","deshalb","sogenannten",
            "zahlen", "beispiel_volker_römermann_fachanwalt" , "lassen", "analyse", # ,
            "macht_daher_schon_länger",
            "im_rahmen",
            "politik_unternehmen","runde",  "sogenannte"  ,
            # SEN
              "zombification","zombie_unternehmen","zombie_wirtschaft","zombie_firma","unternehmenszombies",
              "zombie_banken", "zombifizierung","zombie","zombiefirms", "entstehung_von_zombiefirms",
            "entstehen","oecd-definition", "spd-politiker" # ,"wort","neue"
)




# Document-term matrix
dfm_df <-  tokens( corpus_sen,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_numbers = TRUE,
                   remove_url = FALSE
) %>%
  tokens_tolower() %>% 
  tokens_compound(phrase(c(compound_de,bg))) %>%
  tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de)) %>% # , rem_dekey)) %>% 
  dfm()




# keyness sentence-level ita


corpus_df <- corpus_it08 # change corpus_it08 or corpus_de08
# corpus_df <- corpus_de08 # change corpus_it08 or corpus_de08

 corpus_df <- corpus_reshape(corpus_df, to = "sentences")
# corpus_df <- corpus_reshape(corpus_df, to = "paragraph")

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



zombiefirm_pattern_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,1]
zombiefirm_pattern_it <- paste0("\\b",zombiefirm_pattern_it,"\\b")
zombiefirm_replace_it <- read.xls("zombiefirms.xls",sheet = "ita_lemma")[,2] #1
# zombiefirm_replace <- paste0("\\b",zombiefirm_replace,"\\b",collapse="|")
names(zombiefirm_replace_it) <- zombiefirm_pattern_it
txdf$text <- str_replace_all(txdf$text,regex(zombiefirm_replace_it,ignore_case = T))

corpus_df <- corpus(txdf)

# it keyterm zombie
zombieterms <- unique(read.xls("zombiefirms.xls",sheet = "ita_lemma")[,3]) #1
zombieterms <-  zombieterms[zombieterms != ""]
zombieterms <- c(zombieterms,"zombie")
zombieterms <- paste0("\\b",zombieterms,"\\b",collapse="|")

tx_sen <- txdf %>% filter(str_detect(text,zombieterms))

tx_sen <- tx_sen %>% filter(datet >= "2020-01-01")

corpus_sen <- corpus(tx_sen)


# txit$text <- stri_replace_all_fixed(txit$text, zombiefirm_pattern,  zombiefirm_replace, vectorize_all=FALSE)

compound_it <- c( # "aziende zombie","imprese zombie","zombie company","società zombie","organizzazione zombie",
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
  "sistema creditizio",
  "fine pandemia","fine della pandemia", "in piedi", "pioggia di soldi","alto rendimento","alto adige",
  "posto di lavoro","posto lavoro", "crisi economica","crisi finanziaria","crisi del 2008",
  "posti di lavoro","presidente del consiglio","a favore","piccole e medie imprese","piccole imprese",
  "valore aggiunto", "riproduzione riservata",
  # SEN,
  "tenute in vita","emergenza sanitaria","flusso di credito","flusso di sussidi",
  "banca italia","crisi post-covid","crisi economica","crisi finanziaria","crisi pandemica",
  "crisi industriali","crisi politica","crisi di liquidità","crisi del lavoro","crisi finanziaria",
  "crisi del governo","reddito di cittadinanza",
  "situazione finanziaria","area euro", "miliardi di euro", "milioni di euro", "miliardi euro","milioni euro",
  "tempo indeterminato","situazione finanziaria","situazione straordinaria","in grado",
  "debito buono","debito cattivo", "in grado",  "fine della pandemia", "fine anno", "fine delle moratorie",
  "fine della campagna vaccinale", "tenute in vita",
  # 2nd round
  "consiglio di sorveglianza","consiglio di amministrazione","consiglio di stato","consiglio di fabbrica",
  "presidente del consiglio","presidente del parlamento","ex presidente del consiglio",
  "associazione bancaria","associazione a delinquere","cassa depositi","cassa ordinaria","cassa covid",
  "prestiti zombie","posizioni zombie","capitale umano","capitale produttivo","capitale ambientale",
  "capitale immateriale","stato di emergenza","stato di necessità","fare i conti",
  "mercato interno","mercato economico","mercato del lavoro", 
  #3rd round
  "ministro del lavoro","ministero del lavoro","banche italiane","banca d italia", 
  "banca centrale","banche europee","economia di mercato","economia italiana","economia circolare",
  "economia della conoscenza","economia agricola", "attività economiche","attività assicurative",
  "attività produttive", "beni e servizi","servizi di alloggio", "restano in vita",
  "intervento pubblico","tenuto in vita","tenute in vita","ristori pubblici","ristori del governo",
  "banca italia",
  "film dell orrore", "aiuti di stato","stato di salute","stato di default",
  #4th round
  "business intelligence","economia reale","economia finanziaria","economia espansiva",
  "distruzione creativa","creative destruction","fondo perduto", "gruppo ilva",
  #5th round
  "senza *","niente *",
  "azione di governo","distribuzione dei dividendi","distribuzione della ricchezza","distribuzione del capitale",
  "distribuzione di dividendi","distribuzione degli utili",
  "sovranità monetaria","articolo 11","articolo 54","articolo 45","articolo 50",
  "trasporto locale","trasporto aereo",
  "segretario del tesoro","ministro del tesoro","segreteria al tesoro","tesoro pubblico",
  # kn
  "spese clientelari","gruppo intesa sanpaolo","gruppo cgbi","gruppo dei 30","gruppo dei trenta",
  "gruppo roma capitale","gruppo bancario","gruppo marriot bonvoy",
  "compagnia di bandiera","compagnie aeree",
  "zero ore","interesse a zero","beni intermedi","beni pubblici","beni durevoli","beni di consumo",
  "giro di carte",
  "limite di indebitamento",
  "mancanza di risorse","mancanza di liquidità","mancanza di consumi", "mancanza di interventi",
  "ragazzi di terza media","ragazzi del quartiere",
  "presidente del consiglio","consiglio di amministrazione","cosiglio dei ministri"
  
)

bg <- bg[!(bg %in% c("altman evidenzia poi","banca commerciale classis capital società"))]



rem_it <- c("fra","già?","già","oltre","ieri","può","soprattutto","molto","grandi","meno","tutto","tutti",
            "c'é","c'e","state","essere","percentuale", "solo","parte","ne","cosa","fare",
            "c’è","c ’è","c'è","c 'è","d", "avere","milano","torino","bologna","napoli","pisa","quindi",
            "volta","veneto", "termini","fatto quotidiano","la repubblica","corriere della sera", "il giornale",
            "il corrier","corriere","il fatto","https*","fatto_quotidiano","fatto_quotidiano","fatto","poi", "mai","prima",
            "cento","gran_parte","gran* parte", #
            "anni","soltanto","d accordo","d_accordo","circa","però",
            "proprio","allora","fa","ciò","stata","qui","altra","vero","là","dopo","dare", 
            "invece", "senso", "giorni","passato",
            "addirittura", "base",  "poco", "alto","ora","ancora", "considera","posto",
            "presenza", "tanto",  "tutte","sorta","fronte","causa","mentre","quando",
            "fino","secondo","terzo", "mettere", "mesi", "altri","intervista",
            "spiegato", "presentato","tratta","punto","seguito","caso","deciso", #
            "modo","quali","quasi","cioè", "forse","stesso","certo","po","dichiarato","detto","oggi","particolare",
            "altre","certo","almeno","infatti","quarto","pure","può_permettersi","considerata","trova",
            "così", "proviene","stesso_periodo","quest_anno", # "meno_rispetto","così","cosiddette"
            "numero","è_stato", "era_stato", "fosse_stato", "in_stato_di","credo","perciò" ,
            "riproduzione_riservata","©",
            #
            "molte","sostenere","settore","rapporto","chiesto",
            "anno", "fase_attuale","esistono", "informazioni_disponibili", 
            "contano","creando",  "fine","tempo","situazione","momento","dati","grado" , "milioni","miliardi",
            "in_grado","mondo", "dire",
            "nemmeno","grazie",   #
            # 2nd round
            "imprese_intervistate", "ottenuto" , 
            # 3rd round                                      # ,  # "prova","chiaro","imprese","luglio","agosto",# ,"intervento","rischio","terziario", "futuro", "italia"
            "stima","aumentare","roma","articolo","quantità","numeri","entro",
            "richiede","far","percorso",
            "sempre"  ,
            # TRY TO REMOVE: "aumento", "mondo"
            
            # "d_italia","imprese intervistate","ottenuto","città", "mondo",  "attesa","ultima","domanda","dire","tema","buono", "chiuso",
            # "febbraio","giugno","chiusura","gennaio","aprile","novembre", "linea","mese","orrore", "aumentato",
            # SEN,
             "zombiefirms", "zombie",
             "aziende_zombie", "imprese_zombie", "zombie_company", "società_zombie", "organizzazione_zombie",
             "zombiefirms","zombie",
            "troppe","tante","troppe","tante","passo","possono","società"
) 



dfm_df <-  tokens( corpus_sen,
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






kn_sen_it <- textstat_keyness(dfm_group(dfm_subset(dfm_df, datet >= "2020-01-01" ),groups = rating),
                              target = "right")
kn_sen_it$country <- unique(corpus_sen$country)

kn_sen_it <- kn_sen_it %>% filter(feature %in% c( "banca","imprese","gestione", "credito_garantito","chiusura",
                                                 "debito","futuro","crediti",
                                                 "draghi","ilva","alitalia","g30","danneggiare","intervento_pubblico",
                                                 "politica_industriale","sostegno"
))


kn_sen_it_pl <- textplot_keyness(kn_sen_it, 
                 n = 20, margin = 0.1,
                 labelsize = 8, color = c("black","grey")) +
  ylab(kn_sen_it$country) +
  xlab(expression(chi^2)) +
  theme_bw()  +
  theme( axis.title.y = element_text(size = 20),
         axis.text.y = element_blank(),axis.ticks.y = element_blank(),
         axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 15),
         plot.title = element_text(hjust = 0.5),legend.position = "bottom",
         legend.text = element_text(size=20))
ggsave(filename = paste0("images/kn_senNO_",unique(kn_sen_it$country),".jpg"), width = 15, height = 13 ) # 8)



kn_sen_de <- textstat_keyness(dfm_group(dfm_subset(dfm_df, datet >= "2020-01-01" ),groups = rating),
                              target = "right")
kn_sen_de$country <- unique(corpus_sen$country)

kn_sen_de <- kn_sen_de %>% filter(feature %in% c("wirtschaft", "nullzinspolitik","niedrigzinsen","insolvenzanmeldung",
                                                 "insolvenz", "geschäftspartner","creditreform","kapital",
                                                 "überleben","nicht_überlebensfähig","leben_halten" ,
                                                 "kurzarbeit","pflicht","künstlich","bundesbank","regierung"
))


kn_sen_de_pl <- textplot_keyness(kn_sen_de, 
                 n = 20, margin = 0.1,
                 labelsize = 8, color = c("black","grey")) +
  ylab(kn_sen_de$country) +
  xlab(expression(chi^2)) +
  theme_bw()  +
  theme( axis.title.y = element_text(size = 20),
         axis.text.y = element_blank(),axis.ticks.y = element_blank(),
         axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 15),
         plot.title = element_text(hjust = 0.5),legend.position = "bottom",
         legend.text = element_text(size=20))
ggsave(filename = paste0("images/kn_senNO_",unique(kn_sen_de$country),".jpg"), width = 22, height = 15 ) # 8)

ggpubr::ggarrange(kn_sen_de_pl,kn_sen_it_pl,ncol  =1, nrow  = 2, common.legend = T,legend = "bottom")
ggsave(filename = "images/knsen.jpg", width = 22, height = 16)

# 
# zombieterms <- unique(read.xls("zombiefirms.xls",sheet = "ita_lemma")[,3])
# zombieterms <- c(zombieterms,"zombie")
# zombieterms <- paste0("\\b",zombieterms,"\\b",collapse="|")
# 
# tx_sen <- txdf %>% filter(str_detect(text,zombiefirm_replace_it))
# 
# corpus_sen <- corpus(tx_sen)
# 
# e <- a %>% filter(!(a$doc_id %in% b$doc_id))
# 
# 
# dfm_sen <-  tokens( corpus_sen,
#                    remove_punct = TRUE,
#                    remove_symbols = TRUE,
#                    remove_separators = TRUE,
#                    remove_numbers = TRUE,
#                    remove_url = FALSE
# ) %>%
#   tokens_tolower() %>% 
#   # tokens_compound(phrase(c(compound_it,bg))) %>%
#   # tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it)) %>% 
#   tokens_compound(phrase(c(compound_de,bg))) %>%
#   tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de, rem_dekey)) %>% 
#   dfm()
# 
# dfm_df <-  tokens( corpus_df,
#                    remove_punct = TRUE,
#                    remove_symbols = TRUE,
#                    remove_separators = TRUE,
#                    remove_numbers = TRUE,
#                    remove_url = FALSE
# ) %>%
#   tokens_tolower() %>% 
#   tokens_compound(phrase(c(compound_de,bg))) %>%
#   tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de, rem_dekey)) %>% 
#   dfm()

# kn_it <- textstat_keyness(dfm_group(dfm_subset(dfm_sen, datet >= "2020-01-01" ),groups = rating),
#                           target = "right")
# kn_it$country <-  "Italy"



# Qualitative ####

de_results <- read.xls("zombiefirms.xls",sheet = "de_results")[,1]
it_results <- read.xls("zombiefirms.xls",sheet = "it_results")[,1]

results = list()
for (i in it_results) {
  count_tibble = str_count(tx_sen_ita$text,i)
  results[[i]] = count_tibble
}

results

it_i <- bind_rows(results, .id = i)

tx_sen_ita_wc <- cbind(tx_sen_ita,it_i)
save(tx_sen_ita_wc,file="tx_sen_ita_wc.Rdata")

data_long = gather(tx_par_ita_wc[tx_par_ita_wc$datet >= "2020-01-01",], terms, value, 32:50, factor_key=TRUE)
data_longg %>% group_by(doc_id,text) %>% summarise(tot = sum(value))

df <- data_long[data_long$value > 1,]
ggplot(df,aes(x=terms,y=value)) + geom_bar(stat="identity") + 
  facet_wrap(~ df$doc_id) + coord_flip()

plotly::ggplotly(tbterm)

##






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

numberOfCoocs <- 11

dfm_dfcooc <- dfm_subset(dfm_df, datet >= "2020-01-01" & rating == "left")  # subcorpus

coocCounts <- t(dfm_dfcooc) %*% dfm_dfcooc

coocTerm <- "imprese_zombie"            # term to search

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
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs] # !!
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs] # !!

  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}

# resultGraph[sample(nrow(resultGraph), 6), ]

graphNetwork <- graph.data.frame(resultGraph, directed = F)

graphVs <- V(graphNetwork)[degree(graphNetwork) < 1] # Here for more or less clusters, giving min vertex node to appear <<<<<
graphNetwork <- delete.vertices(graphNetwork, graphVs)

V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange')

## Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
# halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
# E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
# E(graphNetwork)$width <- ifelse(E(graphNetwork)$sig > halfMaxSig, 4, 1)



# Disable edges with radius
E(graphNetwork)$curved <- 0
# Size the nodes by their degree of networking
V(graphNetwork)$size <- log(degree(graphNetwork)) * 5


rem_dekey <- read.xls("zombiefirms.xls",sheet = "rem_dekey")[,1]
rem_itkey <- read.xls("zombiefirms.xls",sheet = "rem_itkey")[,1]
connected_key <- resultGraph %>% filter(from == coocTerm )
# connected_key2 <- resultGraph %>% filter(duplicated(resultGraph$to))
# connected_key <- rbind(connected_key, connected_key2)
# 
# keep_it <- read.xls("zombiefirms.xls",sheet = "rem_itkey")[,2]
# keep_it <- c(keep_it,unique(connected_key),coocTerm)


set.seed(124)
tk_kc <- tkplot(graphNetwork)

tk_kc_l <- tkplot.getcoords(tk_kc)
par(mar = c(0,0,1,0))
plot(graphNetwork,
      layout = tk_kc_l,
     #  layout = layout.fruchterman.reingold,  # Force Directed Layout
    # main = paste(unique(dfm_dfcooc$country),",", unique(dfm_dfcooc$rating),",", unique(dfm_dfcooc$covidtp),":", coocTerm ),
     vertex.label.family = "sans",
     vertex.label.cex = ifelse(V(graphNetwork)$name == coocTerm, 1.3, 1),
     vertex.shape = ifelse(V(graphNetwork)$name == coocTerm, 'square', ifelse(V(graphNetwork)$name %in% connected_key$to, 'square', 'circle') ),
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

# vertex.label = ifelse(V(graphNetwork)$name %in% rem_itkey," ",V(graphNetwork)$name ),
 
 #  vertex.label =  V(graphNetwork)$name,
    # vertex.label.cex = 1, # font size of node names
  #   edge.width = ifelse(E(graphNetwork)$sig > halfMaxSig, 4, 1),
edge.width = E(graphNetwork)$sig,

     vertex.color = "grey"
)
title(paste0(unique(dfm_dfcooc$country)," ",unique(dfm_dfcooc$rating)),cex.main=1.5)



