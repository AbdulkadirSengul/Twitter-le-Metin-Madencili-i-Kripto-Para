install.packages("twitteR") #Twitterdan veri �ekmek i�in kullan�l�r.
install.packages("ROAuth") #OAuth 1.0 spesifikasyonu i�in bir aray�z sa�lar.
install.packages("tm") #Metin madencili�i yaparken kullan�l�r.
install.packages("RCurl") #Genel HTTP istekleri olu�turmas�na izin veren i�levler sa�lar
install.packages("magrittr") #%>% komutu ile zincirleme yapar.
install.packages("dplyr") #veri manip�lasyon i�lemlerini yapar.
install.packages("tidyverse") #veri bilimi i�in tasarlanm�� paketleri i�erir. 
install.packages("ggplot2") #verileri g�rselle�tirmeye yarar.
install.packages("funModeling") #tahmine dayal� modelleme yapar.
install.packages("lubridate") # zaman aral�kl� i�levlerde �al��mak i�in kullan�l�r.
install.packages("stringr") #karakter yap�l� veriler i�in kullan�l�r.
install.packages("tidytext") #D�zenli veri ilkelerini kullanmak bir�ok metin madencili�i g�revini yapar.
install.packages("wordcloud")#Kelime bulutu yap�m� i�in kullan�l�r.
install.packages("RColorBrewer")#Kelime bulutunun renklendirilmesi i�in kullan�l�r.
install.packages("tible") #Modern bir veri �er�evesi olu�turma i�leminde kullan�l�r.
install.packages("tidyr") #Verilerin analizine y�nelik d�zenlemeler i�in kullan�l�r.
install.packages("ggthemes") #Grafiklerin g�r�n�m�n� kopyalayan 'ggplot2' temalar� ve �l�ekleri sa�lar
install.packages("readr") #csv,fwf gibi formattaki tablo yap�s�nda veri i�eren dosyalar�n okunmas�n� sa�lar.
install.packages("readxl") #Excel dosyalar�n� i�e aktar�r.
install.packages("ggpubr") #Fonksiyonel programlama i�lemlerini ger�ekle�tirir.
install.packages("formattable") #'Bi�imlendirilebilir' Veri Yap�lar� Olu�turur
install.packages("ggstance") #Yatay 'ggplot2' Bile�enlerini i�erir.
install.packages("psych") #Psikolojik, Psikometrik ve Ki�ilik Ara�t�rmalar� i�in kullan�l�r.
install.packages("GGally") #ggplot2 uzant�s�d�r.
install.packages("rstatix") #�statisliksel i�lemler i�in kullan�l�r.
install.packages("sentimentr") #Metin polarite duyarl�l���n� hesaplar
install.packages("webshot") #Web sayfalar�n�n ekran g�r�nt�lerini almak i�in kullan�l�r.
install.packages("htmlwidgets") # �e�itli �ekillerde i�lenen HTML widget'lar� olu�turmak i�in bir �er�evedir.
install.packages("syuzhet") #Metinden duygu ve duygudan t�retilen plot yaylar�n� al�r.

library(lubridate)
library(twitteR)
library(tm)
library(ROAuth)
library(RCurl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(funModeling)
library(stringi)
library(stringr)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tible)
library(tidyr)
library(ggthemes)
library(readr)
library(readxl)
library(ggpubr)
library(formattable)
library(ggstance)
library(pastecs)
library(psych)
library(GGally)
library(pander)
library(rstatix)
library(sentimentr)
library(webshot)
library(wordcloud)
library(wordcloud2)
library(htmlwidgets)
library(magrittr)
library(dplyr) 

api_key = "MQ8ndXENKMNh1vClaxtpc2yNV"
api_secret = "QuFM1XLoUfxoQIPIM7mwsGKC0dbkOAPdROC855pR9QhorT6f7X"
acces_token = "1368242770591244288-X1NGouvszgHHlSXXfWIty89M3Oztzb"
acces_token_secret = "6CKCYUKwd6d9ibH6r99nurKPg9lLyZuoWxeEXnVqcDrqZ"
setup_twitter_oauth(api_key,api_secret,acces_token,acces_token_secret)


tweets <- searchTwitteR("#kriptopara", n=10000,  locale = "tr_TR",lang="tr")

tweets.df <- twListToDF(tweets)
tweet_clean <- tweets.df

tweet_clean$text <- stri_enc_toutf8(tweet_clean$text)

view(tweet_clean)
view(tweet_clean$text)

#####rtlerin kald�r�lmas�
tweet_clean$text <- ifelse(str_sub(tweet_clean$text,1,2) == "RT",
                           substring(tweet_clean$text,3),
                           tweet_clean$text)
view(tweet_clean$text)
#URL linklerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "http[^[:space:]]*", "")


#Hashtag "#" ve "@" i�aretlerinin kald�r�lmas�
tweet_clean$text <- str_replace_all(tweet_clean$text, "#\\S+", "")
tweet_clean$text <- str_replace_all(tweet_clean$text, "@\\S+", "")

#Noktalama i�aretlerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[[:punct:][:blank:]]+", " ")

#T�m harflerin k���k harfe d�n��t�r�lmesi
tweet_clean$text  <- str_to_lower(tweet_clean$text, "tr")

#Rakamlar�n temizlenmesi
tweet_clean$text <- removeNumbers(tweet_clean$text)
#stopwordsler
liste=c(stopwords("en"))

#Gereksi tekrarlar ve ba�la�lar�n temizlenmesi
tweet_clean$text = removeWords(tweet_clean$text,liste)

#ASCII format�na uymayan karakterlerin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[<].*[>]", "")
tweet_clean$text <- gsub("\uFFFD", "", tweet_clean$text, fixed =  TRUE)
tweet_clean$text <- gsub("\n", "", tweet_clean$text, fixed =  TRUE)
view(tweet_clean)

#Alfabetik olmayan karakterlerin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[^[:alnum:]]"," " )


cryptopara$Tweets <- cryptopara$Tweets %>% select(text) %>% 
  mutate(linenumber = row_number()) %>% unnest_tokens(word, text)



head(cryptopara$Tweets)

cryptopara$Tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_minimal() +
  ggtitle("Tweetlerde en �ok kullan�lan kelimeler")


library(wordcloud)

tidy_tweets %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

write.csv(tweet_clean$text,file="Crypto.csv")

##############verileri toplama ve toplam analizi yapma
cryptopara = read.table(file.choose(),header=T,sep=";",encoding = "UTF-8")
str(cryptopara)
summary(cryptopara)

#Gereksiz kelimeler
#stopwordsler
liste=c(stopwords("en"))

#Gereksiz tekrarlar ve ba�la�lar�n temizlenmesi
cryptopara$Tweets = removeWords(cryptopara$Tweets,liste)

#kelime bulutu
wordcloud(cryptopara$Tweets ,min.freq=10,scale=c(6,2,14),max.words=100,
          random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))

cryptopara$Tweets<-cryptopara$Tweets %>% as_tibble()%>%rename(word=value)

#polerite
polarite<-sentiment(cryptopara$Tweets$word)

tablo<-cbind(cryptopara$Tweets$word, polarite[,c(3,4)])

ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="blue")+
  geom_hline(yintercept = mean(tablo$sentiment), color="red", size=1)+
  labs(y = "Skor", x = "Kelimelerin Frekans�") +
  theme_igray()+
  labs() +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))

stat.desc(polarite$sentiment, basic=T) %>% pander()



