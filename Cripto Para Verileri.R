install.packages("twitteR") #Twitterdan veri çekmek için kullanýlýr.
install.packages("ROAuth") #OAuth 1.0 spesifikasyonu için bir arayüz saðlar.
install.packages("tm") #Metin madenciliði yaparken kullanýlýr.
install.packages("RCurl") #Genel HTTP istekleri oluþturmasýna izin veren iþlevler saðlar
install.packages("magrittr") #%>% komutu ile zincirleme yapar.
install.packages("dplyr") #veri manipülasyon iþlemlerini yapar.
install.packages("tidyverse") #veri bilimi için tasarlanmýþ paketleri içerir. 
install.packages("ggplot2") #verileri görselleþtirmeye yarar.
install.packages("funModeling") #tahmine dayalý modelleme yapar.
install.packages("lubridate") # zaman aralýklý iþlevlerde çalýþmak için kullanýlýr.
install.packages("stringr") #karakter yapýlý veriler için kullanýlýr.
install.packages("tidytext") #Düzenli veri ilkelerini kullanmak birçok metin madenciliði görevini yapar.
install.packages("wordcloud")#Kelime bulutu yapýmý için kullanýlýr.
install.packages("RColorBrewer")#Kelime bulutunun renklendirilmesi için kullanýlýr.
install.packages("tible") #Modern bir veri çerçevesi oluþturma iþleminde kullanýlýr.
install.packages("tidyr") #Verilerin analizine yönelik düzenlemeler için kullanýlýr.
install.packages("ggthemes") #Grafiklerin görünümünü kopyalayan 'ggplot2' temalarý ve ölçekleri saðlar
install.packages("readr") #csv,fwf gibi formattaki tablo yapýsýnda veri içeren dosyalarýn okunmasýný saðlar.
install.packages("readxl") #Excel dosyalarýný içe aktarýr.
install.packages("ggpubr") #Fonksiyonel programlama iþlemlerini gerçekleþtirir.
install.packages("formattable") #'Biçimlendirilebilir' Veri Yapýlarý Oluþturur
install.packages("ggstance") #Yatay 'ggplot2' Bileþenlerini içerir.
install.packages("psych") #Psikolojik, Psikometrik ve Kiþilik Araþtýrmalarý için kullanýlýr.
install.packages("GGally") #ggplot2 uzantýsýdýr.
install.packages("rstatix") #Ýstatisliksel iþlemler için kullanýlýr.
install.packages("sentimentr") #Metin polarite duyarlýlýðýný hesaplar
install.packages("webshot") #Web sayfalarýnýn ekran görüntülerini almak için kullanýlýr.
install.packages("htmlwidgets") # Çeþitli þekillerde iþlenen HTML widget'larý oluþturmak için bir çerçevedir.
install.packages("syuzhet") #Metinden duygu ve duygudan türetilen plot yaylarýný alýr.

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

#####rtlerin kaldýrýlmasý
tweet_clean$text <- ifelse(str_sub(tweet_clean$text,1,2) == "RT",
                           substring(tweet_clean$text,3),
                           tweet_clean$text)
view(tweet_clean$text)
#URL linklerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "http[^[:space:]]*", "")


#Hashtag "#" ve "@" iþaretlerinin kaldýrýlmasý
tweet_clean$text <- str_replace_all(tweet_clean$text, "#\\S+", "")
tweet_clean$text <- str_replace_all(tweet_clean$text, "@\\S+", "")

#Noktalama iþaretlerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[[:punct:][:blank:]]+", " ")

#Tüm harflerin küçük harfe dönüþtürülmesi
tweet_clean$text  <- str_to_lower(tweet_clean$text, "tr")

#Rakamlarýn temizlenmesi
tweet_clean$text <- removeNumbers(tweet_clean$text)
#stopwordsler
liste=c(stopwords("en"))

#Gereksi tekrarlar ve baðlaçlarýn temizlenmesi
tweet_clean$text = removeWords(tweet_clean$text,liste)

#ASCII formatýna uymayan karakterlerin temizlenmesi
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
  ggtitle("Tweetlerde en çok kullanýlan kelimeler")


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

#Gereksiz tekrarlar ve baðlaçlarýn temizlenmesi
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
  labs(y = "Skor", x = "Kelimelerin Frekansý") +
  theme_igray()+
  labs() +
  theme(plot.caption = element_text(hjust = 0, face = "italic"))

stat.desc(polarite$sentiment, basic=T) %>% pander()



