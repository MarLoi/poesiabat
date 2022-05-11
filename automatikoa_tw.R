library(rtweet)
library(tidyverse)


# # whatever name you assigned to your created app
# appname <- "poesiabat"
# 
# ## api key (example below is not a real key)
# key <- "2C2NmWG7kie43GW8JzA0fckjM"
# 
# ## api secret (example below is not a real key)
# secret <- "GDxSOgk1pdV7Zk9ykPObdRK91bAmtpyppmMxgouYNq8Zq3pnPG"
# 
# twitter_token <- create_token(
#   app = appname,
#   consumer_key = key,
#   consumer_secret = secret,
#   access_token = "1524375667365888000-ZPnskvY3ZsKJYUNwy4uIoZDQkc3eeU",
#   access_secret = "AqzctjmFaRvPq1aJI3bpGGuXGX4lwvcZplGRX0HbSOsAD")


# whatever name you assigned to your created app
appname <- "sareak probatzen"

## api key (example below is not a real key)
key <- "1hFVQmG7WrtRGekqAaUz5yAWj"

## api secret (example below is not a real key)
secret <- "dL25UesAQHuWfYpKDkgk05IdN7ZUwFbImUNN4qbw6vfbdnPZeD"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = "2873995706-hJb5EmtpUJLOeR2BrG2F1WKEEpasynEetOIcHOE",
  access_secret = "RnbguBacjdDgCk5ZOZymfMzRvccvdFUYFE3QlXGa5QuLA")


rstats_tweets <- search_tweets(q = "#poesiabat", n = 10)
rstats_tweets$created_at = as.POSIXct(rstats_tweets$created_at,format="%m/%d/%Y %H:%M:%S", tz = "Europe/Paris")
orain = as.POSIXct(Sys.time(),format="%m/%d/%Y %H:%M:%S")

rstats_tweets_filt = rstats_tweets %>% filter(created_at > (Sys.time()-60000) )

for(i in 1:nrow(rstats_tweets_filt)){

  erabiltzailea = paste('@',rstats_tweets_filt[i,'screen_name'], sep="")
  hitza = str_extract(rstats_tweets_filt[i,'text'], '\\[(.*?)\\]')
  hitza = str_sub(hitza, 2, nchar(hitza)-1)
  
  
  # PROGRAMA ----------------------------------------------------------------
  
  
  load("datuak.RData")
  
  hitza_poema = function(hitza){
  
    aurkituak = berbaka %>% filter(grepl(hitza, word),ignore.case=TRUE)
    
    if(nrow(aurkituak)>0){
    
      ausaz = sample(nrow(aurkituak),1)
      egilea_aurki = aurkituak[ausaz,3]
      poema_zenbakia_aurki = aurkituak[ausaz,1]
      titulua_aurki = aurkituak[ausaz,4]
      
      aukeratua = zerrenda %>% filter (egilea == egilea_aurki, poema_zenbakia == poema_zenbakia_aurki, titulua == titulua_aurki)
      
      izena = aukeratua$poema_izena
      
      if(nchar(izena)<3){
      izena = sub("*([^\n]*\n){2}([^\n]+).*", "\\1", aukeratua$poema)
      #print("1")
      }
      
      if(nchar(izena)<3){
      izena = sub("*([^\n]*\n){2}([^\n]+).*", "\\2", aukeratua$poema)
      #print("2")
      }
      
      
      liburua = ifelse(titulua_aurki == "Poesia kaiera", " antologian ", "' liburuko ")
      
      
      emaitza = paste(erabiltzailea, ' | ',
                      nrow(aurkituak), " poema baino gehiago daude '", hitza, "' hitzarekin. ",
                      "Adibidez ", egilea_aurki, "-ren '", titulua_aurki, liburua, "'", izena, "'", " poeman aurki dezakezu.",
                      sep="")
      
      return(emaitza)
    
    } else {
      
      emaitza = paste(erabiltzailea, ' | ', 'Ez da poesiarik aurkitu zuk emandako hitzarekin.')
      
      return(emaitza)
    }
  
  }
  
  emaitza = hitza_poema(hitza)
  
  
  
  # TWEETA ------------------------------------------------------------------
  
  
  #post_tweet('emaitza')
  print(emaitza)


}
