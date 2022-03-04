movies = read.xlsx("movies_2015_2021.xlsx", sheet = 1)
movies$date = movies$date %>% trimws(.,which = c("both","left","right"), whitespace = "['Release Date:']")
split = function(x){
  x = strsplit(x, " ")[[1]][1:3] %>% paste(collapse=" ")
  return(x)
}
movies$date = movies$date %>% lapply(., FUN = split)
extract = function(x){
  x = regmatches(x, gregexpr(pattern = "2...",text = x))
  return(x[[1]][1])
}
movies$year = movies$year %>% lapply(., FUN = extract)
extract = function(x){
  x = regmatches(x, gregexpr(pattern = ":.* ",text = x))[[1]][1]
  x = trimws(x, which = c("both","left","right"), whitespace = "[':']")
  if(str_contains(x, "INR")){
    x = trimws(x, which = c("both","left","right"), whitespace = "['INR'[:space:]]")
    x = strsplit(x,",")[[1]] %>% paste(collapse = "")
    x = as.numeric(x)/10000000
  }
  else{
    x = trimws(x, which = c("both","left","right"), whitespace = "['\n'[:space:]]")
    x = trimws(x, which = c("both","left","right"), whitespace = "['$'[:space:]]")
    x = strsplit(x,",")[[1]] %>% paste(collapse = "")
    x = as.numeric(as.integer(x) * 72)/10000000
  }
  
  return(x)
}
movies$budget = movies$budget %>% lapply(., FUN = extract)

extract = function(x){
  x = regmatches(x, gregexpr(pattern = ":.*",text = x))[[1]][1]
  x = trimws(x, which = c("both","left","right"), whitespace = "[': ']")
  if(str_contains(x, "INR")){
    x = trimws(x, which = c("both","left","right"), whitespace = "['INR'[:space:]]")
    x = strsplit(x,",")[[1]] %>% paste(collapse = "")
    x = as.numeric(x)/10000000
  }
  else{
    x = trimws(x, which = c("both","left","right"), whitespace = "['\n'[:space:]]")
    x = trimws(x, which = c("both","left","right"), whitespace = "['$'[:space:]]")
    x = strsplit(x,",")[[1]] %>% paste(collapse = "")
    x = as.numeric(as.integer(x) * 72)/10000000
  }
  
  return(x)
}
movies$box_office = movies$box_office %>% lapply(., FUN = extract)


####### how many films actor did #############

salman = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = movies$cast[movies$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"salman khan",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  salman = rbind(salman, data.frame(year = y, actor="Salman Khan", releases = sum))
}

sharukh = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = movies$cast[movies$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"Shah rukh khan",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  sharukh = rbind(sharukh, data.frame(year = y, actor="Shah rukh Khan", releases = sum))
}

akshay = c()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = movies$cast[movies$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"akshay kumar",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  akshay = rbind(akshay,data.frame(year = y, actor="Akshay Kumar", releases = sum))
}

deepika = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = movies$cast[movies$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"Deepika padukone",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  deepika = rbind(deepika, data.frame(year = y, actor="Deepika Padukone", releases = sum))
}

anushka = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = movies$cast[movies$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"anushka sharma",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  anushka = rbind(anushka, data.frame(year = y, actor="Anushka Sharma", releases = sum))
}

alia = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = movies$cast[movies$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"alia bhatt",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  alia = rbind(alia, data.frame(year = y, actor="Alia Bhatt", releases = sum))
}


########### total movies #######
releases = c()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = movies$year[movies$year == y]
  releases = append(releases, length(a))
}
year = c("2015", "2016", "2017", "2018", "2019", "2020")
x = c(1,2,3,4,5,6)
df = data.frame(x = x, year=year, releases = releases)
predict = lm(releases~x, data = df[1:5,])
predict = round(6*predict$coefficients['x'] + predict$coefficients['(Intercept)'])


library(ggplot2)
library(hrbrthemes)
df1 = data.frame(x=c(5,6), year = c("2019", "2020"),releases=c(df$releases[5], predict))
movies_trend = ggplot(df, mapping = aes(x=year, y=releases, group=2)) + 
  geom_vline(xintercept="2019", color="orange", size=.9) +
  geom_text(aes(x="2019", y=300, label = "Start of COVID-19"), angle = 90, vjust = 1.1, colour = "red")+
  geom_line(color="#e60000", size=2, alpha=0.9, linetype=1) +
  geom_line(df1,mapping = aes(x=year, y=releases, group=2), color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  geom_point(aes(x=df1$year[2], y=df1$releases[2])) +
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.text.y = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.title = element_text(face = "bold", color = "black", 
                                  size = 15, angle = 0),
        axis.title.x = element_text(face = "bold", color = "black", 
                                    size = 12),
        axis.title.y = element_text(face = "bold", color = "black", 
                                    size = 12),
        title = element_text(face = "bold", color = "black", 
                             size = 12, angle = 0)) + 
  annotate(geom="point", x=df$year[6], y=df$releases[6], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[6], y=df$releases[6], 
           label=paste("\n    Actual releases-",df$releases[6])) +
  annotate(geom="point", x=df$year[5], y=df$releases[5], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[5], y=df$releases[5], 
           label=paste("\n           ",df$releases[5])) +
  annotate(geom="point", x=df$year[4], y=df$releases[4], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[4], y=df$releases[4], 
           label=paste("\n    ",df$releases[4])) +
  annotate(geom="point", x=df$year[3], y=df$releases[3], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[3], y=df$releases[3], 
           label=paste("\n    ",df$releases[3])) +
  annotate(geom="point", x=df$year[2], y=df$releases[2], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[2], y=df$releases[2], 
           label=paste("\n    ",df$releases[2])) +
  annotate(geom="point", x=df$year[1], y=df$releases[1], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[1], y=df$releases[1], 
           label=paste("        ",df$releases[1])) +
  annotate(geom="point", x=df1$year[2], y=df1$releases[2], size=2, shape=21, fill="black") +
  annotate(geom="point", x=df1$year[2], y=df1$releases[2], size=5, shape=21, fill="transparent") +
  annotate(geom="text", x=df1$year[2], y=df1$releases[2], 
           label=paste("\n\n\n        Predicted \n             releases-",df1$releases[2])) +
  labs (title = "Number of films released",
        y="Count of films ",
        x="Year",
        subtitle="(2015-2020)"
  )

ggsave(movies_trend, filename = "Movies_trend.png", width = 12, height = 6)
################# number of films by actors ##########
df = rbind(anushka,alia,deepika,salman,sharukh,akshay)

levels(df$year) = c("2015","2016","2017","2018","2019","2020")
actor_releases = ggplot(df, mapping = aes(x=year, y=releases, fill=actor)) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~actor) +
  theme_ipsum() +
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 45),
        axis.text.y = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.title = element_text(face = "bold", color = "black", 
                                  size = 11, angle = 0),
        title = element_text(face = "bold", color = "black", 
                             size = 12, angle = 0)) +
  labs (title = "Release of top actors",
        y="Count of films ",
        x="",
        subtitle="(2015-2020)"
  )
ggsave(actor_releases, filename = "Actor_releases.png", width = 12, height = 6)
############### movies delayed ###############
library(tidyverse)
df = filter(movies, (year=="2019"))
df = filter(df,df$date %>% sapply(.,FUN=function(x){
  if(!str_contains(x,"2019", ignore.case = TRUE)){
    if(str_contains(x,"na",ignore.case = TRUE)){
      return(FALSE)
    }
    return(TRUE)
  }
  return(FALSE)
}))
films_not_released = dim(df)[1]

total = 0
ott = 0
for(i in 1:23){
  x = df$distributers[i]
  if(!is.na(x)){
    total = total + 1
    if(str_contains(x,"hotstar",ignore.case = TRUE)){
      ott = ott + 1
    }
    if(str_contains(x,"jio",ignore.case = TRUE)){
      ott = ott + 1
    }
    if(str_contains(x,"netflix",ignore.case = TRUE)){
      ott = ott + 1
    }
    if(str_contains(x,"prime",ignore.case = TRUE)){
      ott = ott + 1
    }
    if(str_contains(x,"zee",ignore.case = TRUE)){
      ott = ott + 1
    }
    if(str_contains(x,"sonyliv",ignore.case = TRUE)){
      ott = ott + 1
    }
  }
}

percentage_of_ott_release = ott/total*100

library(ggplot2)
df_2019 = filter(movies, (year=="2019"))
df = data.frame(released=c("Released", "Not released"), value = c(dim(df_2019)[1]-films_not_released, films_not_released))
movie_2019_status = ggplot(df, aes(x="", y=value, fill=released)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + geom_text(aes(label = paste0(value)), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#e60000", "#3fe04c")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Status of movies in 2019")+
   theme_bw()+ theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "black"))
ggsave(movie_2019_status, filename="movie_2019_status.png", width = 8, height = 8)
df = data.frame(released = c("Released on OTT", "Released in theatre"), value = c((ott/total)*100, 100-(ott/total)*100))
unreleased_status = ggplot(df, aes(x="", y=value, fill=released)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value,3), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#e60000", "#3fe04c")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Platform of delayed movies")+
  theme_bw()+ theme(axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    plot.title = element_text(hjust = 0.5, color = "black"))

ggsave(unreleased_status, filename = "Unreleased_of_2019.png", width = 8, height = 8)
############### webseries data prep######################
webseries_2015_2019 = read.xlsx("webseries_2015_2019.xlsx", sheet = 1)
webseries_2019_2021 = read.xlsx("webseries_2019_2021.xlsx", sheet = 1)
webseries = rbind(webseries_2015_2019, webseries_2019_2021)

webseries$date = webseries$date %>% trimws(.,which = c("both","left","right"), whitespace = "['Release Date:']")
split = function(x){
  x = strsplit(x, " ")[[1]][1:3] %>% paste(collapse=" ")
  return(x)
}
webseries$date = webseries$date %>% lapply(., FUN = split)

extract = function(x){
  x = regmatches(x, gregexpr(pattern = "2...",text = x))
  return(x[[1]][1])
}
webseries$year = webseries$year %>% lapply(., FUN = extract)

releases = c()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$year[webseries$year == y]
  releases = append(releases, length(a))
}
x = c(1,2,3,4,5,6)
year = c("2015", "2016", "2017", "2018", "2019", "2020")
df = data.frame(x = x, year=year, releases = releases)
predict = lm(releases~x, data = df[1:5,])
predict = round(6*predict$coefficients['x'] + predict$coefficients['(Intercept)'])


library(ggplot2)
library(hrbrthemes)
df1 = data.frame(x=c(5,6), year=c("2019","2020"), releases=c(df$releases[5], predict))
webseries_trend = ggplot(df, mapping = aes(x=year, y=releases, group = 2)) + geom_line(color="#e60000", size=2, alpha=0.9, linetype=1) +
  geom_line(df1, mapping = aes(x=year, y=releases, group=2), color="#69a3b2", size=2, alpha=0.9, linetype=2) +
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.text.y = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.title = element_text(face = "bold", color = "black", 
                                  size = 15, angle = 0),
        axis.title.x = element_text(face = "bold", color = "black", 
                                    size = 12),
        axis.title.y = element_text(face = "bold", color = "black", 
                                    size = 12),
        title = element_text(face = "bold", color = "black", 
                             size = 12, angle = 0)) + 
  geom_vline(xintercept="2019", color="orange", size=.9) +
  geom_text(aes(x="2019", y=210, label = "Start of COVID-19"), angle = 90, vjust = 1.1, colour="red")+
  geom_vline(xintercept="2016", color="green", size=.9) +
  geom_text(aes(x="2016", y=315, label = "Jio Telecomm publicly available (5th sept 2016)"), angle = 90, vjust = 1.1, colour="red")+
  annotate(geom="point", x=df$year[6], y=df$releases[6], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[6], y=df$releases[6], 
           label=paste("\n    Actual releases-",df$releases[6])) +
  annotate(geom="point", x=df$year[5], y=df$releases[5], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[5], y=df$releases[5], 
           label=paste("\n    ",df$releases[5])) +
  annotate(geom="point", x=df$year[4], y=df$releases[4], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[4], y=df$releases[4], 
           label=paste("\n    ",df$releases[4])) +
  annotate(geom="point", x=df$year[3], y=df$releases[3], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[3], y=df$releases[3], 
           label=paste("\n    ",df$releases[3])) +
  annotate(geom="point", x=df$year[2], y=df$releases[2], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[2], y=df$releases[2], 
           label=paste("\n    ",df$releases[2])) +
  annotate(geom="point", x=df$year[1], y=df$releases[1], size=3, shape=21, fill="black") +
  annotate(geom="text", x=df$year[1], y=df$releases[1], 
           label=paste("        ",df$releases[1])) +
  annotate(geom="point", x=df1$year[2], y=df1$releases[2], size=2, shape=21, fill="black") +
  annotate(geom="point", x=df1$year[2], y=df1$releases[2], size=5, shape=21, fill="transparent") +
  annotate(geom="text", x=df1$year[2], y=df1$releases[2], 
           label=paste("\n\n\n        Predicted \n             releases-",df1$releases[2])) +
  labs (title = "Number of Webseries released",
        y="Count of webseries ",
        x="Year",
        subtitle="(2015-2020)"
  )
ggsave(webseries_trend, filename = "Webseries_trend.png", width = 12, height = 6)



############### ott count ##################
netflix = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"netflix",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  netflix = rbind(netflix, data.frame(year = y, ott="Netflix", releases = sum))
}
lab = paste("Netflix\t(+",mean(netflix[5:6,3])-mean(netflix[1:4,3]),")",sep="")
netflix = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab,lab), average=c(mean(netflix[1:4,3]), mean(netflix[5:6,3])))

hotstar = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"hotstar",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  hotstar = rbind(hotstar, data.frame(year = y, ott="Hotstar", releases = sum))
}
lab = paste("Hotstar\t(+",mean(hotstar[5:6,3])-mean(hotstar[1:4,3]),")",sep="")
hotstar = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab,lab), average=c(mean(hotstar[1:4,3]), mean(hotstar[5:6,3])))


prime = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"prime",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  prime = rbind(prime, data.frame(year = y, ott="Amazon Prime", releases = sum))
}
lab = paste("Amazon Prime\t(+",mean(prime[5:6,3])-mean(prime[1:4,3]),")",sep="")
prime = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab,lab), average=c(mean(prime[1:4,3]), mean(prime[5:6,3])))

jio = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"jio",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  jio = rbind(jio, data.frame(year = y, ott="Jio Studio", releases = sum))
}
lab = paste("Jio Studio\t(+",mean(jio[5:6,3])-mean(jio[1:4,3]),")",sep="")
jio = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab,lab), average=c(mean(jio[1:4,3]), mean(jio[5:6,3])))

alt = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"alt",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  alt = rbind(alt, data.frame(year = y, ott="ALT Balaji", releases = sum))
}
lab = paste("ALT Balaji\t(+",mean(alt[5:6,3])-mean(alt[1:4,3]),")",sep="")
alt = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab,lab), average=c(mean(alt[1:4,3]), mean(alt[5:6,3])))

zee = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"zee",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  zee = rbind(zee, data.frame(year = y, ott="ZEE 5", releases = sum))
}
lab = paste("ZEE 5\t(+",mean(zee[5:6,3])-mean(zee[1:4,3]),")",sep="")
zee = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab,lab), average=c(mean(zee[1:4,3]), mean(zee[5:6,3])))

sony = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"sonyliv",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  sony = rbind(sony, data.frame(year = y, ott="Sony Liv", releases = sum))
}
lab = paste("Sony Liv\t(+",mean(sony[5:6,3])-mean(sony[1:4,3]),")",sep="")
sony = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab,lab), average=c(mean(sony[1:4,3]), mean(sony[5:6,3])))

ullu = data.frame()
for (y in c("2015", "2016", "2017", "2018", "2019", "2020")){
  a = webseries$distributers[webseries$year==y] %>% sapply(.,FUN = function(x){
    bool = str_contains(x,"ullu",ignore.case = TRUE)
    return(bool)
  })
  sum = table(a)["TRUE"]
  if(is.na(sum)) sum = 0
  ullu = rbind(ullu, data.frame(year = y, ott="ULLU", releases = sum))
}
lab = paste("ULLU\t(+",mean(ullu[5:6,3])-mean(ullu[1:4,3]),")",sep="")
ullu = data.frame(x=c("Pre-lockdown", "Post-lockdown"), ott=c(lab, lab), average=c(mean(ullu[1:4,3]), mean(ullu[5:6,3])))

df = rbind(netflix, prime, jio, ullu, hotstar, zee, alt)

levels(df$ott) = c("Netflix","Amazon Prime","Jio Studio","ULLU","ZEE 5", "ALT Balaji")
ott_comparison = ggplot(df, mapping = aes(x=reorder(x,average), y=average, label=average)) +
  geom_bar(stat = "identity", position = "identity", fill="#e60000")+
  geom_text(aes(label = average), vjust= -0.2)+
  ylim(0,35)+
  theme_ipsum() +
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.text.y = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.title = element_text(face = "bold", color = "black", 
                                  size = 11, angle = 0),
        title = element_text(face = "bold", color = "black", 
                             size = 12, angle = 0)
        ) +
  facet_wrap(~ott) +
  labs (title = "OTT comparison(Average count of releases)",
        y=" Average count of Webseries released ",
        x="",
        subtitle="Pre-lockdown vs Post-lockdown"
  )
ggsave(ott_comparison, filename = "OTT_comparison.png", width = 12, height = 6)

############ JIO inception ############
#https://borgenproject.org/internet-access-india/

df = data.frame(x=c("Before inception of Jio", "After inception of Jio"), usage=c(0.7,11))
jio_inception = ggplot(df, aes(x=reorder(x,usage), y=usage, group=1))+
  geom_line(color="dark green", size=2, alpha=0.9, linetype=1)+
  annotate("point",x="Before inception of Jio",y=0.7,size=4,shape=21,fill="black") +
  annotate("text", x="Before inception of Jio",y=0.7,label="                   700 MB")+
  annotate("point",x="After inception of Jio",y=11,size=4,shape=21,fill="black")+ 
  annotate("text", x="After inception of Jio",y=11,label="\n            11 GB")+
  theme_ipsum() +
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 12, angle = 0),
        axis.text.y = element_text(face = "bold", color = "black", 
                                   size = 12, angle = 0),
        axis.title = element_text(face = "bold", color = "black", 
                                  size = 20, angle = 0),
        title = element_text(face = "bold", color = "black", 
                             size = 12, angle = 0),
        axis.title.y = element_text(face = "bold", color = "black", 
                                    size = 15),
        panel.border = element_blank(),
        axis.line= element_line(colour="black")
  ) +
  labs (title = "Rise in internet usage since inception of JIO",
        y=" Internet Usage (GB)                                    ",
        x="",
        caption = "Source : https://borgenproject.org/internet-access-india/"
  )  
ggsave(jio_inception, filename = "Jio_inception.png", width = 8, height = 8)


######## jio price #######
#https://restofworld.org/2020/how-india-mobile-data-became-worlds-cheapest/#:~:text=At%20one%20point%20in%202016,approximately%20225%20rupees%20(%243).

df = data.frame(x=c("Post-Jio Launch", "Pre-Jio Launch"), price=c(18.5,225))
jio_prices = ggplot(df, aes(x=reorder(x,desc(price)), y=price, group=1))+
  geom_line(color="red", size=2, alpha=0.9, linetype=1)+
  annotate("point",x="Pre-Jio Launch",y=225,size=4,shape=21,fill="black") +
  annotate("text", x="Pre-Jio Launch",y=225,label="                          Rs. 225/GB")+
  annotate("point",x="Post-Jio Launch",y=18.5,size=4,shape=21,fill="black")+ 
  annotate("text", x="Post-Jio Launch",y=18.5,label="\n            Rs.18.5/GB")+
  theme_ipsum() +
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 12, angle = 0),
        axis.text.y = element_text(face = "bold", color = "black", 
                                   size = 12, angle = 0),
        axis.title = element_text(face = "bold", color = "black", 
                                  size = 20, angle = 0),
        title = element_text(face = "bold", color = "black", 
                             size = 12, angle = 0),
        axis.title.y = element_text(face = "bold", color = "black", 
                                    size = 15),
        panel.border = element_blank(),
        axis.line= element_line(colour="black")
  ) +
  labs (title = "Decreases in internet rates after inception of JIO",
        y=" Price (Rs.)                                    ",
        x="",
        caption = "Source : (Deck and Deck, 2021)"
  )  
ggsave(jio_prices, filename = "Jio_prices.png", width = 8, height = 8)

#########################################

