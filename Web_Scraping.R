library(dplyr)
library(rvest)
library(ggplot2)

# Scraping 2020 movie list
url = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2020"
data = read_html(url)

data_high_2020 = data %>% html_nodes(".wikitable") %>% .[[1]] %>% html_table(fill=TRUE)
table_high_2020 = as.data.frame(data_high_2020)

data_jan_mar_2020 = data %>% html_nodes(".wikitable") %>% .[[2]] %>% html_table(fill=TRUE)
table_jan_mar_2020 = as.data.frame(data_jan_mar_2020)

data_apr_jun_2020 = data %>% html_nodes(".wikitable") %>% .[[3]] %>% html_table(fill=TRUE)
table_apr_jun_2020 = as.data.frame(data_apr_jun_2020)

data_oct_dec_2020 = data %>% html_nodes(".wikitable") %>% .[[5]] %>% html_table(fill=TRUE)
table_oct_dec_2020 = as.data.frame(data_oct_dec_2020)

library(htmltab)
data_jul_sept_2020 =  url %>%
  htmltab(5, rm_nodata_cols = F)
table_jul_sept_2020 = as.data.frame(data_jul_sept_2020)

table_jan_mar_2020 =  table_jan_mar_2020[,!(colnames(table_jan_mar_2020) %in% c("Ref."))]
table_apr_jun_2020 =  table_apr_jun_2020[,!(colnames(table_apr_jun_2020) %in% c("Ref."))]
table_jul_sept_2020 =  table_jul_sept_2020[,!(colnames(table_jul_sept_2020) %in% c("Ref."))]
table_oct_dec_2020 =  table_oct_dec_2020[,!(colnames(table_oct_dec_2020) %in% c("Ref."))]
table_high_2020 = table_high_2020[,!colnames(table_high_2020) %in% c("Ref.")]

df_2020 = rbind(rbind(rbind(table_jan_mar_2020, table_apr_jun_2020), table_jul_sept_2020), table_oct_dec_2020)
require(openxlsx)
list_of_datasets <- list("movies_list" = df_2020, "top_movies" = table_high_2020)
write.xlsx(list_of_datasets, file = "movies_2020.xlsx")




#Scraping movies of 2019
url = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2019"
data = read_html(url)

data_high_2019 = data %>% html_nodes(".wikitable") %>% .[[2]] %>% html_table(fill=TRUE)
table_high_2019 = as.data.frame(data_high_2019)

data_jan_mar_2019 = data %>% html_nodes(".wikitable") %>% .[[3]] %>% html_table(fill=TRUE)
table_jan_mar_2019 = as.data.frame(data_jan_mar_2019)

data_apr_jun_2019 = url %>%
  htmltab(6, rm_nodata_cols = F)
table_apr_jun_2019 = as.data.frame(data_apr_jun_2019)

library(htmltab)
data_jul_sept_2019 =  url %>%
  htmltab(7, rm_nodata_cols = F)
table_jul_sept_2019 = as.data.frame(data_jul_sept_2019)

data_oct_dec_2019 = data %>% html_nodes(".wikitable") %>% .[[6]] %>% html_table(fill=TRUE)
table_oct_dec_2019 = as.data.frame(data_oct_dec_2019)


table_jan_mar_2019 =  table_jan_mar_2019[,!(colnames(table_jan_mar_2019) %in% c("Ref."))]
table_apr_jun_2019 =  table_apr_jun_2019[,!(colnames(table_apr_jun_2019) %in% c("Ref."))]
table_jul_sept_2019 =  table_jul_sept_2019[,!(colnames(table_jul_sept_2019) %in% c("Ref."))]
table_oct_dec_2019 =  table_oct_dec_2019[,!(colnames(table_oct_dec_2019) %in% c("Ref."))]
table_high_2019 = table_high_2019[,!colnames(table_high_2019) %in% c("Ref.")]

df_2019 = rbind(rbind(rbind(table_jan_mar_2019, table_apr_jun_2019), table_jul_sept_2019), table_oct_dec_2019)
require(openxlsx)
list_of_datasets <- list("movies_list" = df_2019, "top_movies" = table_high_2019)
write.xlsx(list_of_datasets, file = "movies_2019.xlsx")



#Scraping movies of 2018
url = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2018"
data = read_html(url)

data_high_2018 = data %>% html_nodes(".wikitable") %>% .[[1]] %>% html_table(fill=TRUE)
table_high_2018 = as.data.frame(data_high_2018)

data_jan_mar_2018 = data %>% html_nodes(".wikitable") %>% .[[2]] %>% html_table(fill=TRUE)
table_jan_mar_2018 = as.data.frame(data_jan_mar_2018)

data_apr_jun_2018 = data %>% html_nodes(".wikitable") %>% .[[3]] %>% html_table(fill=TRUE)
table_apr_jun_2018 = as.data.frame(data_apr_jun_2018)

library(htmltab)
data_jul_sept_2018 = data %>% html_nodes(".wikitable") %>% .[[4]] %>% html_table(fill=TRUE)
table_jul_sept_2018 = as.data.frame(data_jul_sept_2018)

data_oct_dec_2018 = data %>% html_nodes(".wikitable") %>% .[[5]] %>% html_table(fill=TRUE)
table_oct_dec_2018 = as.data.frame(data_oct_dec_2018)


table_jan_mar_2018 =  table_jan_mar_2018[,!(colnames(table_jan_mar_2018) %in% c("Ref."))]
table_apr_jun_2018 =  table_apr_jun_2018[,!(colnames(table_apr_jun_2018) %in% c("Ref."))]
table_jul_sept_2018 =  table_jul_sept_2018[,!(colnames(table_jul_sept_2018) %in% c("Ref."))]
table_oct_dec_2018 =  table_oct_dec_2018[,!(colnames(table_oct_dec_2018) %in% c("Ref."))]
table_high_2018 = table_high_2018[,!colnames(table_high_2018) %in% c("Ref.")]

df_2018 = rbind(rbind(rbind(table_jan_mar_2018, table_apr_jun_2018), table_jul_sept_2018), table_oct_dec_2018)
require(openxlsx)
list_of_datasets <- list("movies_list" = df_2018, "top_movies" = table_high_2018)
write.xlsx(list_of_datasets, file = "movies_2018.xlsx")




#Scraping movies of 2017
url = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2017"
data = read_html(url)

data_high_2017 = data %>% html_nodes(".wikitable") %>% .[[1]] %>% html_table(fill=TRUE)
table_high_2017 = as.data.frame(data_high_2017)

data_jan_mar_2017 = data %>% html_nodes(".wikitable") %>% .[[2]] %>% html_table(fill=TRUE)
table_jan_mar_2017 = as.data.frame(data_jan_mar_2017)

data_apr_jun_2017 = data %>% html_nodes(".wikitable") %>% .[[3]] %>% html_table(fill=TRUE)
table_apr_jun_2017 = as.data.frame(data_apr_jun_2017)

library(htmltab)
data_jul_sept_2017 = url %>%
  htmltab(5, rm_nodata_cols = F)
table_jul_sept_2017 = as.data.frame(data_jul_sept_2017)

data_oct_dec_2017 = data %>% html_nodes(".wikitable") %>% .[[5]] %>% html_table(fill=TRUE)
table_oct_dec_2017 = as.data.frame(data_oct_dec_2017)


table_jan_mar_2017 =  table_jan_mar_2017[,!(colnames(table_jan_mar_2017) %in% c("Ref."))]
table_apr_jun_2017 =  table_apr_jun_2017[,!(colnames(table_apr_jun_2017) %in% c("Ref."))]
table_jul_sept_2017 =  table_jul_sept_2017[,!(colnames(table_jul_sept_2017) %in% c("Ref."))]
table_oct_dec_2017 =  table_oct_dec_2017[,!(colnames(table_oct_dec_2017) %in% c("Ref."))]
table_high_2017 = table_high_2017[,!colnames(table_high_2017) %in% c("Ref.")]

df_2017 = rbind(rbind(rbind(table_jan_mar_2017, table_apr_jun_2017), table_jul_sept_2017), table_oct_dec_2017)
require(openxlsx)
list_of_datasets <- list("movies_list" = df_2017, "top_movies" = table_high_2017)
write.xlsx(list_of_datasets, file = "movies_2017.xlsx")



#Scraping movies of 2021
url = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2021"
data = read_html(url)

data_high_2021 = data %>% html_nodes(".wikitable") %>% .[[1]] %>% html_table(fill=TRUE)
table_high_2021 = as.data.frame(data_high_2021)

data_jan_mar_2021 = url %>%
  htmltab(4, rm_nodata_cols = F)
table_jan_mar_2021 = as.data.frame(data_jan_mar_2021)

data_apr_jun_2021 = data %>% html_nodes(".wikitable") %>% .[[3]] %>% html_table(fill=TRUE)
table_apr_jun_2021 = as.data.frame(data_apr_jun_2021)

library(htmltab)
data_jul_sept_2021 = data %>% html_nodes(".wikitable") %>% .[[4]] %>% html_table(fill=TRUE)
table_jul_sept_2021 = as.data.frame(data_jul_sept_2021)

data_oct_dec_2021 = data %>% html_nodes(".wikitable") %>% .[[5]] %>% html_table(fill=TRUE)
table_oct_dec_2021 = as.data.frame(data_oct_dec_2021)


table_jan_mar_2021 =  table_jan_mar_2021[,!(colnames(table_jan_mar_2021) %in% c("Ref."))]
table_apr_jun_2021 =  table_apr_jun_2021[,!(colnames(table_apr_jun_2021) %in% c("Ref."))]
table_jul_sept_2021 =  table_jul_sept_2021[,!(colnames(table_jul_sept_2021) %in% c("Ref."))]
table_oct_dec_2021 =  table_oct_dec_2021[,!(colnames(table_oct_dec_2021) %in% c("Ref."))]
table_high_2021 = table_high_2021[,!colnames(table_high_2021) %in% c("Ref.")]

df_2021 = rbind(rbind(rbind(table_jan_mar_2021, table_apr_jun_2021), table_jul_sept_2021), table_oct_dec_2021)
require(openxlsx)
list_of_datasets <- list("movies_list" = df_2021, "top_movies" = table_high_2021)
write.xlsx(list_of_datasets, file = "movies_2021.xlsx")





#Scraping movies of 2016
url = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2016"
data = read_html(url)

data_high_2016 = data %>% html_nodes(".wikitable") %>% .[[1]] %>% html_table(fill=TRUE)
table_high_2016 = as.data.frame(data_high_2016)

data_jan_mar_2016 = data %>% html_nodes(".wikitable") %>% .[[2]] %>% html_table(fill=TRUE)
table_jan_mar_2016 = as.data.frame(data_jan_mar_2016)

data_apr_jun_2016 = data %>% html_nodes(".wikitable") %>% .[[3]] %>% html_table(fill=TRUE)
table_apr_jun_2016 = as.data.frame(data_apr_jun_2016)

library(htmltab)
data_jul_sept_2016 = data %>% html_nodes(".wikitable") %>% .[[4]] %>% html_table(fill=TRUE)
table_jul_sept_2016 = as.data.frame(data_jul_sept_2016)

data_oct_dec_2016 = data %>% html_nodes(".wikitable") %>% .[[5]] %>% html_table(fill=TRUE)
table_oct_dec_2016 = as.data.frame(data_oct_dec_2016)


table_jan_mar_2016 =  table_jan_mar_2016[,!(colnames(table_jan_mar_2016) %in% c("Ref."))]
table_apr_jun_2016 =  table_apr_jun_2016[,!(colnames(table_apr_jun_2016) %in% c("Ref."))]
table_jul_sept_2016 =  table_jul_sept_2016[,!(colnames(table_jul_sept_2016) %in% c("Ref."))]
table_oct_dec_2016 =  table_oct_dec_2016[,!(colnames(table_oct_dec_2016) %in% c("Ref."))]
table_high_2016 = table_high_2016[,!colnames(table_high_2016) %in% c("Ref.")]

df_2016 = rbind(rbind(rbind(table_jan_mar_2016, table_apr_jun_2016), table_jul_sept_2016), table_oct_dec_2016)
require(openxlsx)
list_of_datasets <- list("movies_list" = df_2016, "top_movies" = table_high_2016)
write.xlsx(list_of_datasets, file = "movies_2016.xlsx")




#Scraping movies of 2015
url = "https://en.wikipedia.org/wiki/List_of_Bollywood_films_of_2015"
data = read_html(url)

data_high_2015 = data %>% html_nodes(".wikitable") %>% .[[1]] %>% html_table(fill=TRUE)
table_high_2015 = as.data.frame(data_high_2015)

data_jan_mar_2015 = data %>% html_nodes(".wikitable") %>% .[[2]] %>% html_table(fill=TRUE)
table_jan_mar_2015 = as.data.frame(data_jan_mar_2015)

data_apr_jun_2015 = data %>% html_nodes(".wikitable") %>% .[[3]] %>% html_table(fill=TRUE)
table_apr_jun_2015 = as.data.frame(data_apr_jun_2015)

library(htmltab)
data_jul_sept_2015 = data %>% html_nodes(".wikitable") %>% .[[4]] %>% html_table(fill=TRUE)
table_jul_sept_2015 = as.data.frame(data_jul_sept_2015)

data_oct_dec_2015 = data %>% html_nodes(".wikitable") %>% .[[5]] %>% html_table(fill=TRUE)
table_oct_dec_2015 = as.data.frame(data_oct_dec_2015)


table_jan_mar_2015 =  table_jan_mar_2015[,!(colnames(table_jan_mar_2015) %in% c("Source"))]
table_apr_jun_2015 =  table_apr_jun_2015[,!(colnames(table_apr_jun_2015) %in% c("Source"))]
table_jul_sept_2015 =  table_jul_sept_2015[,!(colnames(table_jul_sept_2015) %in% c("Source"))]
table_oct_dec_2015 =  table_oct_dec_2015[,!(colnames(table_oct_dec_2015) %in% c("Source"))]
table_high_2015 = table_high_2015[,!colnames(table_high_2015) %in% c("Source")]

df_2015 = rbind(rbind(rbind(table_jan_mar_2015, table_apr_jun_2015), table_jul_sept_2015), table_oct_dec_2015)
require(openxlsx)
list_of_datasets <- list("movies_list" = df_2015, "top_movies" = table_high_2015)
write.xlsx(list_of_datasets, file = "movies_2015.xlsx")



# retrieve info of indian Webseries before 2019
url = "https://www.imdb.com/list/ls031559363/"
data = read_html(url)

webseries = data %>% html_nodes(".lister-item-header a") %>% html_text()
year = data %>% html_nodes(".text-muted.unbold") %>% html_text()
year = year[4:78]
genre = data %>% html_nodes(".genre") %>% html_text()
genre = sapply(genre,function(X) trimws(X, which = "both", whitespace = "[\t\n' ']"))
rating = data %>% html_nodes(".ipl-rating-star.small .ipl-rating-star__rating") %>% html_text()
votes = data %>% html_nodes(".text-muted+ span") %>% html_text()
votes = votes[6:80]
links = data %>% html_nodes(".lister-item-header a") %>% html_attr("href") %>% paste("https://www.imdb.com/",.,sep="")
image_url = data %>% html_nodes(".loadlate") %>% html_attr("loadlate")
image_url = image_url[1:75]
df_web_2019 = data.frame(webseries,year,genre,rating,votes,links,image_url)


#################################################


url =  c("https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250",
         "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250&start=251&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250&start=501&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250&start=751&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250&start=1001&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250&start=1251&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250&start=1501&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=feature&release_date=2015-01-01,2021-01-01&countries=in&languages=hi&count=250&start=1751&ref_=adv_nxt")

movies = data.frame()
for(u in url){
  data = read_html(u)
  df <- data %>% 
    html_nodes('.mode-advanced') %>%    # select enclosing nodes
    # iterate over each, pulling out desired parts and coerce to data.frame
    map_df(~list(name = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},    # replace length-0 elements with NA
                 year = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 rating = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 genre = html_nodes(.x, '.genre') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 votes = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 cast = html_nodes(.x, '.text-muted+ p') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 runtime = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 image_url = html_nodes(.x, '.loadlate') %>% 
                   html_attr("loadlate") %>% 
                   {if(length(.) == 0) NA else .},
                 next_link = html_nodes(.x, '.lister-item-header a') %>% 
                   html_attr("href") %>%
                   {if(length(.) == 0) NA else paste("https://www.imdb.com/",.,sep = "")},
                 certificate = html_nodes(.x, '.certificate') %>% 
                   html_text() %>%
                   {if(length(.) == 0) NA else .}             
    ))
  movies = rbind(movies,df)
}


library(sjmisc)
library(polite)
production = data.frame()

for (link in movies$next_link){
  dat = read_html(link)
  df <- dat %>% html_nodes("#titleDetails")
  k = df %>% html_nodes(".txt-block")
  prods_name = NULL
  budget = NULL
  box_office  =NULL
  date = NULL
  for(h in k){
    h4 = h %>% html_nodes("h4") %>% html_text() %>% trim()
    a = h %>% html_nodes("a")
    #cat(h %>% html_nodes("h4") %>% html_text() %>% trim())
    #cat("\t",h %>% html_nodes("a") %>% html_text() %>% trim()," ----------\n")
    if(str_contains(h4, "Production", ignore.case = TRUE)){
      prods_name = a %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
     }
    if(str_contains(h4, "budget", ignore.case = TRUE)){
      budget = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    if(str_contains(h4, "gross", ignore.case = TRUE)){
      box_office = h %>% html_text() %>% trim()
    }
    if(str_contains(h4, "date", ignore.case = TRUE)){
      date = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    
  }
  if(is.null(prods_name)){
    prods_name = NA
  }  
  if(is.null(budget)){
    budget = NA
  }
  if(is.null(box_office)){
    box_office = NA
  }
  if(is.null(date)){
    date = NA
  }
  table = data.frame(prods_name[-length(prods_name)] %>% paste(collapse = ", "), budget, date, box_office)
  production = rbind(production,table)
}
names(production) = c("studio","budget","date","box_office")

for(i in seq(1:1926)){
  std = production[i,]$studio
  bd = production[i,]$budget
  dt = production[i,]$date
  bo = production[i,]$box_office
  
  if(std=="" && is.na(bd) && is.na(dt) && is.na(bo)){
    dat = read_html(movies$next_link[i])
    df <- dat %>% html_nodes("#titleDetails")
    k = df %>% html_nodes(".txt-block")
    prods_name = NULL
    budget = NULL
    box_office  =NULL
    date = NULL
    for(h in k){
      h4 = h %>% html_nodes("h4") %>% html_text() %>% trim()
      a = h %>% html_nodes("a")
      #cat(h %>% html_nodes("h4") %>% html_text() %>% trim())
      #cat("\t",h %>% html_nodes("a") %>% html_text() %>% trim()," ----------\n")
      if(str_contains(h4, "Production", ignore.case = TRUE)){
        prods_name = a %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      if(str_contains(h4, "budget", ignore.case = TRUE)){
        budget = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      if(str_contains(h4, "gross", ignore.case = TRUE)){
        box_office = h %>% html_text() %>% trim()
      }
      if(str_contains(h4, "date", ignore.case = TRUE)){
        date = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      
    }
    if(is.null(prods_name)){
      prods_name = NA
    }  
    if(is.null(budget)){
      budget = NA
    }
    if(is.null(box_office)){
      box_office = NA
    }
    if(is.null(date)){
      date = NA
    }
    
    production[i,]$studio = prods_name[-length(prods_name)] %>% paste(collapse = ", ")
    production[i,]$budget = budget
    production[i,]$box_office = box_office
    production[i,]$date = date
  }
}
create = function(l){
  cat("----",l)
  return(strsplit(l, '\\?')[[1]][1] %>% paste(.,"companycredits?ref_=tt_dt_co",sep = ""))
}
movies$dist_link = lapply(movies$next_link, FUN=create)

df1 = data.frame()
for(i in seq(1,1926)){
  link = movies$dist_link[i][[1]][1]
  cat("\n###### -----",i,"----",movies$name[i])
  dat = read_html(link)
  df <- dat %>% 
    html_nodes('#distributors+ .simpleList') %>%   
    map_df(~list(distributers = html_nodes(.x, 'a') 
                 %>% html_text() %>% paste(collapse = ", ")%>%
                 {if(length(.) == 0) "null" else .}
    ))
  if(is_empty(dat %>% html_nodes('#distributors+ .simpleList'))){
    df = data.frame(distributers="NA")
  }
  df1 = rbind(df1, df)
  print(df1[i,1])
}


Movie_df = cbind(movies, production, df1)
list_of_datasets <- list("movies_list" = Movie_df)
write.xlsx(list_of_datasets, file = "movies_2015_2021.xlsx")


############### Webseries ###########################
url =  c("https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2019-01-01,2021-01-01&countries=in&languages=hi&count=250",
         "https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2019-01-01,2021-01-01&countries=in&languages=hi&count=250&start=251&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2019-01-01,2021-01-01&countries=in&languages=hi&count=250&start=501&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2019-01-01,2021-01-01&countries=in&languages=hi&count=250&start=751&ref_=adv_nxt")

webseries_1 = data.frame()
for(u in url){
  data = read_html(u)
  df <- data %>% 
    html_nodes('.mode-advanced') %>%    # select enclosing nodes
    # iterate over each, pulling out desired parts and coerce to data.frame
    map_df(~list(name = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},    # replace length-0 elements with NA
                 year = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 rating = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 genre = html_nodes(.x, '.genre') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 votes = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 cast = html_nodes(.x, '.text-muted+ p') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 runtime = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 image_url = html_nodes(.x, '.loadlate') %>% 
                   html_attr("loadlate") %>% 
                   {if(length(.) == 0) NA else .},
                 next_link = html_nodes(.x, '.lister-item-header a') %>% 
                   html_attr("href") %>%
                   {if(length(.) == 0) NA else paste("https://www.imdb.com/",.,sep = "")},
                 certificate = html_nodes(.x, '.certificate') %>% 
                   html_text() %>%
                   {if(length(.) == 0) NA else .}             
    ))
  webseries_1 = rbind(webseries_1,df)
}


production_w = data.frame()

for (link in webseries_1$next_link){
  dat = read_html(link)
  df <- dat %>% html_nodes("#titleDetails")
  k = df %>% html_nodes(".txt-block")
  prods_name = NULL
  budget = NULL
  box_office  =NULL
  date = NULL
  for(h in k){
    h4 = h %>% html_nodes("h4") %>% html_text() %>% trim()
    a = h %>% html_nodes("a")
    #cat(h %>% html_nodes("h4") %>% html_text() %>% trim())
    #cat("\t",h %>% html_nodes("a") %>% html_text() %>% trim()," ----------\n")
    if(str_contains(h4, "Production", ignore.case = TRUE)){
      prods_name = a %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    if(str_contains(h4, "budget", ignore.case = TRUE)){
      budget = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    if(str_contains(h4, "gross", ignore.case = TRUE)){
      box_office = h %>% html_text() %>% trim()
    }
    if(str_contains(h4, "date", ignore.case = TRUE)){
      date = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    
  }
  if(is.null(prods_name)){
    prods_name = NA
  }  
  if(is.null(budget)){
    budget = NA
  }
  if(is.null(box_office)){
    box_office = NA
  }
  if(is.null(date)){
    date = NA
  }
  table = data.frame(prods_name[-length(prods_name)] %>% paste(collapse = ", "), budget, date, box_office)
  production_w = rbind(production_w,table)
}
names(production_w) = c("studio","budget","date","box_office")


for(i in seq(1:752)){
  std = production_w[i,]$studio
  bd = production_w[i,]$budget
  dt = production_w[i,]$date
  bo = production_w[i,]$box_office
  
  if(std=="" && is.na(bd) && is.na(dt) && is.na(bo)){
    dat = read_html(webseries_1$next_link[i])
    df <- dat %>% html_nodes("#titleDetails")
    k = df %>% html_nodes(".txt-block")
    prods_name = NULL
    budget = NULL
    box_office  =NULL
    date = NULL
    for(h in k){
      h4 = h %>% html_nodes("h4") %>% html_text() %>% trim()
      a = h %>% html_nodes("a")
      #cat(h %>% html_nodes("h4") %>% html_text() %>% trim())
      #cat("\t",h %>% html_nodes("a") %>% html_text() %>% trim()," ----------\n")
      if(str_contains(h4, "Production", ignore.case = TRUE)){
        prods_name = a %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      if(str_contains(h4, "budget", ignore.case = TRUE)){
        budget = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      if(str_contains(h4, "gross", ignore.case = TRUE)){
        box_office = h %>% html_text() %>% trim()
      }
      if(str_contains(h4, "date", ignore.case = TRUE)){
        date = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      
    }
    if(is.null(prods_name)){
      prods_name = NA
    }  
    if(is.null(budget)){
      budget = NA
    }
    if(is.null(box_office)){
      box_office = NA
    }
    if(is.null(date)){
      date = NA
    }
    
    production_w[i,]$studio = prods_name[-length(prods_name)] %>% paste(collapse = ", ")
    production_w[i,]$budget = budget
    production_w[i,]$box_office = box_office
    production_w[i,]$date = date

  }
}

create = function(l){
  cat("----",l)
  return(strsplit(l, '\\?')[[1]][1] %>% paste(.,"companycredits?ref_=tt_dt_co",sep = ""))
}
webseries_1$dist_link = lapply(webseries_1$next_link, FUN=create)

df1 = data.frame()
for(i in seq(1,752)){
  link = webseries_1$dist_link[i][[1]][1]
  cat("\n###### -----",i,"----",webseries_1$name[i])
  dat = read_html(link)
  df <- dat %>% 
    html_nodes('#distributors+ .simpleList') %>%   
    map_df(~list(distributers = html_nodes(.x, 'a') 
                 %>% html_text() %>% paste(collapse = ", ")%>%
                 {if(length(.) == 0) "null" else .}
    ))
  if(is_empty(dat %>% html_nodes('#distributors+ .simpleList'))){
    df = data.frame(distributers="NA")
  }
  df1 = rbind(df1, df)
  print(df1[i,1])
}


Webseries_df = cbind(webseries_1, production_w, df1)
list_of_datasets <- list("webseries_list" = Webseries_df)
write.xlsx(list_of_datasets, file = "webseries_2019_2021.xlsx")


##################### webseries before 2019 #####################################
url =  c("https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2015-01-01,2019-01-01&countries=in&languages=hi&count=250",
         "https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2015-01-01,2019-01-01&countries=in&languages=hi&count=250&start=251&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2015-01-01,2019-01-01&countries=in&languages=hi&count=250&start=501&ref_=adv_nxt",
         "https://www.imdb.com/search/title/?title_type=tv_series,tv_miniseries&release_date=2015-01-01,2019-01-01&countries=in&languages=hi&count=250&start=751&ref_=adv_nxt")

webseries_1 = data.frame()
for(u in url){
  data = read_html(u)
  df <- data %>% 
    html_nodes('.mode-advanced') %>%    # select enclosing nodes
    # iterate over each, pulling out desired parts and coerce to data.frame
    map_df(~list(name = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},    # replace length-0 elements with NA
                 year = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 rating = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 genre = html_nodes(.x, '.genre') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 votes = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 cast = html_nodes(.x, '.text-muted+ p') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 runtime = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 image_url = html_nodes(.x, '.loadlate') %>% 
                   html_attr("loadlate") %>% 
                   {if(length(.) == 0) NA else .},
                 next_link = html_nodes(.x, '.lister-item-header a') %>% 
                   html_attr("href") %>%
                   {if(length(.) == 0) NA else paste("https://www.imdb.com/",.,sep = "")},
                 certificate = html_nodes(.x, '.certificate') %>% 
                   html_text() %>%
                   {if(length(.) == 0) NA else .}             
    ))
  webseries_1 = rbind(webseries_1,df)
}


production_w = data.frame()

for (link in webseries_1$next_link){
  dat = read_html(link)
  df <- dat %>% html_nodes("#titleDetails")
  k = df %>% html_nodes(".txt-block")
  prods_name = NULL
  budget = NULL
  box_office  =NULL
  date = NULL
  for(h in k){
    h4 = h %>% html_nodes("h4") %>% html_text() %>% trim()
    a = h %>% html_nodes("a")
    if(str_contains(h4, "Production", ignore.case = TRUE)){
      prods_name = a %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    if(str_contains(h4, "budget", ignore.case = TRUE)){
      budget = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    if(str_contains(h4, "gross", ignore.case = TRUE)){
      box_office = h %>% html_text() %>% trim()
    }
    if(str_contains(h4, "date", ignore.case = TRUE)){
      date = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
    }
    
  }
  if(is.null(prods_name)){
    prods_name = NA
  }  
  if(is.null(budget)){
    budget = NA
  }
  if(is.null(box_office)){
    box_office = NA
  }
  if(is.null(date)){
    date = NA
  }
  cat("\n#####################################################")
  cat("\nprod ---",prods_name[-length(prods_name)] %>% paste(collapse = ", "))
  cat("\nbudget ----",budget)
  cat("\ndate ----",date)
  cat("\nbox ----",box_office)
  table = data.frame(prods_name[-length(prods_name)] %>% paste(collapse = ", "), budget, date, box_office)
  production_w = rbind(production_w,table)
}
names(production_w) = c("studio","budget","date","box_office")


for(i in seq(1:848)){
  std = production_w[i,]$studio
  bd = production_w[i,]$budget
  dt = production_w[i,]$date
  bo = production_w[i,]$box_office
  
  if(std=="" && is.na(bd) && is.na(dt) && is.na(bo)){
    dat = read_html(webseries_1$next_link[i])
    df <- dat %>% html_nodes("#titleDetails")
    k = df %>% html_nodes(".txt-block")
    prods_name = NULL
    budget = NULL
    box_office  =NULL
    date = NULL
    for(h in k){
      h4 = h %>% html_nodes("h4") %>% html_text() %>% trim()
      a = h %>% html_nodes("a")
      #cat(h %>% html_nodes("h4") %>% html_text() %>% trim())
      #cat("\t",h %>% html_nodes("a") %>% html_text() %>% trim()," ----------\n")
      if(str_contains(h4, "Production", ignore.case = TRUE)){
        prods_name = a %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      if(str_contains(h4, "budget", ignore.case = TRUE)){
        budget = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      if(str_contains(h4, "gross", ignore.case = TRUE)){
        box_office = h %>% html_text() %>% trim()
      }
      if(str_contains(h4, "date", ignore.case = TRUE)){
        date = h %>% html_text() %>% trimws(., which = "both", whitespace = "[\t\n' ']")
      }
      
    }
    if(is.null(prods_name)){
      prods_name = NA
    }  
    if(is.null(budget)){
      budget = NA
    }
    if(is.null(box_office)){
      box_office = NA
    }
    if(is.null(date)){
      date = NA
    }
    
    production_w[i,]$studio = prods_name[-length(prods_name)] %>% paste(collapse = ", ")
    production_w[i,]$budget = budget
    production_w[i,]$box_office = box_office
    production_w[i,]$date = date
    
    cat("\n#####################################################")
    cat("\nprod ---",i,prods_name[-length(prods_name)] %>% paste(collapse = ", "))
    cat("\nbudget ----",budget)
    cat("\ndate ----",date)
    cat("\nbox ----",box_office)
    
  }
}

create = function(l){
  cat("----",l)
  return(strsplit(l, '\\?')[[1]][1] %>% paste(.,"companycredits?ref_=tt_dt_co",sep = ""))
}
webseries_1$dist_link = lapply(webseries_1$next_link, FUN=create)

df1 = data.frame()
for(i in seq(1,848)){
  link = webseries_1$dist_link[i][[1]][1]
  cat("\n###### -----",i,"----",webseries_1$name[i])
  dat = read_html(link)
  df <- dat %>% 
    html_nodes('#distributors+ .simpleList') %>%   
    map_df(~list(distributers = html_nodes(.x, 'a') 
                 %>% html_text() %>% paste(collapse = ", ")%>%
                 {if(length(.) == 0) "null" else .}
    ))
  if(is_empty(dat %>% html_nodes('#distributors+ .simpleList'))){
    df = data.frame(distributers="NA")
  }
  df1 = rbind(df1, df)
  print(df1[i,1])
}


Webseries_df = cbind(webseries_1, production_w, df1)
list_of_datasets <- list("webseries_list" = Webseries_df)
write.xlsx(list_of_datasets, file = "webseries_2015_2019.xlsx")


######### prime ############
url = "https://en.wikipedia.org/wiki/List_of_Amazon_India_originals"
data = read_html(url)

prime1 = data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)
prime1 = as.data.frame(prime1)
prime1 = rbind(prime1, as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[2]] %>% html_table(fill=TRUE)))
prime1 = rbind(prime1, as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[3]] %>% html_table(fill=TRUE)))
prime_movies = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[4]] %>% html_table(fill=TRUE))
prime_standup = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[5]] %>% html_table(fill=TRUE))
require(openxlsx)
list_of_datasets <- list("webseries" = prime1, "movies" = prime_movies, "standup" = prime_standup)
write.xlsx(list_of_datasets, file = "Amazon_prime.xlsx")


######### netflix ############
url = "https://en.wikipedia.org/wiki/List_of_Netflix_India_originals"
data = read_html(url)

net = data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)
net = as.data.frame(net)
net = rbind(net, as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[2]] %>% html_table(fill=TRUE)))
df = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[3]] %>% html_table(fill=TRUE))
df$Genre = NA
df$"Language(s)" = NA
net = rbind(net, df)
df = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[4]] %>% html_table(fill=TRUE))
colnames(df)[which(names(df) == "Language")] <- "Language(s)"
colnames(df)[which(names(df) == "Runtime")] <- "Length"
df = df[,-c(3)]
net = rbind(net, df)

net_movies = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[5]] %>% html_table(fill=TRUE))
df = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[6]] %>% html_table(fill=TRUE))
df$Genre = NA
net_movies = rbind(net_movies, df)
net_standup = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[7]] %>% html_table(fill=TRUE))
require(openxlsx)
list_of_datasets <- list("webseries" = net, "movies" = net_movies, "standup" = net_standup)
write.xlsx(list_of_datasets, file = "Netflix.xlsx")


################## ALT ############################
url = "https://en.wikipedia.org/wiki/ALTBalaji"
data = read_html(url)

library(htmltab)
alt =  url %>%
  htmltab(2, rm_nodata_cols = F)
alt = as.data.frame(alt)
list_of_datasets <- list("webseries" = alt)
write.xlsx(list_of_datasets, file = "ALT_balaji.xlsx")



################## Voot ############################
url = "https://en.wikipedia.org/wiki/Voot"
data = read_html(url)

library(htmltab)
voot =  url %>%
  htmltab(2, rm_nodata_cols = F)
voot = as.data.frame(voot)
list_of_datasets <- list("webseries" = voot)
write.xlsx(list_of_datasets, file = "voot.xlsx")


################## Ullu ############################
url = "https://en.wikipedia.org/wiki/Ullu"
data = read_html(url)

library(htmltab)
ullu =  url %>%
  htmltab(2, rm_nodata_cols = F)
ullu = as.data.frame(ullu)
list_of_datasets <- list("webseries" = ullu)
write.xlsx(list_of_datasets, file = "ullu.xlsx")


######### zee 5 ############
url = "https://en.wikipedia.org/wiki/List_of_ZEE5_original_web_series"
data = read_html(url)

zee = data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)
zee = as.data.frame(zee)
zee = rbind(zee, as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[2]] %>% html_table(fill=TRUE)))
zee = rbind(zee, as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[3]] %>% html_table(fill=TRUE)))
zee = rbind(zee, as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[4]] %>% html_table(fill=TRUE)))
zee = zee[,-c(8)]
list_of_datasets <- list("webseries" = zee)
write.xlsx(list_of_datasets, file = "zee5.xlsx")



######### hotstart ############
url = "https://en.wikipedia.org/wiki/List_of_Hotstar_original_programming"
data = read_html(url)

hs = data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)
hs = as.data.frame(hs)
df =  url %>%
  htmltab(2, rm_nodata_cols = F)
df = as.data.frame(df)
hs = rbind(hs, df)
hs = rbind(hs, as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[3]] %>% html_table(fill=TRUE)))
df = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[4]] %>% html_table(fill=TRUE))
colnames(hs)[which(names(hs) == "Language(s)")] <- "Language"
df$Status = NA
hs = hs[,-c(7)]
hs = rbind(hs, df)
df = as.data.frame(data %>% html_nodes(".mw-parser-output") %>% html_nodes("table") %>% .[[5]] %>% html_table(fill=TRUE))[,-c(7)]
hs$Length = NA
df$Status = NA
hs = rbind(hs, df)

df = as.data.frame(url %>%
  htmltab(6, rm_nodata_cols = F))
df = df[,-c(7)]
df$Status = NA
hs = rbind(hs, df)

df = as.data.frame(url %>%
                     htmltab(7, rm_nodata_cols = F))
df = df[,-c(4,8)]
df$Status = NA
hs = rbind(hs, df)

df = as.data.frame(url %>%
                     htmltab(8, rm_nodata_cols = F))
df = df[,-c(6)]
df$Status = NA
df$Genre = NA
hs = rbind(hs, df)

df = as.data.frame(url %>%
                     htmltab(9, rm_nodata_cols = F))
df = df[,-c(7)]
df$Status = NA
hs = rbind(hs, df)

df = as.data.frame(url %>%
                     htmltab(10, rm_nodata_cols = F))
df = df[,-c(7)]
df$Status = NA
colnames(df)[which(names(df) == "Season(s)")] <- "Seasons"
hs = rbind(hs, df)
list_of_datasets <- list("webseries" = hs)
write.xlsx(list_of_datasets, file = "hotstar.xlsx")
