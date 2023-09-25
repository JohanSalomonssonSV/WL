library(tidyverse)
{
nordic_new_highs<-\(){
t <- jsalomon::perf_table_multi("nordic") |> suppressWarnings()
tv<-
  t |> filter(value_traded>=800000) |> transmute(symbol=paste0(exchange, ":", name), 
                   new_high52w=ifelse(high>=price_52_week_high,1,0),
                   new_low52w=ifelse(low<=price_52_week_low,1,0),
                   
                   new_high1m=ifelse(high>=high_1m,1,0),
                   new_low1m=ifelse(low<=low_1m,1,0),
                   
                   new_high3m=ifelse(high>=high_3m,1,0),
                   new_low3m=ifelse(low<=low_3m,1,0),
                   
                   new_highall=ifelse(high>=high_all,1,0),
                   new_lowall=ifelse(low<=low_all,1,0)) |> 
  pivot_longer(cols = 2:9) |> 
    filter(value>0) |> 
    mutate(dur=case_when(name=="new_high1m"   ~ 30 ,
                         name=="new_high3m"   ~ 90 ,
                         name=="new_high52w"   ~ 252 ,
                         name=="new_highall"   ~ 1000 )
           ) |> 
    arrange(-dur) |> 
    distinct(symbol, .keep_all = T)

ath<- paste0(tv |> filter(dur==1000) |> pull(symbol), collapse = ", ")
w52<- paste0(tv |> filter(dur==252) |> pull(symbol), collapse = ", ")
threeMonth<- paste0(tv |> filter(dur==90) |> pull(symbol), collapse = ", ")
oneMonth<- paste0(tv |> filter(dur==30) |> pull(symbol), collapse = ", ")

file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "nordic_New_Highs"))

writeLines(paste0("### ATH,", ath, ", ### Year High ,", w52, ", ### 3 Month High,",threeMonth, ", ### 1 Month High", oneMonth), 
           paste0("C:/Users/johan/Documents/github/avanza/watchlists/nordic_New_Highs",today(),".txt") )

}

us_new_highs<-\(){
  t <- jsalomon::perf_table_multi("america") |> suppressWarnings()
  tv<-
    t |> filter(value_traded>=3000000,
                ((atr/close)*100)/0.87>=5,
                close>=sma200) |> 
    transmute(symbol=paste0(exchange, ":", name),
                               prox_adr=((atr/close)*100)/0.87, 
                                                   new_high52w=ifelse(high>=price_52_week_high,1,0),
                                                   new_low52w=ifelse(low<=price_52_week_low,1,0),
                                                   
                                                   new_high1m=ifelse(high>=high_1m,1,0),
                                                   new_low1m=ifelse(low<=low_1m,1,0),
                                                   
                                                   new_high3m=ifelse(high>=high_3m,1,0),
                                                   new_low3m=ifelse(low<=low_3m,1,0),
                                                   
                                                   new_highall=ifelse(high>=high_all,1,0),
                                                   new_lowall=ifelse(low<=low_all,1,0)) |> 
    pivot_longer(cols = 3:10) |> 
    filter(value>0) |> 
    mutate(dur=case_when(name=="new_high1m"   ~ 30 ,
                         name=="new_high3m"   ~ 90 ,
                         name=="new_high52w"   ~ 252 ,
                         name=="new_highall"   ~ 1000 )
    ) |> 
    arrange(-dur,-prox_adr  ) |> 
    distinct(symbol, .keep_all = T)
  
  ath<- paste0(tv |> filter(dur==1000) |> pull(symbol), collapse = ", ")
  w52<- paste0(tv |> filter(dur==252) |> pull(symbol), collapse = ", ")
  threeMonth<- paste0(tv |> filter(dur==90) |> pull(symbol), collapse = ", ")
  oneMonth<- paste0(tv |> filter(dur==30) |> pull(symbol), collapse = ", ")
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "us_New_Highs"))
  
  writeLines(paste0("### ATH,", ath, ", ### Year High ,", w52, ", ### 3 Month High,",threeMonth, ", ### 1 Month High", oneMonth), 
             paste0("C:/Users/johan/Documents/github/avanza/watchlists/us_New_Highs",today(),".txt") )
  
}

nordic_earnings_WL<-\(){
t <- jsalomon::perf_table_multi("nordic")|> suppressWarnings()
tv<-t |> transmute(name, description,exchange, earnings_release_date=as.Date(as_datetime(earnings_release_date  )),
                   earnings_release_next_date=as.Date(as_datetime(earnings_release_next_date  )) 
) |> 
  filter((earnings_release_date>=today()-1 & earnings_release_date<=today())| earnings_release_next_date==today()  ) |> 
  transmute(wl=paste0(exchange, ":", name)) |> pull(wl)

file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "nordic_earnings"))

writeLines(paste0(tv, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/nordic_earnings",today(),".txt") )
}

us_earnings_WL<-\(){
t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
tv<-t |>  filter(value_traded>=1000000
                 ) |> 
  transmute(name, description,
            exchange,
            earnings_per_share_diluted_yoy_growth_fq, 
            earnings_release_date=as.Date(as_datetime(earnings_release_date  )),
                   earnings_release_next_date=as.Date(as_datetime(earnings_release_next_date  )) 
) |> 
  filter((earnings_release_date>=today()-1 )) |> 
  arrange(-earnings_per_share_diluted_yoy_growth_fq ) |> 
  transmute(wl=paste0(exchange, ":", name)) |> pull(wl)

file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "us_earnings"))

writeLines(paste0(tv, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/us_earnings",today(),".txt") )
}

us_rs_wl<-\(){
  score_function<-\(.,col){
    . |> 
      mutate({{col}}:=ifelse(is.na({{col}}),0,  {{col}} )) |> 
      arrange({{col}}) |> 
      mutate(
        rank=row_number(),
        rank=round(rank/max(rank)*100,2)
      ) |> 
      #deparse({{col}})
      rename("{{col}}_rank":=rank)}
    
  file_metadata <- function(url) {
    
    page <- rvest::read_html(url)
    
    file <- tail(strsplit(url, "/")[[1]], 1)
    div1 <- "text-mono f6 flex-auto pr-3 flex-order-2 flex-md-order-1"
    
    size <- page %>%
      rvest::html_elements(xpath = paste0("//div[@class='", div1, "']")) %>%
      rvest::html_text() %>%
      strsplit("\n") %>%
      sapply(trimws) %>%
      getElement(5)
    
    last_commit <- page %>% 
      rvest::html_elements("relative-time") %>% 
      rvest::html_attr("datetime") %>%
      as.POSIXct()
    
    data.frame(file, size, last_commit)
  }
  data_from_github<-\(raw_url, gt_url){
    d<-read_csv(raw_url)  
    # d<-d |> mutate(github_commited_timestamp= file_metadata(gt_url ),
    #                timestamp=today())
    d}
  #file_metadata("https://github.com/skyte/rs-log/blob/main/output/rs_stocks.csv")
  
  stock_url_raw<-"https://raw.githubusercontent.com/skyte/rs-log/main/output/rs_stocks.csv"
  stock_url<- "https://github.com/skyte/rs-log/blob/main/output/rs_stocks.csv"
  stock_rs<-data_from_github(stock_url_raw,stock_url ) #|> janitor::clean_names()
  stock_rs<-stock_rs |> janitor::clean_names()
  
  t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
  
  wl<-stock_rs |>  left_join(t |> 
                               score_function(earnings_per_share_diluted_yoy_growth_fq)|> 
                               transmute(ticker=name, 
                                            value_traded=value_traded/1000000,
                                            atr,
                                            close,
                                         earnings_per_share_diluted_yoy_growth_fq_rank,
                                         sma20,sma50),
                             ) |> 
    filter(value_traded>=1,
           sma20>=sma50,
           ((atr/close)*100)/0.87>=4.2
           ) |> 
    select(-value_traded) |> 
    mutate(ticker=paste0(exchange,":",ticker)) |> 
    #select(ticker, percentile, contains("ago") ) |> 
    select(symbol=ticker,#industry, 
           contains("ago"),
           contains("rank"),
           eps_yq_rank=earnings_per_share_diluted_yoy_growth_fq_rank)|> 
    filter(x1_month_ago  >=92 | x3_months_ago>=92 | x6_months_ago  >=92  ) |> 
    mutate(score=0.25*x1_month_ago  + 0.25*x3_months_ago  + 0.25*x6_months_ago   +  0.25*eps_yq_rank) |> 
    arrange(-score) |> 
    #pivot_longer(cols = 2:5) |> 
    # filter(value>=97,
    #        !str_detect(ticker, "MKT:")) |> 
    # select(-name) |> 
    # distinct(ticker) |> 
    pull(symbol)
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "us_rs_"))
  writeLines(paste0(wl, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/us_rs_",today(),".txt") )
  
  
}


nordic_bb_break<-\(){
  
  t<-jsalomon::perf_table() |> suppressWarnings()
  wl<-t |> filter(close>=bb_upper,
                  value_traded>=1000000
                  ) |> mutate(symbol=paste0(exchange,":", name)) |> pull(symbol)

  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "nordic_bb_break"))
  
  writeLines(paste0(wl, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/nordic_bb_break",today(),".txt") )
  
    
}

us_bb_break<-\(){
  
  t<-jsalomon::perf_table_multi("america") |> suppressWarnings()
  wl<-t |> filter(close>=bb_upper,
                  value_traded>=5000000
  ) |> mutate(symbol=paste0(exchange,":", name)) |> pull(symbol)
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "us_bb_break"))
  
  writeLines(paste0(wl, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/us_bb_break",today(),".txt") )
  
  
}


nordic_largest_relvol<-\(){
  t <- jsalomon::perf_table_multi("nordic") |> suppressWarnings()
  
  opening<-paste0(today(), " ", "09:00:00")
  now<-paste(today(), " ", str_extract(Sys.time(), "\\d\\d:\\d\\d:\\d\\d"   )  )
  hours_of_day<-as.numeric(difftime(now,opening, units = "hour"))
  hours_of_day<-if_else(hours_of_day<=0 | hours_of_day>=8, 8, hours_of_day)
  #hours_of_day<- 1:8
 # 0.25*hours_of_day^(1/1.03)
  
  
  rv<-t |> mutate(
    
    #adr_proxy= ((atr/close)*100)/0.87,
    rel_vol=volume/average_volume_30d_calc,
    #bullish_half=ifelse(close>=(high+low)/2, 1 ,0),
    name=paste0(exchange, ":", name)
  ) |> 
    filter( (close>=5 &
           average_volume_30d_calc*sma30>=1000000) | (rel_vol >0.25*hours_of_day^(1/1.02) & average_volume_30d_calc*sma30>=1000000*(1/2) ) ,
           exchange!="OSL"
    ) |>
    arrange(-rel_vol)

rv_head<-rv |> filter(change>=0)  |> head(60) |> pull(name)
rv_tail<-rv |> filter(change<0)  |> head(50)|> pull(name)

dom<-\(var) {
  rv |> filter(name %in% rv_head) |> group_by({{var}}) |> 
    summarize(n=n()) |> mutate(pct=100*n/sum(n)) |> arrange(-n)|>
    select(-n) |> head(6)
  }
    
    file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "Nordic_rel_vol"))
    
    writeLines(paste0(paste0(rv_head, collapse = ","),
                      ", ### down , ",
                      paste0(rv_tail, collapse = ","),
                      collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/Nordic_rel_vol",today(),".txt") )
    cat("At Your Service, Sir!\n\n", 
              length(rv_head), " Stocks Up and ", 
              length(rv_tail), "Stocks Down\n\nAll Luck Today!\n\nYou Are a Winner!\n"
        
        )
    print(dom(sector) )
    print(dom(industry))
    
  # paste0("At Your Service, Sir!
  #              Stocks Up and ", "Stocks Down\\n\\nAll Luck Today! You Are a Winner!")
  # prnt.test <- function(x){
  #   cat(x, sep="\n\n")
  # } 
  #prnt.test(c("At Your Service, Sir!",Stocks Up and ", "Stocks Down\\n\\nAll Luck Today! You Are a Winner!",,))
  
}

nordic_recent_movers<-\(var=change_60, var2=change_15, n=15, dir="up" ){
  
  t <- jsalomon::perf_table_multi("nordic") |> suppressWarnings()

  rv<-t |> mutate(
    
    #adr_proxy= ((atr/close)*100)/0.87,
    rel_vol=volume/average_volume_30d_calc,
    #bullish_half=ifelse(close>=(high+low)/2, 1 ,0),
    name=paste0(exchange, ":", name)
  ) |> 
    filter( close>=5,
            average_volume_30d_calc*sma30>=1000000  ,
            exchange!="OSL"
            
    ) 

if( dir=="up"){  
 fin<-bind_rows(  
  rv |> 
    arrange(-{{var}}) |> 
    head(n),
  rv |> 
    arrange(-{{var2}}) |> 
    head(n)
  ) |>arrange(-{{var2}}) |>  distinct(name, .keep_all = T)
} else{
  fin<-bind_rows(  
    rv |> 
      arrange(-abs({{var}}) ) |> 
      head(n),
    rv |> 
      arrange(-abs({{var2}})) |> 
      head(n)
  ) |>arrange(-abs({{var2}})) |>  distinct(name, .keep_all = T)
  
}
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "Nordic_recent_movers"))
  
  writeLines(paste0(fin |> pull(name), collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/Nordic_recent_movers",today(),".txt") )
  
 cat("Here you go!\n\nDon't get to much FOMO!") 
}

  
twenty_in_1m<-\(country="nordic"){
  if (country=="nordic"){
    
    t <- jsalomon::perf_table_multi("nordic")|> suppressWarnings()
    #t<-t |> mutate(symbol=jsalomon::convert_to_yf(name, exchange)) 
    
    all_tickers<-t |> 
      filter(sma30*average_volume_30d_calc>=2000000,
             perf_1m>=20
             ) |> 
      mutate(name=paste0(exchange, ":", name)) |> 
      arrange(perf_1m)
    
    OSL<-all_tickers |> filter(exchange=="OSL") |> pull(name)
    not_OSL<-all_tickers |> filter(exchange!="OSL") |> pull(name)
    
    final_wl<-paste0(paste0(not_OSL, collapse = ","),", ### Norway, ", paste0(OSL, collapse = ","))
    
    file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "Nordic_20pct_in_1m_"))
    
    writeLines(final_wl, paste0("C:/Users/johan/Documents/github/avanza/watchlists/Nordic_20pct_in_1m_",today(),".txt") )
    
    
  }
  else{
    t <- jsalomon::perf_table_multi()|> suppressWarnings()
    #t<-t |> mutate(symbol=jsalomon::convert_to_yf(name, exchange)) 
    
    all_tickers<-t |> 
      filter(sma30*average_volume_30d_calc>=5000000,
             perf_1m>=20
      ) |> 
      mutate(name=paste0(exchange, ":", name)) |> 
      arrange(perf_1m)
    
    wl<-all_tickers |>  pull(name)
   
    
    final_wl<-paste0(wl, collapse = ",")
    
    file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_20pct_in_1m_"))
    
    writeLines(final_wl, paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_20pct_in_1m_",today(),".txt") )
    
    
  }

  
  
}

# Minervini

Nordic_minervini<-function(){
  score_function<-\(.,col){
    . |> 
      mutate({{col}}:=ifelse(is.na({{col}}),0,  {{col}} )) |> 
      arrange({{col}}) |> 
      mutate(
        rank=row_number(),
        rank=round(rank/max(rank)*100,2)
      ) |> 
      rename("{{col}}_rank":=rank)
  }
  
  t <- jsalomon::perf_table_multi("nordic")|> suppressWarnings()
  
  t<-t |> mutate(symbol=jsalomon::convert_to_yf(name, exchange))
  
  first_selection<-t |> 
    score_function(perf_1m) |> 
    score_function(perf_3m) |> 
    score_function(perf_6m) |> 
    score_function(perf_y) |> 
    mutate(weighted_score= 0.25*perf_1m_rank + 0.25*perf_3m_rank+0.25*perf_6m_rank+0.25*perf_y_rank ) |>  
    filter( value_traded>=2000000  ) |> 
    
    filter(weighted_score>=80,
           close>=price_52_week_high*0.75,
           close>price_52_week_low*1.3,
           close>= sma50,
           sma50>=sma200
    ) #|> 
  #select(name,starts_with("perf"),weighted_score  ) 
  
  extra_var<-jsalomon::getstk(unique(first_selection$symbol), start_date = today()-340  )
  extra_var<-extra_var |> left_join(first_selection |> select(symbol, name))
  extra_var<-extra_var |> group_by(symbol) |> mutate(obs=n()) |> filter(obs>200) 
  
  extra_var<-extra_var |>   
    group_by(symbol) |>
    filter(!is.na(close)) |> 
    mutate(sma150=TTR::SMA(close, n=150) ,
           sma200d=TTR::SMA(close, n=200) ,
           above200= ifelse(close>=sma200d, 1,0),
           above200=ifelse(is.na(above200), 0, above200),
           above200_1m=TTR::runSum(above200, n=20),
           above200_1m=ifelse(above200_1m==20, 1,0)
    ) |> 
    filter(date==max(date)) |> 
    select(date, name, symbol, sma150,above200_1m)
  
  final<-first_selection |> left_join(extra_var) |> 
    filter(!is.na(date),
           close>=sma150, 
           above200_1m==1
    ) |> 
    mutate(name=paste0(exchange, ":", name)) |> 
    arrange(-weighted_score ) 
  
  # final<-bind_rows(final |> filter(!str_detect(name, "OSL")), final |> filter(str_detect(name, "OSL"))   )  |> 
  # pull(name)
  # 
  not_norw<-final |> filter(!str_detect(name, "OSL")) |> 
    pull(name)
  
  norw<-final |> filter(str_detect(name, "OSL")) |> 
    pull(name)
  
  final_wl<-paste0(paste0(not_norw, collapse = ","),", ### Norway, ", paste0(norw, collapse = ","))
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "Nordic_minervini_"))
  
  writeLines(final_wl, paste0("C:/Users/johan/Documents/github/avanza/watchlists/Nordic_minervini_",today(),".txt") )
  
  
}
US_minervini<-function(){
  score_function<-\(.,col){
    . |> 
      mutate({{col}}:=ifelse(is.na({{col}}),0,  {{col}} )) |> 
      arrange({{col}}) |> 
      mutate(
        rank=row_number(),
        rank=round(rank/max(rank)*100,2)
      ) |> 
      rename("{{col}}_rank":=rank)
  }
  
  t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
  
  first_selection<-t |> 
    score_function(perf_1m) |> 
    score_function(perf_3m) |> 
    score_function(perf_6m) |> 
    score_function(perf_y) |> 
    mutate(weighted_score= 0.25*perf_1m_rank + 0.25*perf_3m_rank+0.25*perf_6m_rank+0.25*perf_y_rank ) |>  
    filter( value_traded>=5000000  ) |> 
    
    filter(weighted_score>=80,
           close>=price_52_week_high*0.75,
           close>price_52_week_low*1.3,
           close>= sma50,
           sma50>=sma200
    ) #|> 
  #select(name,starts_with("perf"),weighted_score  ) 
  
  extra_var<-jsalomon::getstk(unique(first_selection$name), start_date = today()-340  )
  
  extra_var<-extra_var |> group_by(symbol) |> mutate(obs=n()) |> filter(obs>200) 
  
  extra_var<-extra_var |>   
    group_by(symbol) |> 
    mutate(sma150=TTR::SMA(close, n=150) ,
           sma200d=TTR::SMA(close, n=200) ,
           above200= ifelse(close>=sma200d, 1,0),
           above200=ifelse(is.na(above200), 0, above200),
           above200_1m=TTR::runSum(above200, n=20),
           above200_1m=ifelse(above200_1m==20, 1,0)
    ) |> 
    filter(date==max(date)) |> 
    select(date, name=symbol, sma150,above200_1m)
  
  final<-first_selection |> left_join(extra_var) |> 
    filter(!is.na(date),
           close>=sma150, 
           above200_1m==1
    ) |> 
    mutate(name=paste0(exchange, ":", name)) |> 
    arrange(-weighted_score ) |> pull(name)
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_minervini_"))
  
  writeLines(paste0(final, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_minervini_",today(),".txt") )
  
  
}

  
nordic_rs_wl<-\(){
t <- jsalomon::perf_table_multi("nordic")|> suppressWarnings()
#earnings_per_share_diluted_yoy_growth_ttm, value_traded

score_function<-\(.,col){
  . |> 
    mutate({{col}}:=ifelse(is.na({{col}}),0,  {{col}} )) |> 
    arrange({{col}}) |> 
    mutate(
      rank=row_number(),
      rank=round(rank/max(rank)*100,2)
    ) |> 
    #deparse({{col}})
    rename("{{col}}_rank":=rank)
  #rename(paste0(deparse(substitute({{col}})),"_rank"):=rank   )
  #rename_with(~paste0({{col}}, "rank"), rank)
  #rename(paste0("{{col}}","_rank"):="rank")
}

z<-t |> select(name,exchange,industry,change, perf_w,perf_1m, perf_3m, perf_6m,earnings_per_share_diluted_yoy_growth_fq,contains("ttm"),value_traded, atr, close) |>
  mutate(value_traded=round(value_traded/1000000, 1),
         name=paste0(exchange, ":", name)) |> 
  score_function(perf_w) |> 
  score_function(perf_1m ) |> 
  score_function(perf_3m ) |> 
  score_function(perf_6m ) |> 
  score_function(earnings_per_share_diluted_yoy_growth_fq) |> 
  # score_function(earnings_per_share_diluted_yoy_growth_ttm ) |> 
  # score_function(net_income_yoy_growth_ttm ) |> 
  filter(value_traded>=1,
         !change<=(-6),
         ((atr/close)*100)/0.87>=3.5
         ) |> 
  select(symbol=name,#industry, 
         contains("rank"),
         eps_yq_rank=earnings_per_share_diluted_yoy_growth_fq_rank)|> 
  #filter(perf_w_rank >=95 | perf_1m_rank>=92 | perf_3m_rank>=92 | perf_6m_rank >=90 ) |> 
  mutate(score=0.2*perf_w_rank + 0.2*perf_1m_rank + 0.2*perf_3m_rank + 0.2*perf_6m_rank + 0.2*eps_yq_rank) |> 
  filter(score>=80) |> 
  arrange(-score) |> 
  pull(symbol)
 



file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "nordic_rs_"))

writeLines(paste0(paste0(z[ !str_detect(z, "OSL")], collapse = ","),
                  ", ### Norge , ",
                  paste0(z[ str_detect(z, "OSL")], collapse = ","),
                  collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/nordic_rs_",today(),".txt") )
}

nordic_adr_wl<-\(){
all_file_path<-list.files("C:/Users/johan/Documents/github/avanza/high_adr_plot_nordic", full.names = T)
wl<-tibble(f=all_file_path, mod=format(file.info(all_file_path)$mtime)) |> 
    mutate(mod=as.Date(mod),
           symbol=str_remove_all(f, "C:/Users/johan/Documents/github/avanza/high_adr_plot_nordic/|_plot.pdf"),
           symbol=str_replace(symbol, "-","_"),
           exchange=case_when(str_detect(symbol, "\\.ST") ~"OMXSTO",
                              str_detect(symbol, "\\.OL") ~"OSL",
                              str_detect(symbol, "\\.CO") ~"OMXCOP",
                              str_detect(symbol, "\\.HE") ~"OMXHEX"
                              ),
           symbol=str_remove( paste0(exchange, ":", symbol), "\\.[A-Z][A-Z]")
           ) |> 
  filter(mod==max(mod),
         !str_detect(symbol,"result")
         ) 

get_adr_from_pdf<-function(x,adrExtract="(?<=Adr: )\\d+([.,]\\d+)?",voldolExtract ="(?<=Vol.Dollar avg.: )\\d+([.,]\\d+)?"){
  z<-tibble(f=x,
            adr=str_extract(pdftools::pdf_text(pdf = x), adrExtract),
            dollarvol=str_extract(pdftools::pdf_text(pdf = x),voldolExtract)
  )
  z
  
  
}


adr_frame<-tibble(data.table::rbindlist( map( all_file_path,get_adr_from_pdf ) ) ) |> 
  mutate(adr=as.numeric(adr),
         dollarvol=as.numeric(dollarvol)
  )

wl<-wl |> left_join(adr_frame ) |> arrange(-adr)#|>  pull(symbol)

OSL<- wl |> filter(exchange=="OSL") |> pull(symbol)

not_OSL<- wl |> filter(exchange!="OSL") |> pull(symbol)

print(max(wl$mod))



#wl<-wl |>  pull(symbol)

file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "nordic_adr_"))

writeLines(paste0(paste0(not_OSL, collapse = ","),
                  ", ### Norge , ",
                  paste0(OSL, collapse = ","),
                  collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/nordic_adr_",today(),".txt") )
}



nordic_ipo_wl<-\(days_since=1200, at_least_since_days=200){
  #load("C:/Users/johan/Documents/github/avanza/ipo_date.Rdata")
  
  score_function<-\(.,col){
    . |> 
      mutate({{col}}:=ifelse(is.na({{col}}),0,  {{col}} )) |> 
      arrange({{col}}) |> 
      mutate(
        rank=row_number(),
        rank=round(rank/max(rank)*100,2)
      ) |> 
      #deparse({{col}})
      rename("{{col}}_rank":=rank)
    #rename(paste0(deparse(substitute({{col}})),"_rank"):=rank   )
    #rename_with(~paste0({{col}}, "rank"), rank)
    #rename(paste0("{{col}}","_rank"):="rank")
  }
  ipo_since<-today()-days_since
  alsd<-today()-at_least_since_days
  t <- jsalomon::perf_table_multi("nordic")|> suppressWarnings()
  #load("C:/Users/johan/Documents/github/avanza/ipo_date_df.Rdata")
  load("C:/Users/johan/Documents/github/avanza/ipo_date_nordic.Rdata")
  
  
  
  z<-t |>mutate(symbol=jsalomon::convert_to_yf(str_replace_all( name, "\\.", "-"), exchange),
                name=paste0(exchange, ":", name)
                ) |> 
    left_join(
    all_ipos_nordic 
  ) |> 
    score_function(perf_w) |> 
    score_function(perf_1m) |> 
    score_function(perf_3m) |> 
    score_function(perf_6m) |> 
    filter(IPO_date  >=ipo_since,
           IPO_date<=alsd,
           average_volume_30d_calc*sma30>=2000000,
           ((atr/close)*100)/0.87>=4.55,
           close>=5
           
    ) |> 
    select(name, contains("rank")) |> 
    pivot_longer(cols = 2:5,names_to = "h" ) |> 
    filter(value>=0) |> 
    distinct(name) |> pull(name)
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "nordic_recent_ipos_"))
  
  writeLines(paste0(z, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/nordic_recent_ipos_",today(),".txt") )
}




us_adr_wl<-\(){

extract_symbols_from_plots<-\(PATH="C:/Users/johan/Documents/github/avanza/high_adr_plot_nasdaq", exchange="NASDAQ"){
all_file_path<-list.files(PATH, full.names = T)
wl<-tibble(f=all_file_path, mod=format(file.info(all_file_path)$mtime)) |> 
  mutate(mod=as.Date(mod))
file.remove( wl |> filter(!mod==max(mod)) |> pull(f) )
all_file_path<-list.files(PATH, full.names = T)
get_adr_from_pdf<-function(x,adrExtract="(?<=Adr: )\\d+([.,]\\d+)?",voldolExtract ="(?<=Vol.Dollar avg.: )\\d+([.,]\\d+)?"){
  z<-tibble(f=x,
            adr=str_extract(pdftools::pdf_text(pdf = x), adrExtract),
            dollarvol=str_extract(pdftools::pdf_text(pdf = x),voldolExtract)
  )
  z
  
  
}
# get_adr_from_pdf("C:/Users/johan/Documents/github/avanza/high_adr_plot_nasdaq/ACLX_plot.pdf")
# pdftools::pdf_text(pdf = "C:/Users/johan/Documents/github/avanza/high_adr_plot_nasdaq/ACLX_plot.pdf")

adr_frame<-tibble(data.table::rbindlist( map( all_file_path,get_adr_from_pdf ) ) ) |> 
  mutate(adr=as.numeric(adr),
         dollarvol=as.numeric(dollarvol)
  )#|> 
  #arrange(-adr) |> filter(adr>=0,dollarvol>=50) |> pull(stock)

z<-wl |>left_join(adr_frame) |> 
  mutate(
         symbol=str_remove_all(f, glue::glue("{PATH}/|_plot.pdf")),
         symbol=paste0(exchange, ":",str_replace(symbol, "-","_"))
  ) |> 
  filter(mod==max(mod),
         !str_detect(symbol,"result|Result") ) |> 
  arrange(-adr) #|> 
  #pull(symbol)
 #z<-paste0(z, collapse = ",")
 z
}

nas<-extract_symbols_from_plots(PATH="C:/Users/johan/Documents/github/avanza/high_adr_plot_nasdaq", exchange="NASDAQ")
nyse<-extract_symbols_from_plots(PATH="C:/Users/johan/Documents/github/avanza/high_adr_plot", exchange="NYSE")

watchlist<-bind_rows(nas, nyse) |> arrange(-adr) |> pull(symbol)

file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_adr_"))

writeLines(paste0(watchlist, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_adr_",today(),".txt") )
}

us_ipo_wl<-\(days_since=1000, at_least_since_days=240){
  #load("C:/Users/johan/Documents/github/avanza/ipo_date.Rdata")
  
  score_function<-\(.,col){
    . |> 
      mutate({{col}}:=ifelse(is.na({{col}}),0,  {{col}} )) |> 
      arrange({{col}}) |> 
      mutate(
        rank=row_number(),
        rank=round(rank/max(rank)*100,2)
      ) |> 
      #deparse({{col}})
      rename("{{col}}_rank":=rank)
    #rename(paste0(deparse(substitute({{col}})),"_rank"):=rank   )
    #rename_with(~paste0({{col}}, "rank"), rank)
    #rename(paste0("{{col}}","_rank"):="rank")
  }
  ipo_since<-today()-days_since
  alsd<-today()-at_least_since_days
  t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
  #load("C:/Users/johan/Documents/github/avanza/ipo_date_df.Rdata")
  load("C:/Users/johan/Documents/github/avanza/ipo_date.Rdata")
  
  
  
  z<-t |> left_join(
    all_ipos |> rename(name=symbol)
  ) |> 
    score_function(perf_w) |> 
    score_function(perf_1m) |> 
    score_function(perf_3m) |> 
    score_function(perf_6m) |> 
    filter(IPO_date  >=ipo_since,
           IPO_date<=alsd,
           average_volume_30d_calc*sma30>=5000000,
           ((atr/close)*100)/0.87>=4.55,
           close>=5
           
    ) |> 
    select(name, contains("rank")) |> 
    pivot_longer(cols = 2:5,names_to = "h" ) |> 
    filter(value>=90) |> 
    distinct(name) |> pull(name)
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_recent_ipos_"))
  
  writeLines(paste0(z, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_recent_ipos_",today(),".txt") )
}

us_premarket_wl<-\(){
  t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
  z<-t |> mutate(
    name=paste0(exchange, ":", name),
    pre_dollar_vol=premarket_close*premarket_volume  ) |> 
    arrange(-pre_dollar_vol) |> 
    filter(close>=5) |> 
    mutate(pre_dollar_vol_rank=row_number()) |> 
    arrange(-premarket_gap ) |> 
    mutate(premarket_gap_rank = row_number()) |> 
    arrange(-premarket_change ) |> 
    mutate(premarket_change_rank=row_number()) |> 
    filter(pre_dollar_vol_rank<=10 |
             premarket_gap_rank<=10|
             premarket_change_rank<= 10
           
    ) |> 
    pull(name)
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_premarket_"))
  
  writeLines(paste0(z, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_premarket_",today(),".txt") )
  
}

US_darvas_wl<-\(){
  t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
  darvas<-t |> 
    filter(
      price_52_week_high/price_52_week_low>=2,
      close>=(8/10)*price_52_week_high,
      close>2,
      close>sma50,
      value_traded>=2000000,
      ((atr/close)*100)/0.87>=3.5
    ) |> 
    transmute(name=paste0(exchange, ":",name), 
              close, high_1m, 
              from_high =((close-high_1m)/high_1m)*100,
              rel_vol=round(volume/average_volume_30d_calc,1)
              
    ) |> 
    arrange(-rel_vol) |> 
    pull(name)
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "Darvas_US_"))
  
  writeLines(paste0(darvas, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/Darvas_US_",today(),".txt") )
}
nordic_darvas_wl<-\(){
  t <- jsalomon::perf_table_multi("nordic")|> suppressWarnings()
  darvas<-t |> 
    filter(
      price_52_week_high/price_52_week_low>=2,
      close>=(8/10)*price_52_week_high,
      close>2,
      close>sma50,
      value_traded>=1000000,
      
      ((atr/close)*100)/0.87>=3.5
    ) |> 
    transmute(name=paste0(exchange, ":",name), 
              close, high_1m, 
              from_high =((close-high_1m)/high_1m)*100,
              rel_vol=round(volume/average_volume_30d_calc,1)
              
    ) |> 
    arrange(-rel_vol) |> 
    pull(name)
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "Nordic_Darvas_"))
  
  writeLines(paste0(darvas, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/Nordic_Darvas_",today(),".txt") )
}


## sorted ma (NOT DONE)

us_sorted_ma<-\(){
  t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
  
  
  sorted_ma_wl<-  t |> filter( sma10>=sma20, sma20>=sma50, sma50>=sma200,#, sma10/sma20<=1.005
               average_volume_30d_calc*sma30>=5000000,
               ((atr/close)*100)/0.87>=4.55
               ) |>
    mutate(rel_vol=volume/average_volume_30d_calc) |> 
    arrange(-rel_vol) 
  
sorted_ma_wl<- sorted_ma_wl |> pull(name)
  
file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_sorted_ma"))
  
writeLines(paste0(sorted_ma_wl, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_sorted_ma_",today(),".txt") )
  
  
}




us_topic_wl<- \(topic="artificial|quant|machine learning|\\bai\\b|computing" ){
  
  check_updates_and_loading_desc_data<-\(){
    
    check_newest<-tibble(files=list.files("C:/Users/johan/Documents/github/avanza/description_data/", full.names = T, pattern = ".Rdata"))|> 
      mutate(date=ymd(str_remove_all(files, "C:/Users/johan/Documents/github/avanza/description_data/desc_df_|.Rdata")) ) |> 
      filter(date==max(date)) |> 
      pull(date)
    
    if (today()- check_newest>=10 ){
      
      get_desc<-function(x){
        Sys.sleep(0.5)
        url<-paste0("https://stockanalysis.com/stocks/",x,"/company/")
        xp<-'//*[contains(concat( " ", @class, " " ), concat( " ", "col-span-1", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//span'
        s<-rvest::read_html(url)
        d<-s |> #rvest::html_element(".mb-5 text-base md:text-lg [&>p]:mb-5") #rvest::html_text2("h2") #rvest::html_element(".h2" ) |>
          rvest::html_text(".mb-5 text-base md:text-lg [&>p]:mb-5")
        d
        
        c<-d |> str_split("Chart\n\t\n\n|\n\t\n\tCountry")
        c<-c[[1]][2]
        c<-tibble(symbol=x, desc=c)
        #c |> mutate(desc=str_remove(desc, "Company Description\\n\\t\\t"))
        c
      }
      
      load("C:/Users/johan/Documents/github/avanza/desc_df.Rdata")
      t<-jsalomon::perf_table_multi() |>  suppressWarnings()
      already_have<-desc_df |> mutate(symbol=toupper(symbol#str_replace_all(symbol, "\\.", "_")
      )) |> pull(symbol)
      
      do_not_have<-t |> filter(!name %in% already_have )
      
      
      new_desc_df<-map_dfr(tolower(unique(do_not_have$name)), 
                           function(x){
                             temp<- try(get_desc(x),
                                        silent=TRUE)
                             if(!inherits(temp, "try-error")) temp
                           }
      )
      
      
      if(is_empty(new_desc_df) ){
        print("No new descriptions")
        desc_df<-new_desc_df
      }else{
        desc_df<-desc_df |> bind_rows(new_desc_df)
      }
      
      
     save(desc_df, file=paste0("C:/Users/johan/Documents/github/avanza/description_data/desc_df_",today(),".Rdata"))
      
    }
    
    newest_data<-tibble(files=list.files("C:/Users/johan/Documents/github/avanza/description_data/", full.names = T, pattern = ".Rdata"))|> 
      mutate(date=ymd(str_remove_all(files, "C:/Users/johan/Documents/github/avanza/description_data/desc_df_|.Rdata")) ) |> 
      filter(date==max(date)) |> 
      pull(files)
    
    load(newest_data)
    
    desc_df
    
  }
  
  desc_df<-check_updates_and_loading_desc_data()
  
#load("C:/Users/johan/Documents/github/avanza/desc_df.Rdata")
  
  # desc_df |> mutate(desc=str_remove(desc, "Company Description\n\t\t"),
  #                   desc=str_remove(desc, "\n\n\t.*")
  #                   ) |> sample_n(1) |> pull(desc) 
  # 
detected_tickers<-  
  desc_df |> filter(str_detect(tolower(desc),topic)) |> 
  group_by(symbol) |> 
  separate_longer_delim(desc, delim = " ") |> 
  filter(str_detect(tolower(desc), topic )) |> 
    summarize(n=n()) |> arrange(-n)
# detected_tickers |> filter(symbol=="nvda") 
# desc_df|> filter(symbol=="nvda") |> pull(desc) |> tolower()
#detected_tickers<-desc_df |> filter(str_detect(desc, topic))
t <- jsalomon::perf_table_multi("america")|> suppressWarnings()
z<-t |> filter(tolower(name) %in% detected_tickers$symbol,
            value_traded>=1000000,
            ((atr/close)*100)/0.87>=3.5
            ) |> 
  arrange(-perf_1m ) |> 
  mutate(name=paste0(exchange, ":", name),
         mc_billions=market_cap_basic/1000000000) 


below_1b<-z|> filter(mc_billions<=1) |> 
  pull(name)
below_10b<-z|> filter(mc_billions>1,mc_billions<=10) |> 
  pull(name)
above_10b<-z|> filter(mc_billions>10) |> 
  pull(name)

wl<-paste0("### Below 1 bill, ", paste0(below_1b, collapse = ",") ,", ### Below 10 bill, ",paste0(below_10b, collapse = ","),", ### Above 10 bill, " ,paste0(above_10b, collapse = ",") )

topic_adj<-str_replace_all(topic,"\\||\\s|\\\\","_" )

P<-paste0("US_topic_wl_",topic_adj)
file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = P))

writeLines(wl, paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_topic_wl_",topic_adj,"_",today(),".txt") )

}

us_mansi_wl<-\(){
t <- jsalomon::perf_table_multi("america")|> suppressWarnings()

mansi<-t |> mutate(adr_proxy= ((atr/close)*100)/0.87,
            rel_vol=volume/average_volume_30d_calc,
            bullish_half=ifelse(close>=(high+low)/2, 1 ,0),
            name=paste0(exchange, ":", name)
            ) |> 
  filter(change>=2*adr_proxy ,
         rel_vol>=2,
         bullish_half==1,
         close>=5,
         average_volume_30d_calc*sma30>=2000000
         ) |>
  arrange(-rel_vol) |> 
  pull(name)

file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_Mansi_"))

writeLines(paste0(mansi, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_Mansi_",today(),".txt") )

}
nordic_mansi_wl<-\(adr_crit=1.5, rel_vol_crit=2){
  t <- jsalomon::perf_table_multi("nordic") |> suppressWarnings()
  
  mansi<-t |> mutate(
    
    adr_proxy= ((atr/close)*100)/0.87,
    rel_vol=volume/average_volume_30d_calc,
    bullish_half=ifelse(close>=(high+low)/2, 1 ,0),
    name=paste0(exchange, ":", name)
  ) |> 
    filter(change>=adr_crit*adr_proxy ,
           rel_vol>=rel_vol_crit,
           bullish_half==1,
           close>=5,
           average_volume_30d_calc*sma30>=1000000
    ) |>
    arrange(-rel_vol) |> 
    pull(name)
  
  #is_empty(mansi)  
  
  if(is_empty(mansi) ){
    
    print("No stocks in Mansi scan today!")
  }else{
    
    file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "Nordic_Mansi_"))
    
    writeLines(paste0(mansi, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/Nordic_Mansi_",today(),".txt") )
    print(paste0(length(mansi), " stocks in Mansi scan today!"))
    
  }
}





finviz_screener<-\(url="https://finviz.com/screener.ashx?v=141&f=cap_small,sh_avgvol_o300,sh_relvol_o0.25,ta_change_u2,ta_highlow50d_b0to10h,ta_perf_13w30o,ta_volatility_wo3&ft=4&o=-relativevolume"){
  url_screener<-url
  #https://finviz.com/screener.ashx?v=141&f=cap_smallover,sh_avgvol_o500,sh_curvol_o100,ta_perf_1w20o,ta_volatility_wo3&ft=4&o=-relativevolume
  x<-url_screener %>% 
    rvest::read_html() |> 
    #rvest::html_element(xpath = '//*[(@id = "screener-table")]//td') %>% 
    rvest::html_table()
  x<-x[[22]][-1,] |> tibble() |> janitor::remove_empty("cols")
  colnames(x)<-x[1,]
  x<-x |> janitor::clean_names() |> 
    slice(-1)|> 
    mutate(across(contains(c("perf","volatilit","change" )), ~as.numeric(str_replace(., "%","")) ))
  x
}

#df<-finviz_screener() #|> select(ticker, change,rel_volume  ,perf_week, perf_month, perf_quart, perf_half, perf_year )

us_relvol<-\(url="https://finviz.com/screener.ashx?v=141&f=cap_small,sh_avgvol_o300,sh_relvol_o0.25,ta_change_u2,ta_highlow50d_b0to10h,ta_perf_13w30o,ta_volatility_wo3&ft=4&o=-relativevolume"){
  df<-finviz_screener(url)
  t<-jsalomon::perf_table_multi() |> suppressMessages() |> suppressWarnings()
  
  wl<- df |> left_join(t |> select(ticker=name, exchange) |> mutate(name=paste0(exchange,":",ticker)) ) |> 
    pull(name)
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_relvol_"))
  
  writeLines(paste0(wl, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_relvol_",today(),".txt") )
  
}

us_strength<-\(url="https://finviz.com/screener.ashx?v=141&f=cap_smallover,sh_avgvol_o500,sh_curvol_o100,ta_perf_1w20o,ta_volatility_wo3&ft=4&o=-relativevolume"){
  df<-finviz_screener(url)
  t<-jsalomon::perf_table_multi() |> suppressMessages() |> suppressWarnings()
  
  wl<- df |> left_join(t |> select(ticker=name, exchange) |> mutate(name=paste0(exchange,":",ticker)) ) |> 
    pull(name)
  
  file.remove( list.files("C:/Users/johan/Documents/github/avanza/watchlists/", full.names = T ,pattern = "US_strength_"))
  
  writeLines(paste0(wl, collapse = ","), paste0("C:/Users/johan/Documents/github/avanza/watchlists/US_strength_",today(),".txt") )
  
}

swedish_newsfeed<-\(VeryPos="återkommande|positiv|uppvärderar|värderar upp|reviderar upp|uppreviderar|framtidsutsikt|uppgraderar|rekomend|bud|starkare|mer än väntat|rekord|best|bäst|decision|beslut|godkän|uppköp|omvänd|ökar omsättning|kontrakt|får order|signs an order|höjer riktkurs|reviderar upp|ökning|beställning",
                    Pos="tillväxt|vd|ökar sitt aktieinnehav|insider|rykte|återköp|köper aktier för|steg|avtal|sammarbet|utdelning",
                    Neg="vinstvarn|tar in|emitter|emission",
                    Specific_Stocks= "calliditas|synact|artificial solution|kempower|saab|fractal gaming|alleima|astor|viaplay|gaming innovation"
                      ){

  scrape_data<-function(x){
    url<-x
    s<-url |> rvest::read_html() |> 
      rvest::html_element(".feedArticleList") |>    rvest::html_text()
    k<-s  |> str_split(pattern="(?<=\\d)\r\n(?=\\s)") 
    k
    
  }
  
  cleaning_scraped_data<- function(scraped_data_vector, type ){
    y<-tibble(rev=scraped_data_vector[[1]]) |> 
      filter(#!rev=="\r",
        !rev=="") |> 
      mutate(rev=str_remove_all(rev, "\r\n"),
             rev=str_squish(rev),
             about=str_extract(rev, ".+?(?=\\:|\\s\\-\\s|öppnar)"),
             what=str_extract(rev, "(?<=\\:|\\s\\-\\s).+(?=\\(\\D)"),
             newsSource=str_extract(rev, "Finwire|Direkt"),
             Timestamp=str_extract(rev, "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d\\s\\d\\d\\:\\d\\d"),
             Timestamp=lubridate::ymd_hm(Timestamp),
             type=type
      ) |> 
      filter(  !is.na(Timestamp))
    
    y
    
  }
  
  telegram<-scrape_data("https://www.placera.se/placera/telegram.8.html")
  pm<-scrape_data("https://www.placera.se/placera/pressmeddelanden.9.html")
  oviga<-scrape_data("https://www.placera.se/placera/ovriga-nyheter.9.html")
  
  newsfeed<-bind_rows(cleaning_scraped_data(telegram,"telegram"),
                      cleaning_scraped_data(pm,"pm"),
                      cleaning_scraped_data(oviga, "analys")
  ) 
  
  very_pos<- VeryPos
  
  pos<-Pos
  
  neg<- Neg
  
  specific_stocks<-Specific_Stocks
  
  trigger_words<- paste0(very_pos,"|",pos,"|",neg, "|",specific_stocks)
  
  lookup<-tibble(text=str_split(trigger_words, "\\|")[[1]]) |> mutate(new= paste0("**", text, "**"))
  
  newsfeed_trigger<-newsfeed |> filter(str_detect(tolower(rev), trigger_words)) |> 
    mutate(keywords=str_extract(tolower(rev), trigger_words) )|>  
    arrange(desc(Timestamp))
  
  textcolor<-function(x, color){
    paste0("<span style='color:",color,";'>**",x,"**</span>")
    
  }
  
  newsfeed_trigger<-newsfeed_trigger |>
    mutate(#rev2=str_replace_all(tolower(rev), keywords, paste0("**", keywords, "**") ),
      rev=str_replace_all(tolower(rev), keywords, 
                          case_when(keywords %in% str_split(very_pos, "\\|")[[1]] ~ textcolor(str_replace_all(keywords," ","-"), "#2551C9"),
                                    keywords %in% str_split(neg, "\\|")[[1]] ~ textcolor(str_replace_all(keywords," ","-"), "red"),
                                    keywords %in% str_split(specific_stocks, "\\|")[[1]] ~ textcolor(str_replace_all(keywords," ","-"), "purple"),
                                    TRUE ~textcolor(str_replace_all(keywords," ","-"), "black")
                          )),
      id=row_number())
  
  table_data<-newsfeed_trigger |>  
    group_by(id) |> 
    tidytext::unnest_tokens("text",rev,  token = stringr::str_split, pattern = " ") |> 
    left_join(lookup) |> 
    mutate(new=if_else(is.na(new), text, new)) |> 
    select(-text) |> 
    nest(data =new) %>%
    mutate(NEW_TEXT = map(data, unlist), 
           NEW_TEXT = map_chr(NEW_TEXT, paste, collapse = " "),
           NEW_TEXT = paste0(NEW_TEXT, ".")) |> 
    left_join(newsfeed_trigger |> select(id, Timestamp, type, about))
  
  news<-table_data |> ungroup() |> 
    select(Telegram=NEW_TEXT ,Timestamp  , type) |> 
    gt::gt() |>
    gt::cols_width(
      starts_with("Telegram") ~ px(700),
      everything() ~ px(120)
    ) |> 
    
    gt::tab_options(
      column_labels.background.color = "white",
      table.width = "1500px",
      table.border.top.width = gt::px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = gt::px(3),
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = gt::px(3),
      table.font.size = 10,
      heading.align = "left"
    ) |>
    gt::fmt_markdown(columns = c(Telegram))
  
  news
  
}



net_new_hi<-\(PATH="C:/Users/johan/Documents/github/avanza/market_breath_data"){
  
  f<-list.files(PATH, pattern = ".csv", full.names = T )
  
  
  z<-map_dfr(f,read_csv) |> arrange(date)
  
  z |> select(exchange,date       , contains("high"),contains("low"), 
              -contains("names")) |>  
    mutate(net_high_all=new_highall_pct-new_lowall_pct,
           net_high_52w=new_high52w_pct-new_low52w_pct ,
           net_high_3m=new_high3m_pct -new_low3m_pct ,
           net_high_1m=new_high1m_pct -new_low1m_pct) |> 
    select(exchange, date ,contains("net")) |> 
    pivot_longer(cols = 3:6) |>
    filter(!is.na(value)) |> 
    mutate(name=str_replace_all(name, "_", " ")) |>
    
    filter(str_detect(name, "52w")
           #, exchange=="NASDAQ"
    ) |> 
    
    ggplot(aes(date, value, #group=exchange, 
               color=ifelse(value>=0, "green", "red"),
               fill=ifelse(value>=0, alpha("green",0.5), alpha("red",0.5))
    ))+
    geom_hline(yintercept = 0, lty=2, color="grey")+
    geom_col()+
    geom_hline(data = ~. |> filter(date==max(date)),
               aes(yintercept = value,color=ifelse(value>=0, "green", "red")), lty=3)+
    geom_label(data = ~. |> filter(date==max(date)),
               aes(label=paste0(round(value,1)), color="white")
               ,x=Inf
               ,hjust=1
               ,size=3
    )+
    #geom_line()+
    #geom_point()+
    scale_color_identity()+
    scale_fill_identity()+
    bdscale::scale_x_bd(business.dates=z$date, max.major.breaks=10, labels=scales::date_format("%b-'%y"))+
    facet_wrap(~exchange, ncol=1, scales = "free_y")+
    geom_smooth(aes(group=1, color="yellow"), se=F, linewidth=0.5 )+
    labs(title="Net New Highs",subtitle= "#stocks making new highs % - #stocks making new lows %",y=" ", x="", color="", 
         caption = paste0(max(z$date)))+
    jsalomon::theme_bors()+
    theme(legend.position = "bottom",
          legend.text = element_text(color = "white"
          ),
          axis.title.x = element_blank()
          
    )
}

qullamaggie_plot<- function(stk="SPY", start= "2020-06-01"){
  df<-jsalomon::getstk(stk, start_date =start) |> 
    na.omit() |> 
    mutate(
      ema10=TTR::EMA(close, n=10),
      ema20=TTR::EMA(close, n=20),
      ema50=TTR::EMA(close, n=50),
      sma20=TTR::SMA(close, n=20),
      sma10=TTR::SMA(close, n=10),
      ema_color=case_when(ema10>=ema20 & ema10>=lag(ema10) & ema20>=lag(ema20) ~"green",
                          ema10>=ema20 & (ema10<lag(ema10) | ema20<lag(ema20)) ~"yellow",
                          ema10<ema20 ~"red"
      ),
      sma_color=case_when(sma10>=sma20 & sma10>=lag(sma10) & sma20>=lag(sma20) ~"green",
                          sma10>=sma20 & (sma10<lag(sma10) | sma20<lag(sma20)) ~"yellow",
                          sma10<sma20 ~"red"
      ),
      ext_ema10= ((close/ema10)-1)*100 ,
      ext_ema20= ((close/ema20)-1)*100 ,
      ext_ema50= ((close/ema50)-1)*100 
    ) 
  
  p1<-df |> 
    ggplot(aes(date, close))+
    geom_vline(data = ~. |> filter(sma_color!=lag(sma_color) ), 
               aes(xintercept = date
                   ,color=sma_color
               ), 
               linewidth=0.1)+
    geom_line(linewidth=0.3, color="grey80")+
    geom_line(linewidth=1,aes(date, sma10, color=sma_color, group=1))+
    geom_point(data = ~. |> filter(date==max(date)), aes(date, sma10, color=sma_color, group=1))+
    bdscale::scale_x_bd(business.dates=df$date, max.major.breaks=10, labels=scales::date_format("%b-'%y"))+
    scale_color_identity()+
    jsalomon::theme_bors()+
    theme(axis.title.x =  element_blank() )+
    labs(title = stk,
         x="",y="")
  
  #p2<-
  plot_ext<-\(col){
    df |> 
      mutate(extreme_high=if_else({{col}}>= quantile({{col}}, 0.95, na.rm=T),{{col}}, NA ),
             extreme_low=if_else({{col}}<= quantile({{col}}, 0.05, na.rm=T),{{col}}, NA )
      ) |> 
      ggplot(aes(date,{{col}}))+
      geom_vline(data = ~. |> filter(sma_color!=lag(sma_color) ), 
                 aes(xintercept = date
                     #,color=sma_color
                 ), color="grey80",
                 linewidth=0.1)+
      #scale_color_identity()+
      geom_hline(yintercept = 0, color="grey80")+
      geom_hline(aes(yintercept = max({{col}}, na.rm=T)), color="red", linewidth=0.2)+
      geom_hline(aes(yintercept = min({{col}}, na.rm=T)), color="green", linewidth=0.2)+
      geom_line(aes( color= {{col}}))+
      geom_point(aes(date, extreme_high), color="red")+
      geom_point(aes(date, extreme_high), color="white", size=0.2)+
      geom_point(aes(date, extreme_low), color="green")+
      geom_point(aes(date, extreme_low), color="white", size=0.2)+
      bdscale::scale_x_bd(business.dates=df$date, max.major.breaks=10, labels=scales::date_format("%b-'%y"))+
      scale_color_gradient2(trans= "pseudo_log", low = "green", high = "red", mid = "yellow", midpoint = 0 )+
      #scale_color_identity()+
      jsalomon::theme_bors()+
      theme(axis.title.x =  element_blank() )+
      labs(
        x="")
  }
  p2<-plot_ext(ext_ema10)+theme(axis.text.x = element_blank() )
  p3<-plot_ext(ext_ema20)+theme(axis.text.x = element_blank() )
  p4<-plot_ext(ext_ema50)
  
  design<- 
    "A
A
A
A
B
C
D"
  
  c<-patchwork::wrap_plots(p1,p2,p3,p4, design =design )
  c
  #geom_line(aes(y=ema20),color="blue")#+
  #geom_line(aes(y=sma20), color="red")
}

}






####

nordic_earnings_WL()
nordic_rs_wl()
nordic_adr_wl()
nordic_darvas_wl()
nordic_mansi_wl()
nordic_new_highs()

nordic_bb_break()

Nordic_minervini()


nordic_ipo_wl()


#nordic_premarket_wl()

us_earnings_WL()
us_rs_wl()
us_new_highs()
us_adr_wl()


us_ipo_wl()
us_topic_wl(topic="artificial|quant|quantum|machine learning|\\bai\\b|computing")
us_topic_wl(topic="semiconductor")
us_topic_wl(topic="uranium")
us_topic_wl(topic="china|chinese")
us_topic_wl(topic="lithium")
us_topic_wl(topic="\\boil\\b|\\bcoal\\b")

us_topic_wl(topic="solar")

us_mansi_wl()
us_bb_break()

us_premarket_wl()

US_minervini()


us_sorted_ma()


twenty_in_1m()
twenty_in_1m(country = "america")

us_strength()
us_relvol() #|> suppressWarnings()


nordic_largest_relvol()
nordic_recent_movers(dir="down")
swedish_newsfeed()

net_new_hi()
qullamaggie_plot("SPY")
#qullamaggie_plot("AOI.ST")
# str_extract_all(d, "(?<=Company Description).+" )
# str_extract_all(d, "(?<=Apple).*" )
# s<-s |> rvest::html_elements(xpath = xp ) |> rvest::html_text()
# z<-tibble(symbol=x, ipo= paste0(s, collapse = ": "))
# z

