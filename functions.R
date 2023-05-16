count_missing = function(dat,colname){
  x = dat %>%
    dplyr::mutate(missing = ifelse(is.na(.data[[colname]]),"missing","nonmissing")) %>%
    dplyr::count(missing) %>%
    dplyr::mutate(percent = n/sum(n)*100)
  colnames(x)[1] = colname
  x
}

make_hist = function(dat,colname){
  p=ggplot2::ggplot(data = dat, aes(as.numeric(.data[[colname]])))+
    geom_histogram(alpha=0.5,fill="orange",color="black",binwidth=1)+
    theme_minimal()+
    labs(x=colname,y="count")
    print(p)
}

make_density = function(dat,colname){
  p=ggplot2::ggplot(data = dat, aes(as.numeric(.data[[colname]])))+
    geom_density(alpha=0.5,fill="orange",color="black")+
    theme_minimal()+
    labs(x=colname,y="count")
  print(p)
}

make_barchart = function(dat,colname){
  p=ggplot2::ggplot(data = dat, aes(.data[[colname]]))+
    geom_bar(alpha=0.5,fill="orange",color="black")+
    theme_minimal()+
    labs(x=colname,y="count") 
  print(p)
}

date_from_year = function(x){
  as.Date(paste0("01-01-",x),format = "%d-%m-%Y")
}

delta_dates_years = function(date1,date2){
  as.numeric(( date1-date2 )) / 365.25
}

filter_na = function(dat,colname){
  message("Filtering out NAs for ",colname)
  message("Before filtering n: ",nrow(dat))
  dat = dat %>%
    filter(!is.na(.data[[colname]]))
  message("After filtering n: ",nrow(dat))
  dat
}

get_prop = function(dat,colname){
  dat %>%
    dplyr::count(.data[[colname]]) %>%
    dplyr::mutate(prop = n/sum(n))
}

get_coefs_from_cox = function(x,model_name){
  res = summary(x)$coefficients[rownames(summary(x)$coefficients)=="depresseddepressed",]
  df = data.frame(
    model = model_name,
    HR = exp(res[1]),
    lower_ci = exp(res[1] - 1.96 * res[3]),
    upper_ci = exp(res[1] + 1.96 * res[3]),
    P = res[5])
  df
}
