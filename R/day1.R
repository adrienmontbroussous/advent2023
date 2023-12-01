library(dplyr)
input <- read.delim("../input/day1_.txt",header = FALSE)

extract_first_and_last <- function(character){
  r=c()
  for(c in strsplit(character,"")[[1]]){
    if(!is.na(as.numeric(c))){
      r=rbind(r,as.numeric(c))
    }
  }
  return(10*r[1]+r[length(r)])
}

#part 1
sum(sapply(input$V1,extract_first_and_last))


chercher_nombre <- function(chaine,curseur){
  if(nchar(chaine)>=curseur+2){
    comp=substr(chaine,curseur,curseur+2)
    result=case_match(
      comp,
      "one" ~ 1,
      "two" ~ 2,
      "six" ~ 6,
      .default = NULL
    )
    if (!is.na(result)){
      return (result)
    }
  }
  if(nchar(chaine)>=curseur+3){
      comp=substr(chaine,curseur,curseur+3)
      result=case_match(
        comp,
        "four" ~ 4,
        "five" ~ 5,
        "nine" ~ 9,
        .default = NULL
      )
      if (!is.na(result)){
        return (result)
      }
    
  }
  if(nchar(chaine)>=curseur+4){
    comp=substr(chaine,curseur,curseur+4)
    result=case_match(
      comp,
      "three" ~ 3,
      "seven" ~ 7,
      "eight" ~ 8
    )
    if (!is.na(result)){
      return (result)
    }
  }
  return(NA)
}

extract_first_and_last_advanced <- function(my_string){
  r=c()
  for(i in seq(1:nchar(my_string))){
    my_character=substr(my_string,i,i)
    if(!is.na(as.numeric(my_character))){
      r=rbind(r,as.numeric(my_character))
    }
    else{
      nb_char=chercher_nombre(my_string,i)
      if(!is.na(nb_char)){
        r=rbind(r,nb_char)
      }
    }
  }
  return(10*r[1]+r[length(r)])
}

#part 2 
sum(sapply(input$V1,extract_first_and_last_advanced))
