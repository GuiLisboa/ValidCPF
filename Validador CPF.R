
formata_CPF <- function(strNum)
{
  library(stringr)
  if(str_length(strNum) != 11) {
    stop("Número inválido!")
  }
  
  First <- as.integer(unlist((strsplit(str_sub(strNum,start = 1, end = 9), split = "")))) * 10:2
  
  if(sum(First) %% 11 < 2){
    if(str_sub(strNum,10,10) != 0){
      stop("Digito verificador errado")
    }
  }else{
    if(!(11-sum(First) %% 11) == as.numeric(str_sub(strNum,10,10))){
      stop("Digito verificador errado")
    }
  }
  
  Second <- as.integer(unlist((strsplit(str_sub(strNum,start = 1, end = 10), split = "")))) * 11:2
  
  if(sum(Second) %% 11 < 2){
    if(str_sub(strNum,11,11) != 0){
      stop("Digito verificador errado")
    }
  }else{
    if(!(11-sum(Second) %% 11) == as.numeric(str_sub(strNum,11,11))){
      stop("Digito verificador errado")
    }
  }
  
              
  return(str_replace(string = strNum,
                     pattern = "([0-9]{3})([0-9]{3})([0-9]{3})", 
                     replacement = "\\1.\\2.\\3-"))
}


formata_RG <- function(strNum)
{
  if(str_length(strNum) != 8) {
    stop("Numero invalido!")
  }
  return(str_replace(string = strNum,
                     pattern = "([0-9]{1})([0-9]{3})([0-9]{3})",
                     replacement = "\\1.\\2.\\3-"))
}


print(formata_CPF("30030030030"))


print(formata_RG("85477965"))



