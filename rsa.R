isPrime = function (n) {
  if(n < 30){
    vector1 = c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
    for( i in 1:length(vector1)){
      if(vector1[i] == n){
        return(TRUE)
      }
    }
    return(FALSE)
  }
  if(n%%2 == 0){
    return(FALSE)
  }else{
    limit = as.integer(sqrt(n))+1
    for(i in seq(3, limit, by = 2)){
      if(n%%i == 0){
        return(FALSE)
      }
    }
    return(TRUE)
  }
}


totiente = function(p, q){
  return((p-1) * (q-1))
}


mdc = function(a, b){
  if (b == 0){
    return(a) 
  }else{
    return(mdc(b, a %% b)) 
  }
}


fastExpo = function(a, n, mod){
  require(stringr)
  library(binaryLogic)
  
  vector1 = as.binary(n, littleEndian = TRUE)
  vector2 <- rep(-1, times = length(vector1))
  vector2[1] = a%%mod
  
  for(i in 2:length(vector2)){
    if(vector2[i] == -1){
      vector2[i] =  (vector2[i-1] * vector2[i-1])%%mod   
    }
  }
  
  result = 1
  for(i in 1:length(vector1)){
    if(vector1[i]){
      result = result*vector2[i]
    }
  }
  
  result = result%%mod
  return(result)
}


createPublicKey = function(p, q, e){
  n = p*q
  fileConn<-file("/home/jadson/RSA/publicKey.txt")
  writeLines(c(toString(n),toString(e)), fileConn)
  close(fileConn)
}


charToAscii = function(char){
  require(gtools)
  a = asc(char)
  
  if(a == 32){
    return(28)
  }else{
    return(as.integer(a-63))
  }
}


asciiToChar = function(num){
  if(num == 28){
    return(" ")
  }else{
    return(intToUtf8(num+63))
  }
}


encrypt = function(e, n){
  fileIn<-file("/home/jadson/RSA/text.txt")
  string = readLines(fileIn)
  close(fileIn)
  string = toupper(string)
  require(stringi)
  final = -1
  string2 = substring(string, 1: stri_length(string),1:stri_length(string))
  
  for(i in string2){
    c = fastExpo(charToAscii(i), e, n)
    if(final == -1){
      final = c
    }else{
      final = paste(final,c,sep = " ")
    }
  }
  
  fileOut<-file("/home/jadson/RSA/encrypt.txt")
  writeLines(final, fileOut)
  close(fileOut)
}


euclides_ext = function(a, b, c){
  r = b%%a
  
  if(r == 0){
    return( (c%/%a) %% (b%/%a) )
  }else{
    return( (euclides_ext(r, a, -c) * b + c) %/% (a%%b) )
  }
}

decrypt = function(p, q, e){
  string <- read.table("~/RSA/encrypt.txt", quote="\"", comment.char="")
  t = totiente(p, q)
  d = euclides_ext(e,t,1)
  n = p * q
  final = -1
  
  for(i in string){
    c = asciiToChar(fastExpo(i, d, n))
    if(final == -1){
      final = c
    }else{
      final = paste(final,c,sep = "")
    }
  }
  
  fileOut<-file("/home/jadson/RSA/decrypt.txt")
  writeLines(final, fileOut)
  close(fileOut)
}
