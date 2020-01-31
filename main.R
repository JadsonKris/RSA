source("/home/jadson/RSA/rsa.R")

#source("/home/jadson/RSA/main.R"); menu()

menuAux1 = function(){
  
  p <- as.integer(readline(prompt="Write p: "))
  if(!isPrime(p)){
    cat("P not is prime\n")
    menuAux1()
  }
  
  q <- as.integer(readline(prompt="Write q: "))
  if(!isPrime(q)){
    cat("Q not is prime\n")
    menuAux1()
  }
  
  e <- as.integer(readline(prompt="Write e: "))
  t = totiente(p, q)
  if( mdc(t,e) != 1){
    cat("invalid e\n")
    menuAux1()
  }
  
  createPublicKey(p,q,e)
  cat("Successful Operation\n")
  menu()
}


menuAux2 = function(){
  e <- as.integer(readline(prompt="Write e: "))
  n <- as.integer(readline(prompt="Write n: "))
  
  encrypt(e, n)
  cat("Successful Operation\n")
  menu()
}


menuAux3 = function(){
  p <- as.integer(readline(prompt="Write p: "))
  q <- as.integer(readline(prompt="Write q: "))
  e <- as.integer(readline(prompt="Write e: "))
  
  decrypt(p, q, e)
  cat("Successful Operation\n")
  menu()
}


menu = function(){
  cat("\n\n\nWrite:\n1 for create public key\n2 for encrypt\n3 for decrypt\n4 for exit")
  option <- readline(prompt=" ")
  
  if(option == 1){
    menuAux1()
  }else if(option == 2){
    menuAux2()
  }else if(option == 3){
    menuAux3()
  }else if(option == 4){
    return()
  }else{
    cat("Error 404: input not found\n")
    menu()
  }
}
