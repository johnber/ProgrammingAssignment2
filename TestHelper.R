verify <- function(expected, result) {
  
  expected <- round(expected, 3)
  result <- round(result, 3)
  
  verify((result == expected))
}

verify <- function(condition){
  if(condition){
    print("PASS")
  } 
  else {
    print("FAIL")
  }
}

verify_vectors <- function(a, b){

  if(all(a==b)){
    print("PASS")
  } else {
    print("FAIL")
    print(a)
    print(b)
  }
}