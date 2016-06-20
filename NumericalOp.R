# IEEE 754 round away from zero

round_5up <- function(x, digits=0){
  if(floor(x*10^(digits+1)) %% 10 == 5){
    round(x+sign(x)*10^(-(digits+1)), digits)
  }else{
    round(x, digits)
  }
}
