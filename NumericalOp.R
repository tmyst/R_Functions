# IEEE 754 最近接丸め（0から遠いほうへ）
# 通常日本で最初に習う四捨五入
round_5in <- function(x, digits=0){
  if(floor(x*10^(digits+1)) %% 10 == 5){
    round(x+sign(x)*10^(-(digits+1)), digits)
  }else{
    round(x, digits)
  }
}
