# Write a function that given your birthday (as a date), 
# returns how old you are in years.

age <- function(bday) {
  ans<-today()-bday
  return(as.duration(ans))
}

age(ymd("1998-02-11"))
