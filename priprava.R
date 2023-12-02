vyberovy_prumer <- function(x)
{
  vysledek <- sum(x)/length(x)
  return(vysledek)
}

data <- c(3,4,9,20,22)

print(vyberovy_prumer(data))

median_x <- function(x)
{
  sorted_x = sort(x, decreasing = FALSE)
  if(length(sorted_x) %% 2 == 0 )
  {
    prostredni_prvek <- ceiling((length(sorted_x)/2))
    vysledek <- (sorted_x[prostredni_prvek] + sorted_x[prostredni_prvek+1])/2
  }
  else{
    prostredni_prvek <- ceiling((length(sorted_x)/2))
    vysledek <- sorted_x[prostredni_prvek]
  }
  return(vysledek)
}

print(median(data))

modus <- function(x)
{
  sorted_x <- sort(x, decreasing = FALSE)
  counts <- table(sorted_x)
  max_count <- max(counts)
  modes <- as.numeric(names(counts[counts == max_count]))
  return(modes)
}

print(modus(data))

rozptyl <- function(x)
{
  rozptyl <- (1/(length(x)-1)) * sum((x-(vyberovy_prumer(x)))^2) 
  return(rozptyl)
}

print(rozptyl(data))

smer_odchylka <- function(x)
{
  vysledek <- sqrt(rozptyl(x))
  return(vysledek)
}

print(smer_odchylka(data))

qu_1st <- function(x)
{
  sorted_x <- sort(x, decreasing = FALSE)
  i <- (0.25 * length(sorted_x))+0.5
  f <- i - floor(i)
  vysledek <- (1-f) * sorted_x[floor(i)] + f * sorted_x[ceiling(i)]
  return(vysledek)
}

print(qu_1st(data))

qu_3rd <- function(x) 
{
  sorted_x <- sort(x, decreasing = FALSE)
  i <- (0.75 * length(sorted_x))+0.5
  f <- i - floor(i)
  vysledek <- (1-f) * sorted_x[floor(i)] + f * sorted_x[ceiling(i)] 
  return(vysledek)
}

print(qu_3rd(data)) 

min_hodnota <- function(x)
{
  min_value <- Inf
  for (cislo in x)
  {
    if (cislo < min_value)
    {
      min_value <- cislo
    }
  }
  return(min_value)
}

print(min_hodnota(data)) 

max_hodnota <- function(x)
{
  max_hodnota <- -Inf
  for (cislo in x)
  {
    if (cislo > max_hodnota)
    {
      max_hodnota <- cislo
    }
  }
  return(max_hodnota)
}

print(max_hodnota(data))

library(readxl)

data_ex = read_excel('C:\\Users\\tomik\\OneDrive\\Plocha\\Škola\\PAS\\Data_vyzkum.xlsx')

print(summary(data_ex$Hmotnost))

print(paste("Median: ", median_x(data_ex$Hmotnost)))
print(paste("Vyberovy prumer: ", vyberovy_prumer(data_ex$Hmotnost)))
print(paste("Modus: ", modus(data_ex$Hmotnost)))
print(paste("Rozptyl: ", rozptyl(data_ex$Hmotnost)))
print(paste("Smerodatna odchylka: ", smer_odchylka(data_ex$Hmotnost)))
print(paste("1.Kvantil: ", qu_1st(data_ex$Hmotnost)))
print(paste("3.Kvantil: ", qu_3rd(data_ex$Hmotnost)))
print(paste("Minimalni hodnota: ", min_hodnota(data_ex$Hmotnost)))
print(paste("Maximalni hodnota: ", max_hodnota(data_ex$Hmotnost)))
