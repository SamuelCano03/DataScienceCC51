setwd("C:/Users/HP/Desktop/Data science/")

install.packages("dplyr")
install.packages("modeest") 

library(dplyr)
library(modeest)

booking_data <- read.csv("data/hotel_bookings.csv", na.strings = "" )

#Eliminamos duplicados
booking_data[duplicated(booking_data),]
booking_data_clean <-unique(booking_data)

str(booking_data_clean)

booking_data_clean$hotel<-as.factor(booking_data_clean$hotel)
booking_data_clean$is_canceled<-as.factor(booking_data_clean$is_canceled)
booking_data_clean$arrival_date_year<-as.factor(booking_data_clean$arrival_date_year)
booking_data_clean$arrival_date_month<-as.factor(booking_data_clean$arrival_date_month)
booking_data_clean$arrival_date_week_number<-as.factor(booking_data_clean$arrival_date_week_number)
booking_data_clean$arrival_date_day_of_month<-as.factor(booking_data_clean$arrival_date_day_of_month)
booking_data_clean$meal<-as.factor(booking_data_clean$meal)
booking_data_clean$country <-as.factor(booking_data_clean$country )
booking_data_clean$market_segment<-as.factor(booking_data_clean$market_segment)
booking_data_clean$distribution_channel<-as.factor(booking_data_clean$distribution_channel)
booking_data_clean$is_repeated_guest<-as.factor(booking_data_clean$is_repeated_guest)
booking_data_clean$reserved_room_type<-as.factor(booking_data_clean$reserved_room_type)
booking_data_clean$assigned_room_type<-as.factor(booking_data_clean$assigned_room_type)
booking_data_clean$deposit_type<-as.factor(booking_data_clean$deposit_type)
booking_data_clean$agent<-as.factor(booking_data_clean$agent)
booking_data_clean$company<-as.factor(booking_data_clean$company)
booking_data_clean$customer_type<-as.factor(booking_data_clean$customer_type)
booking_data_clean$reservation_status<-as.factor(booking_data_clean$reservation_status)
booking_data_clean$children<-as.integer(booking_data_clean$children)

str(booking_data_clean)

#Datos N/A
sin_valor <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(booking_data_clean)


#Reemplazandos los datos NA de Children con la Media
mean(booking_data_clean$children, na.rm =TRUE)
booking_data_clean$children<-ifelse(is.na(booking_data_clean$children), mean(booking_data_clean$children, na.rm =
                                                              TRUE), booking_data_clean$children)
#Datos en blanco
en_blanco <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores en blanco:",colSums(x[i]==""),"\n")
  }
}
en_blanco(booking_data_clean)

#Datos NULL
Nulls <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores que son NULL:",colSums(x[i]=="NULL"),"\n")
  }
}
Nulls(booking_data_clean)

booking_data_clean$company<-ifelse(booking_data_clean$company=="NULL", "Not Applicable", booking_data_clean$company)
booking_data_clean$company<-as.factor(booking_data_clean$company)

booking_data_clean$agent<-ifelse(booking_data_clean$agent=="NULL", "Not Applicable", booking_data_clean$agent)
booking_data_clean$agent<-as.factor(booking_data_clean$agent)

mode <- function(x) {
  return(names(which.max(table(x))))
}

booking_data_clean$country<-ifelse(booking_data_clean$country=="NULL", as.character(mode(booking_data_clean$country)), as.character(booking_data_clean$country))
booking_data_clean$country<-as.factor(booking_data_clean$country)

#Limpiamos datos Incoherentes

#Sin adultos en la reserva
NoAdults <- booking_data_clean %>% filter(adults == 0)
booking_data_clean <- booking_data_clean[booking_data_clean$adults != 0,]
#Sin noches reservadas
NoNights <- booking_data_clean %>% filter(stays_in_weekend_nights == 0 & stays_in_week_nights == 0 & is_canceled == 0)
booking_data_clean <- booking_data_clean[booking_data_clean$adults != 0,]


#Reemplazando Outlayers

#Primer Gráfico
booking_data_clean_by_month = booking_data_clean %>%
  mutate(
    arrival_date_month = factor(arrival_date_month, levels = month.name)
  ) %>%
  arrange(arrival_date_month)
count = table(booking_data_clean_by_month$hotel, booking_data_clean_by_month$arrival_date_month)
barplot(count, beside=TRUE, col=c("green", "cyan"), legend.text=TRUE, main="Frecuencia de reservas por mes segun el hotel")
#Segundo Gráfico
booking_data_clean_by_is_repeat <- table(booking_data_clean$is_repeated_guest)
barplot(prop.table(booking_data_clean_by_is_repeat), beside=TRUE, col=c("green", "cyan"), names=c("Nuevos", "Antiguos"), main="% de clientes nuevos y antiguos")

booking_data_clean_by_is_repeat_by_hotel <- table(booking_data_clean$hotel, booking_data_clean$is_repeated_guest)
barplot(booking_data_clean_by_is_repeat_by_hotel, beside=TRUE, col=c("green", "cyan"), names=c("Nuevos", "Antiguos"), legend.text=TRUE, main="% de clientes nuevos y antiguos por hotel")

#Tercer Gráfico

library(ggplot2)
library(scales)
ggplot(data = booking_data_clean, aes(x = distribution_channel, y = as.integer(arrival_date_month), fill = factor(hotel))) +
  geom_boxplot() +geom_hline(aes(yintercept = 6),colour = "red", linetype = "dashed", lwd = 2) +
  #scale_y_continuous(limits = c(0, 15)) +
  theme_bw()
