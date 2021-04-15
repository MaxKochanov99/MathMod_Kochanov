#Кочанов Максим - для региона 31 Белгородская область
#Задание: рассчитайте урожайность пшеницы в период с 2000 по 2003 год взяв для рассчета средние суммы активных температур за эти годы, с метеостанций на расстоянии от 60 до 150 км
#Установка и проверка рабочей директории
setwd("D:/MathMod")

#Устанавка необходимых пакетов
library (tidyverse)
library (rnoaa)
library (lubridate)

#Скачиваем список метеостанций
station_data = ghcnd_stations()
write.csv(station_data,"station_data21.csv")
station_data = read.csv("station_data21.csv")

#Формирование списка метеостанций
Belgorod=data.frame(id  = "Belgorod",latitude= 50.46, longitude = 37.27)

# выбираем метеостанции в фиксированном радиусе от Белгорода или конечное число станций,которые имеют необходимые данные в заданный временной период
Belgorod_around=meteo_nearby_stations(lat_lon_df = Belgorod, station_data = station_data, var = c("TAVG"),
                                      year_min = 2000, year_max = 2003)

#получаем индентификатор метеостанции Белгород
Belgorod_id=Belgorod_around[["Belgorod"]][["id"]][1]

summary(Belgorod_id)
Belgorod_table=Belgorod_around[[1]]
summary(Belgorod_table)

#Получение таблицы ближайших метеостанций
Belgorod_table = data.frame(Belgorod_around)
summary(Belgorod_table)

# отфильтруем все станции, на расстоянии от 60 до 150 км
Belgorod_stations= Belgorod_table[Belgorod_table$Belgorod.distance > 60 & Belgorod_table$Belgorod.distance < 150,]
str(Belgorod_stations)
Belgorod_stations$Belgorod.id

# Создание цикла, в котором скачиваются необходимые данные с метеостанций 

# Промежуточный объект, куда скачиваются данные с кокретной метеостанции
all_i = data.frame()

# Объект куда скачиваются все данные со всех метеостанций
all_Belgorod_meteodata = data.frame()

# Цикл для всех метеостанций
for(i in 1:12)
{
  Belgorod_id =  Belgorod_around[["Belgorod"]] [["id"]] [ i]
  data = meteo_tidy_ghcnd(stationid = Belgorod_id,
                          var = "TAVG",
                          date_min = "2000-01-01",
                          date_max = "2003-12-31")
  all_Belgorod_meteodata =  bind_rows(all_Belgorod_meteodata, data)
}

# Запись полученных данных в файл
write.csv(all_Belgorod_meteodata, "all_Belgorod_meteodata.csv")
all_Belgorod_meteodata

# считываем данные из файла all_Belgorod_meteodata.csv
all_Belgorod_meteodata = read.csv("all_Belgorod_meteodata.csv")

# Посмотрим на данные
str(all_Belgorod_meteodata)

#  Добавим год, месяц,день
all_Belgorod_meteodata = mutate(all_Belgorod_meteodata, year = year(date), month = month(date), day = day(date))
str(all_Belgorod_meteodata)

# Отфильтруем данные за 2000 - 2003 годы
years_Belgorod_meteodata = filter(all_Belgorod_meteodata, year %in% c( 2000:2003))

#  Проверим результат
str(years_Belgorod_meteodata)
summary(years_Belgorod_meteodata)

#Средняя (по годам и метеостанциям) сумма активных температур за месяц
# Приведение средней суммы температур в подходящую форму, при помощи деления на 10
years_Belgorod_meteodata[,"tavg"] = years_Belgorod_meteodata$tavg/10

# Превратим в нули все NA и где 5<tavg>30
years_Belgorod_meteodata[is.na(years_Belgorod_meteodata$tavg),"tavg"] = 0
years_Belgorod_meteodata[years_Belgorod_meteodata$tavg<5, "tavg"] = 0
summary(years_Belgorod_meteodata)

#Группируем по метеостанциям, годам и месяцам
#Группировка по метеостанциям, годам и месяцам при помощи функции group_by
alldays = group_by(years_Belgorod_meteodata, id, year, month)

#Сумма температуру по этим группам с помощью sum 
sumT_alldays_Belgorod = summarize(alldays, tsum = sum(tavg))
summary(sumT_alldays_Belgorod)

#Группировка данных по месяцам  
groups_Belgorod_months = group_by(sumT_alldays_Belgorod, month); groups_Belgorod_months

#Расчет среднего по месяцам для всех метеостанций и всех лет
sumT_months=summarize(groups_Belgorod_months,St=mean(tsum))
sumT_months

# Подготовка к расчету по формуле Урожая

#Ввод констант для расчета урожайности
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)

#Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
y = 1.0

#Коэффициент использования ФАР посевом
Kf = 300

#Калорийность урожая культуры
Qj = 1600

#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2

#Коэффициент "стандартная влажность культуры"
Ej = 25 

#Рассчет Fi по месяцам
sumT_months = mutate(sumT_months, Fi = afi + bfi * y * St)

#Рассчет Yi
sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))

#Расчет урожая, как сумму по месяцам
Yield = sum(sumT_months$Yi)
Yield

# Для региона 31 урожайность пшеницы в период с 2000 по 2003 год составила 19,17 ц/га 

