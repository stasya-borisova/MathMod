#Борисова Анастасия – для региона 28 Амурская область
#ЗАДАНИЕ: для региона 28 рассчитайте урожайность пшеницы в период с 2004 по 2014 год взяв для рассчета 
#средние суммы активных температур за эти годы, с 20 ближайших метеостанций

#Проверка рабочей директории
setwd("D:/Group_125/Borisova/MathMod"); getwd()

#Устанавливаем пакеты
library(rnoaa)
library(tidyverse)
library(lubridate)

# 1. Скачивание  СПИСКА МЕТЕОСТАНЦИЙ 
#station_data = ghcnd_stations() 
#Может занять несколько минут, лучше выполнить один раз в месте с хорошим интернетом 
#и сохранить результат
station_data = read.csv("station_data.csv")

#2. ФОРМИРОВАНИЕ СПИСКА МЕТЕОСТАНЦИЙ
#После получения списка всех станций, выберите из него список станций ближайших к 
#столице вашего региона,создав таблицу с именем региона и координатами его столицы
#координаторы должны быть в десятых градусов
blagoveschensk = data.frame(id = "blagoveschensk", latitude = 50.290640,  longitude = 127.527173)
#прочитайте справку команды meteo_nearby_stations
? meteo_nearby_stations
#можно выбирать метеостанции в некотором фиксированном радиусе от Махачкалы
#или конечное число станций, которые имеют необходимые данные
#в заданный временной период, и выбрать переменные, которые обязательно должны быть в наличии
blagoveschensk_around = meteo_nearby_stations(lat_lon_df = blagoveschensk, station_data = station_data,
                                              limit = 20, var = c("PRCP", "TAVG"),
                                              year_min = 2004, year_max = 2014)
#blagoveschensk_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций, отсортиров-ых по их удаленности от Благовещенска 
#вспомним, как работать со списками
#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Благовещенска, его то мы и попытаемся получить
blagoveschensk_id = blagoveschensk_around[[1]] %>% select(id)
#2)чтобы получить таблицу всех метеостанций вокруг Тулы нужно выбрать целиком первый объект из списка
blagoveschensk_stations  = blagoveschensk_around[[1]]
summary(blagoveschensk_stations)
#в таблице blagoveschensk_table оказалось 20 объектов, ранжированных по расстоянию от Благовещенска
###Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим объект, куда скачаем все данные всех метеостанций
all_blagoveschensk_meteodata = data.frame()
#Цикл для всех метеостанций

for(i in 1:20) 
{ 
  blagoveschensk_id = blagoveschensk_around[["blagoveschensk"]][["id"]][i]
  data = meteo_tidy_ghcnd(stationid = blagoveschensk_id,
                          var = "TAVG",
                          date_min = "2004-01-01",
                          date_max = "2014-12-31")
  all_blagoveschensk_meteodata = bind_rows(all_blagoveschensk_meteodata, data)
}
#Записываем полученные результаты
write.csv(all_blagoveschensk_meteodata,file = "all_blagoveschensk_meteodata.csv")
all_blagoveschensk_meteodata = read.csv("all_blagoveschensk_meteodata.csv")
# 4.Работа с полученными данными
#Создание  векторов с  данными для рассчётов
#коэффициент для экпозиции склона - считаем что все поля идеально ровные
y = 1.0
#Константы
ai = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
#отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "Сумма частей основной и побочной продукции
Lj = 2.2
#Коэффициент "Стандартная влажность культуры"
Ej = 25
#Создадим колонки year, month для группировки, выберем данные только для 2007 года
all_blagoveschensk = all_blagoveschensk_meteodata %>% 
  mutate(year = year(date), month = month(date), day = day(date)) %>%  
  filter(year > 2003 & year < 2015)%>%
  #сгруппируем по годам с учётом id метеостанций
  group_by(year, month, id) %>%
  #Выберем температуры, больше 5 oC
  mutate(tavg=tavg/10) 
#Заменим на нули все значения Na и те, что меньше 5 oC
all_blagoveschensk[is.na(all_blagoveschensk$tavg), "tavg"] = 0
all_blagoveschensk[all_blagoveschensk$tavg<5, "tavg"] = 0
all_blagoveschensk
#Вычислим суммарную среднюю активную температуру по месяцам для каждой метеостанции
all_blagoveschensk = all_blagoveschensk %>% summarise(sum = sum (tavg, na.rm = TRUE)) %>%
  # Вычислим средие активные температуры за месяц со всех метеостанций, для этого сначала сгруппируем 
  # по месяцам
  group_by(month) %>%
  summarise(S = mean(sum,na.rm = TRUE)) %>%
  #Далее рассчитаем урожайность для каждого месяца и создадим колонку для записи результатов
  mutate(F = ((ai + bi * y * S * di) * Kf) / (Qj * Lj * (100 - Ej)))
#Затем посчитаем суммарную урожайность, сложив данные в колонке F 
Yield = sum(all_blagoveschensk$F); Yield
#Урожайность пшеницы в период с 2004 по 2014 составляет 17.75422 ц/га.

