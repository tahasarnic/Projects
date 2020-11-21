#Paketler
library(readr)
library(ggplot2)
library(gganimate)
library(gifski)
library(av)
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(mapproj)
library(sf)

#Veri aktarma ve manipülasyon

covid <- read_csv("https://raw.githubusercontent.com/cads-tedu/R/main/time_series_covid_19_confirmed.csv")


covid <- covid %>%
  rename(Country = `Country/Region`) %>% #Deðiþken ismiyle daha kolay çalýþmak için kolon ismini Country olarak deðiþtiriyoruz.
  select(2:dim(covid)[2]) %>% #Province kolonu hariç tüm deðiþkenleri seçiyoruz.
  gather(key = "date", value = "cases", 4:249) #Kolon olan tarih deðiþkenlerini tek bir tarih kolonu adý altýnda topluyoruz.

#Son oluþan veri setindeki tarih kolonunaki "X" karakterleri yerine "0" yazdýrýyoruz.
covid$date <- str_replace_all(covid$date, pattern = "X", replacement = "0")

#Ay/Gün/Yýl þeklinde girilmiþ tarih verilerimizi bu tanýma uyacak þekilde veri setimize tarih veri tipinde bir deðiþken olarak ekliyoruz.
#Gün/Ay/Yýl þeklinde olsaydý dmy() olarak kodlamamýz gerekecekti. (day/month/year)
covid$date <- mdy(covid$date)


#Görseller
#R içindeki hazýr paketten dünya haritasýný Antartika'yý almayacak þekilde içeri aktarýyoruz.
world_map <- map_data("world") %>%
  filter(region != "Antarctica")

#Harita
anim <- ggplot(world_map, aes(x = long, y = lat, group = group)) + #dünya haritasýný çizdirdik
  geom_polygon(fill="white", colour = "black") + #Ülkelere (polinognlara) beyaz ve sýnýr çizgilerini ise siyah rengi atadýk
  theme_dark() + #Dark temayý seçtik
  theme(axis.text = element_blank(), #ggplot2 ile oluþan x ve y eksenlerindeki eksek deðerlerini sildik
        axis.line = element_blank(), #ggplot2 ile oluþan grafik x ve y ekseni çizgilerini sildik
        axis.title = element_blank(), #ggplot2 ile oluþan x ve y eksen baþlýklarýný sildik
        panel.grid.major = element_blank(), #ggplot2 ile oluþan grafik çizgilerini sildik
        panel.grid.minor = element_blank(), #ggplot2 ile oluþan grafik çizgilerini sildik
        panel.background = element_rect(color = 'black', #Çerçeveyi siyah renk yaptýk
                                        fill = '#0A0C0B', #Grafik arka planýný siyah renk yaptýk (yine 'black' yapabilirdik)
                                        ),
        legend.position=c(0.1, 0.2), #Lejanýn konumunu belirledik (1,1) grafiðin en sað ve üst kýsmý (0,0) ise en sol ve alt kýsmýnu temsil eder
        legend.direction = "vertical", #Lejaný dikey olarak ayarladýk
        legend.background = element_rect(fill = "#0A0C0B"), #Lejan arka plan rengini haritanýn arka plan rengiyle ayný siyahlýkta yaptýk
        legend.title = element_text(color = "white", size = 20), #Lejan baþlýðý rengini ve büyüklüðünü ayarladýk
        legend.text = element_text(color = "white", size=20), #Lejandaki diðer yazýlarýn rengini ve büyüklüðünü ayarladýk
        legend.key = element_rect(fill = "#0A0C0B", color = NA) #Lejan içindeki her deðern (yazýnýn) kendi arka planýný da harita arka planýyla ayný renk yaptýk
        ) +
  labs(size = "Vaka Sayýlarý") + #Lejan baþlýðýný belirledik
  geom_point(data = covid, aes(x=Long, y=Lat, size = cases, group = Country), color = '#FF2525') + #Ülkelerin orta noktalarýnýn enlem boylamlarýna karþýlýk gelecek þekilde nokta grafiðimizi oluþturduk 
  geom_text(data=covid, aes(x=-100, y=-25, label=as.character(date), group=Country), check_overlap = TRUE, size=10, fontface="bold", color = "white") + #Harita üzerine tarih yazdýrcaðýmýz noktayý ve tarih deðiþkenimizi belirledik
  scale_size_continuous(range = c(1,20), limits = c(0,7000000), labels=scales::comma, breaks = c(0, 100000, 300000, 500000, 700000, 1000000, 1500000, 3000000, 4000000, 7000000)) + #Oluþturulan nokta grafiðimizin noktalarýný yeniden düzenledik
  transition_time(date) #ggplot2 ile entegre gganimate fonksiyonu olan transition_time fonksiyonunu grafiðimize ekleyerek ve tarih kolonumuzu animasyonun deðiþkeni olarak ayarladýk


animate(anim, renderer = gifski_renderer(), width = 1920, height = 1080, duration = 15) #gganimate fonksiyonu olan animate fonksiyonu yardýmýyla animasyonumuzu oluþturduk. Oluþacak animasyonun geniþlik ve uzunluðunu ve animasyon süresinin 15 saniye olacak þekilde düzenledik.

#Oluþturulan animasyonun kaydedilmesi
anim_save("dunya_covid.gif")


