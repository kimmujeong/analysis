######정규표현 연습...######
grep('11305..300', bike_file$법정동코드)
grep('^11305',bike_file$법정동코드)
grep('11305[0-9]+300',bike_file$법정동코드)

bike_file %>%
  filter(grepl('^11305',법정동코드))

bike_gangbuk<-bike_file %>% 
  filter(str_detect(법정동코드, "^11305"))