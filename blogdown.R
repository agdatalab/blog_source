## Install from CRAN
install.packages("blogdown")
install.packages('later') # blogdown::serve_site() 실행시 필요

# blog에 포함된 R 코드 사용 패키지는 미리 설치 필요
# install.packages('leaflet') 
# install.packages('DT')
# install.packages('widgetframe')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('gridExtra')

library(blogdown)
blogdown::install_hugo() # hugo 관련 파일 설치

blogdown::build_site() # rmd 파일 수정 후 실행하여 html파일생성, public폴더 생성
blogdown::serve_site() # public 폴더에 저장된 html파일들을 웹으로 표출

# blogdown::new_site()
# blogdown::new_site(theme = 'vimux/mainroad')

# blogdown::serve_site()
# blogdown::build_site()
# blogdown::hugo_build()

library(DT); library(widgetframe); library(dplyr); library(ggplot2); library(gridExtra)
dat <- read.csv('D:/git_repo/agdatalab_data/env_min_spl.csv')
print(paste('number of observation :',nrow(dat)))
names(dat)
dt <-  datatable(head(dat, 25), 
                 options = list(
                   columnDefs = list(list(className = 'dt-center', targets = 5)),
                   pageLength = 5, lengthMenu = c(5, 10, 15, 20)),
                 fillContainer = T)
frameWidget(dt, height = 350, width = '100%')

dat_hour <- dat[,c(1,5,7:19)] %>% group_by(date,hour) %>% summarise_all(funs(round(mean(.,na.rm=T),2)))
nrow(dat_hour)
?summarise_all
dt <-  datatable(head(dat_hour, 25), 
                 options = list(
                   columnDefs = list(list(className = 'dt-center', targets = 5)),
                   pageLength = 5, lengthMenu = c(5, 10, 15, 20)),
                 fillContainer = T)
frameWidget(dt, height = 350, width = '100%')

gg1 <- dat %>% filter(date=='2015-04-19') %>% 
  ggplot() + geom_line(aes(x=hour+minute/60, y=EVTP), color='red', size = 0.8, alpha = 0.8)
gg2 <- dat_hour %>% filter(date=='2015-04-19') %>% 
  ggplot() + geom_line(aes(x=hour, y=EVTP), color='blue', size = 0.8, alpha = 0.8)
grid.arrange(gg1, gg2, ncol=1, nrow=2)

dat_f1 <- dat %>% filter(date=='2015-04-19')
dat_f2 <- dat_hour %>% filter(date=='2015-04-19') 
gg1 <- ggplot() + 
  geom_line(data=dat_f1,aes(x=hour+minute/60, y=EVHM), color='red', size = 0.8, alpha = 0.5) +
  geom_line(data=dat_f2,aes(x=hour, y=EVHM), color='blue', size = 0.8, alpha = 0.8)
gg2 <- ggplot() + 
  geom_line(data=dat_f1,aes(x=hour+minute/60, y=INS), color='red', size = 0.8, alpha = 0.5) +
  geom_line(data=dat_f2,aes(x=hour, y=INS), color='blue', size = 0.8, alpha = 0.8)
gg3 <- ggplot() + 
  geom_line(data=dat_f1,aes(x=hour+minute/60, y=CO2), color='red', size = 0.8, alpha = 0.5) +
  geom_line(data=dat_f2,aes(x=hour, y=CO2), color='blue', size = 0.8, alpha = 0.8)
gg4 <- ggplot() + 
  geom_line(data=dat_f1,aes(x=hour+minute/60, y=WDS), color='red', size = 0.8, alpha = 0.5) +
  geom_line(data=dat_f2,aes(x=hour, y=WDS), color='blue', size = 0.8, alpha = 0.8)
grid.arrange(gg1, gg2, gg3, gg4, ncol=2, nrow=2)

dat_month <- dat %>% group_by(month) %>% summarize(OTP_avg=round(mean(OTP,na.rm=T),2),TP_avg=round(mean(EVTP,na.rm=T),2),TP_sd=round(sd(EVTP,na.rm=T),2),TP_max=max(EVTP,na.rm=T),TP_min=min(EVTP,na.rm=T))
dt <-  datatable(dat_month, options = list(pageLength = 15, lengthMenu = c(15)),fillContainer = T)
frameWidget(dt, height = 500, width = '100%')



gd_maker <- function(gd_tbl,day_start, day_end, ngt_start, ngt_end, field, dhi, dlo, nhi, nlo ) {
  var_hi <- paste(field,"_hi",sep="")
  var_lo <- paste(field,"_lo",sep="")
  for(i in 0:24){
    if(i<=ngt_end){
      gd_tbl[i+1,var_hi] <- nhi
      gd_tbl[i+1,var_lo] <- nlo
    } else if(i>ngt_end && i<day_start) {
      gd_tbl[i+1,var_hi] <- nhi + (i-ngt_end)*(dhi - nhi)/(day_start - ngt_end)
      gd_tbl[i+1,var_lo] <- nlo + (i-ngt_end)*(dlo - nlo)/(day_start - ngt_end)   
    } else if(i>=day_start && i<=day_end ) {
      gd_tbl[i+1,var_hi] <- dhi
      gd_tbl[i+1,var_lo] <- dlo
    } else if(i>day_end && i<ngt_start) {
      gd_tbl[i+1,var_hi] <- dhi - (i-day_end)*(dhi - nhi)/(ngt_start - day_end)
      gd_tbl[i+1,var_lo] <- dlo - (i-day_end)*(dlo - nlo)/(ngt_start - day_end)   
    } else if(i>=ngt_start) {
      gd_tbl[i+1,var_hi] <- nhi
      gd_tbl[i+1,var_lo] <- nlo
    }
  }
  return(gd_tbl)
}
gd_tbl <- data.frame(hour=seq(0,24,1), tp_hi=rep(NA,25), tp_lo=rep(NA,25))
gd_tbl <- gd_maker(gd_tbl,10,16,19,7,'tp',29.5,21,21,15.5)
ggplot() +
  geom_ribbon(data=gd_tbl, aes(x=hour, ymin=tp_lo, ymax=tp_hi), linetype=2, alpha=0.1, fill='red') + 
  labs(x = "일", y = "측정치") + scale_x_continuous(limits = c(0, 24), breaks=c(0,6,12,18,24)) +
  labs(title = "파프리카 적정 실내온도 범위") 


dat_f3 <- dat %>% filter(as.character(date)>='2015-04-19', as.character(date)<='2015-04-23') 
ggplot() +
  geom_ribbon(data=gd_tbl, aes(x=hour, ymin=tp_lo, ymax=tp_hi), linetype=2, alpha=0.1, fill='red') + 
  geom_line(data=dat_f3,aes(x=hour+minute/60, y=EVTP, group=date, color=date), size = 0.5, alpha = 1.0) +
  labs(x = "일", y = "측정치") + scale_x_continuous(limits = c(0, 24), breaks=c(0,6,12,18,24)) +
  labs(title = "파프리카 적정 실내온도 범위") 

dat_f4 <- dat %>% filter(as.character(date)>='2015-06-19', as.character(date)<='2015-06-23') 
ggplot() +
  geom_ribbon(data=gd_tbl, aes(x=hour, ymin=tp_lo, ymax=tp_hi), linetype=2, alpha=0.1, fill='red') + 
  geom_line(data=dat_f4,aes(x=hour+minute/60, y=EVTP, group=date, color=date), size = 0.5, alpha = 1.0) +
  labs(x = "일", y = "측정치") + scale_x_continuous(limits = c(0, 24), breaks=c(0,6,12,18,24)) +
  labs(title = "파프리카 적정 실내온도 범위") 

dat_f5 <- dat %>% filter(as.character(date)>='2015-12-19', as.character(date)<='2015-12-23') 
ggplot() +
  geom_ribbon(data=gd_tbl, aes(x=hour, ymin=tp_lo, ymax=tp_hi), linetype=2, alpha=0.1, fill='red') + 
  geom_line(data=dat_f5,aes(x=hour+minute/60, y=EVTP, group=date, color=date), size = 0.5, alpha = 1.0) +
  labs(x = "일", y = "측정치") + scale_x_continuous(limits = c(0, 24), breaks=c(0,6,12,18,24)) +
  labs(title = "파프리카 적정 실내온도 범위") 




library(dbConnect)
conn <- dbConnect(dbDriver("MySQL"),dbname = 'sisul',host = "147.46.229.85",user = "ais",password = "ezfarm3414" )
dbGetQuery( conn, "set names 'euckr'" )
dbListTables(conn) #DB내 테이블 확인
# dbWriteTable(conn, name='env_min_spl', value=dat, row.names=F, overwrite=F)
x <- dbGetQuery(conn, "SELECT * FROM env_min_spl")
dbDisconnect(conn)



