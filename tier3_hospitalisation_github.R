
rm(list=ls())

library(data.table)
library(ggplot2)
library(ggpmisc)
library(sjstats)
library(MASS)
library(panelr)
library(GGally)
library(psych)
library(geepack)
library(tmap)
library(sf)
library(tmap)
library(tmaptools)
library(caret)
library(randomForest)
library(viridis)  
library(RColorBrewer)
library(regclass)
library(biostat3)
library(regclass)
library(haven)
library(readxl)
library(statar)
library(foreign)
library(janitor)
library(stringr)
library(fuzzyjoin)
library(FNN)
library(splitstackshape)
library(dplyr)
library(tsibble)
library(readODS)
library(ggspatial)
library(microsynth)
library(scales)
library(survey)
library(aweek)  # for the get_aweek function
library(epitools)
library(maptools)
library(cartogram)  # for creating cartograms https://www.r-graph-gallery.com/331-basic-cartogram.html
library(tidyverse)
library(broom) # for the tidy function, which takes the messy output of built-in functions in R, such as lm, nls, or t.test, and turns them into tidy tibbles.
library(mapproj) # for the coord_ma function
library(svglite) # for compiling ggplots into svg format


`%notin%` <- Negate(`%in%`)


################################### Filepath ###################################
a<- "filepath.a"
b<-"filepath.b"
c<-"filepath.c"

########################### Load msoa admissions data ######################
msoa_adm<-fread(normalizePath(file.path(a,"a.csv")))

msoa_adm[isoyear==2021 & isoweek==53, isoyear:=2020]
msoa_adm<-msoa_adm[, list(admi2_cnt=sum(admi2_cnt)), by=.(isoyear, isoweek, msoa11)]
msoa_adm[,char_week:=gsub("-", " ", as.character(trunc(get_aweek(isoweek,isoyear))))]
class(msoa_adm$char_week)
table(msoa_adm$char_week)

setnames(msoa_adm, "isoyear", "year")

summary(msoa_adm$admi2_cnt)

########################### Load the cases_msoa data ###################

load(normalizePath(file.path(b,"b.RData")))
table(cases_msoa$date)

max(cases_msoa$date)
min(cases_msoa$date)

cases_msoa[, char_week:=as.character(week_number)]

table(cases_msoa$char_week)
class(cases_msoa$char_week)

cases_msoa<-merge(cases_msoa,msoa_adm[, .(msoa11,char_week,admi2_cnt)], 
                  by.x=c("msoa11_cd", "char_week"),by.y=c("msoa11", "char_week"), all.x=T)

cases_msoa[is.na(admi2_cnt)==T,admi2_cnt:=0 ]
summary(cases_msoa$admi2_cnt)


tier_date<-as.Date("2020-12-07")
yearweek("2020-12-04")


test0<-cases_msoa[date>=as.Date("2020-10-01") & week_number<=yearweek("2021 W13")]
max(test0$date)
as.Date(yearweek("2021 W12"))
#  identify intervention group 
test0[, intervention:=as.numeric(ladnm=="Liverpool")]
test0[, after:=as.numeric(week_number>yearweek(as.Date("2020-11-13")))]
test0[, id:=as.numeric(as.factor(msoa11_cd))]

table(test0$intervention)


test0[, adm_rate:=admi2_cnt*100000/total_pop]
yearweek(tier_date)

#  identify those who entered tier3 on "2020-12-07"
test0[, tier3:=max(as.numeric(tier=="3" & week_number==yearweek(tier_date))), 
      by=.(msoa11_cd)]
with(test0, table(tier,week_number,  useNA = "ifany"))

with(test0[tier3==1 & week_number==yearweek(tier_date)], table(ladnm))
#  identify those who entered tier2.
test0[, tier2:=max(as.numeric(tier=="2" & week_number==yearweek(tier_date))), 
      by=.(msoa11_cd)]
table(test0$tier2, useNA = "ifany")


test0[, st_week:=as.numeric((week_number-yearweek(tier_date)))]
table(test0$st_week)

# exclude places that were never in tier two or three
test0<-test0[tier2==1|tier3==1,]

test0[, prop_sgtf:=mean(percent_sgtf_cases, na.rm = T), by=.(msoa11_cd)]
test0[, prop_sgtf:=mean(prop_sgtf, na.rm = T), by=.(msoa11_cd)]
summary(test0$prop_sgtf)

table(test0$st_week, useNA = "ifany")

test0[, max_wave:=max(st_week), by=.(msoa11_cd)]
test0[, min_wave:=min(st_week), by=.(msoa11_cd)]
table(test0$max_wave, useNA = "ifany")
table(test0$min_wave, useNA = "ifany")


test0[, num_wave:=.N, by=.(msoa11_cd)]
table(test0$num_wave)
table(test0$st_week)


test0[, id:=as.numeric(as.factor(msoa11_cd))]
test0[, time:=as.numeric(as.factor(st_week))]

table(test0[st_week==0]$time)
table(test0$time)

test0[, regioncdn:=as.numeric(as.factor(regionname))]
table(test0$regionname)
table(test0$regioncdn)

# generate the counts of week within an intervention
class(test0$tier)
class(test0$tier3)
table(test0$tier3)
test0[st_week>=0, week_tier3:=sum(as.numeric(tier=="3")),by = .(msoa11_cd)]
test0[st_week>=0, week_tier4:=sum(as.numeric(tier=="4")),by = .(msoa11_cd)]
table(test0$week_tier3)
table(test0$week_tier4)

test0[, week_tier3:=max(week_tier3, na.rm = T),by = .(msoa11_cd)]
test0[, week_tier4:=max(week_tier4, na.rm = T),by = .(msoa11_cd)]
class(test0$tier2)
table(test0$tier2)

# average test rate before intervention
test0[date<as.Date("2020-11-06"), av_test:=mean(test_rate), by = .(msoa11_cd)]
summary(test0$av_test)
test0[, av_test:=max(av_test, na.rm = T), by = .(msoa11_cd)]
summary(test0$av_test)
#  missing and had to be imputed
prop.table(table(test0$miss_msoa_flag))

test0[, case_rate:=cases_imp1*100000/total_pop]
summary(test0$case_rate)

test0[, after_tier2:=week_number>=yearweek(as.Date("2020-12-07"))]
with(test0, table(time, after_tier2))
test0[, other_lcr:=(ladnm=="Halton"| ladnm=="Sefton"|ladnm=="Knowsley"|
                      ladnm=="St. Helens"|ladnm=="Wirral")]

test0[is.na(lfd_test_la)==T, lfd_test_la:=0]
test0[, lft_test_rate:=lfd_test_la*100/an_pop]
test0[date>as.Date("2020-11-05") & date<as.Date("2020-12-31"), 
      meanlftrate:=mean(lft_test_rate,na.rm=T), by=.(msoa11_cd)]
test0[, meanlftrate:=max(meanlftrate,na.rm=T), by=.(msoa11_cd)]

summary(test0$prop_sgtf)

################################### Timetable ##################################
end.post=max(test0$time)
max(test0$date)
min(test0$date)

min(test0[week_number==yearweek(tier_date)]$time)

table(test0[time==1]$week_number)
get_date(40,2020,1)
get_date(40,2020,7)
# time from which would expect tiers to affect hospital admissions 
end.pre=12
table(test0[time==12]$date)
table(test0[time==12]$week_number)
get_date(51,2021,1)
get_date(51,2021,7)

table(test0[time==11]$week_number)
get_date(50,2021,1)
get_date(50,2021,7)
table(test0[time==10]$week_number)
get_date(49,2021,1)
get_date(49,2021,7)

table(test0[time==21]$date)
table(test0[time==1]$date)
table(test0[time==21]$week_number)
end.post=21
get_date(7,2021,1)
get_date(7,2021,7)

table(test0$time)
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$ladnm)

####################################### Main analysis of synthetic control model ###################################
cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", 
              "imd_score","av_test","prop_sgtf")
match.out2 <- c("admi2_cnt")
set.seed(6768)
## by selecting intervention==0, ladnm=="Liverpool" MSOAs were excluded.
sea2 <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0]), 
                   idvar="id", timevar="time", intvar="tier3", 
                   start.pre=1, end.pre=end.pre, end.post=end.post, 
                   match.out=match.out2, match.covar=cov.var2, 
                   result.var=match.out2 , 
                   omnibus.var=match.out2,
                   test="twosided",
                   confidence = 0.95,
                   perm=250,
                   jack=F, 
                   n.cores = 1)

summary(sea2)
plot_microsynth(sea2)

#  add weights into main dataset. 
weights2<-cbind(ID=row.names(sea2$w$Weights),as.data.frame(sea2$w$Weights),
                row.names=NULL)
weights2<-cbind(weights2,interv2=as.data.frame(sea2$w$Intervention), 
                row.names=NULL)
colnames(weights2)[1]<-"id"
weights2$id<-as.numeric(as.character(weights2$id))
summary(weights2$Main)

test0$Main<-NULL
test0<-as.data.table(merge(test0,weights2[,1:2], by="id", 
                           all.x=T ))
table(test0$after_tier2, useNA = "ifany")
summary(test0$Main)


########################### Plotting out the tiers effect #######################

svyfull<-svydesign(ids=~msoa11_cd, weights=~Main, 
                   data=test0[is.na(Main)==F&intervention==0 & meanlftrate<1 & other_lcr==0])

prop0<-cbind(svyby(~admi2_cnt, denominator=~total_pop, by=~tier3+date,
                   design=svyfull,FUN=svyratio),
             confint(svyby(~admi2_cnt, denominator=~total_pop, by=~tier3+date,
                           design=svyfull,FUN=svyratio))[,2],
             confint(svyby(~admi2_cnt, denominator=~total_pop, by=~tier3+date,
                           design=svyfull,FUN=svyratio))[,1])


names(prop0)<-c("tier3", "date","rate", "se_rate","lcl", "ucl")

text_plot <- data.frame(text = c("Second national\nlockdown begins","Tier 3\nbegins", 
                                 "Tier 3\neffects\nbegin", "Third national\nlockdown begins"), 
                        dates = as.Date(c("2020-11-05","2020-12-07","2020-12-20","2021-01-06")), 
                        stringsAsFactors = FALSE)

ggplot(data=prop0[as.Date(prop0$date)<=as.Date("2021-02-22"),], aes(x = as.Date(date), y = rate*100000, 
                                                                    color=as.factor(tier3)))+ 
  geom_line(aes(lty=as.factor(tier3)), size=1)+
  geom_ribbon(aes(fill=as.factor(tier3), ymin=lcl*100000, ymax=ucl*100000),
              size=0, alpha=0.3)+
  scale_colour_manual(values = c("#440154FF","#FDE725FF"),labels=c( "Tier 2 Synthetic Control", "Tier 3")) +
  scale_linetype_manual(values = c("solid","dashed"),labels=c( "Tier 2 Synthetic Control", "Tier 3")) +
  scale_fill_manual(values = c("#440154FF","#FDE725FF"),labels=c( "Tier 2 Synthetic Control", "Tier 3")) +
  scale_x_date(date_breaks ="weeks" )+
  geom_vline(mapping = aes(xintercept = dates), data = text_plot, 
             show.legend = F, linetype = "dotted") +
  geom_text(aes(x=c(dates[1]-28,dates[2]-12,dates[3],dates[4]), label=text, y=c(40,40,38.5,40)), data = text_plot,
            colour="black", size=6, fontface = "bold.italic",hjust=c(0,0,0,0)) +
  xlab("Week start date") + 
  ylab("Admissions per 100,000 population")+
  theme_classic()+
  theme(text = element_text(size=16),legend.position="bottom", 
        legend.box = "horizontal", legend.text = element_text(size=24),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1))+ 
  theme(strip.background = element_blank())

ggsave("tier3_admission_synth.png",path=normalizePath(file.path(b,"paper")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("tier3_admission_synth.svg",path=normalizePath(file.path(b,"paper")),
       width = 16, height = 8, units="in",dpi=600)


############################ Plotting out the study areas ######################
# load LAD boundaries that are downloaded from https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2017-boundaries-gb-bfc?geometry=-35.772%2C51.103%2C30.981%2C59.783
lad <- read_sf(dsn = normalizePath(file.path(b,"Local_Authority_Districts__December_2017__Boundaries_GB_BFC.shp")))
lad <- lad[grepl('^E',lad$LAD17CD),]
lad <- st_transform(lad, 4326)


# load MSOA boundaries
sp_msoa <- st_read(normalizePath(file.path(b,"msoa_geo.shp")))
sp_msoa <- sp_msoa[grepl('^E',sp_msoa$msoa11cd),]
st_crs(sp_msoa) = 27700

table(test0$intervention)
table(test0[st_week==0 & intervention==0 & meanlftrate<1 & other_lcr==0]$tier3)
adj2_msoa <- merge(sp_msoa,test0[st_week==0 & intervention==0 & meanlftrate<1 & other_lcr==0,
                                 .(st_week,time,intervention,tier3,msoa11_cd,admi2_cnt,Main)],
                   by.x="msoa11cd", by.y="msoa11_cd", all.x=T)
table(adj2_msoa$tier3)
class(adj2_msoa$tier3)

summary(adj2_msoa$Main)

#plotting the study areas of the main result
ggplot() +
  geom_sf(data = adj2_msoa,aes(fill = as.factor(tier3)),
          colour = NA,
          lwd = 0) +
  scale_fill_viridis_d(alpha = 1,
                       breaks=c(0,1),
                       labels=c("Synthetic Control", "Tier 3"),
                       na.value = "white") +
  geom_sf(data = lad,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
  guides(fill = guide_legend(title = "")) +
  theme_void() +
  theme(legend.text=element_text(size=14))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(normalizePath(file.path(b,"paper/tier3_areas.png")), width = 16, height = 14,device="png",
       units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"paper/tier3_areas.svg")), width = 16, height = 14,device="svg",
       units = "in", dpi=700)


#plotting the weights of the main result
table(adj2_msoa$tier3)
table(unique(adj2_msoa[is.na(adj2_msoa$tier3),]$msoa11nm))
NROW(unique(adj2_msoa[is.na(adj2_msoa$tier3),]$msoa11cd))

NROW(unique(adj2_msoa[adj2_msoa$Main==1&adj2_msoa$tier3==1,]$msoa11cd))
NROW(unique(adj2_msoa[adj2_msoa$Main!=1&adj2_msoa$tier3==0,]$msoa11cd))
NROW(unique(adj2_msoa[adj2_msoa$tier3==1,]$msoa11cd))
NROW(unique(adj2_msoa[adj2_msoa$tier3==0,]$msoa11cd))

NROW(unique(adj2_msoa[!is.na(adj2_msoa$Main) &
                        adj2_msoa$Main!=0 & 
                        adj2_msoa$Main!=1,]))

ggplot() +
  geom_sf(data = adj2_msoa[!is.na(adj2_msoa$Main) &
                             adj2_msoa$Main!=0 & 
                             adj2_msoa$Main!=1,],
          aes(geometry = geometry, fill = as.numeric(Main)),
          colour = NA,
          alpha = 1,
          lwd = 0) +
  scale_fill_viridis(
    trans = "sqrt", 
    direction = -1,
    option = "C",
    breaks = c(0,0.01,5,10,20,30), 
    labels = c("0","0","5","10", "20", "30"),
    na.value = "white",
    name="Synthetic control weights") +
  geom_sf(data = lad,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        legend.text.align = 0) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(normalizePath(file.path(b,"paper/tier3_areas_wts.png")), width = 16, height = 14,device="png",
       units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"paper/tier3_areas_wts.svg")), width = 16, height = 14,device="svg",
       units = "in", dpi=700)

########################## Summary statistics table ############################

table(test0$st_week)
min(test0$date)

table1 <- test0[st_week<2 & intervention==0 & meanlftrate<1 & other_lcr==0,
                list(imd_score=weighted.mean(imd_score),
                     pop_dens=weighted.mean(pop_dens),
                     prop_70plus=weighted.mean(prop_70plus)*100,
                     prop_bame=weighted.mean(prop_bame)*100,
                     prop_sgtf=weighted.mean(prop_sgtf),
                     av_test=weighted.mean(av_test),
                     as_prop_vulner=weighted.mean(as_prop_vulner)*100,
                     adm_rate=weighted.mean(adm_rate),
                     total_pop=sum(total_pop),
                     num_msoa=.N),by=.(tier3)]

table1[, total_pop:= total_pop/12]
table1[, num_msoa:= num_msoa/12]
table1<-as.data.table(t(table1), keep.rownames=T)


cols <- c('V1','V2')
table1[,(cols):=lapply(.SD, round),.SDcols=cols]

test0$case
test0[, case_rate:=cases_imp1*100000/total_pop]
test0[st_week<2 & intervention==0 & meanlftrate<1 & other_lcr==0,
      .(case_rate_av=weighted.mean(case_rate)),
      by=.(tier3)]


write.csv(table1,
          file= normalizePath(file.path(b,"papers/fin_summary_tab.csv")))

#############################differences by deprivation#########################
#re weight the data by depriavtion tertiles

table(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$week_number)
test0[intervention==0 & meanlftrate<1 & other_lcr==0, 
      qc_imd:=ntile(imd_score, 3), by=.(st_week)]
summary(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$qc_imd)
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$qc_imd)
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$qc_imd, test0[intervention==0 & meanlftrate<1 & other_lcr==0]$tier3)

cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", 
              "imd_score","av_test","prop_sgtf")
match.out2 <- c("admi2_cnt")
set.seed(6768)
## by selecting intervention==0, ladnm=="Liverpool" MSOAs were excluded.
sea2_imd1 <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_imd==1]), 
                        idvar="id", timevar="time", intvar="tier3", 
                        start.pre=1, end.pre=end.pre, end.post=end.post, 
                        match.out=match.out2, match.covar=cov.var2, 
                        result.var=match.out2 , 
                        omnibus.var=match.out2,
                        test="twosided",
                        confidence = 0.95,
                        perm=250,
                        jack=F, 
                        n.cores = 1)

summary(sea2_imd1 )
plot_microsynth(sea2_imd1)


set.seed(6768)
sea2_imd2 <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_imd==2]), 
                        idvar="id", timevar="time", intvar="tier3", 
                        start.pre=1, end.pre=end.pre, end.post=end.post, 
                        match.out=match.out2, match.covar=cov.var2, 
                        result.var=match.out2 , 
                        omnibus.var=match.out2,
                        test="twosided",
                        confidence = 0.95,
                        perm=250,
                        jack=F, 
                        n.cores = 1)

summary(sea2_imd2 )
plot_microsynth(sea2_imd2)


set.seed(6768)
sea2_imd3 <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_imd==3]), 
                        idvar="id", timevar="time", intvar="tier3", 
                        start.pre=1, end.pre=end.pre, end.post=end.post, 
                        match.out=match.out2, match.covar=cov.var2, 
                        result.var=match.out2 , 
                        omnibus.var=match.out2,
                        test="twosided",
                        confidence = 0.95,
                        perm=250,
                        jack=F, 
                        n.cores = 1)

summary(sea2_imd3 )
plot_microsynth(sea2_imd3)




weights_1<-cbind(ID=row.names(sea2_imd1$w$Weights),as.data.frame(sea2_imd1$w$Weights),
                 row.names=NULL)
weights_1<-cbind(weights_1,interv1=as.data.frame(sea2_imd1$w$Intervention), 
                 row.names=NULL)
colnames(weights_1)[1]<-"id"
weights_1$id<-as.numeric(as.character(weights_1$id))


weights_2<-cbind(ID=row.names(sea2_imd2$w$Weights),as.data.frame(sea2_imd2$w$Weights),
                 row.names=NULL)
weights_2<-cbind(weights_2,interv2=as.data.frame(sea2_imd2$w$Intervention), 
                 row.names=NULL)
colnames(weights_2)[1]<-"id"
weights_2$id<-as.numeric(as.character(weights_2$id))


weights_3<-cbind(ID=row.names(sea2_imd3$w$Weights),as.data.frame(sea2_imd3$w$Weights),
                 row.names=NULL)
weights_3<-cbind(weights_3,interv3=as.data.frame(sea2_imd3$w$Intervention), 
                 row.names=NULL)
colnames(weights_3)[1]<-"id"
weights_3$id<-as.numeric(as.character(weights_3$id))


ls<-list(weights_1,weights_2,weights_3)
strat_weights<-rbindlist(ls)

test0[, wts:=Main]
test0[, Main:=NULL]
test0<-as.data.table(merge(test0,strat_weights[,1:2], by="id", all.x=T ))

table(test0$st_week)

chart_imd <- test0[intervention==0 & meanlftrate<1 & other_lcr==0,
                   admi2_cnt_imd:=weighted.mean(admi2_cnt*Main, w=Main),
                   by=.(st_week, tier3,qc_imd)]

chart_imd[, admi2_imd_rate:=admi2_cnt_imd*100000]
ggplot(chart_imd,aes(x=st_week,y=admi2_imd_rate,group=tier3, color=as.factor(tier3), 
                     lty=as.factor(tier3)))+  
  geom_line(size=1) +facet_grid(qc_imd~.)


test0[intervention==0 & meanlftrate<1 & other_lcr==0, did:=as.numeric(tier3==1 & time>12)]
test0[, wts_imd:=Main]
test0[, Main:=NULL]
svyfull<-svydesign(ids=~0, weights=~wts_imd, data=test0[intervention==0 & meanlftrate<1 & 
                                                          other_lcr==0 & time>12 & time<22])

mod2 <- survey::svyglm(admi2_cnt ~ as.factor(qc_imd)*did+as.factor(qc_imd)*as.factor(time), 
                       design=svyfull, family = poisson(link="log"))
summary(mod2)

resimd<-rbindlist(list(as.data.table(lincom(mod2, c("did"), eform=T)),
                       as.data.table(lincom(mod2, c("did+as.factor(qc_imd)2:did"),eform=T)),
                       as.data.table(lincom(mod2, c("did+as.factor(qc_imd)3:did"),eform=T))))
names(resimd)<-c("est", "lcl", "ucl", "chi", "p")
cols<-c("est", "lcl", "ucl")
resimd[, est:=est-1]
resimd[, ucl:=ucl-1]
resimd[, lcl:=lcl-1]

resimd[2, p2:=lincom(mod2, c("as.factor(qc_imd)2:did"))[5]]
resimd[3, p2:=lincom(mod2, c("as.factor(qc_imd)3:did"))[5]]

resimd[1, result:="IMD1"]
resimd[2, result:="IMD2"]
resimd[3, result:="IMD3"]

############################ differences by new variant ########################

test0[intervention==0 & meanlftrate<1 & other_lcr==0, 
      qc_newvar:=ntile(prop_sgtf, 2), by=.(st_week)]

table(test0[intervention==0 & meanlftrate<1 & other_lcr==0,.(st_week,qc_newvar,tier3)])
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0,.(qc_newvar,tier3)])
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0,.(st_week,tier3)])
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0,.(qc_newvar)])
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0,.(tier3)])


with(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_newvar==2], 
     summary(prop_sgtf))
with(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_newvar==1], 
     summary(prop_sgtf))
summary(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$qc_newvar)
table(test0[intervention==0 & meanlftrate<1 & other_lcr==0]$qc_newvar)


any(is.na(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_newvar==1,
                c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", 
                  "imd_score","av_test","prop_sgtf","admi2_cnt","id", "time", "tier3")]))

cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", 
              "imd_score","av_test","prop_sgtf")
match.out2 <- c("admi2_cnt")
set.seed(6768)
## by selecting intervention==0, ladnm=="Liverpool" MSOAs were excluded.
sea2_newvar1 <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_newvar==1]), 
                           idvar="id", timevar="time", intvar="tier3", 
                           start.pre=1, end.pre=end.pre, end.post=end.post, 
                           match.out=match.out2, match.covar=cov.var2, 
                           result.var=match.out2 , 
                           omnibus.var=match.out2,
                           test="twosided",
                           confidence = 0.95,
                           perm=250,
                           jack=F, 
                           n.cores = 1)

summary(sea2_newvar1 )
plot_microsynth(sea2_newvar1)


set.seed(6768)
sea2_newvar2 <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0 & qc_newvar==2]), 
                           idvar="id", timevar="time", intvar="tier3", 
                           start.pre=1, end.pre=end.pre, end.post=end.post, 
                           match.out=match.out2, match.covar=cov.var2, 
                           result.var=match.out2 , 
                           omnibus.var=match.out2,
                           test="twosided",
                           confidence = 0.95,
                           perm=250,
                           jack=F, 
                           n.cores = 1)

summary(sea2_newvar2)
plot_microsynth(sea2_newvar2)

weights_1<-cbind(ID=row.names(sea2_newvar1$w$Weights),as.data.frame(sea2_newvar1$w$Weights),
                 row.names=NULL)
weights_1<-cbind(weights_1,interv1=as.data.frame(sea2_newvar1$w$Intervention), 
                 row.names=NULL)
colnames(weights_1)[1]<-"id"
weights_1$id<-as.numeric(as.character(weights_1$id))


weights_2<-cbind(ID=row.names(sea2_newvar2$w$Weights),as.data.frame(sea2_newvar2$w$Weights),
                 row.names=NULL)
weights_2<-cbind(weights_2,interv2=as.data.frame(sea2_newvar2$w$Intervention), 
                 row.names=NULL)
colnames(weights_2)[1]<-"id"
weights_2$id<-as.numeric(as.character(weights_2$id))


ls<-list(weights_1,weights_2)
strat_weights<-rbindlist(ls)


test0<-as.data.table(merge(test0,strat_weights[,1:2], by="id", all.x=T))
table(test0$st_week)
chart_newvar <- test0[intervention==0 & meanlftrate<1 & other_lcr==0,
                      admi2_cnt_newvar:=weighted.mean(admi2_cnt*Main, w=Main),
                      by=.(st_week, tier3,qc_newvar)]

chart_newvar[, admi2_newvar_rate:=admi2_cnt_newvar*100000]
ggplot(chart_newvar[!is.na(admi2_newvar_rate)],aes(x=st_week,y=admi2_newvar_rate/100000,
                                                   group=tier3, color=as.factor(tier3), 
                                                   lty=as.factor(tier3)))+  
  geom_line(size=1) +facet_grid(qc_newvar~.)


test0[,wts_newvar:=Main]
test0$Main<-NULL


table(test0$time)

test0[intervention==0 & meanlftrate<1 & other_lcr==0, did:=as.numeric(tier3==1 & time>12)]
svyfull<-svydesign(ids=~0, weights=~wts_newvar, data=test0[intervention==0 & meanlftrate<1 & 
                                                             other_lcr==0 & time>12 & time<22])

mod2 <- survey::svyglm(admi2_cnt ~ as.factor(qc_newvar)*did+as.factor(qc_newvar)*as.factor(time), 
                       design=svyfull, family = poisson(link="log"))
summary(mod2)

res_newvar<-rbindlist(list(as.data.table(lincom(mod2, c("did"), eform=T)),
                           as.data.table(lincom(mod2, c("did+as.factor(qc_newvar)2:did"),eform=T))))
names(res_newvar)<-c("est", "lcl", "ucl", "chi", "p")
cols<-c("est", "lcl", "ucl")
res_newvar[, est:=est-1]
res_newvar[, ucl:=ucl-1]
res_newvar[, lcl:=lcl-1]

res_newvar[2, p2:=lincom(mod2, c("as.factor(qc_newvar)2:did"))[5]]

res_newvar[1, result:=paste(round(min(test0[intervention==0 & meanlftrate<1 & 
                                              other_lcr==0 & qc_newvar==1]$prop_sgtf)), 
                            round(max(test0[intervention==0 & meanlftrate<1 & 
                                              other_lcr==0 & qc_newvar==1]$prop_sgtf)), sep="-")]
res_newvar[2, result:=paste(round(min(test0[intervention==0 & meanlftrate<1 & 
                                              other_lcr==0 & qc_newvar==2]$prop_sgtf)), 
                            round(max(test0[intervention==0 & meanlftrate<1 & 
                                              other_lcr==0 & qc_newvar==2]$prop_sgtf)), sep="-")]

write.csv(res_newvar, normalizePath(file.path(b,"paper/sgtf2.csv")))

########################## Results table ############################
table1 <- as.data.table(sea2$Results$`21`)

table1<-table1[1, c(3,8,9,7)]
names(table1)<-c("est","lcl","ucl","p")
table1[, result:="December - all tier 3"]

ls<-list(table1, resimd, res_newvar )
table2_fin<-rbindlist(ls, fill = T)

write.csv(table2_fin, normalizePath(file.path(b,"paper/table2.csv")))

########################### Table for the appendix #############################
table3 <- as.data.table(lincom(cd3, c("intervention:afterTRUE"), eform=T))
colnames <- names(table3)[1:3]

table3[,(colnames):=lapply(.SD, function(x) 1-x),.SDcols=colnames]


write.csv(table3,
          file= normalizePath(file.path(b,"paper/appendix_tab.csv")))

################ Sensitivity test of removing the 182 MSOAs in Kent ############

kent <- c("Medway","Ashford","Canterbury","Dartford","Dover","Gravesham","Maidstone",
          "Sevenoaks","Folkestone and Hythe","Swale","Thanet","Tonbridge and Malling",
          "Tunbridge Wells")

table(test0[ladnm %in% kent,]$ladnm)
unique(test0[ladnm %in% kent,]$msoa_name)

cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", 
              "imd_score","av_test","prop_sgtf")
match.out2 <- c("admi2_cnt")
set.seed(6768)
## by selecting intervention==0, ladnm=="Liverpool" MSOAs were excluded.
sea2a <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & other_lcr==0
                                        & ladnm %notin% kent]), 
                    idvar="id", timevar="time", intvar="tier3", 
                    start.pre=1, end.pre=end.pre, end.post=end.post, 
                    match.out=match.out2, match.covar=cov.var2, 
                    result.var=match.out2 , 
                    omnibus.var=match.out2,
                    test="twosided",
                    confidence = 0.95,
                    perm=250,
                    jack=F, 
                    n.cores = 1)

summary(sea2a)

table1a <- as.data.table(sea2a$Results$`21`)

table1a<-table1a[1, c(3,8,9,7)]
names(table1a)<-c("est","lcl","ucl","p")
table1a[, result:="December - all tier 3"]

write.csv(table1a, normalizePath(file.path(b,"paper/tab_sf1.csv")))

################ Sensitivity test of including the 200 MSOAs in Liverpool City Region ############
################ and the 142 MSOAs with mean LFT rates above 1 per 100 population per week

table(test0[ladnm %in% kent,]$ladnm)
unique(test0[ladnm %in% kent,]$msoa_name)
unique(test0[ladnm %in% kent & meanlftrate>=1,]$msoa_name)
unique(test0[intervention==1,]$msoa_name)
unique(test0[intervention==1 | other_lcr==1,]$msoa_name)
unique(test0[intervention==0 & meanlftrate>=1 & other_lcr==0]$msoa_name)


cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", 
              "imd_score","av_test","prop_sgtf")
match.out2 <- c("admi2_cnt")
set.seed(6768)

sea2b <- microsynth(as.data.frame(test0), 
                    idvar="id", timevar="time", intvar="tier3", 
                    start.pre=1, end.pre=end.pre, end.post=end.post, 
                    match.out=match.out2, match.covar=cov.var2, 
                    result.var=match.out2 , 
                    omnibus.var=match.out2,
                    test="twosided",
                    confidence = 0.95,
                    perm=250,
                    jack=F, 
                    n.cores = 1)

summary(sea2b)

table1b <- as.data.table(sea2b$Results$`21`)

table1b<-table1b[1, c(3,8,9,7)]
names(table1b)<-c("est","lcl","ucl","p")
table1b[, result:="December - all tier 3"]

write.csv(table1b, normalizePath(file.path(b,"paper/tab_sf2.csv")))

################ Sensitivity tests of the spatial spill-over effect ############
msoa_centroid <- st_read(normalizePath(file.path(b,"msoa_population_weighted_centroids.shp")))
st_set_crs(msoa_centroid,27700)
plot(msoa_centroid$geometry)

# remove msoas in Wales
msoa_centroid <- msoa_centroid[substr(msoa_centroid$msoa11cd, 1,1)=="E", ]
msoa_centroid <- merge(msoa_centroid,test0[st_week==0 & intervention==0 & meanlftrate<1 & other_lcr==0,
                                           .(st_week,time,intervention,tier3,msoa11_cd,admi2_cnt,Main)],
                       by.x="msoa11cd", by.y="msoa11_cd", all.x=T)


table(msoa_centroid$tier3)
class(msoa_centroid$tier3)

msoa_dist <- st_distance(msoa_centroid,msoa_centroid)
msoa_dist <- as.data.frame(msoa_dist)

st_write(msoa_centroid,dsn = normalizePath(file.path(b,"paper/msoa_centroid.shp")), 
         driver = "ESRI Shapefile", append=FALSE)

class(msoa_centroid$msoa11cd)
rnam <- msoa_centroid %>%
  as.data.frame() %>%
  dplyr::select(msoa11cd) %>%
  as.vector()
rnam$msoa11cd <- as.character(rnam$msoa11cd)
class(rnam$msoa11cd)
class(msoa_dist)
rownames(msoa_dist) = make.names(rnam$msoa11cd, unique=TRUE)
colnames(msoa_dist) = make.names(rnam$msoa11cd, unique=TRUE)


length(unique(unlist(msoa_dist)))
summary(unique(unlist(msoa_dist)))

#if the distance threshold is set as 50000
str(msoa_dist)
msoa_dist <- as.data.table(cbind(expand.grid(dimnames(as.matrix(msoa_dist))), 
                                 value = as.vector(as.matrix(msoa_dist))))
class(msoa_dist)
msoa_dist <- msoa_dist[Var1 != Var2,]
msoa_dist[value==0,]

table(msoa_centroid$tier3)
class(msoa_dist)
str(msoa_dist)

msoa_dist$Var1 <- as.character(msoa_dist$Var1)
msoa_dist$Var2 <- as.character(msoa_dist$Var2)
str(msoa_dist)

tier2_remove <- msoa_dist[Var1 %in% unique(test0[tier3==1]$msoa11_cd) &
                            Var2 %in% unique(test0[tier3==0]$msoa11_cd) &
                            value<= 20000]
NROW(unique(tier2_remove$Var2))
NROW(unique(tier2_remove$Var1))
NROW(unique(tier2_remove$Var2))/NROW(unique(test0[tier3==0]$msoa11_cd)) 

NROW(unique(test0[intervention==0 & meanlftrate<1 & 
                    other_lcr==0 & tier3!=1]$msoa11_cd))

NROW(unique(test0[intervention==0 & meanlftrate<1 & 
                    other_lcr==0 & tier3!=1 &
                    msoa11_cd %notin% unique(tier2_remove$Var2)]$msoa11_cd))

NROW(unique(test0[intervention==0 & meanlftrate<1 & 
                    other_lcr==0 & tier3!=1]$msoa11_cd)) -
  NROW(unique(test0[intervention==0 & meanlftrate<1 & 
                      other_lcr==0 & tier3!=1 &
                      msoa11_cd %notin% unique(tier2_remove$Var2)]$msoa11_cd))

cov.var2 <- c("total_pop", "prop_70plus", "prop_bame", "pop_dens", "as_prop_vulner", 
              "imd_score","av_test","prop_sgtf")
match.out2 <- c("admi2_cnt")
set.seed(6768)

## by selecting intervention==0, ladnm=="Liverpool" MSOAs were excluded.
sea2c <- microsynth(as.data.frame(test0[intervention==0 & meanlftrate<1 & 
                                          other_lcr==0 &
                                          msoa11_cd %notin% unique(tier2_remove$Var2)]), 
                    idvar="id", timevar="time", intvar="tier3", 
                    start.pre=1, end.pre=end.pre, end.post=end.post, 
                    match.out=match.out2, match.covar=cov.var2, 
                    result.var=match.out2 , 
                    omnibus.var=match.out2,
                    test="twosided",
                    confidence = 0.95,
                    perm=250,
                    jack=F, 
                    n.cores = 1)

summary(sea2c)

table1c <- as.data.table(sea2c$Results$`21`)

table1c<-table1c[1, c(3,8,9,7)]
names(table1c)<-c("est","lcl","ucl","p")
table1c[, result:="December - all tier 3"]

write.csv(table1c, normalizePath(file.path(b,"paper/tab_sf3.csv")))

## plot out the spillover areas
ggplot() +
  geom_sf(data = adj2_msoa[adj2_msoa$msoa11cd %notin% unique(tier2_remove$Var2),],
          aes(fill = as.factor(tier3)),
          colour = NA,
          lwd = 0) +
  scale_fill_viridis_d(alpha = 1,
                       breaks=c(0,1),
                       labels=c("Synthetic Control", "Tier 3"),
                       na.value = "white") +
  geom_sf(data = lad,aes(fill = NA), na.rm = F,alpha=0,size = 0.1) +
  guides(fill = guide_legend(title = "")) +
  theme_void() +
  theme(legend.text=element_text(size=14))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(normalizePath(file.path(b,"paper/tier3_spillover_areas.png")), width = 16, height = 14,device="png",
       units = "in", dpi=700)
ggsave(normalizePath(file.path(b,"paper/tier3_spillover_areas.svg")), width = 16, height = 14,device="svg",
       units = "in", dpi=700)


########################### Plotting out the lft rate #######################

# Figure SF2 distribution of the LFT rate
plot_lft<-unique(test0[date<as.Date("2021-01-03"), 
                       .(ladcd, lad19_nm, date,lfd_test_la, an_pop, intervention, tier3)])


plot_lft[, lftrate:=lfd_test_la*100/an_pop, by=.(ladcd, date)]

plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02"), 
         meanlftrate:=max(lftrate, na.rm = T), by=.(ladcd)]

sum(plot_lft[date<as.Date("2021-01-03") & intervention==1, lfd_test_la])
sum(plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02") & intervention==1, lfd_test_la])

sum(plot_lft[date<as.Date("2021-01-03") & intervention==1, meanlftrate])
sum(plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02") & intervention==1, meanlftrate])


ggplot(plot_lft[date<=as.Date("2021-01-02")],aes(y=lftrate, x=date, 
                                                 group=ladcd, 
                                                 color=as.factor(intervention)))+
  geom_line()

ggplot(plot_lft[date<=as.Date("2021-01-02") & (meanlftrate<1|intervention==1)],
       aes(y=lftrate, x=date, group=ladcd, color=as.factor(intervention)))+
  geom_line()

check_dist<-unique(plot_lft[date>as.Date("2020-11-05") & date<=as.Date("2021-01-02")&is.na(meanlftrate)==F, 
                            .(meanlftrate,ladcd,intervention)])
table(check_dist$intervention)

check_dist[intervention==1]$meanlftrate

quantile(na.omit(check_dist$meanlftrate), probs = 0.84)
quantile(na.omit(check_dist$meanlftrate), probs = 0.8673)

ggplot(check_dist,aes(meanlftrate))+
  geom_histogram(binwidth = 0.1,fill = "white",colour="black") +
  geom_vline(aes(xintercept = 1), linetype = "longdash")+
  # geom_text(x=c(1.5,4), y=c(60,60), label=c("Included: n= 6290","Excluded: n= 142")) +
  theme_classic()+
  theme(axis.title=element_text(colour="black",size=16,face="bold"),
        axis.text=element_text(colour="black",size=14))+
  xlab("Mean weekly LFT testing rates (%)") + 
  ylab("Numbers of Local Authorities") +
  ylim(0,70)

ggsave("mean_lft_rate.png",path=normalizePath(file.path(b,"paper")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("mean_lft_rate.svg",path=normalizePath(file.path(b,"paper")),
       width = 16, height = 8, units="in",dpi=600)

# Figure SF1 trend of the LFT rate by time
plot_lft<-unique(test0[, .(ladcd, lad19_nm, date,lfd_test_la, an_pop, tier3)])
plot_lft[, lftrate:=lfd_test_la*100/an_pop, by=.(ladcd, date)]
plot_lft[, meanlftrate:=max(lftrate, na.rm = T), by=.(ladcd)]

plot_lft<-plot_lft[, list(lft=sum(lfd_test_la),pop=sum(an_pop)), 
                   by=.(date)]
plot_lft<-plot_lft[, rate:=lft*100/pop,  by=.(date)]
plot_lft<-plot_lft[, ucl:=100*pois.exact(lft,pop)[5],  by=.(date)]
plot_lft<-plot_lft[, lcl:=100*pois.exact(lft,pop)[4],  by=.(date)]


text_plot <- data.frame(text = c("Liverpool community testing\npilot begins",
                                 "National rollout of\ncommunity testing"), 
                        dates = as.Date(c("2020-11-06","2021-01-02")), 
                        stringsAsFactors = FALSE)

ggplot(data=plot_lft[date<=as.Date("2021-02-22")], 
       aes(x = as.Date(date), y = rate))+ 
  geom_line(size=1)+
  scale_x_date(date_breaks ="weeks" )+
  scale_linetype(guide="none")+
  geom_vline(xintercept = as.Date("2020-11-06"), show.legend = FALSE, linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-01-02"), show.legend = FALSE, linetype = "dotted") +
  xlab("Week start date") + ylab("LFT tests per 100 population")+
  geom_text(aes(x=c(dates[1],dates[2]), 
                label=text, y=c(5,5)), data = text_plot, 
            colour="black", size=6, fontface = "bold.italic",hjust=c(0,0)) +
  theme_classic()+
  theme(text = element_text(size=16),legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=24),
        legend.title = element_blank(),legend.key.size = unit(1, "cm"), 
        axis.text.x = element_text(angle = 45,hjust=1))+ theme(strip.background = element_blank())

ggsave("lft_trend.png",path=normalizePath(file.path(b,"paper")),
       width = 16, height = 8, units="in",dpi=600)
ggsave("lft_trend.svg",path=normalizePath(file.path(b,"paper")),
       width = 16, height = 8, units="in",dpi=600)





