#library(devtools)
#install_github("waldronlab/bugsigdbr")
#install_github("waldronlab/BugSigDBStats")

#library(bugsigdbr)
#library(BugSigDBStats)

#d<-read.csv("NO.csv", header = T)
#d$year<- "year"

#for (x in 1:dim(d)[1]){
#d$year[x]<-pmid2pubyear(d$pmid[x])
#}

library(ggpubr)
library(dplyr)
library(pals)
library(patchwork)
library(forcats)
library(plotly)
library(scales) 

d<-read.csv("~/Library/CloudStorage/OneDrive-uoflhealth/NO Trial Designs/NO_Final.csv", header = T, stringsAsFactors = T)

length(unique(d$pmid))
summary(d$Year)
length(d$Outcome)

diag <- d[!duplicated(d$pmid), ]
sort(summary(factor(diag$glioma3)))

table(d$Phase, d$Outcome)
fisher.test(table(d$Phase, d$Outcome))

give.n <- function(x) {return(data.frame(y = max(x)+5, label = paste0("n=", length(x))))}

##### FIGURE 1 ######

#Distribution of Year
a<-gghistogram(d[!duplicated(d[ ,"pmid"]),], x = "Year", bins = 47, fill="lightblue", ylab = "Trials Included", xlab="Publication Year",add = "median")+
  ggtitle("Yearly Distribution of Trials")+labs(tag = "A")+theme(plot.tag = element_text(face = "bold"))

#Distribution of Diagnosis and Recurrence
d1<-structure(list(top_level = c("New", "New", "New", "New", "New", 
                                 "New", "New", "New", "New", "Recurrent", "Recurrent", "Recurrent", 
                                 "New or recurrent", "New or recurrent", "New or recurrent", "New", 
                                 "Recurrent", "New or recurrent"), value = c("GBM", "HGG", "G3G", 
                                                                             "LGG", "1p/19q-codel", "MB", "AA", "DIPG", "EPN", "GBM", "HGG", 
                                                                             "AA", "GBM", "HGG", "MB", "New", "Recurrent", "New or recurrent"
                                 ), width = c(92, 51, 11, 9, 7, 6, 5, 5, 1, 47, 9, 1, 4, 1, 1, 
                                              187, 57, 6), name = c("glioma3", "glioma3", "glioma3", "glioma3", 
                                                                    "glioma3", "glioma3", "glioma3", "glioma3", "glioma3", "glioma3", 
                                                                    "glioma3", "glioma3", "glioma3", "glioma3", "glioma3", "occur", 
                                                                    "occur", "occur"), ymid = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                                                                                                2, 2, 2, 2, 1, 1, 1), ymax = c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 
                                                                                                                               2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 1.5, 1.5, 1.5), 
                   ymin = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 
                            1.5, 1.5, 1.5, 1.5, 1.5, 0.5, 0.5, 0.5), xmin = c(0, 92, 
                                                                              143, 154, 163, 170, 176, 181, 186, 187, 234, 243, 244, 248, 
                                                                              249, 0, 187, 244), xmax = c(92, 143, 154, 163, 170, 176, 
                                                                                                          181, 186, 187, 234, 243, 244, 248, 249, 250, 187, 244, 250
                                                                              ), xmid = c(46, 117.5, 148.5, 158.5, 166.5, 173, 178.5, 183.5, 
                                                                                          186.5, 210.5, 238.5, 243.5, 246, 248.5, 249.5, 93.5, 215.5, 
                                                                                          247), Location = c("Diagnosis", "Diagnosis", "Diagnosis", 
                                                                                                             "Diagnosis", "Diagnosis", "Diagnosis", "Diagnosis", "Diagnosis", 
                                                                                                             "Diagnosis", "Diagnosis", "Diagnosis", "Diagnosis", "Diagnosis", 
                                                                                                             "Diagnosis", "Diagnosis", "Occurence", "Occurence", "Occurence"
                                                                                          )), row.names = c(NA, -18L), class = c("tbl_df", "tbl", "data.frame"
                                                                                          ))

b<-ggplot() +
  geom_rect(data=d1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill=value, color = value), linewidth = 0.5) +
  geom_text(data = d1, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, 
                           label = c("GBM","HGG","","","","","","","","GBM","HGG","","","","","New","Recurrent","")), size = 4, color = "black") +
  scale_y_continuous(limits = c(0.5, 2.5),breaks = c(0, 1, 2),  # Define breaks at these points
    #labels = c("", "Occurrence", "Diagnosis")
    ) +
  scale_color_manual(values=c(rep("black",12)))+
  scale_fill_manual(values=unname(polychrome()))+
  #xlab("Number of Endpoints")+
  theme_pubr()+scale_x_continuous(expand = c(0, 0)) + theme(legend.position = "right",plot.tag = element_text(face = "bold"))+
    rremove("axis")+rremove("ticks")+rremove("xylab")+rremove("xy.text")+
  #geom_text(aes(x = 120, y = 5.6, label = "EPN\nG3G\nDIPG\nLGG\nAA\nMB\n1p/19q-codel"), hjust = 1, size = 4, color = "black")+
  ggtitle("Eligibility Diagnosis")+coord_polar('x')+
  labs(fill = "Color Legend", color = "Color Legend",tag="B")

#phase distribution
c<-ggplot(d %>% 
            count(Phase) %>% 
            mutate(pct = n / sum(n)), aes(x = "", y = pct, fill = Phase)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(x = 1, label = scales::percent(pct, accuracy = .1)), color="white",position = position_stack(vjust = .5)) +
  coord_flip()+ theme_void()+theme(legend.position = "top", plot.tag = element_text(face="bold"))+
  scale_fill_manual(labels = c("2", "3", "2/3"), values=c("darkgreen","brown","black"))+
  ggtitle("Trial Characteristics")+labs(tag = "C")

#outcome distribution
d2<-ggplot(d %>% 
            count(Outcome) %>% 
            mutate(pct = n / sum(n)), aes(x = "", y = pct, fill = Outcome)) +
  geom_bar(stat = "identity", color = "white") +scale_fill_manual(values = c("red","blue"), label=c("OS", "PFS-like"))+
  geom_text(aes(x = 1, label = scales::percent(pct, accuracy = .1)), color="white", position = position_stack(vjust = .5)) +
  coord_flip()+ theme_void()+theme(legend.position = "top")

#Hypotheses Distribution
d$Hypothesis<-factor(d$Hypothesis, levels = c("Superiority","Non-inferiority","Equivalence","N/A"))
e<-ggplot(d %>% 
             count(Hypothesis) %>% 
             mutate(pct = n / sum(n)), aes(x = "", y = pct, fill = Hypothesis)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(x = 1, label = scales::percent(pct, accuracy = .1)), position = position_stack(vjust = .5)) +
  coord_flip()+ theme_void()+theme(legend.position = "top")

(free(a)+free(b)+(c/d2/e))+plot_layout(widths = c(1,2,1.5))
#14x4in PDF

##### FIGURE 2 ######

levels(d$Phase)[levels(d$Phase) == "II/III"] <- "2"

#quartiles <- d |> 
#  group_by(Phase, Outcome) |>
#  reframe(y = quantile(Type1, c(.25, .5, .75), na.rm = T))
#quartiles$y<-round(quartiles$y,2)
f<-ggplot(d, aes(x = Phase, y = Type1/100)) + geom_boxplot(outlier.shape = NA,width=0.4)+geom_jitter(aes(colour = Sided),width = 0.2)+
  #geom_text(data=quartiles,aes(Phase,y/100,label = y), nudge_x = .23, hjust = 0)+
  scale_y_continuous(labels = scales::percent)+stat_compare_means(label.y = 0.17)+theme_pubr()+facet_wrap("Outcome")+
  ggtitle("Type 1 Error")+ylab("Type 1 Error")+
  theme(legend.position = "right",plot.tag = element_text(face = "bold"))+labs(tag = "A")

ff<-ggplot(d[!d$Sided=="N/A",], aes(x = Sided, y = Type1/100)) + geom_boxplot(outlier.shape = NA,width=0.4)+geom_jitter(width = 0.2)+
  #geom_text(data=quartiles,aes(Phase,y/100,label = y), nudge_x = .23, hjust = 0)+
  scale_y_continuous(labels = scales::percent)+labs(tag = "B")+
  stat_compare_means(label.y = 0.17)+theme_pubr()+xlab("Sidedness")+ylab("Type 1 Error")+facet_wrap("Outcome")+
  ggtitle("Sidedness")+theme(strip.background = element_blank(),strip.text.x = element_blank(),plot.tag = element_text(face = "bold"))

fisher.test(table(d[!d$Sided=="N/A",]$Phase, d[!d$Sided=="N/A",]$Sided))

#quartiles <- d |> 
#  group_by(Phase, Outcome) |>
#  reframe(y = quantile(Type2, c(.25, .5, .75), na.rm = T))
#quartiles$y<-round(quartiles$y,2)
g<-ggplot(d, aes(x = Phase, y = Type2/100)) + geom_boxplot(outlier.shape = NA,width=0.4)+geom_jitter(width = 0.2)+
  #geom_text(data=quartiles,aes(Phase,y/100,label = y), nudge_x = .23, hjust = 0)+
  scale_y_continuous(labels = scales::percent)+labs(tag = "C")+
  stat_compare_means(label.y = 0.64)+theme_pubr()+xlab("Phase")+ylab("Power")+facet_wrap("Outcome")+
  ggtitle("Power")+theme(strip.background = element_blank(),strip.text.x = element_blank(),plot.tag = element_text(face = "bold"))

f/ff/g
#9 x 5.5 in PDF (portrait)

Mode <- function(x) {
  ux <- na.omit(x)              # remove NA
  ux <- unique(ux)              # unique values
  ux[which.max(tabulate(match(x, ux)))]  # most frequent
}
Mode(d$Type1)

wilcox.test(Type1 ~ Phase, data = d, na.action = na.omit)
wilcox.test(Type1 ~ Sided, data = d[!d$Sided=="N/A",], na.action = na.omit)


##### FIGURE 3 ######

#control median survivals
mos<-subset(d, Outcome=="OS" & !is.na(plan.con.median)) %>% add_count(glioma2) %>% filter(n > 1) %>% distinct(pmid, .keep_all = TRUE)
h<-ggplot(mos, aes(x=fct_reorder(glioma2, plan.con.median, function(x) quantile(x, 0.5)), y=plan.con.median))+geom_boxplot(fill="orangered")+
  theme_pubr()+xlab("Diagnosis")+ylab("Median OS (months)")+theme(plot.tag = element_text(face = "bold"))+
  coord_flip(ylim = c(0, 60))+ggtitle("Expected Median OS in Control Arms")+labs(tag = "A")
hi<-ggplot(mos, aes(fct_reorder(glioma2, plan.con.median, function(x) quantile(x, 0.5))))+geom_bar()+
  scale_y_continuous(breaks = c(0,15,30), labels = c("0","15","30"), limits = c(0,36))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.25) +ylab("Trials")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+
  coord_flip()

summary(mos[mos$glioma2=="New GBM","plan.con.median"])

mpfs<-subset(d, Outcome=="PFS" & !is.na(plan.con.median)) %>% add_count(glioma2) %>% filter(n > 1) %>% distinct(pmid, .keep_all = TRUE)
i<-ggplot(mpfs, aes(x=fct_reorder(glioma2, plan.con.median, function(x) quantile(x, 0.5)), y=plan.con.median))+geom_boxplot(fill="dodgerblue")+
  theme_pubr()+xlab("Diagnosis")+ylab("Median PFS (months)")+theme(plot.tag = element_text(face = "bold"))+
  coord_flip()+ggtitle("Expected Median PFS in Control Arms")+labs(tag = "B")+rremove("ylab")
ii<-ggplot(mpfs, aes(fct_reorder(glioma2, plan.con.median, function(x) quantile(x, 0.5))))+geom_bar()+
  scale_y_continuous(breaks = c(0,5,10), labels = c("0","5","10"), limits = c(0,14))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.25) +ylab("Trials")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+
  coord_flip()

d$dev<-d$plan.con.median-d$final.con.median
dos<-subset(d, Outcome=="OS" & !is.na(dev)) %>% distinct(pmid, .keep_all = TRUE)
j<-ggplot(dos, aes(x=fct_reorder(glioma2, dev, function(x) quantile(x, 0.5)), y=dev))+geom_boxplot(fill="orangered")+
  geom_hline(yintercept = 0, linetype="longdash")+theme_pubr()+theme(plot.tag = element_text(face = "bold"))+
  xlab("Diagnosis")+ylab("Diff. in Median OS (Expected - Final)")+scale_y_continuous(breaks = seq(-30,20,10))+
  #annotate("text", x = 7, y = -20, label = "Underestimated", size=4)+
  coord_flip(ylim = c(-15, 15))+ggtitle("Deviation in Median OS in Control Arms")+labs(tag = "C")
ji<-ggplot(dos, aes(fct_reorder(glioma2, dev, function(x) quantile(x, 0.5))))+geom_bar()+
  scale_y_continuous(breaks = c(0,10,20), labels = c("0","10","20"), limits = c(0,25))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.25) +ylab("Trials")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+
  coord_flip()

#ggplot(d, aes(plan.con.median, final.con.median, color=Outcome))+geom_point()+geom_abline(slope = 1, intercept = 0, linetype="longdash")+
#  theme_pubr()+geom_smooth(method='lm', formula= y~x, se = F)+xlim(0,65)+ylim(0,65)
wilcox.test(d$plan.con.median, d$final.con.median, paired=T)
gbm<-subset(d,glioma2=="New GBM")
wilcox.test(gbm$plan.con.median, gbm$final.con.median, paired=T)

dpfs<-subset(d, Outcome=="PFS" & !is.na(dev)) %>% distinct(pmid, .keep_all = TRUE)
k<-ggplot(dpfs, aes(x=fct_reorder(glioma2, dev, function(x) quantile(x, 0.5)), y=dev))+geom_boxplot(fill="dodgerblue")+
  geom_hline(yintercept = 0, linetype="longdash")+theme_pubr()+theme(plot.tag = element_text(face = "bold"))+
  ylab("Diff. in Median PFS (Expected - Final)")+rremove("ylab")+
  #annotate("text", x = 6, y = -3.75, label = "Underestimated", size=4)+
  coord_flip(ylim = c(-4, 4))+ggtitle("Deviation in Median PFS in Control Arms")+labs(tag = "D")
ki<-ggplot(dpfs, aes(fct_reorder(glioma2, dev, function(x) quantile(x, 0.5))))+geom_bar()+
  scale_y_continuous(breaks = c(0,5,10,15), labels = c("0","5","10","15"), limits = c(0,15))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.25) +ylab("Trials")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+
  coord_flip()

gbm<-subset(d,glioma2=="New GBM") %>% distinct(pmid, .keep_all = TRUE)
ostime<-gbm[gbm$Outcome=="OS",] %>% distinct(pmid, .keep_all = TRUE)
lm_eqn <- function(df, y, x){
  m <- lm(y ~ x, df)
  p_value <- summary(m)$coefficients[2, 4]
  eq <- substitute(
    italic(y)==a + b*italic(x) * ", "* italic(r)^2*"="*r2 * ", p=" ~ pval, 
    list(
      a = format(unname(coef(m)[1]), digits = 2),
      b = format(unname(coef(m)[2]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 2),
      pval = format(p_value, digits = 2, scientific = T)))
  as.character(as.expression(eq))}

pt<-ggplot(ostime, aes(Year,final.con.median))+geom_smooth(method='lm', formula= y~x, se = T,color = "orangered", fill = "orangered")+geom_point()+
  geom_text(x = 2000, y = 26, label = lm_eqn(ostime, ostime$final.con.median, ostime$Year), parse = TRUE)+theme_pubr()+
  xlab("Year of Trial Publication")+ylab("Median OS")+ggtitle("Control Arm Survival in\nNew GBM Trials Over Time")+
  theme(plot.tag = element_text(face = "bold"))+labs(tag = "E")+ylim(5,31.5)

ft<-ggplot(ostime, aes(Year,plan.con.median))+geom_smooth(method='lm', formula= y~x, se = T,color = "orangered", fill = "orangered")+geom_point()+
  geom_text(x = 2000, y = 26, label = lm_eqn(ostime, ostime$plan.con.median, ostime$Year), parse = TRUE)+theme_pubr()+
  xlab("Year of Trial Publication")+rremove("ylab")+ggtitle("Expected Control Arm Survival in\nNew GBM Trials Over Time")+
  theme(plot.tag = element_text(face = "bold"))+labs(tag = "F")+ylim(5,31.5)

#h + hi + i + ii + j + ji + k + ki + pt + plot_spacer() + ft + plot_spacer() + plot_layout(widths = c(4,1,4,1))

(h + hi + i + ii + 
 j + ji + k + ki + 
    pt + ft) + 
  plot_layout(design = "
      ABCD
      EFGH
      IIKK",
    widths = c(4, 1, 4, 1))
#12.5 x 9 in PDF (landscape)

##### FIGURE 4 ######

#Hazard ratio based on phase, outcome, and diagnosis
d$plan.hr<-ifelse(d$plan.hr>1, 1/d$plan.hr,d$plan.hr)
quartiles <- d |> 
  group_by(Phase, Outcome) |>
  reframe(y = quantile(plan.hr, c(.25, .5, .75), na.rm = T))
quartiles$y<-round(quartiles$y,2)
#run give.n function before this
ggplot(d, aes(x = Phase, y = plan.hr)) + geom_boxplot(width=0.4)+geom_jitter(width = 0.2)+
  geom_text(data=quartiles,aes(Phase,y,label = y), nudge_x = .23, hjust = 0)+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median, position = position_dodge2(width = 0.75), size = 4)+
  stat_compare_means(label.y = 0.9)+theme_pubr()+xlab("Phase")+ylab("Hazard Ratio")+facet_wrap(vars(Outcome))+ggtitle("Target Effect Size")


d3os<-subset(d, Outcome=="OS" & Phase==3 & !is.na(plan.hr)) %>% add_count(glioma2) %>% filter(n > 1) #these are endpoints/arms so unique pmids are not filtered.
l<-ggplot(d3os, aes(x=fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.75)), y=plan.hr))+geom_boxplot(fill="red")+
  theme_pubr()+geom_hline(yintercept = 0.75, linetype="longdash")+geom_hline(yintercept = 0.67, linetype="longdash")+
  xlab("Diagnosis")+ylab("Targeted Hazard Ratio")+theme(plot.tag = element_text(face = "bold"))+
  coord_flip(ylim = c(0.45, 0.8))+ggtitle("OS Targeted HR - Phase 3 Trials")+labs(tag = "A")
li<-ggplot(d3os, aes(fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.75))))+geom_bar()+
  scale_y_continuous(breaks = c(0,20,40), labels = c("0", "20", "40"), limits = c(0,43))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.5) +ylab("Arms")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+
  coord_flip()
summary(d3os[d3os$glioma2=="New GBM","plan.hr"])

d3pfs<-subset(d, Outcome=="PFS" & Phase==3 & !is.na(plan.hr)) %>% add_count(glioma2) %>% filter(n > 1)
m<-ggplot(d3pfs, aes(x=fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.75)), y=plan.hr))+geom_boxplot(fill="dodgerblue")+
  theme_pubr()+geom_hline(yintercept = 0.75, linetype="longdash")+geom_hline(yintercept = 0.67, linetype="longdash")+
  ylab("Targeted Hazard Ratio")+theme(plot.tag = element_text(face = "bold"))+rremove("ylab")+
  coord_flip(ylim = c(0.45, 0.8))+ggtitle("PFS Targeted HR - Phase 3 Trials")+labs(tag = "B")
mi<-ggplot(d3pfs, aes(fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.75))))+geom_bar()+
  scale_y_continuous(breaks = c(0, 10, 20), labels = c("0", "10", "20"), limits = c(0,22))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.5) +ylab("Arms")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+coord_flip()
summary(d3pfs[d3pfs$glioma2=="New GBM","plan.hr"])

d2os<-subset(d, Outcome=="OS" & Phase==2 & !is.na(plan.hr)) %>% add_count(glioma2) %>% filter(n > 1)
n<-ggplot(d2os, aes(x=fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.5)), y=plan.hr))+geom_boxplot(fill="magenta")+
  theme_pubr()+geom_hline(yintercept = 0.75, linetype="longdash")+geom_hline(yintercept = 0.67, linetype="longdash")+
  xlab("Diagnosis")+ylab("Targeted Hazard Ratio")+theme(plot.tag = element_text(face = "bold"))+
  coord_flip(ylim = c(0.45, 0.8))+ggtitle("OS Targeted HR - Phase 2 Trials")+labs(tag = "C")
ni<-ggplot(d2os, aes(fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.5))))+geom_bar()+
  scale_y_continuous(breaks = c(0, 5, 10, 15), labels = c("0", "5", "10", "15"), limits = c(0,18))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.5) +ylab("Arms")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+coord_flip()

#p-values comparing Phase 2 and 3 OS and PFS HRs - run the give.n function below first.
ggplot(d, aes(Phase, plan.hr))+geom_boxplot()+stat_compare_means()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = max, position = position_dodge2(width = 0.75), size = 4)+
  facet_grid(Outcome ~ glioma2)

d2pfs<-subset(d, Outcome=="PFS" & Phase==2 & !is.na(plan.hr)) %>% add_count(glioma2) %>% filter(n > 1)
o<-ggplot(d2pfs, aes(x=fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.75)), y=plan.hr))+geom_boxplot(fill="cadetblue")+
  theme_pubr()+geom_hline(yintercept = 0.75, linetype="longdash")+geom_hline(yintercept = 0.67, linetype="longdash")+
  ylab("Targeted Hazard Ratio")+theme(plot.tag = element_text(face = "bold"))+rremove("ylab")+
  coord_flip(ylim = c(0.45, 0.8))+ggtitle("PFS Targeted HR - Phase 2 Trials")+labs(tag = "D")
oi<-ggplot(d2pfs, aes(fct_reorder(glioma2, plan.hr, function(x) quantile(x, 0.75))))+geom_bar()+
  scale_y_continuous(breaks = c(0, 10, 20), labels = c("0", "10", "20"), limits = c(0,22))+
  geom_text(stat = 'count', aes(label = after_stat(count)), hjust = -0.5) +ylab("Arms")+theme_pubr()+
  rremove("y.axis")+rremove("ylab")+rremove("y.text")+rremove("y.ticks")+coord_flip()

ostime2<-d[d$Outcome=="OS" & d$glioma2=="New GBM",]
pfstime<-d[d$Outcome=="PFS" & d$glioma2=="New GBM",]

shade_data1 <- data.frame(y = c(0.76,0.6,0.37,1),
                         x = c(2000, 2000, 2025, 2025) )

oht<-ggplot(d[d$Outcome=="OS" & d$glioma2=="New GBM",], aes(Year,plan.hr))+
  geom_polygon(data = shade_data1, aes(x = x, y = y), fill = "grey80", alpha = 0.5) + # Shaded area
  geom_smooth(method='lm', formula= y~x, se = T,color = "sandybrown", fill = "sandybrown")+geom_point(aes(color = Phase))+
  geom_text(x = 2004, y = .98, label = lm_eqn(ostime2, ostime2$plan.hr, ostime2$Year), parse = TRUE)+theme_pubr()+
  xlab("Year of Trial Publication")+ylab("Target Hazard Ratio")+ggtitle("OS Targeted HR - New GBM Trials")+
  theme(plot.tag = element_text(face = "bold"), legend.position = c(0.1, 0.25))+labs(tag = "X")+ylim(0.35,1)+xlim(1990,2025)+
  scale_color_manual(values = c("2" = "magenta", "3" = "red"))+labs(tag = "E")

pht<-ggplot(d[d$Outcome=="PFS" & d$glioma2=="New GBM",], aes(Year,plan.hr))+geom_smooth(method='lm', formula= y~x, se = T,color = "dodgerblue", fill = "dodgerblue")+geom_point(aes(color = Phase))+
  geom_text(x = 2003, y = .98, label = lm_eqn(pfstime, pfstime$plan.hr, pfstime$Year), parse = TRUE)+theme_pubr()+
  xlab("Year of Trial Publication")+rremove("ylab")+ggtitle("PFS Targeted HR - New GBM Trials")+
  theme(plot.tag = element_text(face = "bold"), legend.position = c(0.1, 0.25))+labs(tag = "X")+ylim(0.35,1)+xlim(1990,2025)+
  scale_color_manual(values = c("2" = "cadetblue", "3" = "blue"))+labs(tag = "F")

(l + li + m + mi +
  n + ni + o + oi +
    oht + pht) + 
  plot_layout(design = "
      ABCD
      EFGH
      IIKK", widths = c(4, 1, 4, 1))
#12.5 x 9 in PDF (landscape)

##### FIGURE 5 ######

perctime<-d[,c(which(colnames(d)=="plan.con.time"),which(colnames(d)=="plan.con.perc.surv"),
               which(colnames(d)=="final.con.time"),which(colnames(d)=="final.con.perc.surv"),
               which(colnames(d)=="glioma2"),which(colnames(d)=="ss.achieved"),which(colnames(d)=="Outcome"))]
perctime<-rbind(perctime,perctime)
perctime$plan.con.time[251:500]<-perctime$final.con.time[1:250]
perctime$plan.con.perc.surv[251:500]<-perctime$final.con.perc.surv[1:250]
perctime$time<-c(rep("Expected",250),rep("Final",250))

perctime$final.con.perc.surv<-perctime$final.con.time<-NULL
perctime<-perctime[!is.na(perctime$plan.con.time),]
#number of different times
summary(factor(perctime[perctime$Outcome=="OS",]$plan.con.time))
summary(factor(perctime[perctime$Outcome=="PFS",]$plan.con.time))

perctime<-perctime[!is.na(perctime$plan.con.perc.surv),]
pos<-perctime[perctime$Outcome=="OS",]
pos<-pos[pos$glioma2=="New GBM"|pos$glioma2=="New HGG"|pos$glioma2=="New LGG",]
pos<-pos[pos$plan.con.time==9|pos$plan.con.time==12|pos$plan.con.time==24|pos$plan.con.time==60,]
pos<-pos[!is.na(pos$plan.con.time),]
pos<-pos[!(pos$glioma2=="New GBM" & pos$plan.con.time==60),]
pos<-pos[!(pos$glioma2=="New HGG" & pos$plan.con.time==12),]

pos$glioma2<-factor(pos$glioma2, levels = c("New GBM","New HGG","New LGG"))
pos[pos$glioma2=="New GBM" & pos$time=="Expected","plan.con.perc.surv"]
t<-ggplot(pos, aes(factor(plan.con.time),plan.con.perc.surv, fill=time))+geom_boxplot()+
  theme_pubr()+facet_grid(. ~ glioma2, scales = "free_x", space = "free")+ylim(0,90)+theme(plot.tag = element_text(face = "bold"))+
  stat_summary(fun.data = give.n, geom = "text", fun.y = max, position = position_dodge2(width = 0.75), size = 4)+
  labs(fill="Control Arm\nSurvival", tag = "A")+ylab("Percent Survival")+xlab("Time (Months)")+theme(legend.position = "right")+
  ggtitle("Overall Survival")

ppfs<-perctime[perctime$Outcome=="PFS",]
ppfs<-ppfs[!(ppfs$glioma2=="New AA"|ppfs$glioma2=="New GBM - umMGMT"|ppfs$glioma2=="Recurrent HGG"|ppfs$glioma2=="New or Recur GBM"|ppfs$glioma2=="New LGG"|ppfs$glioma2=="New or Recur MB"),]
ppfs<-ppfs[ppfs$plan.con.time==6|ppfs$plan.con.time==12|ppfs$plan.con.time==60,]
ppfs<-ppfs[!is.na(ppfs$plan.con.time),]
ppfs<-ppfs[!(ppfs$glioma2=="New HGG" & ppfs$plan.con.time==6),]

ppfs$glioma2<-factor(ppfs$glioma2, levels = c("New GBM","New HGG","Recurrent GBM","New MB"))
u<-ggplot(ppfs, aes(factor(plan.con.time),plan.con.perc.surv, fill=time))+geom_boxplot()+
  theme_pubr()+facet_grid(. ~ glioma2, scales = "free_x", space = "free")+ylim(0,90)+theme(plot.tag = element_text(face = "bold"))+
  stat_summary(fun.data = give.n, geom = "text", fun.y = max, position = position_dodge2(width = 0.75), size = 4)+
  labs(fill="PFS", tag = "B")+ylab("Percent Survival")+xlab("Time (Months)")+ggtitle("Progression Free Survival")

(t+rremove("legend")+get_legend(t)+plot_layout(widths = c(4.125,1)))/u+rremove("legend")
#9x6in pdf portrait

##### FIGURE 6 ######

#sample size 
shade_data <- data.frame(x = c(min(d$ss.achieved, na.rm = TRUE), max(d$ss.achieved, na.rm = TRUE), 
                              max(d$ss.achieved, na.rm = TRUE), min(d$ss.achieved, na.rm = TRUE)),
                         y = c(2, 1, 0.6, 0) )

p<-ggplot(d) +
  geom_polygon(data = shade_data, aes(x = x, y = y), fill = "grey80", alpha = 0.5) + # Shaded area
  geom_point(aes(ss.achieved, final.hr, fill=plan.hr), shape=21, color="black", stroke=0.5, size=3) + theme_pubr() +
  scale_fill_gradientn(
    colors = jet(100), 
    values = rescale(c(min(d$plan.hr[!is.na(d$plan.hr)]), 0.5, 0.7, max(d$plan.hr[!is.na(d$plan.hr)]))),  # Maps 0.4–0.8 to full gradient, clamps others
    limits = c(min(d$plan.hr[!is.na(d$plan.hr)]), max(d$plan.hr[!is.na(d$plan.hr)])),  # Ensures color scale covers full range
    oob = squish,  # Assigns <0.4 to min color and >0.8 to max color
    name = "Targeted\nHazard\nRatio")+guides(fill=guide_colourbar(theme=theme(legend.key.height = unit(10, "lines"))))+
  theme(legend.position = "right",plot.tag = element_text(face = "bold")) + ylim(-0.5,2.75)+
  geom_hline(yintercept = 1, linetype = "longdash") +ylab("Final Hazard Ratio") +xlab("Sample Size Achieved")+
  ggtitle("Trial Result Based on Sample Size")

p[["data"]]<- p[["data"]] %>% arrange(
    ss.achieved < 400,  # Ensures ss.achieved < 400 comes first
    is.na(plan.hr),      # Places NA values of plan.hr at the bottom
    ifelse(ss.achieved < 400, desc(plan.hr), plan.hr)) %>% # Sort by decreasing order if <400, ascending otherwise
  arrange(!is.na(plan.hr))      # Ensures that NA values come last in both conditions
p
# 5x 4 in PDF

d$ssbin <- cut(d$ss.achieved, breaks=c(0,400,950), labels = c("≤400",">400"))

mean(d$final.hr[!is.na(d$final.hr) & d$ssbin=="≤400"])
mean(d$final.hr[!is.na(d$final.hr) & d$ssbin==">400"])

sd(d$final.hr[!is.na(d$final.hr) & d$ssbin=="≤400"])
sd(d$final.hr[!is.na(d$final.hr) & d$ssbin==">400"])


###### FIGURE 7 ########

d3<-d %>% distinct(pmid, .keep_all = TRUE)

length(d3[is.na(d3$ss) & !is.na(d3$ss.achieved),"pmid"]) #trials that didn't report pre-specified SS
length(d3[!is.na(d3$ss) & !is.na(d3$ss.achieved),"pmid"]) #trials that reported both pre-specified SS and final

d4<-d3[!is.na(d3$ss) & !is.na(d3$ss.achieved),]

#write.csv(d4,"d4.csv")

q<-ggplot(d4)+
#  geom_ribbon(aes(x = ss, ymin = -Inf, ymax = ss), data = subset(d, ss >= 275 & ss <= 505), fill = "gray80", alpha = 0.5) +
  geom_point(aes(ss, ss.achieved, fill=plan.hr), shape=21, color="black", stroke=0.5, size=2)+theme_pubr()+
  theme(legend.position = "right", plot.tag = element_text(face = "bold"))+labs(tag = "A")+
  scale_fill_gradientn(
    colors = jet(100), 
    values = rescale(c(min(d$plan.hr[!is.na(d$plan.hr)]), 0.5, 0.7, max(d$plan.hr[!is.na(d$plan.hr)]))),  # Maps 0.4–0.8 to full gradient, clamps others
    limits = c(min(d$plan.hr[!is.na(d$plan.hr)]), max(d$plan.hr[!is.na(d$plan.hr)])),  # Ensures color scale covers full range
    oob = squish,  # Assigns <0.4 to min color and >0.8 to max color
    name = "Targeted\nHazard\nRatio")+guides(fill=guide_colourbar(theme=theme(legend.key.height = unit(10, "lines"))))+
  geom_abline(slope=1, intercept=0, linetype="longdash")+ylab("Sample Size Achieved")+xlab("Sample Size Planned")+
  ggtitle("Deviation from Target Enrollment")

q[["data"]]<- q[["data"]] %>% arrange(
  ss.achieved < 400,  # Ensures ss.achieved < 400 comes first
  is.na(plan.hr),      # Places NA values of plan.hr at the bottom
  ifelse(ss.achieved < 400, desc(plan.hr), plan.hr)) %>% # Sort by decreasing order if <400, ascending otherwise
  arrange(!is.na(plan.hr))      # Ensures that NA values come last in both conditions

d4$reason<-factor(d4$reason, levels = c("Met target","Interim / ethical / early events", "No reason", "Post-randomization exclusion", "Resource withdrawal / slow accrual"))
q1<-ggplot(d4)+geom_point(aes(ss, ss.achieved, color=reason), size=2)+theme_pubr()+
  geom_abline(slope=1, intercept=0, linetype="longdash")+ylab("Sample Size Achieved")+xlab("Sample Size Planned")+
  scale_color_manual(
    values = c("Met target" = "black",
      "Interim / ethical / early events" = "grey40",
      "No reason" = "grey70",
      "Post-randomization exclusion" = "dodgerblue",
      "Resource withdrawal / slow accrual" = "red"),
    label=c("Met target" = "Met target",
            "Interim / ethical / early events" = "Interim analyses /\nethical concern /\nearly event target",
            "No reason" = "No reason",
            "Post-randomization exclusion" = "Post-randomization\nexclusion",
            "Resource withdrawal / slow accrual" = "Resource withdrawal /\nslow accrual"))+
  ggtitle("Reasons for Under Enrollment")+
  theme(plot.tag = element_text(face = "bold"),legend.position = "right", legend.key.height = unit(2, "lines"))+
  labs(tag = "B", color="Reason")

q1[["data"]]<- q1[["data"]] %>% arrange(reason)      # Ensures that NA values come last in both conditions

gbm<-subset(d,glioma2=="New GBM") %>% distinct(pmid, .keep_all = TRUE)

r<-ggplot(gbm, aes(ss.achieved,ss.achieved/accural))+geom_smooth(method='lm', formula= y~x, se = T)+geom_point()+
  geom_text(x = 380, y = 30, label = lm_eqn(gbm, gbm$ss.achieved/gbm$accural, gbm$ss.achieved), parse = TRUE)+theme_pubr()+
  xlab("Sample Size Achieved")+ylab("Est. Monthly Enrollment")+ggtitle("Enrollment Rate of New GBM Trials")+
  theme(plot.tag = element_text(face = "bold"))+labs(tag = "C")

s<-ggplot(gbm, aes(ss.achieved,followup))+geom_smooth(method='lm', formula= y~x, se = T)+geom_point()+
  geom_text(x = 425, y = 86, label = lm_eqn(gbm, gbm$followup,gbm$ss.achieved), parse = TRUE)+theme_pubr()+
  geom_hline(yintercept = 24, linetype="longdash")+xlab("Sample Size Achieved")+ylab("Followup (Months)")+ggtitle("Followup Time in New GBM Trials")+
  scale_y_continuous(breaks = c(24,50,75,100))+theme(plot.tag = element_text(face = "bold"))+labs(tag = "D")
si<-ggplot(gbm, aes(followup))+geom_boxplot()+coord_flip()+theme_void()

q+q1+plot_spacer()+r+s+si +plot_layout(widths = c(10,10,1))
#12x8 in pdf

ggplot(gbm, aes(ss.achieved/accural,followup))+geom_smooth(method='lm', formula= y~x, se = T)+geom_point()+
  geom_text(x = 15, y = 86, label = lm_eqn(gbm, gbm$followup,gbm$ss.achieved/gbm$accural), parse = TRUE)+theme_pubr()+
  geom_hline(yintercept = 24, linetype="longdash")+xlab("Est. Monthly Enrollment")+ylab("Followup (Months)")+ggtitle("Enrollment in New GBM Trials")+
  scale_y_continuous(breaks = c(24,50,75,100))+theme(plot.tag = element_text(face = "bold"))+labs(tag = "D")


### drop in power with time as survival increases
library(rpact)
a<-48 #accural
b<-24 #follow up
t<-0.25*((a+b)/12) #increase in median OS
m<-15 #median OS

ss<-getSampleSizeSurvival(getDesignGroupSequential(kMax = 1, alpha = 0.025, sided = 1, beta = .2), hazardRatio = 0.75, median2 = m, 
                          accrualTime = c(0,a), followUpTime = b, dropoutRate1 = .1, dropoutRate2 = .1, dropoutTime = a)

p<-getPowerSurvival(getDesignGroupSequential(kMax = 1, alpha = 0.025, sided = 1, beta = .2), hazardRatio = 0.75, median2 = m+t, 
                    accrualTime = c(0,a), followUpTime = b, dropoutRate1 = .1, dropoutRate2 = .1, dropoutTime = a,
                    maxNumberOfSubjects=ss[["maxNumberOfSubjects"]], maxNumberOfEvents=ss[["maxNumberOfEvents"]]*(m/(m+t)), 
                    directionUpper=F)

p[["overallReject"]]