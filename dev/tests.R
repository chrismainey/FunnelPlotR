library(FunnelPlotR)
library(COUNT)
library(ggplot2)

# Setup
data_type = "SR"
limit=95
label_outliers = TRUE
draw_unadjusted = TRUE
draw_adjusted = FALSE
sr_method = "SHMI"
trim_by = 0.1
title="Untitled Funnel Plot"
multiplier = 1
x_label = "Expected"
y_label = "Standardised Ratio"
xrange = "auto"
yrange = "auto"
#return_elements=c("plot", "data", "limits")
theme = funnel_clean()
plot_cols = c("#FF7F0EFF", "#1F77B4FF", "#9467BDFF","#2CA02CFF")
highlight = c("030001")

# lets use the \'medpar\' dataset from the \'COUNT\' package. Little reformatting needed
#library(COUNT)
data(medpar)
medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)

mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)

# Get predicted value for ratio
medpar$prds<- predict(mod, type="response")

# Draw plot, returning just the plot object
fp2<-funnel_plot(denominator=medpar$prds,numerator=medpar$los, multiplier = 100,
                 group = medpar$provnum, limit=99 
                 ,label = "both"
                 
                 , sr_method = "CQC",
                 draw_unadjusted = TRUE, draw_adjusted=FALSE, highlight = "030002", theme=funnel_grey(),
                 plot_cols = c("#FF7F0EFF", "#000000", "#1F77B4FF","#1F77B4FF", "#9467BDFF", "#9467BDFF", "#2CA02CFF", "#2CA02CFF"))
                 
#rm(fp2)
fp2
class(fp2)
plot(fp2)
limits(fp2)
print(fp2)
summary(fp2)
phi(fp2)
tau2(fp2)
outliers(fp2)
source_data(fp2)



fp<-funnel_plot(numerator=medpar$los, denominator=medpar$prds, group = medpar$provnum,
            title = 'Length of Stay Funnel plot for `medpar` data', draw_unadjusted = TRUE,
            draw_adjusted = TRUE, limit=99, label = "outlier", sr_method="SHMI")

fp[[1]]

View(fp[[3]])

fp[[1]]

fp<-readRDS("plot_reference.RDS")
library(gridExtra)

grid.arrange(fp$plot, rtn$plot, nrow=1)


install.packages("C:/Users/Christopher/Documents/R/FunnelPlotR_0.2.9999.tar.gz", repo=NULL)

# proportion 

# Now ratio of counts

b<-funnel_plot(numerator=medpar$died, denominator=1, group = medpar$provnum,
            data_type = "PR",#return_elements=c("plot"),
            title = 'Length of Stay Funnel plot for `medpar` data', 
            draw_adjusted = TRUE, label_outliers = TRUE, limit=95)
b

tau2(b)
phi(b)
limits(b)

ifelse(b[[2]]$rr > b[[2]]$OD99UCL, as.character(b[[2]]$group), "")

b[[2]] %>% 
  arrange(rr) %>% 
  dplyr::select(group, denominator, rr, OD99LCL, OD99UCL)

a<-funnel_plot(numerator=medpar$los, denominator=(medpar$prds*10), group = medpar$provnum,
            data_type = "RC",#return_elements=c("plot"),
            title = 'Length of Stay Funnel plot for `medpar` data', draw_unadjusted = FALSE,
            draw_adjusted = TRUE, label_outliers = TRUE, sr_method="SHMI")

a[1]

a[[2]] %>% 
  arrange(rr) %>% 
  dplyr::select(group, denominator, rr, OD95LCL, OD95UCL, OD99LCL, OD99UCL)

a[3]

draw_plot(mod_plot_agg, x_label, y_label, title, label_outliers,
                    multiplier=multiplier, draw_unadjusted=FALSE, draw_adjusted=TRUE,
                    Tau2=Tau2, Target=sum(numerator)/sum(denominator), xrange=xrange, 
                    yrange=yrange, data_type=data_type,
                    sr_method = sr_method, theme = theme)
