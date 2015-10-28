# rTwChannel
R package for twitter stream analitics developed within the colalboration between DISIT Lab UniFI-LAMMa-IBIMET CNR collaboration.


To install package 

```R
if (!require(devtools)) { install.packages("devtools")}
devtools::install_github("valenitna/rTwChannel")
library(rTwChannel)
```

An example of operative work-chain 

```R
library(rTwChannel)

#################################################################################################
#  Load a social channel from DISIT platform http://www.disit.org/tv/ Twitter Vigilance Dashboard.
# Authentication with user and password is required. 

download_channel(channel="ConsumoSuolo",
                 outfile="ConsumoSuolo.csv",
                 format="csv",
                 start_date="2015-04-20",
                 end_date="2015-09-01",
                 user="",
                 pass="")

consumo_suolo_channel=read.csv("ConsumoSuolo.csv",header=T)

saveRDS(consumo_suolo_channel,"consumo_suolo_channel.rds")
 


download_channel(channel="codified%20hashtags%20allerta",
                 outfile="codified_20hashtags_allerta.csv",
                 format="csv",
                 start_date="2015-03-01",
                 end_date="2015-09-01",
                 user="",
                 pass="")


codified_hashtags_allerta=read.csv("codified_20hashtags_allerta.csv",header=T)

saveRDS(codified_hashtags_allerta,"codified_20hashtags_allerta.rds")

download_channel(channel="MeteoAlert",
                 outfile="MeteoAlert.csv",
                 format="csv",
                 start_date="2015-03-01",
                 end_date="2015-09-01",
                 user="",
                 pass="")


MeteoAlert=read.csv("MeteoAlert.csv",header=T)
saveRDS(MeteoAlert,"MeteoAlert.rds")


download_channel(channel="rossano",
                 outfile="rossano.csv",
                 format="csv",
                 start_date="2015-08-06",
                 end_date="2015-09-01",
                 user="",
                 pass="")


rossano=read.csv("rossano.csv",header=T)
saveRDS(rossano,"rossano.rds")

```

