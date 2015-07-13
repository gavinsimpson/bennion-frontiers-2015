## read the deep dataset -- 56! lakes in TS, rest are cores
deep <- read.csv("data/deep_lakes_ss_cores_with_dates.csv")[, -1] ##[, -c(1,3)]
deepTP <- read.csv("data/deep_lakes_TP.csv")

## split off the cores from the TS data
cores <- deep[57:nrow(deep), ]
deep <- deep[1:56, ]
deepTP <- deepTP[1:56, ]

## set rownames on deep and clean up CodeName col
rownames(deep) <- deep[, 1]
deep <- deep[, -1]

## try to split the cores by site code
cores <- transform(cores,
                   sites = gsub("(^[A-Za-z]+)([0-9_\\.]+)", "\\1",
                   cores$CodeName))
## add depths
cores <- transform(cores,
                   depths = as.numeric(gsub("(^[A-Za-z_]+)([0-9_\\.]+)",
                   "\\2", cores$CodeName)))
## get rid of CodeName; add as rownames
rownames(cores) <- cores$CodeName
cores <- cores[, -1]

## read the shallow dataset -- 152! lakes in TS, rest are cores
shallow <- read.csv("data/shallow_lakes_ss_cores_with_dates.csv")[, -1] ##[, -c(1,3)]
shallowTP <- read.csv("data/shallow_lakes_logTP.csv")
## split off the cores from the TS data
shcores <- shallow[153:nrow(shallow), ]
shallow <- shallow[1:152, ]
shallowTP <- shallowTP[1:152, ]
## set rownames on shallow and clean up CodeName col
rownames(shallow) <- shallow[, 1]
shallow <- shallow[, -1]
## try to split the cores by site code
shcores <- transform(shcores, sites = gsub("(^[A-Za-z]+)([0-9_\\.]+)", "\\1",
                              CodeName))
## add depths
shcores <- transform(shcores,
                     depths = as.numeric(gsub("(^[A-Za-z_]+)([0-9_\\.]+)",
                     "\\2", CodeName)))
## get rid of CodeName; add as rownames
rownames(shcores) <- shcores$CodeName
shcores <- shcores[, -1]
