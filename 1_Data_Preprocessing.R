####Import Positive Raw Data
library("MetaboAnalystR")
library("OptiLCMS")
data_folder_Sample <- "C:/Students/Huy_PLX_VT26/CDF"
data_folder_QC <- "C:/Students/Huy_PLX_VT26/CDF/QC"
raw_data <- PerformROIExtraction(data_folder_QC, rt.idx = 1.0, rmConts = FALSE)
#####
param_initial <- SetPeakParam(platform = "UPLC-G2S")
param_initial <- SetPeakParam(polarity = "positive")
param_optimized <- PerformParamsOptimization(raw_data, param = param_initial, ncore = 10)
#####
rawData <- ImportRawMSData(NULL, data_folder_Sample, plotSettings = SetPlotParam(Plot=FALSE))
mSet <- PerformPeakProfiling(rawData,param_optimized, plotSettings = SetPlotParam(Plot = F), ncore = 10)
raw_path <- "C:/Students/Huy PLX VT26"
save(mSet, file = paste0(raw_path, "HUY1_Profiling_08_03_26__POS.Rda"))
annParams <- SetAnnotationParam(polarity = "positive", mz_abs_add = 0.005)
save(annParams, file = paste0(raw_path, "HUY_Annotpeaks_08_03_26_POS.Rda"))
annotPeaks <- PerformPeakAnnotation(mSet, annParams)
#####
maPeaks <- FormatPeakList(annotPeaks, annParams, filtIso =T, filtAdducts = FALSE, missPercent = 0.75)
write.csv(maPeaks@dataSet, file = "20260309_POS_PLX_2.csv",row.names = F)
save.image("20260309_POS_PLX_2.RData")
