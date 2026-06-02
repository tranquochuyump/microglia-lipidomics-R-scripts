# LC–MS Lipidomics Preprocessing Workflow for MetaboAnalyst

This repository contains a two-step R workflow for preprocessing **positive-mode LC–MS raw data** (MassLynx/NetCDF format) and generating a **MetaboAnalyst-ready peak table**.

The pipeline is designed for untargeted lipidomics datasets with **biological samples, pooled QC samples, and optional blank samples**.

---

## Workflow Overview

The workflow consists of **two scripts**:

### **Script 1 – Raw data import and peak extraction**
This script uses **OptiLCMS** and **MetaboAnalystR** to:

- import positive-mode raw LC–MS data (`.cdf`)
- optimize peak-picking parameters using QC files
- perform peak profiling
- annotate peaks
- export a preprocessed peak table

**Output:**  
`POS_PreprocessedData_Sample.csv`

---

### **Script 2 – QC-based filtering, LOESS correction, missing value imputation, and MetaboAnalyst export**
This script takes the output of Script 1 together with POS_InjectionOrder.xlsx file and performs:

- metadata parsing from the two-header-row peak table
- injection-order mapping
- blank filtering
- prevalence filtering
- QC-based LOESS drift correction
- QC RSD filtering
- D-ratio filtering
- half-minimum imputation
- export of a final file ready for **MetaboAnalyst Online**

**Final output:**  
`POS_DataProcessed_Sample.csv`

This is the file intended for direct upload to **MetaboAnalyst Online**.

