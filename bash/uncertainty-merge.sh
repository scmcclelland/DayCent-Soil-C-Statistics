#!/bin/bash
# Call R script to merge SOC uncertainty by scenario
# 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-conv 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-res 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ntill 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccg 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccl 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ntill-res 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccg-res 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccl-res 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccg-ntill 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccl-ntill 10-yr
Rscript r/combine-uncertainty-output.R uncertainty-rewild 10-yr
# 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-conv 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-res 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ntill 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccg 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccl 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ntill-res 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccg-res 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccl-res 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccg-ntill 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-ccl-ntill 20-yr
Rscript r/combine-uncertainty-output.R uncertainty-rewild 20-yr