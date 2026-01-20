#!/bin/bash
# Call R script to run SOC uncertainty by scenario, crop, irrigation
# conv
Rscript r/uncertainty-somsc-script.R conv maiz 0 &
Rscript r/uncertainty-somsc-script.R conv maiz 1 &
Rscript r/uncertainty-somsc-script.R conv soyb 0 &
Rscript r/uncertainty-somsc-script.R conv soyb 1 &
Rscript r/uncertainty-somsc-script.R conv wwht 0 &
Rscript r/uncertainty-somsc-script.R conv wwht 1 &
Rscript r/uncertainty-somsc-script.R conv swht 0 &
Rscript r/uncertainty-somsc-script.R conv swht 1 &
# res
Rscript r/uncertainty-somsc-script.R res maiz 0 &
Rscript r/uncertainty-somsc-script.R res maiz 1 &
Rscript r/uncertainty-somsc-script.R res soyb 0 &
Rscript r/uncertainty-somsc-script.R res soyb 1 &
Rscript r/uncertainty-somsc-script.R res wwht 0 &
Rscript r/uncertainty-somsc-script.R res wwht 1 &
Rscript r/uncertainty-somsc-script.R res swht 0 &
Rscript r/uncertainty-somsc-script.R res swht 1 &
# ntill
Rscript r/uncertainty-somsc-script.R ntill maiz 0 &
Rscript r/uncertainty-somsc-script.R ntill maiz 1 &
Rscript r/uncertainty-somsc-script.R ntill soyb 0 &
Rscript r/uncertainty-somsc-script.R ntill soyb 1 &
Rscript r/uncertainty-somsc-script.R ntill wwht 0 &
Rscript r/uncertainty-somsc-script.R ntill wwht 1 &
Rscript r/uncertainty-somsc-script.R ntill swht 0 &
Rscript r/uncertainty-somsc-script.R ntill swht 1 &
# ccg
Rscript r/uncertainty-somsc-script.R ccg maiz 0 &
Rscript r/uncertainty-somsc-script.R ccg maiz 1 &
Rscript r/uncertainty-somsc-script.R ccg soyb 0 &
Rscript r/uncertainty-somsc-script.R ccg soyb 1 &
Rscript r/uncertainty-somsc-script.R ccg wwht 0 &
Rscript r/uncertainty-somsc-script.R ccg wwht 1 &
Rscript r/uncertainty-somsc-script.R ccg swht 0 &
Rscript r/uncertainty-somsc-script.R ccg swht 1 &
# ccl
Rscript r/uncertainty-somsc-script.R ccl maiz 0 &
Rscript r/uncertainty-somsc-script.R ccl maiz 1 &
Rscript r/uncertainty-somsc-script.R ccl soyb 0 &
Rscript r/uncertainty-somsc-script.R ccl soyb 1 &
Rscript r/uncertainty-somsc-script.R ccl wwht 0 &
Rscript r/uncertainty-somsc-script.R ccl wwht 1 &
Rscript r/uncertainty-somsc-script.R ccl swht 0 &
Rscript r/uncertainty-somsc-script.R ccl swht 1 &
# ntill-res
Rscript r/uncertainty-somsc-script.R ntill-res maiz 0 &
Rscript r/uncertainty-somsc-script.R ntill-res maiz 1 &
Rscript r/uncertainty-somsc-script.R ntill-res soyb 0 &
Rscript r/uncertainty-somsc-script.R ntill-res soyb 1 &
Rscript r/uncertainty-somsc-script.R ntill-res wwht 0 &
Rscript r/uncertainty-somsc-script.R ntill-res wwht 1 &
Rscript r/uncertainty-somsc-script.R ntill-res swht 0 &
Rscript r/uncertainty-somsc-script.R ntill-res swht 1 &
# ccg-res
Rscript r/uncertainty-somsc-script.R ccg-res maiz 0 &
Rscript r/uncertainty-somsc-script.R ccg-res maiz 1 &
Rscript r/uncertainty-somsc-script.R ccg-res soyb 0 &
Rscript r/uncertainty-somsc-script.R ccg-res soyb 1 &
Rscript r/uncertainty-somsc-script.R ccg-res wwht 0 &
Rscript r/uncertainty-somsc-script.R ccg-res wwht 1 &
Rscript r/uncertainty-somsc-script.R ccg-res swht 0 &
Rscript r/uncertainty-somsc-script.R ccg-res swht 1 &
# ccl-res
Rscript r/uncertainty-somsc-script.R ccl-res maiz 0 &
Rscript r/uncertainty-somsc-script.R ccl-res maiz 1 &
Rscript r/uncertainty-somsc-script.R ccl-res soyb 0 &
Rscript r/uncertainty-somsc-script.R ccl-res soyb 1 &
Rscript r/uncertainty-somsc-script.R ccl-res wwht 0 &
Rscript r/uncertainty-somsc-script.R ccl-res wwht 1 &
Rscript r/uncertainty-somsc-script.R ccl-res swht 0 &
Rscript r/uncertainty-somsc-script.R ccl-res swht 1 &
# ccg-ntill
Rscript r/uncertainty-somsc-script.R ccg-ntill maiz 0 &
Rscript r/uncertainty-somsc-script.R ccg-ntill maiz 1 &
Rscript r/uncertainty-somsc-script.R ccg-ntill soyb 0 &
Rscript r/uncertainty-somsc-script.R ccg-ntill soyb 1 &
Rscript r/uncertainty-somsc-script.R ccg-ntill wwht 0 &
Rscript r/uncertainty-somsc-script.R ccg-ntill wwht 1 &
Rscript r/uncertainty-somsc-script.R ccg-ntill swht 0 &
Rscript r/uncertainty-somsc-script.R ccg-ntill swht 1 &
# ccl-ntill
Rscript r/uncertainty-somsc-script.R ccl-ntill maiz 0 &
Rscript r/uncertainty-somsc-script.R ccl-ntill maiz 1 &
Rscript r/uncertainty-somsc-script.R ccl-ntill soyb 0 &
Rscript r/uncertainty-somsc-script.R ccl-ntill soyb 1 &
Rscript r/uncertainty-somsc-script.R ccl-ntill wwht 0 &
Rscript r/uncertainty-somsc-script.R ccl-ntill wwht 1 &
Rscript r/uncertainty-somsc-script.R ccl-ntill swht 0 &
Rscript r/uncertainty-somsc-script.R ccl-ntill swht 1 &
# rewild
Rscript r/uncertainty-somsc-script.R rewild maiz 0 &
Rscript r/uncertainty-somsc-script.R rewild maiz 1 &
Rscript r/uncertainty-somsc-script.R rewild soyb 0 &
Rscript r/uncertainty-somsc-script.R rewild soyb 1 &
Rscript r/uncertainty-somsc-script.R rewild wwht 0 &
Rscript r/uncertainty-somsc-script.R rewild wwht 1 &
Rscript r/uncertainty-somsc-script.R rewild swht 0 &
Rscript r/uncertainty-somsc-script.R rewild swht 1 &
# Wait for all background tasks to complete
wait
echo "All commands finished."
