#!/bin/bash
#SBATCH --job-name=dc_uncert_test
#SBATCH --partition=short-40core-shared
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=01:00:00
#SBATCH --array=1-3%1
#SBATCH --output=logs/dc_uncert_test_%A_%a.out
#SBATCH --error=logs/dc_uncert_test_%A_%a.err

module purge
module load R/4.2.1

# Preflight: confirm Rscript and glibc compatibility
Rscript --version

# Small trial: 3 tasks only
cmds=(
  "Rscript r/uncertainty-somsc-script.R conv maiz 0"
  "Rscript r/uncertainty-somsc-script.R conv maiz 1"
  "Rscript r/uncertainty-somsc-script.R conv soyb 0"
)

idx=$((SLURM_ARRAY_TASK_ID-1))
eval "${cmds[$idx]}"
