#!/bin/bash
#SBATCH --job-name=dc_proc
#SBATCH --partition=short-40core-shared
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=04:00:00
#SBATCH --array=1-19%4
#SBATCH --output=logs/dc_proc_%A_%a.out
#SBATCH --error=logs/dc_proc_%A_%a.err
#SBATCH --mail-user=shelby.mcclelland@stonybrook.edu
#SBATCH --mail-type=BEGIN,END,FAIL

module purge
module load R/4.2.1

# Preflight: confirm Rscript and glibc compatibility
Rscript --version

cmds=(
  "Rscript r/process-output.R oct-23 2035 conv"
  "Rscript r/process-output.R oct-23 2035 res"
  "Rscript r/process-output.R oct-23 2035 ntill"
  "Rscript r/process-output.R oct-23 2035 ccg"
  "Rscript r/process-output.R oct-23 2035 ccl"
  "Rscript r/process-output.R oct-23 2035 ccg-ntill"
  "Rscript r/process-output.R oct-23 2035 ccl-ntill"
  "Rscript r/process-output.R oct-23 2035 rewild"
  "Rscript r/process-output.R dec-23 2035 conv"
  "Rscript r/process-output.R dec-23 2035 res"
  "Rscript r/process-output.R dec-23 2035 ntill"
  "Rscript r/process-output.R dec-23 2035 ccg"
  "Rscript r/process-output.R dec-23 2035 ccl"
  "Rscript r/process-output.R dec-23 2035 ccg-ntill"
  "Rscript r/process-output.R dec-23 2035 ccl-ntill"
  "Rscript r/process-output.R dec-23 2035 rewild"
  "Rscript r/process-output.R feb-24 2035 ntill-res"
  "Rscript r/process-output.R feb-24 2035 ccg-res"
  "Rscript r/process-output.R feb-24 2035 ccl-res"
)

idx=$((SLURM_ARRAY_TASK_ID-1))
eval "${cmds[$idx]}"
