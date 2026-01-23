#!/bin/bash
#SBATCH --job-name=dc_proc_test
#SBATCH --partition=short-40core-shared
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --time=00:30:00
#SBATCH --array=1-3%1
#SBATCH --output=logs/dc_proc_test_%A_%a.out
#SBATCH --error=logs/dc_proc_test_%A_%a.err

module load R

# Ensure log dir exists on the compute node
mkdir -p logs

# Small trial: 3 tasks only
cmds=(
  "Rscript r/process-output.R oct-23 2035 conv"
  "Rscript r/process-output.R oct-23 2035 res"
  "Rscript r/process-output.R oct-23 2035 ntill"
)

idx=$((SLURM_ARRAY_TASK_ID-1))
eval "${cmds[$idx]}"
