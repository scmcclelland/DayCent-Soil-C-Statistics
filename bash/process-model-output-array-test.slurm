#!/bin/bash
#SBATCH --job-name=dc_proc_test
#SBATCH --partition=short-40core-shared
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64G
#SBATCH --time=00:30:00
#SBATCH --array=1-3%1
#SBATCH --output=slurm-%A_%a.out
#SBATCH --error=slurm-%A_%a.err

module purge
module load R/4.2.1

# Preflight: confirm Rscript and glibc compatibility
Rscript --version

# Small trial: 3 tasks only
cmds=(
  "Rscript r/process-output.R oct-23 2035 conv"
  "Rscript r/process-output.R oct-23 2035 res"
  "Rscript r/process-output.R oct-23 2035 ntill"
)

idx=$((SLURM_ARRAY_TASK_ID-1))
eval "${cmds[$idx]}"
