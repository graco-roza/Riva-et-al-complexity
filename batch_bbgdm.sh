#!/bin/bash -l
#SBATCH --job-name=bbgdm
#SBATCH --account=Project_2005062
#SBATCH --output=out_%a.txt
#SBATCH --error=err_%a.txt
#SBATCH --partition=small
#SBATCH --time=3-00:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=40
#SBATCH --mem=320G
# Load r-env-singularity
module load r-env-singularity

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
sed -i '/TMPDIR/d' ~/.Renviron
fi

# Specify a temp folder path
echo "TMPDIR=/scratch/project_2005062" >> ~/.Renviron

# Run the R script
srun singularity_wrapper exec Rscript --no-save Scripts/S3_Run_BBGDM.R