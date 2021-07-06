#!/bin/bash

# set the number of nodes
#SBATCH --nodes=3

# set the number of CPU cores per node
#SBATCH --ntasks=42
#SBATCH --ntasks-per-node 14

# How much memory is needed (per node)
#SBATCH --mem=160GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=168:00:00

# set name of job
#SBATCH --job-name=tune_final_slurmR

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_tune_final_slurmR.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 11_tune_final_models_remod_slurmR.R