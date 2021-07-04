#!/bin/bash

#SBATCH --nodes=1 #Number of nodes

# set the number of CPU cores per node
#SBATCH --ntasks=70 #How many tasks on each node

# How much memory is needed (per node)
#SBATCH --mem=160GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=120:00:00

# set name of job
#SBATCH --job-name=final_mod_rf

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_final_mod_rf.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 11_tune_final_models_remod.R