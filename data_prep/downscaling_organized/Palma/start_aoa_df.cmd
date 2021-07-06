#!/bin/bash


#SBATCH --nodes=1 #Number of nodes

# set the number of CPU cores per node
#SBATCH --cpus-per-task=40 #How many tasks on each node

# How much memory is needed (per node)
#SBATCH --mem=160GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=70:00:00

# set name of job
#SBATCH --job-name=aoa_df

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_aoa_df.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 14_AOA_df.R
