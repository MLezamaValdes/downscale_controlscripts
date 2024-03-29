#!/bin/bash

# set the number of nodes
#SBATCH --nodes=1

# set the number of CPU cores per node
#SBATCH --ntasks-per-node 40

# How much memory is needed (per node)
#SBATCH --mem=160GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=100:00:00

# set name of job
#SBATCH --job-name=aoa_df_1n

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_aoa_df_1n.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 14_AOA_df_1n.R