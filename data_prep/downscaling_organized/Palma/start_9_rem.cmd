#!/bin/bash

# set the number of nodes
#SBATCH --nodes=2

# set the number of CPU cores per node
#SBATCH --ntasks-per-node 70

# How much memory is needed (per node)
#SBATCH --mem=100GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=100:00:00

# set name of job
#SBATCH --job-name=9_training_ffs_remodelling

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_9_training_ffs_remodelling.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 9_FFS_remodelling_rf.R