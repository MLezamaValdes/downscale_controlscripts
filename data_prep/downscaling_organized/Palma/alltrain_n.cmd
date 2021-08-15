#!/bin/bash


#SBATCH --nodes=1 #Number of nodes

# set the number of CPU cores per node
#SBATCH --ntasks=1 #How many tasks on each node

# How much memory is needed (per node)
#SBATCH --mem=160GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=24:00:00

# set name of job
#SBATCH --job-name=alltrain_n

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output alltrain_n.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla get_alltrain_n.R
