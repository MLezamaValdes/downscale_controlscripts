#!/bin/bash

# set the number of nodes
#SBATCH --nodes=1

# set the number of CPU cores per node
#SBATCH --ntasks-per-node 7

# How much memory is needed (per node)
#SBATCH --mem=90GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=80:00:00

# set name of job
#SBATCH --job-name=ia_hs

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_ia_hs.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 5_ia_hs_Palma.R