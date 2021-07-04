#!/bin/bash

#SBATCH --cpus-per-task=1 # Number of cores per MPI rank 

# set the number of CPU cores per node
#SBATCH --nodes=4 #Number of nodes
#SBATCH --ntasks=40 # Number of MPI ranks
#SBATCH --ntasks-per-node=20 #How many tasks on each node
#SBATCH --cpus-per-task=1 # Number of cores per MPI rank 

# How much memory is needed (per node)
#SBATCH --mem=40GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=72:00:00

# set name of job
#SBATCH --job-name=aoa

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_aoa.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla 14_AOA.R

echo "Running example Rmpi script. Using $SLURM_JOB_NUM_NODES nodes with $SLURM_NTASKS tasks, each with $SLURM_CPUS_PER_TASK cores."
date