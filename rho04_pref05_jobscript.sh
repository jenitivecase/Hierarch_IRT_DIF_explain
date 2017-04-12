#!/bin/bash 
#MSUB -l nodes=1:ppn=3,mem=16gb,walltime=60:00:00:00
#MSUB -M jbrussow@gmail.com
#MSUB -m abe 
#MSUB -N IRT_DIF_rho04_pref05
#MSUB -j oe 

module purge
module use /panfs/pfs.local/work/crmda/tools/modules
module load Rstats/3.3

R --vanilla -f $HOME/IRT_DIF/cluster_rho04_pref05.R 