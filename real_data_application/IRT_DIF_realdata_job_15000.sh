#MSUB -N IRT_DIF_realdata
#MSUB -l nodes=1:ppn=3,walltime=60:00:00:00
#MSUB -l pmem=12gb
#MSUB -M jbrussow@ku.edu
#MSUB -m abe 
#MSUB -j oe 
#MSUB -q crmda
#MSUB -o IRT_DIF_realdata.log


cd $PBS_O_WORKDIR

R --vanilla -f real_data_estimation_cluster_15000.R