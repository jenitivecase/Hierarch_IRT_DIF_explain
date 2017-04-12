#MSUB -N IRT_DIF_rho04_pref05
#MSUB -l nodes=1:ppn=3,walltime=60:00:00:00
#MSUB -l pmem=16gb
#MSUB -M jbrussow@ku.edu
#MSUB -m abe 
#MSUB -j oe 
#MSUB -q crmda
#MSUB -o rho04_pref05_out.log

cd $PBS_O_WORKDIR

module purge
module use /panfs/pfs.local/work/crmda/tools/modules
module load Rstats/3.3

R -f cluster_rho04_pref_05.R