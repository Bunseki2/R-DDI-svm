#!/bin/bash
# This file is runR
#

module load R
sed 's/$SLURM_JOBID/'$SLURM_JOBID'/g' /data/takedatn/SYNERGISTIC/TF_upDn/comp_score_Syn10M.ngene.PC3.Target.updnbyDrug_ave_num.TF.08292017.biowulf.NUM.R > /data/takedatn/SYNERGISTIC/TF_upDn/comp_score_Syn10M.ngene.PC3
.Target.updnbyDrug_ave_num.TF.08292017.biowulf.NUM.22.R 
R --vanilla < /data/takedatn/SYNERGISTIC/TF_upDn/comp_score_Syn10M.ngene.PC3.Target.updnbyDrug_ave_num.TF.08292017.biowulf.NUM.22.R > /scratch/takedatn/comp_score_Syn10M.ngene.PC3.Target.updnbyDrug_ave_num.TF.08292017
.biowulf.NUM.22.Rout

tar -czvf   /lscratch/$SLURM_JOBID/$SLURM_JOBID.NUM.08292017.tar.gz  /lscratch/$SLURM_JOBID/final_scores.*.PC03.6H10M.ngene.Target.UpDndrug.TF.biowulf.08292017.txt
cp /lscratch/$SLURM_JOBID/$SLURM_JOBID.NUM.08292017.tar.gz   /data/takedatn/SYNERGISTIC/TF_upDn/FINAL_SCORES/.
