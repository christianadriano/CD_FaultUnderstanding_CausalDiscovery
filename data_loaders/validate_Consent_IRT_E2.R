"
Validation of the consent file E2 with IRT data

1- Check if same workerID has different demographics
2- Check if same workerID has same adjusted qualification score and profession

"


#Load consent data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes();

#1-check if there are workerID with different demographics
worker_id_list <- unique(df_consent$worker_id)

worker_id <- worker_id_list[1]

for (worker_id in worker_id_list) {
  age <- unique(df_consent[df_consent$worker_id==worker_id,]$age)
  if(length(age)>1)
    print(worker_id)
}

# Follows the problematic worker_ids
# [1] "23EE-1c8E1-7-2:16eA-3g-9A-7-5-3_3"
# [1] "519CE-1a-8G700:117Ec-7c4a-50-9:93Ce-7c-8i-69-4_3"
# [1] "1506IC2A8e2-20:615gC7g-8i-21-3:85aA-1G-5e8-11:89cG-4a4I-5-15_3"
# [1] "18ii-3A8a-2-2-3:17cC4a2e-8-2-6_3"
# [1] "75IG0G-7C6-30:286AC-3A-2g0-74:3aI0C8I-9-2-5_3"
# [1] "922iC0E-2E713:117cA2E0I8-80_3"
# [1] "45ea8E6e0-10:95cg-5I0I151_3"

#-------------------------------------------------------
