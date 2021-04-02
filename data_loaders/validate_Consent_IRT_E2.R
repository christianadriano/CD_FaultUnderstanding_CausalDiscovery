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


for (worker_id in worker_id_list) {
  adjusted_score <- unique(df_consent[df_consent$worker_id==worker_id,]$adjusted_score)
  if(length(adjusted_score)>1)
    print(worker_id)
}

#The following worker_ids have different adjusted scores, which might be ok, because 
#they might have take different tests.

# [1] "358ig-7C7C5-1-2:10EI0c-3A0-3-9_3"
# [1] "27Ei-8i0A-3-59:15cG-7i7C996_3"
# [1] "23EE-1c8E1-7-2:16eA-3g-9A-7-5-3_3"
# [1] "832cg-7G1i-462:73eI-8E-2g-985_3"
# [1] "270ie-2C-4c-550:97ai0I-8C871_3"
# [1] "519CE-1a-8G700:117Ec-7c4a-50-9:93Ce-7c-8i-69-4_3"
# [1] "67AI-8G5A71-3:163Aa0i0a963:161eg5c5I-649_3"
# [1] "94iC8I-7g2-6-9:176ac-2C-6e7-94_3"
# [1] "815aA-9g-4e44-8:182Ae3g-6i-30-5_3"
# [1] "59gA3i5E2-7-9:249IE-2a8E-92-1_3"
# [1] "844Cg1g5e1-17:255ei-8G8E85-8_3"
# [1] "642ci9c-9e-580:266ee6e-1I61-3_3"
# [1] "270eG-5c6A3-46:65eI-2i-2I-9-36_3"
# [1] "941eE-4G9i-56-4:283Aa7E0I474_3"
# [1] "21Ig2A-5e0-88:292CE-5a2c86-4:60ei9e-5I04-3_3"
# [1] "582GG-6a-7c31-1:310Ei-2a5i-4-17_3"
# [1] "1504ic8g0I130:312cI-5a9I-1-82:10eG-8i5a-618_3"
# [1] "645ca0i0i87-5:419ee1I0A201_3"
# [1] "1456eI-3g8a-22-2:488gG-7E2G-8-68_3"
# [1] "1221iC8A5A242:495CC9e6a691:11aE2c-4c-9-86_3"
# [1] "1568aA-4c6i-106:505iC4e-2c-439_3"
# [1] "555aC0a8e4-8-3:535CE8A-5e-95-2_3"
# [1] "96CE1c7G35-1:591ga-7E0E9-88_3"
# [1] "11IC-3a0E1-10:322iA-8I0A31-6:671Ca6c4e-5-9-9_3"
# [1] "1014aA5c4G-408:694aI4G-3E-8-3-6_3"
# [1] "505GE3G-6a90-4:757ia0c-2i-4-17_3"
# [1] "452Ig6c-9G-502:768Ga1I8G-27-2:69eC6i-8G00-4_3"
# [1] "892CA6a-8c802:759GA-8a-5a-285_3"
# [1] "8EI3i9I949:931ii-1c6G-5-95_3"
# [1] "1582ai0e2I065_3"
# [1] "18ii-3A8a-2-2-3:17cC4a2e-8-2-6_3"
# [1] "56II-1e7E-789:57CI-4g3G20-9_3"
# [1] "80ca6g0i-1-49:23iE-4i0a1-40_3"
# [1] "59eA3i6i030:86GA-3G-4C8-5-2_3"
# [1] "92ea8E5i07-8:77cc-5c0G32-4_3"
# [1] "75IG0G-7C6-30:286AC-3A-2g0-74:3aI0C8I-9-2-5_3"
# [1] "63GI-8C5a906:400EE-2a-6c-4-89_3"
# [1] "98cg8a0A69-1:439ee9e-2i-106_3"
# [1] "28Gc-2i-9C-18-2:442cA-6G6i-749_3"
# [1] "94ce-5I9a-4-3-6:477Gi5i-7I316_3"
# [1] "3AG2a6c-6-4-6:485Gg1i-6E-150:13GC7c-2c-843_3"
# [1] "106iG8G-9I-9-80:590CG-6G-7i-71-9_3"
# [1] "680ce-8i-5C06-6:70Ec7A4e-7-84_3"
# [1] "114eE1e0a9-5-8:820Ie-3c1C-85-5_3"
# [1] "103Ee-8a6i-3-7-8:873AG0a3i770_3"
# [1] "922iC0E-2E713:117cA2E0I8-80_3"
# [1] "101GC1i5g-7-61:1008II-5a4e60-9:110gg-3i2G-6-94_3"
# [1] "1198eA8G-7e7-7-3:120Gc-7G5G4-5-4_3"
# [1] "66AC-5a0g-47-9:1443IA-7C-6e967_3"
# [1] "1600Cc-8c9E841:67GE4A0g-870_3"
# [1] "83gE-9E9a-176:1609ca8I0E-894_3"
# [1] "45ea8E6e0-10:95cg-5I0I151_3"
# [1] "4IC-5i7I612:81ia8g6g-3-69:10IE-1i1e662_3"


