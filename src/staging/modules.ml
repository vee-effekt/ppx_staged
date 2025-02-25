module G_Bq = Bq_generator
module G_SR = Staged_generator.MakeStaged(Sr_random)
module G_C = Staged_generator.MakeStaged(C_random)
module G_C_SR = Staged_generator.MakeStaged(C_sr_dropin_random)
