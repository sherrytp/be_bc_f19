* A pdf copy of the paper is included here (`ag2012imperfect.pdf`). The paper and materials can be found [online](https://www.aeaweb.org/articles?id=10.1257/aer.102.7.3317). 
* `punnoise_data.csv` is the authors' data.
* `ag2012_readme.txt` is the authors' file for interpreting the data. Read it if you questions about the variables. 
* Please submit your .Rmd file by email (`degeest@bc.edu`) by midnight on **Sunday 24 November**. 


### NOTE From .txt File 

In the following we explain the variables in punnoise_data.txt	
	
Variable	Definition
treat	Name of treatment
noise	"0 for no noise treatment, 1 for noise treatment"
punishment	"0 if no punishment, 1 if one of the punishment treatments"
p_reg	1 if this is regular punishment treatment
p_strong	1 if this is strong punishment treatment
session	number of session
round	number of round
subject	ID of subject in session
group	ID of group in session
sub_nb_in_group	number of subject in group
group_id	unique ID for group = session*100 + group
	
contribution	"contribution of subject in this round, 0 or 20"
contr_dummy	dummy for contribution = contribution/20
public_record	"public record of subject's contribution this round, only defined if noise=1"
pr_dummy	"dummy for public record, equals public_record/20 if noise=1, equals contr_dummy if noise=0"
oth1_subject	subject ID of other group member 1
oth2_subject	subject ID of other group member 2
oth1_contribution	other group member 1's contribution
oth2_contribution	other group member 2's contribution
oth1_public_record	"other group member 1's public record, only defined if noise=1"
oth2_public_record	"other group member 2's public record, only defined if noise=1"
sum_group_contributions	sum of contributions in group
sum_group_contributions_pr	sum of contributions according to public records
income_pg	income of subject from public good game
income_pg_pr	"income of subject according to public records, only defined of noise=1"
received_punishment	"number of punishment points received (not yet multiplied), only defined if punishment=1"
income_reduction	"resulting income reduction, , only defined if punishment=1, equals 3*received_punishment if p_reg=1, equals 6*received_punishment if p_strong=1"
income_pg_after_reduction	"income from public good after reduction, capped at 0 for p_reg=1, only defined if punishment=1"
punish_oth1	"punishment points assigned to other group member 1, only defined if punishment=1"
punish_oth2	"punishment points assigned to other group member 2, only defined if punishment=1"
punishment_cost	"costs of punishemnt = punish_oth1 + punish_oth2, only defined if punishment=1"
income	"round income, equals income_pg_after_reduction - punishment_cost if punsihment=1, and income_pg otherwise"
	
noise__p_reg	equals noise*p_reg
noise__p_strong	equals noise*p_strong
	
pr_received_punishment	"received punishment of this subject in previous round, only defined for round>1 & punishment=1"
prrp__noise	"equals pr_received_punishment*noise, only defined for round>1 & punishment=1"
prrp__p_strong	"equals pr_received_punishment*p_strong, only defined for round>1 & punishment=1"
prrp__noise__p_strong	"equals pr_received_punishment*noise*p_strong, only defined for round>1 & punishment=1"
	
pr_contr_dummy	"contr_dummy for this subject in previous round, only defined for round>1"
prrp__prcd	"equals pr_received_punishment*pr_contr_dummy, only defined for round>1 & punishment=1"
prrp__prcd__noise	"equals pr_received_punishment*pr_contr_dummy*noise, only defined for round>1 & punishment=1"
prrp__prcd__p_strong	"equals pr_received_punishment*pr_contr_dummy*p_strong, only defined for round>1 & punishment=1"
prrp__prcd__noise__p_strong	"equals pr_received_punishment*pr_contr_dummy*noise*p_strong, only defined for round>1 & punishment=1"
	
	
pr_public_record	"public_record for this subject in previous round, only defined for round>1 & noise=1"
pr_no_contr_sig	"equals 1 if pr_contr_dummy=1 & pr_public_record=0, 0 otherwise, only defined for round>1 and noise=1"
prcd__pr_no_contr_sig	"equals pr_contr_dummy*pr_no_contr_sig, only defined for round>1 and noise=1"
prrp__lrcd__pr_no_contr_sig	"equals pr_received_punishment*pr_contr_dummy*pr_no_contr_sig, only defined for round>1 and noise=1 & punishment=1"
prrp__prcd__p_strong__prncs	"equals pr_received_punishment*pr_contr_dummy*p_strong*pr_no_contr_sig, only defined for round>1 and noise=1 & punishment=1"
	
pr_group_contr	"equals ((sum_group_contributions-contribution)/40) of last round, only defined for round>1"
prcd__pr_group_contr	"equals pr_group_contr if noise=0, and  ((sum_group_contributions_pr-contribution)/40) of last round if noise=1, only defined for round>1"
