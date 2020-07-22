*** Gui Russo
** Moral Disengagement Experiment
* December 20, 2017

* cd "" * Change here as needed
use MoralDisengagementExp.dta, clear * Opening dataset

****************************************
***** Dados básicos e demográficos *****
****************************************

* Modo
tab modo

* Escola
tab escola
egen escola_id=group(escola)

* Sexo
tab sexo
gen feminino=sexo-1
label value sexo 0 "Masculino" 1 "Feminino"
label var sexo sexo
sum(sexo)
return list
g weight=1-(r(mean)-1) if sexo==2
replace weight=r(mean)-1 if sexo==1

tab sexo weight

* Educação da mãe
tab ed2, mi
replace ed2=5 if ed2==.

* Idade
tab idade, mi
gen idade2=idade
replace idade2=999 if idade2==.

* Religião
tab q3c, mi
replace q3c=7 if q3c==.

* Frequência do consumo de notícias e internet
tab gi0
tab www1

g news=6-gi0
g internet=6-www1

* Participação em grupos
tab cp8
replace cp8=5-cp8
tab cp13
replace cp13=5-cp13

g community= 4-cp8
recode community (2 3=1)
corr community cp8

g political_party= 4-cp13
recode political_party (2 3=1)
corr political_party cp13

* Minutos da entrevista
tab minutos

* Importância da religião
tab q5b

drop if escola_id==.
svyset escola_id [w=weight]

* Condição Experimental
tab disengagement_exp

*******************************

* Variáveis dependentes
sum sentence_1 sentence_2 sentence_3 sentence_4 sentence_5 sentence_6
tab sentence_1
tab sentence_2
tab sentence_3
tab sentence_4
tab sentence_5
tab sentence_6

svy: mean sentence_1 sentence_2 sentence_3 sentence_4 sentence_5 sentence_6
svy: mean sentence_1 sentence_2 sentence_3 sentence_4 sentence_5 sentence_6 if disengagement_exp==3 

* E aqui um teste das médias entre itens (olhe o t à direita, se maior que 1.96
* implica em significância estatística, com um intervalo de confiança de 95%
ttest sentence_1==sentence_2
ttest sentence_1==sentence_3
ttest sentence_1==sentence_4
ttest sentence_1==sentence_5
ttest sentence_1==sentence_6

cor sentence_1 sentence_2 sentence_3 sentence_4 sentence_5 sentence_6

**************************************************
mlogit disengagement_exp feminino ib2.ed2 i.q3c q5b modo

*outreg2 feminino disengagement_exp using descritivas.doc, cross label
tab feminino disengagement_exp, col
tab ed2 disengagement_exp, col
tab q3c disengagement_exp, col
tab modo disengagement_exp, col
tab gi0 disengagement_exp, col
tab www1 disengagement_exp, col

***** Sentence 1 *****
svy: ologit sentence_1 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions.doc, replace ctitle(Frase 1) label
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 2 *****
svy: ologit sentence_2 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions.doc, append ctitle(Frase 2) label
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 3 *****
svy: ologit sentence_3 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions.doc, append ctitle(Frase 3) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 4 *****
svy: ologit sentence_4 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions.doc, append ctitle(Frase 4) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 5 *****
svy: ologit sentence_5 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions.doc, append ctitle(Frase 5) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 6 *****
svy: ologit sentence_6 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions.doc, append ctitle(Frase 6) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

*****************************************************************
*************************
* Efeitos Heterogêneos? *
*************************

***** Confiança nas forças militares ******
tab b12
tab b18

g confia_exercito_pm=b12 + b18
g confia_exercito_pm_cat=.
replace confia_exercito_pm_cat=0 if confia_exercito_pm<9
replace confia_exercito_pm_cat=1 if confia_exercito_pm>8
tab confia_exercito_pm_cat disengagement_exp, col

svy: ologit sentence_1 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.confia_exercito_pm_cat
svy: ologit sentence_2 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.confia_exercito_pm_cat
svy: ologit sentence_3 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.confia_exercito_pm_cat
svy: ologit sentence_4 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.confia_exercito_pm_cat
svy: ologit sentence_5 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.confia_exercito_pm_cat
svy: ologit sentence_6 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.confia_exercito_pm_cat

g disengagement_exp_confia=disengagement_exp*confia_exercito_pm_cat
label define disengagement_exp_confia 0 "Não confia" 1 "Políticos pra Confia=1" 2 "Mídia pra Confia=1" 3 "Síntese pra Confia=1", modify
label value disengagement_exp_confia disengagement_exp_confia

*
svy: ologit sentence_1 ib2.disengagement_exp ib2.disengagement_exp_confia feminino ib2.ed2 i.q3c modo
outreg2 using heterogeneous.doc, ctitle(Afirmação 1) label 
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_confia=(0)) atmeans post
svy: ologit sentence_1 ib2.disengagement_exp ib2.disengagement_exp_confia feminino ib2.ed2 i.q3c modo
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_confia=(1 2 3)) atmeans post
*
svy: ologit sentence_2 ib2.disengagement_exp ib2.disengagement_exp_confia feminino ib2.ed2 i.q3c modo
outreg2 using heterogeneous.doc, append ctitle(Afirmação 2) label 
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_confia=(0)) atmeans post
svy: ologit sentence_2 ib2.disengagement_exp ib2.disengagement_exp_confia feminino ib2.ed2 i.q3c modo
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_confia=(1 2 3)) atmeans post
*
svy: ologit sentence_4 ib2.disengagement_exp ib2.disengagement_exp_confia feminino ib2.ed2 i.q3c modo
outreg2 using heterogeneous.doc, append ctitle(Afirmação 4) label 
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_confia=(0)) atmeans post
svy: ologit sentence_4 ib2.disengagement_exp ib2.disengagement_exp_confia feminino ib2.ed2 i.q3c modo
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_confia=(1 2 3)) atmeans post

***** Religiosidade ******
g religiosity=.
replace religiosity=0 if q5b>1
replace religiosity=1 if q5b==1

svy: ologit sentence_1 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.religiosity
svy: ologit sentence_2 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.religiosity
svy: ologit sentence_3 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.religiosity
svy: ologit sentence_4 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.religiosity
svy: ologit sentence_5 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.religiosity
svy: ologit sentence_6 ib2.disengagement_exp feminino ib2.ed2 i.q3c modo i.religiosity

g disengagement_exp_religiosity=disengagement_exp*religiosity
label define disengagement_exp_religiosity 0 "Não Religiosos" 1 "Políticos pra Religiosity=1" 2 "Mídia pra Religiosity=1" 3 "Síntese pra Religiosity=1", modify
label value disengagement_exp_religiosity disengagement_exp_religiosity

*
svy: ologit sentence_1 ib2.disengagement_exp ib2.disengagement_exp_religiosity feminino ib2.ed2 i.q3c modo
outreg2 using heterogeneous.doc, append ctitle(Afirmação 1) label 
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_religiosity=(0)) atmeans post
svy: ologit sentence_1 ib2.disengagement_exp ib2.disengagement_exp_religiosity feminino ib2.ed2 i.q3c modo
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_religiosity=(1 2 3)) atmeans post
*
svy: ologit sentence_2 ib2.disengagement_exp ib2.disengagement_exp_religiosity feminino ib2.ed2 i.q3c modo
outreg2 using heterogeneous.doc, append ctitle(Afirmação 2) label 
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_religiosity=(0)) atmeans post
svy: ologit sentence_2 ib2.disengagement_exp ib2.disengagement_exp_religiosity feminino ib2.ed2 i.q3c modo
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_religiosity=(1 2 3)) atmeans post
*
svy: ologit sentence_6 ib2.disengagement_exp ib2.disengagement_exp_religiosity feminino ib2.ed2 i.q3c modo
outreg2 using heterogeneous.doc, append ctitle(Afirmação 6) label 
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_religiosity=(0)) atmeans post
svy: ologit sentence_6 ib2.disengagement_exp ib2.disengagement_exp_religiosity feminino ib2.ed2 i.q3c modo
margins, predict(outcome(1)) at(disengagement_exp=(1 2 3) disengagement_exp_religiosity=(1 2 3)) atmeans post

*****************************************************************
**************************
**** Outras análises *****
**************************
**
*** Juntando totalmente e parcialmente
g sentence_1_3cat=sentence_1
recode sentence_1_3cat (2=1) (3=2) (4 5=3)

g sentence_2_3cat=sentence_2
recode sentence_2_3cat (2=1) (3=2) (4 5=3)

g sentence_3_3cat=sentence_3
recode sentence_3_3cat (2=1) (3=2) (4 5=3)

g sentence_4_3cat=sentence_4
recode sentence_4_3cat (2=1) (3=2) (4 5=3)

g sentence_5_3cat=sentence_5
recode sentence_5_3cat (2=1) (3=2) (4 5=3)

g sentence_6_3cat=sentence_6
recode sentence_6_3cat (2=1) (3=2) (4 5=3)

***** Sentence 1 *****
svy: ologit sentence_1_3cat ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions_3cat.doc, replace ctitle(Frase 1) label
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 2 *****
svy: ologit sentence_2_3cat ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions_3cat.doc, append ctitle(Frase 2) label
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 3 *****
svy: ologit sentence_3_3cat ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions_3cat.doc, append ctitle(Frase 3) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 4 *****
svy: ologit sentence_4_3cat ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions_3cat.doc, append ctitle(Frase 4) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 5 *****
svy: ologit sentence_5_3cat ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions_3cat.doc, append ctitle(Frase 5) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

***** Sentence 6 *****
svy: ologit sentence_6_3cat ib2.disengagement_exp feminino ib2.ed2 i.q3c modo
outreg2 using regressions_3cat.doc, append ctitle(Frase 6) label 
margins, predict(outcome(1)) at(disengagement_exp=(1(1)3)) atmeans post

**************************************************************
**************************************************************
**************************************************************


* Análise fatorial dos itens
factor sentence_1 sentence_2 sentence_3 sentence_4 sentence_5 sentence_6
rotate

factor sentence_1 sentence_2 sentence_3 sentence_4 sentence_5 sentence_6
predict factor1

factor sentence_3 sentence_5 sentence_6
predict factor_pol

*******************************

mlogit disengagement_exp feminino idade modo ed2 i.q3c news internet community i.escola_id

svy: tab modo disengagement_exp, col
svy: tab idade disengagement_exp, col
svy: tab ed2 disengagement_exp, col
svy: mean minutos, over(disengagement_exp)

