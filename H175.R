# EMD extraction, C02:survey classifier with EMD features of COP signal
rm(list=ls())
gc()
# install.packages("pracma", repos="http://R-Forge.R-project.org")
# install.packages("EMD", repos="http://R-Forge.R-project.org")
t1=Sys.time();print(t1)
rawdata=read.csv("BDS00001.csv",header=T);dim(rawdata);
tt=rawdata[,1];
Fx=rawdata[,2];
Fy=rawdata[,3];
Fz=rawdata[,4];
copx=rawdata[,8];
copy=rawdata[,9];
# EMD
library(EMD)
E1=emd(copx,tt,boundary="wave");
E1.no=E1$nimf;
E1.imf=E1$imf
E1.residue=E1$residue

E2=emd(copy,tt,boundary="wave");
E2.no=E2$nimf;
E2.imf=E2$imf
E2.residue=E2$residue

E3=emd(Fx,tt,boundary="wave");
E3.no=E3$nimf;
E3.imf=E3$imf
E3.residue=E3$residue

E4=emd(Fy,tt,boundary="wave");
E4.no=E4$nimf;
E4.imf=E4$imf
E4.residue=E4$residue

E5=emd(Fz,tt,boundary="wave");
E5.no=E5$nimf;
E5.imf=E5$imf
E5.residue=E5$residue

# mean features
Result_all=matrix(0,1,175);

m1E1=mean(abs(E1.imf[,1]));Result_all[1]=m1E1;
m2E1=mean(abs(E1.imf[,2]));Result_all[2]=m2E1;
m3E1=mean(abs(E1.imf[,3]));Result_all[3]=m3E1;
m4E1=mean(abs(E1.imf[,4]));Result_all[4]=m4E1;
m5E1=mean(abs(E1.imf[,5]));Result_all[5]=m5E1;
m6E1=mean(abs(E1.imf[,6]));Result_all[6]=m6E1;
m7E1=mean(abs(E1.imf[,7]));Result_all[7]=m7E1;

m1E2=mean(abs(E2.imf[,1]));Result_all[8]=m1E2;
m2E2=mean(abs(E2.imf[,2]));Result_all[9]=m2E2;
m3E2=mean(abs(E2.imf[,3]));Result_all[10]=m3E2;
m4E2=mean(abs(E2.imf[,4]));Result_all[11]=m4E2;
m5E2=mean(abs(E2.imf[,5]));Result_all[12]=m5E2;
m6E2=mean(abs(E2.imf[,6]));Result_all[14]=m6E2;
m7E2=mean(abs(E2.imf[,7]));Result_all[14]=m7E2;

m1E3=mean(abs(E3.imf[,1]));Result_all[15]=m1E3;
m2E3=mean(abs(E3.imf[,2]));Result_all[16]=m2E3;
m3E3=mean(abs(E3.imf[,3]));Result_all[17]=m3E3;
m4E3=mean(abs(E3.imf[,4]));Result_all[18]=m4E3;
m5E3=mean(abs(E3.imf[,5]));Result_all[19]=m5E3;
m6E3=mean(abs(E3.imf[,6]));Result_all[20]=m6E3;
m7E3=mean(abs(E3.imf[,7]));Result_all[21]=m7E3;

m1E4=mean(abs(E4.imf[,1]));Result_all[22]=m1E4;
m2E4=mean(abs(E4.imf[,2]));Result_all[23]=m2E4;
m3E4=mean(abs(E4.imf[,3]));Result_all[24]=m3E4;
m4E4=mean(abs(E4.imf[,4]));Result_all[25]=m4E4;
m5E4=mean(abs(E4.imf[,5]));Result_all[26]=m5E4;
m6E4=mean(abs(E4.imf[,6]));Result_all[27]=m6E4;
m7E4=mean(abs(E4.imf[,7]));Result_all[28]=m7E4;
Result_all[15]=m1E4;
m1E5=mean(abs(E5.imf[,1]));Result_all[29]=m1E5;
m2E5=mean(abs(E5.imf[,2]));Result_all[30]=m2E5;
m3E5=mean(abs(E5.imf[,3]));Result_all[31]=m3E5;
m4E5=mean(abs(E5.imf[,4]));Result_all[32]=m4E5;
m5E5=mean(abs(E5.imf[,5]));Result_all[33]=m5E5;
m6E5=mean(abs(E5.imf[,6]));Result_all[34]=m6E5;
m7E5=mean(abs(E5.imf[,7]));Result_all[35]=m7E5;

# STD features
print("STD");Sys.time()
SD1E1=sd(E1.imf[,1]);Result_all[36]=SD1E1;
SD2E1=sd(E1.imf[,2]);Result_all[37]=SD2E1;
SD3E1=sd(E1.imf[,3]);Result_all[38]=SD3E1;
SD4E1=sd(E1.imf[,4]);Result_all[39]=SD4E1;
SD5E1=sd(E1.imf[,5]);Result_all[40]=SD5E1;
SD6E1=sd(E1.imf[,6]);Result_all[41]=SD6E1;
SD7E1=sd(E1.imf[,7]);Result_all[42]=SD7E1;

SD1E2=sd(E2.imf[,1]);Result_all[43]=SD1E2;
SD2E2=sd(E2.imf[,2]);Result_all[44]=SD2E2;
SD3E2=sd(E2.imf[,3]);Result_all[45]=SD3E2;
SD4E2=sd(E2.imf[,4]);Result_all[46]=SD4E2;
SD5E2=sd(E2.imf[,5]);Result_all[47]=SD5E2;
SD6E2=sd(E2.imf[,6]);Result_all[48]=SD6E2;
SD7E2=sd(E2.imf[,7]);Result_all[49]=SD7E2;

SD1E3=sd(E3.imf[,1]);Result_all[50]=SD1E3;
SD2E3=sd(E3.imf[,2]);Result_all[51]=SD2E3;
SD3E3=sd(E3.imf[,3]);Result_all[52]=SD3E3;
SD4E3=sd(E3.imf[,4]);Result_all[53]=SD4E3;
SD5E3=sd(E3.imf[,5]);Result_all[54]=SD5E3;
SD6E3=sd(E3.imf[,6]);Result_all[55]=SD6E3;
SD7E3=sd(E3.imf[,7]);Result_all[56]=SD7E3;

SD1E4=sd(E4.imf[,1]);Result_all[57]=SD1E4;
SD2E4=sd(E4.imf[,2]);Result_all[58]=SD2E4;
SD3E4=sd(E4.imf[,3]);Result_all[59]=SD3E4;
SD4E4=sd(E4.imf[,4]);Result_all[60]=SD4E4;
SD5E4=sd(E4.imf[,5]);Result_all[61]=SD5E4;
SD6E4=sd(E4.imf[,6]);Result_all[62]=SD6E4;
SD7E4=sd(E4.imf[,7]);Result_all[63]=SD7E4;

SD1E5=sd(E5.imf[,1]);Result_all[64]=SD1E5;
SD2E5=sd(E5.imf[,2]);Result_all[65]=SD2E5;
SD3E5=sd(E5.imf[,3]);Result_all[66]=SD3E5;
SD4E5=sd(E5.imf[,4]);Result_all[67]=SD4E5;
SD5E5=sd(E5.imf[,5]);Result_all[68]=SD5E5;
SD6E5=sd(E5.imf[,6]);Result_all[69]=SD6E5;
SD7E5=sd(E5.imf[,7]);Result_all[70]=SD7E5;

# CV features
print("CV");Sys.time()
CV1E1=SD1E1/m1E1;Result_all[71]=CV1E1;
CV2E1=SD2E1/m2E1;Result_all[72]=CV2E1;
CV3E1=SD3E1/m3E1;Result_all[73]=CV3E1;
CV4E1=SD4E1/m4E1;Result_all[74]=CV4E1;
CV5E1=SD5E1/m5E1;Result_all[75]=CV5E1;
CV6E1=SD6E1/m6E1;Result_all[76]=CV6E1;
CV7E1=SD7E1/m7E1;Result_all[77]=CV7E1;

CV1E2=SD1E2/m1E2;Result_all[78]=CV1E2;
CV2E2=SD2E2/m2E2;Result_all[79]=CV2E2;
CV3E2=SD3E2/m3E2;Result_all[80]=CV3E2;
CV4E2=SD4E2/m4E2;Result_all[81]=CV4E2;
CV5E2=SD5E2/m5E2;Result_all[82]=CV5E2;
CV6E2=SD6E2/m6E2;Result_all[83]=CV6E2;
CV7E2=SD7E2/m7E2;Result_all[84]=CV7E2;

CV1E3=SD1E3/m1E3;Result_all[85]=CV1E3;
CV2E3=SD2E3/m2E3;Result_all[86]=CV2E3;
CV3E3=SD3E3/m3E3;Result_all[87]=CV3E3;
CV4E3=SD4E3/m4E3;Result_all[88]=CV4E3;
CV5E3=SD5E3/m5E3;Result_all[89]=CV5E3;
CV6E3=SD6E3/m6E3;Result_all[90]=CV6E3;
CV7E3=SD7E3/m7E3;Result_all[91]=CV7E3;

CV1E4=SD1E4/m1E4;Result_all[92]=CV1E4;
CV2E4=SD2E4/m2E4;Result_all[93]=CV2E4;
CV3E4=SD3E4/m3E4;Result_all[94]=CV3E4;
CV4E4=SD4E4/m4E4;Result_all[95]=CV4E4;
CV5E4=SD5E4/m5E4;Result_all[96]=CV5E4;
CV6E4=SD6E4/m6E4;Result_all[97]=CV6E4;
CV7E4=SD7E4/m7E4;Result_all[98]=CV7E4;

CV1E5=SD1E5/m1E5;Result_all[99]=CV1E5;
CV2E5=SD2E5/m2E5;Result_all[100]=CV2E5;
CV3E5=SD3E5/m3E5;Result_all[101]=CV3E5;
CV4E5=SD4E5/m4E5;Result_all[102]=CV4E5;
CV5E5=SD5E5/m5E5;Result_all[103]=CV5E5;
CV6E5=SD6E5/m6E5;Result_all[104]=CV6E5;
CV7E5=SD7E5/m7E5;Result_all[105]=CV7E5;



# http://127.0.0.1:17734/library/pracma/html/entropy.html
# Approximate Entropy 
 library(pracma)
print("Approximate Entropy E1");Sys.time()
AP1E1=approx_entropy(E1.imf[,1],edim=2,r=0.2*SD1E1,elag = 1);Result_all[106]=AP1E1;
AP2E1=approx_entropy(E1.imf[,2],edim=2,r=0.2*SD2E1,elag = 1);Result_all[107]=AP2E1;
AP3E1=approx_entropy(E1.imf[,3],edim=2,r=0.2*SD3E1,elag = 1);Result_all[108]=AP3E1;
AP4E1=approx_entropy(E1.imf[,4],edim=2,r=0.2*SD4E1,elag = 1);Result_all[109]=AP4E1;
AP5E1=approx_entropy(E1.imf[,5],edim=2,r=0.2*SD5E1,elag = 1);Result_all[110]=AP5E1;
AP6E1=approx_entropy(E1.imf[,6],edim=2,r=0.2*SD6E1,elag = 1);Result_all[111]=AP6E1;
AP7E1=approx_entropy(E1.imf[,7],edim=2,r=0.2*SD7E1,elag = 1);Result_all[112]=AP7E1;

print("Approximate Entropy E2");Sys.time()
AP1E2=approx_entropy(E2.imf[,1],edim=2,r=0.2*SD1E2,elag = 1);Result_all[113]=AP1E2;
AP2E2=approx_entropy(E2.imf[,2],edim=2,r=0.2*SD2E2,elag = 1);Result_all[114]=AP2E2;
AP3E2=approx_entropy(E2.imf[,3],edim=2,r=0.2*SD3E2,elag = 1);Result_all[115]=AP3E2;
AP4E2=approx_entropy(E2.imf[,4],edim=2,r=0.2*SD4E2,elag = 1);Result_all[116]=AP4E2;
AP5E2=approx_entropy(E2.imf[,5],edim=2,r=0.2*SD5E2,elag = 1);Result_all[117]=AP5E2;
AP6E2=approx_entropy(E2.imf[,6],edim=2,r=0.2*SD6E2,elag = 1);Result_all[118]=AP6E2;
AP7E2=approx_entropy(E2.imf[,7],edim=2,r=0.2*SD7E2,elag = 1);Result_all[119]=AP7E2;

print("Approximate Entropy E3");Sys.time()
AP1E3=approx_entropy(E3.imf[,1],edim=2,r=0.2*SD1E3,elag = 1);Result_all[120]=AP1E3;
AP2E3=approx_entropy(E3.imf[,2],edim=2,r=0.2*SD2E3,elag = 1);Result_all[121]=AP2E3;
AP3E3=approx_entropy(E3.imf[,3],edim=2,r=0.2*SD3E3,elag = 1);Result_all[122]=AP3E3;
AP4E3=approx_entropy(E3.imf[,4],edim=2,r=0.2*SD4E3,elag = 1);Result_all[123]=AP4E3;
AP5E3=approx_entropy(E3.imf[,5],edim=2,r=0.2*SD5E3,elag = 1);Result_all[124]=AP5E3;
AP6E3=approx_entropy(E3.imf[,6],edim=2,r=0.2*SD6E3,elag = 1);Result_all[125]=AP6E3;
AP7E3=approx_entropy(E3.imf[,7],edim=2,r=0.2*SD7E3,elag = 1);Result_all[126]=AP7E3;

print("Approximate Entropy E4");Sys.time()
AP1E4=approx_entropy(E4.imf[,1],edim=2,r=0.2*SD1E4,elag = 1);Result_all[127]=AP1E4;
AP2E4=approx_entropy(E4.imf[,2],edim=2,r=0.2*SD2E4,elag = 1);Result_all[128]=AP2E4;
AP3E4=approx_entropy(E4.imf[,3],edim=2,r=0.2*SD3E4,elag = 1);Result_all[129]=AP3E4;
AP4E4=approx_entropy(E4.imf[,4],edim=2,r=0.2*SD4E4,elag = 1);Result_all[130]=AP4E4;
AP5E4=approx_entropy(E4.imf[,5],edim=2,r=0.2*SD5E4,elag = 1);Result_all[131]=AP5E4;
AP6E4=approx_entropy(E4.imf[,6],edim=2,r=0.2*SD6E4,elag = 1);Result_all[132]=AP6E4;
AP7E4=approx_entropy(E4.imf[,7],edim=2,r=0.2*SD7E4,elag = 1);Result_all[133]=AP7E4;

print("Approximate Entropy E5");Sys.time()
AP1E5=approx_entropy(E5.imf[,1],edim=2,r=0.2*SD1E5,elag = 1);Result_all[134]=AP1E5;
AP2E5=approx_entropy(E5.imf[,2],edim=2,r=0.2*SD2E5,elag = 1);Result_all[135]=AP2E5;
AP3E5=approx_entropy(E5.imf[,3],edim=2,r=0.2*SD3E5,elag = 1);Result_all[136]=AP3E5;
AP4E5=approx_entropy(E5.imf[,4],edim=2,r=0.2*SD4E5,elag = 1);Result_all[137]=AP4E5;
AP5E5=approx_entropy(E5.imf[,5],edim=2,r=0.2*SD5E5,elag = 1);Result_all[138]=AP5E5;
AP6E5=approx_entropy(E5.imf[,6],edim=2,r=0.2*SD6E5,elag = 1);Result_all[139]=AP6E5;
AP7E5=approx_entropy(E5.imf[,7],edim=2,r=0.2*SD7E5,elag = 1);Result_all[140]=AP7E5;

#Sample entropy

print("Sample Entropy E1");Sys.time()
SA1E1=sample_entropy(E1.imf[,1],edim=2,r=0.2*SD1E1,tau = 1);Result_all[141]=SA1E1;
SA2E1=sample_entropy(E1.imf[,2],edim=2,r=0.2*SD2E1,tau = 1);Result_all[142]=SA2E1;
SA3E1=sample_entropy(E1.imf[,3],edim=2,r=0.2*SD3E1,tau = 1);Result_all[143]=SA3E1;
SA4E1=sample_entropy(E1.imf[,4],edim=2,r=0.2*SD4E1,tau = 1);Result_all[144]=SA4E1;
SA5E1=sample_entropy(E1.imf[,5],edim=2,r=0.2*SD5E1,tau = 1);Result_all[145]=SA5E1;
SA6E1=sample_entropy(E1.imf[,6],edim=2,r=0.2*SD6E1,tau = 1);Result_all[146]=SA6E1;
SA7E1=sample_entropy(E1.imf[,7],edim=2,r=0.2*SD7E1,tau = 1);Result_all[147]=SA7E1;

print("Sample Entropy E2");Sys.time()
SA1E2=sample_entropy(E2.imf[,1],edim=2,r=0.2*SD1E2,tau = 1);Result_all[148]=SA1E2;
SA2E2=sample_entropy(E2.imf[,2],edim=2,r=0.2*SD2E2,tau = 1);Result_all[149]=SA2E2;
SA3E2=sample_entropy(E2.imf[,3],edim=2,r=0.2*SD3E2,tau = 1);Result_all[150]=SA3E2;
SA4E2=sample_entropy(E2.imf[,4],edim=2,r=0.2*SD4E2,tau = 1);Result_all[151]=SA4E2;
SA5E2=sample_entropy(E2.imf[,5],edim=2,r=0.2*SD5E2,tau = 1);Result_all[152]=SA5E2;
SA6E2=sample_entropy(E2.imf[,6],edim=2,r=0.2*SD6E2,tau = 1);Result_all[153]=SA6E2;
SA7E2=sample_entropy(E2.imf[,7],edim=2,r=0.2*SD7E2,tau = 1);Result_all[154]=SA7E2;

print("Sample Entropy E3");Sys.time()
SA1E3=sample_entropy(E3.imf[,1],edim=2,r=0.2*SD1E3,tau = 1);Result_all[155]=SA1E3;
SA2E3=sample_entropy(E3.imf[,2],edim=2,r=0.2*SD2E3,tau = 1);Result_all[156]=SA2E3;
SA3E3=sample_entropy(E3.imf[,3],edim=2,r=0.2*SD3E3,tau = 1);Result_all[157]=SA3E3;
SA4E3=sample_entropy(E3.imf[,4],edim=2,r=0.2*SD4E3,tau = 1);Result_all[158]=SA4E3;
SA5E3=sample_entropy(E3.imf[,5],edim=2,r=0.2*SD5E3,tau = 1);Result_all[159]=SA5E3;
SA6E3=sample_entropy(E3.imf[,6],edim=2,r=0.2*SD6E3,tau = 1);Result_all[160]=SA6E3;
SA7E3=sample_entropy(E3.imf[,7],edim=2,r=0.2*SD7E3,tau = 1);Result_all[161]=SA7E3;

print("Sample Entropy E4");Sys.time()
SA1E4=sample_entropy(E4.imf[,1],edim=2,r=0.2*SD1E4,tau = 1);Result_all[162]=SA1E4;
SA2E4=sample_entropy(E4.imf[,2],edim=2,r=0.2*SD2E4,tau = 1);Result_all[163]=SA2E4;
SA3E4=sample_entropy(E4.imf[,3],edim=2,r=0.2*SD3E4,tau = 1);Result_all[164]=SA3E4;
SA4E4=sample_entropy(E4.imf[,4],edim=2,r=0.2*SD4E4,tau = 1);Result_all[165]=SA4E4;
SA5E4=sample_entropy(E4.imf[,5],edim=2,r=0.2*SD5E4,tau = 1);Result_all[166]=SA5E4;
SA6E4=sample_entropy(E4.imf[,6],edim=2,r=0.2*SD6E4,tau = 1);Result_all[167]=SA6E4;
SA7E4=sample_entropy(E4.imf[,7],edim=2,r=0.2*SD7E4,tau = 1);Result_all[168]=SA7E4;

print("Sample Entropy E5");Sys.time()
SA1E5=sample_entropy(E5.imf[,1],edim=2,r=0.2*SD1E5,tau = 1);Result_all[169]=SA1E5;
SA2E5=sample_entropy(E5.imf[,2],edim=2,r=0.2*SD2E5,tau = 1);Result_all[170]=SA2E5;
SA3E5=sample_entropy(E5.imf[,3],edim=2,r=0.2*SD3E5,tau = 1);Result_all[171]=SA3E5;
SA4E5=sample_entropy(E5.imf[,4],edim=2,r=0.2*SD4E5,tau = 1);Result_all[172]=SA4E5;
SA5E5=sample_entropy(E5.imf[,5],edim=2,r=0.2*SD5E5,tau = 1);Result_all[173]=SA5E5;
SA6E5=sample_entropy(E5.imf[,6],edim=2,r=0.2*SD6E5,tau = 1);Result_all[174]=SA6E5;
SA7E5=sample_entropy(E5.imf[,7],edim=2,r=0.2*SD7E5,tau = 1);Result_all[175]=SA7E5;
t2=Sys.time();print(t2)
print(Result_all);