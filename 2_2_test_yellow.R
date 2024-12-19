
library(lavaan)
library(readxl)

#================================
data <-read_excel('/Users/jjy/Desktop/final_frame_continues1.xlsx')
data1 <- as.data.frame(data)

#================================
library('fastDummies')

data2 <- dummy_cols(data1,remove_selected_columns=TRUE)
print(dim(data2))
#colnames(data2)

#===============================colnames rename

blue_prw_index <- grep(x=c(colnames(data2)),pattern = 'blue_prw')
blue_pr_index <- grep(x=c(colnames(data2)),pattern = 'blue_pr')
blue_pse_index <- grep(x=c(colnames(data2)),pattern = 'blue_pse')
blue_pa_index <- grep(x=c(colnames(data2)),pattern = 'blue_pa')
blue_travel_index <- grep(x=c(colnames(data2)),pattern = 'blue_travel')
blue_ebike_index <- grep(x=c(colnames(data2)),pattern = 'blue_ebike')
blue_car_index <- grep(x=c(colnames(data2)),pattern = 'blue_car')
blue_taxi_index <- grep(x=c(colnames(data2)),pattern = 'blue_taxi')
blue_bus_index <- grep(x=c(colnames(data2)),pattern = 'blue_bus')
blue_subway_index <- grep(x=c(colnames(data2)),pattern = 'blue_subway')
blue_dTime_index <- grep(x=c(colnames(data2)),pattern = 'blue_dTime')
blue_Stravel_index <- grep(x=c(colnames(data2)),pattern = 'blue_Stravel')
blue_Smode_index <- grep(x=c(colnames(data2)),pattern = 'blue_Smode')
blue_SdTime_index <- grep(x=c(colnames(data2)),pattern = 'blue_SdTime')


C <- c(blue_pr_index,blue_pse_index,blue_pa_index,blue_travel_index,blue_ebike_index,blue_car_index,blue_taxi_index,blue_bus_index,blue_subway_index,blue_dTime_index,blue_Stravel_index,blue_Smode_index,blue_SdTime_index)

for(i in blue_prw_index){C=C[-which(C==i)]}



#============================rename colnames
data2

data3 <- subset(data2, select = -c(C)) 


colnames_data3 <- c(colnames(data3))

colnames_data3_rename <- gsub("yellow", "blue", colnames_data3)
colnames(data3) <- colnames_data3_rename

colnames(data3)

#============================modelling test
data3



m6a <- '
 #=============================================================================================================measurement model
blue_prw =~ blue_prw1 + blue_prw2 + blue_prw3 + blue_prw4 + blue_prw5
 blue_risk =~ blue_pr1 + blue_pr2 + blue_pr3 + blue_pr4 + blue_pr5
blue_self =~ blue_pse1 + blue_pse2 + blue_pse3 + blue_pse4 + blue_pse5
blue_pa =~ blue_pa1 + blue_pa2 + blue_pa3 + blue_pa4 

#=============================================================================================================regressions1
blue_pa ~ a1*blue_prw + a2*blue_risk + a3*blue_self + a4*gender_男 + a5*commute_distance + a6*past_travelDecision + a7*past_travelDtime + a8*past_travelDisruption 
+ a9*bus_access + a10*metro_access + a11*taxi_wait + a12*commuting_time + a13*weather_freq + a14*age + a15*income + a16*car_owner + a17*year_residence +
a18*current_mode_地铁 + a19*current_mode_地面公交 + a20*current_mode_电动自行车+
#a21*past_travelMode_地铁+a22*past_travelMode_地面公交+a23*past_travelMode_电动自行车+
a24*waether_source_交通导航软件+a25*waether_source_其他途径+a26*education_本科以上+a27*job_私企外企+a28*job_其他

blue_risk ~ b1*blue_prw+ b2*gender_男 + b3*commute_distance + b4*past_travelDecision + b5*past_travelDtime + b6*past_travelDisruption 
+ b7*bus_access + b8*metro_access + b9*taxi_wait + b10*commuting_time + b11*weather_freq + b12*age + b13*income + b14*car_owner + b15*year_residence +
b16*current_mode_地铁 + b17*current_mode_地面公交 + b18*current_mode_电动自行车+
#b19*past_travelMode_地铁+b20*past_travelMode_地面公交+b21*past_travelMode_电动自行车+
b22*waether_source_交通导航软件+b23*waether_source_其他途径+b24*education_本科以上+b25*job_私企外企+b26*job_其他

blue_self ~ add1*blue_prw + c1*blue_risk + c2*gender_男 + c3*commute_distance + c4*past_travelDecision + c5*past_travelDtime + c6*past_travelDisruption 
+ c7*bus_access + c8*metro_access + c9*taxi_wait + c10*commuting_time + c11*weather_freq + c12*age + c13*income + c14*car_owner + c15*year_residence +
c16*current_mode_地铁 + c17*current_mode_地面公交 + c18*current_mode_电动自行车+
#c19*past_travelMode_地铁+c20*past_travelMode_地面公交+c21*past_travelMode_电动自行车+
c22*waether_source_交通导航软件+c23*waether_source_其他途径+c24*education_本科以上+c25*job_私企外企+c26*job_其他

blue_prw ~ d1*gender_男 + d2*commute_distance + d3*past_travelDecision + d4*past_travelDtime + d5*past_travelDisruption 
+ d6*bus_access + d7*metro_access + d8*taxi_wait + d9*commuting_time + d10*weather_freq + d11*age + d12*income + d13*car_owner + d14*year_residence +
d15*current_mode_地铁 + d16*current_mode_地面公交 + d17*current_mode_电动自行车+
#d18*past_travelMode_地铁+d19*past_travelMode_地面公交+d20*past_travelMode_电动自行车+
d21*waether_source_交通导航软件+d22*waether_source_其他途径+d23*education_本科以上+d24*job_私企外企+d25*job_其他

#=============================================================================================================covariances

#blue_risk ~~ blue_self
#blue_prw ~~ blue_self

blue_pr2 ~~    blue_bus
blue_prw5 ~~  blue_ebike
blue_pr3 ~~  blue_Smode
blue_prw1 ~~   blue_prw2
blue_pa1 ~~   blue_taxi
blue_prw5 ~~ blue_subway
blue_prw2 ~~  blue_ebike
blue_prw2 ~~    blue_pa4
blue_pr1 ~~  blue_dTime
blue_pr1 ~~    blue_pa2
blue_pr2 ~~    blue_pr5
blue_prw5 ~~    blue_pr2
blue_prw5 ~~    blue_pa2 
blue_pse2 ~~    blue_pa2  
blue_prw3 ~~   blue_prw5  
blue_pr1 ~~    blue_pa3  
blue_prw3 ~~    blue_pr2 
blue_pr2 ~~   blue_pse2 
blue_pse3 ~~  blue_dTime 
blue_pse2 ~~    blue_pa1 
blue_prw3 ~~   blue_pse2 
blue_pse1 ~~    blue_bus 
blue_pse1 ~~ blue_subway 
blue_pr2 ~~   blue_pse1 
blue_pa3 ~~ blue_pa4
blue_pr3 ~~     blue_pr4
blue_pa1 ~~     blue_pa2
blue_pr1 ~~     blue_pr3
blue_pr1 ~~     blue_pr5
blue_prw2 ~~     blue_pr1
blue_pr2 ~~     blue_pa2
blue_pse1 ~~    blue_pse2
blue_pse1 ~~     blue_pa2
blue_pr5 ~~   blue_dTime
blue_pa2 ~~     blue_pa4  
blue_pr1 ~~   blue_ebike  
blue_pr1 ~~     blue_pa4 
blue_pr5 ~~  blue_travel  
blue_prw3 ~~    blue_pse1  
blue_pr3 ~~    blue_pr4



#==================================================================================regressions2-Direct Indirect Effects (以被指向变量所在的直接回归式子为基础套娃)
#blue_prw
blue_prw_pa_direct := a1
blue_prw_pa_indirect := a2*b1 + a3*c1*b1 + a3*add1
blue_prw_pr_direct := b1
blue_prw_sef_indirect := b1*c1

#blue_pr
blue_pr_pa_direct := a2
blue_pr_pa_indirect := a3*c1

blue_pr_sef_direct := c1


#blue_sef
blue_sef_pa_direct := a3

#blue_gender
blue_gender_pa_direct := a4
blue_gender_pa_indirect := a1*d1 + a2*b2 + a2*b1*d1 + a3*c2 + a3*c1*b2 + a3*c1*b1*d1 + a3*add1*d1
blue_gender_pr_direct := b2
blue_gender_pr_indirect := b1*d1
blue_gender_sef_direct := c2
blue_gender_sef_indirect := c1*b2 + c1*b1*d1 + add1*d1
blue_gender_prw_direct := d1

#blue_commute_distance
blue_commute_distance_pa_direct := a5
blue_commute_distance_pa_indirect := a1*d2 + a2*b3 + a2*b1*d2 + a3*c3 + a3*c1*b3 + a3*c1*b1*d2 + a3*add1*d2
blue_commute_distance_pr_direct := b3
blue_commute_distance_pr_indirect := b1*d2
blue_commute_distance_sef_direct := c3
blue_commute_distance_sef_indirect := c1*b3 + c1*b1*d2 + add1*d2
blue_commute_distance_prw_direct := d2

#past_travelDecision
past_travelDecision_pa_direct := a6
past_travelDecision_pa_indirect := a1*d3 + a2*b4 + a2*b1*d3 + a3*c4 + a3*c1*b4 + a3*c1*b1*d3 + a3*add1*d3
past_travelDecision_pr_direct := b4
past_travelDecision_pr_indirect := b1*d3
past_travelDecision_sef_direct := c4
past_travelDecision_sef_indirect := c1*b4 + c1*b1*d3+ add1*d3
past_travelDecision_prw_direct := d3

#past_travelDtime
past_travelDtime_pa_direct := a7
past_travelDtime_pa_indirect := a1*d4 + a2*b5 + a2*b1*d4 + a3*c5 + a3*c1*b5 + a3*c1*b1*d4 + a3*add1*d4
past_travelDtime_pr_direct := b5
past_travelDtime_pr_indirect := b1*d4
past_travelDtime_sef_direct := c5
past_travelDtime_sef_indirect := c1*b5 + c1*b1*d4+ add1*d4
past_travelDtime_prw_direct := d4

#past_travelDisruption
past_travelDisruption_pa_direct := a8
past_travelDisruption_pa_indirect := a1*d5 + a2*b6 + a2*b1*d5 + a3*c6 + a3*c1*b6 + a3*c1*b1*d5 + a3*add1*d5
past_travelDisruption_pr_direct := b6
past_travelDisruption_pr_indirect := b1*d5
past_travelDisruption_sef_direct := c6
past_travelDisruption_sef_indirect := c1*b6 + c1*b1*d5 + add1*d5
past_travelDisruption_prw_direct := d5

#bus_access
bus_access_pa_direct := a9
bus_access_pa_indirect := a1*d6 + a2*b7 + a2*b1*d6 + a3*c7 + a3*c1*b7 + a3*c1*b1*d6 + a3*add1*d6
bus_access_pr_direct := b7
bus_access_pr_indirect := b1*d6
bus_access_sef_direct := c7
bus_access_sef_indirect := c1*b7 + c1*b1*d6 + add1*d6
bus_access_prw_direct := d6


#metro_access
metro_access_pa_direct := a10
metro_access_pa_indirect := a1*d7 + a2*b8 + a2*b1*d7 + a3*c8 + a3*c1*b8 + a3*c1*b1*d7 + a3*add1*d7
metro_access_pr_direct := b8
metro_access_pr_indirect := b1*d7
metro_access_sef_direct := c8
metro_access_sef_indirect := c1*b8 + c1*b1*d7 + add1*d7
metro_access_prw_direct := d7

#taxi_wait
taxi_wait_pa_direct := a11
taxi_wait_pa_indirect := a1*d8 + a2*b9 + a2*b1*d8 + a3*c9 + a3*c1*b9 + a3*c1*b1*d8 ++ a3*add1*d8
taxi_wait_pr_direct := b9
taxi_wait_pr_indirect := b1*d8
taxi_wait_sef_direct := c9
taxi_wait_sef_indirect := c1*b9 + c1*b1*d8 + add1*d8
taxi_wait_prw_direct := d8

#commuting_time
commuting_time_pa_direct := a12
commuting_time_pa_indirect := a1*d9 + a2*b10 + a2*b1*d9 + a3*c10 + a3*c1*b10 + a3*c1*b1*d9 + a3*add1*d9
commuting_time_pr_direct := b10
commuting_time_pr_indirect := b1*d9
commuting_time_sef_direct := c10
commuting_time_sef_indirect := c1*b10 + c1*b1*d9 + add1*d9
commuting_time_prw_direct := d9

#weather_freq
weather_freq_pa_direct := a13
weather_freq_pa_indirect := a1*d10 + a2*b11 + a2*b1*d10 + a3*c11 + a3*c1*b11 + a3*c1*b1*d10 + a3*add1*d10
weather_freq_pr_direct := b11
weather_freq_pr_indirect := b1*d10
weather_freq_sef_direct := c11
weather_freq_sef_indirect := c1*b11 + c1*b1*d10 + add1*d10
weather_freq_prw_direct := d10

#age
age_pa_direct := a14
age_pa_indirect := a1*d11 + a2*b12 + a2*b1*d11 + a3*c12 + a3*c1*b12 + a3*c1*b1*d11 + a3*add1*d11
age_pr_direct := b12
age_pr_indirect := b1*d11
age_sef_direct := c12
age_sef_indirect := c1*b12 + c1*b1*d11 + add1*d11
age_prw_direct := d11

#income
income_pa_direct := a15
income_pa_indirect := a1*d12 + a2*b13 + a2*b1*d12 + a3*c13 + a3*c1*b13 + a3*c1*b1*d12 + a3*add1*d12
income_pr_direct := b13
income_pr_indirect := b1*d12
income_sef_direct := c13
income_sef_indirect := c1*b13 + c1*b1*d12 + add1*d12
income_prw_direct := d12

#car_owner
car_owner_pa_direct := a16
car_owner_pa_indirect := a1*d13 + a2*b14 + a2*b1*d13 + a3*c14 + a3*c1*b14 + a3*c1*b1*d13 + a3*add1*d13
car_owner_pr_direct := b14
car_owner_pr_indirect := b1*d13
car_owner_sef_direct := c14
car_owner_sef_indirect := c1*b14 + c1*b1*d13 + add1*d13
car_owner_prw_direct := d13

#year_residence
year_residence_pa_direct := a17
year_residence_pa_indirect := a1*d14 + a2*b15 + a2*b1*d14 + a3*c15 + a3*c1*b15 + a3*c1*b1*d14 + a3*add1*d14
year_residence_pr_direct := b15
year_residence_pr_indirect := b1*d14
year_residence_sef_direct := c15
year_residence_sef_indirect := c1*b15 + c1*b1*d14 + add1*d14
year_residence_prw_direct := d14

#current_mode_地铁
current_mode_地铁_pa_direct := a18
current_mode_地铁_pa_indirect := a1*d15 + a2*b16 + a2*b1*d15 + a3*c16 + a3*c1*b16 + a3*c1*b1*d15 + a3*add1*d15
current_mode_地铁_pr_direct := b16
current_mode_地铁_pr_indirect := b1*d15
current_mode_地铁_sef_direct := c16
current_mode_地铁_sef_indirect := c1*b16 + c1*b1*d15 + add1*d15
current_mode_地铁_prw_direct := d15

#current_mode_地面公交
current_mode_地面公交_pa_direct := a19
current_mode_地面公交_pa_indirect := a1*d16 + a2*b17 + a2*b1*d16 + a3*c17 + a3*c1*b17 + a3*c1*b1*d16 + a3*add1*d16
current_mode_地面公交_pr_direct := b17
current_mode_地面公交_pr_indirect := b1*d16
current_mode_地面公交_sef_direct := c17
current_mode_地面公交_sef_indirect := c1*b17 + c1*b1*d16 + add1*d16
current_mode_地面公交_prw_direct := d16

#current_mode_电动自行车
current_mode_电动自行车_pa_direct := a20
current_mode_电动自行车_pa_indirect := a1*d17 + a2*b18 + a2*b1*d17 + a3*c18 + a3*c1*b18 + a3*c1*b1*d17 + a3*add1*d17
current_mode_电动自行车_pr_direct := b18
current_mode_电动自行车_pr_indirect := b1*d17
current_mode_电动自行车_sef_direct := c18
current_mode_电动自行车_sef_indirect := c1*b18 + c1*b1*d17 + add1*d17
current_mode_电动自行车_prw_direct := d17


#waether_source_交通导航软件
waether_source_交通导航软件_pa_direct := a24
waether_source_交通导航软件_pa_indirect := a1*d21 + a2*b22 + a2*b1*d21 + a3*c22 + a3*c1*b22 + a3*c1*b1*d21 + a3*add1*d21
waether_source_交通导航软件_pr_direct := b22
waether_source_交通导航软件_pr_indirect := b1*d21
waether_source_交通导航软件_sef_direct := c22
waether_source_交通导航软件_sef_indirect := c1*b22 + c1*b1*d21 + add1*d21
waether_source_交通导航软件_prw_direct := d21

#waether_source_其他途径
waether_source_其他途径_pa_direct := a25
waether_source_其他途径_pa_indirect := a1*d22 + a2*b23 + a2*b1*d22 + a3*c23 + a3*c1*b23 + a3*c1*b1*d22 + a3*add1*d22
waether_source_其他途径_pr_direct := b23
waether_source_其他途径_pr_indirect := b1*d22
waether_source_其他途径_sef_direct := c23
waether_source_其他途径_sef_indirect := c1*b23 + c1*b1*d22 + add1*d22
waether_source_其他途径_prw_direct := d22

#education_本科以上
education_本科以上_pa_direct := a26
education_本科以上_pa_indirect := a1*d23 + a2*b24 + a2*b1*d23 + a3*c24 + a3*c1*b24 + a3*c1*b1*d23 + a3*add1*d23
education_本科以上_pr_direct := b24
education_本科以上_pr_indirect := b1*d23
education_本科以上_sef_direct := c24
education_本科以上_sef_indirect := c1*b24 + c1*b1*d23 + add1*d23
education_本科以上_prw_direct := d23



#job_私企外企
job_私企外企_pa_direct := a27
job_私企外企_pa_indirect := a1*d24 + a2*b25 + a2*b1*d24 + a3*c25 + a3*c1*b25 + a3*c1*b1*d24 +a3*add1*d24 
job_私企外企_pr_direct := b25
job_私企外企_pr_indirect := b1*d24
job_私企外企_sef_direct := c25
job_私企外企_sef_indirect := c1*b25 + c1*b1*d24 + add1*d24
job_私企外企_prw_direct := d24


#job_其他
job_其他_pa_direct := a28
job_其他_pa_indirect := a1*d25 + a2*b26 + a2*b1*d25 + a3*c26 + a3*c1*b26 + a3*c1*b1*d25 +a3*add1*d25 
job_其他_pr_direct := b26
job_其他_pr_indirect := b1*d25
job_其他_sef_direct := c26
job_其他_sef_indirect := c1*b26 + c1*b1*d25 + add1*d25
job_其他_prw_direct := d25



# =============================================================================================================regressions3-travel preferences 

blue_travel ~ z1*blue_pa + z2*blue_prw + z3*blue_risk + z4*blue_self + e1*gender_男 + e2*commute_distance + e3*past_travelDecision + e4*past_travelDtime + 
e5*past_travelDisruption + e6*bus_access + e7*metro_access + e8*taxi_wait + e9*commuting_time + e10*weather_freq + e11*age + e12*income + e13*car_owner + 
e14*year_residence + e15*current_mode_地铁 + e16*current_mode_地面公交 + e17*current_mode_电动自行车 + 
#e18*past_travelMode_地铁 + e19*past_travelMode_地面公交 + e20*past_travelMode_电动自行车 + 
e21*waether_source_交通导航软件 + e22*waether_source_其他途径 + e23*education_本科以上+e24*job_私企外企+e25*job_其他

blue_ebike ~ x1*blue_pa + x2*blue_prw + x3*blue_risk + x4*blue_self + f1*gender_男 + f2*commute_distance + f3*past_travelDecision + f4*past_travelDtime + 
f5*past_travelDisruption + f6*bus_access + f7*metro_access + f8*taxi_wait + f9*commuting_time + f10*weather_freq + f11*age + f12*income + f13*car_owner + 
f14*year_residence + f15*current_mode_地铁 + f16*current_mode_地面公交 + f17*current_mode_电动自行车 + 
#f18*past_travelMode_地铁 + f19*past_travelMode_地面公交 + f20*past_travelMode_电动自行车 + 
f21*waether_source_交通导航软件 + f22*waether_source_其他途径 + f23*education_本科以上+f24*job_私企外企+f25*job_其他

blue_car ~ y1*blue_pa + y2*blue_prw + y3*blue_risk + y4*blue_self + g1*gender_男 + g2*commute_distance + g3*past_travelDecision + g4*past_travelDtime + 
g5*past_travelDisruption + g6*bus_access + g7*metro_access + g8*taxi_wait + g9*commuting_time + g10*weather_freq + g11*age + g12*income + g13*car_owner + 
g14*year_residence + g15*current_mode_地铁 + g16*current_mode_地面公交 + g17*current_mode_电动自行车 + 
#g18*past_travelMode_地铁 + g19*past_travelMode_地面公交 + g20*past_travelMode_电动自行车 +
g21*waether_source_交通导航软件 + g22*waether_source_其他途径 + g23*education_本科以上+g24*job_私企外企+g25*job_其他

blue_taxi ~ q1*blue_pa + q2*blue_prw + q3*blue_risk + q4*blue_self + h1*gender_男 + h2*commute_distance + h3*past_travelDecision + h4*past_travelDtime + 
h5*past_travelDisruption + h6*bus_access + h7*metro_access + h8*taxi_wait + h9*commuting_time + h10*weather_freq + h11*age + h12*income + h13*car_owner + 
h14*year_residence + h15*current_mode_地铁 + h16*current_mode_地面公交 + h17*current_mode_电动自行车 + 
#h18*past_travelMode_地铁 + h19*past_travelMode_地面公交 + h20*past_travelMode_电动自行车 + 
h21*waether_source_交通导航软件 + h22*waether_source_其他途径 + h23*education_本科以上+h24*job_私企外企+h25*job_其他

blue_bus ~ w1*blue_pa + w2*blue_prw + w3*blue_risk + w4*blue_self + i1*gender_男 + i2*commute_distance + i3*past_travelDecision + i4*past_travelDtime + 
i5*past_travelDisruption + i6*bus_access + i7*metro_access + i8*taxi_wait + i9*commuting_time + i10*weather_freq + i11*age + i12*income + i13*car_owner + 
i14*year_residence + i15*current_mode_地铁 + i16*current_mode_地面公交 + i17*current_mode_电动自行车 + 
#i18*past_travelMode_地铁 + i19*past_travelMode_地面公交 + i20*past_travelMode_电动自行车 + 
i21*waether_source_交通导航软件 + i22*waether_source_其他途径 + i23*education_本科以上+i24*job_私企外企+i25*job_其他

blue_subway ~ r1*blue_pa + r2*blue_prw + r3*blue_risk + r4*blue_self + j1*gender_男 + j2*commute_distance + j3*past_travelDecision + j4*past_travelDtime + 
j5*past_travelDisruption + j6*bus_access + j7*metro_access + j8*taxi_wait + j9*commuting_time + j10*weather_freq + j11*age + j12*income + j13*car_owner + 
j14*year_residence + j15*current_mode_地铁 + j16*current_mode_地面公交 + j17*current_mode_电动自行车 + 
#j18*past_travelMode_地铁 + j19*past_travelMode_地面公交 + j20*past_travelMode_电动自行车 + 
j21*waether_source_交通导航软件 + j22*waether_source_其他途径 + j23*education_本科以上+j24*job_私企外企+j25*job_其他

blue_dTime ~ u1*blue_pa + u2*blue_prw + u3*blue_risk + u4*blue_self + k1*gender_男 + k2*commute_distance + k3*past_travelDecision + k4*past_travelDtime + 
k5*past_travelDisruption + k6*bus_access + k7*metro_access + k8*taxi_wait + k9*commuting_time + k10*weather_freq + k11*age + k12*income + k13*car_owner + 
k14*year_residence + k15*current_mode_地铁 + k16*current_mode_地面公交 + k17*current_mode_电动自行车 + 
#k18*past_travelMode_地铁 + k19*past_travelMode_地面公交 + k20*past_travelMode_电动自行车 + 
k21*waether_source_交通导航软件 + k22*waether_source_其他途径 + k23*education_本科以上+k24*job_私企外企+k25*job_其他

blue_Stravel ~ p1*blue_pa + p2*blue_prw + p3*blue_risk + p4*blue_self + m1*gender_男 + m2*commute_distance + m3*past_travelDecision + m4*past_travelDtime + 
m5*past_travelDisruption + m6*bus_access + m7*metro_access + m8*taxi_wait + m9*commuting_time + m10*weather_freq + m11*age + m12*income + m13*car_owner + 
m14*year_residence + m15*current_mode_地铁 + m16*current_mode_地面公交 + m17*current_mode_电动自行车 + 
#m18*past_travelMode_地铁 + m19*past_travelMode_地面公交 + m20*past_travelMode_电动自行车 + 
m21*waether_source_交通导航软件 + m22*waether_source_其他途径 + m23*education_本科以上+m24*job_私企外企+m25*job_其他

blue_Smode ~ l1*blue_pa + l2*blue_prw + l3*blue_risk + l4*blue_self + n1*gender_男 + n2*commute_distance + n3*past_travelDecision + n4*past_travelDtime + 
n5*past_travelDisruption + n6*bus_access + n7*metro_access + n8*taxi_wait + n9*commuting_time + n10*weather_freq + n11*age + n12*income + n13*car_owner + 
n14*year_residence + n15*current_mode_地铁 + n16*current_mode_地面公交 + n17*current_mode_电动自行车 + 
#n18*past_travelMode_地铁 + n19*past_travelMode_地面公交 + n20*past_travelMode_电动自行车 + 
n21*waether_source_交通导航软件 + n22*waether_source_其他途径 + n23*education_本科以上+n24*job_私企外企+n25*job_其他

blue_SdTime ~ v1*blue_pa + v2*blue_prw + v3*blue_risk + v4*blue_self + t1*gender_男 + t2*commute_distance + t3*past_travelDecision + t4*past_travelDtime + 
t5*past_travelDisruption + t6*bus_access + t7*metro_access + t8*taxi_wait + t9*commuting_time + t10*weather_freq + t11*age + t12*income + t13*car_owner + 
t14*year_residence + t15*current_mode_地铁 + t16*current_mode_地面公交 + t17*current_mode_电动自行车 + 
#t18*past_travelMode_地铁 + t19*past_travelMode_地面公交 + t20*past_travelMode_电动自行车 + 
t21*waether_source_交通导航软件 + t22*waether_source_其他途径 + t23*education_本科以上+t24*job_私企外企+t25*job_其他



#===========================================================regressions4-Direct Indirect Effects (以被指向变量所在的直接回归式子为基础套娃)

#blue_pa
blue_pa_blue_travel_direct := z1
blue_pa_blue_ebike_direct := x1
blue_pa_blue_car_direct := y1
blue_pa_blue_taxi_direct := q1
blue_pa_blue_bus_direct := w1
blue_pa_blue_subway_direct := r1
blue_pa_blue_dTime_direct := u1
blue_pa_blue_Stravel_direct := p1
blue_pa_blue_Smode_direct := l1
blue_pa_blue_SdTime_direct := v1


#blue_prw
blue_prw_blue_travel_direct := z2
blue_prw_blue_ebike_direct := x2
blue_prw_blue_car_direct := y2
blue_prw_blue_taxi_direct := q2
blue_prw_blue_bus_direct := w2
blue_prw_blue_subway_direct := r2
blue_prw_blue_dTime_direct := u2
blue_prw_blue_Stravel_direct := p2
blue_prw_blue_Smode_direct := l2
blue_prw_blue_SdTime_direct := v2

blue_prw_blue_travel_indirect := z1*a1 + z1*a2*b1 + z1*a3*c1*b1 + z3*b1 + z4*b1*c1 + z1*a3*add1 +z4*add1
blue_prw_blue_ebike_indirect := x1*a1 + x1*a2*b1 + x1*a3*c1*b1 + x3*b1 + x4*b1*c1 + x1*a3*add1 +x4*add1
blue_prw_blue_car_indirect := y1*a1 + y1*a2*b1 + y1*a3*c1*b1 + y3*b1 + y4*b1*c1 + y1*a3*add1 +y4*add1
blue_prw_blue_taxi_indirect := q1*a1 + q1*a2*b1 + q1*a3*c1*b1 + q3*b1 + q4*b1*c1 + q1*a3*add1 +q4*add1
blue_prw_blue_bus_indirect := w1*a1 + w1*a2*b1 + w1*a3*c1*b1 + w3*b1 + w4*b1*c1 + w1*a3*add1 +w4*add1
blue_prw_blue_subway_indirect := r1*a1 + r1*a2*b1 + r1*a3*c1*b1 + r3*b1 + r4*b1*c1 + r1*a3*add1 +r4*add1
blue_prw_blue_dTime_indirect := u1*a1 + u1*a2*b1 + u1*a3*c1*b1 + u3*b1 + u4*b1*c1 + u1*a3*add1 +u4*add1
blue_prw_blue_Stravel_indirect := p1*a1 + p1*a2*b1 + p1*a3*c1*b1 + p3*b1 + p4*b1*c1 + p1*a3*add1 +p4*add1
blue_prw_blue_Smode_indirect := l1*a1 + l1*a2*b1 + l1*a3*c1*b1 + l3*b1 + l4*b1*c1 + l1*a3*add1 +l4*add1
blue_prw_blue_SdTime_indirect := v1*a1 + v1*a2*b1 + v1*a3*c1*b1 + v3*b1 + v4*b1*c1 + v1*a3*add1 +v4*add1

#blue_pr
blue_pr_blue_travel_direct := z3
blue_pr_blue_ebike_direct := x3
blue_pr_blue_car_direct := y3
blue_pr_blue_taxi_direct := q3
blue_pr_blue_bus_direct := w3
blue_pr_blue_subway_direct := r3
blue_pr_blue_dTime_direct := u3
blue_pr_blue_Stravel_direct := p3
blue_pr_blue_Smode_direct := l3
blue_pr_blue_SdTime_direct := v3

blue_pr_blue_travel_indirect := z1*a2 + z1*a3*c1 + z4*c1 
blue_pr_blue_ebike_indirect := x1*a2 + x1*a3*c1 + x4*c1
blue_pr_blue_car_indirect := y1*a2 + y1*a3*c1 + y4*c1
blue_pr_blue_taxi_indirect := q1*a2 + q1*a3*c1 + q4*c1
blue_pr_blue_bus_indirect := w1*a2 + w1*a3*c1 + w4*c1
blue_pr_blue_subway_indirect := r1*a2 + r1*a3*c1 + r4*c1
blue_pr_blue_dTime_indirect := u1*a2 + u1*a3*c1 + u4*c1
blue_pr_blue_Stravel_indirect := p1*a2 + p1*a3*c1 + p4*c1
blue_pr_blue_Smode_indirect := l1*a2 + l1*a3*c1 + l4*c1
blue_pr_blue_SdTime_indirect := v1*a2 + v1*a3*c1 + v4*c1

#blue_sef
blue_sef_blue_travel_direct:= z4
blue_sef_blue_ebike_direct := x4
blue_sef_blue_car_direct := y4
blue_sef_blue_taxi_direct := q4
blue_sef_blue_bus_direct := w4
blue_sef_blue_subway_direct := r4
blue_sef_blue_dTime_direct := u4
blue_sef_blue_Stravel_direct := p4
blue_sef_blue_Smode_direct := l4
blue_sef_blue_SdTime_direct := v4

blue_sef_blue_travel_indirect := z1*a3
blue_sef_blue_ebike_indirect := x1*a3
blue_sef_blue_car_indirect := y1*a3
blue_sef_blue_taxi_indirect := q1*a3
blue_sef_blue_bus_indirect := w1*a3
blue_sef_blue_subway_indirect := r1*a3
blue_sef_blue_dTime_indirect := u1*a3
blue_sef_blue_Stravel_indirect := p1*a3
blue_sef_blue_Smode_indirect := l1*a3
blue_sef_blue_SdTime_indirect := v1*a3


#blue_gender
blue_gender_blue_travel_direct:= e1
blue_gender_blue_ebike_direct := f1
blue_gender_blue_car_direct := g1
blue_gender_blue_taxi_direct := h1
blue_gender_blue_bus_direct := i1
blue_gender_blue_subway_direct := j1
blue_gender_blue_dTime_direct := k1
blue_gender_blue_Stravel_direct := m1
blue_gender_blue_Smode_direct := n1
blue_gender_blue_SdTime_direct := t1

blue_gender_blue_travel_indirect:= z1*a4 + z1*a1*d1 + z1*a2*b2 + z1*a2*b1*d1 + z1*a3*c2 + z1*a3*c1*b2 + z1*a3*c1*b1*d1 + z2*d1 + z3*b2 + z3* b1*d1 + z4*c2 + z4*(c1*b2 + c1*b1*d1) + z1*a3*add1*d1 + z4*add1*d1
blue_gender_blue_ebike_indirect := x1*a4 + x1*a1*d1 + x1*a2*b2 + x1*a2*b1*d1 + x1*a3*c2 + x1*a3*c1*b2 + x1*a3*c1*b1*d1 + x2*d1 + x3*b2 + x3* b1*d1 + x4*c2 + x4*(c1*b2 + c1*b1*d1) + x1*a3*add1*d1 + x4*add1*d1
blue_gender_blue_car_indirect := y1*a4 + y1*a1*d1 + y1*a2*b2 + y1*a2*b1*d1 + y1*a3*c2 + y1*a3*c1*b2 + y1*a3*c1*b1*d1 + y2*d1 + y3*b2 + y3* b1*d1 + y4*c2 + y4*(c1*b2 + c1*b1*d1) + y1*a3*add1*d1 + y4*add1*d1
blue_gender_blue_taxi_indirect := q1*a4 + q1*a1*d1 + q1*a2*b2 + q1*a2*b1*d1 + q1*a3*c2 + q1*a3*c1*b2 + q1*a3*c1*b1*d1 + q2*d1 + q3*b2 + q3* b1*d1 + q4*c2 + q4*(c1*b2 + c1*b1*d1) + q1*a3*add1*d1 + q4*add1*d1
blue_gender_blue_bus_indirect := w1*a4 + w1*a1*d1 + w1*a2*b2 + w1*a2*b1*d1 + w1*a3*c2 + w1*a3*c1*b2 + w1*a3*c1*b1*d1 + w2*d1 + w3*b2 + w3* b1*d1 + w4*c2 + w4*(c1*b2 + c1*b1*d1) + w1*a3*add1*d1 + w4*add1*d1
blue_gender_blue_subway_indirect := r1*a4 + r1*a1*d1 + r1*a2*b2 + r1*a2*b1*d1 + r1*a3*c2 + r1*a3*c1*b2 + r1*a3*c1*b1*d1 + r2*d1 + r3*b2 + r3* b1*d1 + r4*c2 + r4*(c1*b2 + c1*b1*d1) + r1*a3*add1*d1 + r4*add1*d1
blue_gender_blue_dTime_indirect := u1*a4 + u1*a1*d1 + u1*a2*b2 + u1*a2*b1*d1 + u1*a3*c2 + u1*a3*c1*b2 + u1*a3*c1*b1*d1 + u2*d1 + u3*b2 + u3* b1*d1 + u4*c2 + u4*(c1*b2 + c1*b1*d1) + u1*a3*add1*d1 + u4*add1*d1
blue_gender_blue_Stravel_indirect := p1*a4 + p1*a1*d1 + p1*a2*b2 + p1*a2*b1*d1 + p1*a3*c2 + p1*a3*c1*b2 + p1*a3*c1*b1*d1 + p2*d1 + p3*b2 + p3* b1*d1 + p4*c2 + p4*(c1*b2 + c1*b1*d1) + p1*a3*add1*d1 + p4*add1*d1
blue_gender_blue_Smode_indirect := l1*a4 + l1*a1*d1 + l1*a2*b2 + l1*a2*b1*d1 + l1*a3*c2 + l1*a3*c1*b2 + l1*a3*c1*b1*d1 + l2*d1 + l3*b2 + l3* b1*d1 + l4*c2 + l4*(c1*b2 + c1*b1*d1) + l1*a3*add1*d1 + l4*add1*d1
blue_gender_blue_SdTime_indirect := v1*a4 + v1*a1*d1 + v1*a2*b2 + v1*a2*b1*d1 + v1*a3*c2 + v1*a3*c1*b2 + v1*a3*c1*b1*d1 + v2*d1 + v3*b2 + v3* b1*d1 + v4*c2 + v4*(c1*b2 + c1*b1*d1) + v1*a3*add1*d1 + v4*add1*d1

#blue_commute_distance
blue_commute_distance_blue_travel_direct:= e2
blue_commute_distance_blue_ebike_direct := f2
blue_commute_distance_blue_car_direct := g2
blue_commute_distance_blue_taxi_direct := h2
blue_commute_distance_blue_bus_direct := i2
blue_commute_distance_blue_subway_direct := j2
blue_commute_distance_blue_dTime_direct := k2
blue_commute_distance_blue_Stravel_direct := m2
blue_commute_distance_blue_Smode_direct := n2
blue_commute_distance_blue_SdTime_direct := t2

commute_distanceblue_travel_indirect:= z1*a5 + z1*a1*d2 + z1*a2*b3 + z1*a2*b1*d2 + z1*a3*c3 + z1*a3*c1*b3 + z1*a3*c1*b1*d2 + z2*d2 + z3*b3 + z3* b1*d2 + z4*c3 + z4*(c1*b3 + c1*b1*d2) + z1*a3*add1*d2 + z4*add1*d2
commute_distanceblue_ebike_indirect := x1*a5 + x1*a1*d2 + x1*a2*b3 + x1*a2*b1*d2 + x1*a3*c3 + x1*a3*c1*b3 + x1*a3*c1*b1*d2 + x2*d2 + x3*b3 + x3* b1*d2 + x4*c3 + x4*(c1*b3 + c1*b1*d2) + x1*a3*add1*d2 + x4*add1*d2
commute_distanceblue_car_indirect :=  y1*a5 + y1*a1*d2 + y1*a2*b3 + y1*a2*b1*d2 + y1*a3*c3 + y1*a3*c1*b3 + y1*a3*c1*b1*d2 + y2*d2 + x3*b3 + y3* b1*d2 + y4*c3 + y4*(c1*b3 + c1*b1*d2) + y1*a3*add1*d2 + y4*add1*d2
commute_distanceblue_taxi_indirect := q1*a5 + q1*a1*d2 + q1*a2*b3 + q1*a2*b1*d2 + q1*a3*c3 + q1*a3*c1*b3 + q1*a3*c1*b1*d2 + q2*d2 + q3*b3 + q3* b1*d2 + q4*c3 + q4*(c1*b3 + c1*b1*d2) + q1*a3*add1*d2 + q4*add1*d2
commute_distanceblue_bus_indirect := w1*a5 + w1*a1*d2 + w1*a2*b3 + w1*a2*b1*d2 + w1*a3*c3 + w1*a3*c1*b3 + w1*a3*c1*b1*d2 + w2*d2 + w3*b3 + w3* b1*d2 + w4*c3 + w4*(c1*b3 + c1*b1*d2) + w1*a3*add1*d2 + w4*add1*d2
commute_distanceblue_subway_indirect := r1*a5 + r1*a1*d2 + r1*a2*b3 + r1*a2*b1*d2 + r1*a3*c3 + r1*a3*c1*b3 + r1*a3*c1*b1*d2 + r2*d2 + r3*b3 + r3* b1*d2 + r4*c3 + r4*(c1*b3 + c1*b1*d2) + r1*a3*add1*d2 + r4*add1*d2
commute_distanceblue_dTime_indirect := u1*a5 + u1*a1*d2 + u1*a2*b3 + u1*a2*b1*d2 + u1*a3*c3 + u1*a3*c1*b3 + u1*a3*c1*b1*d2 + u2*d2 + u3*b3 + u3* b1*d2 + u4*c3 + u4*(c1*b3 + c1*b1*d2) + u1*a3*add1*d2 + u4*add1*d2
commute_distanceblue_Stravel_indirect := p1*a5 + p1*a1*d2 + p1*a2*b3 + p1*a2*b1*d2 + p1*a3*c3 + p1*a3*c1*b3 + p1*a3*c1*b1*d2 + p2*d2 + p3*b3 + p3* b1*d2 + p4*c3 + p4*(c1*b3 + c1*b1*d2) + p1*a3*add1*d2 + p4*add1*d2
commute_distanceblue_Smode_indirect := l1*a5 + l1*a1*d2 + l1*a2*b3 + l1*a2*b1*d2 + l1*a3*c3 + l1*a3*c1*b3 + l1*a3*c1*b1*d2 + l2*d2 + l3*b3 + l3* b1*d2 + l4*c3 + l4*(c1*b3 + c1*b1*d2) + l1*a3*add1*d2 + l4*add1*d2
commute_distanceblue_SdTime_indirect := v1*a5 + v1*a1*d2 + v1*a2*b3 + v1*a2*b1*d2 + v1*a3*c3 + v1*a3*c1*b3 + v1*a3*c1*b1*d2 + v2*d2 + v3*b3 + v3* b1*d2 + v4*c3 + v4*(c1*b3 + c1*b1*d2) + v1*a3*add1*d2 + v4*add1*d2


#past_travelDecision
past_travelDecision_blue_travel_direct:= e3
past_travelDecision_blue_ebike_direct := f3
past_travelDecision_blue_car_direct := g3
past_travelDecision_blue_taxi_direct := h3
past_travelDecision_blue_bus_direct := i3
past_travelDecision_blue_subway_direct := j3
past_travelDecision_blue_dTime_direct := k3
past_travelDecision_blue_Stravel_direct := m3
past_travelDecision_blue_Smode_direct := n3
past_travelDecision_blue_SdTime_direct := t3

past_travelDecision_blue_travel_indirect:= z1*a6 + z1*a1*d3 + z1*a2*b4 + z1*a2*b1*d3 + z1*a3*c4 + z1*a3*c1*b4 + z1*a3*c1*b1*d3 + z2*d3 + z3*b4 + z3* b1*d3 + z4*c4 + z4*(c1*b4 + c1*b1*d3) + z1*a3*add1*d3 + z4*add1*d3
past_travelDecision_blue_ebike_indirect := x1*a6 + x1*a1*d3 + x1*a2*b4 + x1*a2*b1*d3 + x1*a3*c4 + x1*a3*c1*b4 + x1*a3*c1*b1*d3 + x2*d3 + x3*b4 + x3* b1*d3 + x4*c4 + x4*(c1*b4 + c1*b1*d3) + x1*a3*add1*d3 + x4*add1*d3
past_travelDecision_blue_car_indirect :=  y1*a6 + y1*a1*d3 + y1*a2*b4 + y1*a2*b1*d3 + y1*a3*c4 + y1*a3*c1*b4 + y1*a3*c1*b1*d3 + y2*d3 + x3*b4 + y3* b1*d3 + y4*c4 + y4*(c1*b4 + c1*b1*d3) + y1*a3*add1*d3 + y4*add1*d3
past_travelDecision_blue_taxi_indirect := q1*a6 + q1*a1*d3 + q1*a2*b4 + q1*a2*b1*d3 + q1*a3*c4 + q1*a3*c1*b4 + q1*a3*c1*b1*d3 + q2*d3 + q3*b4 + q3* b1*d3 + q4*c4 + q4*(c1*b4 + c1*b1*d3) + q1*a3*add1*d3 + q4*add1*d3
past_travelDecision_blue_bus_indirect := w1*a6 + w1*a1*d3 + w1*a2*b4 + w1*a2*b1*d3 + w1*a3*c4 + w1*a3*c1*b4 + w1*a3*c1*b1*d3 + w2*d3 + w3*b4 + w3* b1*d3 + w4*c4 + w4*(c1*b4 + c1*b1*d3) + w1*a3*add1*d3 + w4*add1*d3
past_travelDecision_blue_subway_indirect := r1*a6 + r1*a1*d3 + r1*a2*b4 + r1*a2*b1*d3 + r1*a3*c4 + r1*a3*c1*b4 + r1*a3*c1*b1*d3 + r2*d3 + r3*b4 + r3* b1*d3 + r4*c4 + r4*(c1*b4 + c1*b1*d3) + r1*a3*add1*d3 + r4*add1*d3
past_travelDecision_blue_dTime_indirect := u1*a6 + u1*a1*d3 + u1*a2*b4 + u1*a2*b1*d3 + u1*a3*c4 + u1*a3*c1*b4 + u1*a3*c1*b1*d3 + u2*d3 + u3*b4 + u3* b1*d3 + u4*c4 + u4*(c1*b4 + c1*b1*d3) + u1*a3*add1*d3 + u4*add1*d3
past_travelDecision_blue_Stravel_indirect := p1*a6 + p1*a1*d3 + p1*a2*b4 + p1*a2*b1*d3 + p1*a3*c4 + p1*a3*c1*b4 + p1*a3*c1*b1*d3 + p2*d3 + p3*b4 + p3* b1*d3 + p4*c4 + p4*(c1*b4 + c1*b1*d3) + p1*a3*add1*d3 + p4*add1*d3
past_travelDecision_blue_Smode_indirect := l1*a6 + l1*a1*d3 + l1*a2*b4 + l1*a2*b1*d3 + l1*a3*c4 + l1*a3*c1*b4 + l1*a3*c1*b1*d3 + l2*d3 + l3*b4 + l3* b1*d3 + l4*c4 + l4*(c1*b4 + c1*b1*d3) + l1*a3*add1*d3 + l4*add1*d3
past_travelDecision_blue_SdTime_indirect := v1*a6 + v1*a1*d3 + v1*a2*b4 + v1*a2*b1*d3 + v1*a3*c4 + v1*a3*c1*b4 + v1*a3*c1*b1*d3 + v2*d3 + v3*b4 + v3* b1*d3 + v4*c4 + v4*(c1*b4 + c1*b1*d3) + v1*a3*add1*d3 + v4*add1*d3


#past_travelDtime
past_travelDtime_blue_travel_direct:= e4
past_travelDtime_blue_ebike_direct := f4
past_travelDtime_blue_car_direct := g4
past_travelDtime_blue_taxi_direct := h4
past_travelDtime_blue_bus_direct := i4
past_travelDtime_blue_subway_direct := j4
past_travelDtime_blue_dTime_direct := k4
past_travelDtime_blue_Stravel_direct := m4
past_travelDtime_blue_Smode_direct := n4
past_travelDtime_blue_SdTime_direct := t4

past_travelDtime_blue_travel_indirect:= z1*a7 + z1*a1*d4 + z1*a2*b5 + z1*a2*b1*d4 + z1*a3*c5 + z1*a3*c1*b5 + z1*a3*c1*b1*d4 + z2*d4 + z3*b5 + z3* b1*d4 + z4*c5 + z4*(c1*b5 + c1*b1*d4) + z1*a3*add1*d4 + z4*add1*d4
past_travelDtime_blue_ebike_indirect := x1*a7 + x1*a1*d4 + x1*a2*b5 + x1*a2*b1*d4 + x1*a3*c5 + x1*a3*c1*b5 + x1*a3*c1*b1*d4 + x2*d4 + x3*b5 + x3* b1*d4 + x4*c5 + x4*(c1*b5 + c1*b1*d4) + x1*a3*add1*d4 + x4*add1*d4
past_travelDtime_blue_car_indirect :=  y1*a7 + y1*a1*d4 + y1*a2*b5 + y1*a2*b1*d4 + y1*a3*c5 + y1*a3*c1*b5 + y1*a3*c1*b1*d4 + y2*d4 + x3*b5 + y3* b1*d4 + y4*c5 + y4*(c1*b5 + c1*b1*d4) + y1*a3*add1*d4 + y4*add1*d4
past_travelDtime_blue_taxi_indirect := q1*a7 + q1*a1*d4 + q1*a2*b5 + q1*a2*b1*d4 + q1*a3*c5 + q1*a3*c1*b5 + q1*a3*c1*b1*d4 + q2*d4 + q3*b5 + q3* b1*d4 + q4*c5 + q4*(c1*b5 + c1*b1*d4) + q1*a3*add1*d4 + q4*add1*d4
past_travelDtime_blue_bus_indirect := w1*a7 + w1*a1*d4 + w1*a2*b5 + w1*a2*b1*d4 + w1*a3*c5 + w1*a3*c1*b5 + w1*a3*c1*b1*d4 + w2*d4 + w3*b5 + w3* b1*d4 + w4*c5 + w4*(c1*b5 + c1*b1*d4) + w1*a3*add1*d4 + w4*add1*d4
past_travelDtime_blue_subway_indirect := r1*a7 + r1*a1*d4 + r1*a2*b5 + r1*a2*b1*d4 + r1*a3*c5 + r1*a3*c1*b5 + r1*a3*c1*b1*d4 + r2*d4 + r3*b5 + r3* b1*d4 + r4*c5 + r4*(c1*b5 + c1*b1*d4) + r1*a3*add1*d4 + r4*add1*d4
past_travelDtime_blue_dTime_indirect := u1*a7 + u1*a1*d4 + u1*a2*b5 + u1*a2*b1*d4 + u1*a3*c5 + u1*a3*c1*b5 + u1*a3*c1*b1*d4 + u2*d4 + u3*b5 + u3* b1*d4 + u4*c5 + u4*(c1*b5 + c1*b1*d4) + u1*a3*add1*d4 + u4*add1*d4
past_travelDtime_blue_Stravel_indirect := p1*a7 + p1*a1*d4 + p1*a2*b5 + p1*a2*b1*d4 + p1*a3*c5 + p1*a3*c1*b5 + p1*a3*c1*b1*d4 + p2*d4 + p3*b5 + p3* b1*d4 + p4*c5 + p4*(c1*b5 + c1*b1*d4) + p1*a3*add1*d4 + p4*add1*d4
past_travelDtime_blue_Smode_indirect := l1*a7 + l1*a1*d4 + l1*a2*b5 + l1*a2*b1*d4 + l1*a3*c5 + l1*a3*c1*b5 + l1*a3*c1*b1*d4 + l2*d4 + l3*b5 + l3* b1*d4 + l4*c5 + l4*(c1*b5 + c1*b1*d4) + l1*a3*add1*d4 + l4*add1*d4
past_travelDtime_blue_SdTime_indirect := v1*a7 + v1*a1*d4 + v1*a2*b5 + v1*a2*b1*d4 + v1*a3*c5 + v1*a3*c1*b5 + v1*a3*c1*b1*d4 + v2*d4 + v3*b5 + v3* b1*d4 + v4*c5 + v4*(c1*b5 + c1*b1*d4) + v1*a3*add1*d4 + v4*add1*d4

#past_travelDisruption
past_travelDisruption_blue_travel_direct:= e5
past_travelDisruption_blue_ebike_direct := f5
past_travelDisruption_blue_car_direct := g5
past_travelDisruption_blue_taxi_direct := h5
past_travelDisruption_blue_bus_direct := i5
past_travelDisruption_blue_subway_direct := j5
past_travelDisruption_blue_dTime_direct := k5
past_travelDisruption_blue_Stravel_direct := m5
past_travelDisruption_blue_Smode_direct := n5
past_travelDisruption_blue_SdTime_direct := t5

past_travelDisruption_blue_travel_indirect:= z1*a8 + z1*a1*d5 + z1*a2*b6 + z1*a2*b1*d5 + z1*a3*c6 + z1*a3*c1*b6 + z1*a3*c1*b1*d5 + z2*d5 + z3*b6 + z3* b1*d5 + z4*c6 + z4*(c1*b6 + c1*b1*d5) + z1*a3*add1*d5 + z4*add1*d5
past_travelDisruption_blue_ebike_indirect := x1*a8 + x1*a1*d5 + x1*a2*b6 + x1*a2*b1*d5 + x1*a3*c6 + x1*a3*c1*b6 + x1*a3*c1*b1*d5 + x2*d5 + x3*b6 + x3* b1*d5 + x4*c6 + x4*(c1*b6 + c1*b1*d5) + x1*a3*add1*d5 + x4*add1*d5
past_travelDisruption_blue_car_indirect :=  y1*a8 + y1*a1*d5 + y1*a2*b6 + y1*a2*b1*d5 + y1*a3*c6 + y1*a3*c1*b6 + y1*a3*c1*b1*d5 + y2*d5 + x3*b6 + y3* b1*d5 + y4*c6 + y4*(c1*b6 + c1*b1*d5) + y1*a3*add1*d5 + y4*add1*d5
past_travelDisruption_blue_taxi_indirect := q1*a8 + q1*a1*d5 + q1*a2*b6 + q1*a2*b1*d5 + q1*a3*c6 + q1*a3*c1*b6 + q1*a3*c1*b1*d5 + q2*d5 + q3*b6 + q3* b1*d5 + q4*c6 + q4*(c1*b6 + c1*b1*d5) + q1*a3*add1*d5 + q4*add1*d5
past_travelDisruption_blue_bus_indirect := w1*a8 + w1*a1*d5 + w1*a2*b6 + w1*a2*b1*d5 + w1*a3*c6 + w1*a3*c1*b6 + w1*a3*c1*b1*d5 + w2*d5 + w3*b6 + w3* b1*d5 + w4*c6 + w4*(c1*b6 + c1*b1*d5) + w1*a3*add1*d5 + w4*add1*d5
past_travelDisruption_blue_subway_indirect := r1*a8 + r1*a1*d5 + r1*a2*b6 + r1*a2*b1*d5 + r1*a3*c6 + r1*a3*c1*b6 + r1*a3*c1*b1*d5 + r2*d5 + r3*b6 + r3* b1*d5 + r4*c6 + r4*(c1*b6 + c1*b1*d5) + r1*a3*add1*d5 + r4*add1*d5
past_travelDisruption_blue_dTime_indirect := u1*a8 + u1*a1*d5 + u1*a2*b6 + u1*a2*b1*d5 + u1*a3*c6 + u1*a3*c1*b6 + u1*a3*c1*b1*d5 + u2*d5 + u3*b6 + u3* b1*d5 + u4*c6 + u4*(c1*b6 + c1*b1*d5) + u1*a3*add1*d5 + u4*add1*d5
past_travelDisruption_blue_Stravel_indirect := p1*a8 + p1*a1*d5 + p1*a2*b6 + p1*a2*b1*d5 + p1*a3*c6 + p1*a3*c1*b6 + p1*a3*c1*b1*d5 + p2*d5 + p3*b6 + p3* b1*d5 + p4*c6 + p4*(c1*b6 + c1*b1*d5) + p1*a3*add1*d5 + p4*add1*d5
past_travelDisruption_blue_Smode_indirect := l1*a8 + l1*a1*d5 + l1*a2*b6 + l1*a2*b1*d5 + l1*a3*c6 + l1*a3*c1*b6 + l1*a3*c1*b1*d5 + l2*d5 + l3*b6 + l3* b1*d5 + l4*c6 + l4*(c1*b6 + c1*b1*d5) + l1*a3*add1*d5 + l4*add1*d5
past_travelDisruption_blue_SdTime_indirect := v1*a8 + v1*a1*d5 + v1*a2*b6 + v1*a2*b1*d5 + v1*a3*c6 + v1*a3*c1*b6 + v1*a3*c1*b1*d5 + v2*d5 + v3*b6 + v3* b1*d5 + v4*c6 + v4*(c1*b6 + c1*b1*d5) + v1*a3*add1*d5 + v4*add1*d5


#bus_access
bus_access_blue_travel_direct:= e6
bus_access_blue_ebike_direct := f6
bus_access_blue_car_direct := g6
bus_access_blue_taxi_direct := h6
bus_access_blue_bus_direct := i6
bus_access_blue_subway_direct := j6
bus_access_blue_dTime_direct := k6
bus_access_blue_Stravel_direct := m6
bus_access_blue_Smode_direct := n6
bus_access_blue_SdTime_direct := t6

bus_access_blue_travel_indirect:= z1*a9 + z1*a1*d6 + z1*a2*b7 + z1*a2*b1*d6 + z1*a3*c7 + z1*a3*c1*b7 + z1*a3*c1*b1*d6 + z2*d6 + z3*b7 + z3* b1*d6 + z4*c7 + z4*(c1*b7 + c1*b1*d6) + z1*a3*add1*d6 + z4*add1*d6
bus_access_blue_ebike_indirect := x1*a9 + x1*a1*d6 + x1*a2*b7 + x1*a2*b1*d6 + x1*a3*c7 + x1*a3*c1*b7 + x1*a3*c1*b1*d6 + x2*d6 + x3*b7 + x3* b1*d6 + x4*c7 + x4*(c1*b7 + c1*b1*d6) + x1*a3*add1*d6 + x4*add1*d6
bus_access_blue_car_indirect :=  y1*a9 + y1*a1*d6 + y1*a2*b7 + y1*a2*b1*d6 + y1*a3*c7 + y1*a3*c1*b7 + y1*a3*c1*b1*d6 + y2*d6 + x3*b7 + y3* b1*d6 + y4*c7 + y4*(c1*b7 + c1*b1*d6) + y1*a3*add1*d6 + y4*add1*d6
bus_access_blue_taxi_indirect := q1*a9 + q1*a1*d6 + q1*a2*b7 + q1*a2*b1*d6 + q1*a3*c7 + q1*a3*c1*b7 + q1*a3*c1*b1*d6 + q2*d6 + q3*b7 + q3* b1*d6 + q4*c7 + q4*(c1*b7 + c1*b1*d6) + q1*a3*add1*d6 + q4*add1*d6
bus_access_blue_bus_indirect := w1*a9 + w1*a1*d6 + w1*a2*b7 + w1*a2*b1*d6 + w1*a3*c7 + w1*a3*c1*b7 + w1*a3*c1*b1*d6 + w2*d6 + w3*b7 + w3* b1*d6 + w4*c7 + w4*(c1*b7 + c1*b1*d6) + w1*a3*add1*d6 + w4*add1*d6
bus_access_blue_subway_indirect := r1*a9 + r1*a1*d6 + r1*a2*b7 + r1*a2*b1*d6 + r1*a3*c7 + r1*a3*c1*b7 + r1*a3*c1*b1*d6 + r2*d6 + r3*b7 + r3* b1*d6 + r4*c7 + r4*(c1*b7 + c1*b1*d6) + r1*a3*add1*d6 + r4*add1*d6
bus_access_blue_dTime_indirect := u1*a9 + u1*a1*d6 + u1*a2*b7 + u1*a2*b1*d6 + u1*a3*c7 + u1*a3*c1*b7 + u1*a3*c1*b1*d6 + u2*d6 + u3*b7 + u3* b1*d6 + u4*c7 + u4*(c1*b7 + c1*b1*d6) + u1*a3*add1*d6 + u4*add1*d6
bus_access_blue_Stravel_indirect := p1*a9 + p1*a1*d6 + p1*a2*b7 + p1*a2*b1*d6 + p1*a3*c7 + p1*a3*c1*b7 + p1*a3*c1*b1*d6 + p2*d6 + p3*b7 + p3* b1*d6 + p4*c7 + p4*(c1*b7 + c1*b1*d6) + p1*a3*add1*d6 + p4*add1*d6
bus_access_blue_Smode_indirect := l1*a9 + l1*a1*d6 + l1*a2*b7 + l1*a2*b1*d6 + l1*a3*c7 + l1*a3*c1*b7 + l1*a3*c1*b1*d6 + l2*d6 + l3*b7 + l3* b1*d6 + l4*c7 + l4*(c1*b7 + c1*b1*d6) + l1*a3*add1*d6 + l4*add1*d6
bus_access_blue_SdTime_indirect := v1*a9 + v1*a1*d6 + v1*a2*b7 + v1*a2*b1*d6 + v1*a3*c7 + v1*a3*c1*b7 + v1*a3*c1*b1*d6 + v2*d6 + v3*b7 + v3* b1*d6 + v4*c7 + v4*(c1*b7 + c1*b1*d6) + v1*a3*add1*d6 + v4*add1*d6

#metro_access
metro_access_blue_travel_direct:= e7
metro_access_blue_ebike_direct := f7
metro_access_blue_car_direct := g7
metro_access_blue_taxi_direct := h7
metro_access_blue_bus_direct := i7
metro_access_blue_subway_direct := j7
metro_access_blue_dTime_direct := k7
metro_access_blue_Stravel_direct := m7
metro_access_blue_Smode_direct := n7
metro_access_blue_SdTime_direct := t7

metro_access_blue_travel_indirect:= z1*a10 + z1*a1*d7 + z1*a2*b8 + z1*a2*b1*d7 + z1*a3*c8 + z1*a3*c1*b8 + z1*a3*c1*b1*d7 + z2*d7 + z3*b8 + z3* b1*d7 + z4*c8 + z4*(c1*b8 + c1*b1*d7) + z1*a3*add1*d7 + z4*add1*d7
metro_access_blue_ebike_indirect := x1*a10 + x1*a1*d7 + x1*a2*b8 + x1*a2*b1*d7 + x1*a3*c8 + x1*a3*c1*b8 + x1*a3*c1*b1*d7 + x2*d7 + x3*b8 + x3* b1*d7 + x4*c8 + x4*(c1*b8 + c1*b1*d7) + x1*a3*add1*d7 + x4*add1*d7
metro_access_blue_car_indirect :=  y1*a10 + y1*a1*d7 + y1*a2*b8 + y1*a2*b1*d7 + y1*a3*c8 + y1*a3*c1*b8 + y1*a3*c1*b1*d7 + y2*d7 + x3*b8 + y3* b1*d7 + y4*c8 + y4*(c1*b8 + c1*b1*d7) + y1*a3*add1*d7 + y4*add1*d7
metro_access_blue_taxi_indirect := q1*a10 + q1*a1*d7 + q1*a2*b8 + q1*a2*b1*d7 + q1*a3*c8 + q1*a3*c1*b8 + q1*a3*c1*b1*d7 + q2*d7 + q3*b8 + q3* b1*d7 + q4*c8 + q4*(c1*b8 + c1*b1*d7) + q1*a3*add1*d7 + q4*add1*d7
metro_access_blue_bus_indirect := w1*a10 + w1*a1*d7 + w1*a2*b8 + w1*a2*b1*d7 + w1*a3*c8 + w1*a3*c1*b8 + w1*a3*c1*b1*d7 + w2*d7 + w3*b8 + w3* b1*d7 + w4*c8 + w4*(c1*b8 + c1*b1*d7) + w1*a3*add1*d7 + w4*add1*d7
metro_access_blue_subway_indirect := r1*a10 + r1*a1*d7 + r1*a2*b8 + r1*a2*b1*d7 + r1*a3*c8 + r1*a3*c1*b8 + r1*a3*c1*b1*d7 + r2*d7 + r3*b8 + r3* b1*d7 + r4*c8 + r4*(c1*b8 + c1*b1*d7) + r1*a3*add1*d7 + r4*add1*d7
metro_access_blue_dTime_indirect := u1*a10 + u1*a1*d7 + u1*a2*b8 + u1*a2*b1*d7 + u1*a3*c8 + u1*a3*c1*b8 + u1*a3*c1*b1*d7 + u2*d7 + u3*b8 + u3* b1*d7 + u4*c8 + u4*(c1*b8 + c1*b1*d7) + u1*a3*add1*d7 + u4*add1*d7
metro_access_blue_Stravel_indirect := p1*a10 + p1*a1*d7 + p1*a2*b8 + p1*a2*b1*d7 + p1*a3*c8 + p1*a3*c1*b8 + p1*a3*c1*b1*d7 + p2*d7 + p3*b8 + p3* b1*d7 + p4*c8 + p4*(c1*b8 + c1*b1*d7) + p1*a3*add1*d7 + p4*add1*d7
metro_access_blue_Smode_indirect := l1*a10 + l1*a1*d7 + l1*a2*b8 + l1*a2*b1*d7 + l1*a3*c8 + l1*a3*c1*b8 + l1*a3*c1*b1*d7 + l2*d7 + l3*b8 + l3* b1*d7 + l4*c8 + l4*(c1*b8 + c1*b1*d7) + l1*a3*add1*d7 + l4*add1*d7
metro_access_blue_SdTime_indirect := v1*a10 + v1*a1*d7 + v1*a2*b8 + v1*a2*b1*d7 + v1*a3*c8 + v1*a3*c1*b8 + v1*a3*c1*b1*d7 + v2*d7 + v3*b8 + v3* b1*d7 + v4*c8 + v4*(c1*b8 + c1*b1*d7) + v1*a3*add1*d7 + v4*add1*d7


#taxi_wait
taxi_wait_blue_travel_direct:= e8
taxi_wait_blue_ebike_direct := f8
taxi_wait_blue_car_direct := g8
taxi_wait_blue_taxi_direct := h8
taxi_wait_blue_bus_direct := i8
taxi_wait_blue_subway_direct := j8
taxi_wait_blue_dTime_direct := k8
taxi_wait_blue_Stravel_direct := m8
taxi_wait_blue_Smode_direct := n8
taxi_wait_blue_SdTime_direct := t8

taxi_wait_blue_travel_indirect:= z1*a11 + z1*a1*d8 + z1*a2*b9 + z1*a2*b1*d8 + z1*a3*c9 + z1*a3*c1*b9 + z1*a3*c1*b1*d8 + z2*d8 + z3*b9 + z3* b1*d8 + z4*c9 + z4*(c1*b9 + c1*b1*d8) + z1*a3*add1*d8 + z4*add1*d8
taxi_wait_blue_ebike_indirect := x1*a11 + x1*a1*d8 + x1*a2*b9 + x1*a2*b1*d8 + x1*a3*c9 + x1*a3*c1*b9 + x1*a3*c1*b1*d8 + x2*d8 + x3*b9 + x3* b1*d8 + x4*c9 + x4*(c1*b9 + c1*b1*d8) + x1*a3*add1*d8 + x4*add1*d8
taxi_wait_blue_car_indirect :=  y1*a11 + y1*a1*d8 + y1*a2*b9 + y1*a2*b1*d8 + y1*a3*c9 + y1*a3*c1*b9 + y1*a3*c1*b1*d8 + y2*d8 + x3*b9 + y3* b1*d8 + y4*c9 + y4*(c1*b9 + c1*b1*d8) + y1*a3*add1*d8 + y4*add1*d8
taxi_wait_blue_taxi_indirect := q1*a11 + q1*a1*d8 + q1*a2*b9 + q1*a2*b1*d8 + q1*a3*c9 + q1*a3*c1*b9 + q1*a3*c1*b1*d8 + q2*d8 + q3*b9 + q3* b1*d8 + q4*c9 + q4*(c1*b9 + c1*b1*d8) + q1*a3*add1*d8 + q4*add1*d8
taxi_wait_blue_bus_indirect := w1*a11 + w1*a1*d8 + w1*a2*b9 + w1*a2*b1*d8 + w1*a3*c9 + w1*a3*c1*b9 + w1*a3*c1*b1*d8 + w2*d8 + w3*b9 + w3* b1*d8 + w4*c9 + w4*(c1*b9 + c1*b1*d8) + w1*a3*add1*d8 + w4*add1*d8
taxi_wait_blue_subway_indirect := r1*a11 + r1*a1*d8 + r1*a2*b9 + r1*a2*b1*d8 + r1*a3*c9 + r1*a3*c1*b9 + r1*a3*c1*b1*d8 + r2*d8 + r3*b9 + r3* b1*d8 + r4*c9 + r4*(c1*b9 + c1*b1*d8) + r1*a3*add1*d8 + r4*add1*d8
taxi_wait_blue_dTime_indirect := u1*a11 + u1*a1*d8 + u1*a2*b9 + u1*a2*b1*d8 + u1*a3*c9 + u1*a3*c1*b9 + u1*a3*c1*b1*d8 + u2*d8 + u3*b9 + u3* b1*d8 + u4*c9 + u4*(c1*b9 + c1*b1*d8) + u1*a3*add1*d8 + u4*add1*d8
taxi_wait_blue_Stravel_indirect := p1*a11 + p1*a1*d8 + p1*a2*b9 + p1*a2*b1*d8 + p1*a3*c9 + p1*a3*c1*b9 + p1*a3*c1*b1*d8 + p2*d8 + p3*b9 + p3* b1*d8 + p4*c9 + p4*(c1*b9 + c1*b1*d8) + p1*a3*add1*d8 + p4*add1*d8
taxi_wait_blue_Smode_indirect := l1*a11 + l1*a1*d8 + l1*a2*b9 + l1*a2*b1*d8 + l1*a3*c9 + l1*a3*c1*b9 + l1*a3*c1*b1*d8 + l2*d8 + l3*b9 + l3* b1*d8 + l4*c9 + l4*(c1*b9 + c1*b1*d8) + l1*a3*add1*d8 + l4*add1*d8
taxi_wait_blue_SdTime_indirect := v1*a11 + v1*a1*d8 + v1*a2*b9 + v1*a2*b1*d8 + v1*a3*c9 + v1*a3*c1*b9 + v1*a3*c1*b1*d8 + v2*d8 + v3*b9 + v3* b1*d8 + v4*c9 + v4*(c1*b9 + c1*b1*d8) + v1*a3*add1*d8 + v4*add1*d8


#commuting_time
commuting_time_blue_travel_direct:= e9
commuting_time_blue_ebike_direct := f9
commuting_time_blue_car_direct := g9
commuting_time_blue_taxi_direct := h9
commuting_time_blue_bus_direct := i9
commuting_time_blue_subway_direct := j9
commuting_time_blue_dTime_direct := k9
commuting_time_blue_Stravel_direct := m9
commuting_time_blue_Smode_direct := n9
commuting_time_blue_SdTime_direct := t9

commuting_time_blue_travel_indirect:= z1*a12 + z1*a1*d9 + z1*a2*b10 + z1*a2*b1*d9 + z1*a3*c10 + z1*a3*c1*b10 + z1*a3*c1*b1*d9 + z2*d9 + z3*b10 + z3* b1*d9 + z4*c10 + z4*(c1*b10 + c1*b1*d9) + z1*a3*add1*d9 + z4*add1*d9
commuting_time_blue_ebike_indirect := x1*a12 + x1*a1*d9 + x1*a2*b10 + x1*a2*b1*d9 + x1*a3*c10 + x1*a3*c1*b10 + x1*a3*c1*b1*d9 + x2*d9 + x3*b10 + x3* b1*d9 + x4*c10 + x4*(c1*b10 + c1*b1*d9) + x1*a3*add1*d9 + x4*add1*d9
commuting_time_blue_car_indirect :=  y1*a12 + y1*a1*d9 + y1*a2*b10 + y1*a2*b1*d9 + y1*a3*c10 + y1*a3*c1*b10 + y1*a3*c1*b1*d9 + y2*d9 + x3*b10 + y3* b1*d9 + y4*c10 + y4*(c1*b10 + c1*b1*d9) + y1*a3*add1*d9 + y4*add1*d9
commuting_time_blue_taxi_indirect := q1*a12 + q1*a1*d9 + q1*a2*b10 + q1*a2*b1*d9 + q1*a3*c10 + q1*a3*c1*b10 + q1*a3*c1*b1*d9 + q2*d9 + q3*b10 + q3* b1*d9 + q4*c10 + q4*(c1*b10 + c1*b1*d9) + q1*a3*add1*d9 + q4*add1*d9
commuting_time_blue_bus_indirect := w1*a12 + w1*a1*d9 + w1*a2*b10 + w1*a2*b1*d9 + w1*a3*c10 + w1*a3*c1*b10 + w1*a3*c1*b1*d9 + w2*d9 + w3*b10 + w3* b1*d9 + w4*c10 + w4*(c1*b10 + c1*b1*d9) + w1*a3*add1*d9 + w4*add1*d9
commuting_time_blue_subway_indirect := r1*a12 + r1*a1*d9 + r1*a2*b10 + r1*a2*b1*d9 + r1*a3*c10 + r1*a3*c1*b10 + r1*a3*c1*b1*d9 + r2*d9 + r3*b10 + r3* b1*d9 + r4*c10 + r4*(c1*b10 + c1*b1*d9) + r1*a3*add1*d9 + r4*add1*d9
commuting_time_blue_dTime_indirect := u1*a12 + u1*a1*d9 + u1*a2*b10 + u1*a2*b1*d9 + u1*a3*c10 + u1*a3*c1*b10 + u1*a3*c1*b1*d9 + u2*d9 + u3*b10 + u3* b1*d9 + u4*c10 + u4*(c1*b10 + c1*b1*d9) + u1*a3*add1*d9 + u4*add1*d9
commuting_time_blue_Stravel_indirect := p1*a12 + p1*a1*d9 + p1*a2*b10 + p1*a2*b1*d9 + p1*a3*c10 + p1*a3*c1*b10 + p1*a3*c1*b1*d9 + p2*d9 + p3*b10 + p3* b1*d9 + p4*c10 + p4*(c1*b10 + c1*b1*d9) + p1*a3*add1*d9 + p4*add1*d9
commuting_time_blue_Smode_indirect := l1*a12 + l1*a1*d9 + l1*a2*b10 + l1*a2*b1*d9 + l1*a3*c10 + l1*a3*c1*b10 + l1*a3*c1*b1*d9 + l2*d9 + l3*b10 + l3* b1*d9 + l4*c10 + l4*(c1*b10 + c1*b1*d9) + l1*a3*add1*d9 + l4*add1*d9
commuting_time_blue_SdTime_indirect := v1*a12 + v1*a1*d9 + v1*a2*b10 + v1*a2*b1*d9 + v1*a3*c10 + v1*a3*c1*b10 + v1*a3*c1*b1*d9 + v2*d9 + v3*b10 + v3* b1*d9 + v4*c10 + v4*(c1*b10 + c1*b1*d9) + v1*a3*add1*d9 + v4*add1*d9


#weather_freq
weather_freq_blue_travel_direct:= e10
weather_freq_blue_ebike_direct := f10
weather_freq_blue_car_direct := g10
weather_freq_blue_taxi_direct := h10
weather_freq_blue_bus_direct := i10
weather_freq_blue_subway_direct := j10
weather_freq_blue_dTime_direct := k10
weather_freq_blue_Stravel_direct := m10
weather_freq_blue_Smode_direct := n10
weather_freq_blue_SdTime_direct := t10

weather_freq_blue_travel_indirect:= z1*a13 + z1*a1*d10 + z1*a2*b11 + z1*a2*b1*d10 + z1*a3*c11 + z1*a3*c1*b11 + z1*a3*c1*b1*d10 + z2*d10 + z3*b11 + z3* b1*d10 + z4*c11 + z4*(c1*b11 + c1*b1*d10) + z1*a3*add1*d10 + z4*add1*d10
weather_freq_blue_ebike_indirect := x1*a13 + x1*a1*d10 + x1*a2*b11 + x1*a2*b1*d10 + x1*a3*c11 + x1*a3*c1*b11 + x1*a3*c1*b1*d10 + x2*d10 + x3*b11 + x3* b1*d10 + x4*c11 + x4*(c1*b11 + c1*b1*d10) + x1*a3*add1*d10 + x4*add1*d10
weather_freq_blue_car_indirect :=  y1*a13 + y1*a1*d10 + y1*a2*b11 + y1*a2*b1*d10 + y1*a3*c11 + y1*a3*c1*b11 + y1*a3*c1*b1*d10 + y2*d10 + x3*b11 + y3* b1*d10 + y4*c11 + y4*(c1*b11 + c1*b1*d10) + y1*a3*add1*d10 + y4*add1*d10
weather_freq_blue_taxi_indirect := q1*a13 + q1*a1*d10 + q1*a2*b11 + q1*a2*b1*d10 + q1*a3*c11 + q1*a3*c1*b11 + q1*a3*c1*b1*d10 + q2*d10 + q3*b11 + q3* b1*d10 + q4*c11 + q4*(c1*b11 + c1*b1*d10) + q1*a3*add1*d10 + q4*add1*d10
weather_freq_blue_bus_indirect := w1*a13 + w1*a1*d10 + w1*a2*b11 + w1*a2*b1*d10 + w1*a3*c11 + w1*a3*c1*b11 + w1*a3*c1*b1*d10 + w2*d10 + w3*b11 + w3* b1*d10 + w4*c11 + w4*(c1*b11 + c1*b1*d10) + w1*a3*add1*d10 + w4*add1*d10
weather_freq_blue_subway_indirect := r1*a13 + r1*a1*d10 + r1*a2*b11 + r1*a2*b1*d10 + r1*a3*c11 + r1*a3*c1*b11 + r1*a3*c1*b1*d10 + r2*d10 + r3*b11 + r3* b1*d10 + r4*c11 + r4*(c1*b11 + c1*b1*d10) + r1*a3*add1*d10 + r4*add1*d10
weather_freq_blue_dTime_indirect := u1*a13 + u1*a1*d10 + u1*a2*b11 + u1*a2*b1*d10 + u1*a3*c11 + u1*a3*c1*b11 + u1*a3*c1*b1*d10 + u2*d10 + u3*b11 + u3* b1*d10 + u4*c11 + u4*(c1*b11 + c1*b1*d10) + u1*a3*add1*d10 + u4*add1*d10
weather_freq_blue_Stravel_indirect := p1*a13 + p1*a1*d10 + p1*a2*b11 + p1*a2*b1*d10 + p1*a3*c11 + p1*a3*c1*b11 + p1*a3*c1*b1*d10 + p2*d10 + p3*b11 + p3* b1*d10 + p4*c11 + p4*(c1*b11 + c1*b1*d10) + p1*a3*add1*d10 + p4*add1*d10
weather_freq_blue_Smode_indirect := l1*a13 + l1*a1*d10 + l1*a2*b11 + l1*a2*b1*d10 + l1*a3*c11 + l1*a3*c1*b11 + l1*a3*c1*b1*d10 + l2*d10 + l3*b11 + l3* b1*d10 + l4*c11 + l4*(c1*b11 + c1*b1*d10) + l1*a3*add1*d10 + l4*add1*d10
weather_freq_blue_SdTime_indirect := v1*a13 + v1*a1*d10 + v1*a2*b11 + v1*a2*b1*d10 + v1*a3*c11 + v1*a3*c1*b11 + v1*a3*c1*b1*d10 + v2*d10 + v3*b11 + v3* b1*d10 + v4*c11 + v4*(c1*b11 + c1*b1*d10) + v1*a3*add1*d10 + v4*add1*d10

#age
age_blue_travel_direct:= e11
age_blue_ebike_direct := f11
age_blue_car_direct := g11
age_blue_taxi_direct := h11
age_blue_bus_direct := i11
age_blue_subway_direct := j11
age_blue_dTime_direct := k11
age_blue_Stravel_direct := m11
age_blue_Smode_direct := n11
age_blue_SdTime_direct := t11

age_blue_travel_indirect:= z1*a14 + z1*a1*d11 + z1*a2*b12 + z1*a2*b1*d11 + z1*a3*c12 + z1*a3*c1*b12 + z1*a3*c1*b1*d11 + z2*d11 + z3*b12 + z3* b1*d11 + z4*c12 + z4*(c1*b12 + c1*b1*d11) + z1*a3*add1*d11 + z4*add1*d11
age_blue_ebike_indirect := x1*a14 + x1*a1*d11 + x1*a2*b12 + x1*a2*b1*d11 + x1*a3*c12 + x1*a3*c1*b12 + x1*a3*c1*b1*d11 + x2*d11 + x3*b12 + x3* b1*d11 + x4*c12 + x4*(c1*b12 + c1*b1*d11) + x1*a3*add1*d11 + x4*add1*d11
age_blue_car_indirect :=  y1*a14 + y1*a1*d11 + y1*a2*b12 + y1*a2*b1*d11 + y1*a3*c12 + y1*a3*c1*b12 + y1*a3*c1*b1*d11 + y2*d11 + x3*b12 + y3* b1*d11 + y4*c12 + y4*(c1*b12 + c1*b1*d11) + y1*a3*add1*d11 + y4*add1*d11
age_blue_taxi_indirect := q1*a14 + q1*a1*d11 + q1*a2*b12 + q1*a2*b1*d11 + q1*a3*c12 + q1*a3*c1*b12 + q1*a3*c1*b1*d11 + q2*d11 + q3*b12 + q3* b1*d11 + q4*c12 + q4*(c1*b12 + c1*b1*d11) + q1*a3*add1*d11 + q4*add1*d11
age_blue_bus_indirect := w1*a14 + w1*a1*d11 + w1*a2*b12 + w1*a2*b1*d11 + w1*a3*c12 + w1*a3*c1*b12 + w1*a3*c1*b1*d11 + w2*d11 + w3*b12 + w3* b1*d11 + w4*c12 + w4*(c1*b12 + c1*b1*d11) + w1*a3*add1*d11 + w4*add1*d11
age_blue_subway_indirect := r1*a14 + r1*a1*d11 + r1*a2*b12 + r1*a2*b1*d11 + r1*a3*c12 + r1*a3*c1*b12 + r1*a3*c1*b1*d11 + r2*d11 + r3*b12 + r3* b1*d11 + r4*c12 + r4*(c1*b12 + c1*b1*d11) + r1*a3*add1*d11 + r4*add1*d11
age_blue_dTime_indirect := u1*a14 + u1*a1*d11 + u1*a2*b12 + u1*a2*b1*d11 + u1*a3*c12 + u1*a3*c1*b12 + u1*a3*c1*b1*d11 + u2*d11 + u3*b12 + u3* b1*d11 + u4*c12 + u4*(c1*b12 + c1*b1*d11) + u1*a3*add1*d11 + u4*add1*d11
age_blue_Stravel_indirect := p1*a14 + p1*a1*d11 + p1*a2*b12 + p1*a2*b1*d11 + p1*a3*c12 + p1*a3*c1*b12 + p1*a3*c1*b1*d11 + p2*d11 + p3*b12 + p3* b1*d11 + p4*c12 + p4*(c1*b12 + c1*b1*d11) + p1*a3*add1*d11 + p4*add1*d11
age_blue_Smode_indirect := l1*a14 + l1*a1*d11 + l1*a2*b12 + l1*a2*b1*d11 + l1*a3*c12 + l1*a3*c1*b12 + l1*a3*c1*b1*d11 + l2*d11 + l3*b12 + l3* b1*d11 + l4*c12 + l4*(c1*b12 + c1*b1*d11) + l1*a3*add1*d11 + l4*add1*d11
age_blue_SdTime_indirect := v1*a14 + v1*a1*d11 + v1*a2*b12 + v1*a2*b1*d11 + v1*a3*c12 + v1*a3*c1*b12 + v1*a3*c1*b1*d11 + v2*d11 + v3*b12 + v3* b1*d11 + v4*c12 + v4*(c1*b12 + c1*b1*d11) + v1*a3*add1*d11 + v4*add1*d11

#income
income_blue_travel_direct:= e12
income_blue_ebike_direct := f12
income_blue_car_direct := g12
income_blue_taxi_direct := h12
income_blue_bus_direct := i12
income_blue_subway_direct := j12
income_blue_dTime_direct := k12
income_blue_Stravel_direct := m12
income_blue_Smode_direct := n12
income_blue_SdTime_direct := t12

income_blue_travel_indirect:= z1*a15 + z1*a1*d12 + z1*a2*b13 + z1*a2*b1*d12 + z1*a3*c13 + z1*a3*c1*b13 + z1*a3*c1*b1*d12 + z2*d12 + z3*b13 + z3* b1*d12 + z4*c13 + z4*(c1*b13 + c1*b1*d12) + z1*a3*add1*d12 + z4*add1*d12
income_blue_ebike_indirect := x1*a15 + x1*a1*d12 + x1*a2*b13 + x1*a2*b1*d12 + x1*a3*c13 + x1*a3*c1*b13 + x1*a3*c1*b1*d12 + x2*d12 + x3*b13 + x3* b1*d12 + x4*c13 + x4*(c1*b13 + c1*b1*d12) + x1*a3*add1*d12 + x4*add1*d12
income_blue_car_indirect :=  y1*a15 + y1*a1*d12 + y1*a2*b13 + y1*a2*b1*d12 + y1*a3*c13 + y1*a3*c1*b13 + y1*a3*c1*b1*d12 + y2*d12 + x3*b13 + y3* b1*d12 + y4*c13 + y4*(c1*b13 + c1*b1*d12) + y1*a3*add1*d12 + y4*add1*d12
income_blue_taxi_indirect := q1*a15 + q1*a1*d12 + q1*a2*b13 + q1*a2*b1*d12 + q1*a3*c13 + q1*a3*c1*b13 + q1*a3*c1*b1*d12 + q2*d12 + q3*b13 + q3* b1*d12 + q4*c13 + q4*(c1*b13 + c1*b1*d12) + q1*a3*add1*d12 + q4*add1*d12
income_blue_bus_indirect := w1*a15 + w1*a1*d12 + w1*a2*b13 + w1*a2*b1*d12 + w1*a3*c13 + w1*a3*c1*b13 + w1*a3*c1*b1*d12 + w2*d12 + w3*b13 + w3* b1*d12 + w4*c13 + w4*(c1*b13 + c1*b1*d12) + w1*a3*add1*d12 + w4*add1*d12
income_blue_subway_indirect := r1*a15 + r1*a1*d12 + r1*a2*b13 + r1*a2*b1*d12 + r1*a3*c13 + r1*a3*c1*b13 + r1*a3*c1*b1*d12 + r2*d12 + r3*b13 + r3* b1*d12 + r4*c13 + r4*(c1*b13 + c1*b1*d12) + r1*a3*add1*d12 + r4*add1*d12
income_blue_dTime_indirect := u1*a15 + u1*a1*d12 + u1*a2*b13 + u1*a2*b1*d12 + u1*a3*c13 + u1*a3*c1*b13 + u1*a3*c1*b1*d12 + u2*d12 + u3*b13 + u3* b1*d12 + u4*c13 + u4*(c1*b13 + c1*b1*d12) + u1*a3*add1*d12 + u4*add1*d12
income_blue_Stravel_indirect := p1*a15 + p1*a1*d12 + p1*a2*b13 + p1*a2*b1*d12 + p1*a3*c13 + p1*a3*c1*b13 + p1*a3*c1*b1*d12 + p2*d12 + p3*b13 + p3* b1*d12 + p4*c13 + p4*(c1*b13 + c1*b1*d12) + p1*a3*add1*d12 + p4*add1*d12
income_blue_Smode_indirect := l1*a15 + l1*a1*d12 + l1*a2*b13 + l1*a2*b1*d12 + l1*a3*c13 + l1*a3*c1*b13 + l1*a3*c1*b1*d12 + l2*d12 + l3*b13 + l3* b1*d12 + l4*c13 + l4*(c1*b13 + c1*b1*d12) + l1*a3*add1*d12 + l4*add1*d12
income_blue_SdTime_indirect := v1*a15 + v1*a1*d12 + v1*a2*b13 + v1*a2*b1*d12 + v1*a3*c13 + v1*a3*c1*b13 + v1*a3*c1*b1*d12 + v2*d12 + v3*b13 + v3* b1*d12 + v4*c13 + v4*(c1*b13 + c1*b1*d12) + v1*a3*add1*d12 + v4*add1*d12

#car_owner
car_owner_travel_direct:= e13
car_owner_ebike_direct := f13
car_owner_car_direct := g13
car_owner_taxi_direct := h13
car_owner_bus_direct := i13
car_owner_subway_direct := j13
car_owner_dTime_direct := k13
car_owner_Stravel_direct := m13
car_owner_Smode_direct := n13
car_owner_SdTime_direct := t13

car_owner_travel_indirect:= z1*a16 + z1*a1*d13 + z1*a2*b14 + z1*a2*b1*d13 + z1*a3*c14 + z1*a3*c1*b14 + z1*a3*c1*b1*d13 + z2*d13 + z3*b14 + z3* b1*d13 + z4*c14 + z4*(c1*b14 + c1*b1*d13) + z1*a3*add1*d13 + z4*add1*d13
car_owner_ebike_indirect := x1*a16 + x1*a1*d13 + x1*a2*b14 + x1*a2*b1*d13 + x1*a3*c14 + x1*a3*c1*b14 + x1*a3*c1*b1*d13 + x2*d13 + x3*b14 + x3* b1*d13 + x4*c14 + x4*(c1*b14 + c1*b1*d13) + x1*a3*add1*d13 + x4*add1*d13
car_owner_car_indirect :=  y1*a16 + y1*a1*d13 + y1*a2*b14 + y1*a2*b1*d13 + y1*a3*c14 + y1*a3*c1*b14 + y1*a3*c1*b1*d13 + y2*d13 + x3*b14 + y3* b1*d13 + y4*c14 + y4*(c1*b14 + c1*b1*d13) + y1*a3*add1*d13 + y4*add1*d13
car_owner_taxi_indirect := q1*a16 + q1*a1*d13 + q1*a2*b14 + q1*a2*b1*d13 + q1*a3*c14 + q1*a3*c1*b14 + q1*a3*c1*b1*d13 + q2*d13 + q3*b14 + q3* b1*d13 + q4*c14 + q4*(c1*b14 + c1*b1*d13) + q1*a3*add1*d13 + q4*add1*d13
car_owner_bus_indirect := w1*a16 + w1*a1*d13 + w1*a2*b14 + w1*a2*b1*d13 + w1*a3*c14 + w1*a3*c1*b14 + w1*a3*c1*b1*d13 + w2*d13 + w3*b14 + w3* b1*d13 + w4*c14 + w4*(c1*b14 + c1*b1*d13) + w1*a3*add1*d13 + w4*add1*d13
car_owner_subway_indirect := r1*a16 + r1*a1*d13 + r1*a2*b14 + r1*a2*b1*d13 + r1*a3*c14 + r1*a3*c1*b14 + r1*a3*c1*b1*d13 + r2*d13 + r3*b14 + r3* b1*d13 + r4*c14 + r4*(c1*b14 + c1*b1*d13) + r1*a3*add1*d13 + r4*add1*d13
car_owner_dTime_indirect := u1*a16 + u1*a1*d13 + u1*a2*b14 + u1*a2*b1*d13 + u1*a3*c14 + u1*a3*c1*b14 + u1*a3*c1*b1*d13 + u2*d13 + u3*b14 + u3* b1*d13 + u4*c14 + u4*(c1*b14 + c1*b1*d13) + u1*a3*add1*d13 + u4*add1*d13
car_owner_Stravel_indirect := p1*a16 + p1*a1*d13 + p1*a2*b14 + p1*a2*b1*d13 + p1*a3*c14 + p1*a3*c1*b14 + p1*a3*c1*b1*d13 + p2*d13 + p3*b14 + p3* b1*d13 + p4*c14 + p4*(c1*b14 + c1*b1*d13) + p1*a3*add1*d13 + p4*add1*d13
car_owner_Smode_indirect := l1*a16 + l1*a1*d13 + l1*a2*b14 + l1*a2*b1*d13 + l1*a3*c14 + l1*a3*c1*b14 + l1*a3*c1*b1*d13 + l2*d13 + l3*b14 + l3* b1*d13 + l4*c14 + l4*(c1*b14 + c1*b1*d13) + l1*a3*add1*d13 + l4*add1*d13
car_owner_SdTime_indirect := v1*a16 + v1*a1*d13 + v1*a2*b14 + v1*a2*b1*d13 + v1*a3*c14 + v1*a3*c1*b14 + v1*a3*c1*b1*d13 + v2*d13 + v3*b14 + v3* b1*d13 + v4*c14 + v4*(c1*b14 + c1*b1*d13) + v1*a3*add1*d13 + v4*add1*d13


#year_residence
year_residence_travel_direct:= e14
year_residence_ebike_direct := f14
year_residence_car_direct := g14
year_residence_taxi_direct := h14
year_residence_bus_direct := i14
year_residence_subway_direct := j14
year_residence_dTime_direct := k14
year_residence_Stravel_direct := m14
year_residence_Smode_direct := n14
year_residence_SdTime_direct := t14

year_residence_travel_indirect:= z1*a17 + z1*a1*d14 + z1*a2*b15 + z1*a2*b1*d14 + z1*a3*c15 + z1*a3*c1*b15 + z1*a3*c1*b1*d14 + z2*d14 + z3*b15 + z3* b1*d14 + z4*c15 + z4*(c1*b15 + c1*b1*d14) + z1*a3*add1*d14 + z4*add1*d14
year_residence_ebike_indirect := x1*a17 + x1*a1*d14 + x1*a2*b15 + x1*a2*b1*d14 + x1*a3*c15 + x1*a3*c1*b15 + x1*a3*c1*b1*d14 + x2*d14 + x3*b15 + x3* b1*d14 + x4*c15 + x4*(c1*b15 + c1*b1*d14) + x1*a3*add1*d14 + x4*add1*d14
year_residence_car_indirect :=  y1*a17 + y1*a1*d14 + y1*a2*b15 + y1*a2*b1*d14 + y1*a3*c15 + y1*a3*c1*b15 + y1*a3*c1*b1*d14 + y2*d14 + x3*b15 + y3* b1*d14 + y4*c15 + y4*(c1*b15 + c1*b1*d14) + y1*a3*add1*d14 + y4*add1*d14
year_residence_taxi_indirect := q1*a17 + q1*a1*d14 + q1*a2*b15 + q1*a2*b1*d14 + q1*a3*c15 + q1*a3*c1*b15 + q1*a3*c1*b1*d14 + q2*d14 + q3*b15 + q3* b1*d14 + q4*c15 + q4*(c1*b15 + c1*b1*d14) + q1*a3*add1*d14 + q4*add1*d14
year_residence_bus_indirect := w1*a17 + w1*a1*d14 + w1*a2*b15 + w1*a2*b1*d14 + w1*a3*c15 + w1*a3*c1*b15 + w1*a3*c1*b1*d14 + w2*d14 + w3*b15 + w3* b1*d14 + w4*c15 + w4*(c1*b15 + c1*b1*d14) + w1*a3*add1*d14 + w4*add1*d14
year_residence_subway_indirect := r1*a17 + r1*a1*d14 + r1*a2*b15 + r1*a2*b1*d14 + r1*a3*c15 + r1*a3*c1*b15 + r1*a3*c1*b1*d14 + r2*d14 + r3*b15 + r3* b1*d14 + r4*c15 + r4*(c1*b15 + c1*b1*d14) + r1*a3*add1*d14 + r4*add1*d14
year_residence_dTime_indirect := u1*a17 + u1*a1*d14 + u1*a2*b15 + u1*a2*b1*d14 + u1*a3*c15 + u1*a3*c1*b15 + u1*a3*c1*b1*d14 + u2*d14 + u3*b15 + u3* b1*d14 + u4*c15 + u4*(c1*b15 + c1*b1*d14) + u1*a3*add1*d14 + u4*add1*d14
year_residence_Stravel_indirect := p1*a17 + p1*a1*d14 + p1*a2*b15 + p1*a2*b1*d14 + p1*a3*c15 + p1*a3*c1*b15 + p1*a3*c1*b1*d14 + p2*d14 + p3*b15 + p3* b1*d14 + p4*c15 + p4*(c1*b15 + c1*b1*d14) + p1*a3*add1*d14 + p4*add1*d14
year_residence_Smode_indirect := l1*a17 + l1*a1*d14 + l1*a2*b15 + l1*a2*b1*d14 + l1*a3*c15 + l1*a3*c1*b15 + l1*a3*c1*b1*d14 + l2*d14 + l3*b15 + l3* b1*d14 + l4*c15 + l4*(c1*b15 + c1*b1*d14) + l1*a3*add1*d14 + l4*add1*d14
year_residence_SdTime_indirect := v1*a17 + v1*a1*d14 + v1*a2*b15 + v1*a2*b1*d14 + v1*a3*c15 + v1*a3*c1*b15 + v1*a3*c1*b1*d14 + v2*d14 + v3*b15 + v3* b1*d14 + v4*c15 + v4*(c1*b15 + c1*b1*d14) + v1*a3*add1*d14 + v4*add1*d14


#current_mode_地铁
current_mode_地铁_travel_direct:= e15
current_mode_地铁_ebike_direct := f15
current_mode_地铁_car_direct := g15
current_mode_地铁_taxi_direct := h15
current_mode_地铁_bus_direct := i15
current_mode_地铁_subway_direct := j15
current_mode_地铁_dTime_direct := k15
current_mode_地铁_Stravel_direct := m15
current_mode_地铁_Smode_direct := n15
current_mode_地铁_SdTime_direct := t15

current_mode_地铁_travel_indirect:= z1*a18 + z1*a1*d15 + z1*a2*b16 + z1*a2*b1*d15 + z1*a3*c16 + z1*a3*c1*b16 + z1*a3*c1*b1*d15 + z2*d15 + z3*b16 + z3* b1*d15 + z4*c16 + z4*(c1*b16 + c1*b1*d15) + z1*a3*add1*d15 + z4*add1*d15
current_mode_地铁_ebike_indirect := x1*a18 + x1*a1*d15 + x1*a2*b16 + x1*a2*b1*d15 + x1*a3*c16 + x1*a3*c1*b16 + x1*a3*c1*b1*d15 + x2*d15 + x3*b16 + x3* b1*d15 + x4*c16 + x4*(c1*b16 + c1*b1*d15) + x1*a3*add1*d15 + x4*add1*d15
current_mode_地铁_car_indirect :=  y1*a18 + y1*a1*d15 + y1*a2*b16 + y1*a2*b1*d15 + y1*a3*c16 + y1*a3*c1*b16 + y1*a3*c1*b1*d15 + y2*d15 + x3*b16 + y3* b1*d15 + y4*c16 + y4*(c1*b16 + c1*b1*d15) + y1*a3*add1*d15 + y4*add1*d15
current_mode_地铁_taxi_indirect := q1*a18 + q1*a1*d15 + q1*a2*b16 + q1*a2*b1*d15 + q1*a3*c16 + q1*a3*c1*b16 + q1*a3*c1*b1*d15 + q2*d15 + q3*b16 + q3* b1*d15 + q4*c16 + q4*(c1*b16 + c1*b1*d15) + q1*a3*add1*d15 + q4*add1*d15
current_mode_地铁_bus_indirect := w1*a18 + w1*a1*d15 + w1*a2*b16 + w1*a2*b1*d15 + w1*a3*c16 + w1*a3*c1*b16 + w1*a3*c1*b1*d15 + w2*d15 + w3*b16 + w3* b1*d15 + w4*c16 + w4*(c1*b16 + c1*b1*d15) + w1*a3*add1*d15 + w4*add1*d15
current_mode_地铁_subway_indirect := r1*a18 + r1*a1*d15 + r1*a2*b16 + r1*a2*b1*d15 + r1*a3*c16 + r1*a3*c1*b16 + r1*a3*c1*b1*d15 + r2*d15 + r3*b16 + r3* b1*d15 + r4*c16 + r4*(c1*b16 + c1*b1*d15) + r1*a3*add1*d15 + r4*add1*d15
current_mode_地铁_dTime_indirect := u1*a18 + u1*a1*d15 + u1*a2*b16 + u1*a2*b1*d15 + u1*a3*c16 + u1*a3*c1*b16 + u1*a3*c1*b1*d15 + u2*d15 + u3*b16 + u3* b1*d15 + u4*c16 + u4*(c1*b16 + c1*b1*d15) + u1*a3*add1*d15 + u4*add1*d15
current_mode_地铁_Stravel_indirect := p1*a18 + p1*a1*d15 + p1*a2*b16 + p1*a2*b1*d15 + p1*a3*c16 + p1*a3*c1*b16 + p1*a3*c1*b1*d15 + p2*d15 + p3*b16 + p3* b1*d15 + p4*c16 + p4*(c1*b16 + c1*b1*d15) + p1*a3*add1*d15 + p4*add1*d15
current_mode_地铁_Smode_indirect := l1*a18 + l1*a1*d15 + l1*a2*b16 + l1*a2*b1*d15 + l1*a3*c16 + l1*a3*c1*b16 + l1*a3*c1*b1*d15 + l2*d15 + l3*b16 + l3* b1*d15 + l4*c16 + l4*(c1*b16 + c1*b1*d15) + l1*a3*add1*d15 + l4*add1*d15
current_mode_地铁_SdTime_indirect := v1*a18 + v1*a1*d15 + v1*a2*b16 + v1*a2*b1*d15 + v1*a3*c16 + v1*a3*c1*b16 + v1*a3*c1*b1*d15 + v2*d15 + v3*b16 + v3* b1*d15 + v4*c16 + v4*(c1*b16 + c1*b1*d15) + v1*a3*add1*d15 + v4*add1*d15


#current_mode_地面公交
current_mode_地面公交_travel_direct:= e16
current_mode_地面公交_ebike_direct := f16
current_mode_地面公交_car_direct := g16
current_mode_地面公交_taxi_direct := h16
current_mode_地面公交_bus_direct := i16
current_mode_地面公交_subway_direct := j16
current_mode_地面公交_dTime_direct := k16
current_mode_地面公交_Stravel_direct := m16
current_mode_地面公交_Smode_direct := n16
current_mode_地面公交_SdTime_direct := t16

current_mode_地面公交_travel_indirect:= z1*a19 + z1*a1*d16 + z1*a2*b17 + z1*a2*b1*d16 + z1*a3*c17 + z1*a3*c1*b17 + z1*a3*c1*b1*d16 + z2*d16 + z3*b17 + z3* b1*d16 + z4*c17 + z4*(c1*b17 + c1*b1*d16) + z1*a3*add1*d16 + z4*add1*d16
current_mode_地面公交_ebike_indirect := x1*a19 + x1*a1*d16 + x1*a2*b17 + x1*a2*b1*d16 + x1*a3*c17 + x1*a3*c1*b17 + x1*a3*c1*b1*d16 + x2*d16 + x3*b17 + x3* b1*d16 + x4*c17 + x4*(c1*b17 + c1*b1*d16) + x1*a3*add1*d16 + x4*add1*d16
current_mode_地面公交_car_indirect :=  y1*a19 + y1*a1*d16 + y1*a2*b17 + y1*a2*b1*d16 + y1*a3*c17 + y1*a3*c1*b17 + y1*a3*c1*b1*d16 + y2*d16 + x3*b17 + y3* b1*d16 + y4*c17 + y4*(c1*b17 + c1*b1*d16) + y1*a3*add1*d16 + y4*add1*d16
current_mode_地面公交_taxi_indirect := q1*a19 + q1*a1*d16 + q1*a2*b17 + q1*a2*b1*d16 + q1*a3*c17 + q1*a3*c1*b17 + q1*a3*c1*b1*d16 + q2*d16 + q3*b17 + q3* b1*d16 + q4*c17 + q4*(c1*b17 + c1*b1*d16) + q1*a3*add1*d16 + q4*add1*d16
current_mode_地面公交_bus_indirect := w1*a19 + w1*a1*d16 + w1*a2*b17 + w1*a2*b1*d16 + w1*a3*c17 + w1*a3*c1*b17 + w1*a3*c1*b1*d16 + w2*d16 + w3*b17 + w3* b1*d16 + w4*c17 + w4*(c1*b17 + c1*b1*d16) + w1*a3*add1*d16 + w4*add1*d16
current_mode_地面公交_subway_indirect := r1*a19 + r1*a1*d16 + r1*a2*b17 + r1*a2*b1*d16 + r1*a3*c17 + r1*a3*c1*b17 + r1*a3*c1*b1*d16 + r2*d16 + r3*b17 + r3* b1*d16 + r4*c17 + r4*(c1*b17 + c1*b1*d16) + r1*a3*add1*d16 + r4*add1*d16
current_mode_地面公交_dTime_indirect := u1*a19 + u1*a1*d16 + u1*a2*b17 + u1*a2*b1*d16 + u1*a3*c17 + u1*a3*c1*b17 + u1*a3*c1*b1*d16 + u2*d16 + u3*b17 + u3* b1*d16 + u4*c17 + u4*(c1*b17 + c1*b1*d16) + u1*a3*add1*d16 + u4*add1*d16
current_mode_地面公交_Stravel_indirect := p1*a19 + p1*a1*d16 + p1*a2*b17 + p1*a2*b1*d16 + p1*a3*c17 + p1*a3*c1*b17 + p1*a3*c1*b1*d16 + p2*d16 + p3*b17 + p3* b1*d16 + p4*c17 + p4*(c1*b17 + c1*b1*d16) + p1*a3*add1*d16 + p4*add1*d16
current_mode_地面公交_Smode_indirect := l1*a19 + l1*a1*d16 + l1*a2*b17 + l1*a2*b1*d16 + l1*a3*c17 + l1*a3*c1*b17 + l1*a3*c1*b1*d16 + l2*d16 + l3*b17 + l3* b1*d16 + l4*c17 + l4*(c1*b17 + c1*b1*d16) + l1*a3*add1*d16 + l4*add1*d16
current_mode_地面公交_SdTime_indirect := v1*a19 + v1*a1*d16 + v1*a2*b17 + v1*a2*b1*d16 + v1*a3*c17 + v1*a3*c1*b17 + v1*a3*c1*b1*d16 + v2*d16 + v3*b17 + v3* b1*d16 + v4*c17 + v4*(c1*b17 + c1*b1*d16) + v1*a3*add1*d16 + v4*add1*d16


#current_mode_电动自行车
current_mode_电动自行车_travel_direct:= e17
current_mode_电动自行车_ebike_direct := f17
current_mode_电动自行车_car_direct := g17
current_mode_电动自行车_taxi_direct := h17
current_mode_电动自行车_bus_direct := i17
current_mode_电动自行车_subway_direct := j17
current_mode_电动自行车_dTime_direct := k17
current_mode_电动自行车_Stravel_direct := m17
current_mode_电动自行车_Smode_direct := n17
current_mode_电动自行车_SdTime_direct := t17

current_mode_电动自行车_travel_indirect:= z1*a20 + z1*a1*d17 + z1*a2*b18 + z1*a2*b1*d17 + z1*a3*c18 + z1*a3*c1*b18 + z1*a3*c1*b1*d17 + z2*d17 + z3*b18 + z3* b1*d17 + z4*c18 + z4*(c1*b18 + c1*b1*d17) + z1*a3*add1*d17 + z4*add1*d17
current_mode_电动自行车_ebike_indirect := x1*a20 + x1*a1*d17 + x1*a2*b18 + x1*a2*b1*d17 + x1*a3*c18 + x1*a3*c1*b18 + x1*a3*c1*b1*d17 + x2*d17 + x3*b18 + x3* b1*d17 + x4*c18 + x4*(c1*b18 + c1*b1*d17) + x1*a3*add1*d17 + x4*add1*d17
current_mode_电动自行车_car_indirect :=  y1*a20 + y1*a1*d17 + y1*a2*b18 + y1*a2*b1*d17 + y1*a3*c18 + y1*a3*c1*b18 + y1*a3*c1*b1*d17 + y2*d17 + x3*b18 + y3* b1*d17 + y4*c18 + y4*(c1*b18 + c1*b1*d17) + y1*a3*add1*d17 + y4*add1*d17
current_mode_电动自行车_taxi_indirect := q1*a20 + q1*a1*d17 + q1*a2*b18 + q1*a2*b1*d17 + q1*a3*c18 + q1*a3*c1*b18 + q1*a3*c1*b1*d17 + q2*d17 + q3*b18 + q3* b1*d17 + q4*c18 + q4*(c1*b18 + c1*b1*d17) + q1*a3*add1*d17 + q4*add1*d17
current_mode_电动自行车_bus_indirect := w1*a20 + w1*a1*d17 + w1*a2*b18 + w1*a2*b1*d17 + w1*a3*c18 + w1*a3*c1*b18 + w1*a3*c1*b1*d17 + w2*d17 + w3*b18 + w3* b1*d17 + w4*c18 + w4*(c1*b18 + c1*b1*d17) + w1*a3*add1*d17 + w4*add1*d17
current_mode_电动自行车_subway_indirect := r1*a20 + r1*a1*d17 + r1*a2*b18 + r1*a2*b1*d17 + r1*a3*c18 + r1*a3*c1*b18 + r1*a3*c1*b1*d17 + r2*d17 + r3*b18 + r3* b1*d17 + r4*c18 + r4*(c1*b18 + c1*b1*d17) + r1*a3*add1*d17 + r4*add1*d17
current_mode_电动自行车_dTime_indirect := u1*a20 + u1*a1*d17 + u1*a2*b18 + u1*a2*b1*d17 + u1*a3*c18 + u1*a3*c1*b18 + u1*a3*c1*b1*d17 + u2*d17 + u3*b18 + u3* b1*d17 + u4*c18 + u4*(c1*b18 + c1*b1*d17) + u1*a3*add1*d17 + u4*add1*d17
current_mode_电动自行车_Stravel_indirect := p1*a20 + p1*a1*d17 + p1*a2*b18 + p1*a2*b1*d17 + p1*a3*c18 + p1*a3*c1*b18 + p1*a3*c1*b1*d17 + p2*d17 + p3*b18 + p3* b1*d17 + p4*c18 + p4*(c1*b18 + c1*b1*d17) + p1*a3*add1*d17 + p4*add1*d17
current_mode_电动自行车_Smode_indirect := l1*a20 + l1*a1*d17 + l1*a2*b18 + l1*a2*b1*d17 + l1*a3*c18 + l1*a3*c1*b18 + l1*a3*c1*b1*d17 + l2*d17 + l3*b18 + l3* b1*d17 + l4*c18 + l4*(c1*b18 + c1*b1*d17) + l1*a3*add1*d17 + l4*add1*d17
current_mode_电动自行车_SdTime_indirect := v1*a20 + v1*a1*d17 + v1*a2*b18 + v1*a2*b1*d17 + v1*a3*c18 + v1*a3*c1*b18 + v1*a3*c1*b1*d17 + v2*d17 + v3*b18 + v3* b1*d17 + v4*c18 + v4*(c1*b18 + c1*b1*d17) + v1*a3*add1*d17 + v4*add1*d17


#waether_source_交通导航软件
waether_source_交通导航软件_travel_direct:= e21
waether_source_交通导航软件_ebike_direct := f21
waether_source_交通导航软件_car_direct := g21
waether_source_交通导航软件_taxi_direct := h21
waether_source_交通导航软件_bus_direct := i21
waether_source_交通导航软件_subway_direct := j21
waether_source_交通导航软件_dTime_direct := k21
waether_source_交通导航软件_Stravel_direct := m21
waether_source_交通导航软件_Smode_direct := n21
waether_source_交通导航软件_SdTime_direct := t21

waether_source_交通导航软件_travel_indirect:= z1*a24 + z1*a1*d21 + z1*a2*b22 + z1*a2*b1*d21 + z1*a3*c22 + z1*a3*c1*b22 + z1*a3*c1*b1*d21 + z2*d21 + z3*b22 + z3* b1*d21 + z4*c22 + z4*(c1*b22 + c1*b1*d21) + z1*a3*add1*d21 + z4*add1*d21
waether_source_交通导航软件_ebike_indirect := x1*a24 + x1*a1*d21 + x1*a2*b22 + x1*a2*b1*d21 + x1*a3*c22 + x1*a3*c1*b22 + x1*a3*c1*b1*d21 + x2*d21 + x3*b22 + x3* b1*d21 + x4*c22 + x4*(c1*b22 + c1*b1*d21) + x1*a3*add1*d21 + x4*add1*d21
waether_source_交通导航软件_car_indirect :=  y1*a24 + y1*a1*d21 + y1*a2*b22 + y1*a2*b1*d21 + y1*a3*c22 + y1*a3*c1*b22 + y1*a3*c1*b1*d21 + y2*d21 + x3*b22 + y3* b1*d21 + y4*c22 + y4*(c1*b22 + c1*b1*d21) + y1*a3*add1*d21 + y4*add1*d21
waether_source_交通导航软件_taxi_indirect := q1*a24 + q1*a1*d21 + q1*a2*b22 + q1*a2*b1*d21 + q1*a3*c22 + q1*a3*c1*b22 + q1*a3*c1*b1*d21 + q2*d21 + q3*b22 + q3* b1*d21 + q4*c22 + q4*(c1*b22 + c1*b1*d21) + q1*a3*add1*d21 + q4*add1*d21
waether_source_交通导航软件_bus_indirect := w1*a24 + w1*a1*d21 + w1*a2*b22 + w1*a2*b1*d21 + w1*a3*c22 + w1*a3*c1*b22 + w1*a3*c1*b1*d21 + w2*d21 + w3*b22 + w3* b1*d21 + w4*c22 + w4*(c1*b22 + c1*b1*d21) + w1*a3*add1*d21 + w4*add1*d21
waether_source_交通导航软件_subway_indirect := r1*a24 + r1*a1*d21 + r1*a2*b22 + r1*a2*b1*d21 + r1*a3*c22 + r1*a3*c1*b22 + r1*a3*c1*b1*d21 + r2*d21 + r3*b22 + r3* b1*d21 + r4*c22 + r4*(c1*b22 + c1*b1*d21) + r1*a3*add1*d21 + r4*add1*d21
waether_source_交通导航软件_dTime_indirect := u1*a24 + u1*a1*d21 + u1*a2*b22 + u1*a2*b1*d21 + u1*a3*c22 + u1*a3*c1*b22 + u1*a3*c1*b1*d21 + u2*d21 + u3*b22 + u3* b1*d21 + u4*c22 + u4*(c1*b22 + c1*b1*d21) + u1*a3*add1*d21 + u4*add1*d21
waether_source_交通导航软件_Stravel_indirect := p1*a24 + p1*a1*d21 + p1*a2*b22 + p1*a2*b1*d21 + p1*a3*c22 + p1*a3*c1*b22 + p1*a3*c1*b1*d21 + p2*d21 + p3*b22 + p3* b1*d21 + p4*c22 + p4*(c1*b22 + c1*b1*d21) + p1*a3*add1*d21 + p4*add1*d21
waether_source_交通导航软件_Smode_indirect := l1*a24 + l1*a1*d21 + l1*a2*b22 + l1*a2*b1*d21 + l1*a3*c22 + l1*a3*c1*b22 + l1*a3*c1*b1*d21 + l2*d21 + l3*b22 + l3* b1*d21 + l4*c22 + l4*(c1*b22 + c1*b1*d21) + l1*a3*add1*d21 + l4*add1*d21
waether_source_交通导航软件_SdTime_indirect := v1*a24 + v1*a1*d21 + v1*a2*b22 + v1*a2*b1*d21 + v1*a3*c22 + v1*a3*c1*b22 + v1*a3*c1*b1*d21 + v2*d21 + v3*b22 + v3* b1*d21 + v4*c22 + v4*(c1*b22 + c1*b1*d21) + v1*a3*add1*d21 + v4*add1*d21


#waether_source_天气预报软件
waether_source_其他途径_travel_direct:= e22
waether_source_其他途径_ebike_direct := f22
waether_source_其他途径_car_direct := g22
waether_source_其他途径_taxi_direct := h22
waether_source_其他途径_bus_direct := i22
waether_source_其他途径_subway_direct := j22
waether_source_其他途径_dTime_direct := k22
waether_source_其他途径_Stravel_direct := m22
waether_source_其他途径_Smode_direct := n22
waether_source_其他途径_SdTime_direct := t22

waether_source_其他途径_travel_indirect:= z1*a25 + z1*a1*d22 + z1*a2*b23 + z1*a2*b1*d22 + z1*a3*c23 + z1*a3*c1*b23 + z1*a3*c1*b1*d22 + z2*d22 + z3*b23 + z3* b1*d22 + z4*c23 + z4*(c1*b23 + c1*b1*d22) + z1*a3*add1*d22 + z4*add1*d22
waether_source_其他途径_ebike_indirect := x1*a25 + x1*a1*d22 + x1*a2*b23 + x1*a2*b1*d22 + x1*a3*c23 + x1*a3*c1*b23 + x1*a3*c1*b1*d22 + x2*d22 + x3*b23 + x3* b1*d22 + x4*c23 + x4*(c1*b23 + c1*b1*d22) + x1*a3*add1*d22 + x4*add1*d22
waether_source_其他途径_car_indirect :=  y1*a25 + y1*a1*d22 + y1*a2*b23 + y1*a2*b1*d22 + y1*a3*c23 + y1*a3*c1*b23 + y1*a3*c1*b1*d22 + y2*d22 + x3*b23 + y3* b1*d22 + y4*c23 + y4*(c1*b23 + c1*b1*d22) + y1*a3*add1*d22 + y4*add1*d22
waether_source_其他途径_taxi_indirect := q1*a25 + q1*a1*d22 + q1*a2*b23 + q1*a2*b1*d22 + q1*a3*c23 + q1*a3*c1*b23 + q1*a3*c1*b1*d22 + q2*d22 + q3*b23 + q3* b1*d22 + q4*c23 + q4*(c1*b23 + c1*b1*d22) + q1*a3*add1*d22 + q4*add1*d22
waether_source_其他途径_bus_indirect := w1*a25 + w1*a1*d22 + w1*a2*b23 + w1*a2*b1*d22 + w1*a3*c23 + w1*a3*c1*b23 + w1*a3*c1*b1*d22 + w2*d22 + w3*b23 + w3* b1*d22 + w4*c23 + w4*(c1*b23 + c1*b1*d22) + w1*a3*add1*d22 + w4*add1*d22
waether_source_其他途径件_subway_indirect := r1*a25 + r1*a1*d22 + r1*a2*b23 + r1*a2*b1*d22 + r1*a3*c23 + r1*a3*c1*b23 + r1*a3*c1*b1*d22 + r2*d22 + r3*b23 + r3* b1*d22 + r4*c23 + r4*(c1*b23 + c1*b1*d22) + r1*a3*add1*d22 + r4*add1*d22
waether_source_其他途径_dTime_indirect := u1*a25 + u1*a1*d22 + u1*a2*b23 + u1*a2*b1*d22 + u1*a3*c23 + u1*a3*c1*b23 + u1*a3*c1*b1*d22 + u2*d22 + u3*b23 + u3* b1*d22 + u4*c23 + u4*(c1*b23 + c1*b1*d22) + u1*a3*add1*d22 + u4*add1*d22
waether_source_其他途径_Stravel_indirect := p1*a25 + p1*a1*d22 + p1*a2*b23 + p1*a2*b1*d22 + p1*a3*c23 + p1*a3*c1*b23 + p1*a3*c1*b1*d22 + p2*d22 + p3*b23 + p3* b1*d22 + p4*c23 + p4*(c1*b23 + c1*b1*d22) + p1*a3*add1*d22 + p4*add1*d22
waether_source_其他途径_Smode_indirect := l1*a25 + l1*a1*d22 + l1*a2*b23 + l1*a2*b1*d22 + l1*a3*c23 + l1*a3*c1*b23 + l1*a3*c1*b1*d22 + l2*d22 + l3*b23 + l3* b1*d22 + l4*c23 + l4*(c1*b23 + c1*b1*d22) + l1*a3*add1*d22 + l4*add1*d22
waether_source_其他途径_SdTime_indirect := v1*a25 + v1*a1*d22 + v1*a2*b23 + v1*a2*b1*d22 + v1*a3*c23 + v1*a3*c1*b23 + v1*a3*c1*b1*d22 + v2*d22 + v3*b23 + v3* b1*d22 + v4*c23 + v4*(c1*b23 + c1*b1*d22) + v1*a3*add1*d22 + v4*add1*d22



#education_本科以上
education_本科以上_travel_direct:= e23
education_本科以上_ebike_direct := f23
education_本科以上_car_direct := g23
education_本科以上_taxi_direct := h23
education_本科以上_bus_direct := i23
education_本科以上_subway_direct := j23
education_本科以上_dTime_direct := k23
education_本科以上_Stravel_direct := m23
education_本科以上_Smode_direct := n23
education_本科以上_SdTime_direct := t23

education_本科以上_travel_indirect:= z1*a26 + z1*a1*d23 + z1*a2*b24 + z1*a2*b1*d23 + z1*a3*c24 + z1*a3*c1*b24 + z1*a3*c1*b1*d23 + z2*d23 + z3*b24 + z3* b1*d23 + z4*c24 + z4*(c1*b24 + c1*b1*d23) + z1*a3*add1*d23 + z4*add1*d23
education_本科以上_ebike_indirect := x1*a26 + x1*a1*d23 + x1*a2*b24 + x1*a2*b1*d23 + x1*a3*c24 + x1*a3*c1*b24 + x1*a3*c1*b1*d23 + x2*d23 + x3*b24 + x3* b1*d23 + x4*c24 + x4*(c1*b24 + c1*b1*d23) + x1*a3*add1*d23 + x4*add1*d23
education_本科以上_car_indirect :=  y1*a26 + y1*a1*d23 + y1*a2*b24 + y1*a2*b1*d23 + y1*a3*c24 + y1*a3*c1*b24 + y1*a3*c1*b1*d23 + y2*d23 + x3*b24 + y3* b1*d23 + y4*c24 + y4*(c1*b24 + c1*b1*d23) + y1*a3*add1*d23 + y4*add1*d23
education_本科以上_taxi_indirect := q1*a26 + q1*a1*d23 + q1*a2*b24 + q1*a2*b1*d23 + q1*a3*c24 + q1*a3*c1*b24 + q1*a3*c1*b1*d23 + q2*d23 + q3*b24 + q3* b1*d23 + q4*c24 + q4*(c1*b24 + c1*b1*d23) + q1*a3*add1*d23 + q4*add1*d23
education_本科以上_bus_indirect := w1*a26 + w1*a1*d23 + w1*a2*b24 + w1*a2*b1*d23 + w1*a3*c24 + w1*a3*c1*b24 + w1*a3*c1*b1*d23 + w2*d23 + w3*b24 + w3* b1*d23 + w4*c24 + w4*(c1*b24 + c1*b1*d23) + w1*a3*add1*d23 + w4*add1*d23
education_本科以上_subway_indirect := r1*a26 + r1*a1*d23 + r1*a2*b24 + r1*a2*b1*d23 + r1*a3*c24 + r1*a3*c1*b24 + r1*a3*c1*b1*d23 + r2*d23 + r3*b24 + r3* b1*d23 + r4*c24 + r4*(c1*b24 + c1*b1*d23) + r1*a3*add1*d23 + r4*add1*d23
education_本科以上_dTime_indirect := u1*a26 + u1*a1*d23 + u1*a2*b24 + u1*a2*b1*d23 + u1*a3*c24 + u1*a3*c1*b24 + u1*a3*c1*b1*d23 + u2*d23 + u3*b24 + u3* b1*d23 + u4*c24 + u4*(c1*b24 + c1*b1*d23) + u1*a3*add1*d23 + u4*add1*d23
education_本科以上_Stravel_indirect := p1*a26 + p1*a1*d23 + p1*a2*b24 + p1*a2*b1*d23 + p1*a3*c24 + p1*a3*c1*b24 + p1*a3*c1*b1*d23 + p2*d23 + p3*b24 + p3* b1*d23 + p4*c24 + p4*(c1*b24 + c1*b1*d23) + p1*a3*add1*d23 + p4*add1*d23
education_本科以上_Smode_indirect := l1*a26 + l1*a1*d23 + l1*a2*b24 + l1*a2*b1*d23 + l1*a3*c24 + l1*a3*c1*b24 + l1*a3*c1*b1*d23 + l2*d23 + l3*b24 + l3* b1*d23 + l4*c24 + l4*(c1*b24 + c1*b1*d23) + l1*a3*add1*d23 + l4*add1*d23
education_本科以上_SdTime_indirect := v1*a26 + v1*a1*d23 + v1*a2*b24 + v1*a2*b1*d23 + v1*a3*c24 + v1*a3*c1*b24 + v1*a3*c1*b1*d23 + v2*d23 + v3*b24 + v3* b1*d23 + v4*c24 + v4*(c1*b24 + c1*b1*d23) + v1*a3*add1*d23 + v4*add1*d23



#job_私企外企
job_私企外企_travel_direct:= e24
job_私企外企_ebike_direct := f24
job_私企外企_car_direct := g24
job_私企外企_taxi_direct := h24
job_私企外企_bus_direct := i24
job_私企外企_subway_direct := j24
job_私企外企_dTime_direct := k24
job_私企外企_Stravel_direct := m24
job_私企外企_Smode_direct := n24
job_私企外企_SdTime_direct := t24

job_私企外企_travel_indirect:= z1*a27 + z1*a1*d24 + z1*a2*b25 + z1*a2*b1*d24 + z1*a3*c25 + z1*a3*c1*b25 + z1*a3*c1*b1*d24 + z2*d24 + z3*b25 + z3* b1*d24 + z4*c25 + z4*(c1*b25 + c1*b1*d24) + z1*a3*add1*d24 + z4*add1*d24
job_私企外企_ebike_indirect := x1*a27 + x1*a1*d24 + x1*a2*b25 + x1*a2*b1*d24 + x1*a3*c25 + x1*a3*c1*b25 + x1*a3*c1*b1*d24 + x2*d24 + x3*b25 + x3* b1*d24 + x4*c25 + x4*(c1*b25 + c1*b1*d24) + x1*a3*add1*d24 + z4*add1*d24
job_私企外企_car_indirect :=  y1*a27 + y1*a1*d24 + y1*a2*b25 + y1*a2*b1*d24 + y1*a3*c25 + y1*a3*c1*b25 + y1*a3*c1*b1*d24 + y2*d24 + z3*b25 + y3* b1*d24 + y4*c25 + y4*(c1*b25 + c1*b1*d24) + y1*a3*add1*d24 + y4*add1*d24
job_私企外企_taxi_indirect := q1*a27 + q1*a1*d24 + q1*a2*b25 + q1*a2*b1*d24 + q1*a3*c25 + q1*a3*c1*b25 + q1*a3*c1*b1*d24 + q2*d24 + q3*b25 + q3* b1*d24 + q4*c25 + q4*(c1*b25 + c1*b1*d24) + q1*a3*add1*d24 + q4*add1*d24
job_私企外企_bus_indirect := w1*a27 + w1*a1*d24 + w1*a2*b25 + w1*a2*b1*d24 + w1*a3*c25 + w1*a3*c1*b25 + w1*a3*c1*b1*d24 + w2*d24 + z3*b25 + w3* b1*d24 + w4*c25 + w4*(c1*b25 + c1*b1*d24) + w1*a3*add1*d24 + w4*add1*d24
job_私企外企_subway_indirect := r1*a27 + r1*a1*d24 + r1*a2*b25 + r1*a2*b1*d24 + r1*a3*c25 + r1*a3*c1*b25 + r1*a3*c1*b1*d24 + r2*d24 + r3*b25 + r3* b1*d24 + r4*c25 + r4*(c1*b25 + c1*b1*d24) + r1*a3*add1*d24 + r4*add1*d24
job_私企外企_dTime_indirect := u1*a27 + u1*a1*d24 + u1*a2*b25 + u1*a2*b1*d24 + u1*a3*c25 + u1*a3*c1*b25 + u1*a3*c1*b1*d24 + u2*d24 + u3*b25 + u3* b1*d24 + u4*c25 + u4*(c1*b25 + c1*b1*d24) + u1*a3*add1*d24 + u4*add1*d24
job_私企外企_Stravel_indirect := p1*a27 + p1*a1*d24 + p1*a2*b25 + p1*a2*b1*d24 + p1*a3*c25 + p1*a3*c1*b25 + p1*a3*c1*b1*d24 + p2*d24 + p3*b25 + p3* b1*d24 + p4*c25 + p4*(c1*b25 + c1*b1*d24) + p1*a3*add1*d24 + p4*add1*d24
job_私企外企_Smode_indirect := l1*a27 + l1*a1*d24 + l1*a2*b25 + l1*a2*b1*d24 + l1*a3*c25 + l1*a3*c1*b25 + l1*a3*c1*b1*d24 + l2*d24 + l3*b25 + l3* b1*d24 + l4*c25 + l4*(c1*b25 + c1*b1*d24) + l1*a3*add1*d24 + l4*add1*d24
job_私企外企_SdTime_indirect := v1*a27 + v1*a1*d24 + v1*a2*b25 + v1*a2*b1*d24 + v1*a3*c25 + v1*a3*c1*b25 + v1*a3*c1*b1*d24 + v2*d24 + v3*b25 + v3* b1*d24 + v4*c25 + v4*(c1*b25 + c1*b1*d24) + v1*a3*add1*d24 + v4*add1*d24


#job_其他
job_其他_travel_direct:= e25
job_其他_ebike_direct := f25
job_其他_car_direct := g25
job_其他_taxi_direct := h25
job_其他_bus_direct := i25
job_其他_subway_direct := j25
job_其他_dTime_direct := k25
job_其他_Stravel_direct := m25
job_其他_Smode_direct := n25
job_其他_SdTime_direct := t25

job_其他_travel_indirect:= z1*a28 + z1*a1*d25 + z1*a2*b26 + z1*a2*b1*d25 + z1*a3*c26 + z1*a3*c1*b26 + z1*a3*c1*b1*d25 + z2*d25 + z3*b26 + z3* b1*d25 + z4*c26 + z4*(c1*b26 + c1*b1*d25) + z1*a3*add1*d25 + z4*add1*d25
job_其他_ebike_indirect := x1*a28 + x1*a1*d25 + x1*a2*b26 + x1*a2*b1*d25 + x1*a3*c26 + x1*a3*c1*b26 + x1*a3*c1*b1*d25 + x2*d25 + x3*b26 + x3* b1*d25 + x4*c26 + x4*(c1*b26 + c1*b1*d25) + x1*a3*add1*d25 + z4*add1*d25
job_其他_car_indirect :=  y1*a28 + y1*a1*d25 + y1*a2*b26 + y1*a2*b1*d25 + y1*a3*c26 + y1*a3*c1*b26 + y1*a3*c1*b1*d25 + y2*d25 + z3*b26 + y3* b1*d25 + y4*c26 + y4*(c1*b26 + c1*b1*d25) + y1*a3*add1*d25 + y4*add1*d25
job_其他_taxi_indirect := q1*a28 + q1*a1*d25 + q1*a2*b26 + q1*a2*b1*d25 + q1*a3*c26 + q1*a3*c1*b26 + q1*a3*c1*b1*d25 + q2*d25 + q3*b26 + q3* b1*d25 + q4*c26 + q4*(c1*b26 + c1*b1*d25) + q1*a3*add1*d25 + q4*add1*d25
job_其他_bus_indirect := w1*a28 + w1*a1*d25 + w1*a2*b26 + w1*a2*b1*d25 + w1*a3*c26 + w1*a3*c1*b26 + w1*a3*c1*b1*d25 + w2*d25 + z3*b26 + w3* b1*d25 + w4*c26 + w4*(c1*b26 + c1*b1*d25) + w1*a3*add1*d25 + w4*add1*d25
job_其他_subway_indirect := r1*a28 + r1*a1*d25 + r1*a2*b26 + r1*a2*b1*d25 + r1*a3*c26 + r1*a3*c1*b26 + r1*a3*c1*b1*d25 + r2*d25 + r3*b26 + r3* b1*d25 + r4*c26 + r4*(c1*b26 + c1*b1*d25) + r1*a3*add1*d25 + r4*add1*d25
job_其他_dTime_indirect := u1*a28 + u1*a1*d25 + u1*a2*b26 + u1*a2*b1*d25 + u1*a3*c26 + u1*a3*c1*b26 + u1*a3*c1*b1*d25 + u2*d25 + u3*b26 + u3* b1*d25 + u4*c26 + u4*(c1*b26 + c1*b1*d25) + u1*a3*add1*d25 + u4*add1*d25
job_其他_Stravel_indirect := p1*a28 + p1*a1*d25 + p1*a2*b26 + p1*a2*b1*d25 + p1*a3*c26 + p1*a3*c1*b26 + p1*a3*c1*b1*d25 + p2*d25 + p3*b26 + p3* b1*d25 + p4*c26 + p4*(c1*b26 + c1*b1*d25) + p1*a3*add1*d25 + p4*add1*d25
job_其他_Smode_indirect := l1*a28 + l1*a1*d25 + l1*a2*b26 + l1*a2*b1*d25 + l1*a3*c26 + l1*a3*c1*b26 + l1*a3*c1*b1*d25 + l2*d25 + l3*b26 + l3* b1*d25 + l4*c26 + l4*(c1*b26 + c1*b1*d25) + l1*a3*add1*d25 + l4*add1*d25
job_其他_SdTime_indirect := v1*a28 + v1*a1*d25 + v1*a2*b26 + v1*a2*b1*d25 + v1*a3*c26 + v1*a3*c1*b26 + v1*a3*c1*b1*d25 + v2*d25 + v3*b26 + v3* b1*d25 + v4*c26 + v4*(c1*b26 + c1*b1*d25) + v1*a3*add1*d25 + v4*add1*d25


'




#==================================================================================fit

options(max.print=1000000)

fit6a <- sem(m6a, data=data3,representation = 'RAM',std.lv=TRUE)
sink('/Users/jjy/Desktop/2_2_test_ok_export_dataframe_yellow.txt')
summary(fit6a, standardized=TRUE, fit.measures=TRUE)
sink()






#=========================modindices
modindices(fit6a, sort = TRUE, maximum.number = 50)















