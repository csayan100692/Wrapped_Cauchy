


PBTWRPCAUCHYCP2=function(x,alpha)
{
  library(circular)
  library(CircStats)
  library(doParallel)
  library(foreach)
  library(parallel)
  registerDoParallel(3)
  options(cores=3)
  H=1000
  n1=length(x)
  # y1=numeric(0)
  z=numeric(0)
  # y=numeric(0)
  # z1=numeric(0)
  # bar_nu=numeric(0)
  bar1_nu=numeric(0)

  # m1=numeric(0)
  m11=numeric(0)
  # mh1=numeric(0)
  mh11=numeric(0)
  # mh2=numeric(0)
  mh22=numeric(0)
  # mh3=numeric(0)
  mh33=numeric(0)


  nu_0=0.2

  covg1<-numeric(0)
  covg2<-numeric(0)
  covg3<-numeric(0)
  covg4<-numeric(0)
  covg5<-numeric(0)
  covg6<-numeric(0)

  l1=numeric(0)
  l2=numeric(0)
  l3=numeric(0)
  l4=numeric(0)
  l5=numeric(0)
  l6=numeric(0)


  #covgp<-numeric(0)

  agldata<-array(0,dim = c(2,4))
  agldata[1,]<-c("L","R","cm_s","mu")
  ####################################################################
  #x=as.numeric(rwrappedcauchy(n1, mu=nu_0, rho=0.3,control.circular=list(units="radians")))

  bar_nu=as.numeric(circ.mean(x)%%(2*pi))
  y=(as.numeric(wrpcauchy.ml(x,0.3,0.5)[1]))%%(2*pi)
  m1=median.circular(x)%%(2*pi)
  mh1=medianHL.circular(x,method="HL1")%%(2*pi)
  mh2=medianHL.circular(x,method="HL2")%%(2*pi)
  mh3=medianHL.circular(x,method="HL3")%%(2*pi)
  bar_rho=est.rho(x)

  foreach (h = 1:H) %do%
    {
      g=rwrappedcauchy(n1, mu=bar_nu, rho=bar_rho,
                       control.circular=list(units="radians"))
      bar1_nu[h]=as.numeric(circ.mean(g)%%(2*pi))
      z[h]=(as.numeric(wrpcauchy.ml(g,0.3,0.5)[1]))%%(2*pi)
      m11[h]=median.circular(g)%%(2*pi)
      mh11[h]=medianHL.circular(g,method="HL1")%%(2*pi)
      mh22[h]=medianHL.circular(g,method="HL2")%%(2*pi)
      mh33[h]=medianHL.circular(g,method="HL3")%%(2*pi)
    }

  #CI_s1<-(quantile.circular(z,probs = c(alpha/2,1-alpha/2)))
   CI_s2<-(quantile.circular(z,probs = c(0.025,0.975)))
  # CI_s3<-(quantile.circular(m11,probs = c(0.025,0.975)))
  # CI_s4<-(quantile.circular(mh11,probs = c(0.025,0.975)))
  # CI_s5<-(quantile.circular(mh22,probs = c(0.025,0.975)))
  # CI_s6<-(quantile.circular(mh33,probs = c(0.025,0.975)))
  #L1<-mean(z)-min(as.numeric(CI_s1))
   L2<-mean(z)-min(as.numeric(CI_s2))
  # L3<-mean(m11)-min(as.numeric(CI_s3))
  # L4<-mean(mh11)-min(as.numeric(CI_s4))
  # L5<-mean(mh22)-min(as.numeric(CI_s5))
  # L6<-mean(mh33)-min(as.numeric(CI_s6))
  #R1<-max(as.numeric(CI_s1))-mean(z)
   R2<-max(as.numeric(CI_s2))-mean(z)
  # R3<-max(as.numeric(CI_s3))-mean(m11)
  # R4<-max(as.numeric(CI_s4))-mean(mh11)
  # R5<-max(as.numeric(CI_s5))-mean(mh22)
  # R6<-max(as.numeric(CI_s6))-mean(mh33)
  #I1=c(bar_nu-L1,bar_nu+R1)
   I2=c(y-L2,y+R2)
  # I3=c(m1-L3,m1+R3)
  # I4=c(mh1-L4,mh1+R4)
  # I5=c(mh2-L5,mh2+R5)
  # I6=c(mh3-L6,mh3+R6)
  #agldata[2,]<-as.numeric(c(I1[1],I1[2],bar_nu,nu_0))
   agldata[2,]<-as.numeric(c(I2[1],I2[2],y,nu_0))
  # agldata[4,]<-as.numeric(c(I3[1],I3[2],m1,nu_0))
  # agldata[5,]<-as.numeric(c(I4[1],I4[2],mh1,nu_0))
  # agldata[6,]<-as.numeric(c(I5[1],I5[2],mh2,nu_0))
  # agldata[7,]<-as.numeric(c(I6[1],I6[2],mh3,nu_0))
  ss1<-agldata[1,order(agldata[2,])]
  # ss2<-agldata[1,order(agldata[3,])]
  # ss3<-agldata[1,order(agldata[4,])]
  # ss4<-agldata[1,order(agldata[5,])]
  # ss5<-agldata[1,order(agldata[6,])]
  # ss6<-agldata[1,order(agldata[7,])]
  covg1<-(ss1[2]=="L")*(ss1[3]=="R")+(ss1[1]=="L")*(ss1[4]=="R")+(ss1[1]=="L")*(ss1[2]=="R")+(ss1[3]=="L")*(ss1[4]=="R")
  # covg2[b]<-(ss2[2]=="L")*(ss2[3]=="R")+(ss2[1]=="L")*(ss2[4]=="R")+(ss2[1]=="L")*(ss2[2]=="R")+(ss2[3]=="L")*(ss2[4]=="R")
  # covg3[b]<-(ss3[2]=="L")*(ss3[3]=="R")+(ss3[1]=="L")*(ss3[4]=="R")+(ss3[1]=="L")*(ss3[2]=="R")+(ss3[3]=="L")*(ss3[4]=="R")
  # covg4[b]<-(ss4[2]=="L")*(ss4[3]=="R")+(ss4[1]=="L")*(ss4[4]=="R")+(ss4[1]=="L")*(ss4[2]=="R")+(ss4[3]=="L")*(ss4[4]=="R")
  # covg5[b]<-(ss5[2]=="L")*(ss5[3]=="R")+(ss5[1]=="L")*(ss5[4]=="R")+(ss5[1]=="L")*(ss5[2]=="R")+(ss5[3]=="L")*(ss5[4]=="R")
  # covg6[b]<-(ss6[2]=="L")*(ss6[3]=="R")+(ss6[1]=="L")*(ss6[4]=="R")+(ss6[1]=="L")*(ss6[2]=="R")+(ss6[3]=="L")*(ss6[4]=="R")
  #l1=I1[2]-I1[1]
   l2=I2[2]-I2[1]
  # l3[b]=I3[2]-I3[1]
  # l4[b]=I4[2]-I4[1]
  # l5[b]=I5[2]-I5[1]
  # l6[b]=I6[2]-I6[1]
  # #covgp[b]<-mean(covg)
  # pt<-round(c(L, R, cm_s, mu, covg[i],mean(covg)),3)
  # cat(i,": ",pt , "\n")
  #cat(i,": ","L=",L,"R=", R, "Est.Mean=",cm_s,"Tr.Mean=", mu,"I/O=", covg[b],"Covg.Prob=",mean(covg), "\n")


  return(cat("COVERGE PROBABILITY and Length For ml is =",c(covg1,l2),"\n"))
}

