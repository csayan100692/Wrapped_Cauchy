

PBTwcT2=function(x,alpha)
{
  library(circular)
  library(CircStats)
  library(doParallel)
  library(foreach)
  registerDoParallel(4)
  options(cores=4)
  H=500
  n1=length(x)
  inner_loop_statistic_value_array1=array(0,H)
  count1=0
  d11=array(0,H)
  m11=array(0,H)
  mh11=array(0,H)
  mh22=array(0,H)
  mh33=array(0,H)
  z1=vector(mode='list', length=H)
  bar1_nu=array(0,H)
  nu_0=0.2
  #################################################################
  y=wrpcauchy.ml(x,0.3,0.5)
  C=sum(cos((x)))/n1
  S=sum(sin((x)))/n1
  bar_nu<-circ.mean(x)%%(2*pi)
  m1=median.circular(x)%%(2*pi)
  bar_rho=sqrt(C^2+S^2)
  d1=min(m1-nu_0,2*pi-(m1-nu_0))
  #d2[b]=min(as.numeric(y[[b]][1])%%(2*pi)-nu_0,2*pi-(as.numeric(y[[b]][1])%%(2*pi)-nu_0))
  statistic_value=sqrt(n1)*(d1)/sqrt((1-bar_rho^2)^2/2*bar_rho^2)
  # statistic_value_array2[b]=sqrt(n1)*(as.numeric(y[[b]][1])%%(2*pi)-nu_0)/sqrt((1-bar_rho^2)^2/2*bar_rho^2)
  # statistic_value_array3[b]=sqrt(n1)*(m1[b]-nu_0)/sqrt((1-bar_rho^2)/2*bar_rho^2)
  # statistic_value_array4[b]=sqrt(n1)*(mh1[b]-nu_0)/sqrt((1-bar_rho^2)/2*bar_rho^2)
  # statistic_value_array5[b]=sqrt(n1)*(mh2[b]-nu_0)/sqrt((1-bar_rho^2)/2*bar_rho^2)
  # statistic_value_array6[b]=sqrt(n1)*(mh3[b]-nu_0)/sqrt((1-bar_rho^2)/2*bar_rho^2)
  foreach(h = 1:H) %do%
    {
      g=rwrappedcauchy(n1, mu=nu_0, rho=bar_rho,
                       control.circular=list(units="radians"))

      C1=sum(cos((g)))/n1
      S1=sum(sin((g)))/n1
      bar1_rho=sqrt(C1^2+S1^2)

      bar1_nu[h]=circ.mean(g)%%(2*pi)
      z1[[h]]=wrpcauchy.ml(g,0.3,0.5)
      m11[h]=median.circular(g)%%(2*pi)
      mh11[h]=medianHL.circular(g,method="HL1")%%(2*pi)
      mh22[h]=medianHL.circular(g,method="HL2")%%(2*pi)
      mh33[h]=medianHL.circular(g,method="HL3")%%(2*pi)
      d11[h]=min(m11[h]-nu_0,2*pi-(m11[h]-nu_0))

      inner_loop_statistic_value_array1[h]=sqrt(n1)*(d11[h])/sqrt((1-bar1_rho^2)/2*bar1_rho^2)
      # inner_loop_statistic_value_array2[h]=sqrt(n1)*(as.numeric(z1[[h]][1])%%(2*pi)-nu_0)/sqrt((1-bar1_rho^2)^2/2*bar1_rho^2)
      # inner_loop_statistic_value_array3[h]=sqrt(n1)*(m11[h]-nu_0)/sqrt((1-bar1_rho^2)/2*bar1_rho^2)
      # inner_loop_statistic_value_array4[h]=sqrt(n1)*(mh11[h]-nu_0)/sqrt((1-bar1_rho^2)/2*bar1_rho^2)
      # inner_loop_statistic_value_array5[h]=sqrt(n1)*(mh22[h]-nu_0)/sqrt((1-bar1_rho^2)/2*bar1_rho^2)
      # inner_loop_statistic_value_array6[h]=sqrt(n1)*(mh33[h]-nu_0)/sqrt((1-bar1_rho^2)/2*bar1_rho^2)
    }
  critical_value=quantile((inner_loop_statistic_value_array1),1-alpha)
  # critical_value_array2[b]=quantile((inner_loop_statistic_value_array2),1-alpha1)
  # critical_value_array3[b]=quantile((inner_loop_statistic_value_array3),1-alpha1)
  # critical_value_array4[b]=quantile((inner_loop_statistic_value_array4),1-alpha1)
  # critical_value_array5[b]=quantile((inner_loop_statistic_value_array5),1-alpha1)
  # critical_value_array6[b]=quantile((inner_loop_statistic_value_array6),1-alpha1)
  #

  return(cat("test statistic and critical value for cm is = ", c(statistic_value, critical_value),"\n"))
}


