function []=boot_ci(mu,sigma,n,alpha)

%%code for visualizing the effect of significance thresholding 
%on point and interval estimation as a function of power
%
%boot_ci(mu,sigma,n,alpha)
%
%mu = true population mean
%sigma = population standard deviation
%n = sample size
%alpha = significance threshold for p (e.g. 0.05)
%
%%%%%%NEUROSTORM 2015%%%%%

%%%%default values if no input given (corresponds to power of ~0.65)
if nargin==0
mu=1;sigma=2.5;n=36;alpha=.05;
end

%get power of test given input parameters
power=sampsizepwr('t',[0 sigma],mu,[],n,'Alpha',alpha)

figure(1);

%%generate 10,000 random samples of size n from normal distribution
boot=normrnd(mu,sigma,n,10000);
bootmeans=mean(boot);

%generate upper and lower bounds for 1-alpha confidence intervals for each sample
upp=bootmeans+tinv(1-alpha/2,n-1).*(std(boot)/sqrt(n));
low=bootmeans-tinv(1-alpha/2,n-1).*(std(boot)/sqrt(n));

%calculate proportion of intervals containing true population mean
mu_in_interval=sum(low<=mu & upp>=mu)/10000

%calculate proportion of sample means equal to true population mean (a.k.a. zero)
mean_equals_mu=sum(bootmeans==mu)/10000

%plot histogram, saving values for getting line heights
[a,b]=hist(bootmeans,100);
bar(b,a)

waitforbuttonpress
%add line showing expected value of results significant at alpha
signif_mean=mean(bootmeans(low>0))
line1=line([signif_mean signif_mean],[0 max(a)],...
'LineWidth',4,...
'Color','red');

%add line for expected value of all means (also expected value of population mean)
line2=line([mean(bootmeans) mean(bootmeans)],[0 max(a)],'LineStyle','--',...
'LineWidth',4,...
'Color','black');

title(strcat('Sampling Distribution of All Means, Power=',sprintf('%.2f',power)),'FontSize',18)
legend([line2,line1],'True Mean','Mean of Significant Results')


waitforbuttonpress
figure(2);

%plot sampling distribution for significant tests
[c,d]=hist(bootmeans(low>0),100);
bar(d,c)
waitforbuttonpress
figure(2);

%add line for expected value of all means (also expected value of population mean)
line3=line([mean(bootmeans) mean(bootmeans)],[0 max(c)],'LineStyle','--',...
'LineWidth',4,...
'Color','black');

%add line showing expected value of results significant at alpha
line4=line([signif_mean signif_mean],[0 max(c)],...
'LineWidth',4,...
'Color','red');
title(strcat('Sampling Distribution of Significant Means, Power=',sprintf('%.2f',power)),'FontSize',18)
legend([line3,line4],'True Mean','Mean of Significant Results')

%calculate proportion of significant confidence intervals containing true population mean
signif_mu_in_interval=sum(low(low>0)<=mu & upp(low>0)>=mu)/10000

end