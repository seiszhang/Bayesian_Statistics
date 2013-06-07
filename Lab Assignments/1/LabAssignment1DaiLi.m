%% Code for Lab Assignment 1
clear; clc; close all;
p=0.8;n=30;
theta = linspace(0,1,n); lower=zeros(10000,3);upper=zeros(10000,3);
coverage=zeros(10000,3);Intlength=zeros(10000,3);
for i=1:10000
R = binornd(1,p,1,n);
%Frequentist,Using the Wald Interval
mean1=sum(R)/length(R);
lower(i,1) = mean1 - 1.96*sqrt(mean1*(1-mean1)/length(R));
upper(i,1) = mean1 + 1.96*sqrt(mean1*(1-mean1)/length(R));
%Uniform- Prior Bayesian, it is actually beta(1,1)distribution
[mean2,sigma2]=betastat(1+sum(R),1+length(R)-sum(R));
lower(i,2) = mean2 - 1.96*sqrt(sigma2/length(R));
upper(i,2) = mean2 + 1.96*sqrt(sigma2/length(R));
%Beta(8,2)-Prior Bayesian
[mean3,sigma3]=betastat(8+sum(R),2+length(R)-sum(R));
lower(i,3) = mean3 - 1.96*sqrt(sigma3/length(R));
upper(i,3) = mean3 + 1.96*sqrt(sigma3/length(R));
for k=1:3
    Intlength(i,k)=upper(i,k)-lower(i,k);
    if lower(i,k)<=0.8 && upper(i,k)>=0.8
        coverage(i,k)=1;
    end
end
end
%%%OUTPUT
CoverageFrequentist=sum(coverage(:,1),1)/size(coverage,1)
CoverageUniform=sum(coverage(:,2),1)/size(coverage,1)
CoverageBeta82=sum(coverage(:,3),1)/size(coverage,1)
IntervalLengthFrequentist=mean(Intlength(:,1))
IntervalLengthUniform=mean(Intlength(:,2))
IntervalLengthBeta82=mean(Intlength(:,3))
