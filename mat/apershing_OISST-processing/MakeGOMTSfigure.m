function varargout=MakeGOMTSfigure(yrday, GOMSSTanTS)
%MAKEGOMTSFIGURE--makes standard GOM SST figure
%
% MakeGOMTSfigure(yrday, GOMSSTanTS)
%
% yrday=[year, day], 
% GOMSSTanTS= ts anomaly
%
% Plots the 15d smooth time series and the annual averages.  Then fits
% trends to the whole data and the last 10 years
%

%put into TS format
TSd=[yrday(:,1)+(yrday(:,2)-1)/365 GOMSSTanTS(:)];
TSd15=runmean(TSd,7);%smooth
TSyr=tssummary(TSd,1);
TSyr=TSyr(:,1,[1,3]);%get annual data

I10=find(TSd15(:,1)>=TSd15(end,1)-10);%last ten years

clf;
set(gcf,'position',[52   134   958   540]);%fixed size
g=plot(TSd15(:,1),TSd15(:,2),'b');hold on


lastyr=fix(TSd15(end,1));
if(TSd15(end,1)-lastyr<0.99);
    lastyr=lastyr-1; %must have 75% of a year to include
end
I=find(TSyr(:,1)<=lastyr);
TSyr(:,1)=TSyr(:,1)+0.5;%plot in the middle of the year
h=plotts(TSyr(I,:));
set(h,'color',[1 1 1]*0.4);
set(h,'markerfacecolor',get(h,'color'),'linewidth',2);
%legend([g,h],'15d smoothed','annual average','location','northwest');


I=find(~isnan(TSd15(:,2)));
[coefsA,bint,r,rint,statsA]=regress(TSd15(I,2),[TSd15(I,1),ones(size(I))]);


I10=find(TSd15(:,1)>=lastyr-9 & TSd15(:,1)<lastyr+1 & ~isnan(TSd15(:,2)));%last ten years

[coefs10,bint,r,rint,stats10]=regress(TSd15(I10,2),[TSd15(I10,1),ones(size(I10))]);

plot(TSd15(I([1,end]),1),coefsA(1)*TSd15(I([1,end]),1)+coefsA(2),'k','linewidth',4);
plot(TSd15(I10([1,end]),1),coefs10(1)*TSd15(I10([1,end]),1)+coefs10(2),'r','linewidth',4);

tx(1)=text(1985,-2,sprintf('overall trend: %4.2f ° yr-1',coefsA(1)));
tx(2)=text(2004,-2,sprintf('%d-%d: %4.2f ° yr-1',fix(TSd15(I10(1))),fix(TSd15(I10(end))),coefs10(1)));

set(tx(2),'color','r');
set(tx,'fontsize',14);

xlabel('Year');ylabel('SST Anomaly');
firstyr=fix(TSd15(1,1));
title(sprintf('Gulf of Maine SST Anomalies from %d to %d (relative to 1982-2011)',firstyr,lastyr));
set(gca,'xlim',[firstyr-1,lastyr+2]);

if(nargout==1)
    varargout={TSd15};
elseif(nargout==2)
    varargout={TSd15,TSyr};
end
    