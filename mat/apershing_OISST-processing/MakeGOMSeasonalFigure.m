function MakeGOMSeasonalFigure(yrday,SSTts, yr, SSTyr, YR, refper,noGB);
%MAKEGOMSEASONALFIGURE--plots a year against the annual cycle
%
%  MakeGOMSeasonalFigure(yrday,SSTts, yr, SST, YR, {refper});
%
% 

clf;
if(nargin<6)
    refper=[yr(1),yr(end)];%all data
end
if(nargin<7)
    noGB=0;
end
Iyr=find(yr>=refper(1) & yr<=refper(2));

%the min/max
%h=fill([1:365,365:-1:1],[max(SSTyr(:,1:365),[],1) ,fliplr(min(SSTyr(:,1:365),[],1))],'c');
hold on

%the SD
mn=nanmean(SSTyr(Iyr,1:365),1);
sd=2*nanstd(SSTyr(Iyr,1:365),1,1);
g=fill([1:365,365:-1:1],[mn+sd,fliplr(mn-sd)],'b');

%the mean
g2=plot(1:365,mn,'k');

%this year
I=find(yrday(:,1)==YR);
g3=plot(yrday(I,2),SSTts(I),'r');

if(noGB)
    load('SSTcycleStats_noGB');%working with noGB data
else
    load('SSTcycleStats');
end
T=SSTts(I);%this year;
hw=isheatwave(T,sst90(yrday(I,2)),5,1);%heatwave, 5 day required, allow 1 d break
K=find(~hw);%not heawaves
T(K)=nan;
g3b=plot(yrday(I,2),T,'r');

mx=nanmax(SSTyr,[],1);%max temp
T=SSTts(I);%this year;
K=find(T<mx(1:length(I)));%not max temps
T(K)=nan;
g3c=plotmrks(yrday(I,2),T,'r','s');




J=find(yrday(:,1)==2012);
g4=plot(yrday(J,2),SSTts(J),'k');

%make it pretty
%set(h,'edgecolor','none','facecolor',[[1 1]*0.75 1])
set(g,'edgecolor','none','facecolor',[[1 1]*0.5 1])
set(g2,'linewidth',4)
set(g3,'linewidth',1)
set(g3b,'linewidth',5)
set(g3c,'markersize',15,'color',[1 1 1]*0.05,'markerfacecolor',[0.9 0 0]);
monthlabels;
set(gca,'xlim',[0 365]);
ylabel('Daily Mean Temperature (°C)');
[m,d]=daynum2date(yrday(I(end),2),yrday(I(end),1)-1);
title(sprintf('%d Temperature Through %d/%d, relative to %d-%d',YR,m,d,refper(1),refper(2)));