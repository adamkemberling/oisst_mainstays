function varargout=MakeHWfigures(fname)
%MAKEHWFIGURES--makes the historical figures w/ heatwaves marked
%
% MakeHWfigures
%
% Creates two figures, one showing the absolute temperatures, one showing
% anomalies
%
% HW=MakeHWfigures
%           --returns a mask w/ 1 indicating heatwaves
% [HW,SSTyr,Anomyr]=MakeHWfigurse
%           --returns HW plus the temperature and anomaly matrices
%
%
% Default is to read from GOMSSTcycle.mat but this can be overwritten by
% passing in a file name, i.e.
%
% [HW,SSTyr,Anomyr]=MakeHWfigures('GOMSSTcycle_update')
%
% The file must have 
%
%  SSTts          1xT--daily temperatures in °C, usually the mean over the region        
%  SSTyr         Yx366--SSTts reshaped to a year -by- day grid             
%  yrday        Tx2--time labels for SSTts
%
if(nargin<1)
    fname='GOMSSTcycle';
else
    if(~ (exist(fname,'file') || exist([fname,'.mat'],'file')))
        error('file %s does not exist', fname);
    end
end
load(fname)

yr=(yrday(1,1):yrday(end,1))';

I=find(yr>=1982 & yr<=2011);%climatology

Clim=nanmean(SSTyr(I,:));

Anomyr=SSTyr-repmat(Clim,length(yr),1);%anomalies

load('SSTcycleStats');
HW=nans(size(SSTyr));
for j=1:length(yr);
    I=find(~isnan(SSTyr(j,:)));
    HW(j,I)=isheatwave(SSTyr(j,I),sst90(I),5,1);%heatwave, 5 day required, allow 1 d break
end
[R,C]=find(HW==1);

figure;
F(1)=gcf;
g=ajpcolor(1:366,yr,zeros(size(SSTyr)));
g.FaceColor=[1 1 1]*0.6;
hold on;
ajpcolor(1:366,yr,SSTyr);
h(1)=plot(C,yr(R),'k.');
colormap(parula(length(3:21)))
caxis([2.5 21.5])
cb(1)=colorbar;
cb(1).Label.String='Temperature (°C)';
set(gca,'xlim',[1 365]);
monthlabels
g.FaceColor=[1 1 1]*0.75;

figure;
F(2)=gcf;
g=ajpcolor(1:366,yr,zeros(size(Anomyr)));
hold on;
ajpcolor(1:366,yr,Anomyr);
h(2)=plot(C,yr(R),'k.');
colormap(parula(length(3:21)))
caxis([-1 1]*4.25)
colormap(rwb(17));
cb(2)=colorbar;
cb(2).Label.String='Temperature Anomaly (°C)';
set(gca,'xlim',[1 365]);
monthlabels
g.FaceColor=[1 1 1]*0.75;

set(F,'position',[275    43   555   657]);%long and narrow

if(nargout==1)
    varargout={HW};
elseif(nargout>1)
    varargout={HW,SSTyr,Anomyr};
end
    
