function [TSd15,TSyr,yr,SSTyr,SSTts]=updateGOMSSTfromGlobal
%UPDATEGOMSSTFROMGLOBAL--updates GOM SST data by first loading global files
%updateGOMSST
%
% updates the AVHRR data set for the Gulf of Maine, includinging the
% standard time series figure
%

%1. load the original data:
GOMPATH='/Users/apershing/Work/Projects/CafeGOM/Hydrogaphic/AVHRR_OI/';
load([GOMPATH,'GOMSST']);

%2. get the lastest AVHRROI
CWD=pwd;

LOADDATA=0;
GLOBALPATH='/Users/apershing/WorkLG/AVHRR_OI';

FINISHYEAR=0;%0--update, or year if finishing a particular year

if(exist(GLOBALPATH,'dir'))
    %on ice, so try to load the global data
    cd(GLOBALPATH);
    if(FINISHYEAR>0)
        updateGlobalAVHRR(FINISHYEAR);
    else
        updateGlobalAVHRR;
    end
    load GOMIJ;%indices into global data for GOM
    LOADDATA=1;
end

%LOADDATA=1;%force loading for finishing the year
clk=clock;%current time
if(FINISHYEAR>0)
   clk(1:3)=[FINISHYEAR 12 31];%finish the year
end

if(LOADDATA)
       
    YR=int2str(clk(1));
    cd([GLOBALPATH,'/',YR]);%folder for this year
    if(yrday(end,1)~=clk(1));
        nextday=1;
    else
        nextday=yrday(end,2)+1;
    end
    j=size(yrday,1)+1;
    fname=['AVHRR_OI_',YR,'_',padstr0(nextday,3),'.mat'];
    while(exist(fname,'file'))
        BF=load(fname,'SST');
        SST(:,:,j)=BF.SST(GOMIlat,GOMJlon);
        yrday(j,:)=[clk(1),nextday];
        nextday=nextday+1;
        fname=['AVHRR_OI_',YR,'_',padstr0(nextday,3),'.mat'];
        j=j+1;
    end
    %4. create the anomalies
    SSTan=SST-GOMclim(:,:,yrday(:,2));
    
    %4.1 load the provisional data
    PD=load([GLOBALPATH,'/AVHRROI_provisonal']);
    SST_p=PD.SST(GOMIlat,GOMJlon,:);
    SSTan_p=PD.SSTan(GOMIlat,GOMJlon,:);
    yrday_p=PD.yrday;

    %5. save the data
    cd(CWD);%back home
    save([GOMPATH,'GOMSST'],'-append','SST', 'SSTan', 'yrday','SST_p','SSTan_p');
end

%6. make the figure
figure(1);clf;
[TSd15, TSyr]=MakeGOMTSfigure(yrday, nanmean(reshape(SSTan, size(SSTan,1)*size(SSTan,2), size(SSTan,3)))');
plotmrks(TSyr(end,1),TSyr(end,2));
%7. load the annual cycle
load GOMSSTcycle SST*

%8. create the time series
SST=reshape(SST,size(SST,1)*size(SST,2),size(SST,3));%one long time series
SSTts=nanmean(SST,1)-273.15;%convert to C

yr=(yrday(1,1):yrday(end,1))';
SSTyr=nans(length(yr),366);

for j=1:length(yr);
    I=find(yrday(:,1)==yr(j));
    SSTyr(j,yrday(I,2))=SSTts(I);
end

%9. plot the current year
figure(2);clf
MakeGOMSeasonalFigure(yrday,SSTts, yr, SSTyr, yrday(end,1), [1982 2011]);
hold on
sstprov=nanmean(reshape(SST_p, size(SST_p,1)*size(SST_p,2), size(SST_p,3)));
sstprov=sstprov-273.15;
plot(yrday_p(:,2),sstprov,'r--','linewidth',2);


save([GOMPATH,'GOMSSTcycle'],'-append','SSTts', 'SSTyr', 'yrday');
