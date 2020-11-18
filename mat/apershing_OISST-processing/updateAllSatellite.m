function updateAllSatellite
%updateAllSatellite--updates MURSST and AVHRROI data for the Gulf of Maine
%
% updateAllSatellite
close all
HOME=pwd;

clk=clock;%get the time
THISYEAR=int2str(clk(1));
IMAGEPATH=['/Users/apershing/Dropbox/Work/Projects/CafeGOM/Hydrogaphic/AVHRR_OI/',THISYEAR,'_images'];
if(~exist(IMAGEPATH,'dir'));
    fprintf('creating folder for %s\n',THISYEAR);
    mkdir(IMAGEPATH);
    mkdir([IMAGEPATH,'/oldCB']);
end

GOMPATH='/Users/apershing/Work/Projects/CafeGOM/Hydrogaphic/AVHRR_OI/';
GLOBPATH='/Users/apershing/WorkLG/AVHRR_OI';
MURPATH='/Users/apershing/WorkLG/MURSST/';

addpath(GOMPATH);
addpath(GLOBPATH);
addpath(MURPATH);


%1. Get the AVHRR_OI data

[TSd15,TSyr,yr,SSTyr,SSTts]=updateGOMSSTfromGlobal;%gets the global data and then extracts GoM

fprintf('Updating monthly AVHRR_OI data\n');
updateAVHRR_OI_monthly

%2. Get the MURSST data for this year
cd(MURPATH);
[yrday,lon,lat,SST]=updateNWAMURSST;%updates the NWShelf data
% This is a function.  It does save the data to MURSST_GoM_YYYY

load('AVHRR_Climatology_1982_2011_lg','Chr');
m=size(SST,3);
SST8a=nanmean(SST(:,:,m-7:m)-Chr(:,:,m-7:m),3);

load GOMIJhr %rows/cols for Gulf of Maine
SST=SST(GOMIlat,GOMJlon,:);%trim to the Gulf of Maine
SSTgom=reshape(SST,length(GOMIlat)*length(GOMJlon),size(SST,3));%flatten
SSTgom=nanmean(SSTgom);
load TScomp MtoAVHcoefs;
SSTgom=MtoAVHcoefs(1)*SSTgom+MtoAVHcoefs(2);%calibrate to AVHRR units


%3. add the MUR time series to the seasonal cycle figure
ysp=span(yrday(:,1));
I=find(~isnan(SSTyr(end,:)));
lastday=I(end);
I=find(yrday(:,1)==ysp(2) & yrday(:,2)>=lastday);%max year

figure(2);hold on;
plot(yrday(I,2),SSTgom(I)-273.15,'r');
t=get(gca,'title');
tstr=get(t,'string');
I=find(isnan(yrday(:,2)));
yrday(I,:)=[];
[m,d]=daynum2date(yrday(end,2),yrday(end,1));
I=find(tstr==',');
tstr=tstr(1:I(1)-1);%delete  "relative to 1982-2011"
tstr=[tstr sprintf(' and extended to %d/%d using MURSST',m,d)];
title(tstr);

cd(HOME);

figure(1);
d=daynum(yrday(I(end),1),m,d);
set(1,'position',[ 64   131   803   685]);
EPSPNGsave(sprintf([IMAGEPATH,'/GoMtimeseries_%d_%s'],yrday(I(end),1),padstr0(d,3)));

figure(2);
EPSPNGsave(sprintf([IMAGEPATH,'/GoMcycle_%d_%s'],yrday(I(end),1),padstr0(d,3)));

figure(3);
mapGOMSSTanomaly(lon(22:1449),lat(25:end),SST8a(25:end,22:1449));
set(3,'position',[8          82        1854        1263]);
print('-dpng',sprintf([IMAGEPATH,'/oldCB/GoMsstanomaly_%d_%s'],yrday(I(end),1),padstr0(d,3)));
%build a new colormap to highlight hot colors
map=rwb(17);
map=[map(1:16,:);interp1([1;5],[1 0 0;0 0 0],(1:5)');];%paint it black!
colormap(map);
caxis([-4.25 6.25])
print('-dpng',sprintf([IMAGEPATH,'/GoMsstanomaly_%d_%s_newCB.png'],yrday(I(end),1),padstr0(d,3)));

PD=load([GLOBPATH,'/AVHRROI_provisonal']);%most recent OISST
figure(4);clf;
mapglobalSSTanomaly(PD.lon,PD.lat,nanmean(PD.SSTan,3));
colormap(rwb(33));
caxis([-1 1]*(4+1/8))
[m1,d1]=daynum2date(PD.yrday(1,2)-1,PD.yrday(1,1));
[m2,d2]=daynum2date(PD.yrday(end,2)-1,PD.yrday(end,1));
title(sprintf('%d/%d/%d-%d/%d/%d',m1,d1,yrday(1,1),m2,d2,yrday(end,1)))
set(gca,'position',[0 0 1 0.95])
tightmap;
cb=colorbar('location','southoutside');
cb.Position=[0.2 0.09 0.6,0.0161];
set(4,'position',[  136          82        1726        1239]);
ax=gca;
ax.Title.FontSize=32;
print('-dpng',sprintf([IMAGEPATH,'/GlobalSSTanomaly_%d_%s'],yrday(I(end),1),padstr0(d,3)));
