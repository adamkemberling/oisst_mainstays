function updateGlobalAVHRR(yr)
%UPDATEGLOBALAVHRR--updates the global AVHRROI files
%

if(nargin==0)
    %use the current year
    clk=clock;%current time
    yr=clk(1);%the current year
else
    %yr is provided, so update that year
    clk=[yr,12,31];%the whole year
end

PATH='/Users/apershing/WorkLG/AVHRR_OI';
load([PATH,'/AVHRRclim_1982_2011'],'SSTclim');

if(~exist([PATH,'/',int2str(yr)]))
    fprintf('Creating directory for %d\n',yr);
    mkdir([PATH,'/',int2str(yr)]);
end


base=[int2str(yr),'/AVHRR_OI_',int2str(yr),'_'];%base file name

d=dir([PATH,'/',base,'*.mat']);
n=length(d);%number of files
%check that there are actually n days
if(n>0)
    if(~strcmp([d(n).folder,'/',d(n).name],[PATH,'/',base,padstr0(n,3),'.mat']))
        error('Found %d files in %s, but last file is not day %d\n',n,[PATH,'/',base,'*.mat'], n);
    end
end

today=daynum(yr,clk(2),clk(3))+1;%daynumber of today

c=0;
try
    for j=n+1:today;
        day=j;
        [m,d]=daynum2date(j-1,yr);%the calendar dates
        [SST,yrday,lat,lon]=loadOISSTopendapDay(yr,m,d);
        c=c+1;
        SSTan=SST-SSTclim(:,:,j);
        str=[PATH,'/',base,padstr0(j,3),'.mat'];
        fprintf('%s\n',str);
        save(str,'lon','lat','yrday','SST','SSTan');
    end
catch
    fprintf('Ending after %d days\n',c);
end

try
    fprintf('Trying to update OISST provisional data\n');
    [SST, yrday, lat, lon]=loadOISSTopendapProvisional;
    SSTan=SST-SSTclim(:,:,yrday(:,2));
    save([PATH,'/AVHRROI_provisonal'],'lon','lat','yrday','SST','SSTan');
catch
    fprintf('Unable to update OISST provisional data\n');
end
    