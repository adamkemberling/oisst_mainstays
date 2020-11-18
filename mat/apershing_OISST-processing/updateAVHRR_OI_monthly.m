function updateAVHRR_OI_monthly
%UPDATEAVHRR_OI_MONTHLY--updates the AVHRR_OI_monthly file 
%
% updateAVHRR_OI_monthly
%
% Loads AVHRR_OI_monthly in /Users/apershing/WorkLG/AVHRR_OI and
% computes the monthly means of the global data in the year files. 
%

WDIR='/Users/apershing/WorkLG/AVHRR_OI';

CWD=pwd; %current directory before we start mucking around

cd(WDIR);%change to the working directory

load AVHRR_OI_monthly

Nmon=size(yrmon,1);%number of months
lastyr=yrmon(Nmon,1);
lastmon=yrmon(Nmon,2);

daybrks=linspace(1,366,13);
daybrks=round(daybrks);%assign days to months

maxyr=lastyr;
while(exist(int2str(maxyr+1),'dir'))
    maxyr=maxyr+1;
end
%maxyr is now the maximum year in the current database

MY=int2str(maxyr);
d=dir([MY,'/AVHRR_OI_',MY,'_*.mat']);

ND=length(d);
I=find(ND<=daybrks);
finalmon=I(1)-1;

newyrs=lastyr:maxyr;
newyrmon(:,1)=flatten(repmat(newyrs,12,1));
newyrmon(:,2)=repmat((1:12)',length(newyrs),1);

%filter out the months we want to update
%note that we start with the last year & month in case that was previously
%incomplete
Ist=find(newyrmon(:,1)==lastyr & newyrmon(:,2)==lastmon);
Ien=find(newyrmon(:,1)==maxyr & newyrmon(:,2)==finalmon);
newyrmon=newyrmon(Ist:Ien,:);
Nupdate=size(newyrmon,1);

SSTanew=nans(720,1440,Nupdate);
for j=1:Nupdate
    yr=newyrmon(j,1);
    days=daybrks(newyrmon(j,2)):daybrks(newyrmon(j,2)+1)-1;%days to update
    ND=length(days);
    M=nans(720,1440,ND);
    CY=int2str(yr);
    c=1;
    Indx=zeros(ND,1);
    fprintf('updating %d/%d, %d-%d\n',[newyrmon(j,:),span(days)])
    for k=1:ND;
        DY=padstr0(days(k),3);
        fname=[CY,'/AVHRR_OI_',CY,'_',DY,'.mat'];
        if(exist(fname,'file'))
            load(fname,'SSTan');
            M(:,:,k)=SSTan;
            Indx(c)=k;
            c=c+1;
        end
    end
    SSTanew(:,:,j)=mean(M(:,:,(1:c-1)),3);
end
SSTanM=cat(3,SSTanM(:,:,1:Nmon-1),SSTanew);
yrmon=[yrmon(1:Nmon-1,:);newyrmon];
save AVHRR_OI_monthly -append SSTanM

cd(CWD);