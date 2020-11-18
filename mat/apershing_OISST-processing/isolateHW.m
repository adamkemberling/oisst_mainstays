function HWd=isolateHW(t,HWf)
%ISOLATEHW--identify individual heatwaves
%

n=length(t);
if(length(HWf)~=n)
    error('t and HWf must be the same length');
end

buf=200;
HWd=nans(buf,2);
len=buf;

j=1;
h=0;
while(j<=n)
    while(j<=n & ~HWf(j))
        %not in a heatwave
        j=j+1;
    end
    if(j<=n)
        %found a heatwave
        h=h+1;
        if(h>len)
            HWd=[HWd;nans(buf,2)];
            len=len+buf;
        end
        HWd(h,1)=j;
        while(j<=n & HWf(j))
            %still in heatwave
            j=j+1;
        end
        HWd(h,2)=j-1;
    end
end
HWd=HWd(1:h,:);
HWd(:,3:4)=t(HWd(:,1:2));
