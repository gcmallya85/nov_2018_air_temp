% Process climate division data
% Created by: Ganesh Mallya (gmallya.com)
% Created on: 11/28/2018
% Ref 1: ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/divisional-readme.txt
% Ref 2: https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-divisions.php
% Ref 3: ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpcdv-v1.0.0-20181105

clear all;
clc;
fclose all;

st_code = '12'; % State code for which data have to be extracted (12=Indiana, see Ref 1 for more details)
div_code = '04'; % Division code for which data have to be extracted (04= cli div over West Lafayette, see Ref 2 for more details)

fid  = fopen('climdiv-tmpcdv-v1.0.0-20181105.txt','r');
tline = fgetl(fid); % Read first line from the file
ind = 0; % index to store data
while ischar(tline)
    if strcmp(tline(1:2),st_code) && strcmp(tline(3:4),div_code)
        yr = str2num(tline(7:10));
        % Extract 12 months of data for the given year
        for i = 1:12
            ind = ind + 1;
            year(ind,1) = yr;
            month(ind,1) = i;
            data(ind,1) = str2num(tline((11+(i-1)*7):(17+(i-1)*7)));
        end
    end
    tline = fgetl(fid); % Read next line
end
fclose(fid);

fid = fopen('long_term_air_temp_clidiv.txt','w');
fprintf(fid,'Year,Month,AvgTempDegF');
for j = 1:length(year)
    fprintf(fid,'\n%d,%d,%0.2f',year(j,1),month(j,1),data(j,1));
end