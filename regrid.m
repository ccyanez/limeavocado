function [griddedData, griddedStats] = regrid(data)
%REGRID grid the data and save stats

% bin the data into 0.01 km^2 grid cells (100m by 100m)
[latbin, lonbin] = hista(data.Latitude, data.Longitude, 0.1);

% figure out which bin each data point falls into 
[~, closestlatIndex] = min(abs(latbin-data.Latitude'));
[~, closestlonIndex] = min(abs(lonbin-data.Longitude'));

% add the bin indices to the dataframe
data.latInd = closestlatIndex';
data.lonInd = closestlonIndex';

% compute mean within each bin & save as table
statArray = grpstats(data, {'latInd','lonInd'}, {'mean','std','min','max'}, ...
    'DataVars',{'CO2_dry','CO','CH4_dry','CO2xs','COxs','CH4xs'}); 
header = {'lat','lon','CO2','CO','CH4','CO2xs','COxs','CH4xs'};
griddedData = table(latbin(statArray.latInd), lonbin(statArray.lonInd), statArray.mean_CO2_dry, ...
    statArray.mean_CO, statArray.mean_CH4_dry, statArray.mean_CO2xs, ...
    statArray.mean_COxs, statArray.mean_CH4xs,'VariableNames',header);

griddedStats = [mean(griddedData{:,3:end},1); std(griddedData{:,3:end}); ...
    min(griddedData{:,3:end}); max(griddedData{:,3:end})];
griddedStats = splitvars(table(griddedStats),1,"NewVariableNames",{'CO2','CO','CH4','CO2xs','COxs','CH4xs'});
griddedStats.Properties.RowNames = {'mean','stdev','min','max'};

end