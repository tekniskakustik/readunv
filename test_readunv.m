

d = dir(['.' filesep, 'testdata', filesep, '*.unv']);

nfiles = length(d);
success = false(nfiles, 1);
data = cell(nfiles, 1);

for filecount = 1:nfiles
    [success(filecount), data{filecount}] = readunv([d(filecount).folder, filesep, d(filecount).name]);
end