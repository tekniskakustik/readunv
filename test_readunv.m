

d = dir(['.' filesep, 'testdata', filesep, '*.unv']);

nfiles = length(d);
success = false(nfiles, 1);
data = cell(nfiles, 1);

tic
for filecount = 1:nfiles
    [success(filecount), data{filecount}] = readunv([d(filecount).folder, filesep, d(filecount).name]);
end
toc

if exist('readuff.m', 'file')
    data2 = cell(size(data));
    tic
    for filecount = 1:nfiles
        data2{filecount} = readuff([d(filecount).folder, filesep, d(filecount).name]);
    end
    toc
end