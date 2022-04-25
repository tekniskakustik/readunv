

[~, y2] = readunv('C:\Tamara\readunv\testdata\test2.unv');

%%

idx = 1;
niter = 1;

tic
writeunv('C:\root\Desktop\test0.unv', y2{1}, 1, idx);
writeunv('C:\root\Desktop\test0.unv', y2{2}, 2, idx);
writeunv('C:\root\Desktop\test0.unv', y2{3}, 2, idx);
% toc
writeunv();
toc

tic
writeunv('C:\root\Desktop\test01.unv', y2{1}, 1);
for count = 1:niter
    writeunv('C:\root\Desktop\test01.unv', y2{2}, 0);
end
toc

tic
write151('C:\root\Desktop\test02.unv', y2{1});
for count = 1:niter
    write164('C:\root\Desktop\test02.unv', y2{2});
end
toc


%%
S = y2{3};
idx = 2;
writeunv()
S = rmfield(S, 'data');
filepath = 'C:\root\Desktop\test03.unv';
filepath2 = 'C:\root\Desktop\test04.unv'; 

writeunv(filepath, y2{1}, 1, idx)

writeunv(filepath, y2{2}, 0, idx)

[success, casenum] = writeunv(filepath, S, 0, idx)

stp = 4;
for ii = 1:7  
    DATA = y2{3}.data((1:stp)+(ii-1)*stp);
    writeunv(casenum, DATA, 3, idx, 58)
end
tic
writeunv(-1, filepath, 3, idx, 58)
toc
writeunv();

copyfileQ(filepath, filepath2)

tic
fid = fopen(filepath2, 'r+');
fseek(fid, 518, 'bof');
fwrite(fid, sprintf('%10i', 256), 'char');
fclose(fid);
toc

%%

S.data = double(S.data);
writeunv('C:\root\Desktop\test03.unv', S)