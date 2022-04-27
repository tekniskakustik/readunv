

[~, y2] = readunv('C:\Tamara\readunv\testdata\test2.unv');


L = 512000;
% y2{3}.data = single(abs(y2{3}.data));

y2{3}.data = double(1e6*(2*rand(L, 1)-1));
y2{3}.dataFormatType = 4;
y2{3}.precision = 'double';

% y2{3}.data = 1e6*(2*rand(L, 1)-1) + 1j*1e6*(2*rand(L, 1)-1);
% y2{3}.dataFormatType = 6;
% y2{3}.precision = 'double';

% y2{3}.data = single(1e6*(2*rand(L, 1)-1));
% y2{3}.dataFormatType = 2;
% y2{3}.precision = 'single';

S = y2{3};
idx = 2;
writeunv();
S = rmfield(S, 'data');
S.refEntName = '';
S.rspEntName = '';
S.ID1 = '';
S.ID2 = '';
S.ID4 = '';
S.ID5 = '';
filepath = 'C:\root\Desktop\test03.unv';
filepath2 = 'C:\root\Desktop\test04.unv'; 
filepath3 = 'C:\root\Desktop\test05.unv'; 
filepath4 = 'C:\root\Desktop\test06.unv'; 
filepath5 = 'C:\root\Desktop\test00.unv'; 

niter = 1;

disp('-----')

delete(filepath)
delete(filepath2)
delete(filepath3)
delete(filepath4)
delete(filepath5)

pause(0.5)

tic

for count = 1:niter
%     writeunv(filepath, y2{1}, 1, idx);
%     writeunv(filepath, y2{2}, 2, idx);
    if count == 1
        [success, casenum] = writeunv(filepath, S, 1, idx);
    else
        [success, casenum] = writeunv(filepath, S, 0, idx);
    end
    
    stp = L/4;
    for ii = 1:floor(L/stp)
        DATA = y2{3}.data((1:stp)+(ii-1)*stp);
        writeunv(casenum, DATA, 3, idx, 58);
    end
    writeunv(-1, filepath, 3, idx, 58);
%     writeunv();
    
end
toc


idx = 3;

tic
for count = 1:niter
%     writeunv(filepath4, y2{1}, 1, idx);
%     writeunv(filepath4, y2{2}, 2, idx);
        
    if count == 1
        [success, casenum] = writeunv(filepath4, S, 1, idx);
    else
        [success, casenum] = writeunv(filepath4, S, 0, idx);
    end
    writeunv(casenum, y2{3}.data, 3, idx, 58);
    writeunv(-1, filepath4, 3, idx, 58);
end
toc

idx = 5;

% tic
% for count = 1:niter
%     writeunv(filepath3, y2{1}, 1, idx);
%     writeunv(filepath3, y2{2}, 0, idx);
% %     success = writeunv(filepath3, y2{3}, 2, idx);
%     writeunv();
% end
% toc

writeunv();


tic
writeunv(filepath5, y2{3}, 1);
for count = 1:niter-1
    success = writeunv(filepath5, y2{3}, 0);
end
toc

tic
for count = 1:niter
    writeuff(filepath2, y2(3), 'add');
end
toc



