

niter = 1e3;

mem = zeros(niter, 1);

tic
for count = 1:niter
    m = builtin('memory');
    mem(count) = m.MemUsedMATLAB/1024^2;
    [success, datacell] = readunv(['testdata', filesep, 'test5.unv']);
end
toc

figure(1)
clf
plot(mem)

