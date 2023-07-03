

N = 51200;

C = createTemplate(58);
C.data = rand(N, 1) + 1j*(rand(N, 1));
% C.data = rand(N, 1);
% C.data = single(rand(N, 1) + 1j*(rand(N, 1)));
% C.data = single(rand(N, 1));
C.x = single(1:N);
% C.evenSpacing = 1;
% C.x0 = 1;
% C.dx = 1;
% C.dataType = 'BINARY';

tic
success = writeunv('test.unv', C, 1);
toc
tic
[x, y] = readunv('test.unv')
toc

max(abs(C.data-y{1}.data))