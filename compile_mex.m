

% Tested with Intel Fortran Compiler (ifort) version 2021.5.0, Visual Studio 2019, Windows x64
% NB: -R2018a flag is required
if ispc
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /Qax:AVX2' readunv.F 
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /Qax:AVX2' writeunv.F
elseif ismac

else % linux
    mex FC='ifort' -R2018a COMPFLAGS='$COMPFLAGS /O2 /Qax:AVX2' readunv.F
end

