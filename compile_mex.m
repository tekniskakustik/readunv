

% Tested with:
% Windows: Intel Fortran Compiler (ifort) version 2021.5.0, Visual Studio 2019, Windows 11
% Linux: Not yet supported
% NB: -R2018a flag is required
if ispc
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /Qax:AVX2 /Qunroll-aggressive' readunv.F
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /Qax:AVX2 /assume:buffered_io /Qunroll-aggressive' writeunv.F
elseif ismac

else % linux
    mex FC='ifort' -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /Qunroll-aggressive /Qax:AVX2' readunv.F
    mex FC='ifort' -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /Qax:AVX2 /assume:buffered_io /Qunroll-aggressive' writeunv.F
end

