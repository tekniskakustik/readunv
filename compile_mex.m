

% Tested with:
% Windows: Matlab R2022a, Intel Fortran Compiler (ifort) version 2021.5.0, Visual Studio 2019 (16.11.12), Windows 11 22H2 Build 22598
% Linux: Matlab R2022a, Intel Fortran Compiler (ifort) version 2021.5.0, Clear Linux 36220

% NB: -R2018a flag is required
if ispc || ismac
    
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /QaxAVX2 /Qunroll-aggressive' readunv.F
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /QaxAVX2 /assume:buffered_io /Qunroll-aggressive' writeunv.F

else % linux

    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -Os -i8 -axAVX2 -unroll-aggressive' readunv.F
    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -Os -i8 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F

end
