

% Tested with:
% Windows: Matlab R2022b, Intel Fortran Compiler (ifort) version 2021.7.0, Visual Studio 2019 (16.11.20), Windows 11 22H2 Build 22623
% Linux: Matlab R2022b, Intel Fortran Compiler (ifort) version 2021.7.0, Clear Linux 37440
% Mac: Matlab R2022a, Intel Fortran Compiler (ifort) version 2021.6.0, Macos 12.3.1


% NB: -R2018a flag is required
if ispc
    
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /QaxAVX2 /Qunroll-aggressive' readunv.F
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /Os /QaxAVX2 /assume:buffered_io /Qunroll-aggressive' writeunv.F

elseif ismac

    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -Os -axAVX2 -unroll-aggressive' readunv.F
    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -Os -axAVX2 -assume:buffered_io -unroll-aggressive' writeunv.F

else % linux

    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -Os -i8 -axAVX2 -unroll-aggressive' readunv.F
    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -Os -i8 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F

end

