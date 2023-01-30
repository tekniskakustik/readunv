

% Tested with:
% Windows: Matlab R2022b update 3, Intel Fortran Compiler (ifort) version 2021.8.0, Visual Studio 2019 (16.11.23), Windows 11 22H2 Build 22623
% Linux: Matlab R2022b update 3, Intel Fortran Compiler (ifort) version 2021.7.0, Clear Linux 38150
% Mac: Matlab R2022b update 0, Intel Fortran Compiler (ifort) version 2021.7.0, Macos 12.6, Xcode 14.0.1


% NB: -R2018a flag is required
if ispc
    
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /O2' readunv.F
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /O2 /QaxAVX2 /assume:buffered_io /Qunroll-aggressive' writeunv.F

elseif ismac

    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -O2' readunv.F
    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -O2 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F

else % linux

    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -O2 -i8' readunv.F
    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -O2 -i8 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F

end

