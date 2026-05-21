

% Tested with:
% Windows: Matlab R2024b update 9, Intel Fortran Compiler (ifort) version 2021.13.1, Visual Studio 2022 (17.14.32) Windows 11 25H2 Build 26200.8457
% Linux:   Matlab R2024b update 7, Intel Fortran Compiler (ifort) version 2021.10.0, GCC 15.1.1                    Clear Linux 43760
% Mac:     Not tested


% NB: -R2018a flag is required
if ispc

    mex -R2018a COMPFLAGS='$COMPFLAGS /Qdiag-disable:10448 /warn:unused /O2' readunv.F .\source\read\*.F -output readunv
    mex -R2018a COMPFLAGS='$COMPFLAGS /Qdiag-disable:10448 /warn:unused /O2 /QaxAVX2 /assume:buffered_io /Qunroll-aggressive' writeunv.F .\source\write\*.F  -output writeunv

elseif ismac

    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -O2' readunv.F ./source/read/*.F -output readunv
    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -O2 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F ./source/write/*.F  -output writeunv

else % linux

    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -O2 -i8' readunv.F ./source/read/*.F -output readunv
    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -O2 -i8 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F ./source/write/*.F  -output writeunv

end


