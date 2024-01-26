

% Tested with:
% Windows: Matlab R2023a update 6, Intel Fortran Compiler (ifort) version 2021.10.0, Visual Studio 2022 (17.8.4), Windows 11 22H2 Build 23619
% Linux:   Matlab R2023a update 6, Intel Fortran Compiler (ifort) version 2021.10.0, GCC 13.2.1                   Clear Linux 40680
% Mac:     Matlab R2022b update 0, Intel Fortran Compiler (ifort) version 2021.7.0,  Xcode 14.0.1,                Macos 12.6


% NB: -R2018a flag is required
if ispc
    
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /O2' readunv.F .\source\read\*.F -output readunv
    mex -R2018a COMPFLAGS='$COMPFLAGS /warn:unused /O2 /QaxAVX2 /assume:buffered_io /Qunroll-aggressive' writeunv.F .\source\write\*.F  -output writeunv

elseif ismac

    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -O2' readunv.F ./source/read/*.F -output readunv
    mex -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -warn unused -O2 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F ./source/write/*.F  -output writeunv

else % linux

    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -O2 -i8' readunv.F ./source/read/*.F -output readunv
    mex FC='ifort' -R2018a FOPTIMFLAGS='$FOPTIMFLAGS -O2 -i8 -axAVX2 -assume buffered_io -unroll-aggressive' writeunv.F ./source/write/*.F  -output writeunv

end


