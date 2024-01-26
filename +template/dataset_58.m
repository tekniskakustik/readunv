


classdef dataset_58

    properties (Constant = true)
        datasetType = 58
    end

    properties
        x                           = single.empty  % not needed if evenly spaced; if not even -> interleaved with data into Record 12 (single or double allowed)
        data                        = []            % Record 12, note: must not be empty, single/double and real/complex
        dataType                    = 'ASCII'       % write 'ASCII'-, or 'BINARY' (58b) data (not case sensitive)
        ID1                         = 'NONE'        % Record 1,            "ID Line 1"
        ID2                         = 'NONE'        % Record 2,            "ID Line 2"
        date                        = char.empty    % Record 3,            "ID Line 3", note: ID Line 3 is generally used to identify when the function was created.
        ID4                         = 'NONE'        % Record 4,            "ID Line 4"
        ID5                         = 'NONE'        % Record 5,            "ID Line 5"
        functionType                = 0             % Record 6, Field 1,   "Function Type", note: important!
        functionID                  = 0             % Record 6, Field 2,   "Function Identification Number"
        versionNumber               = 0             % Record 6, Field 3,   "Version Number, or sequence number"
        loadCaseID                  = 0             % Record 6, Field 4,   "Load Case Identification Number"
        rspEntName                  = 'NONE'        % Record 6, Field 5,   "Response Entity Name"
        rspNode                     = 0             % Record 6, Field 6,   "Response Node"
        rspDir                      = 0             % Record 6, Field 7,   "Response Direction"
        refEntName                  = 'NONE'        % Record 6, Field 8,   "Reference Entity Name"
        refNode                     = 0             % Record 6, Field 9,   "Reference Node"
        refDir                      = 0             % Record 6, Field 10,  "Reference Direction"
        x0                          = 0             % Record 7, Field 4,   "Abscissa minimum", (0.0 if spacing uneven)
        dx                          = 0             % Record 7, Field 5,   "Abscissa increment", (0.0 if spacing uneven)
        zAxisValue                  = 0             % Record 7, Field 6,   "Z-axis value", (0.0 if unused)
        abscDataType                = 0             % Record 8, Field 1,   "Specific Data Type", note: important!
        abscLengthUnitsExponent     = 0             % Record 8, Field 2,   "Length units exponent", note: only important if abscDataType is 1
        abscForceUnitsExponent      = 0             % Record 8, Field 3,   "Force units exponent", note: only important if abscDataType is 1
        abscTempUnitsExponent       = 0             % Record 8, Field 4,   "Temperature units exponent", note: only important if abscDataType is 1
        abscAxisLabel               = 'NONE'        % Record 8, Field 5,   "Axis label", 'NONE' if not used
        abscAxisUnitLabel           = 'NONE'        % Record 8, Field 6,   "Axis units label", 'NONE' if not used
        ordNumDataType              = 0             % Record 9, Field 1,   "Specific Data Type", note: important!
        ordNumLengthUnitsExponent   = 0             % Record 9, Field 2,   "Length units exponent", note: only important if ordNumDataType is 1
        ordNumForceUnitsExponent    = 0             % Record 9, Field 3,   "Force units exponent", note: only important if ordNumDataType is 1
        ordNumTempUnitsExponent     = 0             % Record 9, Field 4,   "Temperature units exponent", note: only important if ordNumDataType is 1
        ordNumAxisLabel             = 'NONE'        % Record 9, Field 5,   "Axis label", 'NONE' if not used
        ordNumAxisUnitLabel         = 'NONE'        % Record 9, Field 6,   "Axis units label", 'NONE' if not used
        ordDenomDataType            = 0             % Record 10, Field 1,  "Specific Data Type", note: if record 10 is not used, set this field to zero
        ordDenomLengthUnitsExponent = 0             % Record 10, Field 2,  "Length units exponent", note: only important if ordDenomDataType is 1
        ordDenomForceUnitsExponent  = 0             % Record 10, Field 3,  "Force units exponent", note: only important if ordDenomDataType is 1
        ordDenomTempUnitsExponent   = 0             % Record 10, Field 4,  "Temperature units exponent", note: only important if ordDenomDataType is 1
        ordDenomAxisLabel           = 'NONE'        % Record 10, Field 5,  "Axis label", 'NONE' if not used
        ordDenomAxisUnitLabel       = 'NONE'        % Record 10, Field 6,  "Axis units label", 'NONE' if not used
        zDataType                   = 0             % Record 11, Field 1,  "Specific Data Type", note: if record 11 is not used, set this field to zero
        zLengthUnitsExponent        = 0             % Record 11, Field 2,  "Length units exponent", note: only important if ordDenomDataType is 1
        zForceUnitsExponent         = 0             % Record 11, Field 3,  "Force units exponent", note: only important if ordDenomDataType is 1
        zTempUnitsExponent          = 0             % Record 11, Field 4,  "Temperature units exponent", note: only important if ordDenomDataType is 1
        zAxisLabel                  = 'NONE'        % Record 11, Field 5,  "Axis label", 'NONE' if not used
        zAxisUnitLabel              = 'NONE'        % Record 11, Field 6,  "Axis units label", 'NONE' if not used
    end

    properties (SetAccess = private)
        dataFormatType                              % Record 7, Field 1,   "Ordinate Data Type", note: determined by data
        numValues                                   % Record 7, Field 2,   "Number of data pairs for uneven abscissa spacing, or number of data values for even abscissa spacing", note: determined by data
        evenSpacing                 = []            % Record 7, Field 3,   "Abscissa Spacing", 0 = uneven, 1 = even, note: required!
    end

    methods
        function obj = dataset_58()
            obj.date = datestr(datenummx(clock));
        end

        function delete(~)
            error('dataset_58:delete', 'Not supported on value objects, use clear or similar.')
        end
    end

    methods % set methods

        % x
        function obj = set.x(obj, value)
            if ~isa(value, 'single') && ~isa(value, 'double')
                error('dataset_58:set:x', 'dataset_58.x must be either single or double.')
            else
                obj.x = value;

                if ~isempty(value)
                    if isscalar(value)
                        obj.x0 = value(1); %#ok<MCSUP>
                        obj.dx = 1; %#ok<MCSUP>
                    elseif numel(unique(diff(value))) == 1
                        % obj.evenSpacing = true; %#ok<MCSUP>
                        obj.x0 = value(1); %#ok<MCSUP>
                        obj.dx = value(2)-value(1); %#ok<MCSUP>
                    else                        
                        obj.x0 = 0; %#ok<MCSUP>
                        obj.dx = 0; %#ok<MCSUP>
                        % obj.evenSpacing = false; %#ok<MCSUP>
                    end
                else
                    obj.evenSpacing = true; %#ok<MCSUP>
                end
            end
        end

        % data
        function obj = set.data(obj, value)
            if ~isa(value, 'single') && ~isa(value, 'double')
                error('dataset_58:set:data', 'dataset_58.data must be either single or double.')
            else
                obj.data = value;

                % numValues
                n = numel(obj.data);
                if obj.evenSpacing == 0 %#ok<MCSUP>
                    n = 2*n;
                end
                obj.numValues = n; %#ok<MCSUP>

                % dataFormatType
                if ~isempty(value)
                    if ~isreal(value)
                        if isa(value, 'single')
                            n = 5;
                        else % double, real
                            n = 6;
                        end
                    elseif isa(value, 'single')
                        n = 2;
                    else % double, real
                        n = 4;
                    end
                    obj.dataFormatType = n; %#ok<MCSUP>
                end
            end
        end

        % dataType
        function obj = set.dataType(obj, value)
            if ~ischar(value) || ~any(strcmpi(value, {'ASCII', 'BINARY'}))
                error('dataset_58:set:dataType', 'dataset_58.dataType must be either ''ASCII'' or ''BINARY'' (not case-sensitive).')
            else
                obj.dataType = upper(value);
            end
        end

        % ID1
        function obj = set.ID1(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ID1', 'dataset_58.ID1 must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_58:set:ID1:too_long', 'dataset_58.ID1 is limited to 80 characters')
                end
                obj.ID1 = value(1:min(end, 80));
            end
        end

        % ID2
        function obj = set.ID2(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ID2', 'dataset_58.ID2 must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_58:set:ID2:too_long', 'dataset_58.ID2 is limited to 80 characters')
                end
                obj.ID2 = value(1:min(end, 80));
            end
        end

        % date
        function obj = set.date(obj, value)
            if ~ischar(value)
                error('dataset_58:set:date', 'dataset_58.date must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_58:set:date:too_long', 'dataset_58.date is limited to 80 characters')
                end
                obj.date = value(1:min(end, 80));
            end
        end

        % ID4
        function obj = set.ID4(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ID4', 'dataset_58.ID4 must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_58:set:ID4:too_long', 'dataset_58.ID4 is limited to 80 characters')
                end
                obj.ID4 = value(1:min(end, 80));
            end
        end

        % ID5
        function obj = set.ID5(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ID5', 'dataset_58.ID5 must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_58:set:ID5:too_long', 'dataset_58.ID5 is limited to 80 characters')
                end
                obj.ID5 = value(1:min(end, 80));
            end
        end

        % functionType
        function obj = set.functionType(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 0
                error('dataset_58:set:functionType', 'dataset_58.functionType must be a non-negative numeric scalar.')
            else
                obj.functionType = double(value);
            end
        end

        % functionID
        function obj = set.functionID(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:functionID', 'dataset_58.functionID must be a numeric scalar.')
            else
                obj.functionID = double(value);
            end
        end

        % versionNumber
        function obj = set.versionNumber(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:versionNumber', 'dataset_58.versionNumber must be a numeric scalar.')
            else
                obj.versionNumber = double(value);
            end
        end

        % loadCaseID
        function obj = set.loadCaseID(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:loadCaseID', 'dataset_58.loadCaseID must be a numeric scalar.')
            else
                obj.loadCaseID = double(value);
            end
        end

        % rspEntName
        function obj = set.rspEntName(obj, value)
            if ~ischar(value)
                error('dataset_58:set:rspEntName', 'dataset_58.rspEntName must be a char.')
            else
                if numel(value) > 10
                    warning('dataset_58:set:rspEntName:too_long', 'dataset_58.rspEntName is limited to 10 characters')
                end
                obj.rspEntName = value(1:min(end, 10));
            end
        end

        % rspNode
        function obj = set.rspNode(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:rspNode', 'dataset_58.rspNode must be a numeric scalar.')
            else
                obj.rspNode = double(round(value));
            end
        end

        % rspDir
        function obj = set.rspDir(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < -6 || value > 6
                error('dataset_58:set:rspDir', 'dataset_58.rspDir must be a numeric scalar, -6:6.')
            else
                obj.rspDir = double(round(value));
            end
        end

        % refEntName
        function obj = set.refEntName(obj, value)
            if ~ischar(value)
                error('dataset_58:set:refEntName', 'dataset_58.refEntName must be a char.')
            else
                if numel(value) > 10
                    warning('dataset_58:set:refEntName:too_long', 'dataset_58.refEntName is limited to 10 characters')
                end
                obj.refEntName = value(1:min(end, 10));
            end
        end

        % refNode
        function obj = set.refNode(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:refNode', 'dataset_58.refNode must be a numeric scalar.')
            else
                obj.refNode = double(round(value));
            end
        end

        % refDir
        function obj = set.refDir(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < -6 || value > 6
                error('dataset_58:set:refDir', 'dataset_58.refDir must be a numeric scalar, -6:6.')
            else
                obj.refDir = double(round(value));
            end
        end

        % x0
        function obj = set.x0(obj, value)
            if ~isa(value, 'single') && ~isa(value, 'double') || ~isscalar(value) || isempty(value)
                error('dataset_58:set:x0', 'dataset_58.x0 must be either single or double, scalar and non-empty.')
            else
                obj.x0 = value;
            end
        end

        % dx
        function obj = set.dx(obj, value)
            if ~isa(value, 'single') && ~isa(value, 'double') || ~isscalar(value) || isempty(value)
                error('dataset_58:set:dx', 'dataset_58.dx must be either single or double, scalar and non-empty.')
            else
                obj.dx = value;

                obj.evenSpacing = value ~= 0; %#ok<MCSUP>
            end
        end

        % zAxisValue
        function obj = set.zAxisValue(obj, value)
            if ~isa(value, 'single') && ~isa(value, 'double') || ~isscalar(value)
                error('dataset_58:set:zAxisValue', 'dataset_58.zAxisValue must be either single or double, scalar and non-empty.')
            else
                if isempty(value)
                    obj.zAxisValue = 0;
                else
                    obj.zAxisValue = value;
                end
            end
        end

        % abscDataType
        function obj = set.abscDataType(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 0
                error('dataset_58:set:abscDataType', 'dataset_58.abscDataType must be a non-negative numeric scalar.')
            else
                obj.abscDataType = double(value);
            end
        end

        % abscLengthUnitsExponent
        function obj = set.abscLengthUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:abscLengthUnitsExponent', 'dataset_58.abscLengthUnitsExponent must be a numeric scalar.')
            else
                obj.abscLengthUnitsExponent = double(value);
            end
        end

        % abscForceUnitsExponent
        function obj = set.abscForceUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:abscForceUnitsExponent', 'dataset_58.abscForceUnitsExponent must be a numeric scalar.')
            else
                obj.abscForceUnitsExponent = double(value);
            end
        end

        % abscTempUnitsExponent
        function obj = set.abscTempUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:abscTempUnitsExponent', 'dataset_58.abscTempUnitsExponent must be a numeric scalar.')
            else
                obj.abscTempUnitsExponent = double(value);
            end
        end

        % abscAxisLabel
        function obj = set.abscAxisLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:abscAxisLabel', 'dataset_58.abscAxisLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:abscAxisLabel:too_long', 'dataset_58.abscAxisLabel is limited to 20 characters')
                end
                obj.abscAxisLabel = value(1:min(end, 20));
            end
        end

        % abscAxisUnitLabel
        function obj = set.abscAxisUnitLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:abscAxisUnitLabel', 'dataset_58.abscAxisUnitLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:abscAxisUnitLabel:too_long', 'dataset_58.abscAxisUnitLabel is limited to 20 characters')
                end
                obj.abscAxisUnitLabel = value(1:min(end, 20));
            end
        end

        % ordNumDataType
        function obj = set.ordNumDataType(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 0
                error('dataset_58:set:ordNumDataType', 'dataset_58.ordNumDataType must be a non-negative numeric scalar.')
            else
                obj.ordNumDataType = double(value);
            end
        end

        % ordNumLengthUnitsExponent
        function obj = set.ordNumLengthUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:ordNumLengthUnitsExponent', 'dataset_58.ordNumLengthUnitsExponent must be a numeric scalar.')
            else
                obj.ordNumLengthUnitsExponent = double(value);
            end
        end

        % ordNumForceUnitsExponent
        function obj = set.ordNumForceUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:ordNumForceUnitsExponent', 'dataset_58.ordNumForceUnitsExponent must be a numeric scalar.')
            else
                obj.ordNumForceUnitsExponent = double(value);
            end
        end

        % ordNumTempUnitsExponent
        function obj = set.ordNumTempUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:ordNumTempUnitsExponent', 'dataset_58.ordNumTempUnitsExponent must be a numeric scalar.')
            else
                obj.ordNumTempUnitsExponent = double(value);
            end
        end

        % ordNumAxisLabel
        function obj = set.ordNumAxisLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ordNumAxisLabel', 'dataset_58.ordNumAxisLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:ordNumAxisLabel:too_long', 'dataset_58.ordNumAxisLabel is limited to 20 characters')
                end
                obj.ordNumAxisLabel = value(1:min(end, 20));
            end
        end

        % ordNumAxisUnitLabel
        function obj = set.ordNumAxisUnitLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ordNumAxisUnitLabel', 'dataset_58.ordNumAxisUnitLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:ordNumAxisUnitLabel:too_long', 'dataset_58.ordNumAxisUnitLabel is limited to 20 characters')
                end
                obj.ordNumAxisUnitLabel = value(1:min(end, 20));
            end
        end

        % ordDenomDataType
        function obj = set.ordDenomDataType(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 0
                error('dataset_58:set:ordDenomDataType', 'dataset_58.ordDenomDataType must be a non-negative numeric scalar.')
            else
                obj.ordDenomDataType = double(value);
            end
        end

        % ordDenomLengthUnitsExponent
        function obj = set.ordDenomLengthUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:ordDenomLengthUnitsExponent', 'dataset_58.ordDenomLengthUnitsExponent must be a numeric scalar.')
            else
                obj.ordDenomLengthUnitsExponent = double(value);
            end
        end

        % ordDenomForceUnitsExponent
        function obj = set.ordDenomForceUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:ordDenomForceUnitsExponent', 'dataset_58.ordDenomForceUnitsExponent must be a numeric scalar.')
            else
                obj.ordDenomForceUnitsExponent = double(value);
            end
        end

        % ordDenomTempUnitsExponent
        function obj = set.ordDenomTempUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:ordDenomTempUnitsExponent', 'dataset_58.ordDenomTempUnitsExponent must be a numeric scalar.')
            else
                obj.ordDenomTempUnitsExponent = double(value);
            end
        end

        % ordDenomAxisLabel
        function obj = set.ordDenomAxisLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ordDenomAxisLabel', 'dataset_58.ordDenomAxisLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:ordDenomAxisLabel:too_long', 'dataset_58.ordDenomAxisLabel is limited to 20 characters')
                end
                obj.ordDenomAxisLabel = value(1:min(end, 20));
            end
        end

        % ordDenomAxisUnitLabel
        function obj = set.ordDenomAxisUnitLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:ordDenomAxisUnitLabel', 'dataset_58.ordDenomAxisUnitLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:ordDenomAxisUnitLabel:too_long', 'dataset_58.ordDenomAxisUnitLabel is limited to 20 characters')
                end
                obj.ordDenomAxisUnitLabel = value(1:min(end, 20));
            end
        end

        % zDataType
        function obj = set.zDataType(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 0
                error('dataset_58:set:zDataType', 'dataset_58.zDataType must be a non-negative numeric scalar.')
            else
                obj.zDataType = double(value);
            end
        end

        % zLengthUnitsExponent
        function obj = set.zLengthUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:zLengthUnitsExponent', 'dataset_58.zLengthUnitsExponent must be a numeric scalar.')
            else
                obj.zLengthUnitsExponent = double(value);
            end
        end

        % zForceUnitsExponent
        function obj = set.zForceUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:zForceUnitsExponent', 'dataset_58.zForceUnitsExponent must be a numeric scalar.')
            else
                obj.zForceUnitsExponent = double(value);
            end
        end

        % zTempUnitsExponent
        function obj = set.zTempUnitsExponent(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_58:set:zTempUnitsExponent', 'dataset_58.zTempUnitsExponent must be a numeric scalar.')
            else
                obj.zTempUnitsExponent = double(value);
            end
        end

        % zAxisLabel
        function obj = set.zAxisLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:zAxisLabel', 'dataset_58.zAxisLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:zAxisLabel:too_long', 'dataset_58.zAxisLabel is limited to 20 characters')
                end
                obj.zAxisLabel = value(1:min(end, 20));
            end
        end
        
        % zAxisUnitLabel
        function obj = set.zAxisUnitLabel(obj, value)
            if ~ischar(value)
                error('dataset_58:set:zAxisUnitLabel', 'dataset_58.zAxisUnitLabel must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_58:set:zAxisUnitLabel:too_long', 'dataset_58.zAxisUnitLabel is limited to 20 characters')
                end
                obj.zAxisUnitLabel = value(1:min(end, 20));
            end
        end

    end

end


