


function dataset = createTemplate(casenum)

switch casenum
    case 164
        dataset = struct(...
            'datasetType', 164, ...
            'unitsCode', 1, ...
            'temperatureMode', 1 ...
            );

    case 151
        dataset = struct(...
            'datasetType', 151, ...
            'modelName', '', ...
            'modelDescription', 'NONE', ...
            'databaseProgram', '', ...
            'databaseVersion1', 0, ...
            'databaseVersion2', 0, ...
            'unvProgram', '', ...
            'databaseCreatedTime', datestr(datenummx(clock), 13), ...
            'databaseCreatedDate', datestr(datenummx(clock), 'DD-mmm-YY'), ...
            'fileCreatedDate', datestr(datenummx(clock), 'DD-mmm-YY'), ...
            'fileCreatedTime', datestr(datenummx(clock), 13) ...
            ); %#ok<DATST>

    case 58
        dataset = struct(...
            'datasetType', 58, ...                  % required
            'x', [], ...                            % not needed if evenly spaced; if not even -> mixed with data into Record 12
            'data', [], ...                         % Record 12, note: can't be empty
            'ID1', 'NONE',...                       % Record 1, "ID Line 1"
            'ID2', 'NONE',...                       % Record 2, "ID Line 2"
            'date', datestr(datenummx(clock)), ...  % Record 3, "ID Line 3", note: ID Line 3 is generally used to identify when the function was created.
            'ID4', 'NONE',...                       % Record 4, "ID Line 4"
            'ID5', 'NONE',...                       % Record 5, "ID Line 5"
            'functionType', 0, ...                  % Record 6, Field 1, "Function Type", note: important!
            'functionID', 0, ...                    % Record 6, Field 2, "Function Identification Number"
            'versionNumber', 0, ...                 % Record 6, Field 3, "Version Number, or sequence number"
            'loadCaseID', 0, ...                    % Record 6, Field 4, "Load Case Identification Number"
            'rspEntName', 'NONE', ...               % Record 6, Field 5, "Response Entity Name"
            'rspNode', 0, ...                       % Record 6, Field 6, "Response Node"
            'rspDir', 0, ...                        % Record 6, Field 7, "Response Direction"
            'refEntName', 'NONE', ...               % Record 6, Field 8, "Reference Entity Name"
            'refNode', 0, ...                       % Record 6, Field 9, "Reference Node"
            'refDir', 0, ...                        % Record 6, Field 10, "Reference Direction"
            'dataFormatType', [], ...               % Record 7, Field 1, "Ordinate Data Type", note: determined by data -> ignore
            'numValues', [], ...                    % Record 7, Field 2, "Number of data pairs for uneven abscissa spacing, or number of data values for even abscissa spacing", note: determined by data -> ignore
            'evenSpacing', [], ...                  % Record 7, Field 3, "Abscissa Spacing", 0 = uneven, 1 = even, note: required!
            'x0', 0, ...                            % Record 7, Field 4, "Abscissa minimum", (0.0 if spacing uneven)
            'dx', 0, ...                            % Record 7, Field 5, "Absciissa increment", (0.0 if spacing uneven)
            'zAxisValue', 0, ...                    % Record 7, Field 6, "Z-axis value", (0.0 if unused)
            'abscDataType', 0, ...                  % Record 8, Field 1, "Specific Data Type", note: important!
            'abscLengthUnitsExponent', 0, ...       % Record 8, Field 2, "Length units exponent", note: only important if abscDataType is 1
            'abscForceUnitsExponent', 0, ...        % Record 8, Field 3, "Force units exponent", note: only important if abscDataType is 1
            'abscTempUnitsExponent', 0, ...         % Record 8, Field 4, "Temperature units exponent", note: only important if abscDataType is 1
            'abscAxisLabel', 'NONE', ...            % Record 8, Field 5, "Axis label", 'NONE' if not used
            'abscAxisUnitLabel', 'NONE', ...        % Record 8, Field 6, "Axis units label", 'NONE' if not used
            'ordNumDataType', 0, ...                % Record 9, Field 1, "Specific Data Type", note: important!
            'ordNumLengthUnitsExponent', 0, ...     % Record 9, Field 2, "Length units exponent", note: only important if ordNumDataType is 1
            'ordNumForceUnitsExponent', 0, ...      % Record 9, Field 3, "Force units exponent", note: only important if ordNumDataType is 1
            'ordNumTempUnitsExponent', 0, ...       % Record 9, Field 4, "Temperature units exponent", note: only important if ordNumDataType is 1
            'ordNumAxisLabel', 'NONE', ...          % Record 9, Field 5, "Axis label", 'NONE' if not used
            'ordNumAxisUnitLabel', 'NONE', ...      % Record 9, Field 6, "Axis units label", 'NONE' if not used
            'ordDenomDataType', 0, ...              % Record 10, Field 1, "Specific Data Type", note: if record 10 is not used, set this field to zero
            'ordDenomLengthUnitsExponent', 0, ...   % Record 10, Field 2, "Length units exponent", note: only important if ordDenomDataType is 1
            'ordDenomForceUnitsExponent', 0, ...    % Record 10, Field 3, "Force units exponent", note: only important if ordDenomDataType is 1
            'ordDenomTempUnitsExponent', 0, ...     % Record 10, Field 4, "Temperature units exponent", note: only important if ordDenomDataType is 1
            'ordDenomAxisLabel', 'NONE', ...        % Record 10, Field 5, "Axis label", 'NONE' if not used
            'ordDenomAxisUnitLabel', 'NONE', ...    % Record 10, Field 6, "Axis units label", 'NONE' if not used
            'zDataType', 0, ...                     % Record 11, Field 1, "Specific Data Type", note: if record 11 is not used, set this field to zero
            'zLengthUnitsExponent', 0, ...          % Record 11, Field 2, "Length units exponent", note: only important if ordDenomDataType is 1
            'zForceUnitsExponent', 0, ...           % Record 11, Field 3, "Force units exponent", note: only important if ordDenomDataType is 1
            'zTempUnitsExponent', 0, ...            % Record 11, Field 4, "Temperature units exponent", note: only important if ordDenomDataType is 1
            'zAxisLabel', 'NONE', ...               % Record 11, Field 5, "Axis label", 'NONE' if not used
            'zAxisUnitLabel', 'NONE'  ...           % Record 11, Field 6, "Axis units label", 'NONE' if not used
            ); %#ok<DATST>


    otherwise
        error('createTemplate:unsupportedCaseNum', 'Only UNV types 58, 151 and 164 are currently supported.')

end

end


