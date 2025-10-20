


classdef dataset_151

    properties (Constant = true)
        datasetType = 151
    end

    properties
        modelName               = char.empty()  % 1,    model file name
        modelDescription        = 'NONE'        % 2,    model file description
        databaseProgram         = char.empty()  % 3,    program which created DB
        databaseCreatedDate     = char.empty()  % 4.1,  date database created (DD-MMM-YY)
        databaseCreatedTime     = char.empty()  % 4.2,  time database created (HH:MM:SS)
        databaseVersion1        = 0             % 4.3,  Version from database
        databaseVersion2        = 0             % 4.4,  Version from database
        fileType                = 0             % 4.5,  File type (0 = Universal, 1 = Archive, 2 = Other)
        databaseLastSavedDate   = char.empty()  % 5.1,  date database last saved (DD-MMM-YY)
        databaseLastSavedTime   = char.empty()  % 5.2,  time database last saved (HH:MM:SS)
        unvProgram              = char.empty()  % 6,    program which created universal file
        fileCreatedDate         = char.empty()  % 7.1,  date universal file written (DD-MMM-YY)
        fileCreatedTime         = char.empty()  % 7.2,  time universal file written (HH:MM:SS)
    end

    methods
        function obj = dataset_151()
            t = datenummx(clock);
            current_date = datestr(t, 'DD-mmm-YY');
            current_time = datestr(t, 13);

            obj.databaseCreatedDate = current_date;
            obj.databaseCreatedTime = current_time;

            obj.databaseLastSavedDate = current_date;
            obj.databaseLastSavedTime = current_time;

            obj.fileCreatedDate = current_date;
            obj.fileCreatedTime = current_time;
        end

        function delete(~)
            error('dataset_151:delete', 'Not supported on value objects, use clear or similar.')
        end
    end

    methods % set methods

        % modelName
        function obj = set.modelName(obj, value)
            if ~ischar(value)
                error('dataset_151:set:modelName', 'dataset_151.modelName must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_151:set:modelName:too_long', 'dataset_151.modelName is limited to 80 characters')
                end
                obj.modelName = value(1:min(end, 80));
            end
        end

        % modelDescription
        function obj = set.modelDescription(obj, value)
            if ~ischar(value)
                error('dataset_151:set:modelDescription', 'dataset_151.modelDescription must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_151:set:modelDescription:too_long', 'dataset_151.modelDescription is limited to 80 characters')
                end
                obj.modelDescription = value(1:min(end, 80));
            end
        end

        % databaseProgram
        function obj = set.databaseProgram(obj, value)
            if ~ischar(value)
                error('dataset_151:set:databaseProgram', 'dataset_151.databaseProgram must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_151:set:databaseProgram:too_long', 'dataset_151.databaseProgram is limited to 80 characters')
                end
                obj.databaseProgram = value(1:min(end, 80));
            end
        end

        % databaseCreatedDate
        function obj = set.databaseCreatedDate(obj, value)
            if ~ischar(value) || numel(value) ~= 9
                error('dataset_151:set:databaseCreatedDate', 'dataset_151.databaseCreatedDate must be a char of length 9 (DD-MMM-YY).')
            else
                obj.databaseCreatedDate = value;
            end
        end

        % databaseCreatedDate
        function obj = set.databaseCreatedTime(obj, value)
            if ~ischar(value) || numel(value) ~= 8
                error('dataset_151:set:databaseCreatedTime', 'dataset_151.databaseCreatedTime must be a char of length 8 (HH:MM:SS).')
            else
                obj.databaseCreatedTime = value;
            end
        end

        % databaseVersion1
        function obj = set.databaseVersion1(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_151:set:databaseVersion1', 'dataset_151.databaseVersion1 must be a numeric scalar.')
            else
                obj.databaseVersion1 = double(value);
            end
        end

        % databaseVersion2
        function obj = set.databaseVersion2(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_151:set:databaseVersion2', 'dataset_151.databaseVersion2 must be a numeric scalar.')
            else
                obj.databaseVersion2 = double(value);
            end
        end

        % fileType
        function obj = set.fileType(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 0 || value > 2
                error('dataset_151:set:fileType', 'dataset_151.fileType must be a numeric scalar, range 0:2.')
            else
                obj.fileType = double(round(value));
            end
        end

        % databaseLastSavedDate
        function obj = set.databaseLastSavedDate(obj, value)
            if ~ischar(value) || numel(value) ~= 9
                error('dataset_151:set:databaseLastSavedDate', 'dataset_151.databaseLastSavedDate must be a char of length 9 (DD-MMM-YY).')
            else
                obj.databaseLastSavedDate = value;
            end
        end

        % databaseLastSavedTime
        function obj = set.databaseLastSavedTime(obj, value)
            if ~ischar(value) || numel(value) ~= 8
                error('dataset_151:set:databaseLastSavedTime', 'dataset_151.databaseLastSavedTime must be a char of length 8 (HH:MM:SS).')
            else
                obj.databaseLastSavedTime = value;
            end
        end

        % unvProgram
        function obj = set.unvProgram(obj, value)
            if ~ischar(value)
                error('dataset_151:set:unvProgram', 'dataset_151.unvProgram must be a char.')
            else
                if numel(value) > 80
                    warning('dataset_151:set:unvProgram:too_long', 'dataset_151.unvProgram is limited to 80 characters')
                end
                obj.unvProgram = value(1:min(end, 80));
            end
        end

        % fileCreatedDate
        function obj = set.fileCreatedDate(obj, value)
            if ~ischar(value) || numel(value) ~= 9
                error('dataset_151:set:fileCreatedDate', 'dataset_151.fileCreatedDate must be a char of length 9 (DD-MMM-YY).')
            else
                obj.fileCreatedDate = value;
            end
        end

        % fileCreatedTime
        function obj = set.fileCreatedTime(obj, value)
            if ~ischar(value) || numel(value) ~= 8
                error('dataset_151:set:fileCreatedTime', 'dataset_151.fileCreatedTime must be a char of length 8 (HH:MM:SS).')
            else
                obj.fileCreatedTime = value;
            end
        end

    end

end


