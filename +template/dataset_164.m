


classdef dataset_164

    properties (Constant = true)
        datasetType = 164
    end

    properties
        unitsCode           = 1     % 1.1, units code, see documentation, valid range 1:9
        unitsDescription    = ''    % 1.2, units description (used for documentation only)
        temperatureMode     = 1     % 1.3, temperature mode: 1 = absolute, 2 = relative
        length              = 1     % 2.1, length factor to SI
        force               = 1     % 2.2, force factor to SI
        temperature         = 1     % 2.3, temperature factor to SI
        temperatureOffset   = 0     % 2.4, temperature offset to SI
    end    

    methods
        function obj = dataset_164()
            % do nothing special
        end

        function delete(~)
            error('dataset_164:delete', 'Not supported on value objects, use clear or similar.')
        end
    end

    methods % set methods
        
        % unitsCode
        function obj = set.unitsCode(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 1 || value > 9
                error('dataset_164:set:unitsCode', 'dataset_164.unitsCode must be a numeric scalar 1-9.')
            else
                obj.unitsCode = round(double(value));
            end
        end

        % unitsDescription
        function obj = set.unitsDescription(obj, value)
            if ~ischar(value)
                error('dataset_164:set:unitsDescription', 'dataset_164.unitsDescription must be a char.')
            else
                if numel(value) > 20
                    warning('dataset_164:set:unitsDescription:too_long', 'dataset_164.unitsDescription is limited to 20 characters')
                end
                obj.unitsDescription = value(1:min(end, 20));
            end
        end

        % temperatureMode
        function obj = set.temperatureMode(obj, value)
            if ~isnumeric(value) || ~isscalar(value) || value < 1 || value > 2
                error('dataset_164:set:temperatureMode', 'dataset_164.temperatureMode must be a numeric scalar 1-2.')
            else
                obj.temperatureMode = round(double(value));
            end
        end

        % length
        function obj = set.length(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_164:set:length', 'dataset_164.length must be a numeric scalar.')
            else
                obj.length = double(value);
            end
        end

        % force
        function obj = set.force(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_164:set:force', 'dataset_164.force must be a numeric scalar.')
            else
                obj.force = double(value);
            end
        end

        % temperature
        function obj = set.temperature(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_164:set:temperature', 'dataset_164.temperature must be a numeric scalar.')
            else
                obj.temperature = double(value);
            end
        end

        % temperatureOffset
        function obj = set.temperatureOffset(obj, value)
            if ~isnumeric(value) || ~isscalar(value)
                error('dataset_164:set:temperatureOffset', 'dataset_164.temperatureOffset must be a numeric scalar.')
            else
                obj.temperatureOffset = double(value);
            end
        end

    end

end


