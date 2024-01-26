


function dataset = createTemplate(casenum)


switch casenum
    case 164
        dataset = template.dataset_164();

    case 151
        dataset = template.dataset_151();

    case 58
        dataset = template.dataset_58();

    otherwise
        error('createTemplate:unsupportedCaseNum', 'Only UNV types 58, 151 and 164 are currently supported.')

end


end


