% LSCATTER scatter plot with labels instead of uniform markers.
%
% Usage:
%   lscatter(x,y,l)
%   lscatter(x,y,l,options)
%
% Inputs:
%   x, y, l     x and y are equally long, numerical vectors. The
%               (x,y)-pairs represent the points that will be depicted.
%               l is a cell array or a numerical array with the same length
%               as x or y. It contains the labels that are placed at the
%               (x,y) coordinates.
%   options     This is a list of pairwise options, consisting of a keyword
%               followed by an option setting.
% 
%   One special keyword is 'MissingLabel'. It is the label that is used
%   for coordinates (x(i),y(i)) that have an empty (l(i)=='') or, if l is
%   numerical, have a NaN label. If this option is not set, then
%   coordinates with empty or NaN labels are not shown in the picture.
%   Typical choices for 'MissingLabel' are '?', '\circ', or '\bullet',
%   or similar.
% 
%   The following keywords apply to the labels and have the same meaning
%   as in Matlab's 'text' command :
%   'HorizontalAlignment','VerticalAlignment','BackgroundColor',
%   'Clipping','EdgeColor','FontAngle','FontName','FontSize''FontWeight',
%   'FontUnits','Interpreter','Margin','Position','Rotation','Units'.
% 
%   Some of the 'text'-keywords are renamed in 'lscatter':
%   ---------------   ---------------------------
%   TEXT keyword      LSCATTER keyword
%   ---------------   ---------------------------
%   'Color'           'LabelColor' or 'TextColor'
%   'LineStyle'       'EdgeStyle'
%   'LineWidth'       'EdgeWidth'
%   ---------------   ---------------------------
% 
%   The following keywords apply to the line connecting the points and
%   have the same meaning as in Matlab's 'plot' command.
%   'LineStyle','LineWidth','Marker','MarkerEdgeColor','MarkerFaceColor',
%   'MarkerSize'.
%   By default, no line is drawn. To connect the points with a line you
%   have to set the 'LineStyle' option.
%     
%   The 'Color' keyword for plot is renamed to 'LineColor' in lscatter.
%
% ------------------------------------------------------------------------
%
% Example:
% 
%     % make some data
%     x = rand(1,20)+0.2;
%     y = 1./x + randn(1,20) * 0.1;
%     years = 1991:2010;
% 
%     % now try the following:
%     figure; subplot(2,2,1); lscatter(x,y,years);
%     subplot(2,2,2); lscatter(x,y,years,'FontSize',8,'Rotation',40);
%     subplot(2,2,3); lscatter(x,y,years,'FontSize',8,'Rotation',40, ...
%         'LineStyle',':','LineColor','r');
% 
%     % remove some labels and try again
%     years(8:11) = NaN;
%     subplot(2,2,4); lscatter(x,y,years,'MissingLabel','?', ...
%         'TextColor','b','FontSize',8,'FontWeight','bold');
% 
% ------------------------------------------------------------------------
%
% See also PLOT, SCATTER, TEXT
%
% Author : Yvan Lengwiler
% Release: $1.2$
% Date   : $2010-07-01$

% History:
% 1.0  2010-06-29  First release.
% 1.1  2010-07-01  Options are now case insensitive and can be abbreviated.
% 1.2  2010-07-01  Catches ambiguous options.
% 1.3  2010-10-07  Bug fix thanks to Christ Ftaclas: before, the label
%                  vanished when any of the coordinates became zero :(
%                  Furthermore, added the undocumented 'LineSmoothing'
%                  option as a supported option.

function lscatter(x,y,l,varargin)

    % we need one-dimensional inputs
    x = x(:); y = y(:); l = l(:);
    
    % check sizes
    assert(numel(x) == numel(y) && numel(x) == numel(l), ...
        'lscatter:incompatible_args', ...
        'x, y, and l must have same number of elements.')
    
    % is l a numerical array?
    if isnumeric(l)
        isNAN = isnan(l);   % find the NaNs
        l = num2cell(l);    % transform l into a cell array
        l(isNAN) = {''};    % now replace former NaNs with empty strings
    end
    
    % parse varargin

    % --- define keywords that are recognized by 'lscatter' and how they
    %     are mapped into  keywords for 'plot' and 'text'
    AllKeys = { ...
        'MissingLabel',         '',                     'new'; ...
        'LineColor',            'Color',                'line'; ...
        'LineStyle',            'LineStyle',            'line'; ...
        'LineWidth',            'LineWidth',            'line'; ...
        'LineSmoothing',        'LineSmoothing',        'line'; ...
        'Marker',               'Marker',               'line'; ...
        'MarkerEdgeColor',      'MarkerEdgeColor',      'line'; ...
        'MarkerFaceColor',      'MarkerFaceColor',      'line'; ...
        'MarkerSize'            'MarkerSize',           'line'; ...
        'LabelColor',           'Color',                'text'; ...
        'TextColor',            'Color',                'text'; ...
        'EdgeStyle',            'LineStyle',            'text'; ...
        'EdgeWidth',            'LineWidth',            'text'; ...
        'HorizontalAlignment',  'HorizontalAlignment',  'text'; ...
        'VerticalAlignment',    'VerticalAlignment',    'text'; ...
        'BackgroundColor',      'BackgroundColor',      'text'; ...
        'Clipping',             'Clipping',             'text'; ...
        'EdgeColor',            'EdgeColor',            'text'; ...
        'FontAngle',            'FontAngle',            'text'; ...
        'FontName',             'FontName',             'text'; ...
        'FontSize',             'FontSize',             'text'; ...
        'FontWeight',           'FontWeight',           'text'; ...
        'FontUnits',            'FontUnits',            'text'; ...
        'Interpreter',          'Interpreter',          'text'; ...
        'Margin',               'Margin',               'text'; ...
        'Position',             'Position',             'text'; ...
        'Rotation',             'Rotation',             'text'; ...
        'Units'                 'Units',                'text'};
    
    % --- default values
    missmarker = ''; LineOptions = {}; TextOptions = {};

    % --- now allocate content of varargin to the categories
    while ~isempty(varargin)
        
        % args must come in pairs
        if numel(varargin) == 1
            error('lscatter:missing_option', ...
                'Optional arguments must come in pairs.');
        end
        
        % determine matching keyword
        match = strmatch(lower(varargin{1}),lower(AllKeys(:,1)),'exact');
        % ... no exact match, so check for abbreviated keywords
        if numel(match) == 0
            match = strmatch(lower(varargin{1}),lower(AllKeys(:,1)));
        end
        
        switch numel(match)
            % no match found
            case 0
                error('lscatter:illegal_arg', ...
                    'Argument ''%s'' is not allowed.', varargin{1});

            % unambiguous match found; to which category does is belong?
            case 1
                switch AllKeys{match,3}
                    case 'new'
                        missmarker = varargin{2};
                    case 'line'
                        LineOptions = [LineOptions, ...
                            AllKeys(match,2), varargin(2)];
                    case 'text'
                        TextOptions = [TextOptions, ...
                            AllKeys(match,2), varargin(2)];
                end
                
            % ambiguous match
            otherwise
                error('lscatter:ambiguous_arg', ...
                    ['Argument ''%s'' is ambiguous. It could mean ', ...
                    repmat('''%s'', ',[1 numel(match)-1]), 'or ''%s''.'], ...
                    varargin{1}, AllKeys{match,1});

        end
        
        % remove the two entries we have just dealt with
        varargin(1:2) = [];

    end
    
    % replace the missing labels with the 'missmarker' label
    isEMPTY = cellfun('isempty',l);
    l(isEMPTY) = {missmarker};
    
    % filter out the NaNs
    isok = not(any(isnan([x,y]),2));    % bug fix thanks to Christ Ftaclas
    x = x(isok); y = y(isok); l = l(isok);
    
    % produce the scatter plot

    % --- Make the plot first. This draws the line if the user has
    %     requested one, and also gets the dimensions of the plot right.
    plot(x,y,'Marker','none','LineStyle','none',LineOptions{:});

    % --- Now loop through the points and place the labels.
    for i = 1:numel(x)
        text('Position', [x(i),y(i)], ...
            'String', l(i), ...
            'HorizontalAlignment','center', ...
            'VerticalAlignment','middle', ...
            TextOptions{:});
    end

end
