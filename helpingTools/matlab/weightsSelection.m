%wq = -3.5;
%wp = -2.5;
%w1 = 0.71;
%n = 10;

bestWq = 0;
bestWp = 0;
%minMaxMinMax = 0.058194934579934;
minMaxMinMax = 1;
wp = -4
wq = -4
%for wq = 0.5:-0.1:-6
%    for wp = 0:-0.1:-6
        for n = 1:30
            m = 0:n;   
            minMaxDiff(n) = 10;
            bestW1(n) = 0;    
            for w1 = 6:-0.001:0
                y = exp (wp) * exp(w1*n) * (  (1+exp(wq)) /   (exp(w1) + exp(wq))  ).^(n-m);
                x = y./(1+y);
                gt = 0:1/n:1;
                diff = x-gt;
                maxDiff = max(abs(diff));
                if maxDiff < minMaxDiff(n)
                    minMaxDiff(n) = maxDiff; 
                    bestW1(n) = w1;
                end
            end
            %w1 = fitted(n);
            y = exp (wp) * exp(w1*n) * (  (1+exp(wq)) /   (exp(w1) + exp(wq))  ).^(n-m);
            x = y./(1+y);
            gt = 0:1/n:1;
            diff = x-gt;
            maxDiff = max(abs(diff));
            fprintf('wp=%1.1f, wq=%1.1f, n=%d, w1=%0.4f, wF=%0.4f, maxDiff=%0.4f, maxDiffFit=%0.4f\n',wp, wq,n, bestW1(n), w1, minMaxDiff(n), maxDiff );
            %if minMaxDiff(n) > minMaxMinMax
%                break;
%            end
        end
%         maxMinMax = max(minMaxDiff); 
%         if maxMinMax < minMaxMinMax 
%             minMaxMinMax  = maxMinMax;
%             bestWq = wq
%             bestWp = wp
%         end
%    end
%end
