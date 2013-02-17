%given: n, gt, act

act = actVidMergeWPhrase;% actVidWeightNoMergeWithRelNewparserParam ;
n = nMerge';% nNoMerge;
pr = -3;
target = 0.90;
%n = 1:75;
logTargetDivOneMinusDiv =  log (target / (1-target));

usedW  = (-pr + logTargetDivOneMinusDiv) ./ n;
changedAct = act;
changedAct = changedAct/5;
changedAct  = min(max (changedAct, 0.01), 0.99);
changedAct  = changedAct ./ (1-changedAct);
Z = log(changedAct);
actualN = (-pr + Z')./usedW ;

changedGt = gt;
changedGt = changedGt/5;
changedGt  = min(max (changedGt, 0.01), 0.99);
changedGt  = changedGt ./ (1-changedGt);
Z = log(changedGt);

newW = (-pr + Z')./actualN;

minW = 0.08;
maxW = 3;
changedNewW = min(max(newW, minW), maxW);
expectedAct = exp(pr + changedNewW .* actualN);
expectedAct = expectedAct./(1+expectedAct)


%bestW = changedNewW;
bestCorr = 0;
bestQ = 0;
for q = 0:0.01:1
    medianNewW = changedNewW;
    %Group BY N
    for i = 1:max(n)
        selectedN = changedNewW;
        selectedN = selectedN(n == i);
        medianAll = quantile(selectedN, q);
        stdvAll = std(selectedN);

        selectedN = changedNewW(1:750);
        selectedN = selectedN(n(1:750) == i);
        medianTrain = quantile(selectedN, q);
        stdvTrain = std(selectedN);

        selectedN = changedNewW(751:1500);
        selectedN = selectedN(n(751:1500) == i);
        medianTest = quantile(selectedN, q);
        stdvTest = std(selectedN);
        %fprintf('%d: %2.2f,%2.2f,%2.2f -- %2.2f,%2.2f,%2.2f\n', i, ... 
        %           medianAll,medianTrain, medianTest,  ...
        %           stdvAll, stdvTrain, stdvTest   );
        medianTrain(isnan(medianTrain)) = 0;
        %fprintf('%2.2f, \n', medianTrain);

        medianNewW(n == i) = medianTrain;
    end

    expectedMedianAct = exp(pr + medianNewW.* actualN);
    expectedMedianAct = expectedMedianAct./(1+expectedMedianAct);
    c = corr(gt(1:750)', expectedMedianAct(1:750));
    if c > bestCorr
        bestCorr = c;
        bestQ = q;
        bestExpectedMedianAct = expectedMedianAct;
    end
end

bestQ
bestCorr
corr(gt', bestExpectedMedianAct)
corr(gt(751:1500)', bestExpectedMedianAct(751:1500))
scatter(gt', 5*bestExpectedMedianAct)
line ([0 5], [0 5])
