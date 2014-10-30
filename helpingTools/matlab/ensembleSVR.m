gt;
wordGram;
act;

%basic
features = [wordGram]/5;
model = svmtrain(gt(1:750)', features(1:750, :), '-s 3');
p = svmpredict(gt(1:750)', features(1:750, :), model);
basicTrainCorr = corr (gt(1:750)', p)
p = svmpredict(gt(751:1500)', features(751:1500, :), model);
basicTestCorr = corr (gt(751:1500)', p)
%MLN
features = [act']/5;
model = svmtrain(gt(1:750)', features(1:750, :), '-s 3');
p = svmpredict(gt(1:750)', features(1:750, :), model);
mlnTrainCorr = corr (gt(1:750)', p)
p = svmpredict(gt(751:1500)', features(751:1500, :), model);
mlnTestCorr = corr (gt(751:1500)', p)

%compined
features = [wordGram, act']/5;
model = svmtrain(gt(1:750)', features(1:750, :), '-s 3');
p = svmpredict(gt(1:750)', features(1:750, :), model);
combTrainCorr = corr (gt(1:750)', p)
p = svmpredict(gt(751:1500)', features(751:1500, :), model);
combTestCorr = corr (gt(751:1500)', p)
