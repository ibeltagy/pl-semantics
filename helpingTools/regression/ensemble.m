gt;
wordGram;
act;

%basic
features = [wordGram];
model = mvregress( features(1:750, :), gt(1:750)');
basicTrainCorr = corr(features(1:750, :)*model, gt(1:750)')
basicTestCorr = corr(features(751:1500, :)*model, gt(751:1500)')

%MLN
features = [act'];
model = mvregress( features(1:750, :), gt(1:750)');
mlnTrainCorr = corr(features(1:750, :)*model, gt(1:750)')
mlnTestCorr = corr(features(751:1500, :)*model, gt(751:1500)')

%compined
features = [wordGram, act'];
model = mvregress( features(1:750, :), gt(1:750)');
compTrainCorr = corr(features(1:750, :)*model, gt(1:750)')
compTestCorr = corr(features(751:1500, :)*model, gt(751:1500)')