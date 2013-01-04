addpath(genpath('ezyfit/'));
addpath(genpath('../extractTrainingData'));

weights_n  = [ 
    1.0000
    2.3500
    1.6800
    1.4700
    0.8200
    0.8400
    0.6300
    0.5200
    5.0000
    0.4400
    0.4000
    5.0000
    5.0000
    0.3200
    2.1500
    1.6900
         0
    0.5800
         0
         0
         0
         0
         0
         0
         0
         0
         0
         0
         0
         0
         0
         0
         0
         0
    5.0000  ];
%{ 
weights_n  =[  1
    1
    1
    1
    1
    1
    1
    1
    1
    1
    1
    1
    1.3000 
    1.3000 
    5.0000 
    1.5650 
    0.0000
    0.7500 
    0.0000
    0.0000 
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    0.0000
    8.0000];
    %}
    
verySmall = 0.000001;
veryLarge = 1-verySmall;

wp = -3;
wq = -3;
bestW1 = zeros(size(a,1), 1);
evdP = zeros(size(a,1), 1);
exp_wx = zeros(size(a,1), 1);
wx= zeros(size(a,1), 1);
for i = 1:size(a,1)
    n = a(i,4);
    w1 = weights_n(n);
    actP = a(i, 3);
    gtP = a(i, 2);
    gtP =  min (0.99, gtP);
    gtP =  max (0.01, gtP);
    actZ = (actP / (exp(wp)*(1-actP) ))^(1/n);
    exp_wx (i)= (exp(w1)*(1-actZ)) / (actZ - exp(w1));
    if exp_wx (i) < 0
        exp_wx (i)  = 10000000;
    end
    %evdP(i) = log(exp_wx) ;
    %evdP(i) = exp_wx / (exp_wx  + 1);
    %gtZ = (gtP / (exp(wp)*(1-gtP) ))^(1/n);
    %bestW1(i) = log(gtZ*exp_wx / (1 + exp_wx - gtZ));
    %if ~isreal(bestW1(i)) 
    %    bestW1(i) = -100;
    %end
end
%b = [a,bestW1];
%b = [a,evdP];

b = [a,exp_wx];

minDiffN = -ones(size(weights_n));
diffIndv = zeros(size(a,1), 1);
for w1 = 0.05:0.01:5
    for i = 1:size(b,1)
        n = b(i,4);
        num = exp(wp) * exp(w1*n) * (1 + b(i, 5))^n;
        denum = (exp(w1) + b(i, 5)) ^ n;
        p = num/(num + denum);
        gtProb = b(i, 2);
        gtProb = max (gtProb, 0.02);
        gtProb = min(gtProb, 0.98);
        %diffIndv(i) = abs(p-gtProb);
        diffIndv(i) = p;
    end
    
    for n =  1:size(weights_n,1)
        %maxDiff = max(diffIndv(b(:, 4) == n));
        if (size(diffIndv(b(:, 4) == n), 1) == 0)
            cor = 0;
        else
            cor = corr(b(b(:, 4) == n, 2), diffIndv(b(:, 4) == n));
        end
        if cor > minDiffN(n)
            minDiffN(n) = cor;
            weights_n (n) = w1;            
        end
    end    
end
[(1:35)', weights_n, minDiffN, orig]

%b = a(:, [2 4 35 36 1]);

%maxW1 = 40;
%maxDiff = 0.1;

%b = b(abs(b(:, 5)) <= maxW1, :);
%b = b(b(:, 3) <= maxDiff, :);

%hold off;
%scatter(b(:, 4), b(:, 5));
%hold on;

%{
c = sortrows([b(:, 2), b(:, 4)]);

weights_n = zeros(30, 1);
for i=1:30
    weights_n (i) = median(c(c(:, 1) == i, 2));
    stdv = std(c(c(:, 1) == i, 2));
    if isnan (weights_n (i))
        weights_n (i) = verySmall;
    end 
    fprintf('%2.0f  %2.4f\n', i, stdv );
end
plot(1:30, weights_n);
probs = zeros(size(a,1), 1);
for i = 1:size(a,1)
    n = a(i,4);
    %w1 = 5.7803 * n ^-1.0043;
    %w1 = n*0.12989;
    w1 = weights_n(n);
    ps = a(i, 5:(n+4));
    ps(ps == 0) = verySmall;
    ps(ps == 1) = veryLarge;
    exp_ws = ps./(1-ps);
    num = exp(wp) * exp(w1*n) * prod(1 + exp_ws);
    denum = prod(exp(w1) + exp_ws);
    probs(i) = num/(num + denum);
end

diff = probs - a(:, 3) ; 
correlation = corr(probs, a(:, 2))

%res = [a(:, 1), gt', act', gt' - act'];
%res = [a(:, 1), gt', act', gt' - act' , a(:, 4) ,a(:, 36), weights_n(a(:, 4))]
%res = [a(:, 1), gt', act', gt' - act' , a(:, 4) ,a(:, 36), weights_n(a(:, 4)), a(:, 36) - weights_n(a(:, 4)) ];

%res = [a(:, 1), gt', act', gt' - act' , a(:, 4) ,a(:, 36) ];
%}