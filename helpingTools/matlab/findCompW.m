pr = -3;
target = 0.98;
n = 1:75;
logTargetDivOneMinusDiv =  log (target / (1-target));

wEvdEvd  = (-pr + logTargetDivOneMinusDiv) ./ n

%plot(n, wEvdEvd)
%num = exp()
%wEvdImp = 