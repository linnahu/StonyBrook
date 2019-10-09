% use a "while" loop to compute pi 
count = 0;
pi_cal =0;
n = 1;
p = [];

figure;
axis square; 
while abs(pi_cal - pi) >= 1e-3
    p(n,:) = rand (2,1);
    if p(n,1)^2 + p(n,2)^2 <= 1
        count = count +1;
        hold on;
        plot(p(n,1),p(n,2),'b.');
    else
        hold on;
        plot(p(n,1),p(n,2),'r.');
    end
    pi_cal = 4* count /n;
    n = n+1;
end

pi_cal