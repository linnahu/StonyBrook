% for loop to calculate pi
n = 1000;
count = 0;
x= rand(n,1);
y= rand(n,1);
figure('color','white');
hold all

axis square;
Z = x.^2 + y.^2;
for i=1:n
    if Z(i) <= 1
        count = count + 1;
        plot(x(i),y(i),'b.');
    else
        plot (x(i),y(i),'r.');
    end
end
Pi = 4 * count / n
