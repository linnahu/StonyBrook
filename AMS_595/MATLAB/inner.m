function [ product ] = inner( v1,v2,m )
product = 0;
n1 = size(v1);
n2 = size(v2);
nm = size(m);
v3 = zeros(n2)';

if n1(1) ~= 1 || n2(2) ~= 1 || n1(2) ~= na(1) || na(2) ~= n2(1)
    disp('The sizes of vectors and matrix are not matched');
    product = NaN;
    return;
else
    for j = 1 : n2(1)
        for i = 1 : n1(2)
            v3(j) = v3(j) + v1(i) * m(i,j);
        end
    end
    for k = 1 : n2(1)
        product = product + v3(k) * v2(k);
    end   
end
end
