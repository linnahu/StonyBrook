function [ Live ] = y( M )

[m n] = size(M);

for i = 2 : m-1
    for j = 2 :n-1 
        neighbor_m = [M(i-1,j-1),M(i-1,j),M(i-1,j+1),M(i,j-1),M(i,j+1),M(i+1,j-1),M(i+1,j),M(i+1,j+1)];
        Live(i,j) = sum(neighbor_m == 1);
    end
    neighbor_l = [M(i-1,1),M(i-1,2),M(i,2),M(i+1,1),M(i+1,2)];
    Live(i,1) = sum(neighbor_l == 1);
    neighbor_r = [M(i-1,n-1),M(i-1,n),M(i,n-1),M(i+1,n-1),M(i+1,n)];
    Live(i,n) = sum(neighbor_r == 1);
end
for j = 2:n-1
    neighbor_u = [M(1,j-1),M(1,j+1),M(2,j-1),M(2,j),M(2,j+1)];
    Live(1,j) = sum(neighbor_u == 1);
end
for j = 2:n-1
    neighbor_d = [M(m-1,j-1),M(m-1,j),M(m-1,j+1),M(m,j-1),M(m,j+1)];
    Live(m,j) = sum(neighbor_d == 1);
end
Live(1,1) = sum([M(1,2),M(2,1),M(2,2)] == 1);
Live(1,n) = sum([M(1,n-1),M(2,n-1),M(2,n)] == 1);
Live(m,1) = sum([M(m-1,1),M(m-1,2),M(m,2)] == 1);
Live(m,n) = sum([M(m-1,n-1),M(m-1,n),M(m,n-1)] == 1);     
end


function [ Die ] = X(M)
[m n] = size(M);
for i = 2 : m-1
    for j = 2 :n-1 
        neighbor_m = [M(i-1,j-1),M(i-1,j),M(i-1,j+1),M(i,j-1),M(i,j+1),M(i+1,j-1),M(i+1,j),M(i+1,j+1)];
        Die(i,j) = sum(neighbor_m == 0);
    end
    neighbor_l = [M(i-1,1),M(i-1,2),M(i,2),M(i+1,1),M(i+1,2)];
    Die(i,1) = sum(neighbor_l == 0) + 3;
    neighbor_n = [M(i-1,n-1),M(i-1,n),M(i,n-1),M(i+1,n-1),M(i+1,n)];
    Die(i,n) = sum(neighbor_n == 0) + 3;
end
for j = 2:n-1
    neighbor_u = [M(1,j-1),M(1,j+1),M(2,j-1),M(2,j),M(2,j+1)];
    Die(1,j) = sum(neighbor_u == 0) + 3;
end
for j = 2:n-1
    neighbor_d = [M(m-1,j-1),M(m-1,j),M(m-1,j+1),M(m,j-1),M(m,j+1)];
    Die(m,j) = sum(neighbor_d == 0) + 3;
end
Die(1,1) = sum([M(1,2),M(2,1),M(2,2)] == 0) + 5;
Die(1,n) = sum([M(1,n-1),M(2,n-1),M(2,n)] == 0) + 5;
Die(m,1) = sum([M(m-1,1),M(m-1,2),M(m,2)] == 0) + 5;
Die(m,n) = sum([M(m-1,n-1),M(m-1,n),M(m,n-1)] == 0) + 5;     
end
