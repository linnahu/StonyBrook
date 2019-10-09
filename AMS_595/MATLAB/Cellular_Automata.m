n = 200; 
M = randi([0 1],n,n); 

figure;
imshow(M);

M0 = M;
figure;
loops = 20; 
F(loops) = struct('cdata',[],'colormap',[]);
for j = 1:loops
    imshow(M);    
    drawnow;
    F(j) = getframe;
    Live = y(M);
    M = double(Live > 4);
end

movie(F,3,2);
