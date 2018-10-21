n=250
p=10000
X = matrix(rnorm(n*p,mean=0,sd=1), n, p) 
y = rnorm(0,1)
grid=10^seq(from = 4, to = -3, length=100)  


absinprod <- abs((y%*%X)/length(y)) 

lambda_max <- max(absinprod)

lambdap <- lambda_max

for (i in 1:length(grid)) {
  
}

# lambdap = lambda_max;
# for i = 1:npar    
# lambdac = Lambdav(1,i);
# if lambdac>=lambda_max
# Sol(:,Lambda_ind(i)) = 0; 
# ind_zf(:,Lambda_ind(i)) = 1;      
# else
#   if lambdap==lambda_max
# theta = y/lambdap;
# v = X(:,indmx);
# v1 = sign(v'*theta)*v;
#           else
#           theta = (y - X*Sol(:,Lambda_ind(i-1)))*rlambdap;
#           v1 = y*rlambdap - theta;
#           end
#           
#           rlambdac = rLambdav(1,i);
#           
#           v1 = v1 / norm(v1);
#           v2 = y*rlambdac - theta;
#           Pv2 = v2 - v1*((v1'*v2));
#         o = theta + 0.5*Pv2;
#         phi = 0.5*norm(Pv2);
# 		
# 		% ------- screening by EDPP, remove the ith feature if T(i)=1 ----- %
#         T = 1 - phi*Xnorm' > abs(X'*o)+1e-8;
#         ind_zf(:,Lambda_ind(i)) = T;
#         Xr = X(:,~T);
#         
#         if lambdap == lambda_max
#             opts.x0 = zeros(size(Xr,2),1);
#         else
#             opts.x0 = Sol(~T,Lambda_ind(i-1));
#         end
# 
# 		% ------ solve the Lasso problem on the reduced data matrix ------ %
#         [x1, ~, ~]= LeastR(Xr, y, lambdac, opts);
# 
#         Sol(~T,Lambda_ind(i)) = x1;   
#         
#         lambdap = lambdac;
#         rlambdap = rlambdac;
#     end
# end