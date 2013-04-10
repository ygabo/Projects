function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
Y = [ y == 1:num_labels ];
m = size(X, 1);

% You need to return the following variables correctly 
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m
%
% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.
%
% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%
% =========================================================================

% 1 hidden layer
% INPUT -theta1-> HIDDEN -theta2-> OUTPUT
% to get z1, you apply theta1 to X
% then to get a1, you apply sigmoid
% to get z2, now output, you apply theta2 to a1
% then apply sigmoid to get a2 which is the output
% 400 inputs + 1 for bias
% 25 hidden layer units + 1 for bias
% size(Theta1) is 25x401... 401 in, 25 out
% size(Theta2) is 10x26...  26 in, 10 out (these would the digits 0-9)
% size(X) is 5000x401... 5000 training examples, 401 labels

% add ones to represent as first layer bias
a1 = [ones(m, 1) X];

% calculation of 2nd layer
z2 = a1*Theta1';
% a1 is 25x5000 big, need to add the bias since Theta2 needs 26 for input
a2 = sigmoid(z2)';
% add 2nd layer's bias
a2 = [ones(m,1)'; a2];

% calculation of 3rd layer, for this NN this is the last layer, output
z3 = Theta2*a2;
a3 = sigmoid(z3);

% a3 now is 10x5000, each column is one output of x_i
% need to figure out which index in the column is max
[t,ti] = max(a3, [], 1);
% ti is output, which means one ti is the 'guess'
% of the NN using the current theta to what x_i's answer should be
% we don't need this actually, if were training, need it to predict

% calculate J, also called 'cost'
% in other words, how good is our theta right now
% higher cost == bad theta
J =  -1/m * (sum(sum(( (Y .* log(a3)') + (( 1 - Y ) .* log( 1 - a3 )') ))));

% remove 1st column of thetas, they're the bias added 
% that we dont include in computing J
theta_1 = Theta1(:,2:end);
theta_2 = Theta2(:,2:end);

% add up all the values in the squared thetas for the regularized part
% multiply this by the learning rate
% then append this to the cost function
% we now have a regularized cost function
t = sum(sum(theta_1.^2));
e = sum(sum(theta_2.^2));
s = ((t+e) / (2*m) ) * lambda;
J = J + s;

% RECAP
% we have a1, a2, a3
% theta1, theta2
% it looks like this 
%
% X > a1 ---> theta1 ---> z2 > a2 ---> theta2 ---> z3 > a3  
% 
% We then compute J to see how good this NN is
% Now, we have to do backpropagation
% -------------------------------------------------------------

for i = 1:m
  % calculate delta_3 and delta_2 for ever (xi,yi)
  delta_3 = a3(:,i)' - Y(i,:);
  delta_2 = ( (delta_3 * Theta2) .* sigmoidGradient([1 z2(i,:)]));

  % accumulate all deltas for each (xi,yi)
  Theta2_grad += delta_3' * a2(:,i)';
  Theta1_grad += delta_2(:,2:end)' * a1(i,:);
end

% compute the regularization part
% do not include the first column since that corresponds to
% the bias
% size of Theta1 is 25x401(400 in, 25 out) -- 400 + 1 bias column
% same for Theta2 which is 10x26( 26 in, 10 out) -- 25 + 1 bias column
theta1_reg = lambda * Theta1(:,2:end);
theta2_reg = lambda * Theta2(:,2:end);

% add regularization term to the specific columns
Theta1_grad(:,2:end) += theta1_reg;
Theta2_grad(:,2:end) += theta2_reg;

% divide everything by m
Theta1_grad /= m;
Theta2_grad /= m;

% =========================================================================

% Unroll gradients
grad = [Theta1_grad(:) ; Theta2_grad(:)];


end
