function p = predict(Theta1, Theta2, X)
%PREDICT Predict the label of an input given a trained neural network
%   p = PREDICT(Theta1, Theta2, X) outputs the predicted label of X given the
%   trained weights of a neural network (Theta1, Theta2)

% Useful values
m = size(X, 1);
num_labels = size(Theta2, 1);

% You need to return the following variables correctly 
p = zeros(size(X, 1), 1);

% ====================== YOUR CODE HERE ======================
% Instructions: Complete the following code to make predictions using
%               your learned neural network. You should set p to a 
%               vector containing labels between 1 to num_labels.
%
% Hint: The max function might come in useful. In particular, the max
%       function can also return the index of the max element, for more
%       information see 'help max'. If your examples are in rows, then, you
%       can use max(A, [], 2) to obtain the max for each row.
%

% add ones to represent Xo
X = [ones(m, 1) X];



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
z1 = X*Theta1';
% a1 is 25x5000 big, need to add the bias since Theta2 needs 26 for input
a1 = sigmoid(z1)';
a1 = [ones(m,1)'; a1];
z2 = Theta2*a1;
a2 = sigmoid(z2);
% a2 now is 10x5000, each column is one example eg x_i
% need to figure out which index in the column is max
[t,ti] = max(a2, [], 1);
p = ti';
% =========================================================================


end
