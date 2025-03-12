import pandas as pd
import numpy as np

# Load the datasets; adjust the file paths if necessary
train_df = pd.read_csv('/mnt/data/ionosphere.train.csv')
test_df  = pd.read_csv('/mnt/data/ionosphere.test.csv')

# Assume that the last column contains the labels and the rest are features
X_train = train_df.iloc[:, :-1].values.astype(float)
y_train = train_df.iloc[:, -1].values
X_test  = test_df.iloc[:, :-1].values.astype(float)
y_test  = test_df.iloc[:, -1].values

def predict_knn(X_train, y_train, query, k):
    # Compute Euclidean distances between the query and all training points
    distances = np.linalg.norm(X_train - query, axis=1)
    # Get the indices of the k closest neighbors
    k_indices = np.argsort(distances)[:k]
    # Get the labels of these k neighbors
    k_labels = y_train[k_indices]
    
    # Count the frequency of each label
    unique, counts = np.unique(k_labels, return_counts=True)
    
    # Find the label(s) with maximum count
    max_count = counts.max()
    candidates = unique[counts == max_count]
    
    if len(candidates) == 1:
        return candidates[0]
    else:
        # Tie-breaker: choose the label of the nearest neighbor among candidates
        for idx in k_indices:
            if y_train[idx] in candidates:
                return y_train[idx]

def compute_accuracy(X_train, y_train, X_test, y_test, k):
    predictions = []
    for query in X_test:
        pred = predict_knn(X_train, y_train, query, k)
        predictions.append(pred)
    predictions = np.array(predictions)
    accuracy = np.mean(predictions == y_test)
    return accuracy

# Try different values of k and record the accuracy
k_values = range(1, 21)  # Testing k from 1 to 20
results = {}
for k in k_values:
    acc = compute_accuracy(X_train, y_train, X_test, y_test, k)
    results[k] = acc
    print(f"k = {k:2d}: Accuracy = {acc*100:.2f}%")

# Identify the optimal k (the one with the highest accuracy)
optimal_k = max(results, key=results.get)
print(f"\nOptimal k: {optimal_k} with accuracy {results[optimal_k]*100:.2f}%")
