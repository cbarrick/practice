import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import scipy as sp
import scipy.spatial.distance
import scipy.stats
import sklearn as sk
import sklearn.datasets
import sklearn.metrics


class KMeans(sk.base.BaseEstimator, sk.base.ClusterMixin, sk.base.ClassifierMixin):
    '''A custom implementation of k-means clustering.

    The benefit of this class over `sklearn.cluster.KMeans` is that this class
    allows you to assign labels to the clusters and use it as a classifier.

    Parameters:
        clusters_: The centroids of each of the k clusters.
        labels_: The labels for each cluster.
    '''

    def __init__(self, k):
        self.k = k
        self.clusters_ = None
        self.labels_ = None

    def reset(self):
        self.clusters_ = None
        self.labels_ = None

    def assign(self, X):
        '''Assign each X to its nearest cluster.

        Args:
            X (ndarray): The data points.

        Returns:
            An int vector of the same length as X,
            mapping each data point to a cluster index.
        '''
        C = self.clusters_
        dist = sp.spatial.distance.cdist(X, C)
        assn = np.argmin(dist, axis=-1)
        return assn

    def partial_fit(self, X, **kwargs):
        '''Perform a single iteration of k-means update.

        Args:
            X (ndarray): The data points to fit.
            **kwargs: Passed to `np.allclose` to determine convergence.

        Returns:
            Returns `True` if the model has converged, and `False` otherwise.
        '''
        cluster_shape = (self.k, *X.shape[1:])
        if self.clusters_ is None: self.clusters_ = np.random.random(cluster_shape)
        old = self.clusters_
        new = np.ndarray(cluster_shape)

        assn = self.assign(X)
        for i in range(self.k):
            cluster = X[assn == i]
            if len(cluster) == 0:
                new[i] = np.random.random(X.shape[1:])
            else:
                new[i] = cluster.mean(axis=0)
        self.clusters_ = new

        return np.allclose(new, old, **kwargs)

    def fit(self, X, Y=None, max_iter=100, **kwargs):
        '''Fit the model by performing some maximum number of updates.

        Args:
            X (ndarray): The data points to fit.
            Y (ndarray): Optional labels.
            max_iter (int): The maximum number of updates.
            **kwargs: Passed to `np.allclose` to determine convergence.

        Returns:
            Returns `True` if the model has converged, and `False` otherwise.
        '''
        self.reset()
        converged = False
        for i in range(max_iter):
            converged = self.partial_fit(X, **kwargs)
            if converged: break
        if Y is not None:
            self.label(X, Y)
        return converged

    def label(self, X, Y, assignments=None):
        '''Labels the clusters given a set of labeled points.

        The label of a cluster is the mode of the individual labels occuring
        within the cluster.

        Args:
            X (ndarray):
                The data points.
            Y (ndarray):
                The labels for each data point.
            assignments (array of ints):
                Maps each data point to a cluster.
                The default maps each to the nearest cluster.
        '''
        assn = self.assign(X) if assignments is None else assignments
        self.labels_ = np.zeros((self.k, *Y.shape[1:]))
        for i in range(self.k):
            mode, _ = sp.stats.mode(Y[assn == i])
            self.labels_[i] = mode[0]

    def predict(self, X):
        '''Predicts the values of X.

        If the clusters have been labeled, this returns the label of the
        nearest cluster. Otherwise it returns the centroid of the nearest
        cluster.

        Args:
            X (ndarray): The data to predict.

        Returns:
            An ndarray with the same length as X.
        '''
        assn = self.assign(X)
        if self.labels_ is not None:
            return self.labels_[assn]
        else:
            return self.clusters_[assn]

    def fit_predict(self, X, Y=None, **kwargs):
        '''Calls `est.fit(X, Y, **kwargs)` then returns `est.predict(X)`'''
        self.fit(X, Y, **kwargs)
        return self.predict(X)

    def wcss(self, X):
        '''Computes the within-cluster sum of squares (WCSS).'''
        assn = self.assign(X)
        score = 0
        for i, x in enumerate(X):
            c = self.clusters_[assn[i]]
            score += np.linalg.norm(x - c)
        return score

    def accuracy(self, X, Y, normalize=True, sample_weight=None):
        '''Computes the accuracy of the model.

        If the clusters are not yet labeled, then they are labeled according
        to X and Y. See `KMeans.label`.
        '''
        if self.labels_ is None:
            self.label(X, Y)
        pred = self.predict(X)
        return sk.metrics.accuracy_score(Y, pred, normalize, sample_weight)

    def score(self, X, Y=None):
        '''Computes a measure of performance.

        This method has three different possible outcomes:

        1. If Y is not given, it returns the inverse of the within-cluster sum
           of squares (WCSS).

        2. If Y is given and the clusters are already labeled, it returns the
           accuracy of the model.

        3. If Y is given and the clusters are not yet labeled, the clusters are
           first labeled according X and Y. Then it returns the accuracy.
        '''
        if Y is None:
            return 1 / self.wcss()
        return self.accuracy()

    def report(self, X, Y, **kwargs):
        '''Returns a classification report.'''
        pred = self.predict(X)
        return sk.metrics.classification_report(Y, pred, **kwargs)


if __name__ == '__main__':
    # Load MNIST dataset
    # --------------------------------------------------
    # This code uses Scikit-Learn's `fetch_mldata` function to download the
    # dataset from mldata.org. Unfortunately, mldata.ord is down (permanently?)
    # and is not servicing requests. Thus this code will only work if the file
    # `mnist-original.mat` has been previously downloaded and placed in the
    # `data_home` directory at `{data_home}/mldata/mnist-original.mat`.

    split = 60000
    mnist = sk.datasets.fetch_mldata('MNIST original', data_home='.')
    X_train, Y_train = mnist.data[:split], mnist.target[:split]
    X_test, Y_test = mnist.data[split:], mnist.target[split:]

    # WCSS vs Iteration
    # --------------------------------------------------

    np.random.seed(1337)
    n_iters = 100
    km = KMeans(25)
    wcss_train = np.ndarray(n_iters)
    for i in range(n_iters):
        km.partial_fit(X_train)
        wcss_train[i] = km.wcss(X_train)

    plt.plot(np.arange(n_iters) + 1, wcss_train, label='train')
    plt.title('Train WCSS vs Iteration')
    plt.xlabel('Iteration')
    plt.ylabel('Within-cluster sum of squares')
    plt.show()

    # Accuracy vs K
    # --------------------------------------------------

    for k in (5, 10, 15, 20, 25, 30):
        np.random.seed(1337)
        km = KMeans(k)
        converged = km.fit(X_train, Y_train)
        dot = 'bo' if converged else 'ro'
        plt.plot(k, km.accuracy(X_train, Y_train), dot)
    plt.title('Train Accuracy vs Number of Clusters')
    plt.xlabel('Number of Clusters')
    plt.ylabel('Train Accuracy')
    plt.show()

    # Cluster visualization and classification report
    # --------------------------------------------------
    # This section uses the most recent version of `km`,
    # which should be the best model produced by the above accuracy trial.

    # visualize the cluster centroids
    n_plots = km.k
    n_rows = int(np.sqrt(n_plots))
    n_cols = n_plots // n_rows
    if n_rows * n_cols < n_plots:
        n_rows += 1
    fig, ax = plt.subplots(n_rows, n_cols, figsize=(2*n_cols, 2*n_rows))
    for i in range(km.k):
        c = km.clusters_[i]
        l = km.labels_[i]
        plt.subplot(n_rows, n_cols, i+1)
        plt.title(f'#{i+1} ({int(l)})')
        plt.imshow(c.reshape(28, 28))
    plt.tight_layout()
    plt.show()

    # print the scores
    print(km.report(X_test, Y_test))
