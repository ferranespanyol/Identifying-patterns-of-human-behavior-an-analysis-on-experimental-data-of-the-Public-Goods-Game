from __future__ import print_function
from scipy.special import gammaln
from numpy.linalg import slogdet
import numpy as np
from numpy.matlib import repmat
from numpy.linalg import inv

def mapdp_nw(X, N0, m0, a0, c0, B0, epsilon=1e-6, maxiter=100, fDebug=False):
    '''
    MAP-DP for Normal-Wishart data.

    Inputs:  X  - DxN matrix of data
             N0 - prior count (DP concentration parameter)
             m0 - cluster prior mean
             a0 - cluster prior scale
             c0 - cluster prior degrees of freedom
             B0 - cluster prior precision (inverse covariance)
             epsilon - convergence tolerance
             maxiter - maximum number of iterations, overrides threshold calculation
             fDebug - extra verbose input of algorithm operation

    Outputs: mu - cluster centroids
             z  - data point cluster assignments
             K  - number of clusters
             E  - objective function value for each iteration

    CC BY-SA 3.0 Attribution-Sharealike 3.0, Max A. Little. If you use this
    code in your research, please cite:
    Yordan P. Raykov, Alexis Boukouvalas, Fahd Baig, Max A. Little (2016)
    "What to do when K-means clustering fails: a simple yet principled alternative algorithm",
    PLoS One, (11)9:e0162259
    This implementation follows the description in that paper.
    '''
   
    # Initialization (Alg. 3 line 1)
    (D, N) = X.shape
    assert(D > 0)
    assert(N > 0)
    K = 1
    z = np.zeros((N), dtype=int)  # everybody assigned to first cluster
    Enew = np.inf
    dE = np.inf
    ic = 0  # iteration coung
    E = list()
    # Convergence test (Alg. 3 line 14 and Appendix B)
    while (abs(dE) > epsilon and ic < maxiter):
        Eold = Enew
        dik = np.ones((N, 1)) * np.inf
        for i in range(N):
            dk = np.ones((K+1, 1)) * np.inf
            f = np.empty(K+1)
            Nki = np.ones((K), dtype=int)
            xi = np.atleast_2d(X[:, i]).T  # current data point
            for k in range(K):
                zki = (z == k)
                zki[i] = False
                Nki[k] = zki.sum()
                # Updates meaningless for Nki=0
                if (Nki[k] == 0):
                    continue
                # Update NW cluster hyper parameters (Alg. 3 line 7)
                mki, aki, cki, Bki = nwupd(Nki[k], X[:, zki], m0, a0, c0, B0)

                # Compute Student-t NLL, existing clusters (Alg. 3 line 8)
                dk[k] = stnll(xi, mki, aki, cki, Bki, D)
                # Avoid reinforcement effect at initialization (Appendix B)
                if (ic == 0):
                    Nki[0] = 1
                f[k] = dk[k]-np.log(Nki[k])
            # Compute Student-t NLL, new cluster (Alg. 3 line 9)
            dk[K] = stnll(xi, m0, a0, c0, B0, D)
            f[K] = dk[K]-np.log(N0)
            # Compute MAP assignment (Alg. 3 line 10)
            if(fDebug):
                print(i, 'Compute MAP assignment K=', K, 'f=', f, 'dk=', dk)

            z[i] = np.argmin(f)
            dik[i] = f[z[i]]
            # Create new cluster if required (Alg. 3 line 11-12)
            if (z[i] == K):
                K = K + 1
        # Remove any empty clusters and re-assign (Appendix B)
        Knz = 0
        for k in range(K):
            i = (z == k)
            Nk = i.sum()
            if (Nk > 0):
                z[i] = Knz
                Knz = Knz + 1
        K = Knz
        Nk, _ = np.histogram(z, range(K+1))
        # Compute updated NLL (Alg. 3 line 13)
        Enew = dik.sum()-K*np.log(N0)-np.sum(gammaln(Nk))
        dE = Eold - Enew
        ic += 1
        E.append(Enew)
        print('Iteration %d: K=%d, E=%f, dE=%f\n' % (ic, K, Enew, dE))

    # Compute cluster centroids (Appendix D)
    mu = np.ones((D, K))
    for k in range(K):
        xk = X[:, z == k]
        mu[:, k] = xk.mean(1)
    return mu, z, K, E


def stnll(x, m, a, c, B, D):
    '''
    Compute Student-t negative log likelihood (Appendix A, eqn. (20))
    '''
    mu = m
    nu = a-D+1
    Lambda = c*float(nu)/(c+1)*B
    S = np.dot(np.dot((x-mu).T, Lambda), (x-mu))
    _, logdetL = slogdet(Lambda)
    return float(nu+D)/2.*np.log(1.+S/float(nu))\
        - 0.5*logdetL+gammaln(nu/2.)\
        - gammaln((float(nu)+D)/2.)+D/2.*np.log(float(nu)*np.pi)


def nwupd(Nki, xki, m0, a0, c0, B0):
    '''
    Update Normal-Wishart hyper parameters (Appendix A, eqns. (18-19))
    '''
    xmki = xki.mean(1)[:, None]
    xmcki = xki-repmat(xmki, 1, Nki)
    Ski = np.dot(xmcki, xmcki.T)
    cki = c0+Nki
    mki = (c0*m0+Nki*xmki)/cki
    xm0cki = xmki-m0
    Bki = inv(inv(B0)+Ski+c0*Nki/cki*np.dot(xm0cki, xm0cki.T))
    aki = a0+Nki
    return mki, aki, cki, Bki
