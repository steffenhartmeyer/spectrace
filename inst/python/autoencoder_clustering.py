import numpy as np
import pandas as pd
from sklearn.cluster import KMeans # Clustering algorithm
from sklearn.metrics import silhouette_score # Silhouette score

import torch
from torch.utils.data import DataLoader, TensorDataset # Creating random batches
from torch import nn

##########################
# Encode data
##########################
def py_encode_data(df_spec, df_ref):
  with torch.no_grad():
    X_lab = torch.tensor(np.array(df_ref.iloc[:,1:], dtype="float64")).float()
  X_labels = torch.nn.functional.one_hot(torch.tensor(pd.factorize(df_ref.iloc[:,0])[0])).float()
  
  # unlabelled dataset
  X_unlab = torch.tensor(np.array(df_spec, dtype="float64")).float()
  
  model = py_Autoencoder(X_lab.shape[1], 10, X_labels.shape[1]) # py_Autoencoder(n_spec, n_embedded, n_labels)
  model.train(X_lab, X_unlab, X_labels) # model.train(X_lab, X_unlab, X_labels, lr=0.01, epochs=200, batch_size=5)
  
  df_spec_encoded = model.encode(X_unlab).detach().numpy()
  
  return df_spec_encoded, model

##########################
# Cluster encoded data
##########################
def py_cluster_encoded(df_spec, df_spec_encoded, ref_types, model, n_clusters, n_init=100, n_predict=2):
  cluster_ids = py_kmeans_cluster(df_spec_encoded, n_clusters, n_init, score = False)
  df_spec.insert(0, 'cluster_id', np.array(cluster_ids))
  clusters = py_cluster_predict(df_spec, model, ref_types, n_predict)
  return df_spec, clusters

##########################    
# K-means clustering on encoded spectra
##########################
def py_kmeans_cluster(df, n_clusters, n_init=1000, score=False):
    kmeans_model = KMeans(n_clusters = n_clusters, n_init=n_init)
    cluster_ids = kmeans_model.fit_predict(df) + 1
    if score:
      print(f"K-means silhouette score ({n_clusters} clusters): {silhouette_score(df, cluster_ids):.3f}")
    
    return cluster_ids

##########################
# Top k light source type predictions for each cluster
##########################
def py_cluster_predict(df, model, ref, k=2):
    cols = (["cluster_id"] + [f"type_{i+1}" for i in range(k)] + [f"percent_{i+1}" for i in range(k)] + 
            [f"prob_mean_{i+1}" for i in range(k)] + [f"prob_sd_{i+1}" for i in range(k)])
    df_pred = pd.DataFrame(columns=cols)

    for clust_id in sorted(df.cluster_id.unique()):
        df_clust = df[df.cluster_id == clust_id]

        # top_k = model.classify(torch.FloatTensor(np.array(df_clust.iloc[:,1:]))).detach().mean(0).topk(k)
        probs = model.classify(torch.FloatTensor(np.array(df_clust.iloc[:,1:]))).detach()
        probs_norm = probs / probs.max(dim=1, keepdim=True)[0]
        probs_nan = probs.clone()
        probs_nan[probs_norm != 1] = float('nan')
        
        percent = torch.count_nonzero(probs_norm == 1, dim=0) / probs_norm.size(dim=0)

        top_k = percent.topk(k)
        percent_k = torch.mul(top_k[0], 100).round(decimals=2).numpy()
        ref_k = np.array(ref)[top_k[1]]
        # prob_k = probs[:, top_k[1]].mean(0).round(decimals=2)
        prob_mean_k = probs_nan[:,top_k[1]].nanmean(0).round(decimals=2).numpy()
        prob_sd_k = nanstd(probs_nan[:,top_k[1]],0).round(decimals=2).numpy()

        entry = pd.Series(([clust_id] + [ref_k[i] for i in range(k)] + [percent_k[i] for i in range(k)] + 
                           [prob_mean_k[i] for i in range(k)] + [prob_sd_k[i] for i in range(k)]),
                          index = df_pred.columns)
        df_pred = pd.concat([df_pred, entry.to_frame().T], ignore_index=True)

    return df_pred.sort_values("cluster_id")
  
def nanstd(o,dim):
    return torch.sqrt(
                torch.nanmean(
                    torch.pow( torch.abs(o-torch.nanmean(o,dim=dim).unsqueeze(dim)),2),
                    dim=dim)
                )

##########################
# Silhouette score variation plot to help choose an appropriate number of clusters
##########################
def py_sil_score(df, n_min=2, n_max=20, n_iter=5):
    n_clusters = np.arange(n_min, n_max+1, 1, dtype=int)
    scores = []
    scores_ci = []

    for k in n_clusters:
        print(f"Calculating score for {k} clusters")
        n_scores = []
        for i in range(n_iter):
            cluster_ids = py_kmeans_cluster(df, k, n_init=10)
            n_scores.append(silhouette_score(df, cluster_ids))
        scores.append(np.mean(n_scores))
        scores_ci.append(1.96 * (np.std(n_scores) / np.sqrt(n_iter)))
        
    df_sil = pd.DataFrame(list(zip(n_clusters, scores, scores_ci)),columns=["n_clusters", "score", "CI"])
    return df_sil
    
##########################
# Autoencoder class
##########################
class py_Autoencoder(nn.Module):
    def __init__(self, n_spec, n_embedded, n_labels):
        super(py_Autoencoder, self).__init__()
        
        self.criterion1 = nn.MSELoss()
        self.criterion2 = nn.BCELoss()
        n_hidden = int((n_spec + n_embedded) / 2)
        
        self.encoder = nn.Sequential(
            nn.Linear(n_spec, n_hidden),
            nn.Sigmoid(),
            nn.Linear(n_hidden, n_embedded)
        )
        
        self.decoder = nn.Sequential(
            nn.Linear(n_embedded, n_spec),
            nn.Sigmoid()
        )
        
        self.classifier = nn.Sequential(
            nn.Linear(n_embedded, n_labels),
            nn.Softmax(dim=1)
        )
        
    def encode(self, x):
        encoded = self.encoder(x)
        return encoded
    
    def decode(self, encoded):
        decoded = self.decoder(encoded)
        return decoded
    
    def reconstruct(self, x):
        encoded = self.encode(x)
        decoded = self.decode(encoded)
        return decoded
    
    def classify(self, x):
        encoded = self.encode(x)
        out = self.classifier(encoded)
        return out
        
    def forward(self, x):
        decoded = self.reconstruct(x)
        out = self.classify(x)
        return decoded, out
    
    def train(self, X_lab, X_unlab, X_labels, lr=0.01, epochs=200, batch_size=5, bool_print=True):
        
        dataloader_lab = DataLoader(TensorDataset(X_lab, X_labels), batch_size=batch_size, shuffle=True)
        optimizer = torch.optim.Adam(self.parameters(), lr=lr)      
        
        for epoch in range(epochs): 
            for inputs, labels in dataloader_lab:      
                optimizer.zero_grad()
                decoded, out = self.forward(inputs)
                decoded_unlabelled = self.reconstruct(X_unlab)

                loss_aec_lab = self.criterion1(decoded, inputs)
                loss_aec_unlab = self.criterion1(decoded_unlabelled, X_unlab)
                loss_class = self.criterion2(out, labels)
                loss = loss_aec_lab + loss_aec_unlab + loss_class
                loss.backward()
                optimizer.step()
        
            if bool_print:
                with torch.no_grad():
                    decoded_lab = self.forward(X_lab)[0]
                    decoded_unlab = self.forward(X_unlab)[0]
                    encoded_lab = self.encode(X_lab)
                    loss_lab = self.criterion1(decoded_lab, X_lab)
                    loss_unlab = self.criterion1(decoded_unlab, X_unlab)
                    missclassified = (self.forward(X_lab)[1].argmax(1) - X_labels.argmax(1) != 0).sum().item()

                str_missclassified = f"Missclassified: {missclassified:03d}"
                str_autoenc_loss = f"Autoencoder loss (lab & unlab): {loss_lab:.4f} / {loss_unlab:.4f}"

                print(f"Epoch {epoch+1:02d} - " + str_missclassified + " - " + str_autoenc_loss)
  

