import h5py

f = h5py.File("/files/chunked.hdf5")
f.keys()
dset = f['dataset1']
dset.shape
dset.dtype

out = dset[...]
out

dset.chunks