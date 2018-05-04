Idea:

For converting HDF5 into another format (Parquet) use a pure Java lib with Spark.
For processing HDF5 with Spark use Python.

```bash
eval "$(docker-machine env default)"
docker build -t hdf5_exp .
docker run -dt --name hdf5_exp -v $(pwd)/files:/files hdf5_exp
docker exec -it hdf5_exp /bin/bash

docker stop hdf5_exp
docker rm hdf5_exp
```