#
# This example Dockerfile illustrates a method to install
# additional packages on top of NVIDIA's PyTorch container image.
#
# To use this Dockerfile, use the `docker build` command.
# See https://docs.docker.com/engine/reference/builder/
# for more information.
#
FROM nvcr.io/nvidia/pytorch:19.06-py3

# Install openslide
RUN apt-get update && \
    apt-get install -y --no-install-recommends openslide-tools && \
    rm -rf /var/lib/apt/lists/

# Install some python packages via pip
RUN pip install --upgrade pip && \
    pip install seaborn && \
    pip install ipywidgets && \
    pip install openslide-python && \
    pip install scikit-image && \
    pip uninstall -y pillow && \
    pip install --force-reinstall Pillow-SIMD


# Install APEX with CUDA and C++ extension
#RUN git clone https://github.com/NVIDIA/apex /opt/apex && \
#	cd /opt/apex && \
#	pip install -v --no-cache-dir --global-option="--cpp_ext" --global-option="--cuda_ext" ./

# Add NVIDIA's ResNet-50 v1.5
ADD ResNet-50.tar.gz /workspace

	