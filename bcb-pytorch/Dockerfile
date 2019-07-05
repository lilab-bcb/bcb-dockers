#
# This example Dockerfile illustrates a method to install
# additional packages on top of NVIDIA's PyTorch container image.
#
# To use this Dockerfile, use the `docker build` command.
# See https://docs.docker.com/engine/reference/builder/
# for more information.
#
FROM nvcr.io/nvidia/pytorch:19.05-py3

# Install openslide
RUN apt-get update && \
    apt-get install -y --no-install-recommends openslide-tools && \
    rm -rf /var/lib/apt/lists/

# Install some python packages via pip
RUN pip install --upgrade pip && \
    pip install pillow && \
    pip install seaborn && \
    pip install scikit-image && \
    pip install ipywidgets && \
    pip install openslide-python && \
    pip install --upgrade torchvision

# Add NVIDIA's ResNet-50 v1.5
ADD ResNet-50_v1.5.zip /workspace/ResNet-50

	